//
//  environment.swift
//  日本語ふぉーす(JPForce)
//
//  Created by 佐藤貴之 on 2023/03/06.
//
import Foundation

class Environment {
    init(outer: Environment? = nil) {self.outer = outer}
    var outer: Environment?     // 拡張環境
    private var store: [String: JpfObject] = [:]
    private var stack: [JpfObject] = []
    // MARK: - 辞書操作
    subscript(_ name: String) -> JpfObject? {
        get {store[name] ?? outer?[name]}           // 外部環境の取得は可
        set {
            var object = newValue
            object?.name = name                     // 識別子名をオブジェクトに記録
            store[name] = object
        }
    }
    var enumerated: [(key: String, value: JpfObject)] {
        store.map {(key: $0, value: $1)}
    }
    var values: [JpfObject] {Array(store.values) + (outer?.values ?? [])}
    var enumeratedStringArray: JpfArray {
        return JpfArray(elements: enumerated.map {
            JpfString(value: $0.key + "が" + $0.value.string)
        })
    }
    func contains(_ name: String) -> Bool {store.keys.contains(name)}
    /// 辞書にオブジェクトを追加する。
    ///     未登録のオブジェクトであれば、そのまま辞書に登録
    ///     登録済みのオブジェクトが配列(JpfArray)であれば、配列の要素に追加
    ///     それ以外は、登録済みオブジェクトと追加オブジェクトの配列(JpfArray)を作り、配列を辞書に登録
    /// - Parameters:
    ///   - object: 追加するオブジェクト
    ///   - name: 追加対象の識別子名
    func append(_ object: JpfObject, to name: String) {
        if contains(name) {
            if var array = self[name] as? JpfArray {
                array.elements.append(object)
            } else {
                self[name] = JpfArray(elements: [self[name]!, object])
            }
            return
        }
        self[name] = object
    }
    /// 辞書からオブジェクトを取り出す。
    /// - Parameter name: 対象の識別子名
    /// - Returns: 取り出したオブジェクト(もしくはnil)
    func retrieve(name: String) -> JpfObject? {
        var object = self[name]
        if let array = object as? JpfArray {
            object = array.elements.first
        }
        return object
    }
    /// 辞書から識別子に対応したオブジェクトを削除する。
    /// - Parameter name: 対象の識別子名
    func remove(name: String) {
        if var array = self[name] as? JpfArray {
            array.elements.removeFirst()
            if array.elements.count == 1 {
                self[name] = array.elements.first
            }
        } else {
            self[name] = nil
        }
    }
    // MARK: - スタック操作
    func push(_ object: JpfObject)  {stack.append(object)}
    func push(_ objects: [JpfObject])   {stack += objects}
    func pull() -> JpfObject?       {stack.popLast()}
    /// スタックに指定のオブジェクトがあったら、それを取り出す。(無ければ nil)
    func pull(where codition: (JpfObject) -> Bool) -> JpfObject? {
        guard let p = stack.lastIndex(where: { codition($0) }) else {return nil}
        return stack.remove(at: p)
    }
    func pullAll() -> [JpfObject]   {defer {empty()}; return stack}
    func getAll() -> [JpfObject]    {return stack}
    func drop()                     {_ = pull()} // removeLast()を使うと、emptyチェックが必要
    func drop(_ n: Int)             {stack.removeLast(n <= count ? n : count)}
    func empty()                    {stack.removeAll()}
    func swap()                     {if count >= 2 {stack.swapAt(count-2, count-1)}}
    //
    var isEmpty: Bool               {stack.isEmpty}
    var count: Int                  {stack.count}
    var peek: JpfObject?            {stack.last}
    func peek(_ numberOf: Int) -> [JpfObject]? {
        guard numberOf <= count else {return nil}
        return Array(stack[(count - numberOf)..<count])
    }
    subscript(index: Int) -> JpfObject? {
        guard case 0..<count = index else {return nil}
        return stack[index]
    }
    var string: String              {stack.map {$0.string}.joined(separator: " ")}
    // MARK: - 入力操作
    var isPeekParticle: Bool {peek?.particle != nil}
    func isPeekParticle(_ particle: Token.Particle) -> Bool {
        peek?.particle.map {$0.type} == Token.particle(particle).type
    }
    var unwrappedPeek: JpfObject? {
        if peek is JpfPhrase {return peek?.value}
        return peek
    }
    func unwrapPhrase() -> JpfObject? {
        guard let object = peek as? JpfPhrase else {return nil}
        defer {drop()}
        return object.value ?? object
    }
    func getName(from object: JpfObject?) -> String {
        var name = object?.value?.name ?? ""
        if name.isEmpty, let string = object?.value as? JpfString {name = string.value}
        return name
    }
    func getName() -> String {return getName(from: peek)}
    /// 多重定義の処理ブロックから、引数形式が一致するものを抽出し、処理を行う。
    /// - Parameters:
    ///   - functions: 処理ブロック(【入力が〜、本体が〜。】)
    ///   - local: 処理を行う環境 (selfは入力環境)
    /// - Returns: 処理結果、エラー、nil
    func execute(_ functions: [FunctionBlock], with local: Environment) -> JpfObject? {
        for function in functions.reversed() {              // 多重定義
            let designated = function.signature
            let names = function.parameters.map {$0.value}
            if hasParameters(to: designated, with: names) { // 引数が一致する入力を処理
                if local.apply(function.parameters, with: designated, from: self) {
                    defer {push(local.pullAll())}           // スタックを戻す
                    if let body = function.body, let result = Evaluator(from: body, with: local).object {
                        if result.isError {return result}
                        if result.isReturnValue {return result.value!}
                    }
                    return nil
                }
            }
        }
        if functions.count == 1 {   // 単体の定義の場合、エラーメッセージを作成する
            return errorMessage(to: functions.first!)
        }
        return noMatchingSignature
    }
    /// 外部のスタック上の引数(オブジェクト)の形式チェックを行い値を取り出し、各識別子に引き当て、内部の辞書に登録する。
    /// - Parameters
    ///   - parameters: 識別子(Identifier)の配列
    ///   - designated: チェックをする入力形式
    ///   - outer:  引数を格納しているスタックを含む環境
    /// - Returns: 引数がパラメータおよび指定形式に合わない場合は、falseを返す。
    func apply(_ parameters: [Identifier], with designated: InputFormat, from outer: Environment) -> Bool {
        if designated.numberOfInputs == nil {   // 可変長入力
            for (p, f) in zip(parameters.reversed(), designated.formats.reversed()) {
                if f.particle.hasSuffix("…") {  // 連続する同じ格の句→配列
                    guard let array = getArray(from: outer, with: f) else {return false}
                    store[p.value] = array
                } else {
                    store[p.value] = outer.pull()?.value ?? JpfNull.object
                }
            }
        } else {                                // 固定(指定)長入力
            guard let values = getValues(from: outer.peek(parameters.count)!, with: designated.formats) else {return false}
            zip(parameters, values).forEach {store[$0.value] = $1}
            outer.drop(parameters.count)
        }
        return true
    }
    /// 「と…」の様な同格の入力(句)から値を取り出し、配列にする
    private func getArray(from environment: Environment, with format: (type: String, particle: String)) -> JpfArray? {
        let particle = String(format.particle.dropLast(1))      // 「…」を除く
        var array: [JpfObject] = []
        while let o = environment.peek, o.particle?.literal == particle {   // 指定の格が続く限り
            array.append(o.value ?? JpfNull.object)
            environment.drop()
        }
        guard !array.isEmpty else {return nil}
        return JpfArray(elements: array.reversed())
    }
    /// 入力を指定形式でチェックし、一致すれば値を取り出して返す
    private func getValues(from objects: [JpfObject], with formats: [(type: String, particle: String)]) -> [JpfObject]? {
        var values: [JpfObject] = []
        for object in objects {
            values.append(object.value ?? JpfNull.object)
        }
        return values
    }
    /// スタック上の引数(オブジェクト)と指定された形式が一致するかをチェックする。
    /// - Returns: 一致(true)または、不一致(false)、チェック不要(true)
    /// - Parameters:
    ///   - signature: 指定された形式(引数の数および型と格)
    ///   - names: 引数の識別子
    func hasParameters(to signature: InputFormat, with names: [String]) -> Bool {
        if let number = signature.numberOfInputs {          // 固定長引数
            if number == 0 {return true}                    // 確認する形式が無い
            guard number <= count, let params = peek(number) else {return false}
            for (param, format, name) in zip(params, signature.formats, names) {
                if !isSameName(of: param, as: name) ||
                    !isSameType(of: param, as: format.type) ||
                    !isSameParticle(of: param, as: format.particle) {
                    return false                            // 識別子と、引数(型と格)が一致しない
                }
            }
            return true
        } else {                                            // 可変長引数
            for format in signature.formats {
                if contains(format) {return true}
            }
        }
        return false
    }
    /// スタック上の引数(オブジェクト)に指定された形式が含まれるかチェックする。
    /// - Parameter designated: 指定された形式(型と格)
    /// - Returns: 含まれる(true)か否(false)
    private func contains(_ designated: (type: String, particle: String)) -> Bool {
        let particle = designated.particle.hasSuffix("…") ? String(designated.1.dropLast(1)) : designated.particle
        return getAll().contains { object in
            isSameType(of: object, as: designated.type) && isSameParticle(of: object, as: particle)
        }
    }
    /// 対象のオブジェクトの型をチェック
    func isSameType(of object: JpfObject, as type: String) -> Bool {
        return type.isEmpty || object.value?.contains(type: type) ?? false
    }
    /// 対象のオブジェクトの格をチェック
    func isSameParticle(of object: JpfObject, as particle: String) -> Bool {
        return particle.isEmpty || object.particle?.literal == particle
    }
    /// 対象のオブジェクトの識別子をチェック
    func isSameName(of object: JpfObject, as name: String) -> Bool {
        return object.value?.name.isEmpty ?? true || object.value?.name == name
    }
    func errorMessage(to function: FunctionBlock) -> JpfError? {
        let numberOfParameters = function.parameters.count
        if let numberOfInputs = function.signature.numberOfInputs { // 固定長入力指定
            guard numberOfInputs == numberOfParameters && self.count >= numberOfParameters else {return InputFormatError.numberOfParameters(numberOfParameters).message}
            let parameters = self.peek(numberOfParameters)!
            for (parameter, format) in zip(parameters, function.signature.formats) {
                if let error = errorMessage(to: parameter, with: format) {return error}
            }
        } else {                                                    // 可変長入力指定
            if self.isEmpty {return InputFormatError.numberOfParameters(numberOfParameters).message}
            let params = self.getAll()
            var n = params.count - 1
            for format in function.signature.formats.reversed() {
                if format.particle.hasPrefix("…") {
                    return errorMessage(to: params[0..<n].reversed(), with: format)
                } else {
                    if let error = errorMessage(to: params[n], with: format) {return error}
                    n -= 1
                }
            }
        }
        return nil
    }
    /// 可変長の引数のエラーチェック
    /// - Parameters:
    ///   - parameters: 引数
    ///   - format: チェックする入力形式
    /// - Returns: エラー、無ければnil
    private func errorMessage(to parameters: [JpfObject], with format: (type: String, particle: String)) -> JpfError? {
        let particle = String(format.particle.dropLast(1))      // 「…」を除いた指定の格
        var counter = 0
        for parameter in parameters {
            if !isSameParticle(of: parameter, as: particle) {   // 指定の格と異なる
                break
            }
            if !isSameType(of: parameter, as: format.type) {    // 指定の型と異なる
                return InputFormatError.type(parameter.value?.type ?? "無").message
            }
            counter += 1
        }
        if counter <= 0 {                                       // 可変長の引数が無かった
            return InputFormatError.particle(parameters.first?.particle?.literal ?? "無").message
        }
        return nil
    }
    /// 引数のエラーチェック(型と格)
    /// - Parameters:
    ///   - parameter: 引数
    ///   - format: チェックする入力形式
    /// - Returns: エラー、無ければnil
    private func errorMessage(to parameter: JpfObject, with format: (type: String, particle: String)) -> JpfError? {
        if !isSameType(of: parameter, as: format.type) {return InputFormatError.type(parameter.value?.type ?? "無").message}
        if !isSameParticle(of: parameter, as: format.particle) {return InputFormatError.particle(parameter.particle?.literal ?? "無").message}
        return nil
    }
    // エラー
    enum InputFormatError : Error {
        case numberOfParameters(Int)
        case type(String)
        case particle(String)
        /// エラーメッセージ
        var message: JpfError {
            switch self {
            case .numberOfParameters(let number):  return JpfError("入力の数が足りていない。必要数：\(number)")
            case .type(let type):                  return JpfError("入力の型が異なる。入力の型：\(type)")
            case .particle(let particle):          return JpfError("入力の格が異なる。入力の格：\(particle)")
            }
        }
    }
    let noMatchingSignature = JpfError("入力形式が一致する関数が見つからなかった。")
}

/// zip(_:_:_:)
/// by [Qiita]<https://qiita.com/s2mr/items/024cc33e3bd1add3ad4b>
/// 2024/3/30
func zip<A: Sequence, B: Sequence, C: Sequence>(_ a: A, _ b: B, _ c: C) -> Zip3Sequence<A, B, C> {
    Zip3Sequence(a, b, c)
}
struct Zip3Sequence<A: Sequence, B: Sequence, C: Sequence>: Sequence, IteratorProtocol {
    private var a: A.Iterator
    private var b: B.Iterator
    private var c: C.Iterator
    init(_ a: A, _ b: B, _ c: C) {
        self.a = a.makeIterator()
        self.b = b.makeIterator()
        self.c = c.makeIterator()
    }
    mutating func next() -> (A.Element, B.Element, C.Element)? {
        guard let a = a.next(), let b = b.next(), let c = c.next() else {return nil}
        return (a, b, c)
    }
}
