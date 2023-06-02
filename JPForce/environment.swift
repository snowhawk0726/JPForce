//
//  environment.swift
//  日本語ふぉーす(JPForce)
//
//  Created by 佐藤貴之 on 2023/03/06.
//
import Foundation

class Environment {
    init(outer: Environment? = nil, overwrite: Bool = false) {self.outer = outer;self.overwrite = false}
    var outer: Environment?     // 拡張環境
    var overwrite: Bool
    private var store: [String: JpfObject] = [:]
    private var stack: [JpfObject] = []
    // 辞書操作
    subscript(_ name: String) -> JpfObject? {
        get {store[name] ?? outer?[name]}           // 外部環境の取得は可
        set {
            if overwrite && outer?[name] != nil {   // 外部環境への上書き可
                outer![name] = newValue
                return
            }
            store[name] = newValue
        }
    }
    var enumerated: [(key: String, value: JpfObject)] {
        store.map {(key: $0, value: $1)}
    }
    var values: [JpfObject] {Array(store.values)}
    var enumeratedStringArray: JpfArray {
        return JpfArray(elements: enumerated.map {
            JpfString(value: $0.key + " = " + $0.value.string)
        })
     }
    // スタック操作
    func push(_ object: JpfObject)  {stack.append(object)}
    func push(_ objects: [JpfObject])   {stack += objects}
    func pull() -> JpfObject?       {stack.popLast()}
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
    //
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
    /// 外部のスタック上の引数(オブジェクト)の形式チェックを行い値を取り出し、各識別子に引き当て、内部の辞書に登録する。
    /// - Parameters
    ///   - parameters: 識別子(Identifier)の配列
    ///   - designated: チェックをする入力形式
    ///   - outer:  引数を格納しているスタックを含む環境
    /// - Returns: 引数がパラメータおよび指定形式に合わない場合は、JpfErrorを返す。
    func apply(_ parameters: [Identifier], with designated: InputFormat, from outer: Environment) -> JpfObject {
        if designated.numberOfInputs == nil {    // 可変長入力
            guard let o = outer.peek else {return InputFormatError.numberOfParameters(parameters.count).message}
            for (p, f) in zip(parameters.reversed(), designated.formats.reversed()) {
                if f.particle.hasSuffix("…") {  // 連続する同じ格の句→配列
                    switch getArray(from: outer, with: f) {
                    case .success(let array):
                        store[p.value] = array
                    case .failure(let error):
                        return error.message
                    }
                } else {
                    guard isSameType(of: o, as: f.type) else {return InputFormatError.type(o.value?.type ?? "無").message}
                    guard isSameParticle(of: o, as: f.particle) else {return InputFormatError.particle(o.particle?.literal ?? "無").message}
                    store[p.value] = outer.pull()?.value ?? JpfNull.object
                }
            }
        } else {                                // 固定(指定)長入力
            guard designated.numberOfInputs == parameters.count && outer.count >= parameters.count else {return InputFormatError.numberOfParameters(parameters.count).message}
            switch getValues(from: outer.peek(parameters.count)!, with: designated.formats) {
            case .success(let values):
                zip(parameters, values).forEach {store[$0.value] = $1}
                outer.drop(parameters.count)
            case .failure(let error):
                return error.message
            }
        }
        return JpfBoolean.TRUE
    }
    /// 「と…」の様な同格の入力(句)から値を取り出し、配列にする
    private func getArray(from environment: Environment, with format: (type: String, particle: String)) -> Result<JpfArray, InputFormatError> {
        let particle = String(format.particle.dropLast(1))      // 「…」を除く
        var array: [JpfObject] = []
        while let o = environment.peek, o.particle?.literal == particle {   // 指定の格が続く限り
            guard isSameType(of: o, as: format.type) else {return .failure(.type(o.value?.type ?? "無"))}
            array.append(o.value ?? JpfNull.object)
            environment.drop()
        }
        guard !array.isEmpty else {return .failure(.particle(environment.peek?.particle?.literal ?? "無"))}
        return .success(JpfArray(elements: array.reversed()))
    }
    /// 入力を指定形式でチェックし、一致すれば値を取り出して返す
    private func getValues(from objects: [JpfObject], with formats: [(type: String, particle: String)]) -> Result<[JpfObject], InputFormatError> {
        var values: [JpfObject] = []
        for (o, f) in zip(objects, formats) {
            guard isSameType(of: o, as: f.type) else {return .failure(.type(o.value?.type ?? "無"))}
            guard isSameParticle(of: o, as: f.particle) else {return .failure(.particle(o.particle?.literal ?? "無"))}
            values.append(o.value ?? JpfNull.object)
        }
        return .success(values)
    }
    /// 対象のオブジェクトの型をチェック
    func isSameType(of object: JpfObject, as type: String) -> Bool {
        return type.isEmpty || object.value?.type == type
    }
    /// 対象のオブジェクトの格をチェック
    func isSameParticle(of object: JpfObject, as particle: String) -> Bool {
        return particle.isEmpty || object.particle?.literal == particle
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
}
