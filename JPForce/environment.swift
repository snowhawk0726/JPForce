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
    // 辞書操作
    subscript(_ name: String) -> JpfObject? {
        get {store[name] ?? outer?[name]}   // 外部環境の取得は可(書き込みは不可)
        set {store[name] = newValue}
    }
    var enumerated: [(key: String, value: JpfObject)] {
        store.map {(key: $0, value: $1)} + (outer?.enumerated ?? [])
    }
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
    var unwrappedPeek: JpfObject? {peek?.value}
    func unwrapPhrase() -> JpfObject? {
        guard let object = peek as? JpfPhrase else {return nil}
        defer {drop()}
        return object.value ?? object
    }
    /// 外部のスタック上の引数(オブジェクト)の形式チェックを行い値を取り出し、各識別子に引き当て、内部の辞書に登録する。
    /// - Parameter parameters: 識別子(Identifier)の配列
    /// - Parameter outer:引数を格納しているスタックを含む環境
    /// - Parameter designated: 指定の形式でチェックをする
    /// - Returns: 引数がパラメータおよび指定形式に合わない場合は、JpfErrorを返す。
    func apply(_ parameters: [Identifier], with designated: InputFormat, from outer: Environment) -> JpfObject {
        if designated.numberOfInputs == nil {    // 可変長入力
            guard let o = outer.peek else {return numberOfParameterError + String(parameters.count)}
            for (p, f) in zip(parameters.reversed(), designated.formats.reversed()) {
                if f.particle.hasSuffix("…") {  // 連続する同じ格の句→配列
                    switch getArray(from: outer, with: f) {
                    case .success(let array):
                        store[p.value] = array
                    case .failure(let error):
                        return parameterFormatError(error)
                    }
                } else {
                    guard isSameType(of: o, as: f.type) else {return parameterTypeError + (o.value?.type ?? "無")}
                    guard isSameParticle(of: o, as: f.particle) else {return parameterParticleError + (o.particle?.literal ?? "無")}
                    store[p.value] = outer.pull()?.value ?? JpfNull.object
                }
            }
        } else {                                // 固定(指定)長入力
            guard designated.numberOfInputs == parameters.count && outer.count >= parameters.count else {return numberOfParameterError + String(parameters.count)}
            switch getValues(from: outer.peek(parameters.count)!, with: designated.formats) {
            case .success(let values):
                zip(parameters, values).forEach {store[$0.value] = $1}
                outer.drop(parameters.count)
            case .failure(let error):
                return parameterFormatError(error)
            }
        }
        return JpfBoolean.TRUE
    }
    /// 「と…」の様な同格の入力(句)から値を取り出し、配列にする
    private func getArray(from environment: Environment, with format: (type: String, particle: String)) -> Result<JpfArray, InputFormatError> {
        let particle = String(format.particle.dropLast(1))      // 「…」を除く
        var array: [JpfObject] = []
        var o = environment.peek
        while o != nil && o!.particle?.literal == particle {    // 指定の格が続く限り
            guard isSameType(of: o!, as: format.type) else {return .failure(.parameterTypeError(o?.value?.type ?? "無"))}
            array.append(o!.value ?? JpfNull.object)
            environment.drop()
            o = environment.peek
        }
        guard !array.isEmpty else {return .failure(.parameterParticleError(o?.particle?.literal ?? "無"))}
        return .success(JpfArray(elements: array.reversed()))
    }
    /// 入力を指定形式でチェックし、一致すれば値を取り出して返す
    private func getValues(from objects: [JpfObject], with formats: [(type: String, particle: String)]) -> Result<[JpfObject], InputFormatError> {
        var values: [JpfObject] = []
        for (o, f) in zip(objects, formats) {
            guard isSameType(of: o, as: f.type) else {return .failure(.parameterTypeError(o.value?.type ?? "無"))}
            guard isSameParticle(of: o, as: f.particle) else {return .failure(.parameterParticleError(o.particle?.literal ?? "無"))}
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
        case numberOfParameterError(Int)
        case parameterTypeError(String)
        case parameterParticleError(String)
    }
    var numberOfParameterError: JpfError    {JpfError("「関数」の入力の数が足りていない。必要数：")}
    var parameterTypeError: JpfError        {JpfError("「関数」の入力の型が異なる。入力の型：")}
    var parameterParticleError: JpfError    {JpfError("「関数」の入力の格が異なる。入力の格：")}
    /// パラメータのエラー変換
    private func parameterFormatError(_ error: InputFormatError) -> JpfError {
        switch error {
        case .parameterTypeError(let type):         return parameterTypeError + type
        case .parameterParticleError(let particle): return parameterParticleError + particle
        case .numberOfParameterError(let number):   return numberOfParameterError + String(number)
        }
    }
}
