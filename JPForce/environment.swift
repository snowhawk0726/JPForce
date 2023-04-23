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
        get {store[name] ?? outer?[name]}
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
    /// 外部のスタック上の引数(オブジェクト)を各識別子に引き当て、内部の辞書に登録する。
    /// - Parameter parameters: 識別子(Identifier)の配列
    /// - Parameter outer:引数を格納しているスタックを含む環境
    /// - Returns: 引数がパラメータに満たない場合は、falseを返す。
    func apply(_ parameters: [Identifier], from outer: Environment) -> Bool {
        guard outer.count >= parameters.count else {return false}
        parameters.reversed().forEach {store[$0.value] = outer.pull()}
        return true
    }
}
