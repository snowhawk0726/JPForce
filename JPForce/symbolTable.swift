//
//  symbolTable.swift
//  JPForce
//
//  Created by 佐藤貴之 on 2024/09/30.
//

import Foundation

enum SymbolScope: String {
    case GLOBAL, LOCAL, PREDICATE, PROPETRY, FREE, FUNCTION
    //
    var opCode: Opcode {
        switch self {
        case .GLOBAL: return .opGetGlobal       // 大域
        case .LOCAL: return .opGetLocal         // 局所
        case .PREDICATE: return .opPredicate    // 述語
        case .PROPETRY: return .opGetProperty   // 属性
        case .FREE: return .opGetFree           // 自由変数
        case .FUNCTION: return .opCurrentClosure// 関数
        }
    }
}
struct Symbol : Equatable {
    let name: String
    let scope: SymbolScope
    let index: Int
    //
    var isProperty: Bool {scope == .PROPETRY}
    var isPredicate: Bool {scope == .PREDICATE}
    var isVariable: Bool {scope == .LOCAL || scope == .GLOBAL || scope == .FREE}
    var isGlobal: Bool {scope == .GLOBAL}
    var isLocal: Bool {scope == .LOCAL}
    var isFree: Bool {scope == .FREE}
    /// シンボルに応じた命令語を出力する。
    func emit(with c: Compiler) {_ = c.emit(op: scope.opCode, operand: index)}
}
class SymbolTable : Equatable {
    init() {
        ObjectProperties().names.enumerated().forEach {                 // オブジェクトの属性
            _ = define(name: $1, index: $0, scope: .PROPETRY)
        }
        PredicateOperableFactory.predicates.enumerated().forEach {      // 述語オブジェクト
            _ = define(name: $1.keyword.rawValue, index: $0, scope: .PREDICATE)
        }
    }
    convenience init(outer: SymbolTable) {
        self.init()
        self.outer = outer
    }
    var outer: SymbolTable?
    private var store: [String: Symbol] = [:]
    var numberOfDefinitions = 0
    var freeSymbols: [Symbol] = []
    //
    static func == (lhs: SymbolTable, rhs: SymbolTable) -> Bool {
        lhs.store == rhs.store
        && lhs.numberOfDefinitions == rhs.numberOfDefinitions
    }
    subscript(index: Int) -> String? {  // シンボルテーブルから識別子名を取得
        store.first(where: {$0.value.isVariable && $0.value.index == index})?.key
    }
    // シンボル定義
    func define(name: String, index: Int, scope: SymbolScope) -> Symbol {
        let symbol = Symbol(name: name, scope: scope, index: index)
        store[name] = symbol
        return symbol
    }
    func define(_ name: String) -> Symbol {
        let symbol = define(
            name: name,
            index: numberOfDefinitions,
            scope: outer != nil ? .LOCAL : .GLOBAL
        )
        numberOfDefinitions += 1
        return symbol
    }
    func define(free orignal: Symbol) -> Symbol {
        freeSymbols.append(orignal)
        return define(name: orignal.name, index: freeSymbols.count - 1, scope: .FREE)
    }
    func define(functionName: String) -> Symbol {
        define(name: functionName, index: 0, scope: .FUNCTION)
    }
    // シンボル解決
    func resolve(_ name: String) -> Symbol? {
        if let symbol = store[name] {return symbol}
        // 外側で解決
        guard let symbol = outer?.resolve(name) else {return nil}
        return (symbol.isFree || symbol.isLocal) ? define(free: symbol) : symbol
    }
    func resolve(_ token: Token) -> Symbol? {resolve(token.unwrappedLiteral)}
}
