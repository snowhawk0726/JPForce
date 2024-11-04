//
//  symbolTable.swift
//  JPForce
//
//  Created by 佐藤貴之 on 2024/09/30.
//

import Foundation

enum SymbolScope: String {
    case GLOBAL, LOCAL, PREDICATE, PROPETRY
    //
    var opCode: Opcode {
        switch self {
        case .GLOBAL: return .opGetGlobal       // 大域
        case .LOCAL: return .opGetLocal         // 局所
        case .PREDICATE: return .opPredicate    // 述語
        case .PROPETRY: return .opGetProperty   // 属性
        }
    }
}
struct Symbol : Equatable {
    let name: String
    let scope: SymbolScope
    let index: Int
    //
    var opCode: Opcode {scope.opCode}
    var isProperty: Bool {scope == .PROPETRY}
    var isPredicate: Bool {scope == .PREDICATE}
    var isVariable: Bool {scope == .LOCAL || scope == .GLOBAL}
    var isGlobal: Bool {scope == .GLOBAL}
    var isLocal: Bool {scope == .LOCAL}
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
    //
    static func == (lhs: SymbolTable, rhs: SymbolTable) -> Bool {
        lhs.store == rhs.store
        && lhs.numberOfDefinitions == rhs.numberOfDefinitions
    }
    //
    func define(_ name: String) -> Symbol {
        let symbol = define(
            name: name,
            index: numberOfDefinitions,
            scope: outer != nil ? .LOCAL : .GLOBAL
        )
        numberOfDefinitions += 1
        return symbol
    }
    func define(name: String, index: Int, scope: SymbolScope) -> Symbol {
        let symbol = Symbol(name: name, scope: scope, index: index)
        store[name] = symbol
        return symbol
    }
    func resolve(_ name: String) -> Symbol? {store[name] ?? outer?.resolve(name)}
    func resolve(_ token: Token) -> Symbol? {resolve(token.unwrappedLiteral)}
    //
    subscript(index: Int) -> String? {
        store.first(where: {$0.value.isVariable && $0.value.index == index})?.key
    }
}
