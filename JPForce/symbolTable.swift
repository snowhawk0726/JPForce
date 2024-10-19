//
//  symbolTable.swift
//  JPForce
//
//  Created by 佐藤貴之 on 2024/09/30.
//

import Foundation

enum SymbolScope: String {
    case GLOBAL
    case LOCAL
}
struct Symbol : Equatable {
    let name: String
    let scope: SymbolScope
    let index: Int
}
class SymbolTable : Equatable {
    init() {}
    init(outer: SymbolTable) {
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
    func define(_ name: String) -> Symbol {
        let symbol = Symbol(
            name: name,
            scope: outer != nil ? .LOCAL : .GLOBAL,
            index: numberOfDefinitions
        )
        store[name] = symbol
        numberOfDefinitions += 1
        return symbol
    }
    func resolve(_ name: String) -> Symbol? {store[name] ?? outer?.resolve(name)}
}
