//
//  symbolTable.swift
//  JPForce
//
//  Created by 佐藤貴之 on 2024/09/30.
//

import Foundation

enum SymbolScope: String {
    case GLOBAL
}
struct Symbol : Equatable {
    let name: String
    let scope: SymbolScope
    let index: Int
}
class SymbolTable {
    private var store: [String: Symbol] = [:]
    private var numberOfDefinitions = 0
    //
    func define(_ name: String) -> Symbol {
        let symbol = Symbol(name: name, scope: .GLOBAL, index: numberOfDefinitions)
        store[name] = symbol
        numberOfDefinitions += 1
        return symbol
    }
    func resolve(_ name: String) -> Symbol? {store[name]}
}
