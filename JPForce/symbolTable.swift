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
enum SymbolKind {
    case unknown
    case constant
    case function
}
struct Symbol : Equatable {
    let name: String
    let scope: SymbolScope
    let index: Int
    let isRedefined: Bool
    let kind: SymbolKind
    //
    init(name: String, scope: SymbolScope, index: Int, isRedefined: Bool = false, kind: SymbolKind = .unknown) {
        self.name = name
        self.scope = scope
        self.index = index
        self.isRedefined = isRedefined
        self.kind = kind
    }
    //
    var isProperty: Bool {scope == .PROPETRY}
    var isPredicate: Bool {scope == .PREDICATE}
    var isVariable: Bool {scope == .LOCAL || scope == .GLOBAL || scope == .FREE}
    var isGlobal: Bool {scope == .GLOBAL}
    var isLocal: Bool {scope == .LOCAL}
    var isFree: Bool {scope == .FREE}
    var isConstant: Bool {kind == .constant}
    /// シンボルに応じた命令語を出力する。
    func emitOpGet(with c: Compiler) {
        _ = c.emit(
            op: scope.opCode, operand: index)
    }
    func emitOpSet(with c: Compiler) {
        _ = c.emit(
            op: self.isGlobal ? .opSetGlobal : .opSetLocal,
            operand: self.index
        )
    }
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
    private var userDefinedSymbols: [String: Symbol] = [:]
    private var builtinSymbols: [String: Symbol] = [:]
    var numberOfDefinitions = 0
    var freeSymbols: [Symbol] = []
    //
    static func == (lhs: SymbolTable, rhs: SymbolTable) -> Bool {
        lhs.userDefinedSymbols == rhs.userDefinedSymbols
        && lhs.builtinSymbols == rhs.builtinSymbols
        && lhs.numberOfDefinitions == rhs.numberOfDefinitions
    }
    subscript(index: Int) -> String? {  // シンボルテーブルから識別子名を取得
        userDefinedSymbols.first(where: {$0.value.isVariable && $0.value.index == index})?.key
    }
    // シンボル定義
    func define(name: String, index: Int, scope: SymbolScope, kind: SymbolKind = .unknown) -> Symbol {
        var symbol: Symbol
        if scope == .PREDICATE || scope == .PROPETRY {
            symbol = Symbol(name: name, scope: scope, index: index)
            builtinSymbols[name] = symbol
        } else {
            symbol = Symbol(
                name: name,
                scope: scope,
                index: index,
                isRedefined: Token.isRedefinableKeyword(name),
                kind: kind
            )
            userDefinedSymbols[name] = symbol
        }
        return symbol
    }
    func define(_ name: String, kind: SymbolKind = .unknown) -> Symbol {
        let symbol = define(
            name: name,
            index: numberOfDefinitions,
            scope: outer != nil ? .LOCAL : .GLOBAL,
            kind: kind
        )
        numberOfDefinitions += 1
        return symbol
    }
    func define(free orignal: Symbol) -> Symbol {
        freeSymbols.append(orignal)
        return define(name: orignal.name, index: freeSymbols.count - 1, scope: .FREE)
    }
    func define(functionName: String) -> Symbol {
        define(name: functionName, index: 0, scope: .FUNCTION, kind: .function)
    }
    // シンボルの名前解決
    func resolve(_ name: String) -> Symbol? {
        if let symbol = userDefinedSymbols[name] {
            return symbol
        }
        if let symbol = builtinSymbols[name] {
            return symbol
        }
        // 外側で解決
        guard let symbol = outer?.resolve(name) else {return nil}
        return (symbol.isFree || symbol.isLocal) ? define(free: symbol) : symbol
    }
    func resolve(property: String) -> Symbol? {
        builtinSymbols[property]
    }
    func resolve(_ token: Token) -> Symbol? {
        if token.isExplicit {
            return builtinSymbols[token.unwrappedLiteral]
        }
        return resolve(token.unwrappedLiteral)
    }
    func hasSymbol(name: String) -> Bool {
        resolve(name) != nil
    }
    func hasRedefined(_ token: Token) -> Bool {
        guard let symbol = resolve(token) else {return false}
        return symbol.isRedefined
    }
}
