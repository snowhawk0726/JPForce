//
//  compilerObjectsExtension.swift
//  コンパイラ、VM用JpfObjectのextension
//  JPForce
//
//  Created by 佐藤貴之 on 2025/12/08.
//

import Foundation

extension JpfObject {
    func emit(with c: Compiler) throws {throw JpfError("型「\(self.type)」の翻訳で、コードを出力する方法が未実装")}
    var isDefined: Bool {true}
    var formattedString: String {String(format: "%@(%@)", string, type)}
}
extension JpfInteger {
    func emit(with c: Compiler) throws {
        _ = c.emit(op: .opConstant, operand: c.addConstant(self))
    }
}
extension JpfBoolean {
    func emit(with c: Compiler) throws {
        _ = c.emit(op: isTrue ? .opTrue : .opFalse)
    }
}
extension JpfString {
    func emit(with c: Compiler) throws {
        _ = c.emit(op: .opConstant, operand: c.addConstant(self))
    }
}
extension JpfIdentifier {
    // 初期化
    init(resolving ident: Identifier, with c: Compiler) {
        self.init(resolving: ident.value, with: c, isLhs: ident.isLhs)
    }
    init(resolving name: String, with c: Compiler, isLhs: Bool = false) {
        self.name = name
        self.value = name
        self.isLhs = isLhs
        // 原形 → 終止形で、名前解決
        if let symbol = c.symbolTable.resolve(value) {
            self.symbol = symbol
            return
        }
        if let plain = ContinuativeForm(value).plainForm,
           let symbol = c.symbolTable.resolve(plain) {
            self.value = plain
            self.symbol = symbol
            return
        }
    }
    init(ensuring name: String, with c: Compiler) {
        self.init(resolving: name, with: c)
        if self.symbol == nil {
            self.symbol = c.symbolTable.define(name)
        }
    }
    init(ensuring object: JpfObject?, with c: Compiler) throws {
        guard let target = object?.value as? JpfIdentifier else {
            throw JpfError("objectから、識別子名を取得できなかった。")
        }
        self.init(ensuring: target.name, with: c)
    }
    // コンパイル
    func emit(with c: Compiler) throws {
        guard let symbol = self.symbol else {
            throw undefinedIdentifier(value)
        }
        symbol.emitOpGet(with: c)
    }
    func emitOpSet(with c: Compiler) throws {
        guard let symbol = self.symbol else {
            assertionFailure("識別子をシンボルテーブルに登録し忘れている。")
            throw undefinedIdentifier(value)
        }
        symbol.emitOpSet(with: c)
    }
    // ヘルパー(シンボル)
    mutating func defineSymbol(with c: Compiler) {
        self.symbol = c.symbolTable.define(value)
    }
    var isDefined: Bool {return hasSymbol}
    var hasSymbol: Bool {self.symbol != nil}
    var isProperty: Bool {symbol?.scope == .PROPETRY}
    var isVariable: Bool {symbol?.isVariable ?? false}
}
extension JpfNull {
    func emit(with c: Compiler) throws {_ = c.emit(op: .opNull)}
}
extension JpfPhrase {
    func emit(with c: Compiler) throws {
        if let ident = value as? JpfIdentifier {
            if !ident.hasSymbol {
                // 未定義の(文頭)識別子をシンボルテーブルに登録
                let _ = c.symbolTable.define(ident.value)
                return  // 「代入」で当該識別子に代入するので、コードは出力しない
            }
            // 識別子は既存なので、opGetXXXを出力
            try ident.emit(with: c)
            if let idx = particle?.particleIndex {
                _ = c.emit(op: .opPhrase, operand: idx)
            }
            return
        }
        _ = c.emit(op: .opConstant, operand: c.addConstant(self))
    }
    var isDefined: Bool {
        (value as? JpfIdentifier)?.hasSymbol ?? false
    }
}
extension JpfReturnValue {
    func emit(with c: Compiler) throws {
        try value?.emit(with: c)
        _ = c.emit(op: value != nil ? .opReturnValue : .opReturn)
    }
}
extension JpfArray {
    func emit(with c: Compiler) throws {
        try elements.forEach {try $0.emit(with: c)}
        _ = c.emit(op: .opArrayConst, operand: elements.count)
    }
}
extension JpfDictionary {
    func emit(with c: Compiler) throws {
#if DEBUG   // UnitTestのために、要素の順序正を保つ
        try pairs.values.sorted(by: {$0.key.string < $1.key.string}).forEach {
            try $0.key.emit(with: c)
            try $0.value.emit(with: c)
        }
        _ = c.emit(op: .opDictionaryConst, operand: pairs.count * 2)
#else
        pairs.values.forEach {
            $0.key.emit(with: c)
            $0.value.emit(with: c)
        }
        _ = c.emit(op: .opDictionary, operand: pairs.count * 2)
#endif
    }
}
// Token level emits
extension Token {
    func emitConst(with c: Compiler, operand: Int) {
        _ = c.emit(op: opConst, operand: operand)
    }
    func emitStack(with c: Compiler) {
        _ = c.emit(op: opStack)
    }
    /// 即値オペランドを持つ命令語
    private var opConst: Opcode {
        switch keyword {
        case .DUPLICATE:    return .opDuplicateConst
        case .PULL:         return .opPullConst
        default:
            fatalError("emit not supported for \(self)")
        }
    }
    /// スタック値を取る命令語
    private var opStack: Opcode {
        switch keyword {
        case .DUPLICATE:    return .opDuplicate
        case .PULL:         return .opPull
        default:
            fatalError("emit not supported for \(self)")
        }
    }
}
