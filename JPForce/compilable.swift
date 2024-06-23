//
//  compilable.swift
//  JPForce
//
//  Created by 佐藤貴之 on 2024/06/20.
//

import Foundation

// MARK: - intefaces
protocol Compilable {
    /// ASTノードの翻訳を行う。
    /// - Parameter c: Compilerのインスタンス
    /// - Returns: 正常でnil、エラー時はJpfErrorオブジェクトを返す。
    func compiled(with c: Compiler) -> JpfError?
}
// MARK: - implementations for ast mode compiler
extension Node {
    func compiled(with c: Compiler) -> JpfError? {
        return self.string + "(\(type(of: self)))" + notImplementedError
    }
}
extension Program : Compilable {
    func compiled(with c: Compiler) -> JpfError? {
        for statement in statements {
            if let error = statement.compiled(with: c) {return error}
        }
        return nil
    }
}
extension ExpressionStatement : Compilable {
    func compiled(with c: Compiler) -> JpfError? {
        for expression in expressions {
            if let error = expression.compiled(with: c) {return error}
        }
        return nil
    }
}
extension PredicateExpression : Compilable {
    func compiled(with c: Compiler) -> JpfError? {
        _ = c.emit(op: .opAdd)
        return nil
    }
}
extension PhraseExpression : Compilable {
    func compiled(with c: Compiler) -> JpfError? {
        if let error = left.compiled(with: c) {return error}
        return nil
    }
}
extension IntegerLiteral : Compilable {
    func compiled(with c: Compiler) -> JpfError? {
        let integer = JpfInteger(value: value)
        _ = c.emit(op: .opConstant, operand: c.addConstant(integer))
        return nil
    }
}
