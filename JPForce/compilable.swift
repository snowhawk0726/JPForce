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
    /// - Returns:
    ///   翻訳済みの場合、nilを返す。
    ///   エラーを検出した場合、JpfErrorを返す。
    ///   キャッシュによる演算が継続可能な場合、値(JpfObject)を出力する。
    func compiled(with c: Compiler) -> JpfObject?
}
// MARK: - implementations for ast mode compiler
extension Node {
    func compiled(with c: Compiler) -> JpfObject? {
        return JpfError("\(type(of: self))「\(self.string)」は、翻訳不可(未実装)")
    }
}
extension Program : Compilable {
    func compiled(with c: Compiler) -> JpfObject? {
        for statement in statements {
            if let object = statement.compiled(with: c), object.isError {return object}
        }
        _ = c.emit(op: .opPop)  // TODO: 必要？
        return nil
    }
}
extension BlockStatement : Compilable {
    func compiled(with c: Compiler) -> JpfObject? {
        var result: JpfObject?
        for statement in statements {
            result = statement.compiled(with: c)
            if let object = result, object.isBreakFactor {break}
        }
        return result
    }
}
extension ExpressionStatement : Compilable {
    func compiled(with c: Compiler) -> JpfObject? {
        for expression in expressions {
            guard let object = expression.compiled(with: c) else {continue}
            if object.isError {return object}
            c.push(object)                      // キャッシュで計算を継続
        }
        c.pullAll().forEach {$0.emit(with: c)}  // キャッシュをバイトコードに出力
        return nil
    }
}
extension PredicateExpression : Compilable {
    func compiled(with c: Compiler) -> JpfObject? {
        guard let predicate = PredicateCompilableFactory.create(from: token, with: c) else {return predicateNotSupported + "(述語：\(token.literal))"}
        return predicate.compiled()
    }
}
extension PhraseExpression : Compilable {
    func compiled(with c: Compiler) -> JpfObject? {
        guard let object = left.compiled(with: c) else {return nil}
        if object.isError {return object}
        return JpfPhrase(value: object, particle: token)
    }
}
extension CaseExpression : Compilable {
    /// 条件処理(場合分け)。
    /// 形式１：　(条件)場合、【処理】(、それ以外は、【処理】)
    /// 形式２：   (〜が、〜の)場合、【処理】(、(〜の)場合、【処理】...)(、それ以外は、【処理】)
    /// - Returns: ReturnValueまたはnil、エラー
    func compiled(with c: Compiler) -> JpfObject? {
        if c.isEmpty {
            let opJumpNotTruthyPosition = c.emit(op: .opJumpNotTruthy, operand: 9999)
            if let err = consequence.compiled(with: c) {return err}
            let opJumpPosition = c.emit(op: .opJump, operand: 9999)
            let afterConsequencePositon = c.instructions.count
            c.changeOperand(at: opJumpNotTruthyPosition, operand: afterConsequencePositon)
            if alternative == nil {
                _ = c.emit(op: .opNull)
            } else {
                if let err = alternative!.compiled(with: c) {return err}
            }
            let afterAlternativePosition = c.instructions.count
            c.changeOperand(at: opJumpPosition, operand: afterAlternativePosition)
            return nil
        }
        if let result = evaluated(with: c.environment) {return result}
        _ = c.emit(op: .opNull)
        return nil
    }
}
extension GenitiveExpression : Compilable {
    /// 属格：<オブジェクト>の<オブジェクト>(は、<値>。)を評価する。
    /// - Parameter environment: 入力の環境
    /// - Returns: 評価結果
    func compiled(with c: Compiler) -> JpfObject? {
        return evaluated(with: c.environment)
    }
}
extension IntegerLiteral : Compilable {
    func compiled(with c: Compiler) -> JpfObject? {
        JpfInteger(value: value)
    }
}
extension Boolean : Compilable {
    func compiled(with c: Compiler) -> JpfObject? {
        JpfBoolean(value: value)
    }
}
