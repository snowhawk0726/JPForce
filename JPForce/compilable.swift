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
        return nil
    }
}
extension ExpressionStatement : Compilable {
    func compiled(with c: Compiler) -> JpfObject? {
        for expression in expressions {
            guard let object = expression.compiled(with: c) else {continue}
            if object.isError {return object}
            if let err = c.push(object), err.isError {return err}   // キャッシュで計算を継続
        }
        c.pullAll().forEach {$0.emit(with: c)}  // キャッシュをバイトコードに出力
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
extension DefineStatement : Compilable {
    func compiled(with c: Compiler) -> JpfObject? {
        if let object = value.compiled(with: c), object.isError {return object}
        let symbol = c.symbolTable.define(name.value)
        _ = c.emit(op: .opSetGlobal, operand: symbol.index)
        return nil
    }
}
extension Identifier : Compilable {
    func compiled(with c: Compiler) -> JpfObject? {
        guard let symbol = c.symbolTable.resolve(value) else {
            return JpfError("『\(value)』") + identifierNotFound
        }
        _ = c.emit(op: .opGetGlobal, operand: symbol.index)
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
            let opJumpPosition = (alternative != nil) ? c.emit(op: .opJump, operand: 9999) : -1
            c.changeOperand(at: opJumpNotTruthyPosition, operand: c.lastPosition)   // Jump先書換え
            if let alternative = alternative {
                if let err = alternative.compiled(with: c) {return err}
                c.changeOperand(at: opJumpPosition, operand: c.lastPosition)        // Jump先書換え
            }
            return nil
        }
        return evaluated(with: c.environment)
    }
}
extension GenitiveExpression : Compilable {
    /// 属格：<オブジェクト>の<オブジェクト>(は、<値>。)を評価/コンパイルする。
    /// - Parameter c: コンパイラ
    /// - Returns: 評価結果
    func compiled(with c: Compiler) -> JpfObject? {
        switch right {
        case is Identifier:
            if let err = left.compiled(with: c) {return err}
            if let err = right.compiled(with: c) {return err}
            switch left {
            case is ArrayLiteral, is DictionaryLiteral, is GenitiveExpression:
                _ = c.emit(op: .opIndex)
                return nil
            default:
                break
            }
        case is PredicateExpression:
            if left is Identifier {
                if let err = left.compiled(with: c) {return err}
                if let err = right.compiled(with: c) {return err}
                return nil
            }
        default:
            break
        }
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
extension StringLiteral : Compilable {
    func compiled(with c: Compiler) -> JpfObject? {
        JpfString(value: value)
    }
}
extension ArrayLiteral : Compilable {
    func compiled(with c: Compiler) -> JpfObject? {
        for element in elements {   // 要素を順に出力
            if let result = element.compiled(with: c), result.isError {return result}
        }
        _ = c.emit(op: .opArray, operand: elements.count)
        return nil
    }
}
extension DictionaryLiteral : Compilable {
    func compiled(with c: Compiler) -> JpfObject? {
        for e in pairs {            // キー、値の順にpairを出力
            if let result = e.pair.key.compiled(with: c), result.isError {return result}
            if let result = e.pair.value.compiled(with: c), result.isError {return result}
        }
        _ = c.emit(op: .opDictionary, operand: pairs.count * 2)
        return nil
    }
}
