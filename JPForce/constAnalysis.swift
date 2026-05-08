//
//  constAnalysis.swift
//  JPForce
//
//  Created by 佐藤貴之 on 2026/04/26.
//

import Foundation

// MARK: - intefaces
protocol ConstAnalysis {
    /// ASTノードの定数分析を行う。
    /// - Parameter c: Compilerのインスタンス
    /// - Returns:
    ///   定数を返す。(定数が得られない場合は、nil)
    ///   エラーを検出した場合、JpfErrorを返す。
    func analyze(with c: Compiler) -> JpfObject?
}
// MARK: - implementations for ast constant analyzed
extension Array where Element == Expression {
    /// 複数の式（または引数）を順に翻訳する共通処理
    /// - Parameter expressions: [Expression]
    /// - Returns:
    ///   - スタックトップ(無い場合はnil)
    ///   - JpfObject : 翻訳エラー、またはスタックエラー
    func analyze(with c: Compiler) -> JpfObject? {
        for expression in self {
            guard let const = expression.analyze(with: c) else { continue }
            if const.isError { return const }
            if let err = c.push(const) { return err }
        }
        return c.pull()
    }
}
extension Node {
    func analyze(with c: Compiler) -> JpfObject? {
        return JpfError(
            "型：\(String(describing: self))「\(string)」は、定数分析不可(未実装)です。"
        )
    }
}
extension Program : ConstAnalysis {
    func analyze(with c: Compiler) -> JpfObject? {
        for statement in statements {
            guard let const = statement.analyze(with: c) else { continue }
            if const.isError { return const }
            if let err = c.push(const) { return err }
        }
        return c.pull()
    }
}
extension CompoundStatement : ConstAnalysis {
    func analyze(with c: Compiler) -> JpfObject? {
        for sentence in self.sentences {
            guard let const = sentence.analyze(with: c) else { continue }
            if const.isError { return const }
            if let err = c.push(const) { return err }
        }
        return c.pull()
    }
}
extension BlockStatement : ConstAnalysis {
    func analyze(with c: Compiler) -> JpfObject? {
        for statement in self.statements {
            guard let const = statement.analyze(with: c) else { continue }
            if const.isError { return const }
            if let err = c.push(const) { return err }
        }
        return c.pull()
    }
}
extension SimpleSentence : ConstAnalysis {
    func analyze(with c: Compiler) -> JpfObject? {
        if let const = evaluate(with: c.environment),
           !const.isError {
            return const
        }
        return nil
    }
}
extension DefineStatement : ConstAnalysis {
    func analyze(with c: Compiler) -> JpfObject? {
        _ = c.symbolTable.define(name.value)        // 識別子をシンボルテーブルに登録
        // 右辺
        let const = value.analyze(with: c)
        if let const, const.isError { return const }
        c.environment[name.value] = const           // 定数を登録(または抹消)
        return nil
    }
}
extension ExpressionStatement : ConstAnalysis {
    func analyze(with c: Compiler) -> JpfObject? {
        expressions.analyze(with: c)
    }
}
extension IntegerLiteral : ConstAnalysis {
    func analyze(with c: Compiler) -> JpfObject? {
        JpfInteger(value: value)
    }
}
extension StringLiteral : ConstAnalysis {
    func analyze(with c: Compiler) -> JpfObject? {
        JpfString(value: value)
    }
}
extension Boolean : ConstAnalysis {
    func analyze(with c: Compiler) -> JpfObject? {
        JpfBoolean(value: value)
    }
}
extension RangeLiteral : ConstAnalysis {
    func analyze(with c: Compiler) -> JpfObject? {
         if let range = evaluate(with: c.environment) {
             if range.isError { return nil }        // 定数取得失敗
             return range
        }
        assertionFailure("内部エラー：RangeLiteral#analyze")
        return nil
    }
}
extension Identifier : ConstAnalysis {
    func analyze(with c: Compiler) -> JpfObject? {
        evaluate(with: c.environment)
    }
}
extension OrExpression : ConstAnalysis {
    func analyze(with c: Compiler) -> JpfObject? {
        if let result = evaluate(with: c.environment) {
            if result.isError { return nil }
            return result
        }
        assertionFailure("内部エラー：OrExpression#analyze")
        return nil
    }
}
