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
            c.push(object)                      // キャッシュで計算を継続
        }
        if let object = c.pull() {
            object.emit(with: c)
        }
        _ = c.emit(op: .opPop)
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
extension GenitiveExpression : Compilable {
    func compiled(with c: Compiler) -> JpfObject? {
        guard let object = left.compiled(with: c) else {return nil}
        guard !object.isError else {return object}
        if let phrase = right as? PhraseExpression, phrase.token.isParticle(.WA),
           let value = value?.compiled(with: c) {
            let element = getElement(from: phrase.left, with: c)
            let result = object.assign(value, to: element)              // 値を要素に代入
            guard !result.isError else {return result}
            return nil    // c.assign(result, with: object.name)        // 結果を辞書に登録
        }
        return compiled(object, self.right, with: c)
    }
    /// 属格の右を確認し、<object>の<right>を処理する。
    /// - Parameters:
    ///   - object: 左側のオブジェクト
    ///   - right:  右側の式(ASTノード)
    /// - Parameter compiler: 入力の環境
    /// - Returns: 評価結果
    private func compiled(_ object: JpfObject, _ right: Expression, with compiler: Compiler) -> JpfObject? {
        var accessor: JpfObject? = nil
        let phrase = JpfPhrase(name: object.name, value: object, particle: token)
        switch right {
        case let ident as Identifier:
            compiler.push(phrase)
            accessor = ident.compiled(with: compiler)
            if compiler.peek?.name == object.name && compiler.peek?.particle == Token(.NO) {
                compiler.drop()
            } else {                // objectをsubsciptでアクセス済み
                return accessor
            }
//        case is ComputationLiteral:
//            compiler.push(phrase)
//            if let c = right.compiled(with: compiler) as? JpfComputation {
//                return compiler.isExecutable ? c.getter(with: compiler) : c
//            }
//            return nil
        case let expression as PhraseExpression:
            let object = compiled(object, expression.left, with: compiler)
            return JpfPhrase(name: "", value: object, particle: expression.token)
        case is PredicateExpression, is CaseExpression, is Label:
            compiler.push(phrase)
            return right.compiled(with: compiler)
        default:
            accessor = right.compiled(with: compiler)
        }
        return object   // .accessed(by: accessor!, with: environment)
    }
    /// オブジェクトの要素を取り出す。
    /// - Parameters:
    ///   - expression: 要素を表す式
    ///   - compiler: 要素を含む環境
    /// - Returns: 要素オブジェクト、または識別子名のオブジェクト
    private func getElement(from expression: Expression, with compiler: Compiler) -> JpfObject? {
//        if let ident = expression as? Identifier {
//            if let object = compiler[ident.value] {return object}
//            return JpfString(value: ident.value)
//        }
        return expression.compiled(with: compiler)
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
