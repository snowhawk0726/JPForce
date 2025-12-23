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
        return notImplemented(type: String(describing: self.self), description: self.string)
    }
    // 翻訳エラー
    func notImplemented(type: String, description: String) -> JpfError {
        JpfError("型：\(type)「\(description)」は、翻訳不可(未実装)")
    }
}
extension Program : Compilable {
    func compiled(with c: Compiler) -> JpfObject? {
        c.switchCase.enter()
        for statement in statements {
            if let object = statement.compiled(with: c), object.isError {return object}
        }
        if c.switchCase.hasJumpPositions {return JpfError(c.switchCase.defaultError)}
        c.switchCase.leave()
        return nil
    }
}
extension ExpressionStatement : Compilable {
    func compiled(with c: Compiler) -> JpfObject? {
        casheLeadingIdentifer(with: c)                              // 文頭の識別子をキャッシュ
        for expression in expressions {
            guard let object = expression.compiled(with: c) else {continue}
            if object.isError {return object}
            if let err = c.push(object), err.isError {return err}   // キャッシュで計算を継続
        }
        c.identifier = nil
        do {try c.emitAllCashe()} catch {return jpfError(from: error)}
        return nil
    }
}
private extension ExpressionStatement {
    /// 文頭の識別子をキャッシュする
    func casheLeadingIdentifer(with c: Compiler) {
        if let phrase = expressions.first as? PhraseExpression,
           let ident = phrase.left as? Identifier {
                c.identifier = ident.value
        }
    }
}
extension BlockStatement : Compilable {
    func compiled(with c: Compiler) -> JpfObject? {
        c.switchCase.enter()
        var result: JpfObject?
        for statement in statements {
            result = statement.compiled(with: c)
            if let object = result, object.isBreakFactor {break}
        }
        if c.switchCase.hasJumpPositions {return JpfError(c.switchCase.defaultError)}
        c.switchCase.leave()
        return result
    }
}
extension DefineStatement : Compilable {
    func compiled(with c: Compiler) -> JpfObject? {
        let symbol = c.symbolTable.define(name.value)
        if let object = value.compiled(with: c), object.isError {return object}
        if c.lastOpcode == .opConstant {
            c.setLastConstant(name: name.value)
        }
        symbol.emitOpSet(with: c)
        return nil
    }
}
extension Identifier : Compilable {
    func compiled(with c: Compiler) -> JpfObject? {
        let ident = JpfIdentifier(resolving: self, with: c)
        guard ident.hasSymbol || ident.isLhs else {     // 登録済み、または左辺識別子
            return undefinedIdentifier(value)
        }
        if !c.isEmpty, ident.isProperty  {
            if let cashe = c.peek as? JpfIdentifier {
                // キャッシュを識別子(属性)でアクセス
                c.drop()
                do {
                    try cashe.emit(with: c)             // opGetXXX
                    try ident.emit(with: c)             // opGetProperty
                } catch {
                    return jpfError(from: error)
                }
                return nil
            }
            return evaluated(with: c.environment)
        }
        return ident
    }
}
extension PredicateExpression : Compilable {
    func compiled(with c: Compiler) -> JpfObject? {
        if let predicate = PredicateCompilableFactory.create(from: token, with: c) {
            return predicate.compiled()                 // opPredicate以外の翻訳
        }
        if !c.isEmpty && !c.hasIdentInCashe {
            guard let result = evaluated(with: c.environment) else {return nil}
            if !result.isError {
                return result
            }   // キャッシュでの計算が失敗した場合は、キャッシュを出力(実行時まで実行を先延ばし)
        }
        do {try c.emitAllCashe()} catch {return jpfError(from: error)}
        return c.emit(predicate: token)
    }
}
extension PhraseExpression : Compilable {
    func compiled(with c: Compiler) -> JpfObject? {
        switch left.compiled(with: c) {
        case let ident as JpfIdentifier:
            return JpfPhrase(value: ident, particle: token)
        case let err as JpfError:
            return err
        case let cashe?:
            // キャッシュとして出力
            return JpfPhrase(value: cashe, particle: token)
        default:
            break
        }
        // キャッシュ無し = 翻訳済み
        let particleIndex = token.particleIndex!
        if c.lastOpcode != .opPhrase {
            _ = c.emit(op: .opPhrase, operand: particleIndex)
        } else {    // 直前が opPhrase であれば、直前は省略(格を替える)
            c.changeOperand(at: c.lastPosition!, operand: particleIndex)
        }
        return nil
    }
}
extension CaseExpression : Compilable {
    /// 条件処理(場合分け)。
    /// - Returns: ReturnValueまたはnil、エラー
    func compiled(with c: Compiler) -> JpfObject? {
        guard c.isEmpty else {
            return evaluated(with: c.environment)
        }
        return c.switchCase.isActive ? switchCaseCompiled(with: c) : ifThenCompiled(with: c)
    }
    /// 形式１：　(条件)場合、【処理】(、それ以外は、【処理】)
    private func ifThenCompiled(with c: Compiler) -> JpfObject? {
        let opJumpNotTruthyPosition = c.emit(op: .opJumpNotTruthy, operand: 9999)
        if let err = consequence.compiled(with: c) {return err}
        let opJumpPosition = alternative != nil ? c.emit(op: .opJump, operand: 9999) : -1
        c.changeOperand(at: opJumpNotTruthyPosition, operand: c.nextPosition)   // Jump先書換え
        if let alternative {
            if let err = alternative.compiled(with: c) {return err}
            c.changeOperand(at: opJumpPosition, operand: c.nextPosition)
        }
        return nil
    }
    /// 形式２：   〜が、〜の場合、【処理】(、〜の場合、【処理】...)、それ以外は、【処理】
    /// *: 「〜が」をスタックに積んでいる
    private func switchCaseCompiled(with c: Compiler) -> JpfObject? {
        let opJumpNotTruthyPosition = c.emit(op: .opJumpNotTruthy, operand: 9999)
        _ = c.emit(op: .opDropConst, operand: 1)                                // 「〜が」を捨てる
        if let err = consequence.compiled(with: c) {return err}
        let opJumpPosition = c.emit(op: .opJump, operand: 9999)
        c.switchCase.append(opJumpPosition)
        c.changeOperand(at: opJumpNotTruthyPosition, operand: c.nextPosition)   // Jump先書換え
        if let alternative {
            _ = c.emit(op: .opDropConst, operand: 1)                                // 「〜が」を捨てる
            if let err = alternative.compiled(with: c) {return err}
            c.switchCase.jumpPositions?.forEach { position in
                c.changeOperand(at: position, operand: c.nextPosition)          // Jump先書換え
            }
            c.switchCase.isActive = false
        }
        return nil
    }
}
extension GenitiveExpression : Compilable {
    /// 属格：<オブジェクト>の<オブジェクト>(は、<値>。)を評価/コンパイルする。
    /// - Parameter c: コンパイラ
    /// - Returns: 評価結果
    func compiled(with c: Compiler) -> JpfObject? {
        if let err = compileLeft(with: c) {     // 左項コンパイル
            return err
        }
        return compileRight(with: c)            // 右項コンパイル
    }
}
private extension GenitiveExpression {
    /// 左項を翻訳
    /// - Parameter c: コンパイラ
    /// - Returns: nil: 翻訳継続、それ以外は、エラー
    func compileLeft(with c: Compiler) -> JpfObject? {
        switch left.compiled(with: c) {
        case let ident as JpfIdentifier:
            return c.push(ident)        // 変数の翻訳を先送り
        case let err as JpfError:
            return err
        case let cashe?:                // 計算済み右項
            return c.push(cashe)        // 翻訳継続
        default:
            break
        }
        return nil
    }
    /// 右項を翻訳
    /// - Parameter c: コンパイラ
    /// - Returns: nil: 翻訳完了、それ以外はエラーもしくは評価結果(評価を継続)
    func compileRight(with c: Compiler) -> JpfObject? {
        if right is CaseExpression {                    // 〜の場合、(switch-case)
            return compileCaseExpression(with: c)
        }
        if right is Identifier {                        // 〜の<識別子>
            return compileIdentifier(with: c)
        }
        if right is PhraseExpression {                  // 〜の<句>
            return compilePhraseExpression(with: c)
        }
        // 左項チェック
        do {
            switch c.pull() {
            case let ident as JpfIdentifier:                // 左項が変数
                try emitGenitiveAccess(from: ident, with: c)
                return nil
            case nil:                                       // 左項は翻訳済み
                try emitGenitiveAccess(with: c)
                return nil
            default:
                return evaluated(with: c.environment)       // キャッシュで評価
            }
        } catch {return jpfError(from: error)}
    }
    
    /// 右項の変数を翻訳
    /// 翻訳結果が定数であれば、評価継続。変数であれば、属格アクセスコードを出力
    private func compileIdentifier(with c: Compiler) -> JpfObject? {
        switch right.compiled(with: c) {
        case let ident as JpfIdentifier:
            do {
                try c.emitAllCashe()
                guard ident.hasSymbol else {
                    return "属格の右項" + undefinedIdentifier(ident.value)
                }
                try ident.emit(with: c)
                if ident.isVariable {
                    _ = c.emit(op: .opGenitive)             // 右項変数で索引アクセス
                }
            } catch {return jpfError(from: error)}
            return nil
        case let err as JpfError:
            return err
        case let const?:                                    // 定数の属性 → 定数
            return const
        case nil:
            return nil
        }
    }
    /// 右項の場合文(〜の場合)を翻訳
    /// 両辺が定数の場合、評価、それ以外はコードを出力
    private func compileCaseExpression(with c: Compiler) -> JpfObject? {
        switch normalizedParams(from: c) {
        case (_ as JpfIdentifier, _), (_, _ as JpfIdentifier):
            break                                       // 変数は、emit
        case (_?, _?):                                  // 「〜が」がキャッシュにある
            c.drop()                                    // 「〜の」を捨てる
            return evaluated(with: c.environment)       // キャッシュで評価
        case (nil, _?):
            break
        default:
            return JpfError("場合文の翻訳ができない。")
        }
        c.switchCase.isActive = true
        do {
            try emitCaseCode(with: c)                   // 「(〜が)〜である場合」を出力
        } catch {
            return jpfError(from: error)
        }
        return right.compiled(with: c)
    }
    /// 右項の句を翻訳
    /// 左項を右項で属格アクセスし、格をつけるコードを出力
    private func compilePhraseExpression(with c: Compiler) -> JpfObject? {
        switch right.compiled(with: c) {
        case let rightPhrase as JpfPhrase:
            do {
                try emitGenitiveAccess(from: rightPhrase, with: c)
            } catch {
                return jpfError(from: error)
            }
        default:
            break
        }
        return nil
    }
    /// コードを出力
    /// 「(〜が)〜の場合」(case)を「(〜が)〜である場合」としてコードを出力
    private func emitCaseCode(with c: Compiler) throws {
        let phrase = JpfPhrase(value: c.pull(), particle: Token(.DE))
        if let left = c.pull() {
            try left.emit(with: c)                          // 「<オブジェクト>が」を出力
        }
        _ = c.emit(op: .opDuplicateConst, operand: 1)       // 実行スタックの「〜が」をコピー
        try phrase.emit(with: c)                            // 「<値>で」を出力
        _ = c.emit(predicate: .BE)                          // 「ある」を出力
    }
    /// 句の値で属格アクセスを行い、元の格で句を再構築
    private func emitGenitiveAccess(from phrase: JpfPhrase, with c: Compiler) throws {
        try c.emitAllCashe()
        guard let right = phrase.value, let particle = phrase.particle else {
            throw JpfError("属格の右項翻訳で、句の値または格が無い。")
        }
        try right.emit(with: c)
        _ = c.emit(op: .opGenitive)
        _ = c.emit(particle: particle)
        return
    }
    /// 左項と右項で属格アクセス
    private func emitGenitiveAccess(from left: JpfObject, with c: Compiler) throws {
        try left.emit(with: c)
        try emitGenitiveAccess(with: c)
    }
    /// (翻訳済み)左項と右項で属格アクセス
    private func emitGenitiveAccess(with c: Compiler) throws {
        if let value = right.compiled(with: c) {
            try value.emit(with: c)
            _ = c.emit(op: .opGenitive)
        }
    }
    ///
    /// スタックの引数を正規化する。(２個、アンラップした値を後詰めにする。)
    private func normalizedParams(from stack: Compiler) -> (JpfObject?, JpfObject?) {
        let params = stack.getAll()
        switch params.count {
        case 0: return (nil, nil)
        case 1: return (nil, params[0].value)
        default:
            return (params[params.count-2].value, params[params.count-1].value)
        }
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
        if let result = evaluated(with: c.environment) {
            if !result.isError {return result}  // 正常: JpfObject(JpfArray)に変換
        }
        for exps in elements {
            if let err = exps.compiled(with: c), err.isError {return err}
        }
        _ = c.emit(op: .opArrayConst, operand: elements.count)
        return nil
    }
}
extension DictionaryLiteral : Compilable {
    func compiled(with c: Compiler) -> JpfObject? {
        if let result = evaluated(with: c.environment) {
            if !result.isError {return result}  // 正常: JpfObject(JpfDictionary)に変換
        }
        for pair in pairs {
            if let err = pair.compiled(with: c), err.isError {return err}
        }
        _ = c.emit(op: .opDictionaryConst, operand: pairs.count * 2)
        return nil
    }
}
extension PairExpression : Compilable {
    func compiled(with c: Compiler) -> JpfObject? {
        if let err = pair.key.compiled(with: c) {return err}
        if let err = pair.value.compiled(with: c) {return err}
        return nil
    }
}
extension FunctionLiteral : Compilable {
    func compiled(with c: Compiler) -> JpfObject? {
        do {try c.emitAllCashe()} catch {return jpfError(from: error)}  // キャッシュをバイトコードに出力
        c.enterScope()
        if !name.isEmpty {
            _ = c.symbolTable.define(functionName: name)
        }
        function.parameters.forEach {
            _ = c.symbolTable.define($0.value)
        }
        if let body = function.body,
           let result = body.compiled(with: c) {
           if result.isError {return result}
        }
        if c.lastOpcode != .opReturnValue {
            _ = c.emit(op: .opReturn)
        }
        let freeSymbols = c.symbolTable.freeSymbols // 自由シンボルテーブル
        let numberOfLocals = c.symbolTable.numberOfDefinitions  // ローカル変数の数
        let instructions = c.leaveScope()
        freeSymbols.forEach {$0.emitOpGet(with: c)}
        let compiledFunction = JpfCompiledFunction(
                                    instructions: instructions,
                                    numberOfLocals: numberOfLocals,
                                    numberOfParameters: function.parameters.count)
        let functionIndex = c.addConstant(compiledFunction)
        _ = c.emit(op: .opClosure, operand: functionIndex, freeSymbols.count)
        return nil
    }
}
