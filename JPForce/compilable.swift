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
        c.switchCase.enter()
        for expression in expressions {
            guard let object = expression.compiled(with: c) else {continue}
            if object.isError {return object}
            if let err = c.push(object), err.isError {return err}   // キャッシュで計算を継続
        }
        if c.switchCase.hasJumpPositions {return switchCaseError}
        c.switchCase.leave()
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
        let symbol = c.symbolTable.define(name.value)
        if let object = value.compiled(with: c), object.isError {return object}
        if c.lastOpcode == .opConstant {
            c.setLastConstant(name: name.value)
        }
        _ = c.emit(
            op: symbol.isGlobal ? .opSetGlobal : .opSetLocal,
            operand: symbol.index
        )
        return nil
    }
}
extension Identifier : Compilable {
    func compiled(with c: Compiler) -> JpfObject? {
        guard let symbol = c.symbolTable.resolve(value) else {
            return JpfError("『\(value)』") + identifierNotFound
        }
        if !c.isEmpty && symbol.isProperty {            // 属性をキャッシュでアクセス
            return evaluated(with: c.environment)
        }
        c.pullAll().forEach {$0.emit(with: c)}          // キャッシュを翻訳
        symbol.emit(with: c)
        return nil
    }
}
extension PredicateExpression : Compilable {
    func compiled(with c: Compiler) -> JpfObject? {
        if let predicate = PredicateCompilableFactory.create(from: token, with: c) {
            return predicate.compiled()                 // opPredicate以外の翻訳
        }
        if !c.isEmpty {
            guard let result = evaluated(with: c.environment) else {return nil}
            if !result.isError {
                return result
            }   // キャッシュでの計算が失敗した場合は、キャッシュを出力(実行時まで実行を先延ばし)
        }
        c.pullAll().forEach {$0.emit(with: c)}
        if let symbol = c.symbolTable.resolve(token) {
            symbol.emit(with: c)
        } else {
            fatalError("『\(token.literal)』が未登録。")
        }
        return nil
    }
}
extension PhraseExpression : Compilable {
    func compiled(with c: Compiler) -> JpfObject? {
        guard let object = left.compiled(with: c) else {    // キャッシュ無し = 翻訳済み
            let particleIndex = token.particleIndex!
            if c.lastOpcode != .opPhrase {
                _ = c.emit(op: .opPhrase, operand: particleIndex)
            } else {    // 直前が opPhrase であれば、直前は省略(格を替える)
                c.changeOperand(at: c.lastPosition!, operand: particleIndex)
            }
            return nil
        }
        if object.isError {return object}
        return JpfPhrase(value: object, particle: token)
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
        _ = c.emit(predicate: .DROP)                 // 「〜が」を捨てる
        if let err = consequence.compiled(with: c) {return err}
        let opJumpPosition = c.emit(op: .opJump, operand: 9999)
        c.switchCase.append(opJumpPosition)
        c.changeOperand(at: opJumpNotTruthyPosition, operand: c.nextPosition)   // Jump先書換え
        if let alternative {
            _ = c.emit(predicate: .DROP)             // 「〜が」を捨てる
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
        if let cash = left.compiled(with: c) {          // leftが定数
            if let symbol = c.symbolTable.resolve(right.tokenLiteral),
               symbol.isVariable {                      // rightが変数
                cash.emit(with: c)                      // キャッシュを出力
            } else if right is CaseExpression && c.isEmpty {    // 〜の場合、(switch-case)
                c.switchCase.isActive = true
                _ = c.emit(predicate: .DUPLICATE)               // スタック(〜が)をコピー
                let phrase = JpfPhrase(value: cash, particle: Token(.DE))
                phrase.emit(with: c)                            // キャッシュを出力
                _ = c.emit(predicate: .BE)                      // 「である」を出力
                return right.compiled(with: c)
            } else {
                return evaluated(with: c.environment)   // キャッシュで評価
            }
        }
        if let cash = right.compiled(with: c) {         // rightが定数
            if cash.isError {return cash}
            cash.emit(with: c)                          // キャッシュを出力
            _ = c.emit(op: .opGenitive)                 // 索引をアクセス
        } else {
            guard let symbol = c.symbolTable.resolve(right.tokenLiteral) else {
                return "『\(right.tokenLiteral)』" + identifierNotFound
            }   /* 未定義シンボルはコンパイルエラー → 来ない */
            if symbol.isVariable {                      // rightが変数
                _ = c.emit(op: .opGenitive)             // 索引をアクセス
            }
        }
        return nil
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
        evaluated(with: c.environment)
    }
}
extension DictionaryLiteral : Compilable {
    func compiled(with c: Compiler) -> JpfObject? {
        evaluated(with: c.environment)
    }
}
extension FunctionLiteral : Compilable {
    func compiled(with c: Compiler) -> JpfObject? {
        c.pullAll().forEach {$0.emit(with: c)}      // キャッシュをバイトコードに出力
        // TODO: 関数をキャッシュとして維持できないか？
        c.enterScope()
        if !name.isEmpty {
            _ = c.symbolTable.define(functionName: name)
        }
        functions.array[0].parameters.forEach {
            _ = c.symbolTable.define($0.value)
        }
        if let body = functions.array[0].body,
           let result = body.compiled(with: c) {
           if result.isError {return result}
        }
        if c.lastOpcode != .opReturnValue {
            _ = c.emit(op: .opReturn)
        }
        let freeSymbols = c.symbolTable.freeSymbols // 自由シンボルテーブル
        let numberOfLocals = c.symbolTable.numberOfDefinitions  // ローカル変数の数
        let instructions = c.leaveScope()
        freeSymbols.forEach {$0.emit(with: c)}
        let compiledFunction = JpfCompiledFunction(
                                    instructions: instructions,
                                    numberOfLocals: numberOfLocals,
                                    numberOfParameters: functions.array[0].parameters.count)
        let functionIndex = c.addConstant(compiledFunction)
        _ = c.emit(op: .opClosure, operand: functionIndex, freeSymbols.count)
        return nil
    }
}
