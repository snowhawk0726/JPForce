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
    ///   キャッシュによる演算が継続可能な場合、値(JpfObject)を出力する、
    ///   または、スタック(c.emvirionment.stack)に値をpushする。
    func compile(with c: Compiler) -> JpfObject?
}
// MARK: - implementations for ast node compiler
extension Compilable {
    // compileの結果得られた定数をemitする。
    // 定数がエラー、またはemitが失敗したら、JpfErrorを返す。
    func emit(_ const: JpfObject?, with c: Compiler) -> JpfError? {
        guard let const else { return JpfError("出力するオブジェクトがありません。") }
        if const.isError { return const.error }
        do {
            try const.emit(with: c)
        } catch {
            return jpfError(from: error)
        }
        return nil
    }
}
// [Expression]のコンパイル
extension Array where Element == Expression {
    /// 複数の式（または引数）を順に翻訳する共通処理
    /// - Parameter expressions:
    /// - Returns:
    ///   - nil : 返す結果が無い(翻訳終了)
    ///   - JpfObject : 翻訳エラー、またはスタックエラー
    func compile(with c: Compiler) -> JpfObject? {
        for expression in self {
            guard let object = expression.compile(with: c) else { continue }
            if object.isError { return object }
            if let err = c.push(object) { return err }
        }
        return nil
    }
}
extension Node {
    func compile(with c: Compiler) -> JpfObject? {
        return notImplemented(type: String(describing: self.self), description: self.string)
    }
    // 翻訳エラー
    func notImplemented(type: String, description: String) -> JpfError {
        JpfError("型：\(type)「\(description)」は、翻訳不可(未実装)です。")
    }
}
extension Program : Compilable {
    func compile(with c: Compiler) -> JpfObject? {
        c.switchCase.enter()
        if c.optimizeConstantsEnabled {
            switch analyze(with: c) {
            case .constant(let const):
                do {
                    try const.emit(with: c)
                    return nil
                } catch {
                    return jpfError(from: error)
                }
            case .evaluated:
                return nil
            case .nonConstant:
                break
            case .error(let message):
                return JpfError(message)
            }
        }
        for statement in statements {
            if let error = statement.compile(with: c) {
                return error
            }
        }
        if c.switchCase.hasJumpPositions {return JpfError(c.switchCase.defaultError)}
        c.switchCase.leave()
        return nil
    }
}
extension ExpressionStatement : Compilable {
    func compile(with c: Compiler) -> JpfObject? {
        if let exit = expressions.compile(with: c) {
            return exit
        }
        return nil
    }
}
extension CompoundStatement : Compilable {
    func compile(with c: Compiler) -> JpfObject? {
        if c.optimizeConstantsEnabled {
            switch analyze(with: c) {
            case .constant(let const):
                do {
                    try const.emit(with: c)
                    return nil
                } catch {
                    return jpfError(from: error)
                }
            case .evaluated:
                return nil
            case .nonConstant:
                break
            case .error(let message):
                return JpfError(message)
            }
        }
        for sentence in sentences {
            if let error = sentence.compile(with: c) {
                return error
            }
        }
        return nil
    }
}
extension BlockStatement : Compilable {
    func compile(with c: Compiler) -> JpfObject? {
        c.switchCase.enter()
        for statement in statements {
            if let error = statement.compile(with: c) {
                return error
            }
        }
        if c.switchCase.hasJumpPositions {return JpfError(c.switchCase.defaultError)}
        c.switchCase.leave()
        return nil
    }
}
extension DefineStatement : Compilable {
    func compile(with c: Compiler) -> JpfObject? {
        if c.optimizeConstantsEnabled  {
            switch value.analyze(with: c) {
            case .constant(_), .nonConstant:
                if !c.symbolTable.hasSymbol(name: name.value) { // 二重定義回避
                    if case .error(let message) = analyze(with: c) {
                        return JpfError(message)
                    }
                }
            case .evaluated:
                return nil
            case .error(let message):
                return JpfError(message)
            }
        }
        let symbol = c.symbolTable.define(name.value)
        // 右辺をコンパイル
        if let result = value.compile(with: c) {
            if result.isError {return result}
            do {
                // 右辺を強制emit
                try c.emitAllCashe()
                try result.emit(with: c)
            } catch {
                return jpfError(from: error)
            }
        }
        if c.lastOpcode == .opConstant {
            c.setLastConstant(name: name.value) // オブジェクトのnameに左辺のname.valueを設定
        }
        symbol.emitOpSet(with: c)
        return nil
    }
}
// MARK: Sentence compilers
extension SimpleSentence : Compilable {
    func compile(with c: Compiler) -> JpfObject? {
        if c.optimizeConstantsEnabled {
            switch analyze(with: c) {
            case .constant(let const):
                return const
            case .evaluated:
                return nil
            case .nonConstant:
                break
            case .error(let message):
                return JpfError(message)
            }
        }
        if let exit = arguments.compile(with: c) {
            return exit
        }
        if let result = predicateKind.compile(token: token, auxiliaryVerb: auxiliaryVerb, with: c) {// 述語をコンパイル
            if result.isError {return result}
            if let err = c.push(result) {return err}
        }
        do {try c.emitAllCashe()} catch {return jpfError(from: error)}
        return nil
    }
}
extension SentencePredicateKind {
    func compile(token: Token, auxiliaryVerb: AuxiliaryVerb, with c: Compiler) -> JpfObject? {
        switch self {
        case .builtin:
            if c.symbolTable.hasRedefined(token) {
                fallthrough
            }
            let predicate = PredicateExpression(token: token, auxiliaryToken: auxiliaryVerb.token)
            return predicate.compile(with: c)
        case .custom:
            do {try c.emitAllCashe()} catch {return jpfError(from: error)}
            let identifier = Identifier(from: token, with: auxiliaryVerb.token)
            return identifier.compile(with: c)
        }
    }
}
extension AssignmentSentence : Compilable {
    func compile(with c: Compiler) -> JpfObject? {
        if c.optimizeConstantsEnabled {
            switch analyze(with: c) {
            case .evaluated:
                guard let const = c.environment[referent.value] else {
                    break
                }
                _ = c.symbolTable.define(referent.value, kind: const.symbolKind)
                return nil
            case .error(let message):
                return JpfError(message)
            default:
                break
            }
        }
        do {
            // 右辺(value)の翻訳・出力
            if let value = (self.value as? PhraseExpression)?.left ?? self.value {
                if let result = value.compile(with: c) {
                    if result.isError { return result }
                    try result.emit(with: c)
                }
            }
            // 位置/要素の翻訳
            if let attribute {
                // 要素代入「aのb(左辺)に代入」を翻訳・出力
                let resolvedReferent = try JpfIdentifier(resolving: referent, with: c)
                try resolvedReferent.emit(with: c)
                _ = c.emit(particle: .NO)
                if let result = attribute.compile(with: c) {
                    if result.isError { return result }
                    try result.emit(with: c)
                }
                // 要素代入
                _ = c.emit(predicate: .ASSIGN)
            }
            // 左辺の出力(値代入) *: 既存propertyに優先するため強制シンボル登録・出力
            let symbol = c.symbolTable.define(referent.value)
            symbol.emitOpSet(with: c)
        } catch {
            return jpfError(from: error)
        }
        return nil
    }
}
// MARK: Expression compilers
extension Identifier : Compilable {
    func compile(with c: Compiler) -> JpfObject? {
        if c.optimizeConstantsEnabled {
            switch analyze(with: c) {
            case .constant(let const):
                return const
            case .nonConstant:
                break
            case .evaluated:
                return nil
            case .error(let message):
                return JpfError(message)
            }
        }
        do {
            if self.role == .specifier {// 指定子を優先(定数登録し述語に渡す)
                return JpfSpecifier(from: self)
            }
            if self.isAssignTarget {    // 左辺の場合、識別子オブジェクトを返す
                return try JpfIdentifier(ensuring: self, with: c)
            }
            let ident = try JpfIdentifier(resolving: self, with: c)
            guard ident.hasSymbol else {// 登録済み
                return undefinedIdentifier(value)
            }
            try c.emitAllCashe()
            try ident.emit(with: c)     // scopeに応じたコードを出力
            if auxiliaryToken != nil {  // サ変動詞による呼び出し
                _ = c.emit(op: .opCall)
            }
            return nil
        } catch {
            return jpfError(from: error)
        }
    }
    var isProperty: Bool {
        ObjectProperties.hasName(value)
    }
}
extension PredicateExpression : Compilable {
    func compile(with c: Compiler) -> JpfObject? {
        if c.optimizeConstantsEnabled {
            switch analyze(with: c) {
            case .constant(let const):
                return const
            case .evaluated:
                return nil
            case .nonConstant:
                break
            case .error(let message):
                return JpfError(message)
            }
        }
        if let predicate = PredicateCompilableFactory.create(from: token, with: c) {
            return predicate.compile()                  // opPredicate以外の翻訳
        }
        do {
            try c.emitAllCashe()
        } catch {
            return jpfError(from: error)
        }
        return c.emit(predicate: token)
    }
}
extension PhraseExpression : Compilable {
    func compile(with c: Compiler) -> JpfObject? {
        if c.optimizeConstantsEnabled {
            switch left.analyze(with: c) {
            case .constant(let const):
                return JpfPhrase(value: const, particle: token)
            case .nonConstant, .error(_):
                break
            case .evaluated:
                return nil
            }
        }
        switch left {
        case is PredicateExpression,
             is NominalizedExpression,
             is FunctionLiteral,
             is ComputationLiteral,
             is Identifier:             // 定数化しないノード
            if let result = left.compile(with: c) {
                if result.isError {return result}
                return JpfPhrase(value: result, particle: token)
            }
        default:
            // 定数であればJpfPhraseに
            if case .constant(let const) = left.analyze(with: c) {
                return JpfPhrase(value: const, particle: token)
            }
            // コンパイルフェーズ
            if let result = left.compile(with: c) {
                if result.isError {return result}
                return JpfPhrase(value: result, particle: token)
            }
        }
        // 格をemitまたは差し替え
        if changeConstantToPhrase(with: c) {    // 出力が定数であれば、定数を句に差し替える
            return nil
        }
        let particleIndex = token.particleIndex!
        if c.lastOpcode == .opPhrase {          // 直前の格を差し替える
            c.changeOperand(at: c.lastPosition!, operand: particleIndex)
        } else {
            _ = c.emit(op: .opPhrase, operand: particleIndex)
        }
        return nil
    }
    private func changeConstantToPhrase(with c: Compiler) -> Bool {
        switch c.lastOpcode {
        case .opConstant:
            c.wrapLastConstantAsPhrase(with: token)
        case .opTrue:
            c.removeLastInstruction()
            let phrase = JpfPhrase(value: JpfBoolean.TRUE, particle: token)
            try! phrase.emit(with: c)
        case .opFalse:
            c.removeLastInstruction()
            let phrase = JpfPhrase(value: JpfBoolean.FALSE, particle: token)
            try! phrase.emit(with: c)
        default:
            return false
        }
        return true
    }
}
extension CaseExpression : Compilable {
    /// 条件処理(場合分け)。
    /// - Returns: ReturnValueまたはnil、エラー
    func compile(with c: Compiler) -> JpfObject? {
        if c.optimizeConstantsEnabled {
            switch analyze(with: c) {
            case .constant(let const):
                return const
            case .nonConstant:
                break
            case .evaluated:
                return nil
            case .error(let message):
                return JpfError(message)
            }
        }
        return c.switchCase.isActive ? switchCaseCompile(with: c) : ifThenCompile(with: c)
    }
    /// 形式１：　(条件)場合、【処理】(、それ以外は、【処理】)
    private func ifThenCompile(with c: Compiler) -> JpfObject? {
        let opJumpNotTruthyPosition = c.emit(op: .opJumpNotTruthy, operand: 9999)
        if let err = consequence.compile(with: c) {return err}
        let opJumpPosition = alternative != nil ? c.emit(op: .opJump, operand: 9999) : -1
        c.changeOperand(at: opJumpNotTruthyPosition, operand: c.nextPosition)   // Jump先書換え
        if let alternative {
            if let err = alternative.compile(with: c) {return err}
            c.changeOperand(at: opJumpPosition, operand: c.nextPosition)
        }
        return nil
    }
    /// 形式２：   〜が、〜の場合、【処理】(、〜の場合、【処理】...)、それ以外は、【処理】
    /// *: 「〜が」をスタックに積んでいる
    private func switchCaseCompile(with c: Compiler) -> JpfObject? {
        let opJumpNotTruthyPosition = c.emit(op: .opJumpNotTruthy, operand: 9999)
        _ = c.emit(op: .opDropConst, operand: 1)                                // 「〜が」を捨てる
        if let err = consequence.compile(with: c) {return err}
        let opJumpPosition = c.emit(op: .opJump, operand: 9999)
        c.switchCase.append(opJumpPosition)
        c.changeOperand(at: opJumpNotTruthyPosition, operand: c.nextPosition)   // Jump先書換え
        if let alternative {
            _ = c.emit(op: .opDropConst, operand: 1)                            // 「〜が」を捨てる
            if let err = alternative.compile(with: c) {return err}
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
    func compile(with c: Compiler) -> JpfObject? {
        if c.optimizeConstantsEnabled {
            switch analyze(with: c) {
            case .constant(let value):
                return value
            case .nonConstant, .error(_):
                break
            case .evaluated:
                return nil
            }
        }
        if right is CaseExpression {            // 〜が<左項>の場合
            do {
                try c.emitAllCashe()
            } catch {
                return jpfError(from: error)
            }
            _ = c.emit(op: .opDuplicateConst, operand: 1)   // 実行スタックの「〜が」をコピー
        }
        if let result = left.compile(with: c) {  // 左項コンパイル
            return emit(result, with: c)
        }
        return compile(right, with: c)            // 右項コンパイル
    }
}
private extension GenitiveExpression {
    /// 右項を翻訳
    func compile(_ right: Expression, with c: Compiler) -> JpfObject? {
        // 右項の種類により必要なコードをemit
        var needOpGenitive = false
        switch right {
        case  is CaseExpression:                // 〜の場合、(switch-case)
            c.switchCase.isActive = true
            c.optimizedEmit(particle: .DE)      // <左項>で
            _ = c.emit(predicate: .BE)          // ある
        case let ident as Identifier
            where ident.isProperty:             // 〜の<属性>
            _ = c.emit(property: ident.value)   // 属性取得(opGetProperty)
            return nil
        case let phrase as PhraseExpression:    // 〜の<句>
            if let error = compile(phrase.left, with: c) { return error }
            // 格を付け加える
            _ = c.emit(particle: phrase.token)
            return nil
        case is PredicateExpression:            // 〜の<述語> (例：負数)
            break
        default:
            needOpGenitive = true
            break
        }
        // 右項を翻訳
        if let result = right.compile(with: c) {
            return emit(result, with: c)
        }
        if needOpGenitive {
            _ = c.emit(op: .opGenitive)
        }
        return nil
    }
}
extension ConditionalOperation : Compilable {
    func compile(with c: Compiler) -> JpfObject? {
        if c.optimizeConstantsEnabled {
            switch analyze(with: c) {
            case .constant(let value):
                return value
            case .nonConstant, .error(_):
                break
            case .evaluated:
                return nil
            }
        }
        do {
            // (条件)に → 「に」を取り除く
            if let condition = c.environment.unwrappedPeek {
                c.drop()
                try condition.emit(with: c)
            } else
            if c.removeLastOpPhrase(particle: .NI) == false {
                c.unwrapLastConstantFromPhrase()
            }
            // emit
            let opJumpNotTruthyPosition = c.emit(op: .opJumpNotTruthy, operand: 9999)
            if let value = consequence.compile(with: c) {
                try emitValue(value, with: c)
            }
            let opJumpPosition = c.emit(op: .opJump, operand: 9999)
            c.changeOperand(at: opJumpNotTruthyPosition, operand: c.nextPosition)
            if let value = alternative.compile(with: c) {
                try emitValue(value, with: c)
            }
            c.changeOperand(at: opJumpPosition, operand: c.nextPosition)
        } catch {
            return jpfError(from: error)
        }
        return nil
    }
    private func emitValue(_ value: JpfObject, with c: Compiler) throws {
        if let err = value as? JpfError {
            throw err
        }
        try value.emit(with: c)
    }
}
extension NominalizedExpression : Compilable {
    func compile(with c: Compiler) -> JpfObject? {
        guard token.isKeyword(.QUESTION) else {
            return unsupportedNominalizer(token.literal)
        }
        if c.optimizeConstantsEnabled {
            switch sentence.analyze(with: c) {
            case .constant(let value):
                guard value is JpfBoolean else {
                    return conditionalSentenceNeeded
                }
                return value
            case .nonConstant, .error(_):
                break
            case .evaluated:
                return nil
            }
        }
        if let error = sentence.compile(with: c) {
            return error
        }
        return nil
    }
}
extension PropertyExpression : Compilable {
    func compile(with c: Compiler) -> JpfObject? {
        if c.optimizeConstantsEnabled {
            switch analyze(with: c) {
            case .constant(let const):
                return const
            case .nonConstant:
                break
            case .evaluated:
                return nil
            case .error(let message):
                return JpfError(message)
            }
        }
        do {
            try c.emitGetProperty(name: property.literal)}
        catch {
            return jpfError(from: error)
        }
        return nil
    }
}
extension IntegerLiteral : Compilable {
    func compile(with c: Compiler) -> JpfObject? {
        if c.optimizeConstantsEnabled {
            switch analyze(with: c) {
            case .constant(let const):
                return const
            case .nonConstant:
                break
            case .evaluated:
                return nil
            case .error(let message):
                return JpfError(message)
            }
        }
        try! JpfInteger(value: value).emit(with: c)
        return nil
    }
}
extension Boolean : Compilable {
    func compile(with c: Compiler) -> JpfObject? {
        if c.optimizeConstantsEnabled {
            switch analyze(with: c) {
            case .constant(let const):
                return const
            case .nonConstant:
                break
            case .evaluated:
                return nil
            case .error(let message):
                return JpfError(message)
            }
        }
        try! JpfBoolean(value: value).emit(with: c)
        return nil
    }
}
extension StringLiteral : Compilable {
    func compile(with c: Compiler) -> JpfObject? {
        if c.optimizeConstantsEnabled {
            switch analyze(with: c) {
            case .constant(let const):
                return const
            case .nonConstant:
                break
            case .evaluated:
                return nil
            case .error(let message):
                return JpfError(message)
            }
        }
        try! JpfString(value: value).emit(with: c)
        return nil
    }
}
extension ArrayLiteral : Compilable {
    func compile(with c: Compiler) -> JpfObject? {
        if c.optimizeConstantsEnabled {
            switch analyze(with: c) {
            case .constant(let const):
                return const
            case .nonConstant:
                break
            case .evaluated:
                return nil
            case .error(let message):
                return JpfError(message)
            }
        }
        if let (number, element) = getRepeatingArrayParameters(from: elements) {
            return compileRepeatingArray(number, element, with: c)
        }
        for element in elements {
            if let err = element.compile(with: c), err.isError {return err}
        }
        _ = c.emit(op: .opArrayConst, operand: elements.count)
        return nil
    }
    private func getRepeatingArrayParameters(from exps: [ExpressionStatement]) -> (Expression, Expression)? {
        guard exps.count == 1,
              let expressions = exps.first?.expressions,
              expressions.count == 2,
              let phrase = expressions.first as? PhraseExpression,
              phrase.token.isParticle(.KO),
              let element = expressions.last
        else {return nil}
        return (phrase.left, element)
    }
    private func compileRepeatingArray(_ count: Expression, _ element: Expression, with c: Compiler) -> JpfObject? {
        do {
            if let result = count.compile(with: c) {    // 個数
                if result.isError {return result}
                print("\(count.string)がemitされていない。")
                try result.emit(with: c)
            }
            if let result = element.compile(with: c) {  // 要素
                if result.isError {return result}
                print("\(element.string)がemitされていない。")
                try result.emit(with: c)
            }
        }
        catch {
            return jpfError(from: error)
        }
        _ = c.emit(op: .opArrayRepeat)
        return nil
    }
}
extension DictionaryLiteral : Compilable {
    func compile(with c: Compiler) -> JpfObject? {
        if c.optimizeConstantsEnabled {
            switch analyze(with: c) {
            case .constant(let const):
                return const
            case .nonConstant:
                break
            case .evaluated:
                return nil
            case .error(let message):
                return JpfError(message)
            }
        }
        for pair in pairs {
            if let err = pair.compile(with: c), err.isError {return err}
        }
        _ = c.emit(op: .opDictionaryConst, operand: pairs.count * 2)
        return nil
    }
}
extension PairExpression : Compilable {
    func compile(with c: Compiler) -> JpfObject? {
        if let err = pair.key.compile(with: c) {return err}
        if let err = pair.value.compile(with: c) {return err}
        return nil
    }
}
extension RangeLiteral : Compilable {
    func compile(with c: Compiler) -> JpfObject? {
        do {
            if c.optimizeConstantsEnabled {
                switch analyze(with: c) {
                case .constant(let const):
                    return const
                case .nonConstant:
                    break
                case .evaluated:
                    return nil
                case .error(let message):
                    return JpfError(message)
                }
            }
            var count = 0
            if let lowerBoundary {
                if let result = lowerBoundary.sentence.compile(with: c), result.isError {return result}
                try lowerBoundary.kind.emit(with: c)
                count += 1
            }
            if let upperBoundary {
                if let result = upperBoundary.sentence.compile(with: c), result.isError {return result}
                try upperBoundary.kind.emit(with: c)
                count += 1
            }
            _ = c.emit(op: .opRangeConst, operand: count * 2)
        } catch {
            return jpfError(from: error)
        }
        return nil
    }
}
extension OrExpression : Compilable {
    func compile(with c: Compiler) -> JpfObject? {
        if c.optimizeConstantsEnabled {
            switch analyze(with: c) {
            case .constant(let const):
                return const
            case .nonConstant:
                break
            case .evaluated:
                return nil
            case .error(_):
                break
            }
        }
        // 翻訳(コード出力)
        do {
            try c.emitAllCashe()
            // Left side
            try compileAndEmit(left, with: c)
            // Right side (phrase-aware)
            if let phrase = right as? PhraseExpression {
                try compileAndEmit(phrase.left, with: c)
                _ = c.emit(op: .opArrayConcat)
                if case .particle(let p) = phrase.token {
                    _ = c.emit(particle: p)
                }
            } else {
                try compileAndEmit(right, with: c)
                _ = c.emit(op: .opArrayConcat)
            }
        } catch {
            return jpfError(from: error)
        }
        return nil
    }
    // Helper to compile an expression and emit its value if needed
    private func compileAndEmit(_ exp: Expression, with c: Compiler) throws {
        if let result = exp.compile(with: c) {
            if result.isError { throw result.error! }
            try result.emit(with: c)
        }
    }
}
extension FunctionLiteral : Compilable {
    func compile(with c: Compiler) -> JpfObject? {
        do {try c.emitAllCashe()} catch {return jpfError(from: error)}  // キャッシュをバイトコードに出力
        c.enterScope()
        if !name.isEmpty {
            _ = c.symbolTable.define(functionName: name)
        }
        function.parameters.forEach {
            _ = c.symbolTable.define($0.value)
        }
        if let body = function.body,
           let result = body.compile(with: c) {
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
