//
//  predicateCompilable.swift
//  JPForce
//
//  Created by 佐藤貴之 on 2024/09/07.
//

import Foundation

// MARK: - interfaces for predicate compilation
protocol PredicateCompilable {
    var compiler: Compiler {get}
    /// 述語を翻訳し、バイトコードを出力(emit)する。
    /// 定数で演算可能な場合、演算しCompilerにキャッシュ(push)する。
    /// - Returns: 
    ///   翻訳済みの場合、nilを返す。
    ///   エラーを検出した場合、JpfErrorを返す。
    ///   キャッシュによる演算が継続可能な場合、値(JpfObject)を出力する。
    func compiled() -> JpfObject?
}
// MARK: - predicate compilable instance factory
struct PredicateCompilableFactory {
    static func create(from token: Token, with compiler: Compiler) -> PredicateCompilable? {
        switch token.type {
        case .keyword(.MONO):       return UnwrapCompiler(compiler, by: token)
        case .keyword(.EXECUTE):    return ExecuteCompiler(compiler)
        case .keyword(.SURU):       return PerformCompiler(compiler)    // 〜にする、〜をする
        case .keyword(.RETURN):     return ReturnCompiler(compiler)     // (〜を)返す
        case .keyword(.NULL):       return NullCompiler(compiler)
        case .keyword(.BE),.keyword(.NOT):
                                    return LogicalOperationCompiler(compiler, by: token)
        case .keyword(.ASSIGN):     return AssignOperationCompiler(compiler, by: token)
        case .keyword(.PULL),.keyword(.DUPLICATE):
                                    return PullOperationCompiler(compiler, by: token)
        case .keyword(.IT),.keyword(.ITS):
                                    return ItCompiler(compiler)
        default:                    return nil
        }
    }
}
// MARK: - predicate compilable implements
extension PredicateCompilable {
    var pullDupUsage: JpfError          {JpfError("仕様：(識別子「<識別子>」と…)(識別子「<識別子>」に）(「数値」または「値」を)(<数値>個)")}
}
// MARK: - 補助演算
/// <式>たもの → <式>
/// 「もの」(Keyword(.MONO))は、入力から値(式)を取り出す。
/// 取り出された値は、続く助詞を付けた句としてスタックに返される。
/// 例： 10を5で割ったものに　→ (2に)、2を二倍したものを → (4を)
struct UnwrapCompiler : PredicateCompilable {
    init(_ compiler: Compiler, by token: Token) {self.compiler = compiler; self.op = token}
    let compiler: Compiler, op: Token
    func compiled() -> JpfObject? {
        guard compiler.isEmpty else {
            return UnwrapOperator(compiler.environment, by: op).operated()
        }
        if compiler.lastOpcode == .opPhrase {
            compiler.removeLastInstruction()
        }
        return nil
    }
}
// MARK: - 関数実行/返す
struct ExecuteCompiler : PredicateCompilable {
    init(_ compiler: Compiler) {self.compiler = compiler}
    let compiler: Compiler
    func compiled() -> JpfObject? {
        if compiler.lastOpcode == .opPhrase {
            compiler.removeLastInstruction()
        }
        _ = compiler.emit(op: .opCall)
        return nil
    }
}
struct PerformCompiler : PredicateCompilable {
    init(_ compiler: Compiler) {self.compiler = compiler}
    let compiler: Compiler
    func compiled() -> JpfObject? {
        if let unwrapped = compiler.pull()?.value { // キャッシュの値を取り出し、出力
            unwrapped.emit(with: compiler)
        } else
        if compiler.lastOpcode == .opPhrase {       // をする、にする
            compiler.removeLastInstruction()        // 助詞を除く
        }
        switch compiler.lastOpcode {                // <動名詞>する = 実行
        case .opGetGlobal,.opGetLocal,.opGetFree,.opCurrentClosure:
            _ = compiler.emit(op: .opCall)
        default:
            break
        }
        return nil
    }
}
/// 「(〜を)返す」を翻訳する。
/// ・キャッシュがあれば計算(operated())し、opReturnValueを出力する。
/// ・出力する前に、句をチェックし、
///  「〜を」または「無し」であれば、出力を行う。(格が違う(「を」でない)場合はusageを返す)
///   opPhraseが出力されていたら、それを取り除く。
struct ReturnCompiler : PredicateCompilable {
    init(_ compiler: Compiler) {self.compiler = compiler}
    let compiler: Compiler
    func compiled() -> JpfObject? {
        let op = ReturnOperator(compiler.environment)
        if !compiler.isEmpty, let result = op.operated() {  // キャッシュで計算
            if result.isError {return result}
            result.value?.emit(with: compiler)              // resultはJpfReturnValue
        } else {                                            // 直前の出力で計算
            guard compiler.lastOpcode != .opPhrase ||
                  compiler.removeLastOpPhrase(particle: .WO) else {
                return returnValueUsage                     // 前句の助詞が間違っている
            }
        }
        _ = compiler.emit(op: .opReturnValue)
        return nil
    }
}
struct NullCompiler : PredicateCompilable {
    init(_ compiler: Compiler) {self.compiler = compiler}
    let compiler: Compiler
    func compiled() -> JpfObject? {JpfNull.object}
}
struct LogicalOperationCompiler : PredicateCompilable {
    init(_ compiler: Compiler, by token: Token) {self.compiler = compiler; self.op = token}
    let compiler: Compiler, op: Token
    func compiled() -> JpfObject? {
        if compiler.count >= 2 {                            // キャッシュで計算可
            let booleanOperator = BooleanOperator(compiler.environment, by: op)
            return booleanOperator.operated()
        }
        if compiler.count == 1 {                            // キャッシュで計算不可
            compiler.pull()!.emit(with: compiler)
        }
        guard let symbol = compiler.symbolTable.resolve(op) else {
            fatalError("『\(op.literal)』が未登録。")
        }
        symbol.emit(with: compiler)                         // opPredicate
        return nil
    }
}
/// 識別子名に関する例外
private enum NameError : Error {case notFound}

/// 「代入」をコンパイル
struct AssignOperationCompiler : PredicateCompilable {
    init(_ compiler: Compiler, by token: Token) {self.compiler = compiler; self.op = token}
    let compiler: Compiler, op: Token
    func compiled() -> JpfObject? {
        let params = compiler.pullAll()
        params.forEach {$0.emit(with: compiler)}            // キャッシュを全てemit
        guard let symbol = compiler.symbolTable.resolve(op) else {  // .ASSIGNを解決し、
            fatalError("『\(op.literal)』がシンボルテーブルに未登録。")
        }
        symbol.emit(with: compiler)                         // opPredicateをemit
        do {
            if let name = try getName(from: params) {       // 代入する識別子名を取得
                emitSetInstruction(with: name)              // opSetXXXをemit
            }
        } catch {                                           // 引数エラーを検出
            return assignUsage
        }
        return nil
    }
}
private extension AssignOperationCompiler {
    func getName(from params: [JpfObject]) throws -> String? {
        // 配列・辞書・列挙子の識別子名
        if let name = try getNameOfCollection(with: params) {
            return name
        }
        // 代入する識別子名
        if let name = try getNameForAssignment(with: params) {
            return name
        }
        // 複合代入演算子の識別子名
        if let name = getNameOfCompoundAssign(with: params) {
            return name
        }
        return nil
    }
    func getNameOfCollection(with params: [JpfObject]) throws -> String? {
        var params = params.suffix(3)
        guard params.count == 3 else {return nil}
        let pattern = (params[0].particle, params[1].particle, params[2].particle)
        switch pattern {
        // <配列>の位置<数値>に<値>を代入する
        // <辞書>のキー<キー>に<値>を代入する
        // <列挙子>の値に<値>を代入する
        case (Token(.NO), Token(.NI), Token(.WO)):
            params.swapAt(0, 1)
            params.swapAt(0, 2)
            fallthrough
        // <値>を<配列>の位置<数値>に代入する
        // <値>を<辞書>のキー<キー>に代入する
        // <値>を<列挙子>の値に代入する
        case (Token(.WO), Token(.NO), Token(.NI)):
            /* TODO: params[1]とparams[2]の組で、引数が静的に正しいかチェックし、エラーを投げる。 */
            /* TODO: 代入先の名前が見つからない場合、エラーを投げる。 */
            guard let object = params[1].value else {return nil}
            return getName(from: object)
        default:
            return nil
        }
    }
    func getNameForAssignment(with params: [JpfObject]) throws -> String? {
        var params = params.suffix(2)
        guard params.count == 2 else {return nil}
        let pattern = (params[0].particle, params[1].particle)
        switch pattern {
        // <識別子>に<値>を代入
        case (Token(.NI), Token(.WO)):
            params.swapAt(0, 1)
            fallthrough
        // <値>を<識別子>に代入
        case (Token(.WO),Token(.NI)):
            /* TODO: 代入先の名前が見つからない場合、エラーを投げる。 */
            guard let object = params[1].value else {return nil}
            return getName(from: object)
        default:
            return nil
        }
    }
    func getNameOfCompoundAssign(with params: [JpfObject]) -> String? {
        guard let param = params.last else {return nil}
        guard param.particle?.unwrappedParticle == .TA else {return nil}
        if let object = param.value {
            return getName(from: object)
        }
        return nil
    }
    func getName(from object: JpfObject) -> String? {
        if object.hasName {return object.name}              // 識別子名がある
        return (object as? JpfString)?.value                // 文字列を識別子とする
    }
    func emitSetInstruction(with name: String) {
        let symbol = compiler.symbolTable.define(name)      // 識別子を取得
        _ = compiler.emit(                                  // setコードを出力
            op: symbol.isGlobal ? .opSetGlobal : .opSetLocal,
            operand: symbol.index
        )
    }
}
/// 「得る」または「写す」をコンパイル
struct PullOperationCompiler : PredicateCompilable {
    init(_ compiler: Compiler, by token: Token) {self.compiler = compiler; self.op = token}
    let compiler: Compiler, op: Token
    func compiled() -> JpfObject? {
        // 「<識別子１>」(と「<識別子２>」…)に
        let params = compiler.pullAll()
        do {
            let names = try getNames(from: params)              // 引数から識別子名を取り出す
            params.forEach {$0.emit(with: compiler)}            // キャッシュを全てemit
            if let symbol = compiler.symbolTable.resolve(op) {  // opを解決し、
                symbol.emit(with: compiler)                     // opPredicateをemit
            } else {
                fatalError("『\(op.literal)』が未登録。")
            }
            emitSetInstructions(with: names)                    // opSetXXXをemit
        } catch {
            return pullDupUsage + op.literal + "。"
        }
        return nil
    }
}
private extension PullOperationCompiler {
    /// 引数から識別子の名前を取り出す。
    func getNames(from params: [JpfObject]) throws -> [String] {
        //
        let targets = params
            .compactMap { $0 as? JpfPhrase }
            .filter { $0.isParticle(.TO) || $0.isParticle(.NI) }
        
        return try targets.map { phrase in
            guard let string = phrase.value as? JpfString, !string.value.isEmpty else {
                throw NameError.notFound
            }
            return string.value
        }
    }
    /// 識別子に代入するコードを出力する。
    func emitSetInstructions(with identifiers: [String]) {
        identifiers.forEach { name in
            let symbol = compiler.symbolTable.define(name)
            _ = compiler.emit(
                op: symbol.isGlobal ? .opSetGlobal : .opSetLocal,
                operand: symbol.index
            )
        }
    }
}
struct ItCompiler : PredicateCompilable {
    init(_ compiler: Compiler) {self.compiler = compiler}
    let compiler: Compiler
    func compiled() -> JpfObject? {
        nil // opPredicateのemitを抑止
    }
}
