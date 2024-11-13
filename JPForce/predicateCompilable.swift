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
        default:                    return nil
        }
    }
}
// MARK: - predicate compilable implements
extension PredicateCompilable {
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
                  compiler.removeLastPhrase(particle: .WO) else {
                return op.returnValueUsage                  // 前句の助詞が間違っている
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
