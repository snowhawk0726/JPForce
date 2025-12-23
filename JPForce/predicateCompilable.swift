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
    // 実行
    var functionToBeExecutedNotFound: JpfError {JpfError("実行する関数が無い。")}
    // 代入
    var assignIdentiferNotFound: JpfError {JpfError("代入する対象の識別子が見つからない。")}
    var assignCllectionNotFound: JpfError {JpfError("代入するコレクションが見つからない。")}
    func assignKeyNotFound(of c: String?) -> JpfError {JpfError("代入する「\(c ?? " ??")」のキーが見つからない。")}
    func assignValueNotFound(to c: String?, key: String?) -> JpfError {JpfError("「\(c ?? "??")」の「\(key ?? "??")」に、代入する値が見つからない。")}
    func unregisteredProperty(_ name: String) -> JpfError {JpfError("属性名「\(name)」は未登録。")}
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
        if !compiler.isEmpty {
            do {try emitFunction(with: compiler)} catch {return jpfError(from: error)}
        }
        if compiler.lastOpcode == .opPhrase {
            compiler.removeLastInstruction()
        }
        _ = compiler.emit(op: .opCall)
        return nil
    }
}
private extension ExecuteCompiler {
    func emitFunction(with compiler: Compiler) throws {
        guard let value = compiler.pull()?.value else {
            throw functionToBeExecutedNotFound
        }
        guard value is JpfIdentifier || value is JpfFunction else {
            throw functionToBeExecutedNotFound
        }
        try compiler.emitAllCashe()
        try value.emit(with: compiler)
    }
}
struct PerformCompiler : PredicateCompilable {
    init(_ compiler: Compiler) {self.compiler = compiler}
    let compiler: Compiler
    /// 〜をする、〜にする、〜する
    func compiled() -> JpfObject? {
        // 「する」対象がキャッシュ
        if let unwrapped = compiler.pull()?.value { // キャッシュの値を取り出し、出力
            do {
                try compiler.emitAllCashe()
                try unwrapped.emit(with: compiler)
            } catch {
                return jpfError(from: error)
            }
            return emitOpCallIfNeeded()
        }
        // 「する」対象がコード
        if compiler.lastOpcode == .opPhrase {
            compiler.removeLastInstruction()        // 助詞を除く
        }
        return emitOpCallIfNeeded()
    }
}
private extension PerformCompiler {
    /// <動名詞>する = 実行
    /// - Returns: nil
    func emitOpCallIfNeeded() -> JpfObject? {
        switch compiler.lastOpcode {
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
            do {
                try result.value?.emit(with: compiler)      // resultはJpfReturnValue
            } catch {
                return jpfError(from: error)
            }
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
        if compiler.count >= 2 && !compiler.hasIdentInCashe {   // キャッシュで計算可
            let booleanOperator = BooleanOperator(compiler.environment, by: op)
            return booleanOperator.operated()
        }
        do {try compiler.emitAllCashe()} catch {return jpfError(from: error)}
        return compiler.emit(predicate: op)                     // opPredicate
    }
}
/// 識別子名に関する例外
private enum NameError : Error {case notFound}

/// 「代入」をコンパイル
struct AssignOperationCompiler : PredicateCompilable {
    init(_ compiler: Compiler, by token: Token) {self.compiler = compiler; self.op = token}
    let compiler: Compiler, op: Token
    func compiled() -> JpfObject? {
        let (first, second, third) = normalizeParams(from: compiler)
        let pattern = (first?.particle, second?.particle, third?.particle)
        do {
            try compiler.emitAllCashe()
            switch pattern {
                // コレクションに代入
            case (_, Token(.NO), Token(.NI)):
                return compileCollectionAssignment(value: first, collection: second, key: third)
            case (Token(.NO), Token(.NI), Token(.WO)):
                return compileCollectionAssignment(value: third, collection: first, key: second)
                // 識別子に代入
            case (.none, _, Token(.NI)):
                try emitAssignment(value: second, ident: third)
            case (.none, Token(.NI), Token(.WO)):
                try emitAssignment(value: third, ident: second)
                // 複合代入
            case (.none, _, Token(word: "て")):
                try emitCompoundAssignment(value: third?.value, ident: second?.value)
                // 実行時(コード出力済み)処理
            default:
                try emitAssignment()
            }
        } catch {
            return jpfError(from: error)
        }
        return nil
    }
}
private extension AssignOperationCompiler {
    func compileCollectionAssignment(value: JpfObject?, collection: JpfObject?, key: JpfObject?) -> JpfObject? {
        // 「<値>を」が翻訳済みの場合は、emit
        if value == nil && compiler.isLastOpPhrase(particle: .WO) {
            do {try emitCollectionAssignment(value: nil, collection: collection, key: key)}
            catch {return jpfError(from: error)}
            return nil
        }
        // 引数をチェック
        guard let v = value?.value else {
            return assignValueNotFound(to: collection?.value?.string, key: key?.value?.string)
        }
        guard let c = collection?.value  else {
            return assignCllectionNotFound
        }
        guard let k = key?.value else {
            return assignKeyNotFound(of: c.value?.string)
        }
        // 引数が識別子を含む場合は、emit
        if [v, c, k].contains(where: {$0 is JpfIdentifier}) {
            do {try emitCollectionAssignment(value: value, collection: collection, key: key)} catch {return jpfError(from: error)}
            return nil
        }
        // 引数が定数の場合は、値を代入したコレクションを返す
        return c.assign(v, to: k)
    }
    func emitAssignment(value: JpfObject?, ident: JpfObject?) throws {
        // 「<配列>の」が翻訳済みの場合は、emit
        if compiler.isLastOpPhrase(particle: .NO) {
            try emitCollectionAssignment(value: value, collection: nil, key: ident)
            return
        }
        // 「<値>を<識別子>に代入」をemit
        try value?.value?.emit(with: compiler)
        // 引数から識別子名を取得し、opSetXXをemit
        let ident = try JpfIdentifier(ensuring: ident, with: compiler)
        try ident.emitOpSet(with: compiler)
    }
    func emitCompoundAssignment(value: JpfObject?, ident: JpfObject?) throws {
        if let name = compiler.identifier {
            let ident = JpfIdentifier(ensuring: name, with: compiler)
            try emitAssignment(value: value, ident: ident)
            return
        }
        try emitAssignment(value: value, ident: ident)
    }
    func emitCollectionAssignment(value: JpfObject?, collection: JpfObject?, key: JpfObject?) throws {
        // 引数を適切な順序で、emit
        try emitCollectionAssignmentWithOrder(value: value, collection: collection, key: key)
        // 「代入」をemit
        _ = compiler.emit(predicate: .ASSIGN)
        // 代入されるコレクションが識別子であれば、さらに、setをemit
        if let ident = collection?.value as? JpfIdentifier {
            try ident.emitOpSet(with: compiler)
        }
    }
    /// .ASSIGNに渡す引数を順番に出力(翻訳済みの引数はnil)
    private func emitCollectionAssignmentWithOrder(value: JpfObject?, collection: JpfObject?, key: JpfObject?) throws {
        if key == nil {
            throw assignKeyNotFound(of: collection?.value?.string)
        }
        if collection == nil {
            try key?.emit(with: compiler)       // <キー>に
            try value?.emit(with: compiler)     // <値>を
            return
        }
        try value?.emit(with: compiler)         // <値>を
        try collection?.emit(with: compiler)    // <コレクション>の
        try key?.emit(with: compiler)           // <キー>に
    }
    func emitAssignment() throws {              // 引数が翻訳済みの場合の代入
        guard let name = compiler.identifier else {
            throw assignIdentiferNotFound
        }
        if compiler.isLastOpPhrase(particle: .TA) {         // 直前の出力「た」でを取り除く
            _ = compiler.removeLastOpPhrase(particle: .TA)
            compiler.identifier = nil                       // 複合代入用識別子をクリア
        } else {
            _ = compiler.emit(predicate: .ASSIGN)   // 代入する
        }
        let ident = JpfIdentifier(ensuring: name, with: compiler)
        try ident.emitOpSet(with: compiler)
    }
    //
    func normalizeParams(from stack: Compiler) -> (JpfObject?, JpfObject?, JpfObject?) {
        let params = stack.getAll()
        switch params.count {
        case 0:
            return (nil, nil, nil)
        case 1:
            stack.drop(1)
            return (nil, nil, params[0])
        case 2:
            stack.drop(2)
            return (nil, params[0], params[1])
        default:
            stack.drop(3)
            return (params[params.count-3], params[params.count-2], params[params.count-1])
        }
    }
}
/// 「得る」または「写す」をコンパイル
struct PullOperationCompiler : PredicateCompilable {
    init(_ compiler: Compiler, by token: Token) {self.compiler = compiler; self.op = token}
    let compiler: Compiler, op: Token
    func compiled() -> JpfObject? {
        do {
            let spec = try resolveSpec()
            try emitAllCached()
            //
            let countKind = try resolveCountKind(from: spec)
            try emitValues(by: countKind)
            try emitValueConversion(using: spec.valueMode, for: countKind)
            try emitAssignment(using: spec)
            //
            return nil
        } catch {
            return jpfError(from: error)
        }
    }
}
private extension PullOperationCompiler {
    /// 仕様解決
    func resolveSpec() throws -> PullArgumentSpec {
        let params = compiler.getAll()
        let spec = try PullArgumentResolver().resolve(params)
        compiler.drop(spec.resolvedCount)
        return spec
    }
    func emitAllCached() throws {
        try compiler.emitAllCashe()
    }
    /// 個数指定区分
    enum CountKind {
        case symbol(Symbol) // 変数指定
        case number(Int)    // 数値指定
        case single(Int)    // 省略(識別子数で指定)
    }
    func resolveCountKind(from spec: PullArgumentSpec) throws -> CountKind {
        if let name = spec.countSpec.name {
            guard let symbol = compiler.symbolTable.resolve(name) else {
                throw undefinedIdentifier(name)
            }
            return .symbol(symbol)
        }
        if let count = spec.countSpec.count, count > 1 {
            return .number(count)
        }
        let count = spec.lhs.count > 1 ? spec.lhs.count : 1
        return .single(count)
    }
    /// 値をemit
    func emitValues(by kind: CountKind) throws {
        switch kind {
        case .symbol(let symbol):
            emitValuesToArray(by: symbol)
        case .number(let count):
            emitValuesToArray(by: count)
        case .single(let count):
            op.emitConst(with: compiler, operand: count)
        }
    }
    /// 個数指定区分に対応した値変換(map)をemit
    func emitValueConversion(using mode: ValueMode, for kind: CountKind) throws {
        guard mode != .none else { return }
        // 配列に対する map
        if case .symbol = kind {
            try emitMapProperty(by: mode)
            return
        }
        if case .number(let count) = kind, count > 1 {
            try emitMapProperty(by: mode)
        }
    }
    /// 代入をemit
    func emitAssignment(using spec: PullArgumentSpec) throws {
        try emitSetInstructions(
            to: spec.lhs,
            by: spec.valueMode
        )
    }
    /// ヘルパー
    /// (識別子)指定個数分、配列に出力
    private func emitValuesToArray(by symbol: Symbol) {
        symbol.emitOpGet(with: compiler)        // スタックに指定個数を積む(opGetXXX)
        op.emitStack(with: compiler)            // 指定個数をduplicate/pull
        symbol.emitOpGet(with: compiler)        // スタックに指定個数を積む(opGetXXX)
        _ = compiler.emit(op: .opArray)         // 指定個数で配列化
    }
    /// (数値)指定個数分、配列に出力
    private func emitValuesToArray(by count: Int) {
        op.emitConst(with: compiler, operand: count)
        if count > 1 {
            _ = compiler.emit(op: .opArrayConst, operand: count)
        }
    }
    /// 値を変換(map)
    private func emitMapProperty(by mode: ValueMode) throws {
        guard let symbol = compiler.symbolTable.resolve(mode.rawValue) else {
            throw unregisteredProperty(mode.rawValue)
        }
        _ = compiler.emit(op: .opMapProperty, operand: symbol.index)
    }
    /// 値を変換(stack)
    private func emitGetProperty(by mode: ValueMode) throws {
        guard let symbol = compiler.symbolTable.resolve(mode.rawValue) else {
            throw unregisteredProperty(mode.rawValue)
        }
        _ = compiler.emit(op: .opGetProperty, operand: symbol.index)
    }
    /// 識別子に代入するコードを出力する。
    /// - Parameters:
    ///   - identifiers: 出力する識別子
    ///   - mode:        値変換モード
    func emitSetInstructions(to identifiers: [String], by mode: ValueMode = .none) throws {
        try identifiers.reversed().forEach { name in
            if mode != .none && identifiers.count > 1 {
                try emitGetProperty(by: mode)
            }
            let symbol = compiler.symbolTable.define(name)
            symbol.emitOpSet(with: compiler)
        }
        if identifiers.isEmpty, mode != .none {
            try emitGetProperty(by: mode)
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
