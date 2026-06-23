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
    ///   AnalysisResult を 返す。
    func analyze(with c: Compiler) -> AnalysisResult
}
enum AnalysisResult : Equatable {
    case constant(JpfObject)    // 定数を取得
    case nonConstant            // 定数無し
    case evaluated              // 定数は無いが、実行済み(翻訳不要)
    case error(String)          // 解析エラー(メッセージ)
    //
    static func == (lhs: AnalysisResult, rhs: AnalysisResult) -> Bool {
        switch (lhs, rhs) {
        case (.constant(let l), .constant(let r)):
            return l.isEqual(to: r)
        case (.nonConstant, .nonConstant), (.evaluated, .evaluated):
            return true
        case (.error(let l),.error(let r)):
            return l == r
        default:
            return false
        }
    }
}
@inline(__always)
func aggregateAnalysis<S: Sequence>(_ nodes: S, with c: Compiler) -> AnalysisResult where S.Element == ConstAnalysis {
    var constant = false
    var evaluated = false
    for node in nodes {
        switch node.analyze(with: c) {
        case .constant(let const):
            if let err = c.push(const) { return .error(err.message) }
            constant = true
        case .nonConstant:
            return .nonConstant // コンパイルにフォールバック
        case .evaluated:
            evaluated = true
            constant = false
        case .error(let message):
            return .error(message)
        }
    }
    if constant, !c.isEmpty { return .constant(c.pull()!) }
    return evaluated ? .evaluated : .nonConstant
}
// MARK: - implementations for ast constant analyzed
extension Array where Element == Expression {
    /// 複数の式（または引数）を順に翻訳する共通処理
    func analyze(with c: Compiler) -> AnalysisResult {
        return aggregateAnalysis(self.map { $0 as ConstAnalysis }, with: c)
    }
}
extension Array where Element == Expression {
    /// 引数の副作用なし検証: スタックを一切操作せず、識別子未解決や式評価エラーのみ検出する
    /// - Returns: エラーが無ければ `nil`、エラー文字列があればその内容を返す
    func validateArguments(with c: Compiler) -> String? {
        // 各式に対して、副作用のある push/pull を行わない評価系を使って検証する。
        // ここでは `evaluate(with:)` を用い、結果の error を見る。
        // ただし evaluate は副作用の無い純粋評価であることが前提。
        for expression in self {
            if let result = expression.evaluate(with: c.environment) {
                if let err = result.error {
                    // 述語・式の評価時エラー（未解決識別子含む）
                    return err.message
                }
            }
            // evaluate が nil の場合は、定数化できないだけなのでここではエラー扱いしない
        }
        return nil
    }
}
extension Node {
    func analyze(with c: Compiler) -> AnalysisResult {
        return .error(
            "\(String(describing: type(of: self)))は、定数分析不可(未実装)です。(\(string))"
        )
    }
}
extension Program : ConstAnalysis {
    func analyze(with c: Compiler) -> AnalysisResult {
        return aggregateAnalysis(self.statements.map { $0 as ConstAnalysis }, with: c)
    }
}
extension CompoundStatement : ConstAnalysis {
    func analyze(with c: Compiler) -> AnalysisResult {
        return aggregateAnalysis(self.sentences.map { $0 as ConstAnalysis }, with: c)
    }
}
extension BlockStatement : ConstAnalysis {
    func analyze(with c: Compiler) -> AnalysisResult {
        return aggregateAnalysis(self.statements.map { $0 as ConstAnalysis }, with: c)
    }
}
extension SimpleSentence : ConstAnalysis {
    func analyze(with c: Compiler) -> AnalysisResult {
        // 1) 引数を副作用なしで検証（識別子未定などのエラーは .nonConstant 扱い）
        if let _ = arguments.validateArguments(with: c) {
            // 引数に未解決識別子や評価エラーが含まれる場合は、
            // コンパイルにフォールバックさせるため .nonConstant を返す
            // （述語本体のエラーは次段の evaluate で拾う）
            return .nonConstant
        }
        // 2) 述語本体の評価（こちらは副作用なし evaluate を使用）
        if let const = evaluate(with: c.environment) {
            if let err = const.error {  // 本体のエラー
                return .error(err.message)
            }
            c.drop()
            return .constant(const)
        }
        // 3) 定数化できなかったが、実行は済んだ
        return .evaluated
    }
}
extension AssignmentSentence : ConstAnalysis {
    func analyze(with c: Compiler) -> AnalysisResult {
        if let result = evaluate(with: c.environment) {
            if let err = result.error {
                return .error(err.message)
            }
            return .error(assignUsage.message)
        }
        return .evaluated
    }
}
extension DefineStatement : ConstAnalysis {
    func analyze(with c: Compiler) -> AnalysisResult {
        // 右辺解析
        switch value.analyze(with: c) {
        case .constant(let const):
            // 定数を登録
            c.environment[name.value] = const
            _ = c.symbolTable.define(name.value, kind: const.symbolKind)
        case .nonConstant:
            return .nonConstant
        case .evaluated:
            break
        case .error(let message):
            return .error(message)
        }
        return .evaluated
    }
}
extension ExpressionStatement : ConstAnalysis {
    func analyze(with c: Compiler) -> AnalysisResult {
        expressions.analyze(with: c)
    }
}
extension IntegerLiteral : ConstAnalysis {
    func analyze(with c: Compiler) -> AnalysisResult {
        let const = JpfInteger(value: value)
        return .constant(const)
    }
}
extension Boolean : ConstAnalysis {
    func analyze(with c: Compiler) -> AnalysisResult {
        let const = JpfBoolean(value: value)
        return .constant(const)
    }
}
extension StringLiteral : ConstAnalysis {
    func analyze(with c: Compiler) -> AnalysisResult {
        let const = JpfString(value: value)
        return .constant(const)
    }
}
extension FunctionLiteral : ConstAnalysis {
    func analyze(with c: Compiler) -> AnalysisResult {
        let const = JpfFunction(overload: FunctionBlocks(function), environment: c.environment)
        return .constant(const)
    }
}
extension ComputationLiteral : ConstAnalysis {
    func analyze(with c: Compiler) -> AnalysisResult {
        let const = JpfComputation(setters: setters, getters: getters, environment: c.environment)
        return .constant(const)
    }
}
extension ArrayLiteral : ConstAnalysis {
    func analyze(with c: Compiler) -> AnalysisResult {
        if let result = evaluate(with: c.environment),
           result.isError == false {
            return .constant(result)
        }
        return .nonConstant
    }
}
extension DictionaryLiteral : ConstAnalysis {
    func analyze(with c: Compiler) -> AnalysisResult {
        if let result = evaluate(with: c.environment),
           result.isError == false {
            return .constant(result)
        }
        c.environment.empty()
        return .nonConstant
    }
}
extension RangeLiteral : ConstAnalysis {
    func analyze(with c: Compiler) -> AnalysisResult {
         if let range = evaluate(with: c.environment),
            range.isError == false {
             return .constant(range)
        }
        c.environment.empty()
        return .nonConstant
    }
}
extension Identifier : ConstAnalysis {
    func analyze(with c: Compiler) -> AnalysisResult {
        if let result = evaluate(with: c.environment),
           result.isError == false {
            return .constant(result)
        }
        // コンパイルにフォールバック
        c.environment.empty()
        return .nonConstant
    }
}
extension PredicateExpression : ConstAnalysis {
    func analyze(with c: Compiler) -> AnalysisResult {
        if let result = evaluate(with: c.environment) {
            if let err = result.error {
                return .error(err.message)  // 評価失敗
            }
            c.drop()                        // スタック上の結果は捨てる
            return .constant(result)
        }
        return .evaluated
    }
}
extension PhraseExpression : ConstAnalysis {
    func analyze(with c: Compiler) -> AnalysisResult {
        if let result = evaluate(with: c.environment),
           result.isError == false {
            return .constant(result)
        }
        // コンパイルにフォールバック
        c.environment.empty()
        return .nonConstant
    }
}
extension OrExpression : ConstAnalysis {
    func analyze(with c: Compiler) -> AnalysisResult {
        if let result = evaluate(with: c.environment),
           result.isError == false {
            return .constant(result)
        }
        c.environment.empty()
        return .nonConstant
    }
}
extension GenitiveExpression : ConstAnalysis {
    func analyze(with c: Compiler) -> AnalysisResult {
        if let result = evaluate(with: c.environment),
           result.isError == false {
            return .constant(result)
        }
        // CaseExpressionは値を返さないので、constをスタックから回収
        if right is CaseExpression {
            if let const = c.pull() {
                return .constant(const)
            }
            return .evaluated
        }
        c.environment.empty()
        return .nonConstant
    }
}
extension PropertyExpression : ConstAnalysis {
    func analyze(with c: Compiler) -> AnalysisResult {
        if let result = evaluate(with: c.environment),
           result.isError == false {
            return .constant(result)
        }
        c.environment.empty()
        return .nonConstant
    }
}
extension NominalizedExpression : ConstAnalysis {
    func analyze(with c: Compiler) -> AnalysisResult {
        if let result = evaluate(with: c.environment),
           result.isError == false {
            return .constant(result)
        }
        c.environment.empty()
        return .nonConstant
    }
}
extension ConditionalOperation : ConstAnalysis {
    func analyze(with c: Compiler) -> AnalysisResult {
        if let result = evaluate(with: c.environment),
           result.isError == false {
            return .constant(result)
        }
        c.environment.empty()
        return .nonConstant
    }
}
extension CaseExpression : ConstAnalysis {
    func analyze(with c: Compiler) -> AnalysisResult {
        if let result = evaluate(with: c.environment),
           !result.isError {
            return .constant(result)
        }
        // CaseExpressionは値を返さないので、constをスタックから回収
        if let const = c.pull() {
            return .constant(const)
        }
        return .evaluated
    }
}
