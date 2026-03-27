//
//  sentenceParser.swift
//  JPForce
//
//  Created by 佐藤貴之 on 2026/01/06.
//

import Foundation

// MARK: enums
/// 節の終端
enum SentenceTerminality {
    case terminal
    case conjunctive
    case neutral
    case unknown
}
/// 終端種別
enum SentenceTerminator : String {
    case none       = ""
    case period     = "。"
    case rbbracket  = "】"
    case eol        = "\n"
    case eof        = "\0"
    //
    init(symbol: String) {
        self = Self(rawValue: symbol) ?? .none
    }
    var isExplicit: Bool {self == .period}
}
/// 節の述語種別
enum SentencePredicateKind {
    case builtin                // 組み込み述語(予約語)
    case custom                 // 関数/算出(識別子)
}
/// 補助動詞種別
enum AuxiliaryVerb : String {
    case none       = ""
    case suru       = "する"
    case si         = "し"
    init(auxiliaryToken token: Token?) {
        guard let token else { self = .none; return }
        self = token.isConjunctiveForm ? .si : .suru
    }
    var isConjunctiveForm: Bool {self == .si}
    var token: Token? {
        self == .suru || self == .si ? Token(.SURU) : nil
    }
}
// MARK: - 終端・継続のチェック
/// Statement(文)層
extension Statement {
    var isTerminalCandidate: Bool {true}
    var isConjunctiveForm: Bool {false}
}
extension BlockStatement {
    var isTerminalCandidate: Bool {statements.last?.isTerminalCandidate == true}
    var isConjunctiveForm: Bool {statements.last?.isConjunctiveForm == true}
}
extension CompoundStatement {
    var isTerminalCandidate: Bool {sentences.last?.isTerminalCandidate == true}
    var isConjunctiveForm: Bool {sentences.last?.isConjunctiveForm == true}
}
extension SimpleSentence {
    var isTerminalCandidate: Bool {token.isTerminalCandidate || token.isIdent}
    var isConjunctiveForm: Bool {auxiliaryVerb.isConjunctiveForm || token.isConjunctiveForm}
}
extension AssignmentSentence {
    var isTerminalCandidate: Bool {true}
    var isConjunctiveForm: Bool {auxiliaryVerb.isConjunctiveForm}
}
extension ExpressionStatement {
    var isTerminalCandidate: Bool {expressions.last?.isTerminalCandidate == true}
    var isConjunctiveForm: Bool {expressions.last?.isConjunctiveForm == true}
}
/// Sentence(節)層
extension Sentence {
    var isTerminalConnector: Bool {firstToken?.isTerminalConnector == true}
    var firstToken: Token? {token}
    var isIdentifierOnlySentence: Bool {false}
    var terminality: SentenceTerminality {
        if isConjunctiveForm {return .conjunctive}
        if isTerminalCandidate {return .terminal}
        return .neutral
    }
}
extension ExpressionStatement {
    var firstToken: Token? {
        guard let first = expressions.first else {return nil}
        if let phrase = first as? PhraseExpression {
            return phrase.left.token
        }
        return first.token
    }
    var auxiliaryVerb: AuxiliaryVerb {.none}
    var leadingIdentifier: Identifier? {
        guard let phrase = expressions.first as? PhraseExpression else {return nil}
        return phrase.left as? Identifier
    }
    var isTerminalConnector: Bool {
        firstToken?.isTerminalConnector == true
    }
    var terminality: SentenceTerminality {
        if isConjunctiveForm {return .conjunctive}
        if isTerminalCandidate {return .terminal}
        if terminator.isExplicit {
            return .terminal
        }
        return .neutral
    }
}
// Expression(式)層
extension Expression {
    var sentenceToken: Token {token}
    var auxiliaryVerb: AuxiliaryVerb {.none}
    var isTerminalCandidate: Bool {false}
    var isConjunctiveForm: Bool {false}
    var isLhsExpression: Bool {
        guard
            let phrase = self as? PhraseExpression,
            let identifier = phrase.left as? Identifier
        else {
            return false
        }
        return identifier.isLhs
    }
    func hasKeyword(_ k: Token.Keyword) -> Bool {false}
    var isConjunction: Bool {false}
}
extension PredicateExpression {
    var isPredicate: Bool {token.isPredicate}
    var isTerminalCandidate: Bool {token.isTerminalCandidate}
    var isConjunctiveForm: Bool {auxiliaryToken?.isConjunctiveForm ?? token.isConjunctiveForm}
    var auxiliaryVerb: AuxiliaryVerb {AuxiliaryVerb(auxiliaryToken: auxiliaryToken)}
    func hasKeyword(_ k: Token.Keyword) -> Bool {token.isKeyword(k)}
    var isConjunction: Bool {token.isConjunction}
}
extension Identifier {
    var isPredicate: Bool {true}
    var isTerminalCandidate: Bool {true}
    var isConjunctiveForm: Bool {
        if ObjectProperties.hasName(value) {return false}
        return auxiliaryToken?.isConjunctiveForm ?? token.isConjunctiveForm
    }
    var auxiliaryVerb: AuxiliaryVerb {AuxiliaryVerb(auxiliaryToken: auxiliaryToken)}
}
extension PhraseExpression {
    var isPredicate: Bool {(hasParticle(.TE) || hasParticle(.TA)) && left.isPredicate}
    var isTerminalCandidate: Bool {isConjunctiveForm}
    var isConjunctiveForm: Bool {hasParticle(.TE)}
    var auxiliaryVerb: AuxiliaryVerb {left.auxiliaryVerb}
    func hasKeyword(_ k: Token.Keyword) -> Bool {left.hasKeyword(k)}
    var sentenceToken: Token {left.sentenceToken}
    var sentenceParticle: Token.Particle? {
        if case .particle(let p) = token.type {return p}
        return nil
    }
}
extension CaseExpression {
    var isTerminalCandidate: Bool {lastBlcok.isTerminalCandidate}
    var isConjunctiveForm: Bool {lastBlcok.isConjunctiveForm}
    private var lastBlcok: BlockStatement {alternative ?? consequence}
}
extension LogicalExpression {
    var isTerminalCandidate: Bool {right.isTerminalCandidate}
    var isConjunctiveForm: Bool {right.isConjunctiveForm}
    var isConjunction: Bool {true}
}
extension ConditionalOperation {
    var isTerminalCandidate: Bool {true}
    var isConjunctiveForm: Bool {false}
}
extension LoopExpression {
    var isTerminalCandidate: Bool {body.isTerminalCandidate}
    var isConjunctiveForm: Bool {body.isConjunctiveForm}
}
extension GenitiveExpression {
    var isTerminalCandidate: Bool {right.isTerminalCandidate}
    var isConjunctiveForm: Bool {right.isConjunctiveForm}
}
extension PropertyExpression {
    var isTerminalCandidate: Bool {true}
    var isConjunctiveForm: Bool {false}
}
// Token(語)層
extension Token {
    /// 終止形に接続する語
    var isTerminalConnector: Bool {
        switch self {
        case .keyword(.CASE), .keyword(.QUESTION), .keyword(.KOTO), .keyword(.OR), .keyword(.AND), .keyword(.WHILE): return true
        default : return false
        }
    }
    /// 文の区切りを打ち消すもの
    var isBoundaryCanceler: Bool {
        [.keyword(.ASWELLAS)].contains(self)
    }
    /// 直前の句読点をキャンセルする語
    var isPuncuationCanceler: Bool {
        [.CASE, .QUESTION, .NOT].contains(self.unwrappedKeyword)
    }
    /// 直前の句読点を「、」にする語(接続詞)
    var isConjunction: Bool {
        [.keyword(.OR), .keyword(.AND), .keyword(.ASWELLAS)].contains(self)
    }
}
// Sentenceの解析
extension ExpressionStatementParser {
    func parseSentecne(from es: ExpressionStatement) -> Statement? {
        defer {parser.leadingIdentifier = nil}
        parser.leadingIdentifier = es.leadingIdentifier // 文頭の識別子を記憶
        // 式が空の場合は、終端のみの空文として扱う
        if es.expressions.isEmpty {
            return ExpressionStatement(token: es.token, expressions: [])
        }
        let slices = splitIntoSentenceSlices(from: es)
        var sentences = slices.compactMap {buildSentence(from: $0.expressions)}
        guard sentences.count == slices.count else {
            return nil                          // 構文エラー(構築に失敗)
        }
        validateQuestionPlacement(from: es)
        sentences = rebuild(sentences)          // 文の再構築
        validateCompoundAssignment(sentences: sentences, slices: slices)
        validateSentenceSequence(sentences)
        validateSentenceEnd(sentences, terminator: es.terminator)
        guard parser.errors.isEmpty else {      // 構文エラー
            return nil
        }
        if sentences.count == 1 {
            // 単文の場合、式文が連用形でなければ明示句点を付与
            if let singleES = sentences.first as? ExpressionStatement,
               singleES.isConjunctiveForm == false {
                return ExpressionStatement(token: singleES.token, expressions: singleES.expressions, terminator: .period)
            }
            return sentences.first              // 単文
        }
        // sentences が空になる可能性を考慮して安全に処理
        guard let first = sentences.first else { return nil }
        return CompoundStatement(
            token: first.token,
            sentences: sentences)               // 複文
    }
    /// スライスした expressions
    private struct SentenceSlice {
        let expressions: [Expression]
        let trailingParticle: Token.Particle?   // 節末の助動詞(て)
    }
    /// Sentence境界で式文を分割する
    private func splitIntoSentenceSlices(from es: ExpressionStatement) -> [SentenceSlice] {
        var slices: [SentenceSlice] = []
        var current: [Expression] = []
        
        for (i, expr) in es.expressions.enumerated() {
            current.append(expr)
            let next = i + 1 < es.expressions.count ? es.expressions[i + 1] : nil
            if isSentenceBoundary(current: expr, next: next) {
                slices.append(SentenceSlice(
                    expressions: current,
                    trailingParticle: expr.sentenceParticle
                ))
                current = []
            }
        }
        if !current.isEmpty {
            slices.append(SentenceSlice(
                expressions: current,
                trailingParticle: nil
            ))
        }
        return slices
    }
    /// Sentenceの境界判定
    private func isSentenceBoundary(current: Expression, next: Expression?) -> Bool {
        return current.isTerminalCandidate && (next?.token.isBoundaryCanceler == false)
    }
    /// Sentence構築
    private func buildSentence(from slice: [Expression]) -> Sentence? {
        guard let last = slice.last else {return nil}
        var slice = slice
        if last.isAssignment && !slice.hasImmutableLhs {
            return buildAssignmentSentence(
                from: slice
            )
        }
        // sliceのLHS候補の確定処理
        if last.sentenceToken.hasLhsIdentifier {
            slice.finalizeLhsCandidates()
        } else {
            slice.clearLhsCandidates()
        }
        if last.isPredicate {
            return SimpleSentence(
                token: last.sentenceToken,
                auxiliaryVerb: last.auxiliaryVerb,
                arguments: slice.dropLast(),
                predicateKind: last.sentenceToken.isPredicate ? .builtin : .custom,
                string: slice.toStringWithComma
            )
        }
        // 述語が無い文
        return ExpressionStatement(
            token: last.sentenceToken,
            expressions: slice
        )
    }
    /// 代入節構築
    private func buildAssignmentSentence(from slice: [Expression]) -> AssignmentSentence? {
        guard let last = slice.last else {return nil}
        let lhs = slice.extractLhsIdentifier()
        guard let target = lhs ?? parser.leadingIdentifier else {
            error(message: "代入先が見つからない。", at: last.sentenceToken)
            return nil
        }
        let kind: AssignmentKind = (lhs == nil) ? .compound : .simple
        if kind == .simple {
            target.isLhsCandidate = false
            target.isLhs = true
        }
        let arguments = slice.dropLast().filter {!$0.isLhsExpression}   // 代入右辺(候補)を抽出
        
        return AssignmentSentence(
            token: last.sentenceToken,
            auxiliaryVerb: last.auxiliaryVerb,
            kind: kind,
            target: target,
            arguments: arguments,
            string: slice.toStringWithComma
        )
    }
    private func rebuild(_ sentences: [Sentence]) -> [Sentence] {
        var result = sentences
        if result.count >= 2 {
            for i in stride(from: result.count - 1, through: 1, by: -1) {
                guard let current = result[i] as? ExpressionStatement,
                      let phrase = current.expressions.first as? PhraseExpression,
                      phrase.hasKeyword(.QUESTION) else {
                    continue
                }
                // The sentence before QUESTION must be a boolean-returning SimpleSentence
                guard let prevSimple = result[i - 1] as? SimpleSentence else {
                    error(message: "助詞「か」の前は、真偽値を返す文が必要。", at: phrase.left.token)
                    continue
                }
                // Build noun clause from the previous sentence and inject into current
                let noun = NominalizedExpression(token: phrase.left.token, sentence: prevSimple)
                var expressions = Array(current.expressions.dropFirst())
                expressions.insert(PhraseExpression(token: phrase.token, left: noun), at: 0)
                // Replace current and remove previous
                result[i] = ExpressionStatement(token: current.token, expressions: expressions)
                result.remove(at: i - 1)
            }
        }
        return result
    }
    /// 〜かによって、のチェック
    private func validateQuestionPlacement(from es: ExpressionStatement) {
        let exprs = es.expressions
        guard exprs.count >= 2 else { return }
        if let phrase = exprs[1] as? PhraseExpression,
           phrase.hasKeyword(.QUESTION) {
            error(message: "助詞「か」の前は、真偽値を返す文が必要。", at: phrase.left.token)
        }
    }
    /// 文中制約チェック
    private func validateSentenceSequence(_ sentences: [Sentence]) {
        for (i, sentence) in sentences.dropLast().enumerated() {
            let isNextTerminalConnector = i < sentences.count - 1 && sentences[i + 1].isTerminalConnector   // 次の語が終端(Terminal)に繋がる
            if sentence.terminality == .terminal && !isNextTerminalConnector {
                let identifierDetected = sentence.isIdentifierOnlySentence ?
                    "識別子「\(sentence.tokenLiteral)」は終止形の文として解析された。\n" : ""
                error(message: identifierDetected + "終止形の文の後に、文を続けることはできない。", at: sentence.token)
                return
            }
            if let assignment = sentence as? AssignmentSentence,
               assignment.kind == .compound {
                error(message: "複合代入文では、文末以外に代入を書くことはできない。", at: sentence.token)
                return
            }
        }
    }
    /// 文末チェック
    private func validateSentenceEnd(_ sentences: [Sentence], terminator: SentenceTerminator) {
        guard
            terminator.isExplicit,
            let sentence = sentences.last,
            sentence.terminality == .conjunctive
        else {
            return
        }
        error(message: "連用形の文を「\(terminator.rawValue)」で終えることはできない。", at: sentence.token)
    }
    /// 複合代入文のチェック
    private func validateCompoundAssignment(sentences: [Sentence], slices: [SentenceSlice]) {
        guard let assignment = sentences.last as? AssignmentSentence else {return}
        // 単文代入の構文エラーチェック
        if sentences.count == 1 && assignment.kind == .compound {
            error(message: "代入先が見つからない。", at: assignment.token)
            /* 例： aを代入 ← 文頭に識別子があるのが、単文のためエラー */
            return
        }
        guard slices.count > 1 else {return}
        // 複合代入文のvalidate
        let previousSetence = sentences[sentences.count - 2]
        let previousSlice = slices[slices.count - 2]
        
        guard previousSlice.trailingParticle == .TE else {
            if assignment.kind == .compound {
                error(message: "複合代入文では「て」を省略できない。", at: previousSetence.token)
            }
            return
        }
        if assignment.kind == .simple {
            error(message: "複合代入文では、代入先は文頭で指定。", at: assignment.target.token)
            return
        }
        if sentences.dropLast().contains(where: {$0 is AssignmentSentence}) {
            error(message: "複合代入文では、文末以外に代入を書くことはできない。", at: assignment.token)
            return
        }
        if !previousSetence.token.isValuePredicate {
            error(message: "代入直前の文が値を出力しない。", at: previousSetence.token)
            return
        }
    }
}
/// Setenceインタフェース
extension SimpleSentence {
    var predicate: (any Expression)? {
        guard predicateKind == .builtin else {return nil}
        return PredicateExpression(token: token, auxiliaryToken: auxiliaryVerb.token)
    }
    var literal: (any Expression)? {
        guard token.isIdent, arguments.isEmpty else {return nil}
        return Identifier(from: token)
    }
    var isIdentifierOnlySentence: Bool {token.isIdent && arguments.isEmpty}
    var firstToken: Token? {
        if let phrase = arguments.first as? PhraseExpression {
            return phrase.left.token
        }
        return token
    }
}
/// 旧AST互換インタフェース
extension ExpressionStatement {
    var predicate: (any Expression)? {expressions.last}
    var arguments: [any Expression] {
        predicate != nil ? expressions.dropLast() : expressions
    }
    var expressionCount: Int {expressions.count}
    var literal: Expression? {expressions.first}
}
extension DefineStatement {
    var rhsCount: Int {value.expressionCount}
    var rhsLiteral: Expression? {value.literal}
}
