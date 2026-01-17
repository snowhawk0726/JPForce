//
//  sentenceParser.swift
//  JPForce
//
//  Created by 佐藤貴之 on 2026/01/06.
//

import Foundation

/// 節の終端
protocol SentenceTerminalityProvider {
    var terminality: SentenceTerminality { get }
}
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
    var isExplicit: Bool {self == .period || self == .rbbracket}
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
/// 終端・継続のチェック
///  Sentence(節)層
extension Sentence {
    var terminality: SentenceTerminality {
        (self as? SentenceTerminalityProvider)?.terminality ?? .unknown
    }
}
extension SimpleSentence : SentenceTerminalityProvider {
    var terminality: SentenceTerminality {
        if auxiliaryVerb.isConjunctiveForm || token.isConjunctiveForm {
            return .conjunctive
        }
        if token.isIdent || token.isTerminalCandidate {
            return .terminal
        }
        return .neutral
    }
    var isIdentifierOnlySentence: Bool {token.isIdent && arguments.isEmpty}
}
extension AssignmentSentence : SentenceTerminalityProvider {
    var terminality: SentenceTerminality {
        auxiliaryVerb.isConjunctiveForm ? .conjunctive : .terminal
    }
}
extension ExpressionStatement {
    var auxiliaryVerb: AuxiliaryVerb {.none}
    var leadingIdentifier: Identifier? {
        guard let phrase = expressions.first as? PhraseExpression else {return nil}
        return phrase.left as? Identifier
    }
}
// Expression(式)層
extension Expression {
    var sentenceToken: Token {token}
    var auxiliaryVerb: AuxiliaryVerb {.none}
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
}
extension PredicateExpression {
    var isPredicate: Bool {token.isPredicate}
    var isTerminalCandidate: Bool {token.isTerminalCandidate}
    var isConjunctiveForm: Bool {auxiliaryToken?.isConjunctiveForm ?? token.isConjunctiveForm}
    var auxiliaryVerb: AuxiliaryVerb {AuxiliaryVerb(auxiliaryToken: auxiliaryToken)}
    func hasKeyword(_ k: Token.Keyword) -> Bool {token.isKeyword(k)}
}
extension Identifier {
    var isPredicate: Bool {true}
    var isTerminalCandidate: Bool {true}
    var isConjunctiveForm: Bool {auxiliaryToken?.isConjunctiveForm ?? token.isConjunctiveForm}
    var auxiliaryVerb: AuxiliaryVerb {AuxiliaryVerb(auxiliaryToken: auxiliaryToken)}
}
extension PhraseExpression {
    var isPredicate: Bool {left.isPredicate}
    var isTerminalCandidate: Bool {isConjunctiveForm}
    var isConjunctiveForm: Bool {token.isParticle(.TE)}
    var auxiliaryVerb: AuxiliaryVerb {left.auxiliaryVerb}
    func hasKeyword(_ k: Token.Keyword) -> Bool {left.hasKeyword(k)}
    var sentenceToken: Token {left.sentenceToken}
    var sentenceParticle: Token.Particle? {
        if case .particle(let p) = token.type {return p}
        return nil
    }
}
// Sentenceの解析
extension ExpressionStatementParser {
    func parseSentecne(from es: ExpressionStatement) -> Statement? {
        defer {parser.leadingIdentifier = nil}
        parser.leadingIdentifier = es.leadingIdentifier // 文頭の識別子を記憶
        let slices = splitIntoSentenceSlices(from: es)
        let sentences = slices.compactMap {buildSentence(from: $0.expressions)}
        guard sentences.count == slices.count else {
            return nil                          // 構文エラー(構築に失敗)
        }
        validateCompoundAssignment(sentences: sentences, slices: slices)
        validateSentenceSequence(sentences)
        validateSentenceEnd(sentences, terminator: es.terminator)
        guard parser.errors.isEmpty else {      // 構文エラー
            return nil
        }
        if sentences.count == 1 {
            return sentences.first              // 単文
        }
        return CompoundStatement(
            token: sentences.first!.token,
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
        
        for expr in es.expressions {
            current.append(expr)
            if isSentenceBoundary(expression: expr) {
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
    private func isSentenceBoundary(expression: Expression) -> Bool {
        return expression.isTerminalCandidate
    }
    /// Sentence構築
    private func buildSentence(from slice: [Expression]) -> Sentence? {
        guard let last = slice.last else {return nil}
        var slice = slice
        if last.isAssignment {
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
    /// 文中制約チェック
    private func validateSentenceSequence(_ sentences: [Sentence]) {
        for sentence in sentences.dropLast() {
            if sentence.terminality == .terminal {
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


