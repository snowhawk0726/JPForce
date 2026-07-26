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
    var leadingExpression: Expression? {
        if expressions.first is PhraseExpression ||     // 複合代入
           expressions.first is GenitiveExpression {    // 複合要素代入
            return expressions.first
        }
        return nil
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
    var auxiliaryVerb: AuxiliaryVerb {.none}
    var isTerminalCandidate: Bool {false}
    var isConjunctiveForm: Bool {false}
    var isPredicateToAssign: Bool {
        guard let predicate = self as? PredicateExpression else {
            return false
        }
        return predicate.token.hasAssignTarget
    }
    func hasKeyword(_ k: Token.Keyword) -> Bool {false}
    var isConjunction: Bool {false}
    var hasGenitivePredicate: Bool {
        valueToken.hasGenitivePredicate
    }
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
    var isConjunctiveForm: Bool {hasParticle(.TE) || hasParticle(.TA)}
    var auxiliaryVerb: AuxiliaryVerb {left.auxiliaryVerb}
    func hasKeyword(_ k: Token.Keyword) -> Bool {left.hasKeyword(k)}
    var valueToken: Token {left.valueToken}
    var particle: Token.Particle? {
        if case .particle(let p) = token.type {return p}
        return nil
    }
    var valueExpression: (any Expression) {left}
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
    var isTerminalCandidate: Bool {alternative.isTerminalCandidate}
    var isConjunctiveForm: Bool {alternative.isConjunction}
}
extension LoopExpression {
    var isTerminalCandidate: Bool {body.isTerminalCandidate}
    var isConjunctiveForm: Bool {body.isConjunctiveForm}
}
extension GenitiveExpression {
    var isTerminalCandidate: Bool {right.isTerminalCandidate}
    var isConjunctiveForm: Bool {right.isConjunctiveForm}
    var particle: Token.Particle? {right.particle}
    var valueExpression: (any Expression) {
        GenitiveExpression(token: token, left: left, right: right.valueExpression)
    }
    var leftPhrase: PhraseExpression {
        PhraseExpression(token: token, left: left)
    }
}
extension PropertyExpression {
    var isTerminalCandidate: Bool {true}
    var isConjunctiveForm: Bool {false}
}
// Token(語)層
extension Token {
    /// 終止形に接続する語
    var isTerminalConnector: Bool {
        isKeyword(.CASE) || isKeyword(.QUESTION) || isKeyword(.KOTO) || isKeyword(.OR) || isKeyword(.AND) || isKeyword(.WHILE)
    }
    /// 文の区切りを打ち消すもの
    var isBoundaryCanceler: Bool {
        [.keyword(.ASWELLAS), .keyword(.MONO)].contains(self)
    }
    /// 直前の句読点をキャンセルする語
    var isPuncuationCanceler: Bool {
        [.CASE, .QUESTION, .NOT].contains(self.unwrappedKeyword)
    }
    /// 直前の句読点を「、」にする語(接続詞)
    var isConjunction: Bool {
        isKeyword(.OR) || isKeyword(.AND) || isKeyword(.ASWELLAS)
    }
}
// Sentenceの解析
extension ExpressionStatementParser {
    func parseSentecne(from es: ExpressionStatement) -> Statement? {
        // 属格代入(〜の〜は、value)
        if let genitive = es.expressions.first as? GenitiveExpression,
           genitive.value != nil {
            return buildGenitiveAssignment(from: genitive)
        }
        defer {parser.leadingExpression = nil}
        parser.leadingExpression = es.leadingExpression // 文頭の式を記憶
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
        resolveSpecifierRole(in: sentences)
        resolveAssignIdentifierRole(in: sentences)
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
}
private extension ExpressionStatementParser {
    /// スライスした expressions
    struct SentenceSlice {
        let expressions: [Expression]
        let trailingParticle: Token.Particle?   // 節末の助動詞(て)
    }
    /// Sentence境界で式文を分割する
    func splitIntoSentenceSlices(from es: ExpressionStatement) -> [SentenceSlice] {
        var slices: [SentenceSlice] = []
        var current: [Expression] = []
        
        for (i, expr) in es.expressions.enumerated() {
            current.append(expr)
            let next = i + 1 < es.expressions.count ? es.expressions[i + 1] : nil
            if isSentenceBoundary(current: expr, next: next) {
                slices.append(SentenceSlice(
                    expressions: current,
                    trailingParticle: expr.particle
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
    func isSentenceBoundary(current: Expression, next: Expression?) -> Bool {
        return current.isTerminalCandidate && (next?.valueToken.isBoundaryCanceler == false)
    }
    /// Sentence構築
    func buildSentence(from slice: [Expression]) -> Sentence? {
        guard let last = slice.last else {return nil}
        let lastToken = last.valueToken
        if last.isAssignment {
            if let target = slice.extractLhsIdentifier() {  // 単純代入
                return buildAssignmentSentence(target: target, kind: .simple, with: slice)
            }
            if slice.count == 1,                            // 前文は「て」で分割済み
               let target = parser.leadingIdentifier {      // 複合代入
                return buildAssignmentSentence(target: target, kind: .compound, with: slice)
            }
            guard slice.hasAssignmentTarget else {
                error(message: "代入先が見つかりません。", at: last.valueToken)
                return nil
            }                                               // immutable代入
        }
        // 述語(immutableな代入を含む)のチェック
        if last.isPredicate {
            return SimpleSentence(
                token: lastToken,
                auxiliaryVerb: last.auxiliaryVerb,
                arguments: slice.dropLast(),
                predicateKind: last.valueToken.isPredicate ? .builtin : .custom,
                string: slice.toStringWithComma
            )
        }
        // 述語が無い文
        return ExpressionStatement(
            token: lastToken,
            expressions: slice
        )
    }
    /// 代入節構築
    func buildAssignmentSentence(target: Identifier, kind: AssignmentKind, with slice: [Expression]) -> AssignmentSentence? {
        guard let last = slice.last else {return nil}
        // 代入位置抽出(なければnil)
        let positionExpr = (kind == .compound) ? parser.leadingPosition : extractPosition(from: slice)
        let position = positionExpr.map { PhraseExpression(token: Token(.NI), left: $0) }
        // 右辺抽出
        let rhs = getRhs(from: slice.dropLast())

        return AssignmentSentence(
            token: last.valueToken,
            auxiliaryVerb: last.auxiliaryVerb,
            kind: kind,
            referent: target,
            attribute: position,
            value: rhs,
            string: slice.toStringWithComma
        )
    }
    func extractPosition(from slice: [Expression]) -> Expression? {
        // 要素代入「〜の〜に」である場合は、「〜に」の left を返す
        // (「〜の〜を」も許容)
        var particleNo = false
        for exp in slice {
            guard let phrase = exp as? PhraseExpression else {
                continue
            }
            if phrase.hasParticle(.NO) {
                particleNo = true
                continue
            }
            if particleNo,
               phrase.hasParticle(.NI) || phrase.hasParticle(.WO) {
                return phrase.left
            }
        }
        return nil
    }
    /// 代入対象(〜を)を抽出する
    func getRhs(from phrases: [Expression]) -> Expression? {
        for (i, target) in phrases.enumerated() {
            // 1. 〜、(〜の)〜に(代入)
            guard let phrase = target as? PhraseExpression else {
                return PhraseExpression(token: Token(.WO), left: target)
            }
            // 2. 〜を(〜の)〜に(代入)
            // 3. 〜の〜に〜を(代入, 設定)
            if phrase.hasParticle(.WO) {
                if i == 0 { return phrase }
                if i > 0, phrases[i-1].hasParticle(.NI) {
                    return phrase
                }
            }
            // 4. 〜の〜を〜に(設定)
            if phrase.hasParticle(.NI) {
                if i > 0, phrases[i-1].hasParticle(.WO) {
                    return PhraseExpression(token: Token(.WO), left: phrase.left)
                }
            }
        }
        return nil
    }
    /// 要素代入(Genitive Expressionを正規化)
    func buildGenitiveAssignment(from genitive: GenitiveExpression) -> Statement? {
        // 代入値のStatementを[Setence]に変換
        var sentences: [Sentence] = convertToSeteneces(from: genitive.value)
        // 指定位置(〜は → 〜に)
        guard let phrase = genitive.right as? PhraseExpression else {return nil}
        let position = PhraseExpression(token: Token(.NI), left: phrase.left)
        //
        if let target = genitive.left as? Identifier {
            // 変数に代入
            let assignSentence = AssignmentSentence(
                token: genitive.token,
                kind: .simple,
                referent: target,
                attribute: position,
                value: nil,
                string: genitive.string
            )
            sentences.append(assignSentence)
        } else {
            // 代入したオブジェクトを返す
            let simpleSentence = SimpleSentence(
                token: Token(.ASSIGN),
                auxiliaryVerb: .none,
                arguments: [genitive.leftPhrase, position],
                predicateKind: .builtin,
                string: genitive.string
            )
            sentences.append(simpleSentence)
        }
        guard let token = sentences.first?.token else {return nil}
        resolveAssignIdentifierRole(in: sentences)
        return CompoundStatement(
            token: token,
            sentences: sentences,
            string: genitive.string.withPeriod
        )
    }
    func convertToSeteneces(from valueStatement: Statement?) -> [Sentence] {
        switch valueStatement {
        case let es as ExpressionStatement:
            return [es]
        case let ss as SimpleSentence:
            return [ss]
        case let cs as CompoundStatement:
            return cs.sentences
        default:
            return []
        }
    }
    func rebuild(_ sentences: [Sentence]) -> [Sentence] {
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
                    error(message: "助詞「か」の前は、真偽値を返す文が必要です。", at: phrase.left.token)
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
    // 識別子の役割が「指定子」であるものを確定する
    func resolveSpecifierRole(in sentences: [Sentence]) {
        sentences.forEach { $0.resolveSpeicifierRole() }
    }
    // 要素代入の識別子の役割を解決する。(型がわからない場合は、unredolved)
    func resolveAssignIdentifierRole(in sentences: [Sentence]) {
        sentences.forEach { $0.resolveIdentifierRole() }
    }
    /// 〜かによって、のチェック
    func validateQuestionPlacement(from es: ExpressionStatement) {
        let exprs = es.expressions
        guard exprs.count >= 2 else { return }
        if let phrase = exprs[1] as? PhraseExpression,
           phrase.hasKeyword(.QUESTION) {
            error(message: "助詞「か」の前は、真偽値を返す文が必要です。", at: phrase.left.token)
        }
    }
    /// 文中制約チェック
    func validateSentenceSequence(_ sentences: [Sentence]) {
        for (i, sentence) in sentences.dropLast().enumerated() {
            let isNextTerminalConnector = i < sentences.count - 1 && sentences[i + 1].isTerminalConnector   // 次の語が終端(Terminal)に繋がる
            if sentence.terminality == .terminal && !isNextTerminalConnector {
                let identifierDetected = sentence.isIdentifierOnlySentence ?
                    "識別子「\(sentence.tokenLiteral)」は終止形の文として解析されました。\n" : ""
                error(message: identifierDetected + "終止形の文の後に、文を続けることはできません。", at: sentence.token)
                return
            }
            if let assignment = sentence as? AssignmentSentence,
               assignment.kind == .compound {
                error(message: "複合代入文では、文末以外に代入を書くとことができません。", at: sentence.token)
                return
            }
        }
    }
    /// 文末チェック
    func validateSentenceEnd(_ sentences: [Sentence], terminator: SentenceTerminator) {
        guard
            terminator.isExplicit,
            let sentence = sentences.last,
            sentence.terminality == .conjunctive
        else {
            return
        }
        error(message: "連用形の文を「\(terminator.rawValue)」で終えることはできません。", at: sentence.token)
    }
    /// 複合代入文のチェック
    func validateCompoundAssignment(sentences: [Sentence], slices: [SentenceSlice]) {
        guard let assignment = sentences.last as? AssignmentSentence else {return}
        // 単文代入の構文エラーチェック
        if sentences.count == 1 && assignment.kind == .compound {
            error(message: "代入先が見つかりません。", at: assignment.token)
            /* 例： aを代入 ← 文頭に識別子があるのが、単文のためエラー */
            return
        }
        guard slices.count > 1 else {return}
        // 複合代入文のvalidate
        let previousSetence = sentences[sentences.count - 2]
        let previousSlice = slices[slices.count - 2]
        
        guard previousSlice.trailingParticle == .TE else {
            if assignment.kind == .compound {
                error(message: "複合代入文では「て」を省略できません。", at: previousSetence.token)
            }
            return
        }
        if assignment.kind == .simple {
            error(message: "複合代入文では、代入先は文頭で指定します。", at: assignment.referent.token)
            return
        }
        if sentences.dropLast().contains(where: {$0 is AssignmentSentence}) {
            error(message: "複合代入文では、文末以外に代入を書くとことができません。", at: assignment.token)
            return
        }
        if !previousSetence.token.isValuePredicate {
            error(message: "代入直前の文が、値を出力していません。", at: previousSetence.token)
            return
        }
    }
}
extension Sentence {
    func resolveIdentifierRole() {}
    func resolveSpeicifierRole() {
        guard predicate?.token.hasSpecifier == true else { return }
        arguments
            .compactMap { $0 as? PhraseExpression }
            .compactMap { $0.left as? Identifier }
            .filter { $0.isSpecifier }
            .forEach { $0.role = .specifier }
        /* predicateとargumentsは、Sentenceが実装 */
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
    func resolveIdentifierRole() {
        if token.isKeyword(.SET) {
            let ident = findSetElement(in: arguments)
            ident?.role = .element
            return
        }
        guard token.hasAssignTarget else { return }
        if token.isKeyword(.ASSIGN),
           let (left, rightIdent) = findGenitiveLeftAndRightIdentifier(in: arguments) {
            // 属格(要素)代入の右項の役割を決める
            if rightIdent.isSpecifier {
                rightIdent.role = determineRoleForRightIdentifier(genitiveLeft: left)
            }
            return
        }
        // 左辺代入対象の識別子の役割(.assignTarget)を決める
        let identifiers = findAssignTargets(in: arguments)
        identifiers.forEach { $0.role = .assignTarget }
    }
}
private extension SimpleSentence {
    /// 引数から「aのbに」を探し、aとbを返す。
    func findGenitiveLeftAndRightIdentifier(in args: [any Expression]) -> (genitive: any Expression, ident: Identifier)? {
        for (i, arg) in args.enumerated() {
            guard let genitive = arg as? PhraseExpression,
                  genitive.hasParticle(.NO)
            else { continue }
            let nextIndex = i + 1
            guard nextIndex < args.count,
                  let niPhrase = args[nextIndex] as? PhraseExpression,
                  niPhrase.hasParticle(.NI)
            else { continue }
            guard let ident = niPhrase.left as? Identifier else { continue }
            return (genitive.left, ident)
        }
        return nil
    }
    /// 属格の左項が配列であれば、識別子は「指定子」、それ以外は「未解決」
    func determineRoleForRightIdentifier(genitiveLeft: any Expression) -> IdentifierRole {
        if genitiveLeft is ArrayLiteral {
            return .specifier
        }
        return .unresolved
    }
    /// 代入の対象となる識別子を返す。
    func findAssignTargets(in args: [any Expression]) -> [Identifier] {
        var identifiers: [Identifier] = []
        for arg in args {
            guard let phrase = arg as? PhraseExpression,
                  let ident = phrase.left as? Identifier
            else { continue }
            switch phrase.token {
            case Token(.TO):
                identifiers.append(ident)
                continue
            case Token(.NI):
                identifiers.append(ident)
            default:
                continue
            }
            break
        }
        return identifiers
    }
    // 設定の対象となる要素の識別子を返す
    func findSetElement(in args: [any Expression]) -> Identifier? {
        for (i, arg) in args.enumerated() {
            guard let noPhrase = arg as? PhraseExpression, noPhrase.hasParticle(.NO)
            else { continue }
            let nextIndex = i + 1
            guard nextIndex < args.count, let nextPhrase = args[nextIndex] as? PhraseExpression
            else { continue }
            switch nextPhrase.token {
            case Token(.NI), Token(.WO):
                if let ident = nextPhrase.left as? Identifier { return ident }
            default:
                continue
            }
        }
        return nil
    }
}
extension AssignmentSentence {
    func resolveIdentifierRole() {
        // 1.aに代入
        if attribute == nil && kind == .simple {    // 単純代入
            referent.role = .assignTarget
            return
        }
        // 2.aのbに代入
        guard let phrase = attribute as? PhraseExpression,
              let rightIdent = phrase.left as? Identifier
        else { return }
        // 属格(要素)代入の右項の役割は、.unresolved(左項の型が不明)
        if rightIdent.isSpecifier {
            rightIdent.role = .unresolved
        }
    }
}
/// 旧AST互換インタフェース
extension ExpressionStatement {
    var predicate: (any Expression)? {
        guard let idx = predicateIndex else { return nil }
        let e = expressions[idx]
        if let p = e as? PhraseExpression, p.left.isPredicate {
            return p.left
        }
        return e
    }
    var arguments: [any Expression] {
        guard let idx = predicateIndex else { return expressions }
        if idx == 0 { return [] }
        return Array(expressions.prefix(idx))
    }
    var expressionCount: Int {expressions.count}
    var literal: Expression? {expressions.first}
    //
    private var predicateIndex: Int? {
        for (i, e) in expressions.enumerated() {
            if e.isPredicate { return i }
            if let p = e as? PhraseExpression,
               p.left is PredicateExpression { return i }
        }
        return nil
    }
}
extension DefineStatement {
    var rhsCount: Int {value.expressionCount}
    var rhsLiteral: Expression? {value.literal}
}
