//
//  Abstract Syntax Tree
//  ast.swift
//  日本語ふぉーす(JPForce)
//
//  Created by 佐藤貴之 on 2023/02/21.
//
import Foundation

// MARK: interfaces
protocol Node : Evaluatable, Compilable {
    var tokenLiteral: String {get}
    var string: String {get}
}
protocol Statement : Node {
    var predicate: Expression? {get}
    var arguments: [Expression] {get}
    var expressionCount: Int {get}
    var literal: Expression? {get}
    var isTerminalCandidate: Bool {get}
    var isConjunctiveForm: Bool {get}
}

protocol Expression : Node {
    var token: Token {get}
    var isPredicate: Bool {get}
    var isAssignment: Bool {get}
    var isTerminalCandidate: Bool {get}
    var isConjunctiveForm: Bool {get}
    var auxiliaryVerb: AuxiliaryVerb {get}
    func hasKeyword(_ k: Token.Keyword) -> Bool
    func hasParticle(_ p: Token.Particle) -> Bool
    var sentenceToken: Token {get}
    var sentenceParticle: Token.Particle? {get}
    var isConjunction: Bool {get}
}
protocol Sentence : Statement {
    var token: Token {get}
    var firstToken: Token? {get}
    var auxiliaryVerb: AuxiliaryVerb {get}
    var terminality: SentenceTerminality { get }
    var baseString: String {get}
    var isIdentifierOnlySentence: Bool {get}
    var isTerminalConnector: Bool {get}
}
//
protocol ValueExpression : Expression, Equatable {
    associatedtype ValueType : Equatable
    var token: Token {get}
    var value: ValueType {get}
}
// MARK: - implements
extension Node {
    var string: String {tokenLiteral}
}
extension Statement {
    var predicate: Expression? {nil}
    var arguments: [Expression] {[]}
    var expressionCount: Int {0}
    var literal: Expression? {nil}
}
extension Expression {
    var isPredicate: Bool {false}
    var isAssignment: Bool {hasKeyword(.ASSIGN)}
    func hasParticle(_ p: Token.Particle) -> Bool {false}
    var sentenceParticle: Token.Particle? {nil}
}
extension ValueExpression {
    var tokenLiteral: String {token.literal}
}
extension Sentence {
    // 句読点(pretty-print用)
    var string: String {
        return baseString + punctuation
    }
    var baseString: String {""}
    var punctuation: String {
        switch terminality {
        case .terminal:     return Token.Symbol.PERIOD.rawValue
        case .conjunctive:  return Token.Symbol.COMMA.rawValue
        default:            return ""
        }
    }
}
private extension CompoundStatement {
    func punctuation(for lhs: Sentence, and rhs: Sentence?) -> String {
        // 句読点不要
        if rhs?.firstToken?.isPuncuationCanceler == true {
            return ""
        }
        // 読点必要
        if rhs?.firstToken?.isConjunction == true {
            return Token.Symbol.COMMA.rawValue
        }
        // 文末による句読点
        return lhs.punctuation
    }
}
extension Array where Element == Expression {
    var toStringWithComma: String {
        var s = ""
        for (i, expr) in enumerated() {
            s += expr.string
            if i + 1 < count {
                s += comma(between: expr, and: self[i + 1])
            }
        }
        return s
    }
    private func comma(between lhs: Expression, and rhs: Expression) -> String {
        return needsComma(between: lhs, and: rhs) ? Token.Symbol.COMMA.rawValue : ""
    }
    private func needsComma(between lhs: Expression, and rhs: Expression) -> Bool {
        // 1) 直前が連用形(直後の「ない」は除く)
        if lhs.isConjunctiveForm && !rhs.token.isKeyword(.NOT) { return true }
        // 2) 直前が接続詞（読点必須）
        if lhs.isConjunction { return true }
        // 3) 「が」句の後に特定の構造が続く場合
        if needsCommaAfterGa(lhs: lhs, rhs: rhs) { return true }
        // 4) 「で」句の後に述語でないものが続く場合
        if needsCommaAfterDe(lhs: lhs, rhs: rhs) { return true }
        // 5) 次が接続詞（読点必須）
        if rhs.isConjunction { return true }
        return false
    }
    private func needsCommaAfterGa(lhs: Expression, rhs: Expression) -> Bool {
        guard lhs.hasParticle(.GA) else { return false }
        // 〜の場合（属格の右辺が CaseExpression）
        if let genitive = rhs as? GenitiveExpression, genitive.right is CaseExpression {
            return true
        }
        // 選択肢の「または」
        if rhs is OrExpression { return true }
        return false
    }
    private func needsCommaAfterDe(lhs: Expression, rhs: Expression) -> Bool {
        // 「で」の後に述語でないものが続く場合は読点
        return lhs.hasParticle(.DE) && !rhs.isPredicate
    }
}
// MARK: Program(プログラム)
final class Program : Statement {
    let statements: [Statement]
    init() {self.statements = []}
    init(statements: [Statement]) {self.statements = statements}
    //
    var tokenLiteral: String {statements.first?.tokenLiteral ?? ""}
    var string: String {statements.reduce("") {$0 + $1.string}}
}
// MARK: Statement(文)
// 値を返さないノード
final class DefineStatement : Statement {
    let token: Token                // は、
    let name: Identifier            // 識別子
    let value: Statement            // 値(計算式)
    var isExtended: Bool = false    // 拡張(多重)識別
    init(token: Token, name: Identifier, value: Statement, isExtended: Bool = false) {
        self.token = token
        self.name = name
        self.value = value
        self.isExtended = isExtended
    }
    //
    var tokenLiteral: String {token.literal}
    var string: String {name.string + tokenLiteral + "、" + (isExtended ? (Self.further + "、") : "") +
        value.string}
    static let wa = "は"
    static let further = "さらに"
}
final class ExpressionStatement : Sentence {
    let token: Token                // 式の最後のトークン
    let expressions: [Expression]   // 式
    let terminator: SentenceTerminator
    init(token: Token, expressions: [Expression], terminator: SentenceTerminator = .none) {
        self.token = token
        self.expressions = expressions
        self.terminator = terminator
    }
    //
    var tokenLiteral: String {token.literal}
    var baseString: String {expressions.toStringWithComma}
    //
    static let youso = "要素"
    static let hontai = "本体"
    static let deatte = "であって、"
    static let deari = "であり、"
    static let ga = "が"
    static let wa = "は"
}
final class BlockStatement : Statement {
    let token: Token                // 【トークン
    var statements: [Statement]
    init(token: Token, statements: [Statement]) {
        self.token = token
        self.statements = statements
    }
    //
    var tokenLiteral: String {token.literal}
    var string: String {String((statements.reduce("") {$0 + $1.string}).withoutPunctuation)}
    var stringWithBracket: String {"【" + string + "】"}
}
extension String {
    var withoutPunctuation: Self {
        last == "。" || last == "、" ? String(dropLast()) : self
    }
}
extension BlockStatement {
    var containsLogicalExpression: Bool {
        statements.contains {
            if let exprStmt = $0 as? ExpressionStatement {
                return exprStmt.expressions.contains { $0 is LogicalExpression }
            }
            return false
        }
    }
}
// MARK: Sentence(節)
// 述語を含む単文、複文
final class CompoundStatement : Statement {
    let token: Token
    let sentences: [Sentence]
    init(token: Token, sentences: [Sentence]) {
        self.token = token
        self.sentences = sentences
    }
    //
    var tokenLiteral: String {token.literal}
    var string: String {
        var string = ""
        for (i, sentence) in sentences.enumerated() {
            string += sentence.baseString
            let next = (i < sentences.count - 1) ? sentences[i + 1] : nil
            string += punctuation(for: sentence, and: next) // 句読点
        }
        return string
    }
}
final class SimpleSentence : Sentence {
    let token: Token                    // 述語(または識別子)
    let auxiliaryVerb: AuxiliaryVerb
    let arguments: [Expression]
    let predicateKind: SentencePredicateKind
    init(token: Token,
         auxiliaryVerb: AuxiliaryVerb = .none,
         arguments: [Expression],
         predicateKind: SentencePredicateKind,
         string: String
    ) {
        self.token = token
        self.auxiliaryVerb = auxiliaryVerb
        self.arguments = arguments
        self.predicateKind = predicateKind
        self.baseString = string
    }
    //
    var tokenLiteral: String {token.literal}
    var baseString: String
}
enum AssignmentKind {
    case simple                     // 単純代入
    case compound                   // 複合代入文
}
final class AssignmentSentence : Sentence {
    let token: Token                // 代入
    let auxiliaryVerb: AuxiliaryVerb
    let kind: AssignmentKind
    let target: Identifier          // 左辺
    let arguments: [Expression]     // 右辺候補
    init(token: Token,
         auxiliaryVerb: AuxiliaryVerb = .none,
         kind: AssignmentKind,
         target: Identifier,
         arguments: [Expression],
         string: String
    ) {
        self.token = token
        self.auxiliaryVerb = auxiliaryVerb
        self.kind = kind
        self.target = target
        self.arguments = arguments
        self.baseString = string
    }
    //
    var tokenLiteral: String {token.literal}
    var baseString: String
}
// MARK: Expressions(式)
// 値を返すノード
final class Identifier : Expression {
    let token: Token                // 識別子(.IDENT(value))トークン
    let value: String               // 値(識別子名)
    let auxiliaryToken: Token?      // 補助動詞(する)
    var isLhsCandidate: Bool = false// 左辺候補
    var isLhs: Bool = false         // 左辺(代入される側)
    let isOuter: Bool               // 外部識別子
    //
    init(token: Token, value: String, auxiliaryToken: Token?, isLhs: Bool = false, isOuter: Bool = false) {
        self.token = token
        self.value = value
        self.auxiliaryToken = auxiliaryToken
        self.isLhs = isLhs
        self.isOuter = isOuter
    }
    convenience init(from string: String, with auxiliaryToken: Token? = nil, isOuter: Bool = false) {
        self.init(
            token: Token(ident: string),
            value: string,
            auxiliaryToken: auxiliaryToken,
            isOuter: isOuter
        )
    }
    convenience init(from token: Token, with auxiliaryToken: Token? = nil, isOuter: Bool = false) {
        self.init(
            token: token,
            value: token.literal,
            auxiliaryToken: auxiliaryToken,
            isOuter: isOuter
        )
    }
    //
    var tokenLiteral: String {token.literal}
    var string: String {token.coloredLiteral + (auxiliaryToken?.coloredLiteral ?? "")}
    //
    static let directoryPath = "ディレクトリパス"
}
final class StringLiteral : ValueExpression {
    static func == (lhs: StringLiteral, rhs: StringLiteral) -> Bool {
        lhs.token == rhs.token && lhs.value == rhs.value
    }
    //
    let token: Token                // 文字列(.STRING(value))トークン
    let value: String               // 値(文字列)
    init(token: Token, value: String) {self.token = token; self.value = value}
    convenience init(from string: String) {self.init(token: Token(string: string), value: string)}
    convenience init(from token: Token) {self.init(token: token, value: token.literal)}
    //
    var string: String {"「\(value)」".color(token.color)}
}
final class Label : Expression {
    let token: Token                // ラベル(.keyword())トークン
    let value: Token                // 値(文字列または識別子)
    init(token: Token, value: Token) {self.token = token; self.value = value}
    //
    var tokenLiteral: String {token.literal}
    var string: String {token.coloredLiteral + "「\(value)」".color(token.color)}
}
final class IntegerLiteral : ValueExpression {
    static func == (lhs: IntegerLiteral, rhs: IntegerLiteral) -> Bool {
        lhs.token == rhs.token && lhs.value == rhs.value
    }
    //
    let token: Token                // 数値(.INT(value))トークン
    let value: Int                  // 値(整数)
    init(token: Token, value: Int) {self.token = token; self.value = value}
    convenience init(from integer: Int) {self.init(token: Token(number: integer), value: integer)}
    //
    var string: String {token.coloredLiteral}
}
final class Boolean: ValueExpression {
    static func == (lhs: Boolean, rhs: Boolean) -> Bool {
        lhs.token == rhs.token && lhs.value == rhs.value
    }
    //
    let token: Token                // 真偽値トークン(.TRUEまたは.FALSE)
    let value: Bool
    init(token: Token, value: Bool) {self.token = token; self.value = value}
    convenience init(from bool: Bool) {self.init(token: Token(bool: bool), value: bool)}
    //
    var string: String {tokenLiteral.color(.magenta)}
}
/// 範囲
extension Token {
    var isLower: Bool { [Token(.GTEQUAL), Token(.KARA)].contains(self) }
    var isUpper: Bool { [Token(.LTEQUAL), Token(.MADE), Token(.UNDER)].contains(self) }
}
final class BoundaryExpression : Expression {
    let token: Token            // 以上、以下、未満、から、まで、のトークン
    let sentence: Sentence      // 上下限式
    init(token: Token, sentence: Sentence) {
        self.token = token
        self.sentence = sentence
    }
    var tokenLiteral: String { token.literal }
    var string: String {
        // 例: 1に1を足すから / 100で10を割るまで
        sentence.string.withoutPunctuation + token.coloredLiteral
    }
}
final class RangeLiteral : Expression {
    let token: Token                // 範囲トークン
    let lowerBound: ExpressionStatement?    // 下限式(例：1以上）
    let upperBound: ExpressionStatement?    // 上限式(例：100以下、100未満)
    // 新形式（移行先）
    let lowerBoundary: BoundaryExpression?
    let upperBoundary: BoundaryExpression?
    // 旧: 既存の init を残しつつ
    init(token: Token, lowerBound: ExpressionStatement? = nil, upperBound: ExpressionStatement? = nil) {
        self.token = token
        self.lowerBound = lowerBound
        self.upperBound = upperBound
        self.lowerBoundary = nil
        self.upperBoundary = nil
    }
    // 新: 新形式用の init
    init(token: Token, lower: BoundaryExpression? = nil, upper: BoundaryExpression? = nil) {
        self.token = token
        self.lowerBoundary = lower
        self.upperBoundary = upper
        self.lowerBound = nil
        self.upperBound = nil
    }
    var tokenLiteral: String { token.literal }
    // 新旧両対応の string
    var string: String {
        if lowerBoundary != nil || upperBoundary != nil {
            // 新形式が設定されていれば新形式で表示
            return token.coloredLiteral + "であって、【" + newFormatString + "】"
        } else {
            // フォールバック: 旧形式（従来の表示を維持）
            return token.coloredLiteral + "【" +
                (lowerBound.map { oldFormatString(of: $0) } ?? "") + oldComma +
                (upperBound.map { oldFormatString(of: $0) } ?? "") + "】"
        }
    }
    // 旧形式の表示（従来のまま）
    private func oldFormatString(of es: ExpressionStatement) -> String {
        es.expressions.reduce("") { $0 + $1.string } + es.tokenLiteral
    }
    private var oldComma: String { (lowerBound != nil && upperBound != nil) ? "、" : "" }
    // 新形式の表示
    private var newFormatString: String {
        let lhs = lowerBoundary.map { boundaryString($0) } ?? ""
        let rhs = upperBoundary.map { boundaryString($0) } ?? ""
        let comma = (lowerBoundary != nil && upperBoundary != nil) ? "、" : ""
        return lhs + comma + rhs
    }
    private func boundaryString(_ b: BoundaryExpression) -> String {
        // 例: 1以上 / 10未満 / 3まで 等
        // 式の末尾の句読点は避けたいので withoutPunctuation を活用
        return b.sentence.string.withoutPunctuation + b.token.coloredLiteral
    }
}
/// 句(式+助詞)。助詞(token)はToken.Particle
final class PhraseExpression : Expression {
    let token: Token                // 助詞(postpotional paticle)
    let left: Expression            // 式
    init(token: Token, left: Expression) {
        self.token = token
        self.left = left
    }
    //
    var tokenLiteral: String {token.literal}
    var string: String {left.string + token.coloredLiteral}
}
extension PhraseExpression {
    func hasParticle(_ p: Token.Particle) -> Bool {
        token.isParticle(p)
    }
}
/// 述語。tokenはToken.Keyword, Token.IDENT(_)
final class PredicateExpression : Expression {
    let token: Token                // 述語(predicate keyword)
    let auxiliaryToken: Token?      // 助動詞(する)
    init(token: Token, auxiliaryToken: Token? = nil) {
        self.token = token
        self.auxiliaryToken = auxiliaryToken
    }
    //
    var tokenLiteral: String {token.literal}
    var string: String {token.coloredLiteral + (auxiliaryToken?.coloredLiteral ?? "")}
}
/// 選択肢の「または」
final class OrExpression : Expression {
    let token: Token
    let left: Expression
    let right: Expression
    init(token: Token, left: Expression, right: Expression) {
        self.token = token
        self.left = left
        self.right = right
    }
    //
    var tokenLiteral: String {token.literal}
    var string: String {left.string + "、" + tokenLiteral + "、" + right.string}
}
/// 属格。<オブジェクト>の<要素>(は、<値>。)
final class GenitiveExpression : Expression {
    let token: Token                // 属格(genitive case)
    let left: Expression
    let right: Expression
    let value: Statement?           // 値
    init(token: Token, left: Expression, right: Expression, value: Statement? = nil) {
        self.token = token
        self.left = left
        self.right = right
        self.value = value
    }
    //
    var tokenLiteral: String {token.literal}
    var string: String {
        left.string + tokenLiteral + right.string + (value.map {"、\($0.string)"} ?? "").withoutPeriod
    }
}
// 名詞句 (<形容詞、動詞節>か): 節全体を名詞化
final class NominalizedExpression : Expression {
    let token: Token        // か
    let sentence: Sentence
    init(token: Token, sentence: Sentence) {
        self.token = token
        self.sentence = sentence
    }
    //
    var tokenLiteral: String {token.literal}
    var string: String {sentence.baseString + token.coloredLiteral}
}
/// 属性を取得： その<属性名>
final class PropertyExpression : Expression {
    let token: Token    // その
    let property: Token
    init(token: Token, property: Token) {
        self.token = token
        self.property = property
    }
    var tokenLiteral: String {token.literal}
    var string: String {token.coloredLiteral + property.literal}
}
/// 場合文１： <論理式>場合【<続文>】、それ以外は【<代文>】。
/// 場合文２：<識別子>が<値１>の場合【<文１>】、<値２>の場合【<文２>】、…それ以外は【<代文>】。
final class CaseExpression : Expression {
    static let soreigai = "それ以外"
    let token: Token                // .CASEキーワード(場合)
    let consequence: BlockStatement
    let alternative: BlockStatement?// 「それ以外は」(else)
    init(token: Token, consequence: BlockStatement, alternative: BlockStatement? = nil) {
        self.token = token
        self.consequence = consequence
        self.alternative = alternative
    }
    //
    var tokenLiteral: String {token.literal}
    var string: String {
        token.coloredLiteral + "、【" + consequence.string + "】" +
        (alternative.map {"、" + "それ以外は" + "、【" + $0.string + "】"} ?? "")
    }
}
final class LogicalExpression : Expression {
    let token: Token                // かつ(AND)または、または(OR)
    let right: BlockStatement
    init(token: Token, right: BlockStatement) {
        self.token = token
        self.right = right
    }
    //
    var tokenLiteral: String {token.literal}
    var string: String {
        // 右結合をしている場合、わかりやすい様、明示的に【】を付ける。
        "\(token.coloredLiteral)、" +
        (right.containsLogicalExpression ? right.stringWithBracket : right.string)
    }
}
final class ConditionalOperation : Expression {
    static let ka = "か"
    let token: Token                // .CONDITIONALキーワード(よって)
    let consequence: Expression     // 成立時の値(式)
    let alternative: Expression     // 不成立の値(式)
    init(token: Token, consequence: Expression, alternative: Expression) {
        self.token = token
        self.consequence = consequence
        self.alternative = alternative
    }
    //
    var tokenLiteral: String {token.literal}
    var string: String {
        "\(token.coloredLiteral)、\(consequence.string)か、\(alternative.string)"
    }
}
final class LoopExpression : Expression {
    static let condition = "条件"
    static let aida = "間"
    static let syori = "処理"
    let token: Token                // .LOOPキーワード(反復)
    let parameters: [Identifier]    // カウンターまたは要素
    let condition: Statement?       // 条件式
    let body: BlockStatement
    init(token: Token, parameters: [Identifier], condition: Statement?, body: BlockStatement) {
        self.token = token
        self.parameters = parameters
        self.condition = condition
        self.body = body
    }
    //
    var tokenLiteral: String {token.literal}
    var string: String {
        token.coloredLiteral + "【" +
        (parameters.isEmpty ? "" : "入力が、\(parameters.map {$0.string}.joined(separator: "と、"))であり、") +
        (condition.map {"条件が、\($0.string.withoutPeriod)間、"} ?? "") +
        "処理が、" + body.string + "】"
    }
}
final class FunctionLiteral : Expression {
    let token: Token                // 関数トークン
    var name: String                // 名前
    let function: FunctionBlock     // 関数部
    init(token: Token, function: FunctionBlock, name: String = "") {
        self.token = token
        self.function = function
        self.name = name
    }
    //
    var tokenLiteral: String {token.literal}
    var string: String {"\(token.coloredLiteral)であって、【\(function.string)】"}
}
final class ComputationLiteral : Expression {
    static let settei = "設定"
    static let syutoku = "取得"
    let token: Token                // 算出トークン
    let setters: FunctionBlocks     // 設定ブロック
    let getters: FunctionBlocks     // 取得ブロック
    init(token: Token, setters: FunctionBlocks, getters: FunctionBlocks) {
        self.token = token
        self.setters = setters
        self.getters = getters
    }
    //
    var tokenLiteral: String {token.literal}
    var string: String {
        token.coloredLiteral + "であって、【" +
        (setters.all.reduce("") {$0 + "設定は、\($1.isOverloaded ? "さらに、" : "")【\($1.string)】。"}) +
        (getters.all.reduce("") {$0 + "取得は、\($1.isOverloaded ? "さらに、" : "")【\($1.string)】。"}) +
        "】"
    }
}
final class ProtocolLiteral : Expression {
    static let kiyaku = "規約"
    static let junkyosuru = "準拠する"
    let token: Token                // 規約トークン
    let protocols: [String]         // 準拠する規約
    let clauses: [ClauseLiteral]    // 規約条項
    init(token: Token, protocols: [String], clauses: [ClauseLiteral]) {
        self.token = token
        self.protocols = protocols
        self.clauses = clauses
    }
    //
    var tokenLiteral: String {token.literal}
    var string: String {
        token.coloredLiteral + "であって、【" +
        (protocols.isEmpty ? "" : "準拠する規約は、\(protocols.map {$0}.joined(separator: "と、"))。") +
        "条項が、\(clauses.map {$0.string}.joined(separator: ""))】"
    }
}
enum SignatureKind {
    case function(FunctionSignature)
    case computation(getter: FunctionSignature?, setter: FunctionSignature?)
    case initializer(FunctionSignature)
    case none
}
struct ClauseLiteral {
    static let joukou = "条項"
    //
    let isTypeMember: Bool          // 型の要素
    let identifier: Identifier      // 識別子
    let type: String                // 型の文字列
    let kind: SignatureKind         // シグネチャ種別(無しの場合、.none)
    //
    var signature: FunctionSignature? {
        switch kind {
        case .function(let sig): return sig
        case .initializer(let sig): return sig
        default:
            return nil
        }
    }
    var getterSignature : FunctionSignature? {
        guard case .computation(let gsig, _) = kind else { return nil }
        return gsig
    }
    var setterSignature : FunctionSignature? {
        guard case .computation(_, let ssig) = kind else { return nil }
        return ssig
    }
    var string: String {
        switch kind {
        case .function(let sig):
            return subject + type + ExpressionStatement.deatte + "【\(sig.string)】。"
        case .computation(getter: let gsig, setter: let ssig):
            let setter = ssig.map {"設定は、【\($0.string)】。"} ?? ""
            let getter = gsig.map {"取得は、【\($0.string)】。"} ?? ""
            return subject + type + ExpressionStatement.deatte + "【" + setter + getter + "】。"
        case .initializer(let sig):
            return "初期化は、【\(sig.string)】。"
        case .none:
            return subject + "「\(type)」。"
        }
    }
    private var subject: String {   // (型の)<識別子>は、
        isTypeMember ? TypeLiteral.katano : "" + identifier.value + DefineStatement.wa + "、"
    }
}
struct FunctionSignature {          // 関数シグネチャ
    let parameters: [Identifier]    // 引数
    let paramForm: InputFormat      // 引数形式
    let returnTypes: [String]       // 返り値の型
    //
    var string: String {
        (parameters.isEmpty ? "" : "入力が\(zip(parameters, paramForm.strings).map {$0.string + $1}.joined(separator: "と"))。") +
        (returnTypes.isEmpty ? "" : "出力が「\(returnTypes.joined(separator: "」と「"))」。")
    }
}
final class TypeLiteral : Expression {
    static let syokika = "初期化"
    static let typemembers = "型の要素"
    static let katano = "型の"
    //
    let token: Token                // 型トークン
    let protocols: [String]         // 準拠する規約
    let typeMembers: BlockStatement?// 型の要素
    let initializers: FunctionBlocks// 初期化処理
    let body: BlockStatement?       // インスタンスのメンバー
    init(token: Token, protocols: [String], typeMembers: BlockStatement? = nil, initializers: FunctionBlocks, body: BlockStatement? = nil) {
        self.token = token
        self.protocols = protocols
        self.typeMembers = typeMembers
        self.initializers = initializers
        self.body = body
    }
    //
    var tokenLiteral: String {token.literal}
    var string: String {
        var s = token.coloredLiteral + "であって、【" +
        (protocols.isEmpty ? "" : "準拠する規約は、\(protocols.map {$0}.joined(separator: "と、"))であり、") +
        (typeMembers.map {"型の要素が、\($0.string)であり、"} ?? "") +
        (initializers.all.reduce("") {$0 + "初期化は、\($1.isOverloaded ? "さらに、" : "")【\($1.string)】。"}) +
        (body.map {"本体が、\($0.string)"} ?? "") +
        "】"
        s.range(of: "さらに、").map {s.replaceSubrange($0, with: "")}   // 初出の「さらに、」を取り除く
        return s.replacingOccurrences(of: "。】", with: "】")
    }
}
struct InputFormat {
    struct Format {
        let type: String, particle: String, threeDots: String
        var hasThreeDots: Bool {threeDots.isThreeDots}
        var string: String {
            let concat = type + particle + threeDots
            return concat.isEmpty ? "" : "「\(concat)」"
        }
    }
    let numberOfInputs: Int?                        // 期待するパラメータ数(可変の場合はnil)
    let formats: [Format]                           // 期待するパラメータ毎の型と格(無い場合は"")
    let values: [ExpressionStatement?]              // 既定値
    //
    var strings: [String] {
        zip(formats, values).map { format, value in
            let s = format.string + (value != nil ? ExpressionStatement.wa : "") + (value?.string ?? "")
            return s.replacingOccurrences(of: "。", with: "")
        }
    }
}
extension InputFormat {
    var numberOfDefaultValues: Int {values.compactMap {$0}.count}   // 既定値の数
    // 既定値を持たない最初の位置、形式
    var firstParamIndex: Int? {values.firstIndex {$0 == nil}}
    var firstParamFormat: Format? {firstParamIndex.flatMap {formats[$0]}}
}
extension String {
    var threeDots: String {"…"}
    var isThreeDots: Bool {self == threeDots}
    var hasThreeDots: Bool {self.hasSuffix(threeDots)}
    func getThreeDots() -> Self {self.hasSuffix(threeDots) ? threeDots : ""}
    var removedThreeDots: Self {self.hasSuffix(threeDots) ? String(self.dropLast()) : self}
}
final class FunctionBlock {
    static let input = "入力"
    static let output = "出力"
    static let ari = "あり"
    let parameters: [Identifier]    // 入力パラメータ
    let paramForm: InputFormat      // 入力形式
    let returnTypes: [String]       // 出力型
    let body: BlockStatement?       // 処理本体
    var isOverloaded: Bool = false  // 多重識別
    init(parameters: [Identifier], paramForm: InputFormat, returnTypes: [String], body: BlockStatement? = nil, isOverloaded: Bool = false) {
        self.parameters = parameters
        self.paramForm = paramForm
        self.returnTypes = returnTypes
        self.body = body
        self.isOverloaded = isOverloaded
    }
    //
    var string: String {
        let s =
        (parameters.isEmpty ? "" : "入力が、\(zip(parameters, paramForm.strings).map {$0.string + $1}.joined(separator: "と"))であり、") +
        (returnTypes.isEmpty ? "" : "出力が、「\(returnTypes.joined(separator: "」と「"))」であり、") +
        (body.map {"本体が、\($0.string)"} ?? "")
        return s.hasSuffix("であり、") ? String(s.dropLast("であり、".count)) : s
    }
}
//
extension FunctionBlock {
    var rangeOfInputs: ClosedRange<Int> {   // 入力数の範囲
        guard let max = paramForm.numberOfInputs else {return (-1)...(-1)}
        let min = max - paramForm.numberOfDefaultValues
        return min...max
    }
    func index(of name: String) -> Int? {
        parameters.firstIndex {$0.value == name}
    }
    /// デフォルト値を除いた、必須引数の数
    var requiredParameterCount: Int {
        parameters.count - paramForm.numberOfDefaultValues
    }
    /// 入力パラメータ(名前)の型が、指定の型と一致するか
    /// - Parameters:
    ///   - name: 入力パラメータの名前
    ///   - type: 指定の型
    /// - Returns: 一致(true), 不一致もしくは名前が見つからない(false)
    func hasParameter(named name: String,  ofType type: String) -> Bool {
        guard let i = index(of: name) else {
            return false                    // 入力の名前が一致しない
        }
        let t = paramForm.formats[i].type
        return t.isEmpty || t == type       // チェック不要、もしくは型が一致
    }
    /// 指定の名前の可変超識別子があるか
    func hasVariableParameter(named name: String) -> Bool {
        guard let i = index(of: name) else {
            return false                    // 入力の名前が一致しない
        }
        return paramForm.formats[i].hasThreeDots
    }
    func matches(for other: FunctionSignature) -> Bool {
        guard parameters.count == other.parameters.count else {
            return false    // 引数の数が不一致
        }
        guard zip(parameters, other.parameters).allSatisfy({$0.value == $1.value}) else {
            return false    // 引数の識別子名が不一致
        }
        guard paramForm.numberOfInputs == other.paramForm.numberOfInputs else {
            return false    // 引数の形式が不一致
        }
        guard zip(paramForm.formats, other.paramForm.formats).allSatisfy({
            $0.type == $1.type &&  $0.particle == $1.particle}) else {
            return false    // 形式(型と格)が一致
        }
        return returnTypes == other.returnTypes // 返り値の型の一致/不一致
    }
}
final class CallExpression : Expression {
    static let arguments = "引数"
    //
    let token: Token                // トークン
    let target: Expression          // 呼び出し対象
    let arguments: [DefineStatement]// 引数
    init(token: Token, target: Expression, arguments: [DefineStatement]) {
        self.token = token
        self.target = target
        self.arguments = arguments
    }
    //
    var tokenLiteral: String {token.literal}
    var string: String {
        "\(target.string)【" +
        (arguments.isEmpty ? "" : "引数が、" + arguments.reduce("") {$0 + $1.string.withoutComma}) + "】"
    }
}
final class ArrayLiteral : Expression {
    let token: Token                // 配列トークン
    let elements: [ExpressionStatement]
    init(token: Token, elements: [ExpressionStatement]) {
        self.token = token
        self.elements = elements
    }
    //
    var tokenLiteral: String {token.literal}
    var string: String {
        token.coloredLiteral + "であって、【" +
        (elements.isEmpty ? "" : "要素が、\(elements.map {$0.string}.joined(separator: "と、"))".withoutPeriod) + "】"
    }
}
final class DictionaryLiteral : Expression {
    let token: Token                // 辞書トークン
    let pairs: [PairExpression]
    init(token: Token, pairs: [PairExpression]) {
        self.token = token
        self.pairs = pairs
    }
    //
    var tokenLiteral: String {token.literal}
    var string: String {
        token.coloredLiteral + "であって、【" +
        (pairs.isEmpty ? "" : "要素が、\(pairs.map {$0.string}.joined(separator: "と、"))") + "】"
    }
}
struct PairExpression {
    let pair: (key: ExpressionStatement, value: ExpressionStatement)
    var string: String {pair.key.string.withoutPeriod + "が" + pair.value.string.withoutPeriod}
}
final class EnumLiteral : Expression {
    let token: Token                // 列挙トークン
    let elements: [Statement]       // 列挙子(定義文または列挙子)
    init(token: Token, elements: [Statement]) {
        self.token = token
        self.elements = elements
    }
    //
    var tokenLiteral: String {token.literal}
    var string: String {
        token.coloredLiteral + "であって、【" +
        (elements.isEmpty ? "" : "要素が、\(elements.map {$0.string}.joined(separator: "と、"))".withoutPeriod) + "】"
    }
}
final class EnumeratorLiteral : Expression {
    static let dot = "・"
    let token: Token                // 列挙子トークン(type・name)
    let type: String                // 列挙型の名前(空は無し)
    let name: String                // 列挙子名
    init(token: Token, type: String, name: String) {
        self.token = token
        self.type = type
        self.name = name
    }
    //
    var tokenLiteral: String {token.literal}
    var string: String {token.coloredLiteral}
}
