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
protocol Statement : Node {}

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
}
protocol Sentence : Statement {
    var token: Token {get}
    var firstToken: Token? {get}
    var auxiliaryVerb: AuxiliaryVerb {get}
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
extension Expression {
    var isPredicate: Bool {false}
    var isAssignment: Bool {hasKeyword(.ASSIGN)}
    var isNominalized: Bool {hasKeyword(.MONO)}
    var isTerminalCandidate: Bool {false}
    var isConjunctiveForm: Bool {false}
    func hasParticle(_ p: Token.Particle) -> Bool {false}
    var sentenceParticle: Token.Particle? {nil}
}
extension ValueExpression {
    var tokenLiteral: String {token.literal}
}
extension Sentence {
    var isIdentifierOnlySentence: Bool {false}
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
    func punctuation(for lhs: Sentence, and rhs: Sentence) -> String {
        // 句読点不要
        if rhs.firstToken?.isPuncuationCanceler == true {
            return ""
        }
        // 読点必要
        if rhs.firstToken?.isConjunction == true {
            return Token.Symbol.COMMA.rawValue
        }
        // 文末による句読点
        return lhs.punctuation
    }
}
extension Array where Element == Expression {
    func toStringWithComma(
        includeTrailingComma: Bool = false,
        insertCommaAfter shouldInsert: (Expression) -> Bool
    ) -> String {
        guard !isEmpty else { return "" }
        return enumerated().map { index, expr in
            let isLast = index == count - 1
            if shouldInsert(expr) && (includeTrailingComma || !isLast) {
                return expr.string + "、"
            }
            return expr.string
        }.joined()
    }
    var toStringWithComma: String {
        toStringWithComma {$0.isNominalized || $0 is (any ValueExpression)}
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
    let token: Token                // とは、は、
    let name: Identifier            // 識別子
    let value: ExpressionStatement  // 値(複数の式)
    var isExtended: Bool = false    // 拡張(多重)識別
    init(token: Token, name: Identifier, value: ExpressionStatement, isExtended: Bool = false) {
        self.token = token
        self.name = name
        self.value = value
        self.isExtended = isExtended
    }
    //
    var tokenLiteral: String {token.literal}
    var string: String {name.string + tokenLiteral + "、" + (isExtended ? (Self.further + "、") : "") +
        value.expressions.reduce("") {$0 + $1.string} + (token.isParticle(.TOWA) ? "のこと。" : "。")}
    static let wa = "は"
    static let towa = "とは"
    static let further = "さらに"
    static let koto = "こと"          //　省略可
    static let dearu = "である"        // 省略可
    static let desu = "です"          // 代替可
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
    var string: String {
        let s = expressions.reduce("") {$0 + $1.string + ($1 is PredicateExpression ? "、" : "")} + "。"
        return s.replacingOccurrences(of: "。】", with: "】")
            .replacingOccurrences(of: "、】", with: "】")
            .replacingOccurrences(of: "、。", with: "。")
            .replacingOccurrences(of: "が。", with: "が、")
            .replacingOccurrences(of: "、場合", with: "場合")
            .replacingOccurrences(of: "、する", with: "する")
            .replacingOccurrences(of: "、し", with: "し")
            .replacingOccurrences(of: "、ない", with: "ない")
    }
    static let yousoga = "要素が、"
    static let yousowa = "要素は、"
    static let hontaiga = "本体が、"
    static let hontaiwa = "本体は、"
    static let deatte = "であって、"
    static let deari = "であり、"
    static let ga = "が"
    static let wa = "は"
    static let to = "と"
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
            guard i < sentences.count - 1 else {continue}
            let next = sentences[i + 1]
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
final class RangeLiteral : Expression {
    let token: Token                // 範囲トークン
    let lowerBound: ExpressionStatement?    // 下限式(例：1以上）
    let upperBound: ExpressionStatement?    // 上限式(例：100以下、100未満)
    init(token: Token, lowerBound: ExpressionStatement? = nil, upperBound: ExpressionStatement? = nil) {
        self.token = token
        self.lowerBound = lowerBound
        self.upperBound = upperBound
    }
    //
    var tokenLiteral: String {token.literal}
    var string: String {token.coloredLiteral + "【" +
        (lowerBound.map {string(of: $0)} ?? "") + comma +
        (upperBound.map {string(of: $0)} ?? "") + "】"}
    private func string(of es: ExpressionStatement) -> String {
        es.expressions.reduce("") {$0 + $1.string} + es.tokenLiteral
    }
    private var comma: String {(lowerBound != nil && upperBound != nil) ? "、" : ""}
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
    let value: ExpressionStatement? // 値
    init(token: Token, left: Expression, right: Expression, value: ExpressionStatement? = nil) {
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
    static let syoriga = "処理が、"
    static let syoriwa = "処理は、"
    let token: Token                // .LOOPキーワード(反復)
    let parameters: [Identifier]    // カウンターまたは要素
    let condition: [Expression]     // 条件式
    let body: BlockStatement
    init(token: Token, parameters: [Identifier], condition: [Expression], body: BlockStatement) {
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
        (condition.isEmpty ? "" : "条件が、\(condition.map {$0.string}.joined(separator: "、"))間、") +
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
    static let joukouga = "条項が、"
    static let joukouwa = "条項は、"
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
    static let arguments = "引数が、"
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
        (arguments.isEmpty ? "" : Self.arguments + arguments.reduce("") {$0 + $1.string.withoutComma}) + "】"
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
