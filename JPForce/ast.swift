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
protocol Expression : Node {}
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
extension ValueExpression {
    var tokenLiteral: String {token.literal}
 }
// MARK: Program(プログラム)
class Program : Statement {
    var statements: [Statement] = []
    init() {}
    init(statements: [Statement]) {self.statements = statements}
    //
    var tokenLiteral: String {statements.first?.tokenLiteral ?? ""}
    var string: String {statements.reduce("") {$0 + $1.string}}
}
// MARK: Statement(文)
class DefineStatement : Statement {
    var token: Token                // とは、は、
    var name: Identifier            // 識別子
    var value: ExpressionStatement  // 値(複数の式)
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
class ExpressionStatement : Statement {
    var token: Token                // 式の最初のトークン
    var expressions: [Expression]   // 式
    init(token: Token, expressions: [Expression]) {
        self.token = token
        self.expressions = expressions
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
    }
    //
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
class BlockStatement: Statement {
    var token: Token                // 【トークン
    var statements: [Statement]
    init(token: Token, statements: [Statement]) {
        self.token = token
        self.statements = statements
    }
    //
    var tokenLiteral: String {token.literal}
    var string: String {statements.reduce("") {$0 + $1.string}}
    var stringWithBracket: String {statements.isEmpty ? "【】" : "【\n\t" + string + "\n】"}
}
// MARK: Expressions(式)
class Identifier : Expression {
    var token: Token                // 識別子(.IDENT(value))トークン
    var value: String               // 値(識別子名)
    var isLhs: Bool = false         // 左辺(代入される側)
    //
    init(token: Token, value: String, isLhs: Bool = false) {self.token = token; self.value = value; self.isLhs = isLhs}
    convenience init(from string: String) {self.init(token: Token(ident: string), value: string)}
    convenience init(from token: Token) {self.init(token: token, value: token.literal)}
    //
    var tokenLiteral: String {token.literal}
    var string: String {token.coloredLiteral}
    //
    static let directoryPath = "ディレクトリパス"
}
class StringLiteral : ValueExpression {
    static func == (lhs: StringLiteral, rhs: StringLiteral) -> Bool {
        lhs.token == rhs.token && lhs.value == rhs.value
    }
    //
    var token: Token                // 文字列(.STRING(value))トークン
    var value: String               // 値(文字列)
    init(token: Token, value: String) {self.token = token; self.value = value}
    convenience init(from string: String) {self.init(token: Token(string: string), value: string)}
    convenience init(from token: Token) {self.init(token: token, value: token.literal)}
    //
    var string: String {"「\(value)」".color(token.color)}
}
class Label : Expression {
    var token: Token                // ラベル(.keyword())トークン
    var value: Token                // 値(文字列または識別子)
    init(token: Token, value: Token) {self.token = token; self.value = value}
    //
    var tokenLiteral: String {token.literal}
    var string: String {token.coloredLiteral + "「\(value)」".color(token.color)}
}
class IntegerLiteral : ValueExpression {
    static func == (lhs: IntegerLiteral, rhs: IntegerLiteral) -> Bool {
        lhs.token == rhs.token && lhs.value == rhs.value
    }
    //
    var token: Token                // 数値(.INT(value))トークン
    var value: Int                  // 値(整数)
    init(token: Token, value: Int) {self.token = token; self.value = value}
    convenience init(from integer: Int) {self.init(token: Token(number: integer), value: integer)}
    //
    var string: String {token.coloredLiteral}
}
class Boolean: ValueExpression {
    static func == (lhs: Boolean, rhs: Boolean) -> Bool {
        lhs.token == rhs.token && lhs.value == rhs.value
    }
    //
    var token: Token                // 真偽値トークン(.TRUEまたは.FALSE)
    var value: Bool
    init(token: Token, value: Bool) {self.token = token; self.value = value}
    convenience init(from bool: Bool) {self.init(token: Token(bool: bool), value: bool)}
    //
    var string: String {tokenLiteral.color(.magenta)}
}
class RangeLiteral : Expression {
    var token: Token                // 範囲トークン
    var lowerBound: ExpressionStatement?    // 下限式(例：1以上）
    var upperBound: ExpressionStatement?    // 上限式(例：100以下、100未満)
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
class PhraseExpression : Expression {
    var token: Token                // 助詞(postpotional paticle)
    var left: Expression            // 式
    init(token: Token, left: Expression) {
        self.token = token
        self.left = left
    }
    //
    var tokenLiteral: String {token.literal}
    var string: String {left.string + token.coloredLiteral}
}
/// 述語。tokenはToken.Keyword, Token.IDENT(_)
class PredicateExpression : Expression {
    var token: Token                // 述語(predicate keyword)
    init(token: Token) {self.token = token}
    //
    var tokenLiteral: String {token.literal}
    var string: String {token.coloredLiteral}
}
class OrExpression : Expression {
    var token: Token
    var left: Expression
    var right: Expression
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
class GenitiveExpression : Expression {
    var token: Token                // 属格(genitive case)
    var left: Expression
    var right: Expression
    var value: ExpressionStatement? // 値
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
class CaseExpression : Expression {
    static let soreigai = "それ以外"
    var token: Token                // .CASEキーワード(場合)
    var consequence: BlockStatement
    var alternative: BlockStatement?// 「それ以外は」(else)
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
class LogicalExpression : Expression {
    var token: Token                // かつ(AND)または、または(OR)
    var right: BlockStatement
    init(token: Token, right: BlockStatement) {
        self.token = token
        self.right = right
    }
    //
    var tokenLiteral: String {token.literal}
    var string: String {"\(token.coloredLiteral)、【\(right.string)】"}
}
class ConditionalOperation : Expression {
    static let ka = "か"
    var token: Token                // .CONDITIONALキーワード(よって)
    var consequence: Expression     // 成立時の値(式)
    var alternative: Expression     // 不成立の値(式)
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
class LoopExpression : Expression {
    static let condition = "条件"
    static let aida = "間"
    static let syoriga = "処理が、"
    static let syoriwa = "処理は、"
    var token: Token                // .LOOPキーワード(反復)
    var parameters: [Identifier]    // カウンターまたは要素
    var condition: [Expression]     // 条件式
    var body: BlockStatement
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
class FunctionLiteral : Expression {
    var token: Token                // 関数トークン
    var name = ""                   // 名前
    var function: FunctionBlock     // 関数部
    init(token: Token, function: FunctionBlock, name: String = "") {
        self.token = token
        self.function = function
        self.name = name
    }
    //
    var tokenLiteral: String {token.literal}
    var string: String {"\(token.coloredLiteral)であって、【\(function.string)】"}
}
class ComputationLiteral : Expression {
    static let settei = "設定"
    static let syutoku = "取得"
    var token: Token                // 算出トークン
    var setters: FunctionBlocks     // 設定ブロック
    var getters: FunctionBlocks     // 取得ブロック
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
class ProtocolLiteral : Expression {
    static let kiyaku = "規約"
    static let junkyosuru = "準拠する"
    var token: Token                // 規約トークン
    var protocols: [String]         // 準拠する規約
    var clauses: [ClauseLiteral]    // 規約条項
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
    var isTypeMember: Bool = false  // 型の要素
    var identifier: Identifier      // 識別子
    var type: String                // 型の文字列
    var kind: SignatureKind         // シグネチャ種別(無しの場合、.none)
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
    var parameters: [Identifier]    // 引数
    var paramForm: InputFormat      // 引数形式
    var returnTypes: [String]       // 返り値の型
    //
    var string: String {
        (parameters.isEmpty ? "" : "入力が\(zip(parameters, paramForm.strings).map {$0.string + $1}.joined(separator: "と"))。") +
        (returnTypes.isEmpty ? "" : "出力が「\(returnTypes.joined(separator: "」と「"))」。")
    }
}
class TypeLiteral : Expression {
    static let syokika = "初期化"
    static let typemembers = "型の要素"
    static let katano = "型の"
    var token: Token                // 型トークン
    var protocols: [String]         // 準拠する規約
    var typeMembers: BlockStatement?// 型の要素
    var initializers: FunctionBlocks// 初期化処理
    var body: BlockStatement?       // インスタンスのメンバー
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
    var numberOfInputs: Int?                        // 期待するパラメータ数(可変の場合はnil)
    var formats: [Format]                           // 期待するパラメータ毎の型と格(無い場合は"")
    var values: [ExpressionStatement?]              // 既定値
    //
    var strings: [String] {
        zip(formats, values).map { format, value in
            let s = format.string + (value != nil ? ExpressionStatement.wa : "") + (value?.string ?? "")
            return s.replacingOccurrences(of: "。", with: "")
        }
    }
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
    var parameters: [Identifier]    // 入力パラメータ
    var paramForm: InputFormat      // 入力形式
    var returnTypes: [String]       // 出力型
    var body: BlockStatement?       // 処理本体
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
    //
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
/// 必要入力数ごとの関数ブロック配列(多重定義)
final class FunctionBlocks {
    private static let variadicArity = -1
    private var dictionary: [Int : [FunctionBlock]] = [:]   // 引数数毎の関数ブロック
    private var array: [FunctionBlock] = []                 // 関数ブロック定義順
    private var keyword: Token.Keyword? = nil               // 予約語多重定義
    //
    init() {}
    init(by keyword: Token.Keyword) {self.keyword = keyword}
    /// 関数定義で初期化
    init(_ functionBlock: FunctionBlock) {_ = self.append(functionBlock)}
    /// 必要入力数毎の多重定義配列を返す。(無い場合は空)
    subscript(index: Int) -> [FunctionBlock] {dictionary[index] ?? []}
    /// 関数定義を多重定義(自身)に追加する。
    /// - Parameter functionBlock: 追加する関数ブロック (isOverloaded==falseならば上書き)
    /// - Returns: 追加した自身を返す
    func append(_ functionBlock: FunctionBlock) -> Self {
        functionBlock.rangeOfInputs.forEach {
            register(functionBlock, for: $0)
        }
        if array.isEmpty || functionBlock.isOverloaded {
            array.append(functionBlock)
        }
        return self
    }
    /// 多重定義を多重定義(自身)に追加する。
    /// - Parameter functionBlocks: 追加する多重定義 (hasRedefineならば上書き)
    /// - Returns: 追加した自身を返す
    func append(_ functionBlocks: FunctionBlocks) -> Self {
        if functionBlocks.hasRedefinition {
            applyRedefinition(from: functionBlocks)
        } else {
            applyOverloads(from: functionBlocks)
        }
        return self
    }
    // array/dictionary/keywordを隠蔽
    var all: [FunctionBlock] {array}
    var count: Int {array.count}
    var isEmpty: Bool {array.isEmpty}
    var hasDefinitions: Bool {!array.isEmpty}
    /// 最新定義を取得する
    var latest: FunctionBlock? {array.last}
    var single: FunctionBlock? {array.first}
    /// 単体定義かどうか
    var isSingleDefinition: Bool {array.count == 1}
    /// 指定入力数を受け付ける可能性があるか
    func accepts(numberOfInputs n: Int) -> Bool {
        dictionary.keys.contains(n) ||
        dictionary.keys.contains(Self.variadicArity)
    }
    var isRedefinedPredicate: Bool {keyword != nil}
    //
    @discardableResult
    func markAsOverloaded() -> Self {
        self.array.forEach {$0.isOverloaded = true}
        return self
    }
    @discardableResult
    func redefinePredicate(by keyword: Token.Keyword) -> Self {
        self.keyword = keyword
        return self
    }
    /// 再定義している(多重定義でない関数ブロックが含まれる)
    var hasRedefinition: Bool {array.contains {!$0.isOverloaded}}
    /// 多重定義に、指定のシグネチャと一致する関数ブロックがあるか？
    /// - Parameter signature: 関数シグネチャ
    /// - Returns: シグネチャが一致ならば、true
    func hasSameSignature(for signature: FunctionSignature) -> Bool {
        array.reversed().contains {$0.matches(for: signature)}
    }
    /// 入力と引数の形式が一致する関数ブロックを得る。(call expression用)
    /// - Parameter env: 引数をもつ環境
    /// - Returns: 関数ブロック(無ければnil)
    func resolve(with env: Environment) -> FunctionBlock? {
        let pairs = env.parameterPairs          // 引数の名前(k)と値(v)の組
        let vFunctions = dictionary[Self.variadicArity] // 可変長の定義
        let fFunctions = dictionary[pairs.count]// 固定長の定義
        switch (vFunctions, fFunctions) {
        case let (v?, f?):                      // 両方チェック
            if let function = match(in: v, with: pairs) {return function}
            return match(in: f, with: pairs)
        case let (f?, nil), let (nil, f?):      // 片方チェック
            return match(in: f, with: pairs)
        default:
            return nil
        }
    }
    func operateBuiltinPredicate(with env: Environment) -> JpfObject? {
        guard let keyword else {
            preconditionFailure("operateBuiltinPredicate called without keyword")
        }
        let predicate = PredicateOperableFactory.create(from: keyword, with: env)
        return predicate.operate()
    }
    /// 候補の関数ブロック配列から、指定引数(名前と値)形式を持つ関数ブロックを返す。
    /// - Parameters:
    ///   - functions: 候補の関数ブロック配列
    ///   - pairs: 指定引数(名前と値)
    /// - Returns: 関数ブロック(見つからなければnil)
    private func match(in functions: [FunctionBlock], with pairs: [(String, JpfObject)]) -> FunctionBlock? {
        functions: for f in functions.reversed() {
            arguments: for (k, v) in pairs {
                if f.hasVariableParameter(named: k) {// 可変長識別子の値の型は配列
                    guard v.type == JpfArray.type else {continue functions}
                } else {                            // 固定長識別子の値の型は引数の型
                    guard f.hasParameter(named: k,  ofType: v.type) else {continue functions}
                }
            }
            return f                                // 引数名と型が全て一致
        }
        return nil                                  // 一致する関数が無い
    }
    //
    private func register(_ block: FunctionBlock, for arity: Int) {
        if dictionary[arity] != nil && block.isOverloaded {
            dictionary[arity]!.append(block)
        } else {
            dictionary[arity] = [block]
        }
    }
    private func applyRedefinition(from other: FunctionBlocks) {
        for (arity, blocks) in other.dictionary {
            dictionary[arity] = blocks
        }
        array = other.array
    }
    private func applyOverloads(from other: FunctionBlocks) {
        for (arity, blocks) in other.dictionary {
            if dictionary[arity] != nil {
                dictionary[arity]! += blocks
            } else {
                dictionary[arity] = blocks
            }
        }
        array += other.array
    }
}
class CallExpression : Expression {
    var token: Token                // トークン
    var target: Expression          // 呼び出し対象
    var arguments: [DefineStatement]// 引数
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
    static let arguments = "引数が、"
}
class ArrayLiteral : Expression {
    var token: Token                // 配列トークン
    var elements: [ExpressionStatement]
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
class DictionaryLiteral : Expression {
    var token: Token                // 辞書トークン
    var pairs: [PairExpression]
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
    var pair: (key: ExpressionStatement, value: ExpressionStatement)
    var string: String {pair.key.string.withoutPeriod + "が" + pair.value.string.withoutPeriod}
}
class EnumLiteral : Expression {
    var token: Token                // 列挙トークン
    var elements: [Statement]       // 列挙子(定義文または列挙子)
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
class EnumeratorLiteral : Expression {
    static let dot = "・"
    var token: Token                // 列挙子トークン(type・name)
    var type: String                // 列挙型の名前(空は無し)
    var name: String                // 列挙子名
    init(token: Token, type: String, name: String) {
        self.token = token
        self.type = type
        self.name = name
    }
    //
    var tokenLiteral: String {token.literal}
    var string: String {token.coloredLiteral}
}
