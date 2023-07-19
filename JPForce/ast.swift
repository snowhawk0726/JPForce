//
//  Abstract Syntax Tree
//  ast.swift
//  日本語ふぉーす(JPForce)
//
//  Created by 佐藤貴之 on 2023/02/21.
//
import Foundation

// MARK: interfaces
protocol Node : Evaluatable, NodeAccessible {
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
struct Program : Statement {
    var statements: [Statement] = []
    var tokenLiteral: String {statements.first?.tokenLiteral ?? ""}
    var string: String {statements.reduce("") {$0 + $1.string}}
}
// MARK: Statement(文)
struct DefineStatement : Statement {
    var token: Token                // とは、は、
    var name: Identifier            // 識別子
    var value: ExpressionStatement  // 値(複数の式)
    var isExtended: Bool = false    // 拡張識別
    //
    var tokenLiteral: String {token.literal}
    var string: String {name.string + tokenLiteral + "、" +
        value.expressions.reduce("") {$0 + $1.string} + (token == .particle(.WA) ? "。" : "のこと。")}
    static let wa = "は"
    static let towa = "とは"
    static let further = "さらに"
    static let koto = "こと"          //　省略可
    static let dearu = "である"        //　省略可
    static let desu = "です"          // 代替可
}
struct ExpressionStatement : Statement {
    var token: Token                // 式の最初のトークン
    var expressions: [Expression]   // 式
    //
    var tokenLiteral: String {token.literal}
    var string: String {
        let s = expressions.reduce("") {$0 + $1.string} + "。"
        return s.replacingOccurrences(of: "。】", with: "】")
                .replacingOccurrences(of: "が。", with: "が、")
    }
    //
    static let input = "入力"
    static let condition = "条件"
    static let yousoga = "要素が、"
    static let yousowa = "要素は、"
    static let hontaiga = "本体が、"
    static let hontaiwa = "本体は、"
    static let syoriga = "処理が、"
    static let syoriwa = "処理は、"
    static let joukouga = "条項が、"
    static let joukouwa = "条項は、"
    static let syokika = "初期化"
    static let kiyaku = "規約"
    static let typemembers = "型のメンバー"
    static let katano = "型の"
    static let junkyosuru = "準拠する"
    static let settei = "設定"
    static let syutoku = "取得"
    static let deatte = "であって、"
    static let deari = "であり、"
    static let soreigai = "それ以外"
    static let ga = "が"
    static let wa = "は"
    static let to = "と"
    static let de = "で"
    static let ari = "あり"
    static let aida = "間"
}
struct BlockStatement: Statement {
    var token: Token                // 【トークン
    var statements: [Statement]
    //
    var tokenLiteral: String {token.literal}
    var string: String {statements.reduce("") {$0 + $1.string}}
    var stringWithBracket: String {statements.isEmpty ? "【】" : "【\n\t" + string + "\n】"}
}
// MARK: Expressions(式)
struct Identifier : Expression {
    var token: Token                // 識別子(.IDENT(value))トークン
    var value: String               // 値(識別子名)
    //
    init(token: Token, value: String) {self.token = token; self.value = value}
    init(from string: String) {self.init(token: Token(ident: string), value: string)}
    init(from token: Token) {self.init(token: token, value: token.literal)}
    //
    var tokenLiteral: String {token.literal}
    var string: String {value.color(token.color)}
}
struct StringLiteral : ValueExpression {
    var token: Token                // 文字列(.STRING(value))トークン
    var value: String               // 値(文字列)
    init(token: Token, value: String) {self.token = token; self.value = value}
    init(from string: String) {self.init(token: Token(string: string), value: string)}
    init(from token: Token) {self.init(token: token, value: token.literal)}
    //
    var string: String {"「\(value)」".color(token.color)}
}
struct Label : ValueExpression {
    var token: Token                // ラベル(.keyword())トークン
    var value: String               // 値(文字列)
    init(token: Token, value: String) {self.token = token; self.value = value}
    //
    var string: String {token.coloredLiteral + "「\(value)」".color(token.color)}
}
struct IntegerLiteral : ValueExpression {
    var token: Token                // 数値(.INT(value))トークン
    var value: Int                  // 値(整数)
    init(token: Token, value: Int) {self.token = token; self.value = value}
    init(from integer: Int) {self.init(token: Token(number: integer), value: integer)}
    //
    var string: String {token.coloredLiteral}
}
struct Boolean: ValueExpression {
    var token: Token                // 真偽値トークン(.TRUEまたは.FALSE)
    var value: Bool
    init(token: Token, value: Bool) {self.token = token; self.value = value}
    init(from bool: Bool) {self.init(token: Token(bool: bool), value: bool)}
    //
    var string: String {tokenLiteral.color(.magenta)}
}
struct RangeLiteral : Expression {
    var token: Token                // 範囲トークン
    var lowerBound: ExpressionStatement?    // 下限式(例：1以上）
    var upperBound: ExpressionStatement?    // 上限式(例：100以下、100未満)
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
struct PhraseExpression : Expression {
    var token: Token                // 助詞(postpotional paticle)
    var left: Expression            // 式
    //
    var tokenLiteral: String {token.literal}
    var string: String {left.string + token.coloredLiteral}
}
/// 述語。tokenはToken.Keyword, Token.IDENT(_)
struct PredicateExpression : Expression {
    var token: Token                // 述語(predicate keyword)
    //
    var tokenLiteral: String {token.literal}
    var string: String {token.coloredLiteral}
}
struct InfixExpression : Expression {
    var token: Token
    var left: Expression
    var right: Expression
    //
    var tokenLiteral: String {token.literal}
    var string: String {left.string + "、" + tokenLiteral + "、" + right.string}
}
struct CaseExpression : Expression {
    var token: Token                // .CASEキーワード(場合)
    var consequence: BlockStatement
    var alternative: BlockStatement?// 「それ以外は」(else)
    //
    var tokenLiteral: String {token.literal}
    var string: String {
        token.coloredLiteral + "、【" + consequence.string + "】" +
        (alternative.map {"、" + "それ以外は" + "、【" + $0.string + "】"} ?? "")
    }
}
struct LogicalExpression : Expression {
    var token: Token                // かつ(AND)または、または(OR)
    var right: BlockStatement
    //
    var tokenLiteral: String {token.literal}
    var string: String {"\(token.coloredLiteral)、【\(right.string)】"}
}
struct LoopExpression : Expression {
    var token: Token                // .LOOPキーワード(反復)
    var parameters: [Identifier]    // カウンターまたは要素
    var condition: [Expression]     // 条件式
    var body: BlockStatement
    //
    var tokenLiteral: String {token.literal}
    var string: String {
        token.coloredLiteral + "【" +
        (parameters.isEmpty ? "" : "入力が、\(parameters.map {$0.string}.joined(separator: "と"))であり、") +
        (condition.isEmpty ? "" : "条件が、\(condition.map {$0.string}.joined(separator: "、"))間、") +
        "処理が、" + body.string + "】"
    }
}
struct FunctionLiteral : Expression {
    var token: Token                // 関数トークン
    var parameters: [Identifier]    // 入力パラメータ
    var signature: InputFormat      // 入力形式
    var body: BlockStatement        // 本体ブロック
    //
    var tokenLiteral: String {token.literal}
    var string: String {
        token.coloredLiteral + "であって、【" +
        (parameters.isEmpty ? "" : "入力が" + "、\(zip(parameters, signature.strings).map {$0.string + $1}.joined(separator: "と"))であり、") +
        "本体が、" + body.string + "】"
    }
}
struct ComputationLiteral : Expression {
    var token: Token                // 算出トークン
    var parameters: [Identifier]    // 入力パラメータ
    var signature: InputFormat      // 入力形式
    var setter: BlockStatement?     // 設定ブロック
    var getter: BlockStatement?     // 取得ブロック
    //
    var tokenLiteral: String {token.literal}
    var string: String {
        token.coloredLiteral + "であって、【" +
        (parameters.isEmpty ? "" : "入力が" + "、\(zip(parameters, signature.strings).map {$0.string + $1}.joined(separator: "と"))であり、") +
        (setter.map {"設定は、【\($0.string)】。"} ?? "") +
        (getter.map {"取得は、【\($0.string)】。"} ?? "") +
        "】"
    }
}
struct ProtocolLiteral : Expression {
    var token: Token                // 規約トークン
    var protocols: [String]         // 準拠する規約
    var clauses: [ClauseLiteral]    // 規約条項
    //
    var tokenLiteral: String {token.literal}
    var string: String {
        token.coloredLiteral + "であって、【" +
        (protocols.isEmpty ? "" : "準拠する規約は、\(protocols.map {$0}.joined(separator: "と、"))。") +
        "条項が、\(clauses.map {$0.string}.joined(separator: ""))】"
    }
}
struct ClauseLiteral {
    var identifier: Identifier      // 識別子
    var type: String                // 型の文字列
    var parameters: [Identifier]    // 関数のパラメータ
    var signature: InputFormat?     // 関数の入力
    var isTypeMember: Bool = false  // 型のメンバー
    //
    var string: String {identifier.string + DefineStatement.wa + "、" +
        ((signature.map {"関数であって、【入力が、\(zip(parameters, $0.strings).map {$0.string + $1}.joined(separator: "と"))】"}) ?? "「\(type)」") + "。"
    }
}
struct TypeLiteral : Expression {
    var token: Token                // 型トークン
    var protocols: [String]         // 準拠する規約
    var typeMembers: BlockStatement?// 型のメンバー
    var initializers: [Initializer] // 初期化処理
    var body: BlockStatement?       // インスタンスのメンバー
    //
    var tokenLiteral: String {token.literal}
    var string: String {
        var s = token.coloredLiteral + "であって、【" +
        (protocols.isEmpty ? "" : "準拠する規約は、\(protocols.map {$0}.joined(separator: "と、"))であり、") +
        (typeMembers.map {"型のメンバーが、\($0.string)であり、"} ?? "") +
        (initializers.reduce("") {$0 + "初期化は、\($1.isExtended ? "さらに、" : "")【\($1.string)】。"}) +
        (body.map {"本体が、\($0.string)"} ?? "") +
        "】"
        s.range(of: "さらに、").map {s.replaceSubrange($0, with: "")}   // 初出の「さらに、」を取り除く
        return s.replacingOccurrences(of: "。】", with: "】")
    }
}
struct InputFormat {
    var numberOfInputs: Int?                        // 期待するパラメータ数(可変の場合はnil)
    var formats: [(type: String, particle: String)] // 期待するパラメータ毎の型と格(無い場合は"")
    var strings: [String] {formats.map {!($0.type.isEmpty && $0.particle.isEmpty) ? "「\($0.type + $0.particle)」" : ""}}
}
struct Initializer {
    var parameters: [Identifier]    // 入力パラメータ
    var signature: InputFormat      // 入力形式
    var body: BlockStatement?       // 初期化処理
    var isExtended: Bool = false    // 拡張識別
    var string: String {
        (parameters.isEmpty ? "" : "入力が、\(zip(parameters, signature.strings).map {$0.string + $1}.joined(separator: "と"))であり、") +
        (body.map {"本体が【\($0.string)】"} ?? "")
    }
}
struct ArrayLiteral : Expression {
    var token: Token                // 配列トークン
    var elements: [ExpressionStatement]
    //
    var tokenLiteral: String {token.literal}
    var string: String {
        token.coloredLiteral + "であって、【" +
        (elements.isEmpty ? "" : "要素が、\(elements.map {$0.string}.joined(separator: "と、"))".withoutPeriod) + "】"
    }
}
struct DictionaryLiteral : Expression {
    var token: Token                // 辞書トークン
    var pairs: [PairExpression]
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
struct EnumLiteral : Expression {
    var token: Token                // 列挙トークン
    var elements: [Statement]       // 列挙子(定義文または列挙子)
    //
    var tokenLiteral: String {token.literal}
    var string: String {
        token.coloredLiteral + "であって、【" +
        (elements.isEmpty ? "" : "要素が、\(elements.map {$0.string}.joined(separator: "と、"))".withoutPeriod) + "】"
    }
}
//
func conform(to protocols: [JpfProtocol], with environment: Environment, onType: Bool = false) -> JpfObject? {
    for p in protocols {
        if let result = conform(to: p.clauses, with: environment, onType: onType), result.isError {return result}  // 準拠エラー
    }
    return nil
}
func conform(to protocols: [String], with environment: Environment, onType: Bool = false) -> JpfObject? {
    for s in protocols {
        guard let p = environment[s] as? JpfProtocol else {return designatedObjectNotFound}
        if let result = conform(to: p.clauses, with: environment, onType: onType), result.isError {return result}  // 準拠エラー
    }
    return nil
}
private func conform(to clauses: [ClauseLiteral], with environment: Environment, onType: Bool = false) -> JpfObject? {
    for clause in clauses {
        if onType != clause.isTypeMember {continue}
        guard let target = environment[clause.identifier.value] else {return designatedObjectNotFound + "指定値：\(clause.identifier.value)(\(clause.type))"}  // 対象のオブジェクト
        guard clause.type == target.type else {return designatedTypeNotMatch + "(指定型：\(clause.type)"}
        if let function = target as? JpfFunction {  // メンバー関数パラメタチェック
            guard clause.parameters.count == function.parameters.count else {return numberOfParamsNotMatch + "(指定数：\(clause.parameters.count)"}
            for pairs in zip(clause.parameters, function.parameters) {
                guard pairs.0.value == pairs.1.value else {return nameOfParamsNotMatch + "(指定値：\(pairs.0.value)"}
            }
            if let result = compareSignature(clause.signature, with: function.signature), result.isError {return result}
        }
    }
    return nil
}
private func compareSignature(_ lhs: InputFormat?, with rhs: InputFormat) -> JpfObject? {
    guard let left = lhs else {return nil}          // チェックするシグネチャーが無い
    guard left.numberOfInputs == rhs.numberOfInputs else {return formatOfParamsNotMatch + "(指定入力数：\(left.numberOfInputs.map {"\($0)"} ?? "無し"))"}
    for pairs in zip(left.formats, rhs.formats) {
        guard pairs.0.type == pairs.1.type && pairs.0.particle == pairs.1.particle else {return formatOfParamsNotMatch + "(指定形式：「\(pairs.0.type)\(pairs.0.particle)」)"}
    }
    return nil
}
private var designatedObjectNotFound: JpfError {JpfError("指定した識別子のメンバーが見つからない。")}
private var designatedTypeNotMatch: JpfError{JpfError("メンバーの型が規約に準拠していない。")}
private var numberOfParamsNotMatch: JpfError{JpfError("メンバー関数の引数の数が規約に準拠していない。")}
private var nameOfParamsNotMatch: JpfError  {JpfError("メンバー関数の引数の名前が規約に準拠していない。")}
private var formatOfParamsNotMatch: JpfError{JpfError("メンバー関数の引数の形式が規約に準拠していない。")}
