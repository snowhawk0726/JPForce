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
    var string: String {"(" + left.string + "、" + tokenLiteral + "、" + right.string + ")"}
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
        (!parameters.isEmpty ? "入力が" + "、\(parameters.map {$0.string}.joined(separator: "と"))であり、" : "") +
        (!condition.isEmpty ? "条件が" + "、\(condition.map {$0.string}.joined(separator: "、"))間、" : "") +
        "処理が" + "、" + body.string + "】"
    }
}
struct FunctionLiteral : Expression {
    var token: Token                // 関数トークン
    var parameters: [Identifier]    // 入力パラメータ
    var signature: InputFormat      // 入力形式
    var body: BlockStatement
    //
    var tokenLiteral: String {token.literal}
    var string: String {
        token.coloredLiteral + "であって、【" +
        (!parameters.isEmpty ? "入力が" + "、\(zip(parameters, signature.strings).map {$0.string + $1}.joined(separator: "と"))であり、" : "") +
        "本体が" + "、" + body.string + "】"
    }
}
struct InputFormat {
    var numberOfInputs: Int?                        // 期待するパラメータ数(可変の場合はnil)
    var formats: [(type: String, particle: String)] // 期待するパラメータ毎の型と格(無い場合は"")
    var strings: [String] {formats.map {!($0.type.isEmpty && $0.particle.isEmpty) ? "「\($0.type + $0.particle)」" : ""}}
}
struct ArrayLiteral : Expression {
    var token: Token                // 配列トークン
    var elements: [ExpressionStatement]
    //
    var tokenLiteral: String {token.literal}
    var string: String {
        token.coloredLiteral + "であって、【" +
        (!elements.isEmpty ? "要素が" + "、\(elements.map {$0.string}.joined(separator: "と"))" : "") +
        "】"
    }
}
struct DictionaryLiteral : Expression {
    var token: Token                // 辞書トークン
    var pairs: [PairExpression]
    //
    var tokenLiteral: String {token.literal}
    var string: String {
        token.coloredLiteral + "であって、【" +
        (!pairs.isEmpty ? "要素が" + "、\(pairs.map {$0.string}.joined(separator: "と"))" : "") +
        "】"
    }
    struct PairExpression {
        var pair: (key: ExpressionStatement, value: ExpressionStatement)
        var string: String {pair.key.string + "が" + pair.value.string}
    }
}
