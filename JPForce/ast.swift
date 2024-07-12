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
    var isExtended: Bool = false    // 拡張(多重)識別
    //
    var tokenLiteral: String {token.literal}
    var string: String {name.string + tokenLiteral + "、" + (isExtended ? (Self.further + "、") : "") +
        value.expressions.reduce("") {$0 + $1.string} + (token.isParticle(.TOWA) ? "のこと。" : "。")}
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
    //
    static let directoryPath = "ディレクトリパス"
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
struct Label : Expression {
    var token: Token                // ラベル(.keyword())トークン
    var value: Token                // 値(文字列または識別子)
    init(token: Token, value: Token) {self.token = token; self.value = value}
    //
    var tokenLiteral: String {token.literal}
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
struct OrExpression : Expression {
    var token: Token
    var left: Expression
    var right: Expression
    //
    var tokenLiteral: String {token.literal}
    var string: String {left.string + "、" + tokenLiteral + "、" + right.string}
}
/// 属格。<オブジェクト>の<要素>(は、<値>。)
struct GenitiveExpression : Expression {
    var token: Token                // 属格(genitive case)
    var left: Expression
    var right: Expression
    var value: ExpressionStatement? // 値
    //
    var tokenLiteral: String {token.literal}
    var string: String {
        left.string + tokenLiteral + right.string + (value.map {"、\($0.string)"} ?? "").withoutPeriod
    }
}
/// 場合文１： <論理式>場合【<続文>】、それ以外は【<代文>】。
/// 場合文２：<識別子>が<値１>の場合【<文１>】、<値２>の場合【<文２>】、…それ以外は【<代文>】。
struct CaseExpression : Expression {
    static let soreigai = "それ以外"
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
struct ConditionalOperation : Expression {
    static let ka = "か"
    var token: Token                // .CONDITIONALキーワード(よって)
    var consequence: Expression     // 成立時の値(式)
    var alternative: Expression     // 不成立の値(式)
    //
    var tokenLiteral: String {token.literal}
    var string: String {
        "\(token.coloredLiteral)、\(consequence.string)か、\(alternative.string)"
    }
}
struct LoopExpression : Expression {
    static let condition = "条件"
    static let aida = "間"
    static let syoriga = "処理が、"
    static let syoriwa = "処理は、"
    var token: Token                // .LOOPキーワード(反復)
    var parameters: [Identifier]    // カウンターまたは要素
    var condition: [Expression]     // 条件式
    var body: BlockStatement
    //
    var tokenLiteral: String {token.literal}
    var string: String {
        token.coloredLiteral + "【" +
        (parameters.isEmpty ? "" : "入力が、\(parameters.map {$0.string}.joined(separator: "と、"))であり、") +
        (condition.isEmpty ? "" : "条件が、\(condition.map {$0.string}.joined(separator: "、"))間、") +
        "処理が、" + body.string + "】"
    }
}
struct FunctionLiteral : Expression {
    var token: Token                // 関数トークン
    var functions: FunctionBlocks   // 関数部
    //
    var tokenLiteral: String {token.literal}
    var string: String {
        functions.array.reduce("") {$0 + ($1.isOverloaded ? "さらに、" : "") + "\(token.coloredLiteral)であって、【\($1.string)】"}
    }
}
struct ComputationLiteral : Expression {
    static let settei = "設定"
    static let syutoku = "取得"
    var token: Token                // 算出トークン
    var setters: FunctionBlocks     // 設定ブロック
    var getters: FunctionBlocks     // 取得ブロック
    //
    var tokenLiteral: String {token.literal}
    var string: String {
        token.coloredLiteral + "であって、【" +
        (setters.array.reduce("") {$0 + "設定は、\($1.isOverloaded ? "さらに、" : "")【\($1.string)】。"}) +
        (getters.array.reduce("") {$0 + "取得は、\($1.isOverloaded ? "さらに、" : "")【\($1.string)】。"}) +
        "】"
    }
}
struct ProtocolLiteral : Expression {
    static let kiyaku = "規約"
    static let junkyosuru = "準拠する"
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
    static let joukouga = "条項が、"
    static let joukouwa = "条項は、"
    var identifier: Identifier      // 識別子
    var type: String                // 型の文字列
    var functionParams: ParameterClauseLiteral?
    var getterParams: ParameterClauseLiteral?
    var setterParams: ParameterClauseLiteral?
    var isTypeMember: Bool = false  // 型の要素
    //
    var string: String {identifier.string + DefineStatement.wa + "、" + typeString(type) + "。"}
    private func typeString(_ type: String) -> String {
        switch type {
        case JpfFunction.type:
            if let string = functionParams?.string {
                return type + ExpressionStatement.deatte + "【\(string)】"
            }
        case JpfComputation.type:
            let setter = setterParams?.string
            let getter = getterParams?.string
            if setter != nil || getter != nil {
                let string = 
                    (setter.map {"設定は、【\($0)】。"} ?? "") +
                    (getter.map {"取得は、【\($0)】。"} ?? "")
                return type + ExpressionStatement.deatte + "【\(string)】"
            }
        default:
            break
        }
        return "「\(type)」"
    }
}
struct ParameterClauseLiteral {
    var parameters: [Identifier]    // 引数
    var signature: InputFormat?     // シグネチャ
    var string: String {
        signature.map {"入力が、\(zip(parameters, $0.strings).map {$0.string + $1}.joined(separator: "と、"))"} ?? ""
    }
}
struct TypeLiteral : Expression {
    static let syokika = "初期化"
    static let typemembers = "型の要素"
    static let katano = "型の"
    var token: Token                // 型トークン
    var protocols: [String]         // 準拠する規約
    var typeMembers: BlockStatement?// 型の要素
    var initializers: FunctionBlocks// 初期化処理
    var body: BlockStatement?       // インスタンスのメンバー
    //
    var tokenLiteral: String {token.literal}
    var string: String {
        var s = token.coloredLiteral + "であって、【" +
        (protocols.isEmpty ? "" : "準拠する規約は、\(protocols.map {$0}.joined(separator: "と、"))であり、") +
        (typeMembers.map {"型の要素が、\($0.string)であり、"} ?? "") +
        (initializers.array.reduce("") {$0 + "初期化は、\($1.isOverloaded ? "さらに、" : "")【\($1.string)】。"}) +
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
    func getThreeDots() -> Self {self.hasSuffix(threeDots) ? threeDots : ""}
    var removedThreeDots: Self {self.hasSuffix(threeDots) ? String(self.dropLast()) : self}
}
struct FunctionBlock {
    static let input = "入力"
    static let ari = "あり"
    var parameters: [Identifier]    // 入力パラメータ
    var signature: InputFormat      // 入力形式
    var body: BlockStatement?       // 処理本体
    var isOverloaded: Bool = false  // 多重識別
    var string: String {
        (parameters.isEmpty ? "" : "入力が、\(zip(parameters, signature.strings).map {$0.string + $1}.joined(separator: "と、"))であり、") +
        (body.map {"本体が、\($0.string)"} ?? "")
    }
    var overloaded: Self {
        FunctionBlock(parameters: self.parameters, signature: self.signature, body: self.body, isOverloaded: true)
    }
    var rangeOfInputs: ClosedRange<Int> {   // 入力数の範囲
        guard let max = signature.numberOfInputs else {return (-1)...(-1)}
        let min = max - signature.numberOfDefaultValues
        return min...max
    }
    func index(of name: String) -> Int? {
        parameters.firstIndex {$0.value == name}
    }
    /// 入力パラメータ(名前)の型が、指定の型と一致するか
    /// - Parameters:
    ///   - name: 入力パラメータの名前
    ///   - type: 指定の型
    /// - Returns: 一致(true), 不一致もしくは名前が見つからない(false)
    func isSameType(of name: String, as type: String) -> Bool {
        guard let i = index(of: name) else {
            return false                    // 入力の名前が一致しない
        }
        let t = signature.formats[i].type
        return t.isEmpty || t == type       // チェック不要、もしくは型が一致
    }
    func isVariable(parmeter name: String) -> Bool {
        guard let i = index(of: name) else {
            return false                    // 入力の名前が一致しない
        }
        return signature.formats[i].hasThreeDots
    }
}
/// 必要入力数ごとの関数ブロック配列(多重定義)
struct FunctionBlocks : Collection {
    var dictionary: [Int : [FunctionBlock]] = [:]
    var array: [FunctionBlock] = []
    //
    init() {}
    /// 関数定義で初期化
    init(_ functionBlock: FunctionBlock) {_ = self.append(functionBlock)}
    // Collection protocolに準拠
    typealias Index = Dictionary<Int, [FunctionBlock]>.Index
    var startIndex: Index {dictionary.startIndex}
    var endIndex: Index {dictionary.endIndex}
    func index(after i: Index) -> Index {dictionary.index(after: i)}
    subscript(position: Index) -> Slice<FunctionBlocks> {self[position..<index(after: position)]}
    //
    /// 必要入力数毎の多重定義配列を返す。(無い場合は空)
    subscript(index: Int) -> [FunctionBlock] {dictionary[index] ?? []}
    /// 関数定義を多重定義(自身)に追加する。
    /// - Parameter functionBlock: 追加する関数ブロック (isOverloaded==falseならば上書き)
    /// - Returns: 追加した自身を返す
    mutating func append(_ functionBlock: FunctionBlock) -> Self {
        functionBlock.rangeOfInputs.forEach {
            if dictionary[$0] != nil && functionBlock.isOverloaded {
                dictionary[$0]!.append(functionBlock)
            } else {
                dictionary[$0] = [functionBlock]
            }
        }
        if array.isEmpty || functionBlock.isOverloaded {array.append(functionBlock)}
        return self
    }
    /// 多重定義を多重定義(自身)に追加する。
    /// - Parameter functionBlocks: 追加する多重定義 (hasRedefineならば上書き)
    /// - Returns: 追加した自身を返す
    mutating func append(_ functionBlocks: FunctionBlocks) -> Self {
        functionBlocks.dictionary.keys.forEach {
            if dictionary[$0] != nil && !functionBlocks.hasRedefine {
                dictionary[$0]! += functionBlocks[$0]
            } else {
                dictionary[$0] = functionBlocks[$0]
            }
        }
        if array.isEmpty || functionBlocks.hasRedefine {
            array = functionBlocks.array
        } else {
            array += functionBlocks.array
        }
        return self
    }
    var overloaded: Self {
        var functionBlocks = FunctionBlocks()
        self.array.forEach {_ = functionBlocks.append($0.overloaded)}
        return functionBlocks
    }
    /// 再定義している(多重定義でない関数ブロックが含まれる)
    var hasRedefine: Bool {self.array.contains {$0.isOverloaded == false}}
    /// 規約の条項の引数部と同じ形式の定義有無
    /// - Parameter params: 条項の引数部
    /// - Returns: true: 有る
    func hasParamaeter(to params: ParameterClauseLiteral) -> Bool {
        for definition in array.reversed() {
            guard params.parameters.count == definition.parameters.count else {continue}  // 引数の数
            for pairs in zip(params.parameters, definition.parameters) {                  // 全識別子名
                guard pairs.0.value == pairs.1.value else {continue}
            }
            if let signature = params.signature {                                       // シグネチャ
                guard signature.numberOfInputs == definition.signature.numberOfInputs else {continue}
                for pairs in zip(signature.formats, definition.signature.formats) {
                    guard pairs.0.type == pairs.1.type &&
                            pairs.0.particle == pairs.1.particle else {continue}
                }
            }
            return true
        }
        return false
    }
    /// 入力と引数の形式が一致する関数ブロックを得る。
    /// - Parameter env: 引数をもつ環境
    /// - Returns: 関数ブロック(無ければnil)
    func function(with env: Environment) -> FunctionBlock? {
        let pairs = env.argumentPairs           // 引数の名前(k)と値(v)の組
        let vFunctions = dictionary[-1]         // 可変長の定義
        let fFunctions = dictionary[pairs.count]// 固定長の定義
        switch (vFunctions, fFunctions) {
        case let (v?, f?):                      // 両方チェック
            if let function = function(in: v, with: pairs) {return function}
            return function(in: f, with: pairs)
        case let (f?, nil), let (nil, f?):      // 片方チェック
            return function(in: f, with: pairs)
        default:
            return nil
        }
    }
    /// 候補の関数ブロック配列から、指定引数(名前と値)形式を持つ関数ブロックを返す。
    /// - Parameters:
    ///   - functions: 候補の関数ブロック配列
    ///   - pairs: 指定引数(名前と値)
    /// - Returns: 関数ブロック(見つからなければnil)
    private func function(in functions: [FunctionBlock], with pairs: [(String, JpfObject)]) -> FunctionBlock? {
        functions: for f in functions.reversed() {
            arguments: for (k, v) in pairs {
                if f.isVariable(parmeter: k) {  // 可変長識別子の値の型は配列
                    guard v.type == JpfArray.type else {continue functions}
                } else {                        // 固定長識別子の値の型は引数の型
                    guard f.isSameType(of: k, as: v.type) else {continue functions}
                }
            }
            return f                            // 引数名と型が全て一致
        }
        return nil                              // 一致する関数が無い
    }
}
struct CallExpression : Expression {
    var token: Token                // トークン
    var target: Expression          // 呼び出し対象
    var arguments: [DefineStatement]// 引数
    //
    var tokenLiteral: String {token.literal}
    var string: String {
        "\(target.string)【" +
        (arguments.isEmpty ? "" : Self.arguments + arguments.reduce("") {$0 + $1.string.withoutComma}) + "】"
    }
    static let arguments = "引数が、"
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
struct EnumeratorLiteral : Expression {
    static let dot = "・"
    var token: Token                // 列挙子トークン(type・name)
    var type: String                // 列挙型の名前(空は無し)
    var name: String                // 列挙子名
    //
    var tokenLiteral: String {token.literal}
    var string: String {token.coloredLiteral}
}
