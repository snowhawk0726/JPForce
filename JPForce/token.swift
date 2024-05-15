//
//  token.swift
//  日本語ふぉーす(JPForce)
//
//  Created by 佐藤貴之 on 2023/02/17.
//
import Foundation

enum Token : Equatable {
    // MARK: - 列挙子
    case IDENT(String = "識別子")  // 甲, 乙, ...
    case INT(Int = 0)           // 1343456..., -1
    case STRING(String = "文字列") // 文字列
    case symbol(Symbol)
    case keyword(Keyword)
    case particle(Particle)
    case wrapped(type: TokenType, literal: String)
    case ILLEGAL(String = "未定義")
    //
    enum Symbol : String, CaseIterable {    // 記号
        case PERIOD     = "。"   // 区切り文字
        case COMMA      = "、"
        case LBRACKET   = "「"   // かぎ括弧(corner bracket)
        case RBRACKET   = "」"
        case LWBRACKET  = "『"   // 二重かぎ括弧(white corner bracket)
        case RWBRACKET  = "』"
        case LBBRACKET  = "【"   // 墨付き括弧(black lenticular bracket)
        case RBBRACKET  = "】"
        case LPAREN     = "（"   // 丸かっこ
        case RPAREN     = "）"
        case REFMARK    = "※"   // 米印(reference mark)
        case MINUS      = "-"
        case SPACE      = " "
        case ESC        = "\\"
        case TAB        = "\t"
        case CR         = "\r"
        case EOL        = "\n"  // 行末(end of line)
        case EOF        = "\0"  // 終端(end mark)
    }
    /// 主に格助詞。Keyword, identの関係を表す。
    enum Particle : String {    // 助詞(postpositional particle)
        case WA         = "は"   // 係助詞
        case GA         = "が"   // 格助詞
        case WO         = "を"
        case NI         = "に"
        case DE         = "で"
        case TO         = "と"
        case HE         = "へ"
        case KARA       = "から"
        case YORI       = "より"
        case MADE       = "まで"
        case NO         = "の"
        case DA         = "だ"   // 助動詞
        case TA         = "た"
        case TOWA       = "とは"  // 格助詞 + 係助詞
        case DEWA       = "では"  // ??
        case KO         = "個"   // 数助詞
        case GTEQUAL    = "以上"
        case LTEQUAL    = "以下"
        case UNDER      = "未満"
    }
    /// 内部予約語。主に述語(動詞、形容詞、名詞)。
    /// - 動詞は処理、形容詞は条件、名詞はその他キーワード
    enum Keyword : String {     // キーワード(予約語)
        case ADD        = "足す"
        case SUBSTRACT  = "引く"
        case MULTIPLY   = "掛ける"
        case DIVIDE     = "割る"
        case NEGATE     = "負数"
        case EQUAL      = "等しい"
        case LT         = "小さい"
        case GT         = "大きい"
        case WHILE      = "間"   // workaraound for 小さい間 -> IDENT("小さい間")
        case BE         = "ある"
        case NOT        = "ない"
        case AND        = "かつ"
        case OR         = "または"
        case MATA       = "また"  // workaround for 1または -> 1(また) .WA
        case ASWELLAS   = "および"
        case RETURN     = "返す"
        case CASE       = "場合"
        case QUESTION   = "か"
        case CONDITIONAL = "よって"
        case LOOP       = "反復"
        case FUNCTION   = "関数"
        case EXECUTE    = "実行"
        case SURU       = "する"  // irregular verb
        case KOTO       = "こと"  // end of definition
        case MONO       = "もの"  // unwrap a phrase and return a value in the phrase
        case TRUE       = "真"
        case FALSE      = "偽"
        case NULL       = "無"   // object is nil
        case POSITIVE   = "正"
        case NEGATIVE   = "負"
        case ARRAY      = "配列"
        case DICTIONARY = "辞書"
        case ENUM       = "列挙"
        case RANGE      = "範囲"
        case APPEND     = "追加"
        case REMOVE     = "削除"
        case FOREACH    = "繰り返す"
        case BREAK      = "中止"
        case MAP        = "写像"
        case REDUCE     = "まとめる"
        case FILTER     = "絞り込む"
        case SORT       = "並べ替える"
        case REVERSE    = "逆順"
        case CONTAINS   = "含む"
        case INPUT      = "入力"
        case EMPTY      = "空"
        case PUSH       = "積む"
        case DROP       = "捨てる" // remove last from stack
        case PULL       = "得る"
        case DUPLICATE  = "写す"
        case ASSIGN     = "代入"
        case SWAP       = "入れ替える"
        case PRINT      = "表示"
        case NEWLINE    = "改行"
        case READ       = "音読"
        case ASK        = "尋ねる"
        case FILES      = "ファイル一覧"
        case FILE       = "ファイル"
        case IDENTIFIERS = "識別子一覧"
        case IDENTIFIER = "識別子"
        case PROTOCOL   = "規約"
        case TYPE       = "型"
        case CREATE     = "生成"
        case AVAILABLE  = "利用可能"
        case OVERWRITE  = "上書き"
        case POSITION   = "位置"
        case MEMBER     = "要素"
        case SET        = "設定"
        case COMPUTATION = "算出"
        case INITIALIZATION = "初期化"
    }
    /// 文字列の連想値について識別しないための分類(.IDENT("文字列") → .ident)
    enum TokenType: Hashable {
        case ident, int, string, keyword(Keyword), particle(Particle), symbol(Symbol), illegal
        //
        static func == (lhs: TokenType, rhs: TokenType) -> Bool {
            switch (lhs, rhs) {
            case (.keyword(let lk), .keyword(let rk)):  return lk == rk
            case (.symbol(let ls), .symbol(let rs)):    return ls == rs
            case (.particle(let lp), .particle(let rp)):return lp == rp
            case (.ident, .ident):                      return true
            case (.int, .int):                          return true
            case (.string, .string):                    return true
            case (.illegal, .illegal):                  return true
            default:                                    return false
            }
        }
    }
    // MARK: - イニシャライザ
    init(symbol: String) {self = Self.symbols[symbol].map {.symbol($0)} ?? .IDENT(symbol)}
    init(word: String) {
        self =
        Keyword(rawValue: word).map {.keyword($0)} ??
        Particle(rawValue: word).map {.particle($0)} ??
        word.hankaku.flatMap {Int($0)}.map {.INT($0)} ??    // 全角半角の数値を.INT()に変換
        ContinuativeForm(word).plainFormType.map {.wrapped(type: $0, literal: word)} ??
        .IDENT(word)
    }   // let n = "２".hankaku.map {Int($0)} → Int(_)がInt?を返すため、nはInt??。
    init(symbol: Symbol)        {self = .symbol(symbol)}
    init(keyword: Keyword)      {self = .keyword(keyword)}
    init(paticle: Particle)     {self = .particle(paticle)}
    init(ident: String)         {self = .IDENT(ident)}
    init(number: Int)           {self = .INT(number)}
    init(string: String)        {self = .STRING(string)}
    init(bool: Bool)            {self = bool ? .keyword(.TRUE) : .keyword(.FALSE)}
    init(illegal: String)       {self = .ILLEGAL(illegal)}
    init(type: TokenType, literal: String) {self = .wrapped(type: type, literal: literal)}
    // 省略形(テスト用)
    init()                      {self = .symbol(.EOF)}
    init(_ keyword: Keyword)    {self.init(keyword: keyword)}
    init(_ symbol: Symbol)      {self.init(symbol: symbol)}
    init(_ number: Int)         {self.init(number: number)}
    init(_ particle: Particle)  {self.init(paticle: particle)}
    init(_ bool: Bool)          {self.init(bool: bool)}
    init(_ string: String)      {self.init(string: string)}
    // MARK: - コンピューテッドプロパティ
    var type: TokenType {
        switch self {
        case .IDENT(_):         return .ident
        case .INT(_):           return .int
        case .STRING(_):        return .string
        case .keyword(let k):   return .keyword(k)
        case .symbol(let s):    return .symbol(s)
        case .particle(let p):  return .particle(p)
        case .ILLEGAL(_):       return .illegal
        case .wrapped(let type, _): return type
        }
    }
    var literal: String {
        switch self {
        case .IDENT(let s):     return s
        case .INT(let int):     return String(int)
        case .STRING(let s):    return s
        case .keyword(let k):   return k.rawValue
        case .symbol(let s):    return s.rawValue
        case .particle(let p):  return p.rawValue
        case .ILLEGAL(let s):   return s
        case .wrapped(_, let literal):  return literal
        }
    }
    var unwrappedType: TokenType {
        if case .wrapped(let type,_) = self {return type}
        return self.type
    }
    var unwrappedLiteral: String {
        guard case .wrapped(_,let word) = self else {return literal}
        return ContinuativeForm(word).plainForm ?? literal
    }
    var coloredLiteral: String {literal.color(color)}
    var number: Int? {if case .INT(let int) = self {return int} else {return nil}}
    var color: EscapeCode {
        switch type {
        case .ident:        return .cyan
        case .int:          return .blue
        case .string:       return .red
        case .keyword(_):   return .green
        case .particle(_):  return .magenta
        case .illegal:      return .red
        default:            return .reset
        }
    }
    //　MARK: - 判定
    func isSameCategory(as token: Token) -> Bool {  // .keyword()等のKeywordレベルは比較しない。
        switch (self.type, token.type) {
        case (.keyword(_), .keyword(_)):    return true
        case (.symbol(_), .symbol(_)):      return true
        case (.particle(_), .particle(_)):  return true
        case (.ident, .ident):              return true
        case (.int, .int):                  return true
        case (.string, .string):            return true
        case (.illegal, .illegal):          return true
        default:                            return false
        }
    }
    var isKeyword: Bool {if case .keyword(_) = self {return true} else {return false}}
    var isParticle: Bool {if case .particle(_) = self {return true} else {return false}}
    var isWrapped: Bool {if case .wrapped(_,_) = self {return true} else {return false}}
    var isIdent: Bool   {type == .ident}
    var isString: Bool  {type == .string}
    var isNumber: Bool  {type == .int}
    var isIllegal: Bool {type == .illegal}
    var isTrue: Bool    {self == .keyword(.TRUE)}
    var isEof: Bool     {self == .symbol(.EOF)}
    var isEol: Bool     {self == .symbol(.EOL)}
    var isPeriod: Bool  {self == .symbol(.PERIOD)}
    var isComma: Bool   {self == .symbol(.COMMA)}
    func isKeyword(_ k: Token.Keyword) -> Bool {self == .keyword(k)}
    func isParticle(_ p: Token.Particle) -> Bool {self == .particle(p)}
    //
    // MARK: - 全角半角対応の記号辞書(symbol dictionary for half/fullwidth)
    static var symbols = {
        var d: [String: Symbol] = [:]
        Symbol.allCases.forEach { e in
            e.rawValue.hankaku.map {d[$0] = e}
            e.rawValue.zenkaku.map {d[$0] = e}
        }
        d["−"] = .MINUS
        d["，"] = .COMMA;    d[","] = .COMMA
        d["．"] = .PERIOD;   d["."] = .PERIOD
        return d
    }()
}
    
// MARK: - String extension
extension String {
    var hankaku: String? {applyingTransform(.fullwidthToHalfwidth, reverse: false)} // 半角変換(Fullwidth to Halfwidth (ascii))
    var zenkaku: String? {applyingTransform(.fullwidthToHalfwidth, reverse: true)}  // 全角変換(Halfwidth to Fullwidth)
    var withoutPeriod: Self {self.replacingOccurrences(of: Token.Symbol.PERIOD.rawValue, with: "")}
    var isPlainForm: Bool {                                                         // 終止形？(Check self if palin form)
        PlainForm.hasEnd(of: self) || PlainForm.hasIrregular(self)
    }
    var isContinuativeForm: Bool {                                                  // 連用形？(Check self if continuative form)
        ContinuativeForm.hasEnd(of: self) || ContinuativeForm.hasIrregular(self)
    }
}
// MARK: - 連用形(Continuative Form)
/// 連用形の文字列(word)を、終止形(Plain Form)に変換する。
// ※ structにすると、Token内で Cannot use mutating getter on immutable value: function call returns immutable value
class ContinuativeForm : Equatable {
    init(_ word: String) {self.literal = word}
    init(_ token: Token) {self.literal = token.literal}
    let literal: String
    // ストアドプロパティ
    lazy var plainForm: String? = {
        Self.irregularWords[literal] ??             // 変格活用動詞等の例外
        literal.last.map({String($0)})              // 連用形の語尾 →
            .flatMap({Self.suffixes[$0]})           // 終止形の語尾に変換 →
            .map({String(literal.dropLast()) + $0}) // 語幹 + 終止形の語尾
    }()
    lazy var plainFormType: Token.TokenType? = {
        plainForm.map {
            Token.Keyword(rawValue: $0).map {.keyword($0)} ??
            Token.Particle(rawValue: $0).map {.particle($0)} ??
            .ident
        }
    }()
    // スタティックメンバー
    static func hasEnd(of word: String) -> Bool {   // 語尾が一致するか？
        word.last.map({String($0)})
            .map({suffixes.keys.contains($0)}) ?? false
    }
    static func hasIrregular(_ word: String) -> Bool {
        irregularWords.keys.contains(word)
    }
    static let suffixes = [ //　終止形の語尾 = suffixes[連用形の語尾] 注：連用形の語尾は１文字
        "い":"う", "え":"える",
        "き":"く", "ぎ":"ぐ", "く":"い", "け":"ける",
        "し":"す", "せ":"せる",
        "ち":"つ", "っ":"る", "て":"てる",
        "に":"ぬ", "ね":"ねる",
        "び":"ぶ",
        "み":"む", "め":"める",
        "り":"る", "れ":"れる",
        "ん":"む",
    ]
    static var irregularWords = [
        "し":"する",                               // サ変
        "て":"た", "で":"だ",                       // 助動詞「だ」「た」
        // ＋　助動詞「た」「て」
        "得":"得る", "見":"見る",
        "引い":"引く",
        "含ま":"含む",                              // 例外的に、未然形（含まない）
    ]
    static func == (lhs: ContinuativeForm, rhs: ContinuativeForm) -> Bool {
        lhs.literal == rhs.literal
    }
}
// MARK: - 終止形(Plain Form)
struct PlainForm {
    static let suffixes: [String] = Array(ContinuativeForm.suffixes.values)
    static let irregularWords: [String] = Array(ContinuativeForm.irregularWords.values)
    //
    static func hasEnd(of word: String) -> Bool {
        suffixes.contains(String(word.suffix(2))) ||
        (word.last.map({String($0)}).map({suffixes.contains($0)}) ?? false)
    }
    static func hasIrregular(_ word: String) -> Bool {
        irregularWords.contains(word)
    }
}
