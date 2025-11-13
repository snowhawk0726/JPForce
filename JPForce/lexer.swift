//
//  Lexical analyzer
//  lexer.swift
//  日本語ふぉーす(JPForce)
//
//  Created by 佐藤貴之 on 2023/02/17.
//
//  仕様: 記号は、基本全角
//       「」で囲まれた文字列は、.STRING("文字列")
//       『』で囲まれた文字列は、.IDENT("識別子")
//       （）で囲まれた文字列は、コメント(読み飛ばす)
//        ※ で始まる文字列は、行末までコメント(読み飛ばす)
//       そのほかで、空白や制御文字は無視
import Foundation
import NaturalLanguage

// MARK: - 字句解析器(Lexer)
/// input文字列を解析し、トークンに分解する。
class Lexer {
    //
    init(_ input: String) {self.input = input; tagging(input)}
    init(from other: Lexer) {
        self.input = other.input
        self.position = other.position
        self.positionTobeRead = other.positionTobeRead
        self.taggedWords = other.taggedWords
        self.identifiers = other.identifiers
    }
    //
    private let input: String           // 解析する文字列
    static let EoT = Token()            // End of Tokens
    static let ESC = Token.Symbol.ESC.rawValue
    private var position = 0            // 最後に読み込んだ位置
    private var positionTobeRead = 0    // 次に読もうとしている位置
    private var taggedWords: [(word: String, type: TagType)] = [] // 解析(分類)結果文字列
    enum TagType : String {case word, symbol, none}
    private var identifiers = Identifiers()  // 定義された識別子
    //
    func insert(_ identifier: String) {identifiers.insert(identifier)}
    // トークン解析
    func getNext() -> Token {
        var token = nextToken
        skipToken()
        let compoundToken = Token(word: token.literal + nextToken.literal)
        /// 識別子として使用不可:  助詞を含む文字列、記号・数値で始まる文字列、記号で終わる文字列、予約語「する」「こと」「また」「以上」「以下」「未満」で終わる文字列
        switch (token, nextToken, compoundToken) {
        case (.IDENT(_),.keyword(let keyword),_):       // 識別子 + 予約語 → 合成
            if ![.SURU,.KOTO,.MATA,.CASE].contains(keyword) {
                token = Token(word: token.literal + getNext().literal)
            }   // (する、こと、また、場合を除く)
        case (_,_,.keyword(_)), (_,_,.particle(_)),
            (.IDENT(_),.IDENT(_),_), (.IDENT(_),.wrapped(.ident,_),_), (.IDENT(_),.INT(_),_),
            (.keyword(_),.IDENT(_),_),(.wrapped(.ident,_),.IDENT(_),_):
            token = Token(word: token.literal + getNext().literal)  // 識別子を合成
        case (.keyword(let current),.keyword(let next),_):     // 予約語 + 予約語
            if next == .KOTO ||                         // 「こと」
               (current == .EXECUTE && next == .SURU) { // 「実行する」
                _ = getNext()                           //　「する」「こと」は飛ばす
            }
        case  (.keyword(let current),.wrapped(.keyword,let next),_): // 予約語 + 連用形
            if current == .EXECUTE &&
                ContinuativeForm(next).plainFormType == .keyword(.SURU) {   // 「実行し」
                _ = getNext()                           // 「し」は飛ばす
            }
        case (.INT(_),.IDENT(_),_):                     // 数値 + 単位 → 単位を無視する
            while nextToken.type == .ident {skipToken()}
        case (.symbol(.LWBRACKET),_,_):                 // "『" 文字列開始
            guard let s = readString(until: .RWBRACKET) else {  // 識別子トークンに変換
                return Token(illegal: "』が見つからない")
            }
            token = Token(ident: s)
        case (.symbol(.LBRACKET),_, _):                 // "「" 文字列開始
            guard let s = readString(until: .RBRACKET) else {   // 文字列トークンに変換
                return Token(illegal: "」が見つからない")
            }
            token = Token(string: s)
        case (.symbol(.LPAREN),_,_):                    // "（" コメント開始
            guard skipToken(until: .RPAREN) else {      // "）" まで読み飛ばす
                return Token(illegal: "）が見つからない")
            }
            token = getNext()
        case (.symbol(.REFMARK),_,_):                   // "※" コメント開始
            _ = skipToken(until: .EOL)                  // 行末("\n")まで読み飛ばす
            token = Token(symbol: .EOL)                 // 改行("\n")を返す
        case (.symbol(.MINUS),.INT(_),_):
            if let number = nextToken.number {
                token = Token(number: -number)          // 整数を負にする
                skipToken()
                while nextToken.type == .ident {skipToken()}    // 単位を無視する
            }
        case (.symbol(.SPACE),_,_), (.symbol(.TAB),_,_), (.symbol(.CR),_,_):   // 読み飛ばす文字
            token = getNext()
        default:
            break
        }
        return token
    }
    var enumerated: [(String, String)] {taggedWords.map {($0, $1.rawValue)}}
    //　ヘルパー
    /// 入力をNLTaggerで分解し、タグ(TagType)を付加して、taggedWords に追加
    /// 参考: https://developer.apple.com/documentation/naturallanguage/nltagscheme/2976614-tokentype
    /// - Parameter input: 入力文字列
    private func tagging(_ input: String) {
        let tagger = NLTagger(tagSchemes: [.tokenType])
        tagger.string = input
        tagger.enumerateTags(
            in:    input.startIndex..<input.endIndex,
            unit:  .word,
            scheme:.tokenType,
            options: []) { tag, range in
                if let tag = tag {
                    let s = String(input[range])
                    let tagType: TagType = (tag == .word) ? .word : .symbol
                    append(word: s, type: tagType)
                }
                return true
            }
    }
    /// NLTaggerが合成する単語を必要に応じて分解する。
    /// - 記号(punctuation)をまとめて返すので、記号の場合は１文字ずつに分解
    /// - 数字と後続をまとめるので、分解(小数点はそのまま)→後続も分解
    private func append(word: String, type: TagType) {
        if word.count > 1 && type == .symbol {              // ２文字以上の記号を分解
            word.forEach {taggedWords.append((word: String($0), type: type))}
        } else
        if word.count > 1 && word.first!.isNumber {         // 数値とその他に分解
            let (number, string) = breakUp(word)
            taggedWords.append((word: number, type: .word))
            if !string.isEmpty {
                append(word: string, type: .word)
            }
        } else
        if word.count > 1 && word.first!.isPunctuation {    // 記号とその他に分解
            taggedWords.append((word: String(word.first!), type: .symbol))
            append(word: String(word.suffix(word.count - 1)), type: .word)
        } else {
            taggedWords.append((word: word, type: type))
        }
    }
    private func breakUp(_ word: String) -> (String, String) {
        let s = word.startIndex, e = word.endIndex
        let i = word.firstIndex(where: {!$0.isNumber && $0 != "."}) ?? e
        return (String(word[s..<i]), String(word[i..<e]))
    }
    private lazy var nextToken: Token = getNextToken()
    private func getNextToken() -> Token {
        guard positionTobeRead < taggedWords.count else {return Self.EoT}
        let words = Array((taggedWords.map {$0.word})[positionTobeRead...])
        if let (identifier, count) = identifiers.identifier(in: words) {
            positionTobeRead += count
            return Token(ident: identifier)
        }
        let tagged = taggedWords[positionTobeRead]
        return (tagged.type == .symbol) ?
        Token(symbol: tagged.word) :
        Token(word: tagged.word)
    }
    private func readString(until endSymbol: Token.Symbol) -> String? {
        var string: String = ""
        while nextToken != .symbol(endSymbol) && nextToken != Self.EoT {
            if nextToken.literal == Self.ESC {              // ESC + endSymbol → endSymbol
                skipToken()
                if nextToken != .symbol(endSymbol) {string += Self.ESC}
            }
            string += nextToken.literal
            skipToken()
        }
        guard positionTobeRead < taggedWords.count else {return nil}    // endSymbolが見つからない
        skipToken()                                         // symbolを読み飛ばす
        return string
    }
    private func skipToken() {
        position = positionTobeRead
        positionTobeRead += 1
        nextToken = getNextToken()
    }
    private func skipToken(until endSymbol: Token.Symbol) -> Bool {
        let beginSymbol = [")":"(", "）":"（", "」":"「", "』":"『",][endSymbol.rawValue]
        while nextToken != .symbol(endSymbol) && nextToken != Self.EoT {
            if nextToken.literal == Self.ESC {skipToken()}  // ESC + endSymbolの場合は、スキップ
            if nextToken.literal == beginSymbol {
                while nextToken != .symbol(endSymbol) && nextToken != Self.EoT {
                    guard positionTobeRead < taggedWords.count else {return false}
                    skipToken()
                }
            }   // endSymbolに対応するbeginSymbolを見つけたら、endSymbolmまで読み飛ばす。
            skipToken()
        }
        guard positionTobeRead < taggedWords.count else {return false}  // endSymbolが見つからない
        skipToken()                                         // endSymbolを読み飛ばす
        return true
    }
}
/// 識別子辞書：定義された識別子を記憶する。
struct Identifiers {
    private var identifiers: Set<String> = []
    private var maxLen = 0
    mutating func insert(_ s: String) {identifiers.insert(s); if s.count > maxLen {maxLen = s.count}}
    func identifier(in words: [String]) -> (String, Int)? {
        var string = "", pos = 0
        while pos < words.count && string.count <= maxLen && !identifiers.isEmpty {
            string += String(words[pos])
            if identifiers.contains(string) {return (string, pos)}
            pos += 1
        }
        return nil
    }
}
