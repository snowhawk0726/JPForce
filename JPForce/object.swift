//
//  object.swift
//  日本語ふぉーす(JPForce)
//
//  Created by 佐藤貴之 on 2023/03/06.
//

import Foundation

// MARK: - Interfaces
protocol JpfObject : Accessible {
    static var type: String {get}
    var type: String {get}
    var name: String {get set}
    var string: String {get}
    // 値を取り出す
    var number: Int? {get}
    var particle: Token? {get}
    var value: JpfObject? {get}
    var error: JpfError? {get}
    /// nameと格をキーに、関連する値を取り出す(あるいは計算する)。
    subscript(name: String, particle: Token?) -> JpfObject? {get}
    // 判定
    var isTrue: Bool {get}
    var isNull: Bool {get}
    var isNumber: Bool {get}
    var isReturnValue: Bool {get}
    var isBreak: Bool {get}
    var isContinue: Bool {get}
    var isError: Bool {get}
    var hasName: Bool {get}
    var hasValue: Bool {get}
    func isParticle(_ particle: Token.Particle) -> Bool
    func isEqual(to object: JpfObject) -> Bool
    func contains(name: String) -> Bool
    func contains(type: String) -> Bool
    // 演算
    func add(_ object: JpfObject) -> JpfObject
    func remove(_ object: JpfObject) -> JpfObject
    func contains(_ object: JpfObject) -> JpfObject
    func contains(where function: JpfFunction, with environment: Environment) -> JpfObject
    func foreach(_ function: JpfFunction, with environment: Environment) -> JpfObject?
    func map(_ function: JpfFunction, with environment: Environment) -> JpfObject
    func filter(_ function: JpfFunction, with environment: Environment) -> JpfObject
    func reduce(_ initial: JpfObject, _ function: JpfFunction, with environment: Environment) -> JpfObject
    func sorted() -> JpfObject
    func sorted(by string: JpfString) -> JpfObject
    func sorted(by function: JpfFunction, with environment: Environment) -> JpfObject
    func reversed() -> JpfObject
    func assign(_ value: JpfObject, to target: JpfObject?) -> JpfObject
    var count: JpfObject {get}
    var isEmpty: JpfObject {get}
    // コンパイラ用(in compilerObjectsExtension.swift)
    func emit(with c: Compiler) throws
    var isDefined: Bool {get}   // 識別子が定義済み
}
protocol JpfHashable {
    var hashKey: JpfHashKey {get}
}
// MARK: - Implements
extension JpfObject {
    var type: String {Self.type}
    var number: Int? {nil}
    var particle: Token? {nil}
    var value: JpfObject? {self}
    var error: JpfError? {nil}
    //
    var isTrue: Bool {true}
    var isNull: Bool {false}
    var isNumber: Bool {false}
    var isReturnValue: Bool {false}
    var isBreak: Bool {false}
    var isContinue: Bool {false}
    var isError: Bool {false}
    var isBreakFactor: Bool {isReturnValue || isBreak || isContinue || isError}
    var hasName: Bool {!name.isEmpty}
    var hasValue: Bool {value != nil}
    func isParticle(_ particle: Token.Particle) -> Bool {false}
    func isEqual(to object: JpfObject) -> Bool {type == object.type && isTrue == object.isTrue}
    func contains(name: String) -> Bool {false}
    func contains(type: String) -> Bool {type == self.type}
}
struct JpfInteger : JpfObject, JpfHashable, Comparable {
    static let type = "数値"
    var name: String = ""
    var value: Int
    //
    var string: String {String(value).color(.blue)}
    //
    var number: Int? {value}
    var isNumber: Bool {true}
    func isEqual(to object: JpfObject) -> Bool {
        guard object.isNumber else {return isTrue == object.isTrue}
        return value == object.number
    }
    //
    var hashKey: JpfHashKey {JpfHashKey(type: type, value: value.hashValue)}
    static func < (lhs: Self, rhs: Self) -> Bool {lhs.value < rhs.value}
}
struct JpfBoolean : JpfObject, JpfHashable {
    static let type = "真偽値"
    var name: String = ""
    var value: Bool
    var string: String {(value ? Token.Keyword.TRUE.rawValue : Token.Keyword.FALSE.rawValue).color(.magenta)}
    //
    static func object(of native: Bool) -> JpfBoolean {native ? Self.TRUE : Self.FALSE}
    static let TRUE =  JpfBoolean(value: true)
    static let FALSE = JpfBoolean(value: false)
    //
    var isTrue: Bool {value}
    func isEqual(to object: JpfObject) -> Bool {value == object.isTrue}
    //
    var hashKey: JpfHashKey {JpfHashKey(type: type, value: value.hashValue)}
}
struct JpfString : JpfObject, JpfHashable, Comparable {
    static let type = "文字列"
    var name: String = ""
    var value: String
    var string: String {value.color(.red)}
    //
    func isEqual(to object: JpfObject) -> Bool {
        guard let rhs = object as? JpfString else {return isTrue == object.isTrue}
        return value == rhs.value
    }
    //
    var hashKey: JpfHashKey {JpfHashKey(type: type, value: value.hashValue)}
    static func < (lhs: Self, rhs: Self) -> Bool {lhs.value < rhs.value}
}
struct JpfIdentifier : JpfObject {
    static let type = "識別子"
    var name: String = ""   // AST上の識別子名
    var value: String       // 登録名
    var symbol: Symbol?     // 登録シンボル
    var isLhs: Bool = false // 左辺オブジェクト
    //
    var string: String {value.color(.cyan)}
    // インタープリタ用
    init(from ident: Identifier) {
        self.name = ident.value
        self.value = ident.value
        self.isLhs = ident.isLhs
    }
}
struct JpfRange : JpfObject {
    static let type = "範囲"
    var name: String = ""
    var lowerBound: (JpfInteger, Token)?
    var upperBound: (JpfInteger, Token)?
    //
    var string: String {type.color(.magenta) + "【" +
        (lowerBound.map {$0.string + $1.literal} ?? "") + comma +
        (upperBound.map {$0.string + $1.literal} ?? "") + "】"}
    private var comma: String {(lowerBound != nil && upperBound != nil) ? "、" : ""}
    //
    var lowerBoundNumber: Int? {
        guard lowerBound?.1 == .particle(.KARA) || lowerBound?.1 == .particle(.GTEQUAL) else {return nil}
        return lowerBound?.0.value
    }
    var upperBoundNumber: Int? {
        switch upperBound?.1 {
        case .particle(.MADE),.particle(.LTEQUAL):
            return upperBound?.0.value
        case .particle(.UNDER):
            guard let n = upperBound?.0 else {return nil}
            return n.value - 1
        default:
            return nil
        }
    }
}
struct JpfNull : JpfObject {
    static let type = "無"
    var name: String = ""
    //
    var string: String {type.color(.magenta)}
    //
    static let object = JpfNull()
    private init() {}
    //
    var isTrue: Bool {false}
    var isNull: Bool {true}
    func isEqual(to object: JpfObject) -> Bool {object.isNull}
}
struct JpfPhrase : JpfObject {
    static let type = "句"
    var name: String = ""
    var value: JpfObject?
    var particle: Token?
    //
    var string: String {(value?.string ?? "") + (particle?.literal.color(.magenta) ?? "")}
    //
    var isTrue: Bool {value?.isTrue ?? false}
    var isNull: Bool {value?.isNull ?? false}
    var number: Int? {value?.number}
    var isNumber: Bool {value?.isNumber ?? false}
    var isReturnValue: Bool {value?.isReturnValue ?? false}
    var error: JpfError? {value?.error}
    var isError: Bool {value?.isError ?? false}
    func isParticle(_ p: Token.Particle) -> Bool {self.particle?.isParticle(p) ?? false}
}
struct JpfReturnValue : JpfObject {
    static let type = "返り値"
    var name: String = ""
    var value: JpfObject?       // 返す値がない場合、nil
    //
    var string: String {Self.type + ": " + (value?.string ?? "無し")}
    //
    var isReturnValue: Bool {true}
}
struct JpfLoopControl : JpfObject {
    static let type = "反復制御"
    var name: String = ""
    enum Method {case BREAK, CONTINUE}
    let method: Method
    let value: JpfObject? = nil // 返す値がない
    //
    var string: String {Self.type + "方法: " + (method == .BREAK ? Token(.BREAK).literal : Token(.CONTINUE).literal)}
    //
    var isBreak: Bool {method == .BREAK}
    var isContinue: Bool {method == .CONTINUE}
}
struct JpfFunction : JpfObject {
    static let type = "関数"
    var name: String = ""
    var overload: FunctionBlocks    // 関数ブロック
    var environment: Environment
    //
    var string: String {
        let s = overload.all.reduce("") {$0 +
            "\($1.isOverloaded ? "さらに、" : "")\(type.color(.magenta))であって、【\($1.string)】"
        }
        return s.replacingOccurrences(of: "。】", with: "】")
    }
}
struct JpfComputation : JpfObject {
    static let type = "算出"
    var name: String = ""
    var setters: FunctionBlocks     // 設定ブロック
    var getters: FunctionBlocks     // 取得ブロック
    var environment: Environment
    //
    var string: String {
        let s = type.color(.magenta) + "であって、【" +
        (setters.all.reduce("") {$0 + "設定は、\($1.isOverloaded ? "さらに、" : "")【\($1.string)】。"}) +
        (getters.all.reduce("") {$0 + "取得は、\($1.isOverloaded ? "さらに、" : "")【\($1.string)】。"}) +
        "】"
        return s.replacingOccurrences(of: "。】", with: "】")
    }
}
struct JpfEnum : JpfObject {
    static let type = "列挙"
    var name: String = ""
    var elements: [String]
    var environment: Environment
    //
    var string: String {type.color(.magenta) + "であって、【" +
        (elements.isEmpty ? "" : "要素が、\(elements.map {$0}.joined(separator: "と、"))") + "】"
    }
}
struct JpfEnumerator : JpfObject {
    static let type = "列挙子"
    var type: String
    var name: String = ""
    var identifier: String
    var rawValue: JpfObject?
    //
    var string: String {"列挙型名は" + (!type.isEmpty ? type : "無し") + "で、列挙子は\(identifier)、" + (rawValue.map {"値は\($0.string)。"} ?? "。")}
    //
    func isEqual(to object: JpfObject) -> Bool {
        guard let rhs = object as? JpfEnumerator else {return false}
        return type == rhs.type && identifier == rhs.identifier &&
        (rhs.rawValue != nil ? rawValue?.isEqual(to: rhs.rawValue!) ?? false : rawValue == nil)
    }
}
struct JpfType : JpfObject {
    static let type = "型"
    var name: String = ""
    var initializers: FunctionBlocks// 初期化処理
    var environment: Environment    // 要素(メンバー)を含む環境
    var protocols: [String]         // 準拠する規約
    var body: BlockStatement?
    //
    var string: String {
        var s1 = type.color(.magenta) + "であって、【" +
        (protocols.isEmpty ? "" : "準拠する規約は、\(protocols.map {$0}.joined(separator: "と、"))。") +
        (environment.enumerated.isEmpty ? "" : "型の要素が、\(environment.enumerated.map {$0.key}.joined(separator: "と、"))。") +
        (initializers.all.reduce("") {$0 + "初期化は、さらに、【\($1.string)】。"}) 
        let s2 = (body.map {"本体が、\($0.string)"} ?? "") +
        "】"
        s1.range(of: "さらに、").map {s1.replaceSubrange($0, with: "")} // 初出の「さらに、」を取り除く
        return (s1 + s2).replacingOccurrences(of: "。】", with: "】")
    }
}
struct JpfInstance : JpfObject {
    static let type = "インスタンス"
    static let SELF = "自身"
    var type: String = ""
    var name: String = ""
    var environment: Environment    // 要素(メンバー)を含む環境
    var protocols: [String]         // 準拠する規約
    var available: Set<String>      // 外部から利用可能なメンバー
    //
    var string: String {"型が、\(type)で、要素が、\(environment.enumerated.map {$0.key}.joined(separator: "と、"))。" + available.map {"「\($0)」"}.joined(separator: "と") + "は利用可能。"}
}
struct JpfProtocol : JpfObject {
    static let type = "規約"
    var name: String = ""
    var protocols: [String]         // 準拠する規約
    var clauses: [ClauseLiteral]    // 条項
    var body: BlockStatement?       // 規約のデフォルト実装
    //
    var string: String {
        let s = type.color(.magenta) + "であって、【" +
        (protocols.isEmpty ? "" : "準拠する規約は、\(protocols.map {$0}.joined(separator: "と"))。") +
        "条項が、" + clauses.map {$0.string}.joined(separator: " ") +
        "】"
        return s.replacingOccurrences(of: "。】", with: "】")
    }
}
struct JpfArray : JpfObject {
    static let type = "配列"
    var name: String = ""
    var elements: [JpfObject]
    //
    var string: String {type.color(.magenta) + "であって、【" +
        (elements.isEmpty ? "" :
         "要素が、\(elements.map {$0.string}.joined(separator: "と、"))") + "】"
    }
}
struct JpfInput : JpfObject {
    static let type = "入力"
    var name: String = ""
    var stack: [JpfObject]
    //
    var string: String {
        "(" + (stack.isEmpty ? "" : "\(stack.map {$0.string}.joined(separator: " "))") + ")"
    }
}
struct JpfHashKey : JpfObject, Hashable {
    static let type = "ハッシュ索引"
    var name: String = ""
    var type: String
    var value: Int
    var string: String {"\(type): \(value)"}
}
struct JpfDictionary : JpfObject {
    static let type = "辞書"
    var name: String = ""
    /// キーがハッシュ化されているため、元のキーも値として保持しておく｀
    var pairs: [JpfHashKey : (key: JpfObject, value: JpfObject)]
    //
    var string: String {type.color(.magenta) + "であって、【" +
        (pairs.isEmpty ? "" : "要素が、\(pairs.map {"\($0.value.key.string)が\($0.value.value.string)"}.joined(separator: "と、"))") +
        "】"
    }
    //
    subscript(key: JpfHashKey) -> (JpfObject, JpfObject)? {
        get {return pairs[key]}
        set {pairs[key] = newValue}
    }
    subscript(key: JpfObject) -> JpfObject? {
        get {
            guard let keyObject = key as? JpfHashable else {return nil}
            return pairs[keyObject.hashKey]?.value
        }
        set {
            guard let keyObject = key as? JpfHashable, let value = newValue else {return}
            pairs[keyObject.hashKey] = (key, value)
        }
    }
}
struct JpfError : JpfObject, Error {
    static let type = "エラー"
    var name: String = ""
    var message: String
    var string: String {"\(Self.type)：\(message.color(.red))"}
    var error: JpfError? {self}
    var isError: Bool {true}
    //
    func isEqual(to object: JpfObject) -> Bool {string == object.string}
    //
    init(_ message: String) {self.message = message}
    //
    static func + (lhs: Self, rhs: Self) -> Self {JpfError(lhs.message + rhs.message)}
    static func + (lhs: Self, rhs: String) -> Self {JpfError(lhs.message + rhs)}
    static func + (lhs: String, rhs: Self) -> Self {JpfError(lhs + rhs.message)}
}
struct JpfCompiledFunction : JpfObject {
    static let type = "翻訳済み関数"
    var name: String = ""
    var instructions: Instructions
    var numberOfLocals = 0          // ローカル変数の数
    var numberOfParameters = 0      // 仮引数の数
    //
    var string: String {
        "\(type.color(.magenta))であって、【入力が、\(numberOfParameters)個。本体が、\(instructions.decimalStrings)】"
    }
}
struct JpfClosure : JpfObject {
    init(with fn: JpfCompiledFunction, _ free: [JpfObject] = []) {self.fn = fn; self.free = free}
    static let type = "クロージャ"
    var name: String = ""
    var fn: JpfCompiledFunction?
    var free: [JpfObject] = []
    var string: String {
        "\(type.color(.magenta))であって、\(free.count)個の自由変数を閉包。"
    }
}
