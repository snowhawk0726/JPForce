//
//  predicateOperable.swift
//  日本語ふぉーす(JPForce)
//
//  Created by 佐藤貴之 on 2023/03/06.
//
import Foundation
import AVFAudio

// MARK: - intefaces for operation
protocol PredicateOperable {
    var environment: Environment {get}
    /// 述語を評価する。
    /// - Returns: 評価結果(JpfObjectもしくはnil)
    func operated() -> JpfObject?
}
// MARK: - predicate operable instance factory
struct PredicateOperableFactory {
    // 予約語とそれを実行するPredicateOperableを返す関数の配列
    static let predicates: [(keyword: Token.Keyword, operator: (Environment) -> PredicateOperable)] = [
        (.APPEND,       {AppendOperator($0, by: Token(.APPEND))}),      // 0
        (.REMOVE,       {RemoveOperator($0, by: Token(.REMOVE))}),
        (.CONTAINS,     {ContainsOperator($0, by: Token(.CONTAINS))}),
        (.FOREACH,      {ForeachOperator($0, by: Token(.FOREACH))}),
        (.MAP,          {MapOperator($0, by: Token(.MAP))}),
        (.FILTER,       {FilterOperator($0, by: Token(.FILTER))}),      // 5
        (.REDUCE,       {ReduceOperator($0, by: Token(.REDUCE))}),
        (.SORT,         {SortOperator($0, by: Token(.SORT))}),
        (.REVERSE,      {ReverseOperator($0, by: Token(.REVERSE))}),
        (.PRINT,        {PrintOperator($0, by: Token(.PRINT))}),
        (.ASK,          {PrintOperator($0, by: Token(.ASK))}),          // 10
        (.NEWLINE,      {NewlineOperator($0)}),
        (.READ,         {ReadOperator($0, by: Token(.READ))}),
        (.FILES,        {FilesOperator($0, by: Token(.FILES))}),
        (.INPUT,        {StackOperator($0)}),
        (.DROP,         {DropOperator($0)}),                            // 15
        (.EMPTY,        {EmptyOperator($0, by: Token(.EMPTY))}),
        (.DUPLICATE,    {PullOperator($0, by: Token(.DUPLICATE))}),
        (.PULL,         {PullOperator($0, by: Token(.PULL))}),
        (.PUSH,         {PushOperator($0, by: Token(.PUSH))}),
        (.ASSIGN,       {AssignOperator($0)}),                          // 20
        (.SET,          {SetOperator($0, by: Token(.SET))}),
        (.SWAP,         {SwapOperator($0, by: Token(.SWAP))}),
        (.IDENTIFIERS,  {IdentifiersOperator($0, by: Token(.IDENTIFIERS))}),
        (.ADD,          {AddOperator($0)}),
        (.MULTIPLY,     {MultiplyOperator($0, by: Token(.MULTIPLY))}),  // 25
        (.SUBSTRACT,    {SubstractOperator($0, by: Token(.SUBSTRACT))}),
        (.DIVIDE,       {DivideOperator($0, by: Token(.DIVIDE))}),
        (.NEGATE,       {NegateOperator($0, by: Token(.NEGATE))}),
        (.LT,           {CompareOperator($0, by: Token(.LT))}),
        (.GT,           {CompareOperator($0, by: Token(.GT))}),         // 30
        (.EQUAL,        {BooleanOperator($0, by: Token(.EQUAL))}),
        (.BE,           {BooleanOperator($0, by: Token(.BE))}),
        (.NOT,          {BooleanOperator($0, by: Token(.NOT))}),
        (.RETURN,       {ReturnOperator($0)}),
        (.GOBACK,       {GobackOperator($0)}),                          // 35
        (.BREAK,        {LoopControlOperator($0, by: Token(.BREAK))}),
        (.CONTINUE,     {LoopControlOperator($0, by: Token(.CONTINUE))}),
        (.MONO,         {UnwrapOperator($0, by: Token(.MONO))}),        // 〜たもの
        (.NULL,         {NullOperator($0)}),
        (.EXECUTE,      {ExecuteOperator($0)}),                         // 40 (関数)を実行
        (.CREATE,       {CreateOperator($0)}),                          // (型)から生成
        (.INITIALIZATION,   {InitializeOperator($0)}),
        (.SURU,         {PerformOperator($0)}),                         // 〜にする、〜をする
        (.AVAILABLE,    {AvailableOperator($0)}),
        (.KOTO,         {NopOperator($0)}),                             // 45
    ]
    // アクセサ
    subscript(_ name: String) -> ((Environment) -> PredicateOperable)? { // インデックス：名称
        Self.predicates.first(where: {$0.keyword.rawValue == name})?.operator
    }
    subscript(_ token: Token) -> ((Environment) -> PredicateOperable)? { // インデックス：トークン
        Self.predicates.first(where: {token.isKeyword($0.keyword)})?.operator
    }
    subscript(_ index: Int) -> ((Environment) -> PredicateOperable)? {  // インデックス：位置
        guard (0..<Self.predicates.count).contains(index) else {return nil}
        return Self.predicates[index].operator
    }
    // ファクトリーメソッド
    static func create(from token: Token, with environment: Environment) -> PredicateOperable? {
        Self()[token]?(environment) ?? NopOperator(environment)
    }
}
/// stringをsplit()により、[String]に分解する。
/// 『<識別子>』もしくは、\(<識別子>)を識別子の内容に置き換える。
struct Splitter {
    init(of string: String, with environment: Environment, terminator: String = "") {self.string = string; self.environment = environment; self.terminator = terminator}
    let string: String
    let environment: Environment
    let terminator: String
    var error: JpfError?
    /// 括弧(lとr)で囲われた識別子の中身を文字列の配列に分割する。
    let separators: [(l: String, r: Character, e: String)] = [("『", "』","\\『"), ("\\（", "）","")]
    mutating func split() -> [String]? {
        var strings: [String] = []
        guard let splitted = split(string, with: separators[0]) else {return nil}
        for string in splitted {
            guard let splitted = split(string, with: separators[1]) else {return nil}
            strings += splitted
        }
        return strings
    }
    /// 指定した括弧で囲まれた識別子の値(文字列)を取り出し、前後の文字列とともに配列に格納する。
    /// (改行を回避するために、終端文字(terminator)を文字列に追加する。)
    /// - Parameters:
    ///   - string: 対象の文字列
    ///   - paren: 識別子を囲う記号の組(lとr)と、左(l)のエスケープ文字(e) (""の場合は無し)
    /// - Returns: 文字列の配列、もしくはnil (errorにエラーを出力)
    mutating private func split(_ string: String, with paren: (l: String, r: Character, e: String)) -> [String]? {
        var splitted: [String] = []
        if !paren.e.isEmpty, let escape = string.firstRange(of: paren.e) {  // エスケープ文字の処理
            let (header, body) = string.divided(after: escape)
            guard let rest = split(body, with: paren) else {return nil}     // エスケープ文字の後ろを分解
            splitted = [header + terminator] + rest
        } else
        if let left = string.firstRange(of: paren.l) {                      // 左カッコの処理
            let (header, body) = string.divided(without: left)
            guard let right = body.firstIndex(of: paren.r) else {           // 右カッコの処理
                error = JpfError("識別子を囲う、閉じカッコ「\(paren.r)」が見つからない。")
                return nil
            }
            let (ident, rest) = body.divided(without: right)
            guard let object = environment[ident] else {                    // 識別子の内容を切り出す
                error = JpfError("『\(ident)』(識別子)が定義されていない。")
                return nil
            }
            guard let rest = split(rest, with: paren) else {return nil}     // 残りを分解し、配列に追加
            splitted = [header + object.string + terminator] + rest
        } else {
            splitted = [string]
        }
        return splitted
    }
}
extension String {  // 文字列を分割する。
    /// 自身をindexを境に前後に分割する。(indexを含まない)
    func divided(without index: String.Index) -> (String, String) {
        (String(self[self.startIndex..<index]),
         String(self[self.index(after: index)..<self.endIndex]))
    }
    ///　自身をrangeを境に分割する。(rangeは含まない)
    func divided(without range: Range<String.Index>) -> (String, String) {
        (String(self[self.startIndex..<range.lowerBound]),
         String(self[range.upperBound..<self.endIndex]))
    }
    /// 自身をindexを境に前後に分割する。(indexを含む)
    func divided(after index: String.Index) -> (String, String) {
        (String(self[self.startIndex..<index]),
         String(self[index..<self.endIndex]))
    }
    /// 自身をrangeの上限を境に前後に分割する。(rangeを含む)
    func divided(after range: Range<String.Index>) -> (String, String) {
        self.divided(after: range.upperBound)
    }
}
// MARK: - implements
extension PredicateOperable {
    // Stack operations
    var isPeekNumber: Bool {environment.peek?.isNumber ?? false}
    func isPeekParticle(_ expected: Token.Particle?) -> Bool {
        guard let particle = expected else {return !environment.isPeekParticle}
        return environment.isPeekParticle(particle)
    }
    var leftOperand: JpfObject? {return environment.pull()}
    var leftNumber: Int? {
        guard let number = environment.peek?.number else {return nil}
        environment.drop()
        return number
    }
    var unwrappedValue: JpfObject? {environment.unwrappedValue}
    /// object.stringをout()で出力する。
    /// - Parameters:
    ///   - object: 対象のオブジェクト
    ///   - withEscapeProcessing: エスケープ文字の有無
    ///   - out: 出力関数(引数１：出力文字列、引数２：terminator文字列、もしくはnil)
    /// - Returns: エラー(nilはエラー無し)
    func output(_ object: JpfObject, withEscapeProcessing: Bool, out: (String, String?) -> Void) -> JpfError? {
        switch object.name {                    // ラベルのチェック
        case Token.Keyword.IDENTIFIER.rawValue: // 識別子(定義)を出力
            guard let identifier = object as? JpfString else {break}
            guard let definition = environment[identifier.value] else {return JpfError("『\(identifier.value)』(識別子)が定義されていない。")}
            out(definition.string, nil)
            environment.remove(name: object.name)
            return nil
        case Token.Keyword.FILE.rawValue:       // ファイル(の内容)を出力
            guard let filename = object as? JpfString else {break}
            guard let contents = try? String(contentsOfFile: directoryPath + filename.value, encoding: .utf8) else {return fileReadError}
            out(contents, nil)
            environment.remove(name: object.name)
            return nil
        default:
            break
        }
        let terminator = "\\末尾"
        var splitter = Splitter(of: object.string, with: environment, terminator: withEscapeProcessing ? terminator : "")
        guard let strings = splitter.split() else {return splitter.error!}  // 識別子の内容を出力
        strings.forEach { s in
            if withEscapeProcessing {
                let replaced = replaced(s)                                  // エスケープ処理(変換)
                if let range = replaced.firstRange(of: terminator) {        // 終端を検出
                    let (body, terminator) = replaced.divided(without: range)
                    out(body, terminator)                                   // 終端あり出力
                } else {
                    out(replaced, nil)                                      // 終端なし出力
                }
            } else {
                out(s, nil)                                                 // エスケープ処理なし出力
            }
        }
        return nil
    }
    /// エスケープ文字を(Swiftの)制御コードに変換する。
    /// 「\改行なし」が文字列の後尾にある場合、改行をせずに表示する。
    /// 　(\は、そのまま使えるが、Swiftに合わせた。(「"」とか「'」は合わせてない))
    private func replaced(_ string: String) -> String {
        let codes = [("\\t","\t"),("\\n","\n"),("\\r","\r"),("\\0","\0"),("\\\\","\\"),("\\「","「"),("\\」","」"),("\\『","『"),("\\』","』"),("\\改行なし","\\末尾")]
        return codes.reduce(string) {$0.replacingOccurrences(of: $1.0, with: $1.1)}
    }
    /// ディレクトリパス
    /// 辞書に識別子「ディレクトリパス」があれば、その値を、なければ、書類フォルダのパス(文字列)を返す。
    var directoryPath: String {
        if let path = environment[Identifier.directoryPath] as? JpfString {
            return URL(fileURLWithPath: path.value).path()
        }
        // デフォルト(書類)
        return FileManager.default.urls(for: .documentDirectory, in: .userDomainMask)[0].path()
    }
    // エラー
    var numerationParamError1: JpfError {JpfError("には２つの数値入力が必要。")}
    var numerationParamError2: JpfError {JpfError("には２つ以上の数値入力が必要。")}
    var numerationParamError3: JpfError {JpfError("には１つの数値入力が必要。")}
    var atLeastOneParamError: JpfError  {JpfError("には１つ以上の入力が必要。")}
    var oneParamNeeded: JpfError        {JpfError("には１つの入力が必要。")}
    var twoParamsNeeded: JpfError       {JpfError("には２つの入力が必要。")}
    var additionParamError: JpfError    {JpfError("「足す」には、２つ以上の数値、文字列、配列の入力が必要。")}
    var particleError: JpfError         {JpfError("助詞が間違っている。")}
    var valueNotFound: JpfError         {JpfError("で判定すべき値が無かった。")}
    var determineError: JpfError        {JpfError("判定の述語が間違っている。述語：「ある」「ない」「等しい」")}
    var rangeFormatError: JpfError      {JpfError("範囲の判定対象は数値のみ。")}
    var returnParamError: JpfError      {JpfError("返すべき値がない。")}
    var cannotDivideByZero: JpfError    {JpfError("0で割ることはできない。")}
    var cannotCompare: JpfError         {JpfError("では比較できない。")}
    var functionObjectNotFound: JpfError{JpfError("実行すべき関数が見つからない。")}
    var cannotJudgeGenuineness: JpfError{JpfError("で、正負を判定できる対象は、数値型のみ。")}
    var fileReadError: JpfError         {JpfError("ファイルの読み込みに失敗した。")}
    var availableError: JpfError        {JpfError("利用可能な識別子名でなかった。")}
    var cannotConform: JpfError         {JpfError("には準拠できない。")}
    var valueOfEnumeratorNotFound: JpfError {JpfError("指定の値が見つからない。指定値：")}
    var cannotCreateFromProtocol:  JpfError {JpfError("規約からインスタンスは生成できない。")}
    var cannotInitialize: JpfError      {JpfError("オブジェクトの初期化ができない。")}
    var detectParserError: JpfError     {JpfError("構文解析器がエラーを検出した。")}
    // メッセージ(使い方)
    var printUsage: JpfError            {JpfError("仕様：(〜と…)〜を表示する。")}
    var askUsage: JpfError              {JpfError("仕様：(〜と…)〜と尋ねる。")}
    var readUsage: JpfError             {JpfError("仕様：(〜と…)〜を音読する。")}
    var additionUsage: JpfError         {JpfError("仕様：(〜と…)〜を足す。")}
    var multiplicationUsage: JpfError   {JpfError("仕様：(〜と…)〜を掛ける。")}
    var substractionUsage: JpfError     {JpfError("仕様：(〜から)〜を引く。または、(〜を)〜から引く。")}
    var divisionUsage: JpfError         {JpfError("仕様：(〜を)〜で割る。または、(〜で)〜を割る。")}
    var negateUsage: JpfError           {JpfError("仕様：〜の負数。または、〜を負数にする。")}
    var equalUsage: JpfError            {JpfError("仕様：〜(と)〜(が)等しい。")}
    var beUsage: JpfError               {JpfError("仕様：(〜が)〜である。または、(〜は)〜である。")}
    var notUsage: JpfError              {JpfError("仕様：(〜が)〜で(は)ない。または、(〜は)〜で(は)ない。")}
    var returnValueUsage: JpfError      {JpfError("仕様：(〜を)返す。")}
    var appendUsage: JpfError           {JpfError("仕様：〜(を)〜に追加する。または、〜(に)〜を追加する。")}
    var appendDictionaryUsage: JpfError {JpfError("仕様：〜が〜(を)〜に追加する。または、〜(に)〜が〜を追加する。")}
    var removeUsage: JpfError           {JpfError("仕様：(〜から)〜を削除する。")}
    var rangeCheckUsage: JpfError       {JpfError("仕様：<数値>が範囲【<範囲式>】に")}
    var determineUsage: JpfError        {JpfError("仕様：〜が<配列、範囲>に")}
    var containsUsage: JpfError         {JpfError("仕様：<配列、辞書、範囲>が<値/要素>を含む。")}
    var foreachUsage: JpfError          {JpfError("仕様：<配列、辞書、範囲>で<関数>を繰り返す。")}
    var mapUsage: JpfError              {JpfError("仕様：<配列、辞書、範囲>を<関数>で写像する。または、<範囲>を写像する。")}
    var filterUsage: JpfError           {JpfError("仕様：<配列、辞書>を<関数>で絞り込む。")}
    var reduceUsage: JpfError           {JpfError("仕様：<配列、辞書、範囲>を<初期値>と<関数>でまとめる。")}
    var sortUsage: JpfError             {JpfError("仕様：<配列>を<関数>で並べ替える。または、<配列>を(「昇順」/「降順」に）並べ替える。")}
    var reverseUsage: JpfError          {JpfError("仕様：<配列、文字列>を逆順にする。")}
    var pullDupUsage: JpfError          {JpfError("仕様：(識別子「<識別子>」と…)(識別子「<識別子>」に）(「数値」または「値」を)(<数値>個)")}
    var assignUsage: JpfError           {JpfError("仕様：〜(を)「<識別子>」に代入する。または、「<識別子>」に〜を代入する。")}
    var compoundAssignUsage: JpfError   {JpfError("仕様：<識別子>(を)<計算し>て代入する。")}
    var assignArrayUsage: JpfError      {JpfError("仕様：〜(を)<配列>の位置<数値>に代入する。または、<配列>の位置<数値>に〜を代入する。")}
    var swapUsage: JpfError             {JpfError("仕様：<識別子１>と<識別子２>を入れ替える。または、<識別子１>を<識別子２>と入れ替える。")}
    var createUsage: JpfError           {JpfError("仕様：(「<識別子>」を)(<引数>で)<型>から生成する。または、<型>から(<引数>で)「<識別子>」を生成する。")}
    var createEnumeratorUsage: JpfError {JpfError("仕様：(「<識別子>」を)<値>で<列挙型>から生成する。または、<列挙型>から<値>で「<識別子>」を生成する。")}
    var setUsage: JpfError              {JpfError("仕様：<値>(を)<オブジェクト>の要素「<識別子>」に設定する。または、<オブジェクト>の要素「<識別子>」に<値>を設定する。")}
}
// MARK: - 表示/音声
struct PrintOperator : PredicateOperable {
    init(_ environment: Environment, by op: Token) {self.environment = environment; self.op = op}
    let environment: Environment, op: Token
    func operated() -> JpfObject? {
        guard let object = environment.unwrappedPeek else {return "「\(op.literal)」" + atLeastOneParamError + (op.isKeyword(.PRINT) ? printUsage : askUsage)}
        var objects: [JpfObject] = [object]
        environment.drop()
        while isPeekParticle(.TO) {             // スタックにト格があれば、中身を格納
            objects.append(leftOperand!.value!)
        }
        for object in objects.reversed() {
            let result = output(object, withEscapeProcessing: true) {
                if let terminator = $1 {print($0, terminator: terminator)} else {print($0)}
            }
            if result?.isError ?? false {return result}
        }
        if op.isKeyword(.ASK), let input = readLine() {
            if let err = environment.push(JpfString(value: input)) {return err}
        }
        return nil
    }
}
struct NewlineOperator : PredicateOperable {
    init(_ environment: Environment) {self.environment = environment}
    let environment: Environment
    func operated() -> JpfObject? {print(); return nil}
}
struct ReadOperator : PredicateOperable {
    init(_ environment: Environment, by op: Token) {self.environment = environment; self.op = op}
    let environment: Environment, op: Token
    let synthesizer = AVSpeechSynthesizer()
    func operated() -> JpfObject? {
        guard let object = environment.unwrappedPeek else {return "「\(op.literal)」" + atLeastOneParamError + readUsage}
        var objects: [JpfObject] = [object]
        environment.drop()
        while isPeekParticle(.TO) {             // スタックにト格があれば、中身を格納
            objects.append(leftOperand!.value!)
        }
        for object in objects.reversed() {
            let result = output(object, withEscapeProcessing: false) {
                if $1 == nil {read($0)}
            }
            if result?.isError ?? false {return result}
        }
        return nil
    }
    private func read(_ string: String) {
        let utterance = AVSpeechUtterance(string: string)
        utterance.voice = AVSpeechSynthesisVoice(language: "ja-JP")
        let delegate = ReadDelegate()
        synthesizer.delegate = delegate
        DispatchQueue.main.async {
            synthesizer.speak(utterance)
        }
        CFRunLoopRun()
    }
    private class ReadDelegate : NSObject, AVSpeechSynthesizerDelegate {
        func speechSynthesizer(_ synthesizer: AVSpeechSynthesizer, didFinish utterance: AVSpeechUtterance) {
            CFRunLoopStop(CFRunLoopGetCurrent())
        }
    }
}
struct FilesOperator : PredicateOperable {
    init(_ environment: Environment, by op: Token) {self.environment = environment; self.op = op}
    let environment: Environment, op: Token
    /// 「書類」ディレクトリのファイル一覧を配列して返す。
    /// - Returns: ファイル名の配列、無い場合は「無」
    func operated() -> JpfObject? {
        guard let fileNames = try? FileManager.default.contentsOfDirectory(atPath: directoryPath) else {return JpfNull.object}
        return JpfArray(name: op.literal, elements: fileNames.map {JpfString(value: $0)})
    }
}
struct IdentifiersOperator : PredicateOperable {
    init(_ environment: Environment, by op: Token) {self.environment = environment; self.op = op}
    let environment: Environment, op: Token
    /// 識別子の一覧を辞書にして返す。
    /// - Returns: 識別子(JpfString)と定義内容(JpfString)の辞書
    func operated() -> JpfObject? {environment.stringDictionary}
}
// MARK: - 算術演算
struct AddOperator : PredicateOperable {
    init(_ environment: Environment) {self.environment = environment}
    let environment: Environment
    func operated() -> JpfObject? {
        guard let params = environment.peek(2)  else {return additionParamError + additionUsage}
        var added = params[0].add(params[1])
        if !added.isError {environment.drop(2)}
        while isPeekParticle(.TO) && !added.isError {   // スタックにト格があれば、中身を足す
            added = leftOperand!.add(added)
        }
        return added
    }
}
struct MultiplyOperator : PredicateOperable {
    init(_ environment: Environment, by token: Token) {self.environment = environment; self.op = token}
    let environment: Environment, op: Token
    func operated() -> JpfObject? {
        guard let params = environment.peek(2),
              let left = params[0].number, let right = params[1].number else {return "「\(op.literal)」" + numerationParamError2 + multiplicationUsage}
        let ident = params[0].value?.name ?? ""
        var number = left * right
        environment.drop(2)
        while isPeekParticle(.TO) && isPeekNumber {     // スタックにト格の数値があれば、掛ける
            number *= leftNumber!
        }
        return JpfInteger(name: ident, value: number)
    }
}
struct SubstractOperator : PredicateOperable {
    init(_ environment: Environment, by token: Token) {self.environment = environment; self.op = token}
    let environment: Environment, op: Token
    func operated() -> JpfObject? {
        guard let params = environment.peek(2),
              let left = params[0].number, let right = params[1].number else {return "「\(op.literal)」" + numerationParamError1 + substractionUsage}
        let ident = params[0].value?.name ?? ""
        switch (params[0].particle, params[1].particle) {
        case (Token(.KARA),Token(.WO)), (nil,Token(.WO)), (nil,nil):    // leftから、rightを引く
            environment.drop(2)
            return JpfInteger(name: ident, value: left - right)
        case (Token(.WO),Token(.KARA)), (nil,Token(.KARA)):             // leftを、rightから引く
            environment.drop(2)
            return JpfInteger(name: ident, value: right - left)
        default:
            return substractionUsage
        }
    }
}
struct DivideOperator : PredicateOperable {
    init(_ environment: Environment, by token: Token) {self.environment = environment; self.op = token}
    let environment: Environment, op: Token
    func operated() -> JpfObject? {
        guard let params = environment.peek(2),
              let left = params[0].number, let right = params[1].number else {return "「\(op.literal)」" + numerationParamError1 + divisionUsage}
        let ident = params[0].value?.name ?? ""
        switch (params[0].particle, params[1].particle) {
        case (Token(.WO),Token(.DE)), (nil,Token(.DE)), (nil,nil):      // leftを、rightで割る
            guard right != 0 else {return "\(left)を" + cannotDivideByZero}
            environment.drop(2)
            return JpfInteger(name: ident, value: left / right)
        case (Token(.DE),Token(.WO)), (nil,Token(.WO)):                 // leftで、rightを割る
            guard left != 0 else {return "\(right)を" + cannotDivideByZero}
            environment.drop(2)
            return JpfInteger(name: ident, value: right / left)
        default:
            return divisionUsage
        }
    }
}
struct NegateOperator : PredicateOperable {
    init(_ environment: Environment, by token: Token) {self.environment = environment; self.op = token}
    let environment: Environment, op: Token
    func operated() -> JpfObject? {
        guard let number = environment.peek?.number else {return "「\(op.literal)」" + numerationParamError3 + negateUsage}
        let ident = environment.peek!.value?.name ?? ""
        environment.drop()
        return JpfInteger(name: ident, value: -number)
    }
}
// MARK: - 判定/比較/論理演算
struct BooleanOperator : PredicateOperable {
    init(_ environment: Environment, by op: Token) {self.environment = environment; self.op = op}
    let environment: Environment, op: Token
    func operated() -> JpfObject? {
        if let params = environment.peek(3),
           params[1] is JpfRange && params[2].value is JpfRange {   // 上限と下限に分かれた範囲をマージする。
            guard let range = mergeRanges(params[1], with: params[2]) else {
                return determineUsage + "\(op.literal)。"
            }
            environment.drop(2)
            if let err = environment.push(range) {return err}
        }
        if let params = environment.peek(2) {                   // 入力が２つ
            switch (params[0].particle, params[1].particle, op.type) {
            case (.particle(.GA),.particle(.DE),.keyword(.BE)),(.particle(.WA),.particle(.DE),.keyword(.BE)),
                (.particle(.GA),.particle(.DE),.keyword(.NOT)),(.particle(.WA),.particle(.DE),.keyword(.NOT)),
                (.particle(.GA),.particle(.DEWA),.keyword(.NOT)),(.particle(.WA),.particle(.DEWA),.keyword(.NOT)),
                (_,_,.keyword(.EQUAL)):                         // 当否判定
                guard let left = params[0].value, let right = params[1].value else {
                    return "「\(op.literal)」:" + valueNotFound + ": \(params[0])または\(params[1])"
                }
                environment.drop(2)
                return determined(left, op.type, right)
            case (.particle(.GA),.particle(.NI),.keyword(.BE)),(.particle(.WA),.particle(.NI),.keyword(.BE)),
                (.particle(.GA),.particle(.NI),.keyword(.NOT)),(.particle(.WA),.particle(.NI),.keyword(.NOT)): // 有無判定(含む)
                guard let left = params[0].value, let right = params[1].value else {return determineUsage + "\(op.literal)。"}
                switch right {
                case is JpfRange:
                    guard left.isNumber else {return rangeFormatError + rangeCheckUsage  + "\(op.literal)。"}
                case is JpfArray:
                    break
                default:
                    return determineUsage + "\(op.literal)。"
                }
                environment.drop(2)
                return determined(left, op.type, right)
            case (_,_,.keyword(.BE)),(_,_,.keyword(.NOT)):      // 当否判定
                environment.drop()
                return determined(params[1], op.type)
            default:
                return "「\(op.literal)」:" + particleError + (op.type == .keyword(.BE) ? beUsage : notUsage)
            }
        } else
        if let operand = environment.peek {                     // 入力が１つ
            guard op.type != .keyword(.EQUAL) else {return "「\(op.literal)」" + twoParamsNeeded + equalUsage}
            environment.drop()
            return determined(operand, op.type)                 // 当否判定
        }
        return "「\(op.literal)」" + atLeastOneParamError + (op.type == .keyword(.BE) ? beUsage : notUsage)
    }
    private func determined(_ left: JpfObject, _ opType: Token.TokenType, _ right: JpfObject) -> JpfObject {
        if !(left is JpfArray && opType == .keyword(.EQUAL)) &&
            !(left is JpfRange && opType == .keyword(.EQUAL)) {
            if let array = right as? JpfArray {
                let result = array.contains(left)
                return determined(result, opType)
            }
            if let range = right as? JpfRange {
                let result = range.contains(left)
                guard !result.isError else {return result} // 範囲の形式エラー
                return determined(result, opType)
            }
        }
        switch opType {
        case .keyword(.BE),
             .keyword(.EQUAL):  return JpfBoolean.object(of: left.isEqual(to: right))
        case .keyword(.NOT):    return JpfBoolean.object(of: !left.isEqual(to: right))
        default:                return determineError
        }
    }
    private func determined(_ operand: JpfObject, _ opType: Token.TokenType) -> JpfObject {
        switch opType {
        case .keyword(.BE),
             .keyword(.EQUAL):  return JpfBoolean.object(of: operand.isTrue)
        case .keyword(.NOT):    return JpfBoolean.object(of: !operand.isTrue)
        default:                return determineError
        }
    }
    private func mergeRanges(_ o1: JpfObject, with o2: JpfObject) -> JpfPhrase? {
        guard let r = o1 as? JpfRange, let lower = r.lowerBound else {return nil}
        guard let p = o2 as? JpfPhrase,
              let r = p.value as? JpfRange, let upper = r.upperBound else {return nil}
        return JpfPhrase(value: JpfRange(lowerBound: lower, upperBound: upper), particle: p.particle)
    }
}
struct CompareOperator : PredicateOperable {
    init(_ environment: Environment, by op: Token) {self.environment = environment; self.op = op}
    let environment: Environment, op: Token
    func operated() -> JpfObject? {
        guard let params = environment.peek(2),
              let left = params[0].number, let right = params[1].number else {return "「\(op.literal)」" + numerationParamError1}
        guard let result = compared(left, op.type, right) else {return "「\(left)」と「\(right)」は、「\(op.literal)」" + cannotCompare}
        environment.drop(2)
        return JpfBoolean.object(of: result)
    }
    private func compared(_ left: Int, _ opType: Token.TokenType, _ right: Int) -> Bool? {
        switch opType {
        case .keyword(.LT):         return left < right
        case .keyword(.GT):         return left > right
        default:
            return nil
        }
    }
}
// MARK: - 関数実行/返す
struct ExecuteOperator : PredicateOperable {
    init(_ environment: Environment) {self.environment = environment}
    let environment: Environment
    /// 入力が、関数であれば実行する。
    /// 入力が、文字列であれば、Documentディレクトリにあるファイルを解析・評価する。
    /// - Returns: 実行結果(またはエラー)を返す。
    func operated() -> JpfObject? {
        switch environment.unwrappedPeek {
        case let function as JpfFunction:   // 関数を実行する
            environment.drop()
            return function.executed(with: environment)
        case let filename as JpfString:     // ファイルを実行(解析・評価)する
            if filename.name == Token.Keyword.FILE.rawValue {   // ファイル「ファイル名」
                environment.remove(name: filename.name)
            }
            guard let contents = try? String(contentsOfFile: directoryPath + filename.value, encoding: .utf8) else {return fileReadError}
            environment.drop()
            return executed(by: contents)
        default:
            return functionObjectNotFound
        }
    }
    /// contentsに書かれたプログラムを実行し、結果を返す。
    private func executed(by contents: String) -> JpfObject? {
        let lexer = Lexer(contents)
        let parser = Parser(lexer)
        guard let program = parser.parseProgram(), parser.errors.isEmpty else {
            print(contents)
            parser.errors.forEach {print("\t\($0)")}
            return detectParserError
        }
        return program.evaluated(with: environment)
    }
}
struct PerformOperator : PredicateOperable {
    init(_ environment: Environment) {self.environment = environment}
    let environment: Environment
    /// 入力が、関数であれば実行する。そうでない場合は、値を取り出し返す。
    /// 例： 10を負数にする → (-10)
    /// - Returns: 実行結果、もしくは取り出した入力の値
    func operated() -> JpfObject? {
        switch environment.unwrappedPeek {
        case let function as JpfFunction:
            environment.drop()
            return function.executed(with: environment)
        case let value?:
            environment.drop()
            return value
        default:
            return nil
        }
    }
}
struct ReturnOperator : PredicateOperable {
    init(_ environment: Environment) {self.environment = environment}
    let environment: Environment
    /// 返す：入力の値をラップしたオブジェクトを返す。
    /// - Returns: オブジェクト(返り値)。返す値がない場合は、エラー
    func operated() -> JpfObject? {
        guard isPeekParticle(.WO) || isPeekParticle(nil) else {return returnValueUsage}
        guard var value = environment.unwrappedPeek else {return returnParamError}
        environment.drop()
        value.name = ""
        return JpfReturnValue(value: value)
    }
}
struct GobackOperator : PredicateOperable {
    init(_ environment: Environment) {self.environment = environment}
    let environment: Environment
    /// 返る：nilをラップしたオブジェクトを返す。
    /// - Returns: オブジェクト(返り値)
    func operated() -> JpfObject? {
        return JpfReturnValue(value: nil)
    }
}
struct LoopControlOperator : PredicateOperable {
    init(_ environment: Environment, by token: Token) {self.environment = environment; self.op = token}
    let environment: Environment, op: Token
    /// 反復制御のオブジェクトを返す。
    func operated() -> JpfObject? {
        JpfLoopControl(method: op.isKeyword(.BREAK) ? .BREAK : .CONTINUE)}
}
struct CreateOperator : PredicateOperable {
    init(_ environment: Environment) {self.environment = environment}
    let environment: Environment
    /// 1. インスタンス(オブジェクト)を生成する。
    /// 2. 値から列挙子を生成
    /// 形式：
    /// 1.  (<引数>で)<型>から生成する。(生成したオブジェクトを返す)
    /// 2.「<識別子>」を(<引数>で)<型>から生成する。(nilを返す)
    /// 3.<型>から(<引数>で)「<識別子>」を生成する。(nilを返す)
    /// - Returns: インスタンス・列挙子、またはnil
    func operated() -> JpfObject? {
        if environment.isPeekParticle(.DE) {// <型>から<引数>で → <引数>で<型>から
            guard let phrase = environment.pull(where: {$0.isParticle(.KARA)}) else {return createUsage}
            if let err = environment.push(phrase) {return err}
        }
        guard environment.isPeekParticle(.KARA) || environment.isPeekParticle(.WO) else {return createUsage}
        switch environment.unwrappedPeek {
        case let type as JpfType:           // 型からインスタンスを生成
            environment.drop()
            switch type.create(with: environment) {
            case let instance as JpfInstance:
                let ident = getIdentifier() // 識別子を抽出
                if let result = instance.initialize(with: environment),
                   result.isError {return result}
                if let i = ident {          // 識別子あり？
                    environment[i.value] = instance // 生成したインスタンスを代入
                    return nil
                }
                return instance
            case let error as JpfError:
                return error
            default:
                fatalError()
            }
        case let enumType as JpfEnum:       // 列挙子を値で列挙型から生成
            environment.drop()
            if let identifier = getIdentifier() {   // 識別子あり？
                environment[identifier.value] = createEnumerator(from: enumType)    // 生成した列挙子を代入
                return nil
            }
            return createEnumerator(from: enumType)
        case let identifier as JpfString:   // 識別子名(に代入)
            environment.drop()
            if let object = operated() {
                environment[identifier.value] = object                              // 生成したオブジェクトを識別子に代入
                return nil
            }
            break
        case is JpfProtocol:
            return cannotCreateFromProtocol
        default:
            break
        }
        return createUsage
    }
    /// スタックの中の「句(<識別子>」を)を探し、識別子名(JpfString)を取り出す。
    private func getIdentifier() -> JpfString? {
        guard let phrase = environment.pull(where: {
            guard let o = $0.value else {return false}
            return o.type == JpfString.type && $0.isParticle(.WO)
        }) else {
            return nil
        }
        return phrase.value as? JpfString
    }
    private func createEnumerator(from type: JpfEnum) -> JpfObject? {
        guard environment.isPeekParticle(.DE) || environment.isPeekParticle(.KARA), let object = environment.unwrappedPeek else {return createEnumeratorUsage}
        environment.drop()
        for pairs in type.environment.enumerated {
            if object.isEqual(to: pairs.value) {return JpfEnumerator(type: type.name, identifier: pairs.key, rawValue: pairs.value)}
        }
        return valueOfEnumeratorNotFound + object.string
    }
}
struct InitializeOperator : PredicateOperable {
    init(_ environment: Environment) {self.environment = environment}
    let environment: Environment
    /// インスタンス(オブジェクト)を初期化する。
    func operated() -> JpfObject? {
        guard let instance = environment.unwrappedPeek as? JpfInstance else {return cannotInitialize}
        environment.drop()
        if let result = instance.initialize(with: environment), result.isError {return result} // 初期化
        return nil
    }
}
struct AvailableOperator : PredicateOperable {
    init(_ environment: Environment) {self.environment = environment}
    let environment: Environment
    func operated() -> JpfObject? {
        var members: [JpfObject] = []   // アクセス可能なメンバー(識別子)
        repeat {
            guard let member = environment.peek?.value as? JpfString else {return availableError}
            members.append(member)
            environment.drop()
        } while isPeekParticle(.TO)
        return JpfArray(elements: members)
    }
}
// MARK: - 要素アクセス

/// 代入(assign)
/// 1. 配列、辞書に代入(引数３)
/// <配列>の位置<数値>に<値>を代入する (<値>を<配列>の位置<数値>に代入する)
/// <配列>の位置『<識別子>』に<値>を代入する (<値>を<配列>の位置『<識別子>』に代入する)
/// <辞書>のキー<キー>に<値>を代入する (<値>を<辞書>のキー<キー>に代入する)
/// <辞書>のキー『<識別子>』に<値>を代入する (<値>を<辞書>のキー『<識別子>』に代入する)
/// 2.列挙子に代入(引数２)
/// <列挙子>に<値>を代入 (<値>を<列挙子>に代入)
/// 3. 識別子に代入(引数２)
/// <識別子>に<値>を代入 (<値>を<識別子>に代入)
/// 4.識別子に計算して代入(引数１)
/// <識別子>(に)<計算し>て代入
struct AssignOperator : PredicateOperable {
    init(_ environment: Environment) {self.environment = environment}
    let environment: Environment
    func operated() -> JpfObject? {
        if var params = environment.peek(3) {       // 配列または辞書に値を代入
            switch (params[0].particle, params[1].particle, params[2].particle) {
            case (Token(.NO),Token(.NI),Token(.WO)):
                params.swapAt(0, 1)
                params.swapAt(0, 2)
                fallthrough
            case (Token(.WO),Token(.NO),Token(.NI)):
                guard let value = params[0].value,
                      let object = params[1].value else {break}
                let result = object.assign(value, to: params[2].value)
                guard !result.isError else {return result}
                environment.drop(3)
                guard let name = params[1].value?.name,     // 代入対象の識別子を得る。
                      !name.isEmpty else {return result}    // 代入した結果を返す。
                return environment.assign(result, with: name)// 結果をさらに識別子に代入する。
            default:
                break
            }
        }
        if var params = environment.peek(2) {
            switch (params[0].particle, params[1].particle) {
            case (Token(.NI),Token(.WO)):
                params.swapAt(0, 1)
                fallthrough
            case (Token(.WO),Token(.NI)), (nil,Token(.NI)):
                guard let value = params[0].value else {break}
                if let enumerator = params[1].value as? JpfEnumerator { // 列挙子に代入
                    environment.drop(2)
                    return JpfEnumerator(type: enumerator.type, name: enumerator.name, identifier: enumerator.identifier, rawValue: value)  // 列挙子に値を代入し返す。
                }
                let name = environment.getName(from: params[1])
                guard !name.isEmpty else {break}
                environment.drop(2)
                return environment.assign(value, with: params[1].value) // 識別子に値を代入
            default:
                break
            }
        }
        if let param = environment.peek {           // 計算して代入
            guard param.value?.name != "" else {return JpfError("代入先の識別子が空(「」)。") + compoundAssignUsage}
            if let value = param.value,
               param.particle?.unwrappedLiteral == Token(.TA).literal {     // 助詞「て」
                environment.drop()
                return environment.assign(value, with: value.name)  // 識別子に計算した値を代入
            } else {
                return compoundAssignUsage
            }
        }
        return assignUsage
    }
}
/// オブジェクトの要素に値を設定する。
/// <値>(を)<オブジェクト>の要素「<識別子>」に設定する。
/// <オブジェクト>の要素「<識別子>」に<値>を設定する。
/// <オブジェクト>の要素「<識別子>」を<値>に設定する。
struct SetOperator : PredicateOperable {
    init(_ environment: Environment, by op: Token) {self.environment = environment; self.op = op}
    let environment: Environment, op: Token
    func operated() -> JpfObject? {
        if var params = environment.peek(3) {
            switch (params[0].particle, params[1].particle, params[2].particle) {
            case (Token(.NO),Token(.NI),Token(.WO)),    // 対象の要素「m」に値を設定
                 (Token(.NO),Token(.WO),Token(.NI)):    // 対象の要素「m」を値に設定
                params.swapAt(0, 1)
                params.swapAt(0, 2)
                fallthrough
            case (Token(.WO),Token(.NO),Token(.NI)):    // 値を、対象の要素「m」に設定
                guard let value = params[0].value, let object = params[1].value else {break}
                guard let label = params[2].value?.name,
                      label == Token.Keyword.MEMBER.rawValue else {break}
                environment.remove(name: label)         // ラベル「要素」を削除
                let result = object.assign(value, to: params[2].value)
                guard !result.isError else {return result}
                environment.drop(3)
                return nil
            default:
                break
            }
        }
        return setUsage
    }
}
struct AppendOperator : PredicateOperable {
    init(_ environment: Environment, by op: Token) {self.environment = environment; self.op = op}
    let environment: Environment, op: Token
    func operated() -> JpfObject? {
        if let operands = environment.peek(3),
           let appended = appendedDictionary(with: operands) {
            return appended
        }   // 入力が３でニ格が辞書でない場合は、以下を試す。
        guard let params = environment.peek(2) else {return "「\(op.literal) 」" + twoParamsNeeded + appendUsage}
        return appendedArrayOrDictionary(with: (params[0], params[1]))
    }
    /// 「要素を配列に追加する」または「配列(に)要素を追加する」
    /// 「辞書を辞書に追加する」または「辞書(に)辞書を追加する」(キーが重複した場合、上書きされる)
    /// - Parameter pair: ２つの入力(句または値)
    /// - Returns: 追加した配列または辞書を返す。形式が合わない場合は、使い方をエラーとして返す。
    private func appendedArrayOrDictionary(with pair: (first: JpfObject, second: JpfObject)) -> JpfObject? {
        var first = pair.first, second = pair.second
        switch (first.particle, second.particle) {
        case (.particle(.NI),.particle(.WO)),(nil,.particle(.WO)):
            swap(&first, &second)
            fallthrough
        case (.particle(.WO),.particle(.NI)),(nil,.particle(.NI)):
            switch second.value {
            case var array as JpfArray:
                environment.drop(2)
                first.value.map {array.elements.append($0)}
                return array
            case let dictionary as JpfDictionary:
                guard let tobeAdded = first.value as? JpfDictionary else {break}
                environment.drop(2)
                let pairs = dictionary.pairs.merging(tobeAdded.pairs) {(current, new) in new}
                return JpfDictionary(pairs: pairs)
            default:
                break
            }
        default:
            break
        }
        return appendUsage
    }
    /// 「キーが値を辞書に追加する」または「辞書(に)キーが値を追加する」(キーが重複した場合、上書きされる)
    /// - Parameter operands: ３つの入力(句または値)
    /// - Returns: 追加した辞書を返す。形式が合わない場合は、使い方をエラーとして返す。
    ///            ただし、ニ格の句が辞書でない場合は、nilを返す。
    private func appendedDictionary(with operands: [JpfObject]) -> JpfObject? {
        var particles: [Token.Particle: JpfObject?] = [.GA: operands[0].value, .WO: operands[1].value, .NI: operands[2].value]
        switch (operands[0].particle, operands[1].particle, operands[2].particle) {
        case (.particle(.NI),.particle(.GA),.particle(.WO)),(nil,.particle(.GA),.particle(.WO)):
            particles[.NI] = operands[0].value; particles[.GA] = operands[1].value; particles[.WO] = operands[2].value
            fallthrough
        case (.particle(.GA),.particle(.WO),.particle(.NI)),(.particle(.GA),nil,.particle(.NI)):
            guard var dictioncary = particles[.NI] as? JpfDictionary else {return nil}
            guard let key = particles[.GA] as? JpfObject, let value = particles[.WO] as? JpfObject else {break}
            environment.drop(3)
            dictioncary[key] = value
            return dictioncary
        default:
            return nil
        }
        return appendDictionaryUsage
    }
}
struct RemoveOperator : PredicateOperable {
    init(_ environment: Environment, by op: Token) {self.environment = environment; self.op = op}
    let environment: Environment, op: Token
    func operated() -> JpfObject? {
        if var params = environment.peek(2) {
            switch (params[0].particle, params[1].particle) {
            case (.particle(.KARA),.particle(.WO)), (nil,.particle(.WO)):
                params.swapAt(0, 1)
                fallthrough
            case (.particle(.WO),.particle(.KARA)):
                guard let object = params[1].value else {return removeUsage}
                let result = object.remove(params[0])
                guard !result.isError else {return result}
                environment.drop(2)
                return result
            default:
                break
            }
        }
        if isPeekParticle(.WO), let object = environment.peek?.value, !object.name.isEmpty {
            environment.drop()
            environment[object.name] = nil          // 対象を辞書から消す
            return nil
        }
        return removeUsage
    }
}
struct ContainsOperator : PredicateOperable {
    init(_ environment: Environment, by op: Token) {self.environment = environment; self.op = op}
    let environment: Environment, op: Token
    func operated() -> JpfObject? {
        guard let params = environment.peek(2),
              (params[0].particle == .particle(.GA) || params[0].particle == nil),
              params[1].particle == .particle(.WO) else {return "「\(op.literal) 」" + twoParamsNeeded + containsUsage}
        guard let left = params[0].value, let right = params[1].value else {return containsUsage}
        environment.drop(2)
        if let function = right as? JpfFunction {
            return left.contains(where: function, with: environment)
        }
        return left.contains(right)
    }
}
struct ForeachOperator : PredicateOperable {
    init(_ environment: Environment, by op: Token) {self.environment = environment; self.op = op}
    let environment: Environment, op: Token
    func operated() -> JpfObject? {
        guard let params = environment.peek(2),
              (params[0].particle == .particle(.DE) || params[0].particle == nil),
              params[1].particle == .particle(.WO) else {return "「\(op.literal) 」" + twoParamsNeeded + foreachUsage}
        guard let left = params[0].value, let right = params[1].value as? JpfFunction else {return foreachUsage}
        environment.drop(2)
        return left.foreach(right, with: environment)
    }
}
struct MapOperator : PredicateOperable {
    init(_ environment: Environment, by op: Token) {self.environment = environment; self.op = op}
    let environment: Environment, op: Token
    func operated() -> JpfObject? {
        if let params = environment.peek(2),
           (params[0].particle == .particle(.WO) || params[0].particle == nil),
           params[1].particle == .particle(.DE) {
            guard let left = params[0].value, let function = params[1].value as? JpfFunction else {return mapUsage}
            environment.drop(2)
            return left.map(function, with: environment)
        } else
        if let range = environment.unwrappedPeek as? JpfRange { // 範囲を配列に変換
            environment.drop()
            return range.map()
        }
        return "「\(op.literal) 」" + atLeastOneParamError + mapUsage
    }
}
struct FilterOperator : PredicateOperable {
    init(_ environment: Environment, by op: Token) {self.environment = environment; self.op = op}
    let environment: Environment, op: Token
    func operated() -> JpfObject? {
        guard let params = environment.peek(2),
              (params[0].particle == .particle(.WO) || params[0].particle == nil),
              params[1].particle == .particle(.DE) else {return "「\(op.literal) 」" + twoParamsNeeded + filterUsage}
        guard let left = params[0].value, let right = params[1].value as? JpfFunction else {return filterUsage}
        environment.drop(2)
        return left.filter(right, with: environment)
    }
}
struct ReduceOperator : PredicateOperable {
    init(_ environment: Environment, by op: Token) {self.environment = environment; self.op = op}
    let environment: Environment, op: Token
    func operated() -> JpfObject? {
        guard let params = environment.peek(3),
              (params[0].particle == .particle(.WO) || params[0].particle == nil),
              params[1].particle == .particle(.TO),
              params[2].particle == .particle(.DE) else {return "「\(op.literal) 」" + twoParamsNeeded + reduceUsage}
        guard let left = params[0].value, let initial = params[1].value, let right = params[2].value as? JpfFunction else {return reduceUsage}
        environment.drop(3)
        return left.reduce(initial, right, with: environment)
    }
}
struct SortOperator : PredicateOperable {
    init(_ environment: Environment, by op: Token) {self.environment = environment; self.op = op}
    let environment: Environment, op: Token
    func operated() -> JpfObject? {
        if let params = environment.peek(2),
            (params[0].particle == .particle(.WO) || params[0].particle == nil),
            let left = params[0].value {
            if params[1].particle == .particle(.DE), let right = params[1].value as? JpfFunction {
                environment.drop(2)
                return left.sorted(by: right, with: environment)
            } else
            if params[1].particle == .particle(.NI), let right = params[1].value as? JpfString {
                environment.drop(2)
                return left.sorted(by: right)
            } else {
                return sortUsage
            }
        } else
        if let param = environment.peek,
           (param.particle == .particle(.WO) || param.particle == nil) {
            guard let left = param.value else {return sortUsage}
            environment.drop()
            return left.sorted()
        }
        return sortUsage
    }
}
struct ReverseOperator : PredicateOperable {
    init(_ environment: Environment, by op: Token) {self.environment = environment; self.op = op}
    let environment: Environment, op: Token
    func operated() -> JpfObject? {
        guard let param = environment.peek,
              (param.particle == .particle(.WO) || param.particle == nil),
              let left = param.value else {return reverseUsage}
        environment.drop()
        return left.reversed()
    }
}

// MARK: - 補助演算
/// <式>たもの → <式>
/// 「もの」(Keyword(.MONO))は、入力から値(式)を取り出す。
/// 取り出された値は、続く助詞を付けた句としてスタックに返される。
/// 例： 10を5で割ったものに　→ (2に)、2を二倍したものを → (4を)
struct UnwrapOperator : PredicateOperable {
    init(_ environment: Environment, by op: Token) {self.environment = environment; self.op = op}
    let environment: Environment, op: Token
    func operated() -> JpfObject? {
        guard let object = environment.unwrappedPeek else {return "「\(op.literal)」" + oneParamNeeded}
        environment.drop()
        return object
    }
}
struct NullOperator : PredicateOperable {
    init(_ environment: Environment) {self.environment = environment}
    let environment: Environment
    func operated() -> JpfObject? {JpfNull.object}
}
struct NopOperator : PredicateOperable {
    init(_ environment: Environment) {self.environment = environment}
    let environment: Environment
    func operated() -> JpfObject? {nil}
}
// MARK: - スタック(入力)操作
struct StackOperator : PredicateOperable {
    init(_ environment: Environment) {self.environment = environment}
    let environment: Environment
    func operated() -> JpfObject? {JpfInput(stack: environment.getAll())}
}
struct PushOperator : PredicateOperable {
    init(_ environment: Environment, by op: Token) {self.environment = environment; self.op = op}
    let environment: Environment, op: Token
    func operated() -> JpfObject? {
        if isPeekParticle(.WO), let value = environment.unwrappedPeek {
            environment.drop()
            if let err = environment.push(value) {return err}
        }
        return nil
    }
}
struct PullOperator : PredicateOperable {
    init(_ environment: Environment, by op: Token) {self.environment = environment; self.op = op}
    let environment: Environment, op: Token
    func operated() -> JpfObject? {
        var method: String?, number = 1, identifiers: [String] = []
        if isPeekParticle(.KO) && isPeekNumber {    // n個写す(得る)
            number = leftNumber!
        }
        if isPeekParticle(.WO), let string = environment.unwrappedPeek as? JpfString, isMethod(string.value) {
            // 取得方法(「値」をor「数値」を
            environment.drop()
            method = string.value
        }
        if isPeekParticle(.NI) {                    // 複写or移動先の識別子
            repeat {
                let identifier = environment.getName()
                guard !identifier.isEmpty else {return pullDupUsage + op.literal + "。"}
                environment.drop()
                identifiers.insert(identifier, at: 0)
            } while environment.isPeekParticle(.TO)
        }
        if identifiers.count == 1 && number > 1 {   // 「<識別子>」にn個
            environment[identifiers.first!] = getObjects(from: environment, numberOf: number, by: method)
            return nil
        }
        if !identifiers.isEmpty {                   // (「<識別子>」と…)「<識別子>」に (n個は無視)
            guard let array = getObjects(from: environment, numberOf: identifiers.count, by: method) as? JpfArray else {return JpfNull.object}
            zip(identifiers, array.elements).forEach {environment[$0] = $1}
            return nil
        }
        if number > 1 {                             // n個 → 配列
            return getObjects(from: environment, numberOf: number, by: method)
        }
        defer {if op.isKeyword(.PULL) {environment.drop()}}
        return environment.peek.map {getObject(from: $0, by: method)} ?? JpfNull.object
    }
    private func getObjects(from environment: Environment, numberOf: Int, by method: String?) -> JpfObject {
        guard let values = environment.peek(numberOf) else {return JpfNull.object}
        if op.isKeyword(.PULL) {environment.drop(numberOf)}
        guard check(values, by: method) else {return JpfNull.object}
        return JpfArray(elements: values.map {getObject(from: $0, by: method)})
    }
    private func getObject(from object: JpfObject, by method: String?) -> JpfObject {
        switch method {
        case "値":   return object.value ?? JpfNull.object
        case "数値":  return object.number.map {JpfInteger(value: $0)} ?? JpfNull.object
        default:    return object
        }
    }
    private func check(_ objects: [JpfObject], by method: String?) -> Bool {
        switch method {
        case "値":   return !objects.contains(where: {!$0.hasValue})
        case "数値":  return !objects.contains(where: {!$0.isNumber})
        default:    return true
        }
    }
    private func isMethod(_ s: String?) -> Bool {
        return s == "値" || s == "数値"
    }
}
struct DropOperator : PredicateOperable {
   init(_ environment: Environment) {self.environment = environment}
   let environment: Environment
    func operated() -> JpfObject? {
        if environment.isPeekParticle(.KO), let number = leftNumber {  // n個捨てる
            environment.drop(number)
        } else {
            environment.drop()                      // 捨てる
        }
        return nil
   }
}
struct EmptyOperator : PredicateOperable {
    init(_ environment: Environment, by op: Token) {self.environment = environment; self.op = op}
    let environment: Environment, op: Token
    func operated() -> JpfObject? {
        if isPeekParticle(.GA), let value = unwrappedValue {
            return value.isEmpty                    // <値>が空？
        }
        environment.empty()                         // 入力を空にする
        return nil
    }
}
struct SwapOperator : PredicateOperable {
    init(_ environment: Environment, by op: Token) {self.environment = environment; self.op = op}
    let environment: Environment, op: Token
    func operated() -> JpfObject? {
        guard let params = environment.peek(2) else {return swapUsage}
        switch (params[0].particle, params[1].particle) {
        case (Token(.TO),Token(.WO)),(Token(.WO),Token(.TO)):
            let name1 = environment.getName(from: params[0]), name2 = environment.getName(from: params[1])
            guard environment[name1] != nil && environment[name2] != nil else {fallthrough}
            environment.drop(2)
            swap(&environment[name1]!.name, &environment[name2]!.name)
            swap(&environment[name1], &environment[name2])
            return nil
        case (nil,nil):
            environment.swap()
            return nil
        default:
            return swapUsage
        }
    }
}
