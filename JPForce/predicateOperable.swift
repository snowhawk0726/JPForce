//
//  predicateOperable.swift
//  日本語ふぉーす(JPForce)
//
//  Created by 佐藤貴之 on 2023/03/06.
//
import Foundation
import AVFoundation

// MARK: - intefaces for operation
protocol PredicateOperable {
    var environment: Environment {get}
    func operated() -> JpfObject?
}
// MARK: - predicate operable instance factory
struct PredicateOperableFactory {
    static func create(from token: Token, with environment: Environment) -> PredicateOperable? {
        switch token.type {
        case .keyword(.ADD):        return AddOperator(environment, by: token)
        case .keyword(.MULTIPLY):   return MultiplyOperator(environment, by: token)
        case .keyword(.SUBSTRACT):  return SubstractOperator(environment, by: token)
        case .keyword(.DIVIDE):     return DivideOperator(environment, by: token)
        case .keyword(.NEGATE):     return NegateOperator(environment, by: token)
        case .keyword(.POSITIVE),.keyword(.NEGATIVE):
                                    return SignOperator(environment, by: token)
        case .keyword(.RETURN):     return ReturnOperator(environment, by: token)
        case .keyword(.BREAK):      return BreakOperator(environment, by: token)
        case .keyword(.MONO):       return UnwrapOperator(environment, by: token)  // 〜たもの
        case .keyword(.NULL):       return NullOperator(environment)
        case .keyword(.INPUT):      return StackOperator(environment)
        case .keyword(.DROP):       return DropOperator(environment)
        case .keyword(.EMPTY):      return EmptyOperator(environment, by: token)
        case .keyword(.DUPLICATE),.keyword(.PULL):
                                    return PullOperator(environment, by: token)
        case .keyword(.ASSIGN),.keyword(.OVERWRITE):
                                    return AssignOperator(environment, by: token)
        case .keyword(.SWAP):       return SwapOperator(environment, by: token)
        case .keyword(.IDENTIFIERS):
                                    return IdentifiersOperator(environment, by: token)
        case .keyword(.EXECUTE):    return ExecuteOperator(environment) // (関数)を実行
        case .keyword(.GENERATE):   return GenerateOperator(environment)// (型)から生成
        case .keyword(.SURU):       return PerformOperator(environment) // 〜にする、〜をする
        case .keyword(.APPEND):     return AppendOperator(environment, by: token)
        case .keyword(.REMOVE):     return RemoveOperator(environment, by: token)
        case .keyword(.CONTAINS):   return ContainsOperator(environment, by: token)
        case .keyword(.FOREACH):    return ForeachOperator(environment, by: token)
        case .keyword(.MAP):        return MapOperator(environment, by: token)
        case .keyword(.FILTER):     return FilterOperator(environment, by: token)
        case .keyword(.REDUCE):     return ReduceOperator(environment, by: token)
        case .keyword(.SORT):       return SortOperator(environment, by: token)
        case .keyword(.REVERSE):    return ReverseOperator(environment, by: token)
        case .keyword(.PRINT):      return PrintOperator(environment, by: token)
        case .keyword(.NEWLINE):    return NewlineOperator(environment)
        case .keyword(.READ):       return ReadOperator(environment, by: token)
        case .keyword(.FILES):      return FilesOperator(environment, by: token)
        case .keyword(.EQUAL),.keyword(.BE),.keyword(.NOT):
                                    return BooleanOperator(environment, by: token)
        case .keyword(.LT),.keyword(.GT):
                                    return CompareOperator(environment, by: token)
        case .keyword(.KOTO):       return NopOperator(environment)
        case .keyword(_):           return NopOperator(environment)
        default:                    return nil
        }
    }
}
struct Splitter {
    init(of string: String, with environment: Environment, terminator: String = "\\末尾") {self.string = string; self.environment = environment; self.terminator = terminator}
    let string: String
    let environment: Environment
    let terminator: String
    var error: JpfError?
    /// \と括弧で囲われた識別子の中身を文字列の配列に分割する。
    let separators: [(String, Character)] = [("\\『", "』"), ("\\（", "）")]
    mutating func split() -> [String]? {
        var strings: [String] = []
        guard let splitted = split(string, with: separators[0]) else {return nil}
        for string in splitted {
            guard let splitted = split(string, with: separators[1]) else {return nil}
            strings += splitted
        }
        return strings
    }
    mutating private func split(_ s: String, with parentheses: (String, Character)) -> [String]? {
        let beginOfIdent = parentheses.0, endOfIdent = parentheses.1
        var splitted: [String] = []
        if let range = s.firstRange(of: beginOfIdent) {
            guard let i = s[range.upperBound..<s.endIndex].firstIndex(of: endOfIdent) else {
                error = JpfError("識別子を囲う、閉じカッコ「\(endOfIdent)」が見つからない。")
                return nil
            }
            splitted.append(String(s[s.startIndex..<range.lowerBound]) + terminator)// 識別子の前の部分を切り出す
            let name = String(s[range.upperBound..<i])
            guard let object = environment[name] else {                             // 識別子を切り出す
                error = JpfError("『\(name)』(識別子)が定義されていない。")
                return nil
            }
            splitted.append(object.string + terminator)
            let s = String(s[s.index(after: i)..<s.endIndex])
            guard let strings = split(s, with: (beginOfIdent, endOfIdent)) else {return nil}   // 残りを分割し、配列に追加
            splitted += strings
        } else {
            splitted = [s]
        }
        return splitted
    }
}
// MARK: - implements
extension PredicateOperable {
    // Stack operations
    var isPeekNumber: Bool {environment.peek?.isNumber ?? false}
    func isPeekParticle(_ expected: Token.Particle) -> Bool {environment.peek?.particle == .particle(expected)}
    var leftOperand: JpfObject? {return environment.pull()}
    var leftNumber: Int? {
        guard let number = environment.peek?.number else {return nil}
        environment.drop()
        return number
    }
    // エラー
    var numerationParamError1: JpfError {JpfError("には２つの数値入力が必要。")}
    var numerationParamError2: JpfError {JpfError("には２つ以上の数値入力が必要。")}
    var numerationParamError3: JpfError {JpfError("には１つの数値入力が必要。")}
    var atLeastOneParamError: JpfError  {JpfError("には１つ以上の入力が必要。")}
    var oneParamNeeded: JpfError        {JpfError("には１つの入力が必要。")}
    var twoParamsNeeded: JpfError       {JpfError("には２つの入力が必要。")}
    var additionParamError: JpfError    {JpfError("には、２つ以上の数値、文字列、配列の入力が必要。")}
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
    var detectParserError: JpfError     {JpfError("構文解析器がエラーを検出した。")}
    // メッセージ(使い方)
    var printUsage: JpfError            {JpfError("仕様：(〜と…)〜を表示する。")}
    var readUsage: JpfError             {JpfError("仕様：(〜と…)〜を読む。")}
    var additionUsage: JpfError         {JpfError("仕様：(〜と…)〜を足す。")}
    var multiplicationUsage: JpfError   {JpfError("仕様：(〜と…)〜を掛ける。")}
    var substractionUsage: JpfError     {JpfError("仕様：(〜から)〜を引く。または、(〜を)〜から引く。")}
    var divisionUsage: JpfError         {JpfError("仕様：(〜を)〜で割る。または、(〜で)〜を割る。")}
    var negateUsage: JpfError           {JpfError("仕様：〜の負数。または、〜を負数にする。")}
    var equalUsage: JpfError            {JpfError("仕様：〜(と)〜(が)等しい。")}
    var beUsage: JpfError               {JpfError("仕様：(〜が)〜である。または、(〜は)〜である。")}
    var notUsage: JpfError              {JpfError("仕様：(〜が)〜で(は)ない。または、(〜は)〜で(は)ない。")}
    var appendUsage: JpfError           {JpfError("仕様：〜(を)〜に追加する。または、〜(に)〜を追加する。")}
    var appendDictionaryUsage: JpfError {JpfError("仕様：〜が〜(を)〜に追加する。または、〜(に)〜が〜を追加する。")}
    var removeUsage: JpfError           {JpfError("仕様：(〜から)〜を削除する。")}
    var rangeCheckUsage: JpfError       {JpfError("仕様：<数値>が範囲【<範囲式>】に")}
    var determineUsage: JpfError        {JpfError("仕様：〜が<配列、範囲>に")}
    var containsUsage: JpfError         {JpfError("仕様：<配列、辞書、範囲>が<要素>を含む。")}
    var foreachUsage: JpfError          {JpfError("仕様：<配列、辞書、範囲>を<関数>で繰り返す。")}
    var mapUsage: JpfError              {JpfError("仕様：<配列、辞書、範囲>を<関数>で写像する。または、<範囲>写像する。")}
    var filterUsage: JpfError           {JpfError("仕様：<配列、辞書>を<関数>でフィルターする。")}
    var reduceUsage: JpfError           {JpfError("仕様：<配列、辞書、範囲>を<初期値>と<関数>でまとめる。")}
    var sortUsage: JpfError             {JpfError("仕様：<配列>を<関数>で並び替える。または、<配列>を（「昇順」に、または「降順」に）並び替える。")}
    var reverseUsage: JpfError          {JpfError("仕様：<配列、文字列>を逆順にする。")}
    var pullDupUsage: JpfError             {JpfError("仕様：(「<識別子>」と…)(「<識別子>」に）(「数値」または「値」を)(<数値>個)")}
    var assignUsage: JpfError           {JpfError("仕様：〜(を)「<識別子>」に代入する。または、「<識別子>」に〜を代入する。")}
    var overwriteUsage: JpfError           {JpfError("仕様：〜(を)<識別子>に上書きする。または、<識別子>に〜を上書きする。")}
    var swapUsage: JpfError             {JpfError("仕様：「<識別子>」と「<識別子>」を入れ替える。または、〜と〜を入れ替える。")}
    var generateUsage: JpfError         {JpfError("仕様：<識別子(型)>から生成する。")}
}
// MARK: - 表示/音声
struct PrintOperator : PredicateOperable {
    init(_ environment: Environment, by op: Token) {self.environment = environment; self.op = op}
    let environment: Environment, op: Token
    func operated() -> JpfObject? {
        guard let object = environment.unwrappedPeek else {return "「\(op.literal)」" + atLeastOneParamError + printUsage}
        var objects: [JpfObject] = [object]
        environment.drop()
        while isPeekParticle(.TO) {             // スタックにト格があれば、中身を格納
            objects.append(leftOperand!.value!)
        }
        for object in objects.reversed() {
            let result = printWithTerminator(object.string)
            if result.isError {return result}
        }
        return nil
    }
    /// 文字列の中のエスケープ文字を解析して表示(print)する。
    /// (stringが識別子として登録されている場合はエスケープせずに登録されたオブジェクトを表示する。)
    /// - Parameter string: 制御文字を含む文字列
    /// 「\末尾」は、文字列の末尾を改行の代わりに表示する。
    /// 例： 「こんにちは\末尾、」と「みなさん。」を表示する。
    ///     → こんにちは、みなさん。
    private func printWithTerminator(_ string: String) -> JpfObject {
        switch string {
        case Token.Keyword.IDENTIFIER.rawValue: // 識別子をエスケープ文字制御なしで表示する。
            guard let identifier = environment[string] as? JpfString else {break}
            guard let object = environment[identifier.value] else {return JpfError("『\(identifier.value)』(識別子)が定義されていない。")}
            print(object.string)
            environment[string] = nil
            return JpfBoolean.TRUE
        case Token.Keyword.FILE.rawValue:       // ファイルの内容をエスケープ文字制御なしで表示する。
            guard let filename = environment[string] as? JpfString else {break}
            let url = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask)[0]
            guard let contents = try? String(contentsOfFile: url.path() + filename.value, encoding: .utf8) else {return fileReadError}
            print(contents)
            environment[string] = nil
            return JpfBoolean.TRUE
        default:
            break
        }                                       // エスケープ文字制御した結果を表示する。
        var splitter = Splitter(of: replaced(string), with: environment)
        guard let strings = splitter.split() else {return splitter.error!}
        strings.forEach { s in
            if let range = s.firstRange(of: "\\末尾") {
                let terminator = String(s[range.upperBound..<s.endIndex])   // 「末尾」の後の文字列
                print(s[s.startIndex..<range.lowerBound], terminator: terminator)
            } else {
                print(s)
            }
        }
        return JpfBoolean.TRUE
    }
    /// エスケープ文字を(Swiftの)制御コードに変換する。
    /// 「\改行なし」が文字列の後尾にある場合、改行をせずに表示する。
    /// 　(\は、そのまま使えるが、Swiftに合わせた。(「"」とか「'」は合わせてない))
    private func replaced(_ string: String) -> String {
        let codes = [("\\t","\t"),("\\n","\n"),("\\r","\r"),("\\0","\0"),("\\\\","\\"),("\\改行なし","\\末尾")]
        return codes.reduce(string) {$0.replacingOccurrences(of: $1.0, with: $1.1)}
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
            let result = readWithoutTerminator(object.string)
            if result.isError {return result}
        }
        return nil
    }
    private func readWithoutTerminator(_ string: String) -> JpfObject {
        switch string {
        case Token.Keyword.IDENTIFIER.rawValue: // 識別子をエスケープ文字制御なしで表示する。
            guard let identifier = environment[string] as? JpfString else {break}
            guard let object = environment[identifier.value] else {return JpfError("『\(identifier.value)』(識別子)が定義されていない。")}
            read(object.string)
            environment[string] = nil
            return JpfBoolean.TRUE
        case Token.Keyword.FILE.rawValue:       // ファイルの内容をエスケープ文字制御なしで表示する。
            guard let filename = environment[string] as? JpfString else {break}
            let url = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask)[0]
            guard let contents = try? String(contentsOfFile: url.path() + filename.value, encoding: .utf8) else {return fileReadError}
            read(contents)
            environment[string] = nil
            return JpfBoolean.TRUE
        default:
            break
        }
        var splitter = Splitter(of: string, with: environment, terminator: "")
        guard let splitted = splitter.split() else {return splitter.error!}
        splitted.forEach {read($0)}
        return JpfBoolean.TRUE
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
        let documentUrl = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask)[0]
        guard let fileNames = try? FileManager.default.contentsOfDirectory(atPath: documentUrl.path()) else {return JpfNull.object}
        return JpfArray(elements: fileNames.map {JpfString(value: $0)})
    }
}
// MARK: - 算術演算
struct AddOperator : PredicateOperable {
    init(_ environment: Environment, by token: Token) {self.environment = environment; self.op = token}
    let environment: Environment, op: Token
    func operated() -> JpfObject? {
        guard let params = environment.peek(2)  else {return "「\(op.literal)」" + additionParamError + additionUsage}
        let left = params[0]; let right = params[1]
        var added = left.add(right)
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
        var number = left * right
        environment.drop(2)
        while isPeekParticle(.TO) && isPeekNumber {     // スタックにト格の数値があれば、掛ける
            number *= leftNumber!
        }
        return JpfInteger(value: number)
    }
}
struct SubstractOperator : PredicateOperable {
    init(_ environment: Environment, by token: Token) {self.environment = environment; self.op = token}
    let environment: Environment, op: Token
    func operated() -> JpfObject? {
        guard let params = environment.peek(2),
              let left = params[0].number, let right = params[1].number else {return "「\(op.literal)」" + numerationParamError1 + substractionUsage}
        switch (params[0].particle, params[1].particle) {
        case (Token(.KARA),Token(.WO)), (nil,Token(.WO)), (nil,nil):    // leftから、rightを引く
            environment.drop(2)
            return JpfInteger(value: left - right)
        case (Token(.WO),Token(.KARA)), (nil,Token(.KARA)):             // leftを、rightから引く
            environment.drop(2)
            return JpfInteger(value: right - left)
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
        switch (params[0].particle, params[1].particle) {
        case (Token(.WO),Token(.DE)), (nil,Token(.DE)), (nil,nil):      // leftを、rightで割る
            guard right != 0 else {return "\(left)を" + cannotDivideByZero}
            environment.drop(2)
            return JpfInteger(value: left / right)
        case (Token(.DE),Token(.WO)), (nil,Token(.WO)):                 // leftで、rightを割る
            guard left != 0 else {return "\(right)を" + cannotDivideByZero}
            environment.drop(2)
            return JpfInteger(value: right / left)
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
        return JpfInteger(value: -number)
    }
}
struct SignOperator : PredicateOperable {
    init(_ environment: Environment, by op: Token) {self.environment = environment; self.op = op}
    let environment: Environment, op: Token
    func operated() -> JpfObject? {
        if let number = environment.unwrappedPeek as? JpfInteger {
            environment.drop()
            return number[op.literal]
        }
        return "「\(op.literal)」" + cannotJudgeGenuineness
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
            environment.push(range)
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
        if let array = right as? JpfArray {
            let result = array.contains(left)
            return determined(result, opType)
        }
        if let range = right as? JpfRange {
            let result = range.contains(left)
            guard !result.isError else {return result} // 範囲の形式エラー
            return determined(result, opType)
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
        case .keyword(.LT),
             .keyword(.UNDER):      return left < right
        case .keyword(.GT):         return left > right
        case .keyword(.GTEQUAL):    return left >= right
        case .keyword(.LTEQUAL):    return left <= right
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
        case let overload as JpfArray:      // 多重定義を実行する
            environment.drop()
            return overload.executed(with: environment)
        case var filename as JpfString:     // ファイルを実行(解析・評価)する
            if filename.value == Token.Keyword.FILE.rawValue,
               let object = environment[filename.value] as? JpfString {   // ファイル「ファイル名」
                environment[filename.value] = nil
                filename = object
            }
            let url = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask)[0]
            guard let contents = try? String(contentsOfFile: url.path() + filename.value, encoding: .utf8) else {return fileReadError}
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
    /// (入力の助詞「に」の付いた句から値(式)を取り出す。<式>にする → <式>)
    /// 例： 10を負数にする → (-10)
    /// - Returns: 実行結果、もしくは取り出した入力の値
    func operated() -> JpfObject? {
        if let function = environment.unwrappedPeek as? JpfFunction {
            environment.drop()
            return function.executed(with: environment)
        } else
        if let overload = environment.unwrappedPeek as? JpfArray,
           overload.elements.first is JpfFunction {
            environment.drop()
            return overload.executed(with: environment)
        }
        return environment.unwrapPhrase()
    }
}
struct ReturnOperator : PredicateOperable {
    init(_ environment: Environment, by token: Token) {self.environment = environment; self.op = token}
    let environment: Environment, op: Token
    /// 入力を拾い、ラップした値を返す
    /// - Returns: 返り値。入力が空ならエラー
    func operated() -> JpfObject? {
        guard !environment.isEmpty else {return returnParamError}
        return JpfReturnValue(value: isPeekParticle(.WO) ? environment.unwrapPhrase() : leftOperand)
    }
}
struct BreakOperator : PredicateOperable {
    init(_ environment: Environment, by token: Token) {self.environment = environment; self.op = token}
    let environment: Environment, op: Token
    /// 処理を中止する。
    func operated() -> JpfObject? {JpfReturnValue(value: nil)}
}
struct GenerateOperator : PredicateOperable {
    init(_ environment: Environment) {self.environment = environment}
    let environment: Environment
    func operated() -> JpfObject? {
        guard environment.isPeekParticle(.KARA), let type = environment.peek?.value as? JpfType else {return generateUsage}
        let local = Environment(outer: self.environment)    // 型の環境を拡張
        let stackEnv = environment.outer != nil ? environment : self.environment
        let result = local.apply(type.parameters, with: type.signature, from: stackEnv)
        guard !result.isError else {return result}
        defer {environment.push(local.pullAll())}           // スタックを戻す
        if let initial = type.initializer,                  // 初期化ブロック
           let result = Evaluator(from: initial, with: local).object {
            guard !result.isError else {return result}      // 初期化失敗
        }
        if let result = Evaluator(from: type.body, with: local).object {
            guard !result.isError else {return result}      // メンバ登録
        }
        return nil
    }
}
// MARK: - 要素アクセス
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
        guard var params = environment.peek(2) else {return "「\(op.literal) 」" + twoParamsNeeded + removeUsage}
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
              (params[0].particle == .particle(.WO) || params[0].particle == nil),
              params[1].particle == .particle(.DE) else {return "「\(op.literal) 」" + twoParamsNeeded + foreachUsage}
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
struct PullOperator : PredicateOperable {
    init(_ environment: Environment, by op: Token) {self.environment = environment; self.op = op}
    let environment: Environment, op: Token
    func operated() -> JpfObject? {
        var method: String?, number = 1, identifiers: [String] = []
        if environment.isPeekParticle(.KO), environment.peek?.isNumber ?? false {  // n個写す(得る)
            number = leftNumber!
        }
        if environment.isPeekParticle(.WO), let string = environment.peek?.value as? JpfString {    // 取得方法(「値」or「数値」
            environment.drop()
            method = string.value
        }
        if environment.isPeekParticle(.NI) {        // 複写or移動先
            repeat {
                guard let identifier = environment.peek?.value as? JpfString else {return pullDupUsage + op.literal + "。"}
                environment.drop()
                identifiers.append(identifier.value)
            } while environment.isPeekParticle(.TO)
        }
        if identifiers.count == 1 && number > 1 {   // 「<識別子>」にn個
            environment[identifiers.first!] = getObjects(from: environment, numberOf: number, by: method)
            return nil
        }
        if !identifiers.isEmpty {                   // (「<識別子>」と…)「<識別子>」に (n個は無視)
            guard let array = getObjects(from: environment, numberOf: identifiers.count, by: method) as? JpfArray else {return JpfNull.object}
            zip(identifiers.reversed(), array.elements).forEach {environment[$0] = $1}
            return nil
        }
        if number > 1 {                             // n個 → 配列
            return getObjects(from: environment, numberOf: number, by: method)
        }
        defer {if op.type == .keyword(.PULL) {environment.drop()}}
        return environment.peek.map {getObject(from: $0, by: method)} ?? JpfNull.object
    }
    private func getObjects(from environment: Environment, numberOf: Int, by method: String?) -> JpfObject {
        guard let values = environment.peek(numberOf) else {return JpfNull.object}
        if op.type == .keyword(.PULL) {environment.drop(numberOf)}
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
        if isPeekParticle(.GA), let object = leftOperand?.value {   // <値>が空？
            return object[op.literal]
        }
        if isPeekParticle(.WO) {
            switch environment.peek?.value {
            case let array as JpfArray:             // <配列>を空にする。
                environment.drop()
                return array.remove(JpfString(value: "全て"))
            case let dictionary as JpfDictionary:   // <辞書>を空にする。
                environment.drop()
                return dictionary.remove(JpfString(value: "全て"))
            default:
                break
            }
        }
        environment.empty()                         // 入力を空にする
        return nil
    }
}
struct AssignOperator : PredicateOperable {
    init(_ environment: Environment, by op: Token) {self.environment = environment; self.op = op}
    let environment: Environment, op: Token
    func operated() -> JpfObject? {
        guard var params = environment.peek(2) else {return usage}
        switch (params[0].particle, params[1].particle) {
        case (Token(.NI),Token(.WO)):
            params.swapAt(0, 1)
            fallthrough
        case (Token(.WO),Token(.NI)), (nil,Token(.NI)):
            let name = environment.getName(from: params[1])
            guard !name.isEmpty else {return usage}
            var value = params[0].value
            value?.name = name
            environment.drop(2)
            if op == .keyword(.OVERWRITE) && environment.outer?[name] != nil {
                environment.outer![name] = value
                return nil
            }
            environment[name] = value
            return nil
        default:
            return usage
        }
    }
    private var usage: JpfError {op == .keyword(.ASSIGN) ? assignUsage : overwriteUsage}
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
struct IdentifiersOperator : PredicateOperable {
    init(_ environment: Environment, by op: Token) {self.environment = environment; self.op = op}
    let environment: Environment, op: Token
    /// 識別子の一覧を配列して返す。
    /// - Returns: 識別子(JpfString)の配列
    func operated() -> JpfObject? {JpfArray(elements: environment.enumerated.map {JpfString(value: $0.key)})}
}
