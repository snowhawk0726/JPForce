//
//  environment.swift
//  日本語ふぉーす(JPForce)
//
//  Created by 佐藤貴之 on 2023/03/06.
//
import Foundation

class Environment {
    init(outer: Environment? = nil) {self.outer = outer}
    var outer: Environment?     // 拡張環境
    private var store: [String: JpfObject] = [:]
    private var stack: [JpfObject] = []
    var isExecutable: Bool = true                   // false: 実行抑止
    // MARK: - 辞書操作
    subscript(_ name: String) -> JpfObject? {
        get {store[name] ?? outer?[name]}           // 外部環境の取得は可
        set {
            var object = newValue
            object?.name = name                     // 識別子名をオブジェクトに記録
            store[name] = object
        }
    }
    var enumerated: [(key: String, value: JpfObject)] {
        store.map {(key: $0, value: $1)}
    }
    var values: [JpfObject] {Array(store.values) + (outer?.values ?? [])}
    /// storeを、JpfDictionaryに変換する。(値は、store[key].stirng)
    var stringDictionary: JpfDictionary {
        let keys = store.keys.map {JpfString(value: $0).hashKey}
        let values = store.map {(key: JpfString(value: $0), value: JpfString(value: $1.string))}
        return JpfDictionary(pairs: Dictionary(uniqueKeysWithValues: zip(keys, values)))
    }
    func contains(_ name: String) -> Bool {store.keys.contains(name)}
    /// 辞書にオブジェクトを追加する。
    ///     未登録のオブジェクトであれば、そのまま辞書に登録
    ///     登録済みのオブジェクトが配列(JpfArray)であれば、配列の要素に追加
    ///     それ以外は、登録済みオブジェクトと追加オブジェクトの配列(JpfArray)を作り、配列を辞書に登録
    /// - Parameters:
    ///   - object: 追加するオブジェクト
    ///   - name: 追加対象の識別子名
    func append(_ object: JpfObject, to name: String) {
        if contains(name) {
            if var array = self[name] as? JpfArray {
                array.elements.append(object)
            } else {
                self[name] = JpfArray(elements: [self[name]!, object])
            }
            return
        }
        self[name] = object
    }
    /// 辞書からオブジェクトを取り出す。
    /// - Parameter name: 対象の識別子名
    /// - Returns: 取り出したオブジェクト(もしくはnil)
    func retrieve(name: String) -> JpfObject? {
        var object = self[name]
        if let array = object as? JpfArray {
            object = array.elements.first
        }
        return object
    }
    /// 辞書から識別子に対応したオブジェクトを削除する。
    /// - Parameter name: 対象の識別子名
    func remove(name: String) {
        if var array = self[name] as? JpfArray {
            array.elements.removeFirst()
            if array.elements.count == 1 {
                self[name] = array.elements.first
            }
        } else {
            self[name] = nil
        }
    }
    /// インスタンスの「自身」を削除した環境を返す。
    var removedSelf: Self {
        let removed = self
        removed[JpfInstance.SELF] = nil
        return removed
    }
    // MARK: - スタック操作
    func push(_ object: JpfObject)  {stack.append(object)}
    func push(_ objects: [JpfObject])   {stack += objects}
    func pull() -> JpfObject?       {stack.popLast()}
    /// スタックに指定のオブジェクトがあったら、それを取り出す。(無ければ nil)
    func pull(where codition: (JpfObject) -> Bool) -> JpfObject? {
        guard let p = stack.lastIndex(where: { codition($0) }) else {return nil}
        return stack.remove(at: p)
    }
    func pullAll() -> [JpfObject]   {defer {empty()}; return stack}
    func getAll() -> [JpfObject]    {return stack}
    func drop()                     {_ = pull()} // removeLast()を使うと、emptyチェックが必要
    func drop(_ n: Int)             {stack.removeLast(n <= count ? n : count)}
    func empty()                    {stack.removeAll()}
    func swap()                     {if count >= 2 {stack.swapAt(count-2, count-1)}}
    //
    var isEmpty: Bool               {stack.isEmpty}
    var count: Int                  {stack.count}
    var peek: JpfObject?            {stack.last}
    /// 指定個数のオブジェクトをスタックからコピー
    /// - Parameter numberOf: 指定個数
    /// - Returns: 指定個数のオブジェクト配列を返す。
    ///            オブジェクト数(count)が指定個数に満たない場合は、nil。
    ///            numberOfが、0以下の場合は、[]。
    func peek(_ numberOf: Int) -> [JpfObject]? {
        guard numberOf <= count else {return nil}
        return Array(stack[(count - numberOf)..<count])
    }
    subscript(index: Int) -> JpfObject? {
        guard case 0..<count = index else {return nil}
        return stack[index]
    }
    var string: String              {stack.map {$0.string}.joined(separator: " ")}
    // MARK: - 入力操作
    var isPeekParticle: Bool {peek?.particle != nil}
    func isPeekParticle(_ particle: Token.Particle) -> Bool {
        peek?.particle.map {$0.type} == Token.particle(particle).type
    }
    var unwrappedPeek: JpfObject? {
        if peek is JpfPhrase {return peek?.value}
        return peek
    }
    func unwrapPhrase() -> JpfObject? {
        guard let object = peek as? JpfPhrase else {return nil}
        drop()
        return object.value
    }
    func getName(from object: JpfObject?) -> String {
        var name = object?.value?.name ?? ""
        if name.isEmpty, let string = object?.value as? JpfString {name = string.value}
        return name
    }
    func getName() -> String {return getName(from: peek)}
    /// 多重定義の関数ブロックから、引数形式が一致するものを抽出し、処理を行う。
    /// - Parameters:
    ///   - functions: 処理ブロック(【入力が〜、本体が〜。】)
    ///   - local: 関数処理を行う環境 (selfは入力環境)
    /// - Returns: 処理結果、エラー、またはnil
    func execute(_ functions: FunctionBlocks, with local: Environment) -> JpfObject? {
        for n in stride(from: count, through: -1, by: -1) {     // スタック上の引数の数毎
            for function in functions[n].reversed() {           // 引数の数に応じた多重定義毎
                let designated = function.signature
                if hasParameters(to: designated, number: n) {   // 引数が一致する入力を処理
                    if let error = local.apply(function, numberOfArguments: n, from: self) {
                        return error
                    }
                    return execute(function, with: local)
                }
            }
        }
        if functions.array.count == 1 {             // 単体の定義の場合、エラーメッセージを作成する
            return errorMessage(to: functions.array.last!)
        }
        return InputFormatError.noMatchingSignature.message
    }
    /// 多重定義の関数ブロックから、引数形式が一致するものを抽出し、実行する。
    /// - Parameters:
    ///   - functions: 対象の多重定義
    ///   - local: 関数の実行環境
    /// - Returns: 処理結果、エラー、またはnil
    func call(_ functions: FunctionBlocks, with local: Environment) -> JpfObject? {
        if let function = functions.function(with: local) {
            if let error = local.apply(function) {  // 残りの引数を割り当てる
                return error
            }
            return execute(function, with: local)
        }
        if functions.array.count == 1 {             // 単体の定義の場合、エラーメッセージを作成する
            let f = functions.array.last!           // 最新定義
            let n = local.store.count               // 指定引数の数
            guard functions.dictionary.keys.contains(n) || f.signature.numberOfInputs == nil else {
                let n = f.parameters.count - f.signature.numberOfDefaultValues
                return InputFormatError.numberOfParameters(n).message
            }
            return errorMessage(to: f, with: local)
        }
        return InputFormatError.noMatchingSignature.message
    }
    /// 関数のローカル環境で、関数を実行する。
    /// - Parameters:
    ///   - function: 実行する関数ブロック
    ///   - local: 関数のローカル環境
    /// - Returns: 返り値(無いときはnil)、またはエラー
    private func execute(_ function: FunctionBlock, with local: Environment) -> JpfObject? {
        defer {push(local.pullAll())}           // スタックを戻す
        if let body = function.body, let result = Evaluator(from: body, with: local).object {
            if result.isError {return result}
            if result.isReturnValue {return result.value}
        }
        return nil
    }
    /// スタック上の引数(オブジェクト)と指定された形式が一致するかをチェックする。
    /// - Parameters:
    ///   - signature: 指定された形式(引数の数および型と格)
    ///   - number:    引数の数(-1: 不明)
    /// - Returns: true: 一致、または引数無し、
    ///            false: 不一致、または引数が指定未満
    func hasParameters(to signature: InputFormat, number: Int) -> Bool {
        guard let arguments = number >= 0 ? peek(number) : getAll() else {
            return false                            // 引数が指定に満たない
        }
        switch number {
        case -1:                                    // 可変長引数あり
            for (format, value) in zip(signature.formats, signature.values) {
                if format.hasThreeDots {            // 可変長の入力
                    continue                        // 引数のシグネチャにかかわらず、チェックok
                } else {                            // 固定長の入力
                    if value != nil {continue}      // 既定値あり
                    if !arguments.contains(where: {isSameFormat(of: $0, as: format)}) {
                        return false                // 型または格が一致する引数が無い
                    }
                }
            }
        case 0:                                     // 引数無し(チェック不要)
            break
        default:                                    // 固定長引数
            for (argument, format) in zip(arguments, signature.formats) {
                if !isSameFormat(of: argument, as: format) {
                    return false                    // 引数と型または格が一致しない
                }
            }
        }
        return true
    }
    /// 外部のスタック上の引数(オブジェクト)の形式チェックを行い値を取り出し、各識別子に引き当て、内部(self)の辞書に登録する。
    /// - Parameters
    ///   - function: 対象の関数ブロック
    ///   - numberOfArguments: スタックから引き当てる引数の数
    ///   - outer:  引数を格納しているスタックを含む環境
    /// - Returns: 引数がパラメータおよび指定形式に合わない場合は、InputFormatError(JpfError)を返す。
    private func apply(_ function: FunctionBlock, numberOfArguments: Int, from outer: Environment) -> JpfError? {
        let designated = function.signature
        guard let arguments = numberOfArguments >= 0 ? outer.peek(numberOfArguments) : outer.getAll() else {
            return InputFormatError.noParameterValue.message
        }                                           // 引数を取得
        var a = 0                                   // 引数の位置
        if numberOfArguments < 0 {                  // 可変長の識別子に対応する位置を求める
            guard let format = designated.firstParamFormat else {   // 最初の(既定値の無い)入力形式
                return InputFormatError.notFoundFirstFormat.message
            }
            a = arguments.firstIndex {isSameFormat(of: $0, as: format)} ?? 0
        }
        for i in 0..<function.parameters.count {    // 入力の位置
            let argument = a < arguments.count ?
                            arguments[a] : nil      // 引数(無しはnil)
            let idetifier = function.parameters[i]  // 入力の識別子
            let format = designated.formats[i]      // 指定形式
            var value: JpfObject                    // 割り当てる値
            if !format.hasThreeDots {               // 固定長
                value = argument?.value ?? getDefaultValue(with: designated, at: i)
                a += 1
            } else {                                // 可変長
                let length = numberOfElements(in: [JpfObject](arguments[a..<arguments.count]), withConsecutive: format)
                let elements = [JpfObject](arguments[a..<(a + length)]).map {$0.value ?? JpfNull.object}    // 要素の値を取り出す。
                // TODO: スタック中の値にnilがあった場合、エラーにすべきか？ → ありえないので、発生したら検討する。
                value = JpfArray(elements: elements)
                a += length
            }
            if value.isError {return value.error}
            store[idetifier.value] = value          // 値の割り当て
        }
        outer.drop(a)
        return nil
    }
    /// 辞書に登録済み引数以外を、既定値で登録する。
    /// - Parameter function: 対象の関数ブロック
    /// - Returns: エラー、無しはnil
    private func apply(_ function: FunctionBlock) -> JpfError? {
        let difference = Set(function.parameters.map {$0.value}).subtracting(Set(store.keys))
        for name in difference {
            let i = function.index(of: name)!   // parametersに属するので、nilにはならない。
            store[name] = getDefaultValue(with: function.signature, at: i)
        }
        return nil
    }
    /// 指定された既定値式を評価し、値を得る
    /// - Parameters:
    ///   - designated: 指定の入力形式
    ///   - p: 既定値の位置
    /// - Returns: 既定値、またはエラー
    private func getDefaultValue(with designated: InputFormat, at p: Int) -> JpfObject {
        if let expression = designated.values[p] {
            guard let result = expression.evaluated(with: self) else {
                return InputFormatError.failedToEvaluate.message
            }
            if result.isError {return result}
            let type = designated.formats[p].type               // 指定の型
            if !type.isEmpty && type != result.type {
                return InputFormatError.typeOfDefaultValue.message
            }
            return pull()!
        }
        return InputFormatError.cannotGetDefault.message
    }
    /// 同じ指定格を持つ連続するオブジェクトの数を返す。
    /// - Parameters:
    ///   - elelments: 母集団(対象のオブジェクト配列)
    ///   - p: 指定格
    /// - Returns: オブジェクトの数
    private func numberOfElements(in elelments: [JpfObject], withConsecutive format: InputFormat.Format) -> Int {
        var counter = 0
        while (counter < elelments.count && isSameFormat(of: elelments[counter], as: format)) {counter += 1}
        return counter
    }
    private func isSameFormat(of object: JpfObject, as format: InputFormat.Format) -> Bool {
        return isSameType(of: object, as: format.type) && isSameParticle(of: object, as: format.particle)
    }
    /// 対象のオブジェクトの型をチェック
    private func isSameType(of object: JpfObject, as type: String) -> Bool {
        return type.isEmpty || object.value?.contains(type: type) ?? false
    }
    /// 対象のオブジェクトの格をチェック
    private func isSameParticle(of object: JpfObject, as particle: String) -> Bool {
        return particle.isEmpty || object.particle == Token(word: particle)
    }
    private func isSameParticle(of object: JpfObject, as particle: Token.Particle?) -> Bool {
        if let p = particle {
            return object.particle == Token(p)
        }
        return object.particle == nil
    }
    /// 対象のオブジェクトの識別子をチェック
    private func isSameName(of object: JpfObject, as name: String) -> Bool {
        return object.value?.name.isEmpty ?? true || object.value?.name == name
    }
    /// 形式から格を取得
    private func getParticle(from s: String) -> Token.Particle? {
        return Token.Particle(rawValue: s)
    }
    /// 関数ブロックのエラーをチェックし、結果を返す。
    /// - Parameter function: 対象の関数ブロック
    /// - Returns: エラー(無しの場合はnil)
    private func errorMessage(to function: FunctionBlock) -> JpfError? {
        let numberOfParameters = function.parameters.count
        if let numberOfInputs = function.signature.numberOfInputs { // 固定長入力指定
            guard numberOfInputs == numberOfParameters && self.count >= numberOfParameters else {
                return InputFormatError.numberOfParameters(numberOfParameters).message
            }
            let parameters = self.peek(numberOfParameters)!
            for (parameter, format) in zip(parameters, function.signature.formats) {
                if let error = errorMessage(to: parameter, with: format) {return error}
            }
        } else {                                                    // 可変長入力指定
            if self.isEmpty {return InputFormatError.numberOfParameters(numberOfParameters).message}
            let params = self.getAll()
            var n = params.count - 1
            for (format, value) in zip(function.signature.formats.reversed(),function.signature.values.reversed()) {
                guard n >= 0 else {break}                           // 引数が無い
                if format.hasThreeDots {                            // 可変長指定
                    return errorMessage(to: params[0..<n].reversed(), with: format)
                } else {
                    if value != nil {continue}
                    if let error = errorMessage(to: params[n], with: format) {return error}
                    n -= 1
                }
            }
        }
        return nil
    }
    private func errorMessage(to f: FunctionBlock, with args: Environment) -> JpfError? {
        let pairs = args.enumerated
        for (k, v) in pairs {
            if f.isVariable(parmeter: k) {
                guard v.type == JpfArray.type else {
                    return InputFormatError.variable(v.type).message
                }
            } else {
                guard f.isSameType(of: k, as: v.type) else {
                    return InputFormatError.type(v.type).message
                }
            }
        }
        return nil
    }
    /// 可変長の引数のエラーチェック
    /// - Parameters:
    ///   - parameters: 引数
    ///   - format: チェックする入力形式
    /// - Returns: エラー、無ければnil
    private func errorMessage(to parameters: [JpfObject], with format: InputFormat.Format) -> JpfError? {
        guard let particle = getParticle(from: format.particle) else {  // 格をチェック
            return InputFormatError.particleFormat(format.particle).message
        }
        if let index = parameters.firstIndex(where: {!isSameParticle(of: $0, as: particle)}) {
            return InputFormatError.particle(parameters[index].particle?.literal ?? "無").message
        }
        if let index = parameters.firstIndex(where: {!isSameType(of: $0, as: format.type)}) {
            return InputFormatError.type(parameters[index].value?.type ?? "無").message
        }
        return nil
    }
    /// 引数のエラーチェック(型と格)
    /// - Parameters:
    ///   - parameter: 引数
    ///   - format: チェックする入力形式
    /// - Returns: エラー、無ければnil
    private func errorMessage(to parameter: JpfObject, with format: InputFormat.Format) -> JpfError? {
        if !isSameType(of: parameter, as: format.type) {return InputFormatError.type(parameter.value?.type ?? "無").message}
        if !isSameParticle(of: parameter, as: format.particle) {return InputFormatError.particle(parameter.particle?.literal ?? "無").message}
        return nil
    }
    // エラー
    enum InputFormatError : Error {
        case numberOfParameters(Int)
        case type(String)
        case particle(String)
        case particleFormat(String)
        case variable(String)
        case notFoundFirstFormat
        case noMatchingSignature
        case noParameterValue
        case failedToEvaluate
        case typeOfDefaultValue
        case cannotGetDefault
        /// エラーメッセージ
        var message: JpfError {
            switch self {
            case .numberOfParameters(let number):   return JpfError("入力の数が足りていない。必要数：\(number)")
            case .type(let type):                   return JpfError("入力の型が異なる。入力の型：\(type)")
            case .particle(let particle):           return JpfError("入力の格が異なる。入力の格：\(particle)")
            case .particleFormat(let s):            return JpfError("格の形式が誤っている。指定：\(s)")
            case .variable(let type):               return JpfError("可変長識別子の値が配列ではない。型：\(type)")
            case .notFoundFirstFormat:              return JpfError("可変長識別子に既定値が設定されている。")
            case .noMatchingSignature:              return JpfError("入力形式が一致する関数が見つからなかった。")
            case .noParameterValue:                 return JpfError("固定長パラメータの値が取得できなかった。")
            case .failedToEvaluate:                 return JpfError("既定値の評価に失敗した。")
            case .typeOfDefaultValue:               return JpfError("既定値の型が指定形式と一致しない。")
            case .cannotGetDefault:                 return JpfError("既定値が設定されていなかった。")
            }
        }
    }
    /// 規約(protocols)に違反していたら、エラーを返す。
    func conform(to protocols: [JpfProtocol], isTypeMember: Bool = false) -> JpfError? {
        for p in protocols {
            if let result = conform(to: p.clauses, isTypeMember: isTypeMember), result.isError {return result}  // 準拠エラー
        }
        return nil
    }
    /// 規約(protocols)に違反していたら、エラーを返す。
    func conform(to protocols: [String], isTypeMember: Bool = false) -> JpfError? {
        for s in protocols {
            guard let p = self[s] as? JpfProtocol else {return ConformityViolation.identifier(s).error}
            if let result = conform(to: p.clauses, isTypeMember: isTypeMember), result.isError {return result}  // 準拠エラー
        }
        return nil
    }
    /// 条項(clauses)に違反していたら、エラーを返す。
    private func conform(to clauses: [ClauseLiteral],  isTypeMember: Bool = false) -> JpfError? {
        for clause in clauses {
            guard isTypeMember == clause.isTypeMember else {continue}
            let name = clause.identifier.value, type = clause.type
            guard let target = self[name] else {return ConformityViolation.identifier(name + "\(type)").error}  // 対象のオブジェクト
            guard type == target.type else {return ConformityViolation.type(type).error}
            switch target {
            case let function as JpfFunction:                                   // 関数の引数チェック
                guard let params = clause.functionParams else {return nil}      // 引数無し
                if function.functions.hasParamaeter(to: params) {return nil}    // 引数準拠
                // エラー処理(引数準拠違反)
                guard let f = function.functions.array.last else {return ConformityViolation.targetNotFound.error}
                return errorMessage(of: clause.functionParams, with: f)
            case let computation as JpfComputation:                             // 算出の引数チェック
                if clause.getterParams == nil && clause.setterParams == nil {
                    return nil                                                  // 引数無し
                }
                if let params1 = clause.getterParams, let params2 = clause.setterParams,
                   computation.getters.hasParamaeter(to: params1) &&
                   computation.setters.hasParamaeter(to: params2) {
                    return nil                                                  // 設定と取得、両方の引数が準拠
                }                             
                if let params = clause.getterParams,
                   computation.getters.hasParamaeter(to: params) {return nil}   // 取得の引数が準拠
                if let params = clause.setterParams,
                   computation.setters.hasParamaeter(to: params) {return nil}   // 設定の引数が準拠
                // エラー処理(いずれかの引数が準拠違反)
                if let getter = computation.getters.array.last {
                    return errorMessage(of: clause.functionParams, with: getter)
                }
                if let setter = computation.setters.array.last {
                    return errorMessage(of: clause.functionParams, with: setter)
                }
                return ConformityViolation.targetNotFound.error
            default:
                break
            }
        }
        return nil
    }
    private func errorMessage(of params: ParameterClauseLiteral?, with f: FunctionBlock) -> JpfError {
        // 規約に引数の指定が無い
        guard let parameters = params?.parameters else {
            return ConformityViolation.degignatedError.error
        }
        // 引数の数が一致しない
        guard parameters.count == f.parameters.count else {return ConformityViolation.numberOfParams(parameters.count).error}
        for pairs in zip(parameters, f.parameters) {
            guard pairs.0.value == pairs.1.value else {return ConformityViolation.nameOfParams(pairs.0.value).error}
        }
        // シグネチャが一致しない
        return compareSignature(params?.signature, to: f.signature)
    }
    /// シグネチャに一致しなければ、エラーを返す。
    private func compareSignature(_ lhs: InputFormat?, to rhs: InputFormat) -> JpfError {
        // チェックするシグネチャーが無い
        guard let target = lhs else {return ConformityViolation.degignatedError.error}
        // 入力の数が一致しない
        guard target.numberOfInputs == rhs.numberOfInputs else {return ConformityViolation.formatOfParams(target.numberOfInputs.map {"\($0)"} ?? "無し").error}
        // 入力の形式が一致しない
        for pairs in zip(target.formats, rhs.formats) {
            guard pairs.0.type == pairs.1.type && pairs.0.particle == pairs.1.particle && pairs.0.threeDots == pairs.1.threeDots else {
                return ConformityViolation.formatOfParams("「\(pairs.0.type)\(pairs.0.particle)」").error
            }
        }
        return ConformityViolation.degignatedError.error
    }
    /// 準拠違反
    enum ConformityViolation : Error {
        case identifier(String)
        case type(String)
        case numberOfParams(Int)
        case nameOfParams(String)
        case formatOfParams(String)
        case targetNotFound             // 準拠対象
        case degignatedError            // 指定形式のエラー
        /// エラーメッセージ
        var error: JpfError {
            switch self {
            case .identifier(let s):        return JpfError("指定した識別子が見つからない。識別子名:" + s)
            case .type(let s):              return JpfError("指定した型が規約に準拠していない。型名：" + s)
            case .numberOfParams(let n):    return JpfError("引数の数が規約に準拠していない。指定数：" + String(n))
            case .nameOfParams(let s):      return JpfError("引数の名前が規約に準拠していない。指定値：" + s)
            case .formatOfParams(let s):    return JpfError("引数の形式が規約に準拠していない。指定形式：" + s)
            case .targetNotFound:           return JpfError("対象となる定義がみつからない。")
            case .degignatedError:          return JpfError("規約の指定が間違っている。")
            }
        }
    }
}
