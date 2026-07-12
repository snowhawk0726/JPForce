//
//  builtin.swift   (拡張オブジェクト)
//  JPForce
//
//  Created by 佐藤貴之 on 2023/03/24.
//

import Foundation

protocol ContainerProtocol {
    func contains(_ element: JpfObject) -> JpfObject
}

/// オブジェクトの属性(属性名(name)とアクセス方法(acessor))
struct ObjectProperties {
    init() {
        self.allProperties = commonProperties + specificProperties
    }
    typealias NamedAccessor = (name: String, accessor: (JpfObject) -> JpfObject?)
    let allProperties: [NamedAccessor]
    let commonProperties: [NamedAccessor] = [       // オブジェクト共通属性
        ("型", {JpfString(value: $0.type)}),                         // 0
        ("格", {$0.particle.map {JpfString(value: $0.literal)} ?? JpfNull.object}),
        ("値", {$0.value ?? JpfNull.object}),
        ("数値", {$0.number.map {JpfInteger(value: $0)} ?? JpfNull.object}),
        ("文字列", {JpfString(value: $0.string)}),
        ("識別子名", {   /* 三項演算子だと型推論がうまくいかなかった */       // 5
            guard !$0.name.isEmpty else {return JpfNull.object}
            return JpfString(value: $0.name)
        }),
        ("数", {$0.count}),
        ("空", {$0.isEmpty}),
    ]
    let specificProperties: [NamedAccessor] = [     // オブジェクト固有属性
        ("正", {($0.value!)["正", $0.particle]}),                     // 8 (JpfInteger)
        ("負", {($0.value!)["負", $0.particle]}),
        ("最初", {($0.value!)["最初", $0.particle]}),                 // 10 (JpfString, JpfArray, ...)
        ("先頭", {($0.value!)["先頭", $0.particle]}),
        ("最後", {($0.value!)["最後", $0.particle]}),
        ("後尾", {($0.value!)["後尾", $0.particle]}),
        ("残り", {($0.value!)["残り", $0.particle]}),
        ("列挙子", {($0.value!)["列挙子", $0.particle]}),              // 15 (JpfEnum, JpfEnumerator)
        ("値", {($0)["値", $0.value is JpfPhrase ? $0.particle : Token(.NO)]}),
    ]
    // アクセサ
    subscript(index: Int) -> NamedAccessor? {
        (0..<allProperties.count).contains(index) ? allProperties[index] : nil
    }
    subscript(name: String) -> NamedAccessor? {allProperties.first {$0.name == name}}
    var names: [String] {allProperties.map(\.name)}
    var common: [String] {commonProperties.map(\.name)}
    var specific: [String] {specificProperties.map(\.name)}
    // 全属性名
    static let allPropertyNames: Set<String> = {
        let base = ObjectProperties()
        return Set(base.allProperties.map(\.name))
    }()
    static func hasName(_ name: String) -> Bool {allPropertyNames.contains(name)}
}

extension JpfObject {
    /// オブジェクトへのアクセス
    subscript(name: String, particle: Token?) -> JpfObject? {
        get {getProperty(by: name, with: particle)}
    }
    func getProperty(by name: String, with particle: Token? = nil) -> JpfObject? {
        let properties = ObjectProperties()
        switch particle {
        case Token(.NO) where properties.common.contains(name) && name != "空",
             Token(.GA) where name == "空",
             nil        where properties.common.contains(name):
            return properties[name]?.accessor(self)
        default:
            if properties.specific.contains(name) {
                return "\(self.string)(\(self.type))を「\(name)」" + cannotAccessObjectWith
            }
        }
        return nil
    }
    /// オブジェクト自身の対象に、値を設定する。対象が算出である場合は、算出の設定処理を行う。
    /// - Parameters:
    ///   - value: 設定する値
    ///   - target: 設定する対象識別子
    ///   - environment: オブジェクトの環境
    /// - Returns: エラーまたはオブジェクト自身
    func assign(_ value: JpfObject, to target: JpfObject?, with environment: Environment) -> JpfObject {
        guard let name = getName(target), environment.contains(name)
        else {
            return JpfError(identifierNotFound + ":\(target?.string ?? "無し")")
        }
        if let computation = environment[name] as? JpfComputation {         // 算出の設定を行う
            if let err = environment.push(value) {return err}               // 設定値
            if let result = computation.setter(with: environment), result.isError {return result}
        } else {
            environment[name] = value
        }
        return self
    }
    private func getName(_ target: JpfObject?) -> String? {
        switch target {
        case let s as JpfString:        return s.value
        case let i as JpfIdentifier:    return i.value
        default:
            return nil
        }
    }
    // デフォルト実装(エラー)
    var count: JpfObject {JpfError(cannotCount)}
    var isEmpty: JpfObject {JpfError(cannotCount)}
    func add(_ object: JpfObject) -> JpfObject {JpfError("「\(type)」と「\(object.type)」" + cannotAdd)}
    func remove(_ object: JpfObject) -> JpfObject {JpfError("「\(type)」から「\(object.string)」" + cannotRemove)}
    func contains(_ object: JpfObject) -> JpfObject {JpfBoolean.FALSE}
    func contains(where function: JpfFunction, with environment: Environment) -> JpfObject {JpfBoolean.FALSE}
    func foreach(_ function: JpfFunction, with environment: Environment) -> JpfObject? {JpfError(cannotRepeat)}
    func map(_ function: JpfFunction, with environment: Environment) -> JpfObject {JpfError(cannotMapping)}
    func filter(_ function: JpfFunction, with environment: Environment) -> JpfObject {JpfError(cannotFilter)}
    func reduce(_ initial: JpfObject, _ function: JpfFunction, with environment: Environment) -> JpfObject {JpfError(cannotReduce)}
    func sorted() -> JpfObject {JpfError(cannotSortByOrder)}
    func sorted(by string: JpfString) -> JpfObject {JpfError(cannotSortByOrder)}
    func sorted(by function: JpfFunction, with environment: Environment) -> JpfObject {JpfError(cannotSortByFunc)}
    func reversed() -> JpfObject {JpfError(cannotReverse)}
    func assign(_ value: JpfObject, to target: JpfObject?) -> JpfObject {JpfError(cannotAssign)}
    // エラーメッセージ
    var cannotCount: String     {"\(type)型の要素の数は、数えることができません。"}
    var cannotAdd: String       {"を足すことはできません。"}
    var cannotRemove: String    {"を削除することはできません。"}
    var removeTargetNotFound: String{"削除しようとする対象が見つかりません。"}
    var cannotCompare: String   {"を比較することはできません。"}
    var cannotCountRange: String{"「範囲」の上下限が数値でないため、数えることができません。"}
    var rangeFormatError: String{"「範囲」の形式が間違っています。"}
    var cannotRepeat: String    {"「\(type)」を「関数」で、繰り返すことはできません。仕様：<配列、辞書、範囲>を<関数>で繰り返す。"}
    var cannotReduce: String    {"「\(type)」を「関数」で、まとめることはできません。仕様：<配列、辞書>を<初期値>と<関数>でまとめる。"}
    var cannotMapping: String   {"「\(type)」を「関数」で、写像することはできません。仕様：<配列、辞書>を<関数>で写像する。"}
    var cannotFilter: String   {"「\(type)」を「関数」で、絞り込むことはできません。仕様：<配列、辞書>を<関数>で絞り込む。"}
    var cannotSortByFunc: String{"「\(type)」を並べ替えることはできません。仕様：<配列>を<関数>で並べ替える。"}
    var cannotSortByOrder: String{"「\(type)」を並べ替えることはできません。仕様：<配列>を(「昇順」に、または「降順」に)並べ替える。"}
    var cannotReverse: String   {"「\(type)」は逆順にすることはできません。仕様：<配列、文字列>を逆順にする。"}
    var cannotAssign: String    {"「\(type)」に値を代入することはできません。"}
    var identifierNotAvailable: String  {"(識別子)は利用可能ではありません。"}
    var arrayPositionError: String      {"指定位置が、配列内にありません。"}
    var arrayPositionIsNotNumber: String{"指定位置が、数値ではありません。"}
    var keyIsNotHashable: String        {"キーが、ハッシュ化可能な型ではありません。"}
    var identifierNotFound: String      {"指定した識別子名が見つかりません。"}
    var typeNotFound: String            {"型「\(type)」の定義が見つかりません。"}
    var notExecutableObject: JpfError   {JpfError("「関数」以外を実行しようとしました。型：")}
    var functionParameterError: JpfError{JpfError("「関数」の入力が指定形式と一致しません。")}
    var getterNotFound: JpfError        {JpfError("「算出」の取得定義が見つかりません。")}
    var setterNotFound: JpfError        {JpfError("「算出」の設定定義が見つかりません。")}
}
extension JpfInteger {
    func add(_ object: JpfObject) -> JpfObject {
        guard let number = object.number else {return JpfError("「\(type)」と「\(object.type)」" + cannotAdd)}
        return JpfInteger(name: self.name, value: value + number)
    }
    subscript(name: String, particle: Token?) -> JpfObject? {
        switch (particle, name) {
        case (Token(.GA),"正"),(nil,"正"):    return JpfBoolean(value: value > 0)
        case (Token(.GA),"負"),(nil,"負"):    return JpfBoolean(value: value < 0)
        default:                            return getProperty(by: name, with: particle)
        }
    }
}
extension RangeBoundary {
    func isEqual(to bound: RangeBoundary) -> Bool {
        value.value == bound.value.value &&
        inclusive == bound.inclusive
    }
}
extension JpfRange : ContainerProtocol {
    /// 範囲の比較（未満と他の上限は不一致）
    func isEqual(to object: JpfObject) -> Bool {
        guard let rhs = object as? Self else {return false}
        // 下限・上限ともに、「数値」と「inclusive（含む/含まない）」が一致しているかで判定
        switch (self.lowerBound, rhs.lowerBound, self.upperBound, rhs.upperBound) {
        case (nil, nil, nil, nil):          // 空
            return true
        case let (l1?, l2?, nil, nil):      // 下限のみ
            return l1.isEqual(to: l2)
        case let (nil, nil, u1?, u2?):      // 上限のみ
            return u1.isEqual(to: u2)
        case let (l1?, l2?, u1?, u2?):      // 上下限
            return l1.isEqual(to: l2) && u1.isEqual(to: u2)
        default:
            return false
        }
    }
    var count: JpfObject {
        guard let lowerBoundNumber, let upperBoundNumber else {return JpfError(cannotCountRange)}
        return JpfInteger(value: upperBoundNumber - lowerBoundNumber + 1)
    }
    var isEmpty: JpfObject {
        JpfBoolean.object(of: self.count.number == 0)
    }
    func contains(_ object: JpfObject) -> JpfObject {
        guard let v = object.number else {
            return JpfError("「\(object.type)」と「\(type)」" + cannotCompare)
        }
        switch (lowerBoundNumber, upperBoundNumber) {
        case let (l?, u?):
            return JpfBoolean.object(of: l <= v && v <= u)
        case let (l?, nil):
            return JpfBoolean.object(of: v >= l)
        case let (nil, u?):
            return JpfBoolean.object(of: v <= u)
        case (nil, nil):
            return JpfError(rangeFormatError)
        }
    }
    /// 範囲の形式チェック(nil: エラー無し)
    var error: JpfError? {
        switch (lowerBound, upperBound) {
        case (nil, nil):
            return JpfError(rangeFormatError)
        default:
            return nil
        }
    }
    // 要素アクセス
    func foreach(_ function: JpfFunction, with environment: Environment) -> JpfObject? {
        switch (lowerBound?.value.number, lowerBound?.inclusive, upperBound?.value.number, upperBound?.inclusive) {
        case (let l?, true, let u?, true):
            (l...u).forEach { element in
                if environment.push(JpfInteger(value: element)) != nil {return}
                _ = function.execute(with: environment)
            }
        case (let l?, true, let u?, false):
            (l..<u).forEach { element in
                if environment.push(JpfInteger(value: element)) != nil {return}
                _ = function.execute(with: environment)
            }
        default:
            return JpfError(rangeFormatError)
        }
        return nil
    }
    func map() -> JpfObject {
        var mapped: [JpfObject] = []
        switch (lowerBound?.value.number, lowerBound?.inclusive, upperBound?.value.number, upperBound?.inclusive) {
        case (let l?, true, let u?, true):
            mapped = (l...u).map {JpfInteger(value: $0)}
        case (let l?, true, let u?, false):
            mapped = (l..<u).map {JpfInteger(value: $0)}
        default:
            return JpfError(rangeFormatError)
        }
        return JpfArray(name: self.name, elements: mapped)
    }
    func map(_ function: JpfFunction, with environment: Environment) -> JpfObject {
        var mapped: [JpfObject] = []
        switch (lowerBound?.value.number, lowerBound?.inclusive, upperBound?.value.number, upperBound?.inclusive) {
        case (let l?, true, let u?, true):
            mapped = (l...u).map { element in
                if let err = environment.push(JpfInteger(value: element)) {return err}
                return function.execute(with: environment) ??
                environment.pull() ?? JpfNull.object
            }
        case (let l?, true, let u?, false):
            mapped = (l..<u).map { element in
                if let err = environment.push(JpfInteger(value: element)) {return err}
                return function.execute(with: environment) ??
                environment.pull() ?? JpfNull.object
            }
        default:
            return JpfError(rangeFormatError)
        }
        return JpfArray(name: self.name, elements: mapped)
    }
    func reduce(_ initial: JpfObject, _ function: JpfFunction, with environment: Environment) -> JpfObject {
        switch (lowerBound?.value.number, lowerBound?.inclusive, upperBound?.value.number, upperBound?.inclusive) {
        case (let l?, true, let u?, true):
            return (l...u).reduce(initial) {f($0, $1, function, with: environment)}
        case (let l?, true, let u?, false):
            return (l..<u).reduce(initial) {f($0, $1, function, with: environment)}
        default:
            return JpfError(rangeFormatError)
        }
    }
    private func f(_ initial: JpfObject, _ element: Int, _ function: JpfFunction, with environment: Environment) -> JpfObject {
        if let err = environment.push(initial) {return err}
        if let err = environment.push(JpfInteger(value: element)) {return err}
        return function.execute(with: environment) ??
        environment.pull() ?? initial
    }
    var toObjects: [any JpfObject]? {
        guard let min = lowerBoundNumber else {return nil}
        guard let max = upperBoundNumber else {return nil}
        let array = Array(min...max)
        return array.map {JpfInteger(value: $0)}
    }
}
extension JpfString : ContainerProtocol {
    var count: JpfObject {JpfInteger(value: value.count)}
    var isEmpty: JpfObject {JpfBoolean.object(of: value.isEmpty)}
    var number: Int? {Int(value)}
    func add(_ object: JpfObject) -> JpfObject {
        guard let string = object as? Self else {return JpfError("「\(type)」と「\(object.type)」" + cannotAdd)}
        return JpfString(name: self.name, value: value + string.value)
    }
    func contains(_ object: JpfObject) -> JpfObject {
        guard let string = object as? JpfString else {return JpfBoolean.FALSE}
        return JpfBoolean.object(of: value.contains(string.value))
    }
    func reversed() -> JpfObject {JpfString(name: self.name, value: String(value.reversed()))}
    // 要素アクセス
    subscript(index: Int) -> JpfObject? {
        guard case 0..<value.count = index else {return JpfNull.object}
        let i = value.index(value.startIndex, offsetBy: index)
        return JpfString(value: String(value[i]))
    }
    subscript(name: String, particle: Token?) -> JpfObject? {
        switch (particle, name) {
        case (Token(.NO),"最初"),(Token(.TA),"最初"),(nil,"最初"),
             (Token(.NO),"先頭"),(Token(.TA),"先頭"),(nil,"先頭"):
            return value.first.map {JpfString(value: String($0))} ?? JpfNull.object
        case (Token(.NO),"最後"),(Token(.TA),"最後"),(nil,"最後"),
             (Token(.NO),"後尾"),(Token(.TA),"後尾"),(nil,"後尾"):
            return value.last.map {JpfString(value: String($0))} ?? JpfNull.object
        case (Token(.NO),"残り"),(Token(.TA),"残り"),(nil,"残り"):
            guard !value.isEmpty else {return JpfNull.object}
            return JpfString(value: String(value.dropFirst()))
        default:
            return getProperty(by: name, with: particle)
        }
    }
    subscript(range: JpfRange) -> JpfObject? {
        switch (range.lowerBound?.value.number, range.lowerBound?.inclusive, range.upperBound?.value.number, range.upperBound?.inclusive) {
        case (let l?, true, let u?, true):
            return JpfString(value: String(value[index(of: l)...index(of: u)]))
        case (let l?, true, let u?, false):
            return JpfString(value: String(value[index(of: l)..<index(of: u)]))
        case (let l?, true, nil, nil):
            return JpfString(value: String(value[index(of: l)...]))
        case (nil, nil, let u?, true):
            return JpfString(value: String(value[...index(of: u)]))
        case (nil, nil, let u?, false):
            return JpfString(value: String(value[..<index(of: u)]))
        default:
            break
        }
        return JpfError(rangeFormatError)
    }
    private func index(of: Int) -> String.Index {
        value.index(value.startIndex, offsetBy: of)
    }
}
extension JpfInput {
    var count: JpfObject {JpfInteger(value: stack.count)}
    var isEmpty: JpfObject {JpfBoolean.object(of: stack.isEmpty)}
    // 要素アクセス
    subscript(index: Int) -> JpfObject? {
        guard case 0..<stack.count = index else {return JpfNull.object}
        return stack[index]
    }
    subscript(name: String, particle: Token?) -> JpfObject? {
        switch (particle, name) {
        case (Token(.NO),"最初"),(Token(.TA),"最初"),(nil,"最初"),
             (Token(.NO),"先頭"),(Token(.TA),"先頭"),(nil,"先頭"):
            return stack.first ?? JpfNull.object
        case (Token(.NO),"最後"),(Token(.TA),"最後"),(nil,"最後"),
             (Token(.NO),"後尾"),(Token(.TA),"後尾"),(nil,"後尾"):
            return stack.last ?? JpfNull.object
        default:
            return getProperty(by: name, with: particle)
        }
    }
}
extension JpfArray : ContainerProtocol {
    func isEqual(to object: JpfObject) -> Bool {
        guard let rhs = object as? Self else {return false}
        return elements.count == rhs.elements.count &&
        !zip(elements, rhs.elements).contains(where: {!$0.isEqual(to: $1)})
    }
    var count: JpfObject {JpfInteger(value: elements.count)}
    var isEmpty: JpfObject {JpfBoolean.object(of: elements.isEmpty)}
    func add(_ object: JpfObject) -> JpfObject {
        guard let array = object as? Self else {return JpfError("「\(type)」と「\(object.type)」" + cannotAdd)}
        return JpfArray(name: self.name, elements: elements + array.elements)
    }
    // 要素アクセス
    subscript(index: Int) -> JpfObject? {
        guard case 0..<elements.count = index else {return JpfNull.object}
        return elements[index]
    }
    subscript(name: String, particle: Token?) -> JpfObject? {
        switch (particle, name) {
        case (Token(.NO),"最初"),(Token(.TA),"最初"),(nil,"最初"),
             (Token(.NO),"先頭"),(Token(.TA),"先頭"),(nil,"先頭"):
            return elements.first ?? JpfNull.object
        case (Token(.NO),"最後"),(Token(.TA),"最後"),(nil,"最後"),
             (Token(.NO),"後尾"),(Token(.TA),"後尾"),(nil,"後尾"):
            return elements.last ?? JpfNull.object
        case (Token(.NO),"残り"),(Token(.TA),"残り"),(nil,"残り"):
            guard !elements.isEmpty else {return JpfNull.object}
            return JpfArray(name: self.name, elements: [JpfObject](elements.dropFirst()))
        default:
            break
        }
        return getProperty(by: name, with: particle)
    }
    subscript(range: JpfRange) -> JpfObject? {
        switch (range.lowerBound?.value.number, range.lowerBound?.inclusive, range.upperBound?.value.number, range.upperBound?.inclusive) {
        case (let l?, true, let u?, true):
            return JpfArray(name: self.name, elements: Array(elements[l...u]))
        case (let l?, true, let u?, false):
            return JpfArray(name: self.name, elements: Array(elements[l..<u]))
        case (let l?, true, nil, nil):
            return JpfArray(name: self.name, elements: Array(elements[l...]))
        case (nil, nil, let u?, true):
            return JpfArray(name: self.name, elements: Array(elements[...u]))
        case (nil, nil, let u?, false):
            return JpfArray(name: self.name, elements: Array(elements[..<u]))
        default:
            break
        }
        return JpfError(rangeFormatError)
    }
    func assign(_ value: JpfObject, to target: JpfObject?) -> JpfObject {
        var array = self
        do {
            guard let position = try getNumber(of: target) else {
                return JpfError(arrayPositionError) + "(位置：\(target?.string ?? "無し"))"
            }
            array.elements[position] = value
            return array
        } catch {
            return jpfError(from: error)
        }
    }
    func insert(_ value: JpfObject, at target: JpfObject?) -> JpfObject {
        var array = self
        do {
            guard let position = try getNumber(of: target) else {
                return self
            }
            array.elements.insert(value, at: position)
            return array
        } catch {
            return jpfError(from: error)
        }
    }
    func remove(_ target: JpfObject) -> JpfObject {
        var objects = elements
        do {
            // TODO: 指定子をJpfStringでなくJpfIdentifierにする
            if let specifier = getSpecifier(from: target.value),
               specifier == "全て" {
                objects.removeAll()
            } else
            if let index = try getNumber(of: target) {
                objects.remove(at: index)
            } else {    // 削除できない場合
                return self
            }
        } catch {
            return jpfError(from: error)
        }
        return JpfArray(name: self.name, elements: objects)
    }
    /// 指定位置を数値として返す
    /// - Parameter obj: 指定オブジェクト(正しくない場合は、エラーを投げる)
    /// - Returns: 指定位置(範囲外はnilを返す)
    private func getNumber(of obj: JpfObject?) throws -> Int? {
        // TODO: 指定子をJpfStringでなくJpfIdentifierにする
        if let specifier = getSpecifier(from: obj?.value) {
            switch specifier {
            case "最初", "先頭":    return 0
            case "最後", "後尾":    return elements.count - 1
            default:
                break
            }
        }
        guard let position = obj?.number else {
            throw JpfError(arrayPositionIsNotNumber) + "(指定：\(obj?.string ?? "無し"))"
        }
        guard case 0..<elements.count = position else {
            return nil
        }
        return position
    }
    private func getSpecifier(from value: JpfObject?) -> String? {
        if let ident = value as? JpfIdentifier {
            return ident.value
        }
        if let stirng = value as? JpfString {
            return stirng.value
        }
        return nil
    }
    func contains(_ object: JpfObject) -> JpfObject {
        return JpfBoolean.object(of: elements.contains {
            if object is JpfRange && $0.isNumber {  // 要素(数値)が範囲内か
                return object.contains($0).isTrue
            }
            return $0.type == object.type &&
                    $0.isEqual(to: object)          // 要素がオブジェクトと一致するか
        })
    }
    func contains(where function: JpfFunction, with environment: Environment) -> JpfObject {
        for element in elements {
            if let err = environment.push(element) {return err}
            if let result = function.execute(with: environment) ?? environment.pull(),
               result.isTrue {
                return result
            }
        }
        return JpfBoolean.FALSE
    }
    func foreach(_ function: JpfFunction, with environment: Environment) -> JpfObject? {
        for element in elements {
            if let err = environment.push(element) {return err}
            _ = function.execute(with: environment)
        }
        return nil
    }
    func map(_ function: JpfFunction, with environment: Environment) -> JpfObject {
        JpfArray(name: self.name, elements: elements.map { element in
            if let err = environment.push(element) {return err}
            return function.execute(with: environment) ??
            environment.pull() ?? JpfNull.object
        })
    }
    func filter(_ function: JpfFunction, with environment: Environment) -> JpfObject {
        JpfArray(name: self.name, elements: elements.filter { element in
            if environment.push(element) != nil {return false}
            let result = function.execute(with: environment) ?? environment.pull()
            return result?.isTrue ?? false
        })
    }
    func reduce(_ initial: JpfObject, _ function: JpfFunction, with environment: Environment) -> JpfObject {
        elements.reduce(initial) { initial, element in
            if let err = environment.push(initial) {return err}
            if let err = environment.push(element) {return err}
            return function.execute(with: environment) ??
            environment.pull() ?? initial
        }
    }
    func sorted(by string: JpfString) -> JpfObject {
        switch string.value {       // 指定の順序で並べ替える
        case "昇順":
            return sorted()
        case "降順":
            if let elements: [JpfInteger] = cast(self.elements) {
                return JpfArray(name: self.name, elements: elements.sorted(by: >))
            } else
            if let elements: [JpfString] = cast(self.elements) {
                return JpfArray(name: self.name, elements: elements.sorted(by: >))
            }
        default:
            break
        }
        return JpfError(cannotSortByOrder)
    }
    func sorted() -> JpfObject {    // 昇順に並べ替える
        if let elements: [JpfInteger] = cast(self.elements) {
            return JpfArray(name: self.name, elements: elements.sorted())
        } else
        if let elements: [JpfString] = cast(self.elements) {
            return JpfArray(name: self.name, elements: elements.sorted())
        }
        return JpfError(cannotSortByOrder)
    }
    func sorted(by function: JpfFunction, with environment: Environment) -> JpfObject { //関数の条件で並べ替える
        return JpfArray(name: self.name, elements: elements.sorted { lhs, rhs in
            if environment.push(lhs) != nil {return false}
            if environment.push(rhs) != nil {return false}
            let result = function.execute(with: environment) ?? environment.pull()
            return result?.isTrue ?? false
        })
    }
    /// 要素が並べ替え可能であれば、その型(T)にキャストする
    /// - Parameter elements: テストする要素
    /// - Returns: キャストした要素(できなければnil)
    private func cast<T : Comparable>(_ elements: [JpfObject]) -> [T]? {
        var results: [T] = []
        for element in elements {
            guard let value = element as? T else {return nil}
            results.append(value)
        }
        return results
    }
    func reversed() -> JpfObject {JpfArray(name: self.name, elements: elements.reversed())}
    var toObjects: [any JpfObject]? {elements}
}
extension JpfDictionary {
    func isEqual(to object: JpfObject) -> Bool {
        guard let rhs = object as? Self else {return false}
        return pairs.count == rhs.pairs.count &&
        !zip(pairs.keys, rhs.pairs.keys).contains(where: {!$0.isEqual(to: $1)}) &&
        !pairs.keys.contains(where: {!pairs[$0]!.key.isEqual(to: rhs.pairs[$0]!.key)}) &&
        !pairs.keys.contains(where: {!pairs[$0]!.value.isEqual(to: rhs.pairs[$0]!.value)})
    }   // 両辺のハッシュキーが等しく、左辺のハッシュキーで引いたキーおよび値が等しい。
    var count: JpfObject {JpfInteger(value: pairs.count)}
    var isEmpty: JpfObject {JpfBoolean.object(of: pairs.isEmpty)}
    // 要素アクセス
    subscript(object: JpfHashable) -> JpfObject? {pairs[object.hashKey]?.value}
    func assign(_ value: JpfObject, to target: JpfObject?) -> JpfObject {
        guard let key = target, key is JpfHashable else {return JpfError(keyIsNotHashable + "型：\(target?.type ?? "無し")")}
        var dictionary = self
        dictionary[key] = value
        return dictionary
    }
    func remove(_ object: JpfObject) -> JpfObject {
        var objects = pairs
        let error = JpfError("「\(type)」から「\(object.string)」" + cannotRemove)
        if let key = object.value as? JpfHashable {
            if let keyword = object.value as? JpfString, keyword.string == "全て" {
                return JpfDictionary(name: self.name, pairs: [:])
            }
            objects[key.hashKey] = nil
            return JpfDictionary(name: self.name, pairs: objects)
        }
        return error
    }
    func contains(name: String) -> Bool {
        pairs.values.contains(where: {($0.key as? JpfString)?.value == name})
    }
    func contains(where function: JpfFunction, with environment: Environment) -> JpfObject {
        for element in pairs.values {
            if let err = environment.push(element.key) {return err}
            if let err = environment.push(element.value) {return err}
            if let result = function.execute(with: environment) ?? environment.pull(),
               result.isTrue {
                return result
            }
        }
        return JpfBoolean.FALSE
    }
    func foreach(_ function: JpfFunction, with environment: Environment) -> JpfObject? {
        for (key, value) in pairs.values {
            if let err = environment.push(key) {return err}
            if let err = environment.push(value) {return err}
            _ = function.execute(with: environment)
        }
        return nil
    }
    func map(_ function: JpfFunction, with environment: Environment) -> JpfObject {
        JpfArray(name: self.name, elements: pairs.values.map { key, value in
            if let err = environment.push(key) {return err}
            if let err = environment.push(value) {return err}
            return function.execute(with: environment) ??
            environment.pull() ?? JpfNull.object
        })
    }
    func filter(_ function: JpfFunction, with environment: Environment) -> JpfObject {
        let values = pairs.values.filter { key, value in
            if environment.push(key) != nil {return false}
            if environment.push(value) != nil {return false}
            let result = function.execute(with: environment) ?? environment.pull()
            return result?.isTrue ?? false
        }
        let keys = values.map {
            let k = $0.key as! JpfHashable
            return k.hashKey
        }
        return JpfDictionary(pairs: Dictionary(uniqueKeysWithValues: zip(keys, values)))
    }
    func reduce(_ initial: JpfObject, _ function: JpfFunction, with environment: Environment) -> JpfObject {
        pairs.values.reduce(initial) { initial, element in
            if let err = environment.push(initial) {return err}
            if let err = environment.push(element.key) {return err}
            if let err = environment.push(element.value) {return err}
            return function.execute(with: environment) ??
            environment.pull() ?? initial
        }
    }
}
extension JpfPhrase {
    func isEqual(to object: JpfObject) -> Bool {
        guard let l = value, let r = object.value else {return (value == nil) && (object.value == nil)}
        return l.isEqual(to: r)
    }   // 両辺がnilは、true
    var count: JpfObject {value.map {$0.count} ?? JpfError(cannotCount)}
    var isEmpty: JpfObject {value.map {$0.isEmpty} ?? JpfError(cannotCount)}
    func add(_ object: JpfObject) -> JpfObject {
        guard let lhs = value, let rhs = object.value else {return JpfError("「\(string)」と「\(object.string)」" + cannotAdd)}
        return lhs.add(rhs)
    }
    var toObjects: [any JpfObject]? {value?.toObjects ?? []}
}
extension JpfType {
    func contains(name: String) -> Bool {environment.contains(name)}
    subscript(name: String, particle: Token?) -> JpfObject? {
        // nameかその連用形が辞書にあるかであれば、オブジェクトを返す。
        let canditate = environment[name] != nil ? name : ContinuativeForm(name).plainForm ?? ""
        if environment.contains(canditate), let object = environment[canditate] {return object}
        // 名前が辞書に無いなら、デフォルト
        return getProperty(by: name, with: particle)
    }
    func assign(_ value: JpfObject, to target: JpfObject?) -> JpfObject {
        assign(value, to: target, with: environment)
    }
}
extension JpfInstance {
    var count: JpfObject {                                      // 利用可能要素(メンバー)数
        JpfInteger(value: availableMembers.count)
    }
    func contains(type: String) -> Bool {return type == self.type || protocols.contains(type)}
    func contains(name: String) -> Bool {environment.contains(name)}
    subscript(name: String, particle: Token?) -> JpfObject? {
        // nameが利用可能なメンバー名であれば、オブジェクトを返す。
        let canditate = environment[name] != nil ? name : ContinuativeForm(name).plainForm ?? ""
        if environment.contains(canditate), let member = environment[canditate] {   // outer除く
            guard availableMembers.contains(canditate) else {return JpfError("『\(name)』" + identifierNotAvailable)}
            if member is JpfFunction {
                if let inputs = environment.outer?.pullAll() {  // 引数をインスタンスに移動(自身を除く)
                    pushParameters(inputs)
                }
            }
            return member                                       // メンバーを返す
        }
        // 利用可能な名前でないなら、デフォルト
        return getProperty(by: name, with: particle)
    }
    func assign(_ value: JpfObject, to target: JpfObject?) -> JpfObject {
        guard availableMembers.contains(target?.name ?? "") else {return JpfError(identifierNotAvailable)}
        return assign(value, to: target, with: environment)
    }
    private func pushParameters(_ objects: [JpfObject]) {
        if environment.push(objects) != nil {return}
        _ = environment.pull {  // 自身を取り除く
            guard let i = $0.value as? JpfInstance else {return false}
            return i.name == self.name
        }
    }
}
extension JpfEnum {
    var count: JpfObject {JpfInteger(value: elements.count)}
    var isEmpty: JpfObject {JpfBoolean(value: elements.isEmpty)}
    func contains(name: String) -> Bool {environment.contains(name)}
    subscript(name: String, particle: Token?) -> JpfObject? {
        if particle == .particle(.NO), elements.contains(name) {
            return JpfEnumerator(type: self.name, identifier: name, rawValue: environment[name])
        } else
        if particle == .particle(.NO), name == "列挙子" {
            return JpfArray(name: self.name, elements: elements.map {JpfString(value: $0)})
        }
        return getProperty(by: name, with: particle)
    }
}
extension JpfEnumerator {
    subscript(name: String, particle: Token?) -> JpfObject? {
        if particle == .particle(.NO) {
            switch name {
            case "列挙子": return JpfString(value: identifier)
            case "値":   return rawValue
            default:
                // <列挙子>の<要素名> → rawValueが、列挙・型・辞書であれば、名前から属性(値)を引く
                if let rawValue, rawValue.contains(name: name) {
                    return rawValue[name, particle]?["値", particle]
                }
                break
            }
        }
        return getProperty(by: name, with: particle)
    }
}
