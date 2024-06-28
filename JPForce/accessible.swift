//
//  accessible.swift
//  JPForce
//
//  Created by 佐藤貴之 on 2024/06/24.
//

import Foundation

protocol Accessible {
    /// オブジェクトをaccessorによって、アクセスする。
    /// - Parameters:
    ///   - accessor: 対象のオブジェクトにアクセスする後置オブジェクト
    ///   - environment: ローカル環境
    /// - Returns: アクセス結果(オブジェクト)
    func accessed(by accessor: JpfObject, with environment: Environment) -> JpfObject?
}
extension JpfObject {
    /// デフォルト実装(acessorが文字列ならば、オブジェクトの属性を取得)
    func accessed(by accessor: JpfObject, with environment: Environment) -> JpfObject? {
        guard let string = accessor as? JpfString else {
            return "\(self.string)を\(accessor.type)" + cannotAccessObjectWith
        }
        return getObject(from: string.value)
    }
    var unusableAsIndex: JpfError       {JpfError("は、「配列」の要素の索引として使用できない。")}
    var unusableAsHashKey: JpfError     {JpfError("は、「辞書」の要素の索引(ハッシュキー)として使用できない。")}
    var cannotAccessObjectWith: JpfError{JpfError("でアクセスすることはできない。")}
}
extension JpfArray : Accessible {
    func accessed(by accessor: JpfObject, with environment: Environment) -> (any JpfObject)? {
        switch accessor {
        case let int as JpfInteger:     // <配列>の<位置>
            return self[int.value]
        case let range as JpfRange:     // <配列>の<範囲>
            guard let subArray = self[range] else {break}
            return subArray
        case let string as JpfString:   // <配列>の<要素属性>、または<オブジェクト>の<属性>
            return self[string.value, nil] ?? "「\(accessor.type)」" + unusableAsIndex
        default:
            return "「\(accessor.type)」" + unusableAsIndex
        }
        return JpfNull.object
    }
}
extension JpfInput : Accessible {
    func accessed(by accessor: JpfObject, with environment: Environment) -> (any JpfObject)? {
        switch accessor {
        case let int as JpfInteger:     // <入力>の<位置>
            return self[int.value]
        case let string as JpfString:   // <入力>の<要素属性>、または<オブジェクト>の<属性>
            return self[string.value, nil] ?? "「\(accessor.type)」" + unusableAsIndex
        default:
            break
        }
        return "「\(accessor.type)」" + unusableAsIndex
    }
}
extension JpfDictionary : Accessible {
    func accessed(by accessor: JpfObject, with environment: Environment) -> (any JpfObject)? {
        switch accessor {
        case let string as JpfString:   // <辞書>の<索引(文字列)>、または<オブジェクト>の<属性>
            if let pair = pairs[string.hashKey] {
                return pair.value
            }
            return getObject(from: string.value) ?? JpfNull.object
        case let int as JpfInteger:     // <辞書>の<索引(数値)>
            guard let pair = pairs[int.hashKey] else {break}
            return pair.value
        case let boolean as JpfBoolean: // <辞書>の<索引(真偽値)>
            guard let pair = pairs[boolean.hashKey] else {break}
            return pair.value
        default:
            return "「\(accessor.type)」" + unusableAsHashKey
        }
        return JpfNull.object
    }
}
extension JpfString : Accessible {
    func accessed(by accessor: any JpfObject, with environment: Environment) -> (any JpfObject)? {
        switch accessor {
        case let int as JpfInteger:     // <文字列>の<位置>
            return self[int.value]
        case let range as JpfRange:     // <文字列>の<範囲>
            guard let subString = self[range] else {return JpfNull.object}
            return subString
        case let string as JpfString:   // <文字列>の<属性>
            return self[string.value, nil] ??  "\(self.string)を\(string.value)" + cannotAccessObjectWith
        default:
            break
        }
        return "\(self.string)を\(accessor.type)" + cannotAccessObjectWith
    }
}
