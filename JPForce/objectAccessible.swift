//
//  objectAccessible.swift
//  JPForce
//
//  Created by 佐藤貴之 on 2023/04/03.
//

import Foundation

// MARK: - intefaces
protocol ObjectAccessible {
    /// オブジェクトへのアクセスを行う。
    /// - Parameter environment: スタックから対象のオブジェクトを取り出す。
    /// - Returns: オブジェクトから取り出した結果(なければnil)を返す
}
protocol NodeAccessible : ObjectAccessible {
    func accessed(with environment: Environment) -> JpfObject?
}
protocol JpfObjectAccessible : ObjectAccessible {
    func accessed(by name: String, with environment: Environment) -> JpfObject?
}
// MARK: - implements (evalutable extentions)
extension ObjectAccessible {
    /// オブジェクト(配列、辞書)へのアクセスである場合、エラーを返す。
    /// - Parameters:
    ///   - type: アクセスしたオブジェクトの型
    ///   - environment: 入力から対象のオブジェクトを得、チェックする。
    /// - Returns: エラーを返す。(対象でない場合は、nil)
    func accessed(type: String, with environment: Environment) -> JpfObject? {
        if environment.isPeekParticle(.NO) {
            switch environment.unwrappedPeek {
            case is JpfArray:
                return "「\(type)」" + unusableAsIndex
            case is JpfDictionary:
                return  "「\(type)」" + unusableAsHashKey
            default:
                break
            }
        }
        return nil
    }
    /// 数値で入力オブジェクトへアクセスをする。
    /// - Parameters:
    ///   - integer: アクセスする索引(数値)
    ///   - environment: 入力から対象のオブジェクトを得る。
    /// - Returns: アクセスした結果を返す。(対象でない場合は、nil)
    func accessed(by integer: Int, with environment: Environment) -> JpfObject? {
        if environment.isPeekParticle(.NO) {    // ノ格でアクセス
            switch environment.unwrappedPeek {
            case let array as JpfArray:
                environment.drop()
                return array[integer]           // <配列>の<数値>
            case let dictionary as JpfDictionary:
                environment.drop()
                let hashKey = JpfHashKey(type: JpfInteger.type, value: integer.hashValue)
                guard let pair = dictionary.pairs[hashKey] else {return JpfNull.object}
                return pair.value               // <辞書>の<索引(数値)>
            default:
                break
            }
        }
        return nil
    }
    /// ハッシュキーでオブジェクトのアクセスをする。
    /// - Parameters:
    ///   - key: アクセスする索引(ハッシュキー)
    ///   - environment: 入力から対象のオブジェクトを得る。
    /// - Returns: アクセスした結果を返す。(対象でない場合は、nil)
    func accessed(by key: JpfHashKey, with environment: Environment) -> JpfObject? {
        if environment.isPeekParticle(.NO), let dictionary = environment.unwrappedPeek as? JpfDictionary {
            environment.drop()
            guard let pair = dictionary.pairs[key] else {return JpfNull.object}
            return pair.value                   // <辞書>の<索引(ハッシュキー)>
        }
        return nil
    }
    /// 範囲で配列にアクセスする。
    /// - Parameters:
    ///   - range: JpfRangeで表される範囲
    ///   - environment: 対象の配列を取得
    /// - Returns: 範囲の配列、またはエラー
    func accessed(by range: JpfRange, with environment: Environment) -> JpfObject? {
        if environment.isPeekParticle(.NO), let array = environment.unwrappedPeek as? JpfArray {
            environment.drop()
            guard let subArray = array[range] else {return JpfNull.object}
            return subArray                     // <配列>の<範囲>
        }
        return nil
    }
    var unusableAsIndex: JpfError       {JpfError("は、「配列」の要素の索引として使用できない。")}
    var unusableAsHashKey: JpfError     {JpfError("は、「辞書」の要素の索引(ハッシュキー)として使用できない。")}
}
// MARK: - ASTノードを拡張
extension Node {
    func accessed(with environment: Environment) -> JpfObject? {accessed(type: tokenLiteral, with: environment)}
}
extension IntegerLiteral : NodeAccessible {
    func accessed(with environment: Environment) -> JpfObject? {accessed(by: value, with: environment)}
}
extension StringLiteral : NodeAccessible {
    func accessed(with environment: Environment) -> JpfObject? {
        let hashKey = JpfHashKey(type: JpfString.type, value: value.hashValue)
        return accessed(by: hashKey, with: environment)
    }
}
extension Boolean : NodeAccessible {
    func accessed(with environment: Environment) -> JpfObject? {
        let hashKey = JpfHashKey(type: JpfBoolean.type, value: value.hashValue)
        return accessed(by: hashKey, with: environment)
    }
}
// MARK: - オブジェクトを拡張
extension JpfObject {
    func accessed(by name: String, with environment: Environment) -> JpfObject? {accessed(type: type, with: environment) ?? self}
}
extension JpfInteger : JpfObjectAccessible {
    func accessed(by neme: String, with environment: Environment) -> JpfObject? {accessed(by: value, with: environment) ?? self}
}
extension JpfString : JpfObjectAccessible {
    func accessed(by neme: String, with environment: Environment) -> JpfObject? {accessed(by: self.hashKey, with: environment) ?? self}
}
extension JpfBoolean : JpfObjectAccessible {
    func accessed(by name: String, with environment: Environment) -> JpfObject? {accessed(by: self.hashKey, with: environment) ?? self}
}
extension JpfRange : JpfObjectAccessible {
    func accessed(by name: String, with environment: Environment) -> JpfObject? {accessed(by: self, with: environment) ?? self
    }
}
extension JpfFunction : JpfObjectAccessible {
    func accessed(by name: String, with environment: Environment) -> JpfObject? {
        accessed(type: type, with: environment) ??  // 関数でオブジェクトにアクセスしている？→ Yesならエラー
        (name.isExecutable ? executed(with: environment) : self)
    }
}
extension JpfArray : JpfObjectAccessible {
    func accessed(by name: String, with environment: Environment) -> JpfObject? {
        accessed(type: type, with: environment) ??  // 関数配列でオブジェクトにアクセスしている？→ Yesならエラー
        (isExecutable(by: name) ? executed(with: environment) : self)
    }
    private func isExecutable(by name: String) -> Bool {name.isExecutable && elements.first is JpfFunction}
}
