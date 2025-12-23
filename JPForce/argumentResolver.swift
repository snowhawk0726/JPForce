//
//  argumentResolver.swift
//  述語の引数を解析
//  JPForce
//
//  Created by 佐藤貴之 on 2025/12/17.
//

import Foundation

// MARK: - protocol
protocol ArgumentResolver {
    associatedtype Spec
    func resolve(_ arguments: [JpfObject]) throws -> Spec
}
// MARK: - implements
// Common
// Errors
extension ArgumentResolver {
    var argumentsNotFound:      JpfError {JpfError("解析すべき引数がない。")}
    var countValueNotFouond:    JpfError {JpfError("個数の値が見つからない。")}
    var wrongValueMode:         JpfError {JpfError("変換は「値」または「数値」を指定。)")}
    var worngIdentifier:        JpfError {JpfError("識別子の指定が誤っている。")}
}
// MARK: - predicates
/// 得る(pull) / 写す(duplicate)
struct PullArgumentResolver: ArgumentResolver {
    func resolve(_ arguments: [JpfObject]) throws -> PullArgumentSpec {
        var resolvedCount = 0
        var lhs: [String] = []
        var valueMode = ValueMode.none
        var countSpec = CountSpec.one
        //
        guard !arguments.isEmpty else {
            return PullArgumentSpec(resolvedCount: 0, lhs: [], valueMode: .none, countSpec: .one)
        }
        let args = ArgumentIterator(arguments)
        var argument = args.next()
        // 個数指定
        if let arg = argument, arg.isParticle(.KO) {
            if let number = arg.number {
                countSpec = .count(number)      // 数値
            } else {
                guard let name = arg.value?.name, !name.isEmpty else {
                    throw countValueNotFouond
                }
                countSpec = .dynamic(name)      // 変数
            }
            resolvedCount += 1
            argument = args.next()
        }
        // 値変換指定
        if let arg = argument, arg.isParticle(.WO) {
            guard
                let string = arg.value as? JpfString,
                let mode = ValueMode(rawValue: string.value)
            else {
                throw wrongValueMode
            }
            valueMode = mode
            resolvedCount += 1
            argument = args.next()
        }
        // 代入識別子指定
        if let arg = argument, arg.isParticle(.NI) {
            repeat {
                guard
                    let ident = argument?.value as? JpfIdentifier, ident.isLhs,
                    !ident.name.isEmpty
                else {
                    throw worngIdentifier
                }
                lhs.insert(ident.name, at: 0)
                resolvedCount += 1
                argument = args.next()
            } while argument?.isParticle(.TO) ?? false
        }
        // 仕様作成
        let spec = PullArgumentSpec(
            resolvedCount: resolvedCount,
            lhs: lhs,
            valueMode: valueMode,
            countSpec: countSpec
        )
        // 引数検証
        try spec.validate()

        return spec
    }
}
private class ArgumentIterator {
    private let arguments: [JpfObject]
    private var index: Int
    init(_ arguments: [JpfObject]) {
        self.arguments = arguments
        self.index = arguments.count - 1
    }
    func next() -> JpfObject? {
        guard index >= 0 else {return nil}
        defer {index -= 1}
        return arguments[index]
    }
}
/// 得る/写すの引数仕様
/// (<識別子１>(と<識別子２>…)に)(<「数値」または「値」>を)(n個)]
struct PullArgumentSpec {
    let resolvedCount: Int
    let lhs: [String]
    let valueMode: ValueMode
    let countSpec: CountSpec
    //
    func validate() throws {
        if lhs.count > 1 && countSpec.count != 1 {
            throw multipleLhsWithArray
        }
    }
}
extension PullArgumentSpec {
    var multipleLhsWithArray:   JpfError {JpfError("スタック値の配列を複数の識別子に割り当てることはできない。")}
}
enum ValueMode : String {
    case value  = "値"
    case number = "数値"
    case none   = "無し"
}
enum CountSpec {
    case one                // 1個
    case count(Int)         // 数値個
    case dynamic(String)    // 識別子個
    //
    var count: Int? {
        switch self {
        case .count(let count): return count
        case .one:              return 1
        case .dynamic:          return nil
        }
    }
    var name: String? {
        if case .dynamic(let name) = self {
            return name
        }
        return nil
    }
}
