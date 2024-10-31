//
//  code.swift
//  JPForce
//
//  Created by 佐藤貴之 on 2024/06/18.
//

import Foundation

// MARK: - definitions for instruction
typealias Byte = UInt8
typealias Instructions = [Byte]

/// 命令語
enum Opcode : Byte {
    case opConstant = 0
    case opSetGlobal
    case opGetGlobal
    case opSetLocal
    case opGetLocal
    case opPop              // 5 (未使用)
    case opPhrase           // 句を形成する
    case opTrue
    case opFalse
    case opNull
    case opArray            // 10
    case opDictionary
    case opGenitive
    case opJump
    case opJumpNotTruthy
    case opCall             // 15
    case opReturnValue
    case opReturn
    case opPredicate
    case opGetProperty
    //
    var definition: Definition {
        switch self {
        case .opConstant:       Definition(name: "OpConstant",      operandWidths: [2])
        case .opSetGlobal:      Definition(name: "OpSetGlobal",     operandWidths: [2])
        case .opGetGlobal:      Definition(name: "OpGetGlobal",     operandWidths: [2])
        case .opSetLocal:       Definition(name: "OpSetLocal",      operandWidths: [1])
        case .opGetLocal:       Definition(name: "OpGetLocal",      operandWidths: [1])
        case .opPop:            Definition(name: "OpPop",           operandWidths: [])
        case .opPhrase:         Definition(name: "OpPhrase",        operandWidths: [2])
        case .opTrue:           Definition(name: "OpTrue",          operandWidths: [])
        case .opFalse:          Definition(name: "OpFalse",         operandWidths: [])
        case .opNull:           Definition(name: "OpNull",          operandWidths: [])
        case .opArray:          Definition(name: "OpArray",         operandWidths: [2])
        case .opDictionary:     Definition(name: "OpDictionary",    operandWidths: [2])
        case .opGenitive:       Definition(name: "OpGenitive",      operandWidths: [])
        case .opJump:           Definition(name: "OpJump",          operandWidths: [2])
        case .opJumpNotTruthy:  Definition(name: "OpJumpNotTruthy", operandWidths: [2])
        case .opCall:           Definition(name: "OpCall",          operandWidths: [])
        case .opReturnValue:    Definition(name: "OpReturnValue",   operandWidths: [])
        case .opReturn:         Definition(name: "OpReturn",        operandWidths: [])
        case .opPredicate:      Definition(name: "OpPredicate",     operandWidths: [1])
        case .opGetProperty:    Definition(name: "OpGetProperty",   operandWidths: [1])
        }
    }
    var operandWidth: Int {self.definition.operandWidths.first ?? 0}    // オペランド長
}
/// 命令定義
struct Definition {
    var name: String
    var operandWidths: [Int]
}
// MARK: - implements for instruction
func lookUp(_ op: Byte) -> Definition? {
    Opcode(rawValue: op)?.definition
}
/// バイト列（インストラクション）を生成する。
/// - Parameters:
///   - op: オペコード
///   - operands: オペランド
/// - Returns: バイト列
func make(op: Opcode, operands: [Int] = []) -> [Byte] {
    let instrunctionLength = 1 + op.operandWidth
    var instrunction = [Byte](repeating: 0, count: instrunctionLength)
    instrunction[0] = op.rawValue
    var offset = 1
    for operand in operands {
        switch op.operandWidth {
        case 2:
            let bytes = withUnsafeBytes(of: UInt16(operand).bigEndian) {Array($0)}
            instrunction.replaceSubrange(offset..<offset+2, with: bytes)
        case 1:
            instrunction[offset] = Byte(operand)
        default:
            break
        }
        offset += op.operandWidth
    }
    return instrunction
}
func make(op: Opcode, operand: Int) -> [Byte] {
    return make(op: op, operands: [operand])
}
/// インストラクションからオペランドを読み込み、数値列と読み込んだバイト数を返す。
/// - Parameters:
///   - definition: 命令定義
///   - instruction: インストラクション
/// - Returns: 読み込んだオペランド([Int])と、バイト数(Int)
func readOperands(with def: Definition, from ins: Instructions) -> ([Int], Int) {
    var operands = [Int](repeating: 0, count: def.operandWidths.count)
    var offset = 0
    for (i, width) in def.operandWidths.enumerated() {
        switch width {
        case 2:
            operands[i] = Int(readUInt16(from: Array(ins[offset...])))
        case 1:
            operands[i] = Int(readUInt8(from: Array(ins[offset...])))
        default:
            break
        }
        offset += width
    }
    return (operands, offset)
}
/// インストラクションから、16ビットをビッグエンディアンで読み込む。
/// - Parameter ins: インストラクション
/// - Returns: ビッグエンディアンの16ビットデータ(UInt16)
func readUInt16(from ins: Instructions) -> UInt16 {Data(ins).withUnsafeBytes {$0.load(as: UInt16.self)}.bigEndian}
func readUInt8(from ins: Instructions) -> UInt8 {ins[0]}
//
extension Instructions {
    /// インストラクション(バイト列)を文字列形式に整形(逆アセンブル)する。
    var string: String {
        var s = ""
        var i = 0
        while i < self.count {
            guard let def = lookUp(self[i]) else {
                s = "Error: opcode \(self[i]) undefined."
                continue
            }
            let (operands, read) = readOperands(with: def, from: Array(self[(i+1)...]))
            s += String(format: "%04d %@\n", i, formattedInstruction(def, operands))
            i += 1 + read
        }
        return s
    }
    /// インストラクション(バイト列)を１６進表示する。
    var quoted: String {
        "\"" + (self.map {String(format: "0x%02x", $0)}).joined(separator: ", ") + "\""
    }
    /// 命令定義とオペランド列から、文字列形式を作成する。("命令語 オペランド")
    /// - Parameters:
    ///   - def: 命令定義
    ///   - operands: オペランド列
    /// - Returns: 整形された文字列
    private func formattedInstruction(_ def: Definition, _ operands: [Int]) -> String {
        let operandCount = def.operandWidths.count
        if operands.count != operandCount {
            return "Error: operand count \(operands.count) does not match defined \(operandCount)"
        }
        switch operandCount {
        case 0:
            return def.name
        case 1:
            return "\(def.name) \(operands[0])"
        default:
            return "Error: unhandled operandCount for \(def.name)"
        }
    }
}
extension Byte {
    var quoted: String {"\"" + String(format: "0x%02x", self) + "\""}
}
