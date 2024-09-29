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
    case opPop
    case opTrue
    case opFalse
    case opNull
    case opBe
    case opNot
    case opEqual
    case opGreaterThan
    case opLessThan
    case opJump
    case opJumpNotTruthy
    case opAdd
    case opSub
    case opMul
    case opDiv
    case opNeg
}
/// 命令定義
struct Definition {
    var name: String
    var operandWidths: [Int]
}
var definitions: [Opcode: Definition] = [
    .opConstant:    Definition(name: "OpConstant", operandWidths: [2]),
    .opPop:         Definition(name: "OpPop", operandWidths: []),
    .opTrue:        Definition(name: "OpTrue", operandWidths: []),
    .opFalse:       Definition(name: "OpFalse", operandWidths: []),
    .opNull:        Definition(name: "OpNull", operandWidths: []),
    .opBe:          Definition(name: "OpBe", operandWidths: []),
    .opNot:         Definition(name: "OpNot", operandWidths: []),
    .opEqual:       Definition(name: "OpEqual", operandWidths: []),
    .opGreaterThan: Definition(name: "OpGreaterThan", operandWidths: []),
    .opLessThan:    Definition(name: "OpLessThan", operandWidths: []),
    .opJump:        Definition(name: "OpJump", operandWidths: [2]),
    .opJumpNotTruthy:
                    Definition(name: "OpJumpNotTruthy", operandWidths: [2]),
    .opAdd:         Definition(name: "OpAdd", operandWidths: []),
    .opSub:         Definition(name: "OpSub", operandWidths: []),
    .opMul:         Definition(name: "OpMul", operandWidths: []),
    .opDiv:         Definition(name: "OpDiv", operandWidths: []),
    .opNeg:         Definition(name: "OpNeg", operandWidths: []),
]
// MARK: - implements for instruction
func lookUp(_ op: Byte) -> Definition? {
    guard let op = Opcode(rawValue: op) else {return nil}
    return definitions[op]
}
/// バイト列（インストラクション）を生成する。
/// - Parameters:
///   - op: オペコード
///   - operands: オペランド
/// - Returns: バイト列
func make(op: Opcode, operands: [Int] = []) -> [Byte] {
    guard let def = definitions[op] else {return []}
    let instrunctionLength = def.operandWidths.reduce(1) {$0 + $1}
    var instrunction = [Byte](repeating: 0, count: instrunctionLength)
    instrunction[0] = op.rawValue
    var offset = 1
    for (operand, operandWidth) in zip(operands, def.operandWidths) {
        switch operandWidth {
        case 2:
            let bytes = withUnsafeBytes(of: UInt16(operand).bigEndian) {Array($0)}
            instrunction.replaceSubrange(offset..<offset+2, with: bytes)
        default:
            break
        }
        offset += operandWidth
    }
    return instrunction
}
func make(op: Opcode, operand: Int) -> [Byte] {
    return make(op: op, operands: [operand])
}
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
}
extension Byte {
    var quoted: String {"\"" + String(format: "0x%02x", self) + "\""}
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
func readUInt16(from ins: Instructions) -> UInt16 {
    Data(ins).withUnsafeBytes {$0.load(as: UInt16.self)}.bigEndian
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
