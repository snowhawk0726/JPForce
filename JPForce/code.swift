//
//  code.swift
//  JPForce
//
//  Created by 佐藤貴之 on 2024/06/18.
//

import Foundation

// MARK: - definitions for instruction
typealias Byte = UInt8          // 1バイト
typealias Instruction = [Byte]  // 命令語

/// 命令語(オペコードとオペランド)
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
    var name: String {definition.name}                  // オペコード名
    var operandWidths: [Int] {definition.operandWidths} // オペランド長の配列
    var operandWidth: Int {operandWidths.first ?? 0}    // 第一オペランド長
    /// バイト列からオペコードに応じたオペランドを読み込み、オペランドの配列と読み込んだバイト数を返す。
    /// - Parameter bytes: オペランドのバイト列
    /// - Returns: オペランドの配列と読み込んだバイト数
    func readOperands(with bytes: [Byte]) -> ([Int], Int) {
        var operands = [Int](repeating: 0, count: operandWidths.count)
        var offset = 0
        for (i, width) in operandWidths.enumerated() {
            switch width {
            case 2:
                operands[i] = Int(readUInt16(from: Array(bytes[offset...])))
            case 1:
                operands[i] = Int(readUInt8(from: Array(bytes[offset...])))
            default:
                break
            }
            offset += width
        }
        return (operands, offset)
    }
}
/// 命令語(インストラクション)定義
struct Definition {
    var name: String            // オペコード名
    var operandWidths: [Int]    // 複数オペランド(インデックス)
}
// MARK: - implements for instruction
func lookUp(_ op: Byte) -> Definition? {
    Opcode(rawValue: op)?.definition
}
/// インストラクションを生成する。
/// - Parameters:
///   - op: オペコード
///   - operands: オペランド
/// - Returns: インストラクション(命令語)
func make(op: Opcode, operands: [Int] = []) -> Instruction {
    let instrunctionLength = 1 + op.operandWidth
    var instrunction = Instruction(repeating: 0, count: instrunctionLength)
    instrunction[0] = op.rawValue
    var offset = 1
    for operand in operands {
        switch op.operandWidth {    // オペランド長(バイト数)
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
func make(op: Opcode, operand: Int) -> Instruction {
    return make(op: op, operands: [operand])
}
/// バイト列から、16/8ビットをビッグエンディアンで読み込む。
/// - Parameter bytes: バイト列
/// - Returns: ビッグエンディアンの16ビットデータ(UInt16)
func readUInt16(from bytes: [Byte]) -> UInt16 {Data(bytes).withUnsafeBytes {$0.load(as: UInt16.self)}.bigEndian}
func readUInt8(from bytes: [Byte]) -> UInt8 {bytes[0]}
//
struct Instructions : ExpressibleByArrayLiteral {
    init(arrayLiteral elements: Byte...) {self.bytes = elements}
    init(_ bytes: [Byte]) {self.bytes = bytes}
    init(_ bytes: [Instruction]) {self.bytes = bytes.flatMap(\.self)}   // インストラクション列から生成
    //
    var bytes: [Byte] = []
    //
    var count: Int {bytes.count}
    subscript(index: Int) -> Byte {
        get {bytes[index]}
        set {bytes[index] = newValue}
    }
    // 範囲型による呼び出し
    subscript(range: Range<Int>) -> Self {
        get {Self(Array(bytes[range]))}
        set {bytes.replaceSubrange(range, with: newValue.bytes)}
    }
    subscript(range: ClosedRange<Int>) -> Self {
        get {Self(Array(bytes[range]))}
        set {bytes.replaceSubrange(range, with: newValue.bytes)}
    }
    subscript(range: PartialRangeFrom<Int>) -> Self {
        get {Self(Array(bytes[range]))}
        set {bytes.replaceSubrange(range, with: newValue.bytes)}
    }
    /// インストラクション(バイト列)を文字列形式に整形(逆アセンブル)する。
    var string: String {
        var s = ""
        var i = 0
        while i < self.count {
            guard let op = Opcode(rawValue: bytes[i]) else {
                s = "Error: opcode \(self[i]) undefined."
                continue
            }
            let (operands, read) = op.readOperands(with: Array(bytes[(i+1)...]))
            s += String(format: "%04d %@\n", i, formattedInstruction(op.definition, operands))
            i += 1 + read
        }
        return s
    }
    func disassemble(with constants: [JpfObject], _ symbolTable: SymbolTable) -> String {
        var s = ""
        var i = 0
        while i < self.count {
            guard let op = Opcode(rawValue: bytes[i]) else {
                s = "Error: opcode \(self[i]) undefined."
                continue
            }
            let (operands, read) = op.readOperands(with: Array(bytes[(i+1)...]))
            s += String(format: "%04d %@\n", i, formattedInstruction(op, operands, with: constants, symbolTable))
            i += 1 + read
        }
        return s
    }
    /// インストラクション(バイト列)を１６進表示する。
    var quoted: String {
        "\"" + (bytes.map {String(format: "0x%02x", $0)}).joined(separator: ", ") + "\""
    }
    var decimalStrings: String {bytes.map {String(format: "%02d", $0)}.joined(separator: " ")}
    /// 命令定義とオペランド列から、文字列形式を作成する。("命令語 オペランド")
    /// - Parameters:
    ///   - def: 命令定義
    ///   - operands: オペランド列
    /// - Returns: 整形された文字列
    private func formattedInstruction(_ def: Definition, _ operands: [Int]) -> String {
        let operandCount = def.operandWidths.count  // 定義されているオペランド数
        if operands.count != operandCount {         // 入力のオペランド数
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
    /// オペランド(インデックス)に、シンボルテーブルと定数表の情報を付加した文字列形式を返す。
    private func formattedInstruction(_ op: Opcode, _ operands: [Int], with constants: [JpfObject], _ symbolTable: SymbolTable) -> String {
        guard let index = operands.first else {
            return formattedInstruction(op.definition, operands)
        }
        var s = ""
        switch op {
        case .opConstant, .opPhrase:
            s = constants[index].formattedString                            // 定数のオブジェクト
        case .opGetGlobal, .opSetGlobal, .opGetLocal, .opSetLocal:
            s = (symbolTable[index] ?? "??") + "(識別子名)"
        case .opGetProperty:
            s = ObjectProperties().names[index]                             // 属性名
        case .opPredicate:
            s = PredicateOperableFactory.predicates[index].keyword.rawValue // 述語名
        default :
            return formattedInstruction(op.definition, operands)
        }
        return formattedInstruction(op.definition, operands) + "\t: " + s
    }
}
extension Byte {
    var quoted: String {"\"" + String(format: "0x%02x", self) + "\""}
}
