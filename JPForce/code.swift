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
    case opClosure          // 20
    case opGetFree
    case opCurrentClosure
    //
    var definition: (name: String, operandWidths: [Int]) {
        switch self {
        case .opConstant:       (name: "OpConstant",      operandWidths: [2])   // 定数表位置
        case .opSetGlobal:      (name: "OpSetGlobal",     operandWidths: [2])   // 大域表位置
        case .opGetGlobal:      (name: "OpGetGlobal",     operandWidths: [2])   // 大域表位置
        case .opSetLocal:       (name: "OpSetLocal",      operandWidths: [1])   // 局所表位置
        case .opGetLocal:       (name: "OpGetLocal",      operandWidths: [1])   // 局所表位置
        case .opPop:            (name: "OpPop",           operandWidths: [])
        case .opPhrase:         (name: "OpPhrase",        operandWidths: [1])   // 格インデックス
        case .opTrue:           (name: "OpTrue",          operandWidths: [])
        case .opFalse:          (name: "OpFalse",         operandWidths: [])
        case .opNull:           (name: "OpNull",          operandWidths: [])
        case .opArray:          (name: "OpArray",         operandWidths: [2])   // 要素数
        case .opDictionary:     (name: "OpDictionary",    operandWidths: [2])   // 要素数 x ２
        case .opGenitive:       (name: "OpGenitive",      operandWidths: [])
        case .opJump:           (name: "OpJump",          operandWidths: [2])   // 飛び先
        case .opJumpNotTruthy:  (name: "OpJumpNotTruthy", operandWidths: [2])   // 飛び先
        case .opCall:           (name: "OpCall",          operandWidths: [])
        case .opReturnValue:    (name: "OpReturnValue",   operandWidths: [])
        case .opReturn:         (name: "OpReturn",        operandWidths: [])
        case .opPredicate:      (name: "OpPredicate",     operandWidths: [1])   // 述語インデックス
        case .opGetProperty:    (name: "OpGetProperty",   operandWidths: [1])   // 属性インデックス
        case .opClosure:        (name: "OpClosure",       operandWidths: [2, 1])// 関数インデックス、自由変数数
        case .opGetFree:        (name: "OpGetFree",       operandWidths: [1])   // 自由変数位置
        case .opCurrentClosure: (name: "OpCurrentClosure",operandWidths: [])
        }
    }
    var name: String {definition.name}                  // オペコード名
    var operandWidths: [Int] {definition.operandWidths} // オペランド長の配列
    var operandWidth: Int {operandWidths.reduce(0, +)}  // 総オペランド長
    /// バイト列からオペコードに応じたオペランドを読み込み、オペランドの配列と読み込んだバイト数を返す。
    /// - Parameter bytes: オペランドのバイト列
    /// - Returns: オペランドの配列と読み込んだバイト数
    func readOperand(from bytes: [Byte]) -> ([Int], Int) {
        var operands = [Int](repeating: 0, count: operandWidths.count)
        var offset = 0
        for (i, width) in operandWidths.enumerated() {
            switch width {
            case 2: operands[i] = Int(readUInt16(from: Array(bytes[offset...])))
            case 1: operands[i] = Int(readUInt8(from: Array(bytes[offset...])))
            default:
                break
            }
            offset += width
        }
        return (operands, offset)
    }
}
// MARK: - implements for instruction
/// インストラクションを生成する。
/// - Parameters:
///   - op: オペコード
///   - operands: オペランド
/// - Returns: インストラクション(命令語)
func make(op: Opcode, operands: [Int]) -> Instruction {
    var instrunction = Instruction(repeating: 0, count: 1 + op.operandWidth)
    instrunction[0] = op.rawValue
    var offset = 1
    for (operand, width) in zip(operands, op.operandWidths) {
        switch width {    // オペランド長(バイト数)
        case 2:
            let bytes = withUnsafeBytes(of: UInt16(operand).bigEndian) {Array($0)}
            instrunction.replaceSubrange(offset..<offset+2, with: bytes)
        case 1:
            instrunction[offset] = Byte(operand)
        default:
            break
        }
        offset += width
    }
    return instrunction
}
func make(op: Opcode, operand: Int...) -> Instruction {
    return make(op: op, operands: operand)
}
func make(predicate keyword: Token.Keyword) -> Instruction {
    make(op: .opPredicate, operand: PredicateOperableFactory.index(of: keyword)!)
}
func make(phraseWith particle: Token.Particle) -> Instruction {
    make(op: .opPhrase, operand: Token(particle).particleIndex!)
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
        disassemble {op, operands in formattedInstruction(op, operands)}
    }
    func disassemble(with constants: [JpfObject], _ symbolTable: SymbolTable) -> String {
        disassemble {op, operands in formattedInstruction(op, operands, with: constants, symbolTable)}
    }
    private func disassemble(formatter: (Opcode, [Int]) -> String) -> String {
        var s = "", i = 0
        while i < self.count {
            guard let op = Opcode(rawValue: bytes[i]) else {
                s = "Error: opcode \(self[i]) undefined.\n"
                i += 1
                continue
            }
            let (operands, read) = op.readOperand(from: Array(bytes[(i+1)...]))
            s += String(format: "%04d %@\n", i, formatter(op, operands))
            i += 1 + read
        }
        return s
    }
    /// バイト列を「"」で囲った１６進２桁表示する。
    var quoted: String {
        "\"" + (bytes.map {String(format: "0x%02x", $0)}).joined(separator: ", ") + "\""
    }
    /// バイト列を１０進２桁表示する。
    var decimalStrings: String {bytes.map {String(format: "%02d", $0)}.joined(separator: " ")}
    /// オペコードとオペランド列から、文字列形式のインストラクションを作成する。
    /// - Parameters:
    ///   - op: オペコード
    ///   - operands: オペランド列
    /// - Returns: 整形された文字列
    private func formattedInstruction(_ op: Opcode, _ operands: [Int]) -> String {
        let operandCount = op.operandWidths.count   // 定義されているオペランド数
        if operands.count != operandCount {         // 入力のオペランド数
            return "Error: operand count \(operands.count) does not match defined \(operandCount)"
        }
        switch operandCount {
        case 0:     return op.name
        case 1:     return "\(op.name) \(operands[0])"
        case 2:     return "\(op.name) \(operands[0]), \(operands[1])"
        default:    return "Error: unhandled operandCount for \(op.name)"
        }
    }
    /// オペランド(インデックス)に、シンボルテーブルと定数表の情報を付加した文字列形式を返す。
    private func formattedInstruction(_ op: Opcode, _ operands: [Int], with constants: [JpfObject], _ symbolTable: SymbolTable) -> String {
        guard let index = operands.first else {
            return formattedInstruction(op, operands)
        }
        var s = ""
        switch op {
        case .opConstant, .opClosure:
            s = constants[index].formattedString                            // 定数のオブジェクト
        case .opPhrase:
            s = Token.particles[index].rawValue + "(格)"                     // 格
        case .opGetGlobal, .opSetGlobal:
            s = (symbolTable[index] ?? "??") + "(識別子名)"                  // 識別子名
        case .opGetLocal, .opSetLocal, .opGetFree:
            s = "??(識別子名)"                                               // 識別子名
        case .opGetProperty:
            s = ObjectProperties().names[index]                             // 属性名
        case .opPredicate:
            s = PredicateOperableFactory.predicates[index].keyword.rawValue // 述語名
        default :
            return formattedInstruction(op, operands)
        }
        return formattedInstruction(op, operands) + (op == .opPhrase ? "\t\t: " :  "\t: ") + s
    }
}
extension Byte {
    var quoted: String {"\"" + String(format: "0x%02x", self) + "\""}
}
