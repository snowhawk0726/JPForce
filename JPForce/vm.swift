//
//  vm.swift
//  JPForce
//
//  Created by 佐藤貴之 on 2024/06/21.
//

import Foundation

let stackSize = 2048
class VM {
    let constants: [JpfObject]
    let instructions: Instructions
    var stack: [JpfObject]
    var sp: Int // 次の値を示す。スタックトップは、stack[sp-1]
    init(with bytecode: Bytecode) {
        self.instructions = bytecode.instructions
        self.constants = bytecode.constants
        self.stack = [JpfObject](repeating: JpfNull.object, count: stackSize)
        self.sp = 0
    }
    var stackTop: JpfObject? {
        if sp == 0 {return nil}
        return stack[sp - 1]
    }
    var lastPoppedStackElem: JpfObject {
        return stack[sp]
    }
    //
    func run() -> JpfError? {
        var ip = 0  // instructionsの位置を示す。
        while ip < instructions.count {
            let opcode = Opcode(rawValue: instructions[ip])!
            switch opcode {
            case .opConstant:
                let constIndex = Int(readUInt16(from: Array(instructions[(ip+1)...])))
                ip += 2
                if let error = push(constants[constIndex]) {return error}
            case .opPop:
                _ = pull()
            case .opJump:
                let position = Int(readUInt16(from: Array(instructions[(ip+1)...])))
                ip = position - 1           // 飛び先
            case .opJumpNotTruthy:
                let position = Int(readUInt16(from: Array(instructions[(ip+1)...])))
                ip += 2
                let condition = pull()
                if !condition.isTrue {      // Not Truthy
                    ip = position - 1       // 飛び先
                }
            default:
                guard let executer = CodeExecutableFactory.create(from: opcode, with: self) else {return codeNotSupported + "(命令語：\(definitions[opcode]!.name))"}
                if let error = executer.execute() {return error}
            }
            ip += 1
        }
        return nil
    }
    func push(_ object: JpfObject) -> JpfError? {
        guard sp < stackSize else {
            return JpfError("stack overflow")
        }
        stack[sp] = object
        sp += 1
        return nil
    }
    func pull() -> JpfObject {
        guard sp > 0 else {
            return JpfError("stack underflow")
        }
        let object = stack[sp - 1]
        sp -= 1
        return object
    }
    func peek() -> JpfObject? {
        guard sp > 0 else {return nil}
        return stack[sp - 1]
    }
    func peek(_ n: Int) -> [JpfObject]? {
        guard n > 0 && n < (sp + 1) else {return nil}
        return Array(stack[0..<n])
    }
    func drop() {
        guard sp > 0 else {return}
        sp -= 1
    }
    func drop(_ n: Int) {
        guard sp >= n else {return}
        sp -= n
    }
    // エラー
    let codeNotSupported = JpfError("該当する命令語は、未実装。")
}
