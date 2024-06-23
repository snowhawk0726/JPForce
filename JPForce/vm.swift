//
//  vm.swift
//  JPForce
//
//  Created by 佐藤貴之 on 2024/06/21.
//

import Foundation

let stackSize = 2048
struct VM {
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
    //
    mutating func run() -> JpfError? {
        var ip = 0
        while ip < instructions.count {
            switch Opcode(rawValue: instructions[ip]) {
            case .opConstant:
                let constIndex = Int(readUInt16(from: Array(instructions[(ip+1)...])))
                ip += 2
                if let error = push(constants[constIndex]) {return error}
            case .opAdd:
                let right = pull(), left = pull()
                guard let leftValue = left.number, let rightValue = right.number else {
                    return JpfError("opAdd: 足す数が無い。")
                }
                if let error = push(JpfInteger(value: leftValue + rightValue)) {return error}
            default:
                break
            }
            ip += 1
        }
        return nil
    }
    private mutating func push(_ object: JpfObject) -> JpfError? {
        if sp >= stackSize {
            return JpfError("stack overflow")
        }
        stack[sp] = object
        sp += 1
        return nil
    }
    private mutating func pull() -> JpfObject {
        let object = stack[sp - 1]
        sp -= 1
        return object
    }
}
