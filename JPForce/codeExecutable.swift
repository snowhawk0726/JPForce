//
//  codeExecutable.swift
//  JPForce
//
//  Created by 佐藤貴之 on 2024/09/08.
//

import Foundation

// MARK: - intefaces for execution
protocol CodeExecutable {
    var vm: VM {get}
    /// バイトコードを実行する。
    /// - Returns: 正常時はnil
    func execute() -> JpfError?
}
// MARK: - implementations for CodeExecutable
extension CodeExecutable {
    func binaryIntegerOperation(with vm: VM, operation: (Int, Int) -> Int) -> JpfError? {
        let right = vm.pull(), left = vm.pull()
        guard let leftValue = left.number, let rightValue = right.number else {
            return JpfError("二項数値演算の項が、数値でない。")
        }
        let value = operation(leftValue, rightValue)
        if let error = vm.push(JpfInteger(value: value)) {return error}
        return nil
    }
    func binaryIntegerComparison(with vm: VM, operation: (Int, Int) -> Bool) -> JpfError? {
        let right = vm.pull(), left = vm.pull()
        guard let leftValue = left.number, let rightValue = right.number else {
            return JpfError("二項数値演算の項が、数値でない。")
        }
        let value = operation(leftValue, rightValue)
        if let error = vm.push(JpfBoolean.object(of: value)) {return error}
        return nil
    }
    func binaryObjectComparison(with vm: VM, operation: (JpfObject, JpfObject) -> Bool) -> JpfError? {
        let right = vm.pull(), left = vm.pull()
        let value = operation(left, right)
        if let error = vm.push(JpfBoolean.object(of: value)) {return error}
        return nil
    }
    func logicalTest(with vm: VM, negative: Bool = false) -> JpfError? {
        if let params = vm.peek(2) {
            switch (params[0].particle, params[1].particle) {
            case (.particle(.GA), .particle(.DE)),
                 (nil, .particle(.DE)),
                 (nil, nil):
                vm.drop(2)
                let value = params[0].isEqual(to: params[1]) != negative
                if let err = vm.push(JpfBoolean.object(of: value)) {return err}
                return nil
            default:
                break
            }
        } else
        if let param = vm.peek() {
            if param.isParticle(.DE) || param.particle == nil {
                vm.drop()
                let value = param.isTrue != negative
                if let err = vm.push(JpfBoolean.object(of: value)) {return err}
                return nil
            }
        }
        return JpfError("対象となる句(オブジェクト)が無い。")
    }
}
// MARK: - instance factory
struct CodeExecutableFactory {
    static func create(from code: Opcode, with vm: VM) -> CodeExecutable? {
        switch code {
        case .opAdd:    return AddExecuter(vm)
        case .opSub:    return SubExecuter(vm)
        case .opMul:    return MulExecuter(vm)
        case .opDiv:    return DivExecuter(vm)
        case .opTrue,.opFalse:
                        return BooleanExecuter(vm, by: code)
        case .opNull:   return NullExecuter(vm)
        case .opNeg:    return NegExecuter(vm)
        case .opBe:     return BeExecuter(vm)
        case .opNot:    return NotExecuter(vm)
        case .opEqual:  return EqualExecuter(vm)
        case .opGreaterThan:
                        return GreaterThanExecuter(vm)
        case .opLessThan:
                        return LessThanExecuter(vm)
        default:        return nil
        }
    }
}
// MARK: - 算術演算
struct AddExecuter : CodeExecutable {
    init(_ vm: VM) {self.vm = vm}
    let vm: VM
    func execute() -> JpfError? {
        return binaryIntegerOperation(with: vm) {$0 + $1}
    }
}
struct SubExecuter : CodeExecutable {
    init(_ vm: VM) {self.vm = vm}
    let vm: VM
    func execute() -> JpfError? {
        return binaryIntegerOperation(with: vm) {$0 - $1}
    }
}
struct MulExecuter : CodeExecutable {
    init(_ vm: VM) {self.vm = vm}
    let vm: VM
    func execute() -> JpfError? {
        return binaryIntegerOperation(with: vm) {$0 * $1}
    }
}
struct DivExecuter : CodeExecutable {
    init(_ vm: VM) {self.vm = vm}
    let vm: VM
    func execute() -> JpfError? {
        return binaryIntegerOperation(with: vm) {$0 / $1}
    }
}
struct NegExecuter : CodeExecutable {
    init(_ vm: VM) {self.vm = vm}
    let vm: VM
    func execute() -> JpfError? {
        let value = vm.pull()
        guard let number = value.number else {return JpfError("対象が、数値でない。")}
        if let err = vm.push(JpfInteger(name: value.name, value: -number)) {return err}
        return nil
    }
}
struct BeExecuter : CodeExecutable {
    init(_ vm: VM) {self.vm = vm}
    let vm: VM
    func execute() -> JpfError? {
        return logicalTest(with: vm, negative: false)
    }
}
struct NotExecuter : CodeExecutable {
    init(_ vm: VM) {self.vm = vm}
    let vm: VM
    func execute() -> JpfError? {
        return logicalTest(with: vm, negative: true)
    }
}
struct EqualExecuter : CodeExecutable {
    init(_ vm: VM) {self.vm = vm}
    let vm: VM
    func execute() -> JpfError? {
        return binaryObjectComparison(with: vm) {$0.isEqual(to: $1)}
    }
}
struct GreaterThanExecuter : CodeExecutable {
    init(_ vm: VM) {self.vm = vm}
    let vm: VM
    func execute() -> JpfError? {
        return binaryIntegerComparison(with: vm) {$0 > $1}
    }
}
struct LessThanExecuter : CodeExecutable {
    init(_ vm: VM) {self.vm = vm}
    let vm: VM
    func execute() -> JpfError? {
        return binaryIntegerComparison(with: vm) {$0 < $1}
    }
}
struct BooleanExecuter : CodeExecutable {
    init(_ vm: VM, by code: Opcode) {self.vm = vm; self.code = code}
    let vm: VM, code: Opcode
    func execute() -> JpfError? {
        if let error = vm.push(JpfBoolean.object(of: code == .opTrue)) {return error}
        return nil
    }
}
struct NullExecuter : CodeExecutable {
    init(_ vm: VM) {self.vm = vm}
    let vm: VM
    func execute() -> JpfError? {
        if let error = vm.push(JpfNull.object) {return error}
        return nil
    }
}
