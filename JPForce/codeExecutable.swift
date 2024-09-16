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
    func binaryIntegerOperation(with vm:VM, operation: (Int, Int) -> Int) -> JpfError? {
        let right = vm.pull(), left = vm.pull()
        guard let leftValue = left.number, let rightValue = right.number else {
            return JpfError("二項数値演算の項が、数値でない。")
        }
        if let error = vm.push(JpfInteger(value: operation(leftValue, rightValue))) {return error}
        return nil
    }
    func binaryComparisonOperation(with vm:VM, operation: (JpfObject, JpfObject) -> Bool) -> JpfError? {
        let right = vm.pull(), left = vm.pull()
        if let error = vm.push(JpfBoolean(value: operation(left, right))) {return error}
        return nil
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
        case .opNot:    return NotExecuter(vm)
        case .opEqual:  return EqualExecuter(vm)
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
struct NotExecuter : CodeExecutable {
    init(_ vm: VM) {self.vm = vm}
    let vm: VM
    func execute() -> JpfError? {
        let object = vm.pull()
        if let error = vm.push(JpfBoolean.object(of: !object.isTrue)) {return error}
        return nil
    }
}
struct EqualExecuter : CodeExecutable {
    init(_ vm: VM) {self.vm = vm}
    let vm: VM
    func execute() -> JpfError? {
        return binaryComparisonOperation(with: vm) {$0.isEqual(to: $1)}
    }
}
