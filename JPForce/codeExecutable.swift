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
    // エラー
    var genitiveParamError: JpfError    {JpfError("属格の左右オブジェクトが足りない。")}
    var callingFunctionNotFound: JpfError {JpfError("呼び出し先の関数が無い。")}
    var returnValueNotFound: JpfError   {JpfError("返すべき値が無い。")}
}
// MARK: - instance factory
struct CodeExecutableFactory {
    static func create(from code: Opcode, with vm: VM) -> CodeExecutable? {
        switch code {
        case .opTrue,.opFalse:
                        return BooleanExecuter(vm, by: code)
        case .opNull:   return NullExecuter(vm)
        case .opGenitive:
                        return GenitiveExecuter(vm)
        case .opCall:   return CallExecuter(vm)
        case .opReturnValue:
                        return ReturnValueExecuter(vm)
        case .opReturn: return ReturnExecuter(vm)
        default:        return nil
        }
    }
}
// MARK: - implementation for executers
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
struct GenitiveExecuter : CodeExecutable {
    init(_ vm: VM) {self.vm = vm}
    let vm: VM
    func execute() -> JpfError? {
        guard let index = vm.pull(), let left = vm.pull() else {return genitiveParamError}
        let environment = Environment(with: vm.stack)
        guard let result = left.accessed(by: index, with: environment) else {return nil}
        if result.isError {return result.error}
        if let error = vm.push(result) {return error}
        return nil
    }
}
struct CallExecuter : CodeExecutable {
    init(_ vm: VM) {self.vm = vm}
    let vm: VM
    func execute() -> JpfError? {
        guard let function = vm.pull()?.value as? JpfCompiledFunction else {return callingFunctionNotFound}
        let arguments = vm.peekAll()                        // 引数候補
        // TODO: - 引数をチェックし、実行する関数を特定し、Frameに割り当てる。
        guard arguments.count >= function.numberOfParameters else {
            return InputFormatError.numberOfParameters(function.numberOfParameters).message
        }
        vm.drop(function.numberOfParameters)
        let frame = Frame(with: function, basePointer: vm.sp)
        vm.push(frame)
        // TODO: - 助詞をチェックし、引数をローカル変数に割り当てる。
        if let error = vm.push(Array(arguments.suffix(function.numberOfParameters)).map {$0.value!}) {return error}
        vm.sp = frame.basePointer + function.numberOfLocals // ローカル変数のスロットを確保
        return nil
    }
}
struct ReturnValueExecuter : CodeExecutable {
    init(_ vm: VM) {self.vm = vm}
    let vm: VM
    func execute() -> JpfError? {
        guard vm.sp > vm.currentFrame.sp,                   // spがローカル変数スロット外
              let returnValue = vm.pull() else {return returnValueNotFound}
        if let frame = vm.popFrame() {
            vm.movePushed(from: frame.sp, to: frame.basePointer)
        }
        if let error = vm.push(returnValue) {return error}
        return nil
    }
}
struct ReturnExecuter : CodeExecutable {
    init(_ vm: VM) {self.vm = vm}
    let vm: VM
    func execute() -> JpfError? {
        if let frame = vm.popFrame() {
            vm.movePushed(from: frame.sp, to: frame.basePointer)
        }
        return nil
    }
}
