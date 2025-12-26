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
    func execute() throws
}
// MARK: - implementations for CodeExecutable
extension CodeExecutable {
    // エラー
    func unknownError(with error: Error) -> JpfError {JpfError("不明なエラーが発生。\(error)")}
    var genitiveParamError: JpfError        {JpfError("属格の左右オブジェクトが足りない。")}
    var callingFunctionNotFound: JpfError   {JpfError("呼び出し先の関数が無い。")}
    var returnValueNotFound: JpfError       {JpfError("返すべき値が無い。")}
    var propertyNotFound: JpfError          {JpfError("指定した属性が見つからない。")}
    var objectNotFound: JpfError            {JpfError("対象のオブジェクトが見つからない。")}
    var failedToGetProperty: JpfError       {JpfError("属性の取得に失敗した。")}
    var failedToGetFunction: JpfError       {JpfError("翻訳済み関数の取得に失敗した。")}
    var failedToGetClosure: JpfError        {JpfError("クロージャの取得に失敗した。")}
    var failedToGetFreeVariables: JpfError  {JpfError("自由変数の取得に失敗した。")}
    var cannotFoundPredicate: JpfError      {JpfError("述語が定義されていない。")}
    var stackValueIsNotNumber: JpfError     {JpfError("スタックの値が数値でない。")}
    func dictionaryKeyNotHashable(_ key: JpfObject) -> JpfError {
        JpfError("辞書のキー「\(key)」がハッシュ可能な型でない。")}
}
// MARK: - instance factory
struct CodeExecutableFactory {
    static func create(from op: Opcode, operandBytes: [Byte], with vm: VM) -> CodeExecutable {
        switch op {
        case .opTrue,.opFalse:  return BooleanExecuter(vm, by: op)
        case .opNull:           return NullExecuter(vm)
        case .opArrayConst:     return ArrayConstExecuter(vm, with: operandBytes)
        case .opArray:          return ArrayStackExecuter(vm)
        case .opDictionaryConst:return DictionaryConstExecuter(vm, with: operandBytes)
        case .opGenitive:       return GenitiveExecuter(vm)
        case .opJump, .opJumpNotTruthy:
                                return JumpExecuter(vm, by: op, with: operandBytes)
        case .opCall:           return CallExecuter(vm)
        case .opReturnValue:    return ReturnValueExecuter(vm)
        case .opReturn:         return ReturnExecuter(vm)
        case .opConstant:       return ConstantExecuter(vm, with: operandBytes)
        case .opPhrase:         return PhraseExecuter(vm, with: operandBytes)
        case .opPop:            return PopExecuter(vm)
        case .opSetGlobal:      return SetGlobalExecuter(vm, with: operandBytes)
        case .opGetGlobal:      return GetGlobalExecuter(vm, with: operandBytes)
        case .opSetLocal:       return SetLocalExecuter(vm, with: operandBytes)
        case .opGetLocal:       return GetLocalExecuter(vm, with: operandBytes)
        case .opGetProperty:    return GetPropertyExecuter(vm, with: operandBytes)
        case .opPredicate:      return PredicateExecuter(vm, with: operandBytes)
        case .opClosure:        return ClosureExecuter(vm, with: operandBytes)
        case .opGetFree:        return GetFreeExecuter(vm, with: operandBytes)
        case .opCurrentClosure: return CurrentClosureExecuter(vm)
        case .opPullConst:      return PullConstExecuter(vm, with: operandBytes)
        case .opPull:           return PullStackExecuter(vm)
        case .opDuplicateConst: return DuplicateConstExecuter(vm, with: operandBytes)
        case .opDuplicate:      return DuplicateStackExecuter(vm)
        case .opDropConst:      return DropConstExecuter(vm, with: operandBytes)
        case .opDrop:           return DropStackExecuter(vm)
        case .opMapProperty:    return MapPropertyExecuter(vm, with: operandBytes)
        }
    }
}
// MARK: - implementation for executers
struct BooleanExecuter : CodeExecutable {
    init(_ vm: VM, by op: Opcode) {self.vm = vm; self.op = op}
    let vm: VM, op: Opcode
    func execute() throws {
        try vm.push(JpfBoolean.object(of: op == .opTrue))
    }
}
struct NullExecuter : CodeExecutable {
    init(_ vm: VM) {self.vm = vm}
    let vm: VM
    func execute() throws {
        try vm.push(JpfNull.object)
    }
}
struct ArrayConstExecuter : CodeExecutable {
    init(_ vm: VM, with bytes: [Byte]) {self.vm = vm; self.bytes = bytes}
    let vm: VM, bytes: [Byte]
    func execute() throws {
        let numberOfElements = Int(readUInt16(from: bytes))
        vm.currentFrame.advanceIp(by: 2)
        let array = try buildArray(with: numberOfElements)
        try vm.push(array)
    }
}
struct ArrayStackExecuter : CodeExecutable {
    init(_ vm: VM) {self.vm = vm}
    let vm: VM
    func execute() throws {
        guard let numberOfElements = vm.pull()?.number else {
            throw stackValueIsNotNumber
        }
        let array = try buildArray(with: numberOfElements)
        try vm.push(array)
    }
}
private extension CodeExecutable {
    func buildArray(with n: Int) throws -> JpfArray {
        guard let objects = vm.peek(n) else {throw notEnoughStackValues}
        vm.drop(n)
        return JpfArray(elements: objects)
    }
}
struct DictionaryConstExecuter : CodeExecutable {
    init(_ vm: VM, with bytes: [Byte]) {self.vm = vm; self.bytes = bytes}
    let vm: VM, bytes: [Byte]
    func execute() throws {
        let numberOfElements = Int(readUInt16(from: bytes))
        vm.currentFrame.advanceIp(by: 2)
        let dictionary = try buildDictionary(with: numberOfElements)
        try vm.push(dictionary)
    }
}
private extension DictionaryConstExecuter {
    func buildDictionary(with n: Int) throws -> JpfDictionary {
        var pairs: [JpfHashKey: (key: JpfObject, value: JpfObject)] = [:]
        guard let objects = vm.peek(n) else {throw notEnoughStackValues}
        vm.drop(n)
        for i in stride(from: 0, to: n, by: 2) {
            let key = objects[i], value = objects[i+1]
            guard let hash = key as? JpfHashable else {throw dictionaryKeyNotHashable(key)}
            pairs[hash.hashKey] = (key, value)
        }
        return JpfDictionary(pairs: pairs)
    }
}
struct GenitiveExecuter : CodeExecutable {
    init(_ vm: VM) {self.vm = vm}
    let vm: VM
    func execute() throws {
        guard let index = vm.pull(), let left = vm.pull() else {throw genitiveParamError}
        let environment = Environment(with: vm.stack)
        guard let result = left.accesse(by: index, with: environment) else {return}
        if result.isError {throw result.error!}
        try vm.push(result)
    }
}
struct CallExecuter : CodeExecutable {
    init(_ vm: VM) {self.vm = vm}
    let vm: VM
    func execute() throws {
        guard let closure = vm.pull()?.value as? JpfClosure,
              let function = closure.fn else {throw callingFunctionNotFound}
        let arguments = vm.peekAll()                        // 引数候補
        // TODO: - 引数をチェックし、実行する関数を特定し、Frameに割り当てる。
        guard arguments.count >= function.numberOfParameters else {
            throw InputFormatError.numberOfParameters(function.numberOfParameters).message
        }
        vm.drop(function.numberOfParameters)
        let frame = Frame(with: closure, basePointer: vm.sp)
        vm.push(frame)
        // TODO: - 助詞をチェックし、引数をローカル変数に割り当てる。
        try vm.push(Array(arguments.suffix(function.numberOfParameters)).map {$0.value!})
        vm.sp = frame.basePointer + function.numberOfLocals // ローカル変数のスロットを確保
    }
}
struct ReturnValueExecuter : CodeExecutable {
    init(_ vm: VM) {self.vm = vm}
    let vm: VM
    func execute() throws {
        guard vm.sp > vm.currentFrame.sp,                   // spがローカル変数スロット外
              let returnValue = vm.pull() else {throw returnValueNotFound}
        if let frame = vm.popFrame() {
            vm.movePushed(from: frame.sp, to: frame.basePointer)
        }
        try vm.push(returnValue)
    }
}
struct ReturnExecuter : CodeExecutable {
    init(_ vm: VM) {self.vm = vm}
    let vm: VM
    func execute() throws {
        if let frame = vm.popFrame() {
            vm.movePushed(from: frame.sp, to: frame.basePointer)
        }
    }
}
struct JumpExecuter : CodeExecutable {
    init(_ vm: VM, by op: Opcode, with bytes: [Byte]) {self.vm = vm; self.op = op; self.bytes = bytes}
    let vm: VM, op: Opcode, bytes: [Byte]
    func execute() throws {
        let position = Int(readUInt16(from: bytes))
        vm.currentFrame.advanceIp(by: 2)
        if op == .opJump || isNotTruthy {
            vm.currentFrame.setIp(to: position - 1) // 飛び先にjump
        }
    }
    var isNotTruthy: Bool {
        if let condition = vm.pull() {return !condition.isTrue}
        return false
    }
}
struct ConstantExecuter : CodeExecutable {
    init(_ vm: VM, with bytes: [Byte]) {self.vm = vm; self.bytes = bytes}
    let vm: VM, bytes: [Byte]
    func execute() throws {
        let constIndex = Int(readUInt16(from: bytes))
        vm.currentFrame.advanceIp(by: 2)
        try vm.push(vm.constants[constIndex])
    }
}
struct PhraseExecuter : CodeExecutable {
    init(_ vm: VM, with bytes: [Byte]) {self.vm = vm; self.bytes = bytes}
    let vm: VM, bytes: [Byte]
    func execute() throws {
        let particleIndex = Int(readUInt8(from: bytes))
        vm.currentFrame.advanceIp(by: 1)
        if let value = vm.pull() {
            let p = Token.particles[particleIndex]
            let phrase = JpfPhrase(value: value, particle: Token(p))
            try vm.push(phrase)
        }
    }
}
struct PopExecuter : CodeExecutable {
    init(_ vm: VM) {self.vm = vm}
    let vm: VM
    func execute() throws {
        vm.drop()
    }
}
struct SetGlobalExecuter : CodeExecutable {
    init(_ vm: VM, with bytes: [Byte]) {self.vm = vm; self.bytes = bytes}
    let vm: VM, bytes: [Byte]
    func execute() throws {
        let globalIndex = Int(readUInt16(from: bytes))
        vm.currentFrame.advanceIp(by: 2)
        guard let result = vm.pull() else {throw notEnoughStackValues}
        vm.globals[globalIndex] = result
    }
}
struct GetGlobalExecuter : CodeExecutable {
    init(_ vm: VM, with bytes: [Byte]) {self.vm = vm; self.bytes = bytes}
    let vm: VM, bytes: [Byte]
    func execute() throws {
        let globalIndex = Int(readUInt16(from: bytes))
        vm.currentFrame.advanceIp(by: 2)
        try vm.push(vm.globals[globalIndex])
    }
}
struct SetLocalExecuter : CodeExecutable {
    init(_ vm: VM, with bytes: [Byte]) {self.vm = vm; self.bytes = bytes}
    let vm: VM, bytes: [Byte]
    func execute() throws {
        let localIndex = Int(readUInt8(from: bytes))
        vm.currentFrame.advanceIp(by: 1)
        vm.stack[vm.currentFrame.basePointer + localIndex] = vm.pull()
    }
}
struct GetLocalExecuter : CodeExecutable {
    init(_ vm: VM, with bytes: [Byte]) {self.vm = vm; self.bytes = bytes}
    let vm: VM, bytes: [Byte]
    func execute() throws {
        let localIndex = Int(readUInt8(from: bytes))
        vm.currentFrame.advanceIp(by: 1)
        try vm.push(vm.stack[vm.currentFrame.basePointer + localIndex]!)
    }
}
struct GetPropertyExecuter : CodeExecutable {
    init(_ vm: VM, with bytes: [Byte]) {self.vm = vm; self.bytes = bytes}
    let vm: VM, bytes: [Byte]
    func execute() throws {
        let index = Int(readUInt8(from: bytes))
        vm.currentFrame.advanceIp(by: 1)
        guard let accessor = ObjectProperties()[index]?.accessor else {throw propertyNotFound}
        guard let object = vm.pull() else {throw objectNotFound}
        let result = try getProperty(of: object, with: accessor)
        try vm.push(result)
    }
}
struct PredicateExecuter : CodeExecutable {
    init(_ vm: VM, with bytes: [Byte]) {self.vm = vm; self.bytes = bytes}
    let vm: VM, bytes: [Byte]
    func execute() throws {
        let index = Int(readUInt8(from: bytes))
        vm.currentFrame.advanceIp(by: 1)
        let predicate = PredicateOperableFactory.predicates[index].operator
        let environment = Environment(with: vm.stack)   // スタックを入力として渡す
        guard let result = predicate(environment).operate() else {return}
        if result.isError {throw result.error!}
        try vm.push(result)
    }
}
struct ClosureExecuter : CodeExecutable {
    init(_ vm: VM, with bytes: [Byte]) {self.vm = vm; self.bytes = bytes}
    let vm: VM, bytes: [Byte]
    func execute() throws {
        let functionIndex = Int(readUInt16(from: bytes))
        let numberOfFreeVariables = Int(readUInt8(from: Array(bytes[2...])))
        vm.currentFrame.advanceIp(by: 3)
        try pushClosure(with: functionIndex, numberOfFreeVariables)
    }
}
private extension ClosureExecuter {
    func pushClosure(with index: Int, _ number: Int) throws {
        guard let function = vm.constants[index] as? JpfCompiledFunction else {throw failedToGetFunction}
        let freeVariables = (0..<number).compactMap {vm.stack[vm.sp - number + $0]}
        guard freeVariables.count == number else {throw "\(number)個の" + failedToGetFreeVariables}
        vm.sp -= number         // 自由変数領域解放
        let closure = JpfClosure(with: function, freeVariables)
        try vm.push(closure)
    }
}
struct GetFreeExecuter : CodeExecutable {
    init(_ vm: VM, with bytes: [Byte]) {self.vm = vm; self.bytes = bytes}
    let vm: VM, bytes: [Byte]
    func execute() throws {
        let index = Int(readUInt8(from: bytes))
        vm.currentFrame.advanceIp(by: 1)
        guard let freeVariable = vm.currentFrame.cl?.free[index] else {throw failedToGetClosure}
        try vm.push(freeVariable)
    }
}
struct CurrentClosureExecuter : CodeExecutable {
    init(_ vm: VM) {self.vm = vm}
    let vm: VM
    func execute() throws {
        guard let currentClosure = vm.currentFrame.cl else {throw failedToGetClosure}
        try vm.push(currentClosure)
    }
}
struct PullConstExecuter : CodeExecutable {
    init(_ vm: VM, with bytes: [Byte]) {self.vm = vm; self.bytes = bytes}
    let vm: VM, bytes: [Byte]
    func execute() throws {
        let numberOfElements = Int(readUInt8(from: bytes))
        vm.currentFrame.advanceIp(by: 1)
        if vm.count < numberOfElements {throw notEnoughStackValues}
    }
}
struct PullStackExecuter : CodeExecutable {
    init(_ vm: VM) {self.vm = vm}
    let vm: VM
    func execute() throws {
        guard let numberOfElements = vm.pull()?.number else {throw stackValueIsNotNumber}
        if vm.count < numberOfElements {throw notEnoughStackValues}
    }
}
struct DuplicateConstExecuter : CodeExecutable {
    init(_ vm: VM, with bytes: [Byte]) {self.vm = vm; self.bytes = bytes}
    let vm: VM, bytes: [Byte]
    func execute() throws {
        let numberOfElements = Int(readUInt8(from: bytes))
        vm.currentFrame.advanceIp(by: 1)
        try duplicateStackValues(with: numberOfElements)
    }
}
struct DuplicateStackExecuter : CodeExecutable {
    init(_ vm: VM) {self.vm = vm}
    let vm: VM
    func execute() throws {
        guard let numberOfElements = vm.pull()?.number else {throw stackValueIsNotNumber}
        try duplicateStackValues(with: numberOfElements)
    }
}
private extension CodeExecutable {
    func duplicateStackValues(with n: Int) throws {
        guard let objs = vm.peek(n) else {throw notEnoughStackValues}
        try vm.push(objs)
    }
}
struct DropConstExecuter : CodeExecutable {
    init(_ vm: VM, with bytes: [Byte]) {self.vm = vm; self.bytes = bytes}
    let vm: VM, bytes: [Byte]
    func execute() throws {
        let numberOfElements = Int(readUInt8(from: bytes))
        vm.currentFrame.advanceIp(by: 1)
        vm.drop(numberOfElements)
    }
}
struct DropStackExecuter : CodeExecutable {
    init(_ vm: VM) {self.vm = vm}
    let vm: VM
    func execute() throws {
        guard let numberOfElements = vm.pull()?.number else {throw stackValueIsNotNumber}
        vm.drop(numberOfElements)
    }
}
struct MapPropertyExecuter : CodeExecutable {
    init(_ vm: VM, with bytes: [Byte]) {self.vm = vm; self.bytes = bytes}
    let vm: VM, bytes: [Byte]
    func execute() throws {
        let index = Int(readUInt8(from: bytes))
        vm.currentFrame.advanceIp(by: 1)
        guard let accessor = ObjectProperties()[index]?.accessor else {throw propertyNotFound}
        guard let object = vm.pull() else {throw objectNotFound}
        let result = try mapProperty(of: object, using: accessor)
        try vm.push(result)
    }
}
extension MapPropertyExecuter {
    func mapProperty(of obj: JpfObject,
                     using accessor: (JpfObject) -> JpfObject?
    ) throws(JpfError) -> JpfObject {
        // 対象がが配列の場合
        if let array = obj as? JpfArray {
            var mapped: [JpfObject] = []
            mapped.reserveCapacity(array.elements.count)
            // 要素の変換
            for element in array.elements {
                mapped.append(
                    try getProperty(of: element, with: accessor)
                )
            }
            // 変換後の配列を返す
            return JpfArray(name: array.name, elements: mapped)
        }
        // その他は、通常のgetPropertyで返す
        return try getProperty(of: obj, with: accessor)
    }
}
extension CodeExecutable {
    func getProperty(of obj: JpfObject,
                     with getPropertyOf: (JpfObject) -> JpfObject?
    ) throws(JpfError) -> JpfObject {
        guard let result = getPropertyOf(obj) else {
            throw failedToGetProperty
        }
        if let err = result.error {throw err}
        return result
    }
}
