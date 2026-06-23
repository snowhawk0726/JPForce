//
//  repl.swift
//  日本語ふぉーす(JPForce)
//
//  Created by 佐藤貴之 on 2023/03/02.
//

import Foundation

var numberOfStack = 0
//
struct Repl {
    func start() {
        enum Mode {case interpriter, vm}
        var mode: Mode = .interpriter
        var optimize: Bool = true
        //
        let environment = Environment()     // for Intepriter
        var constants: [JpfObject] = []     // for Compiler & VM
        let globals = GlobalStore()         // for Compiler & VM
        let symbolTable = SymbolTable()     // for Compiler
        let stack = Stack()                 // for VM
        print("日本語ふぉーす(JPForce)のREPLです。")
        while true {
            printPrompt()
            guard let line = readLine(), !line.isEmpty else {
                return
            }
            switch line.lowercased() {
            case "vm":
                mode = .vm
                print("仮想マシン・モード")
                continue
            case "interpriter":
                mode = .interpriter
                print("インタプリタ・モード")
                continue
            case "optimize":
                print("定数解析：\(optimize ? "on" : "off")")
                continue
            case "optimize off":
                optimize = false
                print("定数解析：off")
                continue
            case "optimize on":
                optimize = true
                print("定数解析：on")
                continue
            default:
                break
            }
            let lexer = Lexer(line)
            let parser = Parser(lexer)
            parser.options.useSentenceAST = true
            guard let program = parser.parseProgram(), parser.errors.isEmpty else {
                printErros(of: parser.errors)
                continue
            }
            if mode == .vm {
                runVirtualMachine(of: program, &constants, symbolTable, globals, stack, optimize: optimize)
            } else {
                runEvaluator(of: program, with: environment)
            }
        }
    }
    private func printPrompt() {
        print(">> ", terminator: "")
    }
    private func printErros(of errors: [String]) {
        print("構文解析器が、\(errors.count)個のエラーを検出しました。")
        errors.forEach {print("\t\($0)")}
    }
    private func runEvaluator(of program: Program, with environment: Environment) {
        let evaluated = program.evaluate(with: environment)
        evaluated.map {print("評価結果: \($0.string)")}
        print("入力: (\(environment.string))")
    }
    private func runVirtualMachine(of program: Program, _ constants: inout [JpfObject], _ symbolTable: SymbolTable, _ globals: GlobalStore, _ stack: Stack, optimize: Bool = true) {
        // 翻訳部
        let compiler = Compiler(from: program, symbolTable, constants)
        compiler.optimizeConstantsEnabled = optimize

        if optimize {
            // 定数解析(評価)
            syncGlobalsToEnvironment(from: globals, symbolTable: symbolTable, compiler)
            syncStackToEnvironment(from: stack, compiler)
            switch compiler.analyze() {
            case .error(let errorMessage):
                print("定数解析で、エラーを検出しました。")
                print("\tエラー: \(errorMessage)")
                return
            case .constant(let analyzed):
                syncEnvironmentToGlobals(from: compiler, symbolTable: symbolTable, globals: globals)
                syncEnvironmentToStack(from: compiler, stack: stack)
                print("実行結果(定数解析): \(analyzed.string)")
                // 定数計算のスタックをVMのスタックに移す
                _ = stack.push(analyzed)
                numberOfStack = stack.count
                print("入力: (\(stack.string))")
                return
            case .nonConstant:
                break
            case .evaluated:
                syncEnvironmentToGlobals(from: compiler, symbolTable: symbolTable, globals: globals)
                syncEnvironmentToStack(from: compiler, stack: stack)
                print("実行結果(定数解析): nil")
                numberOfStack = stack.count
                print("入力: (\(stack.string))")
                return
            }
        }
        if let result = compiler.compile() {
            if let error = result.error {
                print("翻訳器が、エラーを検出しました。")
                print("\tエラー: \(error.message)")
                return
            }
            print("実行結果(翻訳): \(result.string)")
            // 定数計算のスタックをVMのスタックに移す
            _ = stack.push(result)
            numberOfStack = stack.count
            print("入力: (\(stack.string))")
            return
        }
        constants = compiler.bytecode.constants
        print("翻訳結果：")
        print(compiler.bytecode.instructions.disassemble(with: constants, symbolTable))
        constants.enumerated().forEach {
            if let function = $1 as? JpfCompiledFunction {
                print("翻訳済み関数(\($0))：")
                print(function.instructions.disassemble(with: constants, symbolTable))
            }
        }
        // 実行部
        let machine = VM(with: compiler.bytecode, globals, stack)
        if let error = machine.run() {
            print("バイトコード実行時にエラーを検出しました。")
            print(error.message)
            return
        }
        // 結果表示
        if let result = machine.stackTop,
           numberOfStack != machine.stack.count {   // スタック数が増減した場合
            print("実行結果: \(result.string)")
        }
        numberOfStack = machine.stack.count
        print("入力: (\(machine.string))")
    }
    /// (翻訳)環境 -> 大域変数 同期関数
    private func syncEnvironmentToGlobals(from compiler: Compiler,
                                          symbolTable: SymbolTable,
                                          globals: GlobalStore) {
        // 1) compiler.environment.store（仮）から全エントリを取得
        // 2) SymbolTable でグローバルシンボルを確保（無ければ define）
        // 3) 対応 index に値を書き込む
        for (name, value) in compiler.environment.enumerated {
            // シンボル解決（グローバル）
            let sym = symbolTable.resolve(name) ?? symbolTable.define(name)
            // グローバルに反映
            globals[sym.index] = value
        }
    }
    /// 大域変数 ->  (翻訳)環境 同期関数
    private func syncGlobalsToEnvironment(from globals: GlobalStore,
                                          symbolTable: SymbolTable,
                                          _ compiler: Compiler) {
        for index in 0..<globals.count {
            let name = symbolTable[index]!
            compiler.environment[name] = globals[index]
        }
    }
    /// (翻訳)環境 -> スタック 同期関数
    private func syncEnvironmentToStack(from compiler: Compiler, stack: Stack) {
        compiler.environment.pullAll().forEach { _ = stack.push($0) }
    }
    /// スタック -> (翻訳)環境 同期関数
    private func syncStackToEnvironment(from stack: Stack, _ compiler: Compiler) {
        stack.pullAll().forEach { _ = compiler.push($0) }
    }
}
