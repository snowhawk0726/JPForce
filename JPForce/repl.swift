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
            if line.lowercased() == "vm" {
                mode = .vm
                print("仮想マシン・モード")
                continue
            } else
            if line.lowercased() == "interpriter" {
                mode = .interpriter
                print("インタプリタ・モード")
                continue
            }
            let lexer = Lexer(line)
            let parser = Parser(lexer)
            guard let program = parser.parseProgram(), parser.errors.isEmpty else {
                printErros(of: parser.errors)
                continue
            }
            if mode == .vm {
                runVirtualMachine(of: program, &constants, symbolTable, globals, stack)
            } else {
                runEvaluator(of: program, with: environment)
            }
        }
    }
    private func printPrompt() {
        print(">> ", terminator: "")
    }
    private func printErros(of errors: [String]) {
        print("構文解析器が、\(errors.count)個のエラーを検出した。")
        errors.forEach {print("\t\($0)")}
    }
    private func runEvaluator(of program: Program, with environment: Environment) {
        let evaluated = program.evaluate(with: environment)
        evaluated.map {print("評価結果: \($0.string)")}
        print("入力: (\(environment.string))")
    }
    private func runVirtualMachine(of program: Program, _ constants: inout [JpfObject], _ symbolTable: SymbolTable, _ globals: GlobalStore, _ stack: Stack) {
        // 翻訳部
        let compiler = Compiler(from: program, symbolTable, constants)
        if let error = compiler.compile() {
            print("翻訳器が、エラーを検出した。")
            print("\tエラー: \(error.message)")
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
            print("バイトコード実行時にエラーを検出した。")
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
}
