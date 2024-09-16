//
//  repl.swift
//  日本語ふぉーす(JPForce)
//
//  Created by 佐藤貴之 on 2023/03/02.
//

import Foundation

struct Repl {
    func start() {
        enum Mode {case interpriter, vm}
        var mode: Mode = .interpriter
        let environment = Environment()
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
                runVirtualMachine(of: program)
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
        let evaluated = program.evaluated(with: environment)
        evaluated.map {print("評価結果: \($0.string)")}
        print("入力: (\(environment.string))")
    }
    private func runVirtualMachine(of program: Program) {
        // 翻訳部
        let compiler = Compiler(from: program)
        if let error = compiler.compile() {
            print("翻訳器が、エラーを検出した。")
            print("Error: \(error.message)")
            return
        }
        print("翻訳結果：")
        print(compiler.bytecode.instructions.string)
        // 実行部
        let machine = VM(with: compiler.bytecode)
        if let error = machine.run() {
            print("バイトコード実行時にエラーを検出した。")
            print(error.message)
            return
        }
        // 結果表示
        if let stackTop = machine.stackTop {
            print("実行結果(スタック): \(stackTop.string)")
        }
        let lastPopped = machine.lastPoppedStackElem
        print("実行結果: \(lastPopped.string)")
    }
}
