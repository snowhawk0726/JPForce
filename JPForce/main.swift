//
//  main.swift
//  日本語ふぉーす(JPForce)
//
//  Created by 佐藤貴之 on 2023/02/17.
//
import Foundation

Repl().start()
//
print("lexer動作確認。")
while true {
    print("テストする日本語を入れてください：")
    guard let line = readLine() else {break}
    let lexer = Lexer(line)
    print()
    print("tagged:\t\t", terminator: "")
    lexer.enumerated.forEach {print($0 + "(\(String($1.first!)))", terminator: "")}
    print()
    var token = lexer.getNext()
    while token != .symbol(.EOF) {
        print("tokenized:\t\(token.coloredLiteral)\t\(token)")
        token = lexer.getNext()
    }
}
