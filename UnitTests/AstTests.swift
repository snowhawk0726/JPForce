//
//  AstTests.swift
//  UnitTests
//
//  Created by 佐藤貴之 on 2023/02/22.
//

import XCTest

final class AstTests: XCTestCase {
    override func setUpWithError() throws {
    }
    override func tearDownWithError() throws {
    }
    func testString() throws {
        let testPattern = ["myVar","は","、","anotherVar","。"]
        print("テストパターン: \(testPattern)")
        let statements = [DefineStatement(
            token:  Token(word: testPattern[1]),
            name:   Identifier(from: testPattern[0]),
            value:  ExpressionStatement(
                token:       Token(word: testPattern[3]),
                expressions: [Identifier(from: testPattern[3])])
        )]
        let program = Program(statements: statements)
        XCTAssertEqual(program.string, testPattern.reduce("") {$0 + $1})
        print("テスト(\(program.string))成功！！")
    }
}
