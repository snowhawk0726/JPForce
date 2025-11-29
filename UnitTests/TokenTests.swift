//
//  TokenTests.swift
//  UnitTests
//
//  Created by 佐藤貴之 on 2023/02/28.
//

import XCTest

final class TokenTests: XCTestCase {
    override func setUpWithError() throws {
    }
    override func tearDownWithError() throws {
    }
    func testWordInitializers() throws {
        let testPatterns: [(input: String, expected: Token, plain: String) ] = [
            ("足す", .keyword(.ADD), "足す"),
            ("足し", .wrapped(type: .keyword(.ADD), literal: "足し"), "足す"),
            ("た", .particle(.TA), "た"),
            ("て", .wrapped(type: .particle(.TA), literal: "て"), "た"),
            ("-9", .INT(-9), "-9"),
            ("識別", .IDENT("識別"), "識別"),
            ("余り", .wrapped(type: .ident, literal: "余り"), "余り"),
            ("加え", .wrapped(type: .ident, literal: "加え"), "加え"),        // 識別子の語尾変換はしない
            ("し", .wrapped(type: .keyword(.SURU), literal: "し"), "する"),
            ("あっ", .wrapped(type: .keyword(.BE), literal: "あっ"), "ある"),
        ]
        for test in testPatterns {
            print("テストパターン: \(test.input)")
            let token = Token(word: test.input)
            XCTAssertEqual(token, test.expected)
            XCTAssertEqual(token.unwrappedLiteral, test.plain)
            print("テスト終了(\(token) & unwrappedLiteral:「\(token.unwrappedLiteral)」)")
        }
    }
    func testSameCategories() throws {
        let testPatterns: [(token: Token, comparison: Token, result: Bool)] = [
            (.keyword(.EQUAL), .keyword(.EQUAL), true),
            (.keyword(.EQUAL), .keyword(.NOT), true),
            (.symbol(.COMMA), .symbol(.PERIOD), true),
            (.particle(.KARA), .particle(.YORI), true),
            (.INT(0), .INT(99), true),
            (.IDENT("甲"), .IDENT("乙"), true),
            (.STRING("文字"), .STRING("列"), true),
            (.ILLEGAL("不当"), .ILLEGAL("違法"), true),
            (Token(word: "貸し"), Token(word: "貸す"), true),
            (.STRING("文字列"), .IDENT("文字列"), false),
        ]
        for test in testPatterns {
            print("テストパターン: \(test.token)と\(test.comparison)のカテゴリ")
            let result = test.token.isSameCategory(as: test.comparison)
            XCTAssertEqual(result, test.result)
            print("テスト終了(\(result))")
        }
    }
}
