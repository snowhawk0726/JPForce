//
//  ObjectTests.swift
//  UnitTests
//
//  Created by 佐藤貴之 on 2023/04/02.
//

import XCTest

final class ObjectTests: XCTestCase {
    override func setUpWithError() throws {
    }
    override func tearDownWithError() throws {
    }
    func testStringHashKey() {
        let hello1 = JpfString(value: "Hello World")
        let hello2 = JpfString(value: "Hello World")
        let diff1 = JpfString(value: "My name is johnny")
        let diff2 = JpfString(value: "My name is johnny")
        XCTAssertEqual(hello1.hashKey, hello2.hashKey)
        XCTAssertEqual(diff1.hashKey, diff2.hashKey)
        XCTAssertNotEqual(hello1.hashKey, diff1.hashKey)
    }
}
