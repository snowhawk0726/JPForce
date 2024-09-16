//
//  VMTests.swift
//  UnitTests
//
//  Created by 佐藤貴之 on 2024/06/21.
//

import XCTest

final class VMTests: XCTestCase {
    typealias VmTestCase = (input: String, expected: Any)
    //
    override func setUpWithError() throws {
    }
    override func tearDownWithError() throws {
    }
    func testIntegerArithmetic() throws {
        let testPattern: [VmTestCase] = [
            ("1", 1), ("2", 2),
            ("5の負数", -5), ("10を負数にする", -10),
            ("1と2を足す", 3),
            ("1から2を引く", -1),
            ("1と2を掛ける", 2),
            ("4を2で割る", 2),
            ("50を2で割り、2を掛け、10を足し、5を引く", 55),
            ("5と5と5と5を足し、10を引く", 10),
            ("2と2と2と2と2を掛ける", 32),
            ("5と5と5と5を足し、10を引く", 10),
            ("5と2を掛け、10を足す", 20),
            ("5に、2と10を掛けたものを、足す", 25),
            ("5に、2と10を足したものを、掛ける", 60),
            ("-50と100と-50を足す", 0),
            ("5に、10と2を掛けたものを足し、15を3で割ったものを足し、2を掛け、-10を足す", 50),
        ]
        try runVmTests(with: testPattern)
    }
    func testBooleanObjectExpressions() throws {
        let testPattern: [VmTestCase] = [
            ("真", true), ("偽", false),
            ("真である", true), ("真でない", false), ("偽である", false), ("偽でない", true),
            ("5でない", false), ("真でなくない", true), ("偽でなくない", false), ("5でなくない", true),
            ("1が正", true), ("1が負", false), ("-1が正", false), ("-1が負", true), ("0が正", false), ("0が負", false),
        ]
        try runVmTests(with: testPattern)
    }
    // MARK: - Helpers
    private func runVmTests(with tests: [VmTestCase]) throws {
        for t in tests {
            let program = parseProgram(with: t.input)
            let compiler = Compiler(from: program)
            XCTAssertNil(compiler.compile())
            let vm = VM(with: compiler.bytecode)
            XCTAssertNil(vm.run())
            let element = vm.lastPoppedStackElem
            try testExpectedObject(t.expected, element)
        }
    }
    private func testExpectedObject(_ expected: Any, _ actual: JpfObject?) throws {
        switch expected {
        case let integer as Int:
            try testIntegerObject(Int64(integer), actual)
        case let boolean as Bool:
            try testBooleanObject(boolean, actual)
        default:
            break
        }
    }
    private func testIntegerObject(_ expected: Int64, _ actual: JpfObject?) throws {
        let integer = try XCTUnwrap(actual as? JpfInteger)
        XCTAssertEqual(integer.value, Int(expected))
    }
    private func testBooleanObject(_ expected: Bool, _ actual: JpfObject?) throws {
        let boolean = try XCTUnwrap(actual as? JpfBoolean)
        XCTAssertEqual(boolean.value, expected)
    }
}
