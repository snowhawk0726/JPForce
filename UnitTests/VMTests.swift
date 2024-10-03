//
//  VMTests.swift
//  UnitTests
//
//  Created by 佐藤貴之 on 2024/06/21.
//

import XCTest

final class VMTests: XCTestCase {
    typealias VmTestCase = (input: String, expected: Any?)
    //
    override func setUpWithError() throws {
    }
    override func tearDownWithError() throws {
    }
    func testIntegerArithmetic() throws {
        let testPattern: [VmTestCase] = [
            ("1", 1), ("2", 2),
            ("5の負数", -5), ("10を負数にする", -10), ("5を。負数にする", -5),
            ("1と2を足す", 3), ("1と2を。足す", 3), ("1と。2を足す", 3),
            ("1から2を引く", -1), ("1から2を。引く", -1),
            ("1と2を掛ける", 2), ("1と2を。掛ける", 2),
            ("4を2で割る", 2), ("4を2で。割る", 2),
            ("50を2で割り、2を掛け、10を足し、5を引く", 55),
            ("5と5と5と5を足し、10を引く", 10),
            ("2と2と2と2と2を掛ける", 32),
            ("5と5と5と5を足し、10を引く", 10),
            ("5と2を掛け、10を足す", 20),
            ("5に、2と10を掛けたものを、足す", 25),
            ("5に、2と10を足したものを、掛ける", 60),
            ("-50と100と-50を足す", 0),
            ("5に、10と2を掛けたものを足し、15を3で割ったものを足し、2を掛け、-10を足す", 50),
            ("5に。10と2を掛けたものを足す。15を3で割ったものを足す。2を掛ける。-10を足す", 50),
        ]
        try runVmTests(with: testPattern)
    }
    func testBooleanObjectExpressions() throws {
        let testPattern: [VmTestCase] = [
            ("真", true), ("偽", false),
            ("真である", true), ("真でない", false), ("偽である", false), ("偽でない", true),
            ("真で。ある", true), ("真で。ない", false), ("偽で。ある", false), ("偽で。ない", true),
            ("5でない", false), ("真でなくない", true), ("偽でなくない", false), ("5でなくない", true),
            ("5で。ない", false), ("真で。なくない", true), ("偽で。なくない", false), ("5で。なくない", true),
            ("1が正", true), ("1が負", false), ("-1が正", false), ("-1が負", true), ("0が正", false), ("0が負", false),
            ("1。正", true), ("1。負", false), ("-1。正", false), ("-1。負", true), ("0。正", false), ("0。負", false),
        ]
        try runVmTests(with: testPattern)
    }
    func testBooleanExpressions() throws {
        let testPattern: [VmTestCase] = [
            ("１が２より小さい", true), ("１が２より大きい", false),
            ("１が２より。小さい", true), ("１が２より。大きい", false),
            ("１が１より小さい", false), ("１が１より大きい", false),
            ("１が１より。小さい", false), ("１が１より。大きい", false),
            ("１が１に等しい", true), ("１が１に等しくない", false),
            ("１が１に。等しい", true), ("１が１に。等しくない", false),
            ("１が２に等しい", false), ("１が２に等しくない", true),
            ("１が２に。等しい", false), ("１が２に。等しくない", true),
            ("真が真に等しい", true), ("偽が偽に等しい", true), ("真が偽に等しい", false),
            ("真が真に。等しい", true), ("偽が偽に。等しい", true), ("真が偽に。等しい", false),
            ("真が偽に等しくない", true), ("偽が真に等しくない", true),
            ("真が偽に。等しくない", true), ("偽が真に。等しくない", true),
            ("１より２が小さいは、真に等しい", true), ("１より２が小さいは、偽に等しい", false),
            ("１より２が。小さいは、真に等しい", true), ("１より２が。小さいは、偽に等しい", false),
            ("１より２が大きいは、真に等しい", false), ("１より２が大きいは、偽に等しい", true),
            ("１より２が。大きいは、真に等しい", false), ("１より２が。大きいは、偽に等しい", true),
            ("１より２が小さいは、真である", true), ("１より２が小さいは、偽である", false),
            ("１より２が。小さいは、真である", true), ("１より２が。小さいは、偽である", false),
            ("１より２が小さいは、真でない", false), ("１より２が小さいは、偽でない", true),
            ("１より２が。小さいは、真でない", false), ("１より２が。小さいは、偽でない", true),
//            ("偽である場合、【５】でない", true),
        ]
        try runVmTests(with: testPattern)
    }
    func testConditionals() throws {
        let testPattern: [VmTestCase] = [
            ("真である場合、【10】。", 10),
            ("真である場合【10】、それ以外は【20】。", 10), ("偽である場合【10】、それ以外は【20】。", 20),
            ("１である場合、【10】。", 10),
            ("１が２より小さい場合、【10】", 10),
            ("１が２より小さい場合【10】、それ以外は【20】", 10),
            ("１が２より大きい場合【10】、それ以外は【20】", 20),
            ("１が２より大きい場合、【10】", nil),
            ("偽である場合、【10】。", nil),
//            ("偽である場合【10】、である場合【10】、それ以外は【20】", 20),
        ]
        try runVmTests(with: testPattern)
    }
    func testGlobalDefineStatements() throws {
        let testPattern: [VmTestCase] = [
            ("一は１。一", 1),
            ("一は１。二は２。一と二を足す", 3),
            ("一は１。二は、一と一を足す。一と二を足す", 3),
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
            let element = vm.stackTop
            try testExpectedObject(t.expected, element)
        }
    }
    private func testExpectedObject(_ expected: Any?, _ actual: JpfObject?) throws {
        switch expected {
        case let integer as Int:
            try testIntegerObject(Int64(integer), actual)
        case let boolean as Bool:
            try testBooleanObject(boolean, actual)
        case nil:
            XCTAssertNil(actual)
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
