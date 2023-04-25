//
//  color.swift
//  JPForce
//
//  Created by 佐藤貴之 on 2023/04/21.
//

import Foundation
enum EscapeCode : UInt {
    #if DEBUG
        static var enabled = false
    #else
        static var enabled = true
    #endif
    case reset = 0
    case black = 30, red, green, yellow, blue, magenta, cyan, white
    // 黒、赤、緑、黄色、青、紫、水色、白
    static func code(from value: UInt) -> String {"\u{1b}[\(value)m"}
    var string: String {Self.code(from: self.rawValue)}
}
extension String {
    func color(_ esc: EscapeCode, reset: Bool = true) -> Self {
        EscapeCode.enabled ? (esc.string + self + (reset ? EscapeCode.reset.string : "")) : self
    }
}
