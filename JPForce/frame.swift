//
//  frame.swift
//  JPForce
//
//  Created by 佐藤貴之 on 2024/10/12.
//

import Foundation

/// 実行フレーム(アクティベーションレコード)
class Frame {
    init() {}
    init(with fn : JpfCompiledFunction, basePointer: Int) {
        self.fn = fn
        self.basePointer = basePointer
        self.sp = basePointer + fn.numberOfLocals
    }
    var fn: JpfCompiledFunction?
    var ip = -1             // フレーム内の実行位置
    var basePointer = 0     // ローカル変数のスロット位置
    var sp = 0              // Frameのsp初期値
    //
    var insturctions: Instructions? {fn?.instructions}
    // ip制御
    var isIpInRange: Bool {
        guard let i = insturctions else {return false}
        return ip < i.count - 1
    }
    func advanceIp(by offset: Int) {ip += offset}
    func advancedIp() -> Int {
        ip += 1
        return ip
    }
    func setIp(to position: Int) {ip = position}
}
