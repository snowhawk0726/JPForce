// ParserOptions.swift
// Shadow mode configuration for sentence parsing

import Foundation

public struct ParserOptions {
    public var enableSentenceShadowMode: Bool
    public var verboseShadowLog: Bool
    public init(enableSentenceShadowMode: Bool = false, verboseShadowLog: Bool = false) {
        self.enableSentenceShadowMode = enableSentenceShadowMode
        self.verboseShadowLog = verboseShadowLog
    }
}
