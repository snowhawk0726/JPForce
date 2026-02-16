// ParserOptions.swift
// Shadow mode configuration for sentence parsing

import Foundation

public struct ParserOptions {
    public var enableSentenceShadowMode: Bool
    public var verboseShadowLog: Bool
    public var useSentenceAST: Bool
    public init(enableSentenceShadowMode: Bool = false, verboseShadowLog: Bool = false, useSentenceAST: Bool = false) {
        self.enableSentenceShadowMode = enableSentenceShadowMode
        self.verboseShadowLog = verboseShadowLog
        self.useSentenceAST = useSentenceAST
    }
}
