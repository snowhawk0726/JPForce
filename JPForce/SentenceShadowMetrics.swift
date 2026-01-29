// SentenceShadowMetrics.swift
// Metrics and helpers for sentence shadow parsing

import Foundation

public enum ShadowDiff {
    case failedBuild
    case kindMismatch
    case multiSentenceMismatch
    case terminatorMismatch
}

public struct ShadowContext {
    public let tokenLiteral: String
    public let snippet: String
    public init(tokenLiteral: String, snippet: String) {
        self.tokenLiteral = tokenLiteral
        self.snippet = snippet
    }
}

public struct SentenceShadowMetrics {
    public var failedBuildCount: Int = 0
    public var kindMismatchCount: Int = 0
    public var multiSentenceMismatchCount: Int = 0
    public var terminatorMismatchCount: Int = 0
    public var samples: [String] = []
    public var maxSamples: Int = 20

    public init() {}

    public mutating func record(_ diff: ShadowDiff, context: ShadowContext) {
        switch diff {
        case .failedBuild: failedBuildCount += 1
        case .kindMismatch: kindMismatchCount += 1
        case .multiSentenceMismatch: multiSentenceMismatchCount += 1
        case .terminatorMismatch: terminatorMismatchCount += 1
        }
        if samples.count < maxSamples {
            samples.append(context.snippet)
        }
    }
}
