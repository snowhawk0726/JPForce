// ExpressionStatementShadow.swift
// Runs parseSentence in shadow mode to collect diffs

import Foundation

struct ExpressionStatementShadowRunner {
    static func shadowCheck(_ es: ExpressionStatement, parser: Parser) {
        let parserForShadow = ExpressionStatementParser(parser)
        guard let sentence = parserForShadow.parseSentecne(from: es) else {
            parser.shadowMetrics.record(ShadowDiff.failedBuild, context: ShadowContext(tokenLiteral: es.token.literal, snippet: es.string))
            return
        }
        // Kind diffs (coarse-grained)
        if sentence is CompoundStatement {
            let count = (sentence as? CompoundStatement)?.sentences.count ?? 0
            let snippet = sentence.string + "(\(count))"
            parser.shadowMetrics.record(ShadowDiff.multiSentenceMismatch, context: ShadowContext(tokenLiteral: es.token.literal, snippet: snippet))
        } else if sentence is AssignmentSentence {
            parser.shadowMetrics.record(ShadowDiff.kindMismatch, context: ShadowContext(tokenLiteral: es.token.literal, snippet: es.string))
        } else if sentence is SimpleSentence {
            // close enough; no op
        }
        // Terminator handling mismatches: explicit terminator with conjunctive sentence
        let term = (sentence as? Sentence)?.terminality ?? nil
        if es.terminator.isExplicit,
           term == .conjunctive, !parser.isInCaseClause {
                parser.shadowMetrics.record(
                    ShadowDiff.terminatorMismatch,
                    context: ShadowContext(
                        tokenLiteral: es.token.literal,
                        snippet: sentence.string
                    )
                )
        }
        if parser.options.verboseShadowLog {
            print("[Shadow] \(es.string) -> \(String(describing: type(of: sentence)))")
            print("[Shadow] \(sentence.string)(sentence.string)")
        }
    }
}
