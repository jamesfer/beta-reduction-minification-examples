// interface Token {
//   value: string,
//   position: number,
// }
//
// const enum ExpressionKind {
//   NumberLiteral,
//   BinaryOperator,
// }
//
// interface BaseExpression<K extends ExpressionKind> {
//   kind: K,
//   tokens: Token[],
// }
//
// const enum Operator {
//   Addition,
//   Subtraction,
//   Multiplication,
//   Division,
// }
//
// interface BinaryOperatorExpression extends BaseExpression<ExpressionKind.BinaryOperator> {
//   left: Expression,
//   right: Expression,
//   operator: Operator,
// }
//
// interface NumberLiteralExpression extends BaseExpression<ExpressionKind.NumberLiteral> {
//   value: number,
// }
//
// type Expression =
//   | BinaryOperatorExpression
//   | NumberLiteralExpression;
//
// interface Message {
//   body: string,
//   tokens: Token[],
// }
//
// type Parser<R> = (
//   left: Expression | null,
//   tokens: Token[],
// ) => [Message[], Token[], R | null];
//
// type Matcher<T extends Expression = Expression, R extends Expression = Expression> = (
//   left: Expression | null,
//   tokens: Token[],
// ) => [Message[], Expression | null];
//
// function makeParser<T>(value: T): Parser<T> {
//   return (_, tokens) => [[], tokens, value];
// }
//
// function getTokens(): Parser<Token[]> {
//   return (_, tokens) => [[], tokens, tokens];
// }
//
// function consumeTokens(count: number): Parser<{}> {
//   return (_, tokens) => [[], tokens.slice(count), {}];
// }
//
// function logError(body: string): Parser<{}> {
//   return (_, tokens) => [[{ body, tokens: [] }], tokens, {}];
// }
//
//
// // function chain2<T1 extends Expression, T2 extends Expression, R extends Expression>(
// //   a: Matcher<T1, T2>,
// //   b: Matcher<T2, R>,
// // ): Matcher<T1, R> {
// //   return (left, tokens) => {
// //     const firstResult = a(left, tokens);
// //     if (firstResult[1]) {
// //       const secondResult = b(firstResult[1], tokens.slice(firstResult[1].tokens.length));
// //       return [
// //         [...firstResult[0], ...secondResult[0]],
// //         secondResult[1],
// //       ];
// //     }
// //     return firstResult;
// //   }
// // }
//
// function pipe2<T1, T2, R>(a: Parser<T1>, b: (r: T1) => Parser<T2>): Parser<T2> {
//   return (left, tokens) => {
//     const resultA = a(left, tokens);
//     if (resultA[2]) {
//       const resultB = b(resultA[2])(left, resultA[1]);
//       return [[...resultA[0], ...resultB[0]], resultB[1], resultB[2]];
//     }
//     return [resultA[0], resultA[1], null];
//   }
// }
//
// function tokenMatches(pattern: RegExp | string) {
//   return (token: Token) => typeof pattern === 'string'
//     ? token.value === pattern
//     : pattern.test(token.value);
// }
//
// function requireToken(predicate: (token: Token) => boolean): Parser<Token> {
//   return pipe(
//     getTokens(),
//     tokens => makeParser(tokens.length > 0 && predicate(tokens[0]) ? tokens[0] : null),
//
//   );
//   return (_, tokens) => {
//     const matched = tokens.length > 0 && predicate(tokens[0]);
//     [[], matched ? tokens[0] : null]
//   }
// }
//
// function requireLeft(): Parser<{} | null> {
//   return (left, tokens) => [[], tokens, left ? {} : null];
// }
//
// function requireNoLeft(): Parser<{} | null> {
//   return (left, tokens) => [[], tokens, left ? null : {}];
// }
//
// // function map(parser: Parser<T>, mapper)
//
// function wrapFunctionResult<T, R>(fn: (a: T) => R): (a: T) => Parser<R> {
//   return a => makeParser(fn(a));
// }
//
// const numberLiteral = chain(
//   requireNoLeft,
//   pipe2(
//     requireToken(tokenMatches(/^[0-9]+$/)),
//     wrapFunctionResult(token => ({
//       kind: ExpressionKind.NumberLiteral,
//       tokens: [token],
//       value: +token,
//     })),
//   ),
// );
//
// const binaryOperation = chain(
//   requireLeft,
//   requireToken(tokenMatches(/^[+-/*]$/)),
//   numberLiteral,
// );

