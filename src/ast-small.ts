/**************************************************
 *
 * Function utils
 *
 **************************************************/

/**
 * Creates a function that will always return the given value.
 * @inline
 */
function constant<T>(value: T): () => T {
  return () => value;
}

/**
 * Creates a function that is the composition of the two given functions.
 * @inline
 */
function compose<A, B, T>(outer: (b: B) => T, inner: (a: A) => B): (a: A) => T {
  return a => outer(inner(a));
}

/**
 * Converts a function that accepts individual arguments to one that accepts an array of arguments.
 */
function spread<T1, R>(fn: (a1: T1) => R): (args: [T1]) => R;
function spread<T1, T2, R>(fn: (a1: T1, a2: T2) => R): (args: [T1, T2]) => R;
function spread<T1, T2, T3, R>(fn: (a1: T1, a2: T2, a3: T3) => R): (args: [T1, T2, T3]) => R;
/** @inline */
function spread<T, R>(fn: (...args: T[]) => R): (args: T[]) => R {
  return args => fn.apply(undefined, args);
}

/**************************************************
 *
 * Monads
 *
 **************************************************/

/**
 * Specifies the different kind of values that comprise the maybe monad.
 */
const enum MaybeConstructor {
  Nothing,
  Just,
}

/**
 * Nothing is a special value that indicates that an operation produced no value.
 */
interface Nothing {
  maybeConstructor: MaybeConstructor.Nothing;
}

/**
 * The nothing constant is used as a representation of Nothing so duplicate objects do not need to
 * be created.
 */
const nothing: Nothing = {
  maybeConstructor: MaybeConstructor.Nothing,
};

/**
 * Just indicates that the maybe monad actually does contain a value.
 */
interface Just<T> {
  maybeConstructor: MaybeConstructor.Just;
  value: T;
}

/**
 * Convenient constructor for producing a Just object that wraps value.
 * @inline
 */
function just<T>(value: T): Just<T> {
  return {
    value,
    maybeConstructor: MaybeConstructor.Just,
  };
}

/**
 * The maybe monad is used to indicate that an operation may or may not have produced a value.
 */
type Maybe<T> =
  | Nothing
  | Just<T>;

/**
 * Combines two maybe monads together. If either of them are nothing, the result becomes nothing.
 * If the first contains a value, it is passed into the function to produce the second.
 * @inline
 */
function bindMaybe<T, R>(monad: Maybe<T>, fn: (value: T) => Maybe<R>): Maybe<R> {
  return monad.maybeConstructor === MaybeConstructor.Nothing
    ? nothing as Maybe<any>
    : fn(monad.value);
}

/**
 * If the maybe value is nothing, replaces it with the given fallback.
 * @inline
 */
function fallbackIfNothing<T>(monad: Maybe<T>, fallback: () => Maybe<T>): Maybe<T> {
  return monad.maybeConstructor === MaybeConstructor.Nothing ? fallback() : monad;
}

/**
 * Converts a maybe monad value into a plain value, or null if the monad was nothing.
*/
function runMaybe<T>(monad: Maybe<T>): T | null {
  return monad.maybeConstructor === MaybeConstructor.Nothing ? null : monad.value;
}

interface FallbackList {
  parent?: FallbackList;
  matcher: ParseResult<any>;
  tokens: Token[];
  fallback: ParseResult<any>;
}

function fallbackExists(
  fallbackList: FallbackList | undefined,
  matcher: ParseResult<any>,
  tokens: Token[],
): ParseResult<any> | undefined {
  return !fallbackList
    ? undefined
    : fallbackList.matcher === matcher && fallbackList.tokens === tokens
      ? fallbackList.fallback
      : fallbackExists(fallbackList.parent, matcher, tokens);
}

/**
 * @inline
 */
function addFallback(
  parent: FallbackList | undefined,
  matcher: ParseResult<any>,
  tokens: Token[],
  fallback: ParseResult<any>,
): FallbackList {
  return { parent, matcher, tokens, fallback };
}

type ParseResult<V> = (previousCalls: FallbackList | undefined) => (state: Token[]) => (
  Maybe<[Token[], V]>
);

/**
 * Converts a function from working on regular values to working on parse results.
 * @inline
 */
function liftA2<A, B, T>(
  fn: (a: A, b: B) => ParseResult<T>,
): (a: ParseResult<A>, b: ParseResult<B>) => ParseResult<T> {
  return (monadA, monadB) => bindResult(monadA, a => bindResult(monadB, b => fn(a, b)));
}

/**
 * Unwraps the result and passes its value to an operation. Returns the result of the operation.
 * @2inline
 */
function bindResult<T, U>(
  monad: ParseResult<T>,
  operation: (value: T) => ParseResult<U>,
): ParseResult<U> {
  return previousCalls => tokens => (
    bindMaybe(monad(previousCalls)(tokens), result => (
      operation(result[1])(previousCalls)(result[0])
    ))
  );
}

/**
 * Like bindResult but will unwrap two results and pass them both to the operation.
 * @inline
 */
function bindResultA2<T1, T2, U>(
  monad1: ParseResult<T1>,
  monad2: ParseResult<T2>,
  operation: (value1: T1, value2: T2) => ParseResult<U>,
): ParseResult<U> {
  return bindResult(monad1, value1 => bindResult(monad2,  value2 => operation(value1, value2)));
}

/**
 * If the first result is nothing, will replace it with the value of the second.
 * @inline
 */
function replaceResultIfNothing<T, U>(
  monad: ParseResult<T>,
  operation: ParseResult<T>,
): ParseResult<T> {
  return previousCalls => tokens => (
    fallbackIfNothing(monad(previousCalls)(tokens), () => operation(previousCalls)(tokens))
  );
}

/**
 * Wraps a plain value in a parse result
 * @2inline
 */
function wrapResult<T>(value: T): ParseResult<T> {
  return constant((tokens: Token[]) => just<[Token[], T]>([tokens, value]));
}

/**
 * A parse result with the value nothing.
 * @inline
 */
function createNothingResult(): ParseResult<any> {
  return constant(constant(nothing));
}

/**
 * Returns a parse result that contains the current list of tokens.
 * @2inline
 */
function askTokens(): ParseResult<Token[]> {
  return constant((tokens: Token[]) => just<[Token[], Token[]]>([tokens, tokens]));
}

/**
 * Helper function used within recurse.
 */
function createInnerRecurse<T>(
  matcher: ParseResult<T>,
): (fallbackList: FallbackList, lastResult?: T | undefined) => ParseResult<any> {
  // This function needs to be named so we can call it recursively
  return function recurse(
    fallbackList: FallbackList,
    lastResult: T | undefined = undefined,
  ): ParseResult<any> {
    return replaceResultIfNothing(
      bindResultA2(constant(matcher(fallbackList)), askTokens(), (result, tokens) => (
        recurse(
          // Add new fallback
          addFallback(fallbackList, matcher, tokens, wrapResult(result)),
          result,
        )
      )),
      lastResult ? wrapResult(lastResult) : createNothingResult(),
    );
  };
}

/**
 * Supports basic protection from infinite recursion when writing left recurse patterns.
 */
function leftRecurse<T, U>(baseCase: ParseResult<U>, matcher: ParseResult<T>): ParseResult<T | U> {
  return previousCalls => tokens => (
    // Check if a fallback value was already provided
    fallbackExists(previousCalls, matcher, tokens)
    // Recurse with a new fallback
    || createInnerRecurse(matcher)(addFallback(previousCalls, matcher, tokens, baseCase))
  )(previousCalls)(tokens);
}

/**
 * Returns a parse result that will set the remaining tokens.
 * @inline
 */
function setTokens(tokens: Token[]): ParseResult<undefined> {
  return constant(constant(just<[Token[], undefined]>([tokens, undefined])));
}

/**
 * Executes a parse result and returns its inner value, or undefined.
 */
function runParser<T>(
  parser: ParseResult<T>,
  tokens: Token[],
  previousCalls: FallbackList | undefined = undefined,
): T | undefined {
  const result = runMaybe(parser(previousCalls)(tokens));
  return result ? result[1] : undefined;
}

/**************************************************
 *
 * Language utilities
 *
 **************************************************/

/**
 * Indicates the type of an artifact.
 */
export enum ArtifactType {
  Token = 'Token',
  Expression = 'Expression',
}

/**
 * All accepted simple tokens.
 */
export enum TokenSymbol {
  plus = '+',
  minus = '-',
  times = '*',
  divide = '/',
  power = '^',
  openParen = '(',
  closeParen = ')',
}

/**
 * The number literal token can contain any integer number.
 */
export interface NumberLiteral {
  type: ArtifactType.Token;
  value: number;
}

/**
 * Convenient constructor for producing a number literal object.
 * @inline
 */
function numberLit(value: number): NumberLiteral {
  return { value, type: ArtifactType.Token };
}

/**
 * Tokens make up the building blocks of the language. It is either a simple TokenSymbol or a
 * NumberLiteral.
 */
export type Token =
  | TokenSymbol
  | NumberLiteral;

/**
 * Returns true if a token is a TokenSymbol.
 * @inline
 */
function isSymbol(symbol: TokenSymbol): (token: Token) => token is TokenSymbol {
  return (token): token is TokenSymbol => token === symbol;
}

/**
 * Returns true if a token is a NumberLiteral
 * @inline
 */
function isNumberLiteral(token: Token): token is NumberLiteral {
  return typeof token === 'object';
}

/**
 * Returns true if the next token passes the predicate
 * @inline
 */
function nextTokenIs(tokens: Token[], predicate: (token: Token) => boolean): boolean {
  return tokens.length > 0 && predicate(tokens[0]);
}

/**
 * Wraps a value and only executes it when needed. Useful for grammars that recursively reference
 * their syntax elements.
 * @2inline
 */
function lazy<R>(value: () => ParseResult<R>): ParseResult<R> {
  return bindResult(wrapResult(undefined), value);
}

/**
 * Produces a new matcher that will alter the value of an existing matcher when it matches anything
 * using a mapping function.
 * @2inline
 */
function map<T, U>(matcher: ParseResult<T>, mapper: (value: T) => U): ParseResult<U> {
  return bindResult(matcher, compose<T, U, ParseResult<U>>(wrapResult, mapper));
}

/**
 * Extracts the next token from the incoming stream if it satisfies a predicate and returns it as
 * the value. If the next token doesn't pass the predicate, returns nothing.
 */
function takeOne<T extends Token>(predicate: (token: Token) => token is T): ParseResult<T>;
/** @2inline */
function takeOne(predicate: (t: Token) => boolean): ParseResult<Token> {
  return bindResult(askTokens(), tokens => (
    nextTokenIs(tokens, predicate)
      ? bindResult(setTokens(tokens.slice(1)), constant(wrapResult(tokens[0])))
      : createNothingResult()
  ));
}

interface Match<T> {
  tokens: Token[];
  result: T;
}

/**
 * Returns the longest match from the set. The longest match is the one with the fewest remaining
 * tokens.
 */
function longestMatch<T>(matches: Match<T>[]): Match<T> {
  return matches.slice(1).reduce(
    (shortestMatch, currentMatch) => (
      currentMatch.tokens.length < shortestMatch.tokens.length ? currentMatch : shortestMatch
    ),
    matches[0],
  );
}

/**
 * Returns the first match found by going through the given matchers in order.
 */
function oneOf<R>(...matchers: ParseResult<R>[]): ParseResult<R> {
  // TODO clean up
  return fallbackList => (tokens) => {
    const allMatches = matchers.reduce<ParseResult<Match<R>[]>>(
      (all, current) => bindResult(all, array => (
        // Skip the matcher if it fails
        replaceResultIfNothing(
          bindResultA2(
            constant(constant(current(fallbackList)(tokens))),
            askTokens(),
            // Combine each of the results into an array
            (result, tokens) => wrapResult([...array, { tokens, result }]),
          ),
          all,
        )
      )),
      wrapResult([]),
    );

    const selectLongestMatch = bindResult(
      // Run each matcher
      allMatches,
      matches => matches.length === 0
        // Return nothing if none of them matched
        ? createNothingResult()
        // Otherwise select the longest match
        : bindResult(
          wrapResult(longestMatch(matches)),
          // Update the tokens state with the result of the longest match
          longest => bindResult(setTokens(longest.tokens), constant(wrapResult(longest.result))),
        ),
    );

    return selectLongestMatch(fallbackList)(tokens);
  };
}

/**
 * Matches all of the given matchers in order or returns nothing if any of them fail.
 */
/* tslint:disable:max-line-length */
function chain<T1>(m1: ParseResult<T1>): ParseResult<[T1]>;
function chain<T1, T2>(m1: ParseResult<T1>, m2: ParseResult<T2>): ParseResult<[T1, T2]>;
function chain<T1, T2, T3>(m1: ParseResult<T1>, m2: ParseResult<T2>, m3: ParseResult<T3>): ParseResult<[T1, T2, T3]>;
/* tslint:enable:max-line-length */
function chain<R>(...matchers: ParseResult<R>[]): ParseResult<R[]> {
  return matchers.reduce(
    liftA2<R[], R, R[]>((previous, current) => wrapResult([...previous, current])),
    wrapResult([] as R[]),
  );
}

/**************************************************
 *
 * Language utilities
 *
 **************************************************/

/**
 * The three kinds of expressions in the language.
 */
export enum ExpressionType {
  Operation = 'Operation',
  Identifier = 'Identifier',
  NumberLiteral = 'NumberLiteral',
}

/**
 * An identifier expression is used whenever a token that references a function is used.
 */
export interface IdentifierExpression {
  type: ArtifactType.Expression;
  expressionType: ExpressionType.Identifier;
  name: string;
  tokens: Token[];
}

/**
 * An operation expression is like a function call.
 */
export interface OperationExpression {
  type: ArtifactType.Expression;
  expressionType: ExpressionType.Operation;
  left: Expression;
  right: Expression;
  op: Expression;
  tokens: Token[];
}

/**
 * The number literal expression just wraps a number literal token.
 */
export interface NumberLiteralExpression {
  type: ArtifactType.Expression;
  expressionType: ExpressionType.NumberLiteral;
  value: number;
  tokens: Token[];
}

/**
 * The three expressions that make up our language.
 */
export type Expression =
  | IdentifierExpression
  | OperationExpression
  | NumberLiteralExpression;

/**
 * Convenient constructor for making an identifier expression.
 * @2inline
 */
function identifierExpression(name: TokenSymbol): IdentifierExpression {
  return {
    name,
    type: ArtifactType.Expression,
    expressionType: ExpressionType.Identifier,
    tokens: [name],
  };
}

/**
 * Convenient constructor for making a number literal expression.
 * @2inline
 */
function numberLiteralExpression(number: NumberLiteral): NumberLiteralExpression {
  return {
    type: ArtifactType.Expression,
    expressionType: ExpressionType.NumberLiteral,
    value: number.value,
    tokens: [number],
  };
}

/**
 * Convenient constructor for making an operation expression.
 * @2inline
 */
function operationExpression(
  left: Expression,
  op: Expression,
  right: Expression,
): OperationExpression {
  return {
    left,
    op,
    right,
    type: ArtifactType.Expression,
    expressionType: ExpressionType.Operation,
    tokens: [...left.tokens, ...op.tokens, ...right.tokens],
  };
}

/**
 * Convenient constructor for making a group expression.
 * @2inline
 */
function groupExpression(
  leftParen: Token,
  expression: Expression,
  rightParen: Token,
): Expression {
  return { ...expression, tokens: [leftParen, ...expression.tokens, rightParen] };
}

/**************************************************
 *
 * AST expression patterns
 *
 **************************************************/

const plus = map(takeOne(isSymbol(TokenSymbol.plus)), identifierExpression);
const minus = map(takeOne(isSymbol(TokenSymbol.minus)), identifierExpression);
const times = map(takeOne(isSymbol(TokenSymbol.times)), identifierExpression);
const divide = map(takeOne(isSymbol(TokenSymbol.divide)), identifierExpression);
const power = map(takeOne(isSymbol(TokenSymbol.power)), identifierExpression);
const number = map(takeOne(isNumberLiteral), numberLiteralExpression);
const openParen = takeOne(isSymbol(TokenSymbol.openParen));
const closeParen = takeOne(isSymbol(TokenSymbol.closeParen));

const group: ParseResult<Expression> = map(
  chain(openParen, lazy(() => expression), closeParen),
  spread(groupExpression),
);
const groupPrecedence = oneOf(group, number);

const exponentialOperation: ParseResult<Expression> = map(
  chain(groupPrecedence, power, lazy(() => exponentPrecedence)),
  spread(operationExpression),
);
const exponentPrecedence = oneOf(exponentialOperation, groupPrecedence);

const productOperation: ParseResult<Expression> = leftRecurse(exponentPrecedence, map(
  chain(lazy(() => productPrecedence), oneOf(times, divide), exponentPrecedence),
  spread(operationExpression),
));
const productPrecedence = oneOf(productOperation, exponentPrecedence);

const sumOperation: ParseResult<Expression> = leftRecurse(productPrecedence, map(
  chain(lazy(() => sumPrecedence), oneOf(plus, minus), productPrecedence),
  spread(operationExpression),
));
const sumPrecedence = oneOf(sumOperation, productPrecedence);

const expression = sumPrecedence;

/**
 * Converts an array of strings into a list of tokens that are then run through the ast expression
 * parsers.
 */
export function parse(strings: string[]): Expression | Token | undefined {
  // Parse each of the strings into a token
  const tokens: Token[] = strings.map((string) => {
    if (/^[0-9]+$/.test(string)) {
      return numberLit(+string);
    }
    if (Object.values(TokenSymbol).includes(string)) {
      return string as TokenSymbol;
    }
    throw new Error('Unrecognized syntax');
  });

  return runParser(expression, tokens);
}
