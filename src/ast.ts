/**************************************************
 * Function utils
 **************************************************/

function constant<T>(value: T): () => T {
  return () => value;
}

function compose<A, B, T>(outer: (b: B) => T, inner: (a: A) => B): (a: A) => T {
  return a => outer(inner(a));
}

function spread<T1, R>(fn: (a1: T1) => R): (args: [T1]) => R;
function spread<T1, T2, R>(fn: (a1: T1, a2: T2) => R): (args: [T1, T2]) => R;
function spread<T1, T2, T3, R>(fn: (a1: T1, a2: T2, a3: T3) => R): (args: [T1, T2, T3]) => R;
function spread<T, R>(fn: (...args: T[]) => R): (args: T[]) => R {
  return (args) => fn(...args);
}





/**************************************************
 * Monads
 **************************************************/

/**
 * Specifies the different kind of values that comprise the maybe monad.
 */
enum MaybeConstructor {
  Nothing,
  Just,
}

/**
 * Nothing is a special value that indicates that an operation produced no value.
 */
interface Nothing {
  maybeConstructor: MaybeConstructor.Nothing,
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
  maybeConstructor: MaybeConstructor.Just,
  value: T
}

/**
 * Convenient constructor for producing a Just object that wraps value.
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
 */
function bindMaybe<T, R>(monad: Maybe<T>, fn: (value: T) => Maybe<R>): Maybe<R> {
  return monad.maybeConstructor === MaybeConstructor.Nothing
    ? nothing as Maybe<any>
    : fn(monad.value);
}

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
  parent?: FallbackList,
  matcher: ParseResult<any>,
  tokens: Token[],
  fallback: ParseResult<any>,
}

function fallbackExists(fallbackList: FallbackList | undefined, matcher: ParseResult<any>, tokens: Token[]): ParseResult<any> | undefined {
  return !fallbackList
    ? undefined
    : fallbackList.matcher === matcher && fallbackList.tokens === tokens
      ? fallbackList.fallback
      : fallbackExists(fallbackList.parent, matcher, tokens);
}

function addFallback(parent: FallbackList | undefined, matcher: ParseResult<any>, tokens: Token[], fallback: ParseResult<any>): FallbackList {
  return { parent, matcher, tokens, fallback };
}







type ParseResult<V> = (previousCalls: FallbackList | undefined) => (state: Token[]) => Maybe<[Token[], V]>;

function liftA2<A, B, T>(fn: (a: A, b: B) => ParseResult<T>): (a: ParseResult<A>, b: ParseResult<B>) => ParseResult<T> {
  return (monadA, monadB) => bindResult(monadA, a => bindResult(monadB, b => fn(a, b)));
}

function bindResult<T, U>(monad: ParseResult<T>, operation: (value: T) => ParseResult<U>): ParseResult<U> {
  return previousCalls => tokens => (
    bindMaybe(monad(previousCalls)(tokens), result => (
      operation(result[1])(previousCalls)(result[0])
    ))
  );
}

function bindResultA2<T1, T2, U>(monad1: ParseResult<T1>, monad2: ParseResult<T2>, operation: (value1: T1, value2: T2) => ParseResult<U>): ParseResult<U> {
  return bindResult(monad1, value1 => bindResult(monad2,  value2 => operation(value1, value2)));
}

function replaceResultIfNothing<T, U>(monad: ParseResult<T>, operation: ParseResult<T>): ParseResult<T> {
  return previousCalls => tokens => (
    fallbackIfNothing(monad(previousCalls)(tokens), () => operation(previousCalls)(tokens))
  );
}

function wrapResult<T>(value: T): ParseResult<T> {
  return constant((tokens: Token[]) => just<[Token[], T]>([tokens, value]));
}

function createNothingResult(): ParseResult<any> {
  return constant(constant(nothing));
}

/**
 * Helper function used within recurse.
 */
function createInnerRecurse<T>(matcher: ParseResult<T>): (previousCalls: FallbackList, lastResult?: T | undefined) => ParseResult<any> {
  return function recurse(recursivePreviousCalls: FallbackList, lastResult: T | undefined = undefined): ParseResult<any> {
    return replaceResultIfNothing(
      bindResultA2(constant(matcher(recursivePreviousCalls)), askTokens(), (result, tokens) => recurse(
        // Add new fallback
        addFallback(recursivePreviousCalls, matcher, tokens, wrapResult(result)),
        result,
      )),
      lastResult ? wrapResult(lastResult) : createNothingResult(),
    )
  }
}

/**
 * Supports basic protection from infinite recursion when writing left recurse patterns
 */
function leftRecurse<T, U>(baseCase: ParseResult<U>, matcher: ParseResult<T>): ParseResult<T | U> {
  const innerRecurse = createInnerRecurse(matcher);
  return previousCalls => tokens => {
    // Check if any protection is already in place
    const previousFallback = fallbackExists(previousCalls, matcher, tokens);
    return previousFallback !== undefined
      ? previousFallback(previousCalls)(tokens)
      // Recurse with a new fallback
      : innerRecurse(addFallback(previousCalls, matcher, tokens, baseCase))(previousCalls)(tokens)
  }
}

function askTokens(): ParseResult<Token[]> {
  return constant((tokens: Token[]) => just<[Token[], Token[]]>([tokens, tokens]));
}

function setTokens(tokens: Token[]): ParseResult<undefined> {
  return constant(constant(just<[Token[], undefined]>([tokens, undefined])));
}

function runParser<T>(parser: ParseResult<T>, tokens: Token[], previousCalls: FallbackList | undefined = undefined): T | undefined {
  const result = runMaybe(parser(previousCalls)(tokens));
  return result ? result[1] : undefined;
}








/**************************************************
 * Language utilities
 **************************************************/

/**
 * Indicates the type of an artifact.
 */
enum ArtifactType {
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
interface NumberLiteral {
  type: ArtifactType.Token,
  value: number,
}

/**
 * Convenient constructor for producing a number literal object.
 */
export function numberLit(value: number): NumberLiteral {
  return { value, type: ArtifactType.Token };
}

/**
 * Tokens make up the building blocks of the language. It is either a simple TokenSymbol or a
 * NumberLiteral.
 */
type Token =
  | TokenSymbol
  | NumberLiteral;


/**
 * Returns true if a token is a TokenSymbol.
 */
function isSymbol(symbol: TokenSymbol): (token: Token) => token is TokenSymbol {
  return (token): token is TokenSymbol => token === symbol;
}

/**
 * Returns true if a token is a NumberLiteral
 */
function isNumberLiteral(token: Token): token is NumberLiteral {
  return typeof token === 'object';
}

/**
 * Returns true if the next token passes the predicate
 */
function nextTokenIs(tokens: Token[], predicate: (token: Token) => boolean): boolean {
  return tokens.length > 0 && predicate(tokens[0]);
}

/**
 * Wraps a value and only executes it when needed. Useful for grammars that recursively reference
 * their syntax elements.
 */
function lazy<R>(value: () => ParseResult<R>): ParseResult<R> {
  return bindResult(wrapResult(undefined), value);
}

/**
 * Produces a new matcher that will alter the value of an existing matcher when it matches anything
 * using a mapping function.
 */
function map<T, U>(matcher: ParseResult<T>, mapper: (value: T) => U): ParseResult<U> {
  return bindResult(matcher, compose<T, U, ParseResult<U>>(wrapResult, mapper));
}

/**
 * Extracts the next token from the incoming stream if it satisfies a predicate and returns it as
 * the value. If the next token doesn't pass the predicate, returns nothing.
 */
function takeOne<T extends Token>(predicate: (token: Token) => token is T): ParseResult<T>;
function takeOne(predicate: (t: Token) => boolean): ParseResult<Token> {
  return bindResult(askTokens(), tokens => (
    nextTokenIs(tokens, predicate)
      ? bindResult(setTokens(tokens.slice(1)), constant(wrapResult(tokens[0])))
      : createNothingResult()
  ));
}

interface Match<T> {
  tokens: Token[],
  result: T,
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
  return p => t => {
    const allMatches = matchers.reduce<ParseResult<Match<R>[]>>(
      (all, current) => bindResult(all, array => (
        // Skip the matcher if it fails
        replaceResultIfNothing(
          bindResultA2(constant(constant(current(p)(t))), askTokens(), (result, tokens) => (
            // Combine each of the results into an array
            wrapResult([...array, { tokens, result }])
          )),
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
          longest => bindResult(setTokens(longest.tokens), constant(wrapResult(longest.result)))
        ),
    );

    return selectLongestMatch(p)(t);
  }
}

/**
 * Matches all of the given matchers in order or returns nothing if any of them fail.
 */
function chain<T1>(m1: ParseResult<T1>): ParseResult<[T1]>;
function chain<T1, T2>(m1: ParseResult<T1>, m2: ParseResult<T2>): ParseResult<[T1, T2]>;
function chain<T1, T2, T3>(m1: ParseResult<T1>, m2: ParseResult<T2>, m3: ParseResult<T3>): ParseResult<[T1, T2, T3]>;
function chain<R>(...matchers: ParseResult<R>[]): ParseResult<R[]> {
  return matchers.reduce(
    liftA2<R[], R, R[]>((previous, current) => wrapResult([...previous, current])),
    wrapResult([] as R[]),
  );
}









/**************************************************
 * Language utilities
 **************************************************/


/**
 * The three kinds of expressions in the language.
 */
enum ExpressionType {
  Operation = 'Operation',
  Identifier = 'Identifier',
  NumberLiteral = 'NumberLiteral',
}

/**
 * An identifier expression is used whenever a token that references a function is used.
 */
interface IdentifierExpression {
  type: ArtifactType.Expression,
  expressionType: ExpressionType.Identifier,
  name: string,
  tokens: Token[],
}

/**
 * An operation expression is like a function call.
 */
interface OperationExpression {
  type: ArtifactType.Expression,
  expressionType: ExpressionType.Operation,
  left: Expression,
  right: Expression,
  op: Expression,
  tokens: Token[],
}

/**
 * The number literal expression just wraps a number literal token.
 */
interface NumberLiteralExpression {
  type: ArtifactType.Expression,
  expressionType: ExpressionType.NumberLiteral,
  value: number,
  tokens: Token[],
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
 */
export function identifierExpression(name: TokenSymbol): IdentifierExpression {
  return {
    name,
    type: ArtifactType.Expression,
    expressionType: ExpressionType.Identifier,
    tokens: [name],
  };
}

/**
 * Convenient constructor for making a number literal expression.
 */
export function numberLiteralExpression(number: NumberLiteral): NumberLiteralExpression {
  return {
    type: ArtifactType.Expression,
    expressionType: ExpressionType.NumberLiteral,
    value: number.value,
    tokens: [number],
  }
}

/**
 * Convenient constructor for making an operation expression.
 */
export function operationExpression(left: Expression, op: Expression, right: Expression): OperationExpression {
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
 */
export function groupExpression(leftParen: Token, expression: Expression, rightParen: Token): Expression {
  return {
    ...expression,
    tokens: [leftParen, ...expression.tokens, rightParen],
  };
}





/**************************************************
 * AST expression patterns
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
  const tokens: Token[] = strings.map(string => {
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

