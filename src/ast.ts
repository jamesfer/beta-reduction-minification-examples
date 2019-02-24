/**************************************************
 * Monads
 **************************************************/

/**
 * The type of monad being used. Useful for differentiating different kinds of monads.
 */
enum MonadType {
  Maybe,
  State,
}

/**
 * Base class for all monad objects.
 */
interface BaseMonad<T extends MonadType> {
  monadType: MonadType,
}


/**
 * The state monad is used to compose operations that each rely on a global mutable state.
 */
interface State<S, V> extends BaseMonad<MonadType.State> {
  fn: (state: S) => [S, V],
}

interface StateT<S, V> {
  fn: (state: S) => Maybe<[S, V]>
}

/**
 * Creates a state monad.
 */
function state<S, V>(fn: (state: S) => [S, V]): State<S, V> {
  return {
    fn,
    monadType: MonadType.State,
  };
}

/**
 * Executes a state monad starting with the given initial state and returns the result.
 */
function runState<S, V>(state: S, monad: State<S, V>): V {
  return monad.fn(state)[1];
}

/**
 * Composes two state monad values together to produce another. The first is an existing monad
 * value that is unwrapped and passed into the function which produces the second.
 */
function bindState<S, V1, V2>(monad: State<S, V1>, fn: (value: V1) => State<S, V2>): State<S, V2> {
  return state((s) => {
    const result = monad.fn(s);
    return fn(result[1]).fn(result[0]);
  });
}

/**
 * Wraps a plain value in an empty state monad.
 */
function returnState<V>(value: V): State<any, V> {
  return state(s => [s, value]);
}


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
interface Nothing extends BaseMonad<MonadType.Maybe> {
  maybeConstructor: MaybeConstructor.Nothing,
}

/**
 * The nothing constant is used as a representation of Nothing so duplicate objects do not need to
 * be created.
 */
const nothing: Nothing = {
  monadType: MonadType.Maybe,
  maybeConstructor: MaybeConstructor.Nothing,
};

/**
 * Just indicates that the maybe monad actually does contain a value.
 */
interface Just<T> extends BaseMonad<MonadType.Maybe> {
  maybeConstructor: MaybeConstructor.Just,
  value: T
}

/**
 * Convenient constructor for producing a Just object that wraps value.
 */
function just<T>(value: T): Just<T> {
  return {
    value,
    monadType: MonadType.Maybe,
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
    : just(fn(monad.value));
}

function returnMaybe<T>(value: T): Maybe<T> {
  return just(value);
}

/**
 * Converts a maybe monad value into a plain value, or null if the monad was nothing.
 */
function runMaybe<T>(monad: Maybe<T>): T | null {
  return monad.maybeConstructor === MaybeConstructor.Nothing ? null : monad.value;
}



type Combined<S, V> = State<S, Maybe<V>>;

function bindCombined<T, R1, R2>(combined: Combined<T, R1>, fn: (value: R1) => Combined<T, R2>): Combined<T, R2> {
  return bindState(combined, (monad): Combined<T, R2> => {
    return monad.maybeConstructor === MaybeConstructor.Nothing
      ? returnState(nothing)
      : fn(monad.value);
  });
}

function returnCombined<V>(value: V): Combined<any, V> {
  return returnState(just(value));
}

export function runCombined<S, R>(state: S, monad: Combined<S, R>): R | null {
  return runMaybe(runState(state, monad));
}



type ParserMonad<V> = (previousCalls: PreviousCalls) => (state: Token[]) => Maybe<[Token[], V]>;

function bindParser<T, U>(monad: ParserMonad<T>, operation: (value: T) => ParserMonad<U>): ParserMonad<U> {
  return previousCalls => tokens => (
    bindMaybe(monad(previousCalls)(tokens), result => (
      operation(result[1])(previousCalls)(result[0])
    ))
  );
}

function returnParser<T>(value: T): ParserMonad<T> {
  return previousCalls => tokens => just<[Token[], T]>([tokens, value])
}

function protectParser<T>(monad: ParserMonad<T>): ParserMonad<T> {
  return previousCalls => (tokens): Maybe<[Token[], T]> => {
    return previousCallExists(previousCalls, monad, tokens)
      ? nothing
      : monad(extendPreviousCalls(previousCalls, monad, tokens))(tokens);
  };
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
  times = '*',
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


export type ASTMonad<V> = Combined<Token[], V>;

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
 * Wraps a value and only executes it when needed. Useful for grammars that recursively reference
 * their syntax elements.
 */
function lazy<R>(value: () => ParserMonad<R>): ParserMonad<R> {
  return bindParser(returnParser(null), value);
}

/**
 * Produces a new matcher that will alter the value of an existing matcher when it matches anything
 * using a mapping function.
 */
function map<T, U>(matcher: ASTMonad<T>, mapper: (value: T) => U): ASTMonad<U> {
  return bindCombined(matcher, value => returnCombined(mapper(value)));
}

/**
 * Extracts the next token from the incoming stream if it satisfies a predicate and returns it as
 * the value. If the next token doesn't pass the predicate, returns nothing.
 */
function takeOne<T extends Token>(predicate: (token: Token) => token is T): ASTMonad<T>;
function takeOne(predicate: (t: Token) => boolean): ASTMonad<Token> {
  return state<Token[], Maybe<Token>>(tokens => (
    tokens.length > 0 && predicate(tokens[0])
      ? [tokens.slice(1), just(tokens[0])]
      : [tokens, nothing]
  ));
}

/**
 * Returns the first match found by going through the given matchers in order.
 */
function oneOf<R>(...matchers: ASTMonad<R>[]): ASTMonad<R> {
  return matchers.reduce<ASTMonad<R>>(
    (previousValue, processor) => (
      bindState(previousValue, maybePrevious => (
        maybePrevious.maybeConstructor === MaybeConstructor.Nothing
          ? processor
          : returnState(maybePrevious)
      ))
    ),
    returnState(nothing),
  );
}

/**
 * Matches all of the given matchers in order or returns nothing if any of them fail.
 */
function chain<R>(...matchers: ASTMonad<R>[]): ASTMonad<R[]> {
  const combinedMatchers = matchers.reduce<ASTMonad<R[]>>(
    (previousValue, processor) => (
      bindCombined(previousValue, previous => (
        bindCombined(processor, current => returnCombined([...previous, current]))
      ))
    ),
    returnCombined([] as R[]),
  );

  return state<Token[], Maybe<R[]>>((s) => {
    const result = combinedMatchers.fn(s);
    return result[1].maybeConstructor === MaybeConstructor.Nothing
      ? [s, nothing]
      : result;
  });
}



interface PreviousCalls {
  parent?: PreviousCalls,
  matcher: any,
  value: any,
}

function previousCallExists(previousCalls: PreviousCalls, matcher: any, value: any): boolean {
  if (previousCalls.matcher === matcher && previousCalls.value === value) {
    return true;
  }

  if (previousCalls.parent) {
    return previousCallExists(previousCalls.parent, matcher, value);
  }

  return false;
}

function extendPreviousCalls(previousCalls: PreviousCalls, matcher: any, value: any): PreviousCalls {
  return {
    matcher,
    value,
    parent: previousCalls,
  };
}



function protect<V>(matcher: (tokens: Token[]) => Maybe<[Token[], V]>): (previousCalls: PreviousCalls) => (tokens: Token[]) => Maybe<[Token[], [PreviousCalls, V]]> {
  return (previousCalls): (tokens: Token[]) => Maybe<[Token[], [PreviousCalls, V]]> => {
    return (tokens): Maybe<[Token[], [PreviousCalls, V]]> => {
      if (previousCallExists(previousCalls, matcher, tokens)) {
        return nothing;
      }
      return bindMaybe(matcher(tokens), result => (
        returnMaybe<[Token[], [PreviousCalls, V]]>([
          result[0], [extendPreviousCalls(previousCalls, matcher, tokens), result[1]]
        ])
      ))
    }
  }
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
type Expression =
  | IdentifierExpression
  | OperationExpression
  | NumberLiteralExpression;

/**
 * Convenient constructor for making an identifier expression.
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
 */
function numberLiteralExpression(number: NumberLiteral): NumberLiteralExpression {
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
function operationExpression(left: Expression, op: Expression, right: Expression): OperationExpression {
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
 * Convenient constructor for making an operation expression from an array of three expressions.
 */
function operationExpressionMapper(expressions: Expression[]): OperationExpression {
  return operationExpression(expressions[0], expressions[1], expressions[2]);
}





/**************************************************
 * AST expression patterns
 **************************************************/


const plus = map(takeOne(isSymbol(TokenSymbol.plus)), identifierExpression);
const times = map(takeOne(isSymbol(TokenSymbol.times)), identifierExpression);
const power = map(takeOne(isSymbol(TokenSymbol.power)), identifierExpression);
const number = map(takeOne(isNumberLiteral), numberLiteralExpression);
const openParen = takeOne(isSymbol(TokenSymbol.openParen));
const closeParen = takeOne(isSymbol(TokenSymbol.closeParen));

const exponential: ASTMonad<any> = oneOf<Expression[]>(
  chain<Expression>(number, power, lazy(() => exponential)),
  number as ASTMonad<Expression[]>,
);
const product: ASTMonad<any> = oneOf<Expression>(
  map(chain<Expression>(exponential, times, lazy(() => product)), operationExpressionMapper),
  exponential,
);
const sum: ASTMonad<any> = oneOf<Expression>(
  map(chain<Expression>(product, plus, lazy(() => sum)), operationExpressionMapper),
  product,
);
const operation: ASTMonad<any> = sum;

const grouping: ASTMonad<any> = chain(openParen, lazy(() => expression), closeParen);

const expression: ASTMonad<any> = oneOf(grouping, operation);


/**
 * Converts an array of strings into a list of tokens that are then run through the ast expression
 * parsers.
 */
export function parse(strings: string[]): Expression | Token | null {
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

  return runCombined(tokens, expression);
}

