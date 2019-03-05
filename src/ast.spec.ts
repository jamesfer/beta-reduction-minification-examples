import {
  Expression,
  groupExpression,
  identifierExpression,
  numberLit,
  numberLiteralExpression,
  operationExpression,
  parse,
  TokenSymbol,
} from './ast';

const plusIdentifier = identifierExpression(TokenSymbol.plus);
const minusIdentifier = identifierExpression(TokenSymbol.minus);
const timesIdentifier = identifierExpression(TokenSymbol.times);
const divideIdentifier = identifierExpression(TokenSymbol.divide);
const powerIdentifier = identifierExpression(TokenSymbol.power);

function makeNumberLiteral(number: number) {
  return numberLiteralExpression(numberLit(number));
}

function makeGroup(expression: Expression): Expression {
  return groupExpression(
    TokenSymbol.openParen,
    expression,
    TokenSymbol.closeParen,
  )
}

describe('parse', () => {
  it('should parse simple numbers', () => {
    expect(parse(['1'])).toEqual(makeNumberLiteral(1));
  });

  it('should parse large numbers', () => {
    expect(parse(['4535'])).toEqual(makeNumberLiteral(4535));
  });

  it('should parse plus expressions', () => {
    expect(parse(['23', '+', '32'])).toEqual(operationExpression(
      makeNumberLiteral(23),
      plusIdentifier,
      makeNumberLiteral(32),
    ));
  });

  it('should parse times expressions', () => {
    expect(parse(['23', '*', '32'])).toEqual(operationExpression(
      makeNumberLiteral(23),
      timesIdentifier,
      makeNumberLiteral(32),
    ));
  });

  it('should parse exponential expressions', () => {
    expect(parse(['23', '^', '32'])).toEqual(operationExpression(
      makeNumberLiteral(23),
      powerIdentifier,
      makeNumberLiteral(32),
    ));
  });

  it('should parse multiple expressions with correct order of operations', () => {
    expect(parse(['23', '+', '32', '*', '2'])).toEqual(operationExpression(
      makeNumberLiteral(23),
      plusIdentifier,
      operationExpression(
        makeNumberLiteral(32),
        timesIdentifier,
        makeNumberLiteral(2),
      ),
    ));
  });

  it('should parse groupings expressions', () => {
    expect(parse(['(', '23', '+', '32', ')', '*', '2'])).toEqual(operationExpression(
      makeGroup(operationExpression(
        makeNumberLiteral(23),
        plusIdentifier,
        makeNumberLiteral(32),
      )),
      timesIdentifier,
      makeNumberLiteral(2),
    ));
  });

  it('should parse groups in exponent position', () => {
    expect(parse(['100', '^', '(', '23', '+', '32', ')'])).toEqual(operationExpression(
      makeNumberLiteral(100),
      powerIdentifier,
      makeGroup(operationExpression(
        makeNumberLiteral(23),
        plusIdentifier,
        makeNumberLiteral(32),
      )),
    ));
  });

  it('should parse an expression before an exponent', () => {
    expect(parse(['4', '/', '10', '^', '(', '23', '+', '2', '*', '5', ')'])).toEqual(operationExpression(
      makeNumberLiteral(4),
      divideIdentifier,
      operationExpression(
        makeNumberLiteral(10),
        powerIdentifier,
        makeGroup(operationExpression(
          makeNumberLiteral(23),
          plusIdentifier,
          operationExpression(
            makeNumberLiteral(2),
            timesIdentifier,
            makeNumberLiteral(5),
          )
        )),
      ),
    ));
  });

  it('should correctly associate consecutive subtractions', () => {
    expect(parse(['10', '-', '2', '-', '5'])).toEqual(operationExpression(
      operationExpression(
        makeNumberLiteral(10),
        minusIdentifier,
        makeNumberLiteral(2),
      ),
      minusIdentifier,
      makeNumberLiteral(5),
    ));
  });

  it('should correctly associate consecutive divisions', () => {
    expect(parse(['2', '/', '5', '/', '7', '/', '8', '/', '1', '/', '4'])).toEqual(operationExpression(
      operationExpression(
        operationExpression(
          operationExpression(
            operationExpression(
              makeNumberLiteral(2),
              divideIdentifier,
              makeNumberLiteral(5),
            ),
            divideIdentifier,
            makeNumberLiteral(7),
          ),
          divideIdentifier,
          makeNumberLiteral(8),
        ),
        divideIdentifier,
        makeNumberLiteral(1),
      ),
      divideIdentifier,
      makeNumberLiteral(4),
    ));
  });

  it('should parse a complex grouped equation', () => {
    expect(parse(['(', '5', '-', '5', ')', '*', '(', '3', '+', '4', ')', '/', '1'])).toEqual(
      operationExpression(
        operationExpression(
          makeGroup(operationExpression(
            makeNumberLiteral(5),
            minusIdentifier,
            makeNumberLiteral(5),
          )),
          timesIdentifier,
          makeGroup(operationExpression(
            makeNumberLiteral(3),
            plusIdentifier,
            makeNumberLiteral(4),
          )),
        ),
        divideIdentifier,
        makeNumberLiteral(1),
      ),
    );
  });

  it('should parse lots of nested groups', () => {
    const number1 = makeNumberLiteral(1);
    expect(parse(['(', '(', '(', '1', ')', ')', ')'])).toEqual({
      ...number1,
      tokens: [
        TokenSymbol.openParen,
        TokenSymbol.openParen,
        TokenSymbol.openParen,
        ...number1.tokens,
        TokenSymbol.closeParen,
        TokenSymbol.closeParen,
        TokenSymbol.closeParen,
      ],
    });
  });

  it('should return undefined if the expression contains mismatched braces', () => {
    expect(parse(['(', '23', '+', '32', '*', '2'])).toBe(undefined);
  });
});
