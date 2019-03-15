import { performance } from 'perf_hooks';
import { parse } from './ast';

const inputs: string[][] = [
  ['1', '+', '1'],
  ['(', '34', '-', '23', ')', '*', '2'],
  ['4', '/', '10', '^', '(', '23', '+', '2', '*', '5', ')'],
  ['2', '/', '5', '/', '7', '/', '8', '/', '1', '/', '3'],
  ['(', '5', '-', '5', ')', '*', '(', '3', '+', '4', ')', '/', '1'],
];

const iterationCount = 10;
const inputResults = inputs.map(input => ({
  input,
  totalTime: 0,
}));

for (let iteration = 0; iteration < iterationCount; iteration += 1) {
  for (let test = 0; test < inputs.length; test += 1) {
    const before = performance.now();
    parse(inputs[test]);
    const after = performance.now();
    inputResults[test].totalTime += after - before;
  }
}

inputResults.forEach(({ input, totalTime }) => {
  console.log('Input:       ', input.join(''));
  console.log('Total time:  ', `${totalTime}`.slice(0, 4));
  console.log('Average time:', `${totalTime / iterationCount}`.slice(0, 4));
  console.log();
});
