import { performance } from 'perf_hooks';
import { parse } from './ast';
import { inputs } from './benchmark-inputs';

const iterationCount = 10;

const inputResults = inputs.map(input => ({
  input,
  totalTime: 0,
}));

for (let iteration = 0; iteration < iterationCount; iteration++) {
  for (let test = 0; test < inputs.length; test++) {
    const before = performance.now();
    parse(inputs[test]);
    const after = performance.now();
    inputResults[test].totalTime += after - before;
  }
}

inputResults.forEach(({ input, totalTime }) => {
  console.log('Input:       ', input.join(''));
  console.log('Total time:  ', ('' + totalTime).slice(0, 4));
  console.log('Average time:', ('' + totalTime / iterationCount).slice(0, 4));
  console.log();
});


