import nodeResolve from 'rollup-plugin-node-resolve';
import commonjs from 'rollup-plugin-commonjs';

export default {
  input: 'src/main.bs.js',
  output: {
    file: 'js/app.js',
    format: 'iife',
    name: 'todo'
  },
  plugins: [
    nodeResolve({module: true, browser: true}),
    commonjs()
  ]
};

