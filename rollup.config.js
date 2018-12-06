import nodeResolve from 'rollup-plugin-node-resolve';
import commonjs from 'rollup-plugin-commonjs';

import { terser } from "rollup-plugin-terser";

export default {
  input: 'src/virtualdom.bs.js',
  output: {
    file: 'index.js',
    format: 'iife',
    name: 'virtualdom'
  },
  plugins: [
    nodeResolve({module: true, browser: true}),
    commonjs()
    //terser()
  ]
};
