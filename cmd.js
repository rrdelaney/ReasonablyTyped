const { readFileSync } = require('fs')
const retyped = require('./retyped_node')

const fname = 'test.js'
const fcont = readFileSync(fname).toString()

const output = retyped.compile(fname, fcont)
console.log(output)