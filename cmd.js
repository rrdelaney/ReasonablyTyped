#!/usr/bin / env node
const yargs = require('yargs')
const retyped = require('./retyped_node')

const a = yargs
  .usage('$0 <cmd> [args]')
  .command(
    'generate [files...]',
    'Generate BuckleScript interfaces from a file'
  )
  .help()
  .argv

console.log(a)