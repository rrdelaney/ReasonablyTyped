#!/usr/bin/env node

const fs = require('fs')
const path = require('path')
const yargs = require('yargs')
const retyped = require('./retyped_node')
const refmt = require('./refmt_node')

yargs
  .usage('retyped')
  .version(require('./package.json').version)
  .command('compile [files...]', 'Generate BuckleScript interfaces from a file', {}, compileFiles)
  .option('flow-typed', {
    describe: 'Generate interfaces from the flow-typed directory'
  })
  .option('no-fmt', {
    describe: 'Don\'t run the resulting code through refmt'
  })
  .demandCommand(1, '')
  .help()
  .argv

function compileFiles ({ files: argsFiles, flowTyped, fmt = true }) {
  const flowTypedFiles = flowTyped ? getFlowFiles() : Promise.resolve([])

  flowTypedFiles
    .then(flowFiles => [...argsFiles, ...flowFiles])
    .then(allFiles => allFiles.map(f => compileFile(f, fmt)))
    .then(compiledFiles => Promise.all(compiledFiles))
    .then(result => {
      console.log(`Compiled ${result.length} file${result.length > 1 ? 's' : ''}`)
    })
    .catch(err => {
      console.error(err.toString())
    })
}

function getFlowFiles () {
  const flowTypedPath = path.join('flow-typed', 'npm')

  return new Promise((resolve, reject) => {
    fs.readdir(flowTypedPath, (err, results) => {
      if (err) return reject(err)

      const flowTyped = results
        .filter(f => f.endsWith('.js'))
        .map(p => path.join(flowTypedPath, p))

      resolve(flowTyped)
    })
  })
}

function compileFile (filePath, shouldFmt) {
  return new Promise((resolve, reject) => {
    fs.readFile(filePath, (err, data) => {
      if (err) return reject(err)

      const fileName = path.basename(filePath)
      const [moduleName, _flowCode, unfmtCode] = retyped.compile(fileName, data.toString())
      const [_fmtType, bsCode] = refmt.refmt(unfmtCode, 'RE', 'implementation', 'RE')

      if (shouldFmt && bsCode.includes('<SYNTAX ERROR>')) {
        console.error(`Error in ${filePath}:`)
        console.error(bsCode)
        return resolve()
      }

      const writeDirName = path.dirname(filePath)
      const writeFileName = path.join(writeDirName, moduleName + '.re')
      const writeCode = shouldFmt ? bsCode : unfmtCode

      console.log(`${filePath} -> ${writeFileName}`)
      fs.writeFile(writeFileName, writeCode, err => {
        if (err) return reject(err)

        resolve()
      })
    })
  })
}
