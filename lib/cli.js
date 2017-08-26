#!/usr/bin/env node

const fs = require('fs')
const path = require('path')
const yargs = require('yargs')
const codeFrame = require('babel-code-frame')
const chalk = require('chalk')
const ReasonablyTyped = require('./')

yargs
  .usage('retyped')
  .version(require('../package.json').version)
  .command(
    'compile [files...]',
    'Generate BuckleScript interfaces from a file',
    {},
    compileFiles
  )
  .option('flow-typed', {
    describe: 'Generate interfaces from the flow-typed directory',
    boolean: true
  })
  .demandCommand(1, '')
  .help().argv

function compileFiles({ files: argsFiles, flowTyped }) {
  const flowTypedFiles = flowTyped ? getFlowFiles() : Promise.resolve([])

  flowTypedFiles
    .then(flowFiles => [...argsFiles, ...flowFiles])
    .then(allFiles => allFiles.map(compileFile))
    .then(compiledFiles => Promise.all(compiledFiles))
    .then(result => {
      const filesWritten = result.filter(s => s === true).length
      const errorCount = result.filter(s => s === false).length

      console.log(
        `\nCompiled ${chalk.yellow(`${filesWritten}`)} file${filesWritten > 1
          ? 's'
          : ''}`
      )

      if (errorCount > 0) {
        console.log(
          `There were errors with ${chalk.red(
            `${errorCount}`
          )} file${errorCount > 1 ? 's' : ''}`
        )
      }
    })
    .catch(err => {
      console.error(err)
    })
}

function getFlowFiles() {
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

function compileFile(filePath) {
  return new Promise((resolve, reject) => {
    fs.readFile(filePath, (err, data) => {
      if (err) {
        console.log(`${chalk.red('✘')} Cannot read file "${filePath}"`)
        return resolve(false)
      }

      const fileName = path.basename(filePath)
      const flowCode = data.toString()
      let moduleName
      let bsCode

      try {
        const result = ReasonablyTyped.compile(flowCode, fileName, true)

        moduleName = result[0]
        bsCode = result[1]
      } catch (e) {
        const fileLocation = e.message.match(/\[in.*from (\d+):(\d+).*\]/)
        if (!fileLocation) return reject(e)

        const [, lineNumber, colNumber] = fileLocation
        const [, ...extractedError] = e.message.split(':')
        const errorMessage = extractedError.length
          ? extractedError.join(':')
          : e.message

        console.log()
        console.log(chalk.red(`✘ ${errorMessage}`))
        console.log()
        console.log(
          codeFrame(flowCode, parseInt(lineNumber), parseInt(colNumber), {
            highlightCode: true
          })
        )
        console.log()

        return resolve(false)
      }

      const writeDirName = path.dirname(filePath)
      const writeFileName = path.join(writeDirName, moduleName + '.re')

      console.log(
        `${chalk.green('✓')} ${filePath} -> ${chalk.underline(writeFileName)}`
      )

      fs.writeFile(writeFileName, bsCode, err => {
        if (err) return reject(err)

        resolve(true)
      })
    })
  })
}
