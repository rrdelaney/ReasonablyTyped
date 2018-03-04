#!/usr/bin/env node

const fs = require('fs')
const path = require('path')
const meow = require('meow')
const codeFrame = require('babel-code-frame')
const chalk = require('chalk')
const ReasonablyTyped = require('./')

const cli = meow(
  `
        Usage:
          $ retyped ...files

        Examples:
          $ retyped file1.js file2.js file3.d.ts
`,
  {flags: {debug: {type: 'boolean'}}},
)

const debugMode = cli.flags.debug === true

compileFiles(cli.input)

function compileFiles(files) {
  Promise.all(files.map(compileFile)).then(result => {
    const filesWritten = result.filter(s => s === true).length
    const errorCount = result.filter(s => s === false).length

    console.log(
      `\nCompiled ${chalk.yellow(`${filesWritten}`)} file${
        filesWritten !== 1 ? 's' : ''
      }`,
    )

    if (errorCount > 0) {
      console.log(
        `There were errors with ${chalk.red(`${errorCount}`)} file${
          errorCount > 1 ? 's' : ''
        }`,
      )
    }
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
      const fileSource = data.toString()
      let moduleName
      let bsCode

      try {
        const result = ReasonablyTyped.compile(
          fileSource,
          fileName,
          false,
          debugMode,
        )

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
          codeFrame(fileSource, parseInt(lineNumber), parseInt(colNumber), {
            highlightCode: true,
          }),
        )
        console.log()

        return resolve(false)
      }

      if (debugMode) return resolve(null)

      const writeDirName = path.dirname(filePath)
      const writeFileName = path.join(writeDirName, moduleName + '.re')

      console.log(
        `${chalk.green('✓')} ${filePath} -> ${chalk.underline(writeFileName)}`,
      )

      fs.writeFile(writeFileName, bsCode, err => {
        if (err) return reject(err)

        resolve(true)
      })
    })
  })
}
