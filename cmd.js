#!/usr/bin/env node

const fs = require('fs')
const path = require('path')
const yargs = require('yargs')
const retyped = require('./retyped_node')

yargs
  .usage('retyped')
  .version(require('./package.json').version)
  .command('compile [files...]', 'Generate BuckleScript interfaces from a file', {}, compileFiles)
  .option('flow-typed', {
    describe: 'Generate interfaces from the flow-typed directory'
  })
  .demandCommand(1, '')
  .help()
  .argv

function compileFiles ({ files }) {
  files.forEach(compileFile)
}

function compileFile (filePath) {
  return new Promise((resolve, reject) => {
    fs.readFile(filePath, (err, data) => {
      if (err) return reject(err)

      const fileName = path.basename(filePath)
      const [moduleName, _, bsCode] = retyped.compile(fileName, data.toString())

      const writeDirName = path.dirname(filePath)
      const writeFileName = path.join(writeDirName, moduleName + '.re')

      console.log(`Writing ${filePath} -> ${writeFileName}`)
      fs.writeFile(writeFileName, bsCode, err => {
        if (err) return reject(err)

        resolve()
      })
    })
  })
}
