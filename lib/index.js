const reason = require('reason')
const codeFrame = require('babel-code-frame')
const Retyped = require('./js/src/compiler')

/**
 * Runs `refmt` on a string of Reason code
 *
 * @param {string} source Reason source code
 * @return {string} Formatted Reason code
 */
function format(source) {
  var fmtedCode = 'NotInitialized'
  try {
    fmtedCode = reason.printRE(reason.parseRE(source))
  } catch (e) {
    fmtedCode =
      'line ' +
      e.location.startLine +
      ', characters ' +
      e.location.startLineStartChar +
      '-' +
      e.location.endLineEndChar +
      ', ' +
      e.message +
      '\n' +
      source
  }

  return fmtedCode
}

/**
 * Compiles a Flow libdef to a Reason interface, formatted and error handled
 *
 * @param {string} source Flow libdef to compile
 * @param {string} [filename] Name of file being compiled for better error messages
 * @return {string} Reason interface
 */
function compile(
  source,
  filename = '',
  includeModule = false,
  debugMode = false,
) {
  let res
  let resName

  try {
    const [moduleName, bsCode, diagnosticErrors] = Retyped.compile(filename, source, debugMode)
    const fmtCode = format(bsCode)
    res = fmtCode
    resName = moduleName
    if (diagnosticErrors.length > 0) {
      console.log('\x1b[31m%s\n\x1b[0m', '[PARSE ERROR]');
      diagnosticErrors.forEach(e => {
        console.log()
        console.log(
          codeFrame(e.source, e.line, e.column, {
            highlightCode: true,
          }),
        )
        console.log()
      });
      process.exit(1)
    }
  } catch (e) {
    console.error(e)
    throw new Error(`${e[0][0]}`)
  }

  if (res.includes('SYNTAX ERROR>')) {
    throw new Error(res)
  }

  if (!includeModule) {
    return res
  } else {
    return [resName, res]
  }
}

module.exports = {
  format,
  compile
}
