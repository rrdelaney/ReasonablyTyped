const { default: Flowgen } = require('flowgen')
const Retyped = require('./retyped_node')
const Refmt = require('./refmt_node')

/**
 * Runs `refmt` on a string of Reason code
 *
 * @param {string} source Reason source code
 * @return {string} Formatted Reason code
 */
function format(source) {
  const [fmtType, fmtedCode] = Refmt.refmt(source, 'RE', 'implementation', 'RE')

  return fmtedCode
}

/**
 * Compiles a Flow libdef to a Reason interface, formatted and error handled
 *
 * @param {string} source Flow libdef to compile
 * @param {string} [filename] Name of file being compiled for better error messages
 * @return {string} Reason interface
 */
function compile(source, filename = '', includeModule = false) {
  const isTypescript = filename.endsWith('.d.ts')
  if (isTypescript) {
    const isModule = source.includes('declare module')
    const tsSource = Flowgen.compiler.compileDefinitionString(source)
    source = isModule ? tsSource : `declare module "${filename.replace('.d.ts', '')}" {
      ${tsSource}
    }`
  }

  let res
  let resName

  try {
    const [moduleName, flowCode, bsCode] = Retyped.compile(filename, source)
    const [fmtType, fmtCode] = Refmt.refmt(bsCode, 'RE', 'implementation', 'RE')
    res = fmtCode
    resName = moduleName
  } catch (e) {
    throw new Error(`${e[1][1].c}: ${e[2].c}`)
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

module.exports = { format, compile }
