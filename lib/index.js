// @flow

const Retyped = require('./retyped_node')
const Refmt = require('./refmt_node')

/**
 * Runs `refmt` on a string of Reason code
 *
 * @param {string} source Reason source code
 * @return {string} Formatted Reason code
 */
function format (source /*: string */) /*: string */ {
  const [fmtType, fmtedCode] = Refmt.refmt(source, 'RE', 'implementation', 'RE')

  return fmtedCode
}

/**
 * Compiles a Flow libdef to a Reason interface, formatted and error handled
 *
 * @param {string} source Flow libdef to compile
 * @return {string} Reason interface
 */
function compile (source /*: string */) /*: string */ {
  let res

  try {
    const [moduleName, flowCode, bsCode] = Retyped.compile('', source)
    const [fmtType, fmtCode] = Refmt.refmt(bsCode, 'RE', 'implementation', 'RE')
    res = fmtCode
  } catch (e) {
    throw new Error(`${e[1][1].c}: ${e[2].c}`)
  }

  if (res.includes('SYNTAX ERROR>')) {
    throw new Error(res)
  }

  return res
}

module.exports = { format, compile }
