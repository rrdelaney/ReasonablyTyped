// @flow

const Retyped = require('./retyped_node')
const Refmt = require('./refmt_node')

/*::
type Compiler = (fileName: string, source: string) => string
*/

/**
 * Compiles a Flow libdef to a Reason interface
 */
module.exports.rawCompile /*: Compiler */ = Retyped.compile

/*::
type Refmt = (code: string, inFile: 'RE' | 'ML', fileType: 'interface' | 'implementation', outFile: 'RE' | 'ML') => string
*/

/**
 * Runs `refmt` on a codeblock
 */
module.exports.refmt /*: Refmt */ = Refmt.refmt

/**
 * Compiles a Flow libdef to a Reason interface, formatted and error handled
 *
 * @param {string} source Flow libdef to compile
 * @return {string} Reason interface
 */
module.exports.compile = (source /*: string */) /*: string */ => {
  let res

  try {
    const [moduleName, flowCode, bsCode] = Retyped.compile('', source)
    const [fmtType, fmtCode] = Refmt.refmt(bsCode, 'RE', 'implementation', 'RE')
    res = fmtCode
  } catch (e) {
    con
    throw new Error(`${e[1][1].c}: ${e[2].c}`)
  }

  if (res.includes('SYNTAX ERROR>')) {
    throw new Error(res)
  }

  return res
}