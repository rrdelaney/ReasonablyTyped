// @flow

/*::
type Compiler = (fileName: string, source: string) => string
*/

/**
 * Compiles a Flow libdef to a Reason interface
 */
module.exports.compile /*: Compiler */ = require('./retyped_node').compile

/*::
type Refmt = (code: string, inFile: 'RE' | 'ML', fileType: 'interface' | 'implementation', outFile: 'RE' | 'ML') => string
*/

/**
 * Runs `refmt` on a codeblock
 */
module.exports.refmt /*: Refmt */ = require('./refmt_node').refmt

/**
 * Compiles a Flow libdef to a Reason interface, formatted and error handled
 */
module.exports.compileSource = (source /*: string */) => {
  let res

  try {
    res = refmt(compile('', source), 'RE', 'implementation', 'RE')
  } catch (e) {
    throw new Error(`${e[1][1].c}: ${e[2].c}`)
  }

  if (res.includes('SYNTAX ERROR>')) {
    throw new Error(res)
  }

  return res
}