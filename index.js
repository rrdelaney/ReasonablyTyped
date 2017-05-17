// @flow

/*::
type Compiler = (fileName: string, source: string) => string
*/

module.exports.compile /*: Compiler */ = require('./retyped_node').compile

/*::
type Refmt = (code: string, inFile: 'RE' | 'ML', fileType: 'interface' | 'implementation', outFile: 'RE' | 'ML') => string
*/

module.exports.refmt /*: Refmt */ = require('./refmt_node').refmt