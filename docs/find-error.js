const { writeFileSync } = require('fs')
const alltests = require('./test-results')

const results = alltests.testResults[0].assertionResults
  .filter(_ => _.failureMessages.length === 1)
  .map(_ => _.failureMessages[0])
  .map(_ => _.split('[')[0])
  .map(_ => (_.includes('SYNTAX ERROR') ? 'SYNTAX ERROR' : _))
  .map(_ => _.split('\n')[0])
  .map(_ => (_.startsWith('Error:') ? _.split(':')[2] : _))
  .reduce((count, err) => {
    count[err] = !count[err] ? 1 : count[err] + 1
    return count
  }, {})

const count = Object.entries(results)
  .sort(([, count1], [, count2]) => count2 - count1)
  .map(_ => _[1] + ' ' + _[0])
  .join('\n')

console.log(count)
