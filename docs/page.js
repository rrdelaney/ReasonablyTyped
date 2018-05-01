const libs = {
  'is-url': [
    "declare module 'is-url' {",
    '  declare module.exports: (url: string) => boolean',
    '}',
  ].join('\n'),
}

const defaultLibDef = [
  "declare module 'math-fns' {",
  '  declare function add(x: number, y: number): number',
  '  declare function double(x: number | string): number',
  '}',
].join('\n')

class SyntaxError extends Error {}

const compileFlow = flow => {
  const [moduleId, reason] = retyped.compile(flow, 'test.js', true)
  return `/* Module ${moduleId} */\n\n${reason}`
}

const compileTypeScript = ts => {
  const [moduleId, reason] = retyped.compile(ts, 'test.ts', true)
  return `/* Module ${moduleId} */\n\n${reason}`
}

const compileLibdef = () => {
  try {
    const libdef = $('#libdef-input').val()
    const lang = $('#libdef-lang').val()
    $('#libdef-lang-label').text(lang + ' Declaration')
    const output =
      lang === 'Flow' ? compileFlow(libdef) : compileTypeScript(libdef)
    $('#libdef-output').removeClass('error')
    $('#libdef-output ').val(output)
  } catch (e) {
    console.error(e)
    const errorText =
      e instanceof SyntaxError
        ? 'Error parsing input code O_o'
        : 'Unable to generate code :('

    $('#libdef-output').addClass('error')
    $('#libdef-output').val(errorText)
  }
}

$('.ui.dropdown').dropdown()

$('.code.column').each(function() {
  const $this = $(this)
  const libdef = compileFlow($this.text())
  const outputBlock = $this
    .siblings()
    .first()
    .find('code')
  outputBlock.text(libdef)
})

$('#libdef-input').val(defaultLibDef)
$('#libdef-output').val(compileFlow(defaultLibDef))

$('#libdef-input').on('change keyup paste ', compileLibdef)
$('#libdef-lang').on('change', compileLibdef)

$.getJSON('test-results.json', data => {
  const results = data.testResults[0].assertionResults
  const passedTests = data.numPassedTests
  const allTests = data.numTotalTests
  const allPassing = allTests === passedTests

  if (allPassing) {
    $('#is-it-ready').text('Yes')
    $('#is-it-ready-message').text('Woohoo!')
  } else {
    $('#is-it-ready').text('No')
    $('#is-it-ready-message').text(
      `But it passes ${passedTests} / ${allTests} tests`,
    )
  }

  $('#is-it-ready-table').append(
    results.map(result => {
      const title = result.title.replace('Library ', '')
      const isFailing = result.status === 'failed'
      const message = result.failureMessages[0]

      const classes = [isFailing ? 'failing' : 'passing', 'test-box'].join(' ')
      const tooltip = isFailing
        ? message.replace(/"/g, '&quot;').split('at compile')[0]
        : ''

      return $(
        `<div class="${classes}" data-title="${title}" data-content="${tooltip}"></div>`,
      )
    }),
  )

  $('.test-box').popup()
})
