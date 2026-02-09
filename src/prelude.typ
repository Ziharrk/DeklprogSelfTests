#import "@preview/cetz:0.4.2"
#import "@preview/ctheorems:1.1.3": *
#import "@preview/diagraph:0.3.6"
#import "@preview/finite:0.5.0"
#import "@preview/heroic:0.1.0": *

#let meta = (
  title: "Verständnisfragen zum Modul Deklarative Programmierung",
  author: ("Melf Kammholz",),
  description: "Verständnisfragen zum Modul Deklarative Programmierung",
  keywords: (
    "Deklarative Programmierung",
    "Funktionale Programmierung",
    "Logische Programmierung",
    "Haskell",
    "Prolog",
    "Verständnisfragen"
  )
)

#let config(
  paper: "a4",
  lang: "de",

  fontsize: 10.5pt,
  text-font: none,
  math-font: none,
  raw-font: "CaskaydiaCove NF",

  content,
) = {
  set page(paper: paper, numbering: "1")

  set text(size: fontsize, lang: lang)

  show raw: set text(font: raw-font)
  show raw.where(block: false): box
  set raw(syntaxes: "../syntaxes/prolog.sublime-syntax")

  show link: underline
  show heading.where(level: 1): set block(below: 1.25em)
  show math.equation.where(block: false): box

  show: thmrules

  content
}


#let blue = rgb("#648fff")
#let magenta = rgb("#dc267f")


#let is-type = type => {
  val => std.type(val) == dictionary and "type" in val and val.type == type
}

#let get-metadata = type => {
  query(metadata)
    .map(res => res.value)
    .filter(is-type(type))
    .map(res => res.value)
}


#let tag(fill: blue, content) = {
  box(
    inset: (x: 0.8em),
    box(
      fill: fill.lighten(90%),
      outset: (x: 0.6em - 0.25pt, y: 0.4em - 0.25pt),
      stroke: 0.25pt + fill.darken(5%),
      radius: 1pt,
      text(fill: fill.darken(5%), content)
    )
  )
}


#let dd(k) = {
  let m = (
    "1": blue,
    "2": orange,
    "3": magenta
  )
  let d = gray.darken(10%)
  if type(k) == int { m.at(str(k), default: d) } else { d }
}

#let titlefmt(level, clock) = {
  let fill = dd(level)

  let label = s => (
    strong(smallcaps(s), delta: 200)
      + if clock { " " + hi("clock", solid: false) } else {}
  )

  s => box(
    radius: 1pt,
    outset: (y: if clock { 2pt } else { 3pt }, x: 4pt),
    baseline: if clock { 2pt } else { 0% + 0pt },
    fill: fill.lighten(90%),
    text(fill: fill.darken(5%), label(s)),
  )
}

#let nemo-state = state("test", none)

#let nemo-new(title: none, tags: ()) = (
  title: none,
  tags: (),
  footnotes: (),
  has-hints: false,
  extra: none
)

#let note(content) = {
  nemo-state.update(state => {
    state.footnotes.push(content)
    state
  })
  context super(str(nemo-state.get().footnotes.len()))
}

#let extra(content) = {
  nemo-state.update(state => {
    state.extra = content
    state
  })
}

#let nemo-env(identifier, head) = thmenv(
  identifier,
  none,
  none,
  (name, number, body, title: none, level: none, tags: (), clock: false, breakable: false) => {
    let fill = dd(level)
    let stroke = 0.25pt + fill

    nemo-state.update(_ => nemo-new())
    block(inset: 1em, radius: 1pt, stroke: stroke, breakable: breakable)[
      #grid(
        columns: (1fr, auto),
        grid.cell(titlefmt(level, clock)(head + " " + number) + if title != none { h(1em) + strong(title, delta: 200) }),
        grid.cell(move(dy: 1.25pt, tags.join()))
      )
      #body

      // print test footer if footnotes or extra content is available
      #context {
        let (footnotes, extra) = nemo-state.get()
        if footnotes.len() > 0 or extra != none {
          text(0.8em, {
            line(length: 100%, stroke: stroke)
            for (i, footnote) in footnotes.enumerate() {
              super(str(i + 1)) + footnote + linebreak()
            }

            if extra != none { block(extra) }
          })
        }
      }
    ]
  }
)

#let remark = thmplain("remark", "Bemerkung")
#let test = nemo-env("test", "Test")
#let challenge = nemo-env("challenge", "Challenge")

#let hint(content) = {
  nemo-state.update(state => {
    state.has-hints = true
    state
  })

  context {
    let fig = query(figure.where(kind: "thmenv").before(here())).last()
    let num = thmcounters.get().counters.at(lower(fig.supplement.text)).last()
    let value = strong[Hinweis zu #fig.supplement #num] + h(0.5em) + content
    metadata((type: "hint", value: value))
  }
}


// boxes for references and self-evaluation

#let mybox(color, icon, label, content) = {
  box(
    width: 100%,
    fill: color.lighten(97%),
    stroke: (left: 1pt + color),
    inset: (y: 0.75em, x: 1em),
    radius: 1pt,
  )[
    #text(fill: color)[
      #hi(icon, height: 1.2em, solid: false)
      #h(0.2em)
      #text(weight: "bold", label)
    ]
    #v(-0.5em)
    #content
  ]
}

#let check = mybox.with(purple, "academic-cap", "Selbstevaluation")
#let refs = mybox.with(magenta, "book-open", "Referenzen")

#let git(path) = "https://github.com/Ziharrk/DeklprogSelfTests/" + path

