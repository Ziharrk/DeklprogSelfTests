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

#let thm-stroke-state = state("thm-stroke", 1pt + black)
#let thm-stroke(k) = 0.25pt + dd(k).lighten(35%)

#let thmstyle(fill, breakable) = (
  stroke: thm-stroke(fill),
  radius: 1pt,
  inset: 1em,
  breakable: breakable,
)

#let sep(..args) = context line(
  length: 100%,
  stroke: thm-stroke-state.get(),
  ..args,
)

#let extra(content) = block[
  #sep()
  #text(0.8em, content)
]

#let mythmplain = (identifier, name, task) => {
  if task {
    (dd: none, clock: false, breakable: false, ..args) => thmplain(
      identifier,
      name,
      titlefmt: titlefmt(dd, clock),
      bodyfmt: body => {
        // update for separator
        thm-stroke-state.update(_ => thm-stroke(dd))
        body
      },
      separator: h(0.5em),
    )(
      numbering: (..args) => args.at(-1),
      ..thmstyle(dd, breakable),
      ..args,
    )
  } else {
    thmplain(
      identifier,
      name,
      titlefmt: strong,
      separator: h(0.5em),
    ).with(
      numbering: (..args) => args.at(-1),
    )
  }
}

#let remark = mythmplain("remark", "Bemerkung", false)
#let test = mythmplain("test", "Test", true)
#let challenge = mythmplain("challenge", "Challenge", true)

#let hint(content) = {
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
    #thm-stroke-state.update(_ => 0.75pt + color)
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

