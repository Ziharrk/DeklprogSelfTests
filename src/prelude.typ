#import "@preview/cetz:0.4.2"
#import "@preview/ctheorems:1.1.3": *
#import "@preview/diagraph:0.3.6"
#import "@preview/finite:0.5.0"
#import "@preview/heroic:0.1.0": *
#import "@preview/suiji:0.5.1"

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
    "Verständnisfragen",
  ),
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
  show heading: set block(above: 1.4em, below: 1em)
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
      stroke: 0.25pt + fill.lighten(10%),
      radius: 1pt,
      text(fill: fill.darken(5%), content),
    ),
  )
}

// https://github.com/typst/typst/issues/1988#issuecomment-2466619917
#let get-now() = {
  let now-str = if "now" in sys.inputs {
    sys.inputs.now
  } else {
    datetime.today().display("[year] [month] [day]") + " 00 00 00"
  }
  let (year, month, day, hour, minute, second) = now-str.split(" ").map(int)
  datetime(year: year, month: month, day: day, hour: hour, minute: minute, second: second)
}

#let epoch = datetime(year: 1970, month: 01, day: 01, hour: 0, minute: 0, second: 0)
#let rng = state("rng", suiji.gen-rng(int((get-now() - epoch).seconds())))

#let hl() = {
  let animals = (
    emoji.dog,
    emoji.bird,
    emoji.mammoth,
    emoji.otter,
    emoji.owl,
    emoji.panda,
    emoji.parrot,
    emoji.penguin,
    emoji.seal,
    emoji.sloth,
    emoji.bear,
    emoji.turtle,
    emoji.whale.spout
  )
  context {
    let (_, k) = suiji.integers(rng.get(), low: 0, high: animals.len() - 1)
    box(inset: (left: .6em - 0.25pt), scale(x: 150%, y: 150%, animals.at(k)))
  }

  // does not trigger layout iteration
  // TODO better way to advance rng?
  rng.update(_rng => suiji.integers(_rng, low: 0, high: animals.len() - 1).at(0))
}


#let nemo-level-colors = (
  "1": blue,
  "2": orange,
  "3": magenta,
)

#let nemo-default-level-color = gray.darken(40%)

#let nemo-get-level-color(level) = if level != none {
  nemo-level-colors.at(str(level), default: nemo-default-level-color)
} else {
  nemo-default-level-color
}

#let nemo-make-titlefmt(level, clock) = {
  let color = nemo-get-level-color(level)

  let clock-tag = if clock { " " + hi("clock", solid: false) }
  let label = s => strong(smallcaps(s), delta: 200) + clock-tag

  let y-outset = if clock { 2pt } else { 3pt }
  let baseline = if clock { 2pt } else { 0pt }

  s => box(
    radius: 1pt,
    outset: (x: 4pt, y: y-outset),
    baseline: baseline,
    fill: color.lighten(90%),
    text(fill: color.darken(5%), label(s)),
  )
}

#let nemo-state = state("test", none)

#let nemo-new(title: none, tags: ()) = (
  title: none,
  tags: (),
  footnotes: (),
  hints: (),
  extra: none,
)

#let note(content) = {
  nemo-state.update(state => {
    state.footnotes.push(content)
    state
  })
  context super(str(nemo-state.get().footnotes.len()))
}

#let nemo-boxfmt(
  head,
  name,
  number,
  body,
  title: none,
  level: none,
  tags: (),
  clock: false,
  breakable: false,
  extra: none,
  hints: (),
) = {
  let fill = nemo-get-level-color(level)
  let stroke = 0.25pt + fill.lighten(60%)

  nemo-state.update(_ => nemo-new())

  for (i, hint) in hints.enumerate() {
    context {
      let num = thmcounters.get().counters.at(lower(head)).last()
      let hint-label = label("hint-" + str(num) + "-" + str(i))
      let value = [#nemo-boxfmt(
          "Hinweis zu",
          none,
          link(here())[#head #num],
          hint,
        ) #hint-label]
      metadata((type: "hint", value: value))
      nemo-state.update(state => {
        state.hints.push(hint-label)
        state
      })
    }
  }

  let titlefmt = nemo-make-titlefmt(level, clock)

  let header = context {
    let hint-labels = nemo-state.get().hints
    grid(
      columns: (1fr, auto),
      grid.cell(
        titlefmt(head + " " + number)
          + if title != none { h(1em) + strong(title, delta: 200) },
      ),
      grid.cell(move(
        dy: 1.25pt,
        tags.join()
          + for (i, hint) in hint-labels.enumerate() {
            tag(fill: teal.darken(10%), link(hint, "Hinweis " + str(i + 1)))
          },
      )),
    )
  }

  let footer = context {
    let (footnotes,) = nemo-state.get()
    if (
      footnotes.len() > 0 or extra != none and extra.fields().children.len() > 0
    ) {
      text(0.8em, {
        line(length: 100%, stroke: stroke)
        for (i, footnote) in footnotes.enumerate() {
          super(str(i + 1)) + footnote + linebreak()
        }

        if extra != none and extra.fields().children.len() > 0 { block(extra) }
      })
    }
  }

  block(
    inset: 1em,
    radius: 1pt,
    stroke: stroke,
    breakable: breakable,
    header + body + footer,
  )
}


#let nemo-env(identifier, head) = thmenv(
  identifier,
  none,
  none,
  nemo-boxfmt.with(head),
).with(supplement: head)

#let args-to-named(env) = (..args) => {
  let body = args.at(0)
  let extra = if args.pos().len() >= 2 { args.at(1) } else { none }
  let hints = if args.pos().len() >= 3 { args.pos().slice(2) } else { () }
  env(body, extra: extra, hints: hints, ..args.named())
}

#let remark = nemo-env("remark", "Bemerkung").with(breakable: true)
#let test = args-to-named(nemo-env("test", "Test"))
#let challenge = args-to-named(nemo-env("challenge", "Challenge"))


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

