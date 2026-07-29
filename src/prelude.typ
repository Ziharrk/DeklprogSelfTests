#import "@preview/cetz:0.4.2"
#import "@preview/ctheorems:1.1.3": *
#import "@preview/diagraph:0.3.6"
#import "@preview/finite:0.5.0"
#import "@preview/heroic:0.1.0": *
#import "@preview/suiji:0.4.0"

#let meta = (
  title: "Lern- und Vertiefungsaufgaben zum Modul Deklarative Programmierung",
  author: ("Melf Kammholz",),
  description: "Eine Sammlung ergänzender Aufgaben zur Erarbeitung, Festigung und Vertiefung der Modulinhalte",
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

  show link: it => {
    let no-hl = type(it.dest) == location or type(it.dest) == label and str(it.dest).starts-with("hint")
    if no-hl {
      it
    } else {
      underline(
        stroke: (thickness: 0.035em, cap: "round"),
        evade: true,
        offset: 2pt,
        extent: 1pt,
        it
      )
    }
  }

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
  // box(
  //   inset: (x: 0.8em),
  //   box(
  //     fill: fill.lighten(90%),
  //     outset: (x: 0.6em - 0.25pt, y: 0.4em - 0.25pt),
  //     stroke: 0.25pt + fill.lighten(10%),
  //     radius: 1pt,
  //     text(fill: fill.darken(5%), content),
  //   ),
  // )


  let label = s => strong(smallcaps(s), delta: 200)

  let y-outset = 3pt
  let baseline = 0pt

  box(
    radius: 1pt,
    inset: (x: 0.5em, y: 3pt),
    // outset: (x: 4pt, y: y-outset),
    baseline: baseline,
    fill: fill.lighten(90%),
    text(fill: fill.darken(5%), label(content)),
  )
}

#let tag-level-up = tag(fill: eastern, "Level Up")
#let tag-deep-dive = tag(fill: purple, "Deep Dive")
#let tag-exam25-one = tag(fill: green.darken(20%), "Klausur 1. WS25/26")

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
    emoji.badger,
    emoji.bear,
    emoji.bee,
    emoji.bird,
    emoji.bison,
    emoji.boar,
    emoji.butterfly,
    emoji.chipmunk,
    emoji.crocodile,
    emoji.dino.pod,
    emoji.dog,
    emoji.duck,
    emoji.eagle,
    emoji.elephant,
    emoji.fish,
    emoji.fox,
    emoji.giraffe,
    emoji.goat,
    emoji.hedgehog,
    emoji.hippo,
    emoji.kangaroo,
    emoji.leopard,
    emoji.lion,
    emoji.lizard,
    emoji.mammoth,
    emoji.monkey,
    emoji.moose,
    emoji.orangutan,
    // emoji.orca,
    emoji.otter,
    emoji.owl,
    emoji.panda,
    emoji.parrot,
    emoji.peacock,
    emoji.penguin,
    emoji.pig,
    emoji.ram,
    emoji.rhino,
    emoji.seal,
    emoji.sloth,
    emoji.snake,
    emoji.tiger,
    emoji.turtle,
    emoji.whale.spout,
    emoji.wolf,
    emoji.zebra,
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

#let deps-counter = counter("deps-counter")
#context deps-counter.update(1)
#let deps-labels = state("deps-labels", (:))
#let deps-graph-forward = state("deps-graph-forward", (:))
#let deps-graph-backward = state("deps-graph-backward", (:))

#let deps-build-backward() = {
  let graph = deps-graph-forward.get()
  let transpose = (:)
  for (from, tos) in graph {
    for to in tos {
      if not to in transpose {
        transpose.insert(to, ())
      }
      transpose.at(to).push(from)
    }
  }
  deps-graph-backward.update(transpose)
}

#let deps-goto(icon, tests) = context {
  let locs = query(figure.where(kind: "thmenv")).map(fig => fig.location())

  box(grid(
    columns: 1 + tests.len(),
    rows: auto,
    align: horizon,
    gutter: 0.25em,
    move(dy: -2pt, text(1em, icon)),
    ..tests.map(test => context {
      let label = deps-labels.final().at(test)
      link(locs.at(int(test)), label)
    })
  ))
}

#let nemo-state = state("test", none)

#let nemo-new(title: none, tags: ()) = (
  id: none,
  title: none,
  level: none,
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
  deps: (),
  clock: false,
  breakable: false,
  extra: none,
  hints: (),
) = {
  let fill = nemo-get-level-color(level)
  let stroke = 0.25pt + fill.lighten(60%)

  nemo-state.update(_ => nemo-new())

  let titlefmt = nemo-make-titlefmt(level, clock)
  let titlefmt-noclock = nemo-make-titlefmt(level, false)

  context {
    let id = str(deps-counter.get().at(0))
    deps-counter.step()
    nemo-state.update(test => {
      test.id = id
      test
    })

    for dep in deps {
      let did = str(deps-counter.at(dep).at(0))
      deps-graph-forward.update(graph => {
        let adj = graph.at(did, default: ())
        adj.push(id)
        graph.insert(did, adj)
        graph
      })
    }


    let counters = thmcounters.get().counters
    if lower(head) in counters {
      let num = thmcounters.get().counters.at(lower(head)).last()
      deps-labels.update(labels => {
        labels.insert(id, box(inset: 0.5em, titlefmt-noclock(head + " " + str(num))))
        labels
      })
    }
  }

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


  let header = context {
    let hint-labels = nemo-state.get().hints
    let id = nemo-state.get().id
    let backward = deps-graph-backward.final().at(id, default: ())

    grid(
      columns: (1fr, auto, auto),
      gutter: 0.5em,
      align: (x, y) => horizon + if x > 0 { right } else { left },
      grid.cell(
        titlefmt(head + " " + number)
          + if title != none { h(1em) + strong(title, delta: 200) },
      ),
      if backward.len() > 0 {
        let sep = if tags.len() > 0 or hint-labels.len() > 0 {
          (
            stroke: (right: 0.5pt + gray.lighten(20%)),
            inset: (right: 0.5em)
          )
        } else {
          (:)
        }

        grid.cell(
          ..sep,
          deps-goto(emoji.seedling, backward)
        )
      } else {
        []
      },
      grid.cell(move(
        dy: 1.25pt,
        tags.map(tag => box(inset: (left: 0.5em), tag)).join()
          + for (i, hint) in hint-labels.enumerate() {
            box(
              inset: (left: 0.5em),
              tag(
                fill: gray.darken(40%),
                link(hint, "Hinweis " + str(i + 1))
              )
            )
          },
      ))
    )
  }

  let body = context {
    let id = nemo-state.get().id
    let forward = deps-graph-forward.final().at(id, default: ())

    body
    if forward.len() > 0 {
      align(right, {
        box(
          stroke: (left: 0.5pt + gray.lighten(20%)),
          inset: (left: 0.5em),
          deps-goto(emoji.tree.deciduous, forward)
        )
      })
    }
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

#let oplus = symbol(
  "\u{2295}",
  ("big", "\u{2A01}")
)

#let otimes = symbol(
  "\u{2297}",
  ("big", "\u{2A02}")
)

