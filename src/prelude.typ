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

#let template-state = state("template-state", (funs: ()))
#let template-index = json("index.json")
#let template-hs-pattern = regex("^([^\s]*)?\s*::")

#let template-get-files() = {
  let state = template-state.get()
  let funs = state.funs.dedup()

  let files = ()

  let agree = (:)
  for fun in funs {
    if fun in template-index {
      for loc in template-index.at(fun) {
        agree.insert(loc.file, agree.at(loc.file, default: 0) + 1)
      }
    }
  }
  for (file, count) in agree {
    let prob = float(count) / float(funs.len())
    files.push((file, prob))
  }

  files
}

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

  show raw: it => {
    if it.lang == "hs" {
      let match = it.text.match(template-hs-pattern)
      if match == none or match.captures.len() == 0 {
        it
      } else {
        let name = match.captures.at(0)
        template-state.update(state => {
          state.funs.push(name)
          state
        })
        it
      }
    } else {
      it
    }
  }

  set heading(
    numbering: (..args) => numbering("1.1", ..args) + h(0.5em)
  )
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

#let draft(content) = {
  if sys.inputs.at("draft", default: none) == "1" {
    set text(fill: red.darken(10%))
    content
  }
}

// draft notes are hidden if `--input draft=1` is not provided
#let draft-note(content) = draft(
  block(
    width: 100%,
    fill: red.lighten(75%),
    inset: (y: 1em),
    outset: (x: 2in),
    breakable: false,
    {
      set text(fill: red.darken(40%))
      content
    }
  )
)


#let tag(fill: blue, content) = {
  box(
    radius: 1pt,
    inset: (x: 4pt, y: 3pt),
    // inset: (x: 0.5em, y: 3pt),
    fill: fill.lighten(90%),
    text(fill: fill.darken(5%), strong(smallcaps(content), delta: 200)),
  )
}

#let tag-level-up = tag(fill: eastern, "Level Up")
#let tag-deep-dive = tag(fill: purple, "Deep Dive")
#let tag-exam25-one = tag(fill: green.darken(20%), "Klausur 1. WS25/26")
#let tag-bul = tag(fill: olive, "Berechnungen & Logik")

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

#let random-animal() = {
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
    animals.at(k)
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
    inset: (x: 4pt, y: y-outset),
    baseline: baseline,
    fill: color.lighten(90%),
    text(fill: color.darken(5%), label(s)),
  )
}

#let deps-counter = counter("deps-counter")
#context deps-counter.update(1)
#let deps-labels = state("deps-labels", (:))
// #let deps-depths = state("deps-depths", (:))
#let deps-graph-forward = state("deps-graph-forward", (:))
#let deps-graph-backward = state("deps-graph-backward", (:))

#let deps-build-backward() = {
  let graph = deps-graph-forward.final()
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

// #let deps-compute-depths() = {
//   let forward-graph = deps-graph-forward.final()
//   let backward-graph = deps-graph-backward.final()
//
//   let depths = (:)
//   let q = ()
//   for v in forward-graph.keys() {
//     if not v in backward-graph or backward-graph.at(v).len() == 0 {
//       depths.insert(v, 0)
//       q.push(v)
//     }
//   }
//
//   let i = 0
//   let deg = (:)
//   while i < q.len() {
//     let v = q.at(i)
//     for w in forward-graph.at(v, default: ()) {
//       if not w in deg {
//         deg.insert(w, 0)
//       }
//       deg.at(w) += 1
//       if backward-graph.at(w, default: ()).len() == deg.at(w) {
//         if not w in depths or depths.at(w) < depths.at(v) + 1 {
//           depths.insert(w, depths.at(v) + 1)
//           q.push(w)
//         }
//       }
//     }
//     i += 1
//   }
//   for v in q.rev() {
//     for w in backward-graph.at(v, default: ()) {
//       depths.at(w) = calc.max(depths.at(v), depths.at(w))
//     }
//   }
//
//   deps-depths.update(depths)
// }

#let deps-goto(icon, tests, swap: false) = context {
  let locs = query(figure.where(kind: "thmenv")).map(fig => fig.location())

  let cols = (
    move(dy: -2pt, text(1em, icon)),
    ..tests.map(test => context {
      let label = deps-labels.final().at(test, default: none)
      link(locs.at(int(test)), label)
    })
  )

  if swap {
    let a = cols.at(0)
    let b = cols.at(-1)
    cols.at(0) = b
    cols.at(-1) = a
  }

  box(grid(
    columns: 1 + tests.len(),
    rows: auto,
    align: horizon,
    gutter: 0.25em,
    ..cols
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
  animal: none,
  extra: none,
  hints: (),
  templates: none
) = {
  let fill = nemo-get-level-color(level)
  let stroke = 0.25pt + fill.lighten(60%)

  nemo-state.update(_ => nemo-new())
  template-state.update((funs: ()))

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
    // let depths = deps-depths.final()

    // [Test/Challenge] [Title] [Tags] [Animal]
    grid(
      columns: (auto, 1fr, auto, auto),
      // stroke: 1pt + black,
      column-gutter: 0.5em,
      align: (x, y) => top + ("2": right).at(str(x), default: left),
      grid.cell(titlefmt(head + " " + number)),
      grid.cell(inset: (left: 0.5em, top: 3pt - if clock { 1pt } else { 0pt }), if title != none { strong(title, delta: 200) } else { [] }),
      grid.cell(rowspan: 2, {
        stack(
          spacing: 0.5em,
          ..tags,
          for (i, hint) in hint-labels.enumerate() {
            tag(
              fill: gray.darken(40%),
              link(hint, "Hinweis " + str(i + 1))
            )
          }
        )
        // if depths.at(id, default: 0) > 0 {
        //   box(
        //     radius: 1pt,
        //     inset: (x: 0.5em, y: 4pt),
        //     fill: gradient.linear(..color.map.flare, angle: 45deg),
        //     text(fill: white, strong(smallcaps("Adventure"), delta: 200)),
        //   )
        // } else {
        //   []
        // }
      }),
      if animal == none {
        []
      } else {
        if animal == true {
          grid.cell(text(1.5em, baseline: -3.2pt, random-animal()))
        } else {
          grid.cell(text(1.5em, baseline: -3.2pt, animal))
        }
      }
    )
  }

  let body = context {
    let id = nemo-state.get().id
    let forward = deps-graph-forward.final().at(id, default: ())
    let backward = deps-graph-backward.final().at(id, default: ())

    body
    if backward.len() > 0 or forward.len() > 0 {
      grid(
        columns: (auto, 1fr),
        align: (left, right),
        if backward.len() > 0 {
          deps-goto(emoji.seedling, backward)
        } else {
          []
        },
        if forward.len() > 0 {
          deps-goto(emoji.tree.deciduous, forward, swap: true)
        } else {
          []
        }
      )
    }
  }


  let footer = context {
    let (footnotes,) = nemo-state.get()
    let templates-from-index = template-get-files()

    let blocks = ()

    if footnotes.len() > 0 {
      let block = {
        for (i, footnote) in footnotes.enumerate() {
          super(str(i + 1)) + footnote + linebreak()
        }
      }
      blocks.push(block)
    }

    if extra != none {
      blocks.push(block(extra))
    }
    // if extra != none and extra.fields().children.len() > 0 {
    //   blocks.push(block(extra))
    // }

    if type(templates) == list and templates.len() > 0 or templates-from-index.len() > 0 {
      let what = if lower(head) == "challenge" { "Diese Challenge" } else { "Dieser Test" }
      let templates = if templates == none { templates-from-index } else { templates }
      let templates = templates.filter(((file, prob)) => prob >= 0.79).map(((file, _)) => file)
      let block = {
        if templates.len() == 1 {
            [#what hat eine Vorlage: #raw(templates.at(0)).]
        } else if templates.len() > 1 {
          [#what hat Vorlagen: #templates.map(raw).join(", ", last: " und ").]
        }
      }
      blocks.push(block)
    }

    if templates != none {
      blocks.push(draft[
        Für diesen Tests sind die Vorlagen manuell überschrieben worden. Im
        Index wurden #templates-from-index.map(((file, prob)) => file + " (" + str(prob) + ")").map(raw).join(", ", last: " und ") gefunden.
      ])
    }

    if blocks.len() > 0 {
      line(length: 100%, stroke: stroke)
      {
        set text(0.8em)
        blocks.join()
      }
    }

    // if (
    //   footnotes.len() > 0 or extra != none and extra.fields().children.len() > 0 or (templates != none and templates.len() > 0) or templates-from-index.len() > 0
    // ) {
    //   text(0.8em, {
    //     line(length: 100%, stroke: stroke)
    //     for (i, footnote) in footnotes.enumerate() {
    //       super(str(i + 1)) + footnote + linebreak()
    //     }
    //
    //
    //     let what = if lower(head) == "challenge" { "Diese Challenge" } else { "Dieser Test" }
    //     let templates = if templates == none { templates-from-index } else { templates }
    //     if templates.len() == 1 {
    //       [#what hat eine Vorlage: #raw(templates.at(0)).]
    //     } else if templates.len() > 1 {
    //       [#what hat Vorlagen: #templates.map(raw).join(", ", last: " und ").]
    //     }
    //   })
    // }
  }

  let template-notice = context {
    let funs = template-state.get().funs
    let templates = template-get-files()

    let a = lower(head) == "challenge"
    let b = lower(head) == "test" and level != none and level > 2
    let c = funs.len() > 0 and templates.len() ==  0
    if (a or b) and c {
      draft-note[
        #head *#title* hat keine Vorlage.
      ]
    }
  }

  block(
    inset: 1em,
    radius: 1pt,
    stroke: stroke,
    breakable: breakable,
    header + body + footer
  ) + template-notice
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


