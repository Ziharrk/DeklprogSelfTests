// TODO gebrochene Boxen haben oben und unten gezogene Grenzen, die sind hässlich
//      deshalb breakable: false, Ziel ist aber wieder true
// TODO für ein paar Tests Lösungen angeben?
#import "prelude.typ": *
#show: config

#set document(..meta)
#metadata((typst-preview: (title: meta.title)))

#[
  #set align(horizon)
  #set page(
    numbering: none,
    footer: align(center)[
      Dieses Dokument ist vom #datetime.today().display("[day].[month].[year]").
      Die aktuelle Version des Dokuments kannst du im moodle oder
      #link(git("releases/download/latest/main.pdf"))[direkt von GitHub herunterladen].
      Dieses Dokument wird ständig aktualisiert.
    ]
  )

  #show title: set text(1.3em)
  #title()

  #v(1em)

  #text(1.25em, meta.description)

  #counter(page).update(0)
]

#include "preface.typ"

#outline()
#pagebreak(weak: true)
#context counter(heading).update(0)

#include "haskell.typ"
#include "prolog.typ"
#include "hints.typ"
#include "extra.typ"
#include "appendix.typ"

#context {
  deps-build-backward()
  // deps-compute-depths()
}

// #pagebreak(weak: true)

// #context {
//   let forward-graph = deps-graph-forward.final()
//   let backward-graph = deps-graph-backward.final()
//   let labels = deps-labels.final()
//
//   let nodes = forward-graph.keys() + backward-graph.keys()
//   let edges = backward-graph.pairs().map(((v, ws)) => ws.map(w => v + " -> " + w).join(";\n")).join(";\n") + ";"
//
//   diagraph.render(
//     engine: "neato",
//     "digraph {"
//     + "node[shape=none];"
//     + "edge[arrowsize=0.5,len=2];"
//     + edges
//     + "}",
//     labels: labels
//   )
// }
//
