#import "prelude.typ": *
#show: config

#set document(..meta)

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
}
