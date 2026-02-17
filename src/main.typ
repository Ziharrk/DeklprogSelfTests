// TODO gebrochene Boxen haben oben und unten gezogene Grenzen, die sind hässlich
//      deshalb breakable: false, Ziel ist aber wieder true
// TODO für ein paar Tests Lösungen angeben?
#import "prelude.typ": *
#show: config

#set document(..meta)


#include "preface.typ"

#outline()
#pagebreak(weak: true)

#include "haskell.typ"
#include "prolog.typ"
#include "hints.typ"
#include "extra.typ"
#include "exams.typ"
#include "appendix.typ"

