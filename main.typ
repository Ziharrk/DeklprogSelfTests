#import "@preview/ctheorems:1.1.3": *

#show link: underline
#show heading.where(level: 1): set block(below: 1.25em)
#show: thmrules

#let test = thmplain(
    "test",
    "Test",
    titlefmt: strong, 
    separator: h(0.5em)
  ).with(
    numbering: (_, counter) => counter
  )

Dieses Dokument enthält Fragen, kleine Aufgaben und andere Ressourcen zum Thema
Deklarative Programmierung. Die Inhalte dieses Dokuments sollen dir helfen,
dein Verständnis über Haskell und Prolog zu prüfen.

= Funktionale Programmierung

// Ausdrücke und Funktionen

#test[
  Was bedeutet es, wenn eine Funktion keine Seiteneffekte hat?
]

#test[
  Haskell ist eine streng getypte Programmiersprache. Was bedeutet das?
]

#test[
  Wenn du eine Schleife in Haskell umsetzen möchtest, auf welches Konzept musst
  du dann zurückgreifen?
]

#test[
  In imperativen Programmiersprachen sind Variablen Namen für Speicherzellen,
  deren Werte zum Beispiel in Schleifen verändert werden können. Als Beispiel
  betrachte
  ```py
  def clz(n):
    k = 0
    while n > 0:
      n /= 2
      k += 1
    return 64 - k
  ```
  In Haskell sind Variablen keine Namen für Speicherzellen. Wie können wir
  dieses Programm in Haskell umsetzen? Wo wandert das `k` hin?
]

#test[
  Auf was müssen wir achten, wenn wir eine rekursive Funktion definieren?
  Die Antwort ist abhängig von dem, was die Funktion berechnen soll. Denke über
  die verschiedenen Möglichkeiten nach.
]

#test[
  Gegeben sei das folgende Haskell-Programm.
  ```hs
  even :: Int -> Bool
  even 0 = True
  even n = odd (n - 1)

  odd :: Int -> Bool
  odd 0 = False
  odd n = even (n - 1)
  ```

  - Berechne das Ergebnis von `odd 1` händisch.
  - Wie sieht der Auswertungsgraph für den Ausdruck `odd 1` aus?
    - Welcher Pfad entspricht deiner händischen Auswertung?
    - Welcher Pfad entspricht der Auswertung wie sie in Haskell stattfindet?
    - Wie sieht es mit Python aus?
]

#test[
  Es wird als sauberer Programmierstil angesehen, Hilfsfunktionen, die nur für
  eine Funktion relevant sind, nicht auf der höchsten Ebene zu definieren.
  Mithilfe welcher Konstrukte kannst du diese lokal definieren?
]

#test[
  Das Potenzieren einer Zahl $x$ (oder eines Elements einer Halbgruppe) mit
  einem natürlich-zahligen Exponent $n$ ist in $cal(O)(log n)$ Laufzeit möglich
  #footnote[#link("https://de.wikipedia.org/wiki/Bin%C3%A4re_Exponentiation")[Binäre Exponentiation -- Wikipedia]].
  Dafür betrachten wir
  $
  x^n = cases((x^(n/2))^2 & "falls" n "gerade", x dot x(x^(n/2))^2 & "sonst")
  $
  Implementiere eine Funktion, die diese Variante des Potenzierens umsetzt.
]

#test[
  Gegeben ist folgender Ausdruck.
  ```hs
  let v = 3
      w = 5
      x = 4
      y = v + x
      z = x + y
   in y
  ```
  Welche Belegungen der Variablen werden tatsächlich berechnet, wenn wir `y`
  ausrechnen?
]

#test[
  Ist der folgende Ausdruck typkorrekt?
  #align(center)[```hs if 0 then 3.141 else 3141```]
]


// Datentypen

#test[
  Wie werden algebraische Datentypen in Haskell definiert?
]

#test[
  Was ist charakterisierend für Aufzählungstypen, einen Verbundstypen und
  einem rekursiven Datentypen? Gebe Beispiele für jeden dieser Typarten an.
]

#test[
  Geben ist der Typ ```hs IntList``` mit ```hs data IntList = Nil | Cons Int IntList```.
  Weiter kann mithilfe der Funktion
  ```hs
  lengthIntList :: IntList -> Int
  lengthIntList Nil         = 0
  lengthIntList (Cons _ xs) = 1 + lengthIntList xs
  ```
  die Länge einer solchen Liste berechnet werden. Du möchtest nun auch
  die Längen von Listen berechnen, die Buchstaben, Booleans oder Gleitkommazahlen
  enthalten. Was stört dich am bisherigen Vorgehen? Kennst du ein Konzept
  mit dessen Hilfe du besser an dein Ziel kommst?
]

#test[
  Wie ist die Funktion `lengthIntList :: IntList -> Int` aus dem vorherigen
  Test definiert?
]

#test[
  Du hast einen Datentypen definiert und möchtest dir Werte des Typen nun
  z.B. im GHCi anzeigen lassen. Was kannst du tun, um an dieses Ziel zu kommen?
]

#pagebreak()

Wenn du auf der Suche nach weiteren Übungsaufgaben bist, mit denen du deine
Programmierkenntnisse in Prolog verbessern möchtest, bietet sich die Liste
#link("https://www.ic.unicamp.br/~meidanis/courses/mc336/2009s2/prolog/problemas/")[P-99: Ninety-Nine Prolog Problems]
an. Lösungen sind ebenso auf der Seite verfügbar. Für Haskell gibt es eine
ähnliche Seite #link("https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems")[H-99: Ninety-Nine Haskell Problems].

Weitere Links:
- #link("https://learnyouahaskell.github.io/")[Learn You A Haskell]
- #link("https://pbv.github.io/haskelite/site/index.html")[Haskelite]: Ein Schritt-für-Schritt Interpreter für (eine Teilmenge von) Haskell
// - #link("https://www.adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html")[Functors, Applicatives, And Monads In Pictures]
- #link("https://hackage.haskell.org/package/CheatSheet-1.7/src/CheatSheet.pdf")[Haskell Cheatsheet]
// - #link("https://alhassy.com/PrologCheatSheet/CheatSheet.pdf")[Prolog Cheatsheet]

