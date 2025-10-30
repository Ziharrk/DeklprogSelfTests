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

#let challenge = thmplain(
    "challenge",
    "Challenge",
    titlefmt: strong, 
    separator: h(0.5em)
  ).with(
    numbering: (_, counter) => counter
  )

#let hint(content) = {
  context {
    let fig = query(figure.where(kind: "thmenv").before(here())).last()
    let num = thmcounters.get().counters.at(lower(fig.supplement.text)).last()
    let value = strong[Hinweis zu #fig.supplement #num] + h(0.5em) + content
    metadata((type: "hint", value: value))
  }
}

#set page(
  paper: "a4",
  numbering: "1"
)

#text(0.8em)[
  Dieses Dokument ist vom #datetime.today().display("[day].[month].[year]"). Die 
  aktuelle Version des Dokuments kannst du im moodle oder 
  #link("https://github.com/Ziharrk/DeklprogSelfTests/raw/refs/heads/main/main.pdf")[direkt von GitHub herunterladen].
]

Dieses Dokument enthält Fragen, kleine Aufgaben und andere Ressourcen zum Thema
Deklarative Programmierung. Die Inhalte dieses Dokuments sollen dir helfen,
dein Verständnis über Haskell und Prolog zu prüfen. 

Größere Aufgaben haben wir als Challenges markiert. Diese Aufgaben benötigen 
öfter mehrere Konzepte und führen zusätzlich Konzepte ein, die nur für das 
Lösen der Aufgabe wichtig sind.

Wenn du Anmerkungen oder weitere Ideen für Inhalte für dieses Dokument hast,
schickt uns diese gerne über z.B. mattermost - oder 
#link("https://github.com/Ziharrk/DeklprogSelfTests/")[stellt eine PR auf GitHub].

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
      n //= 2
      k += 1
    return 64 - k

  def popcnt(n):
    k = 0
    while n > 0:
      if n % 2 == 1:
        k += 1
      n //= 2
    return k
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

  - Berechne das Ergebnis von `odd (1 + 1)` händisch.
  - Wie sieht der Auswertungsgraph für den Ausdruck `odd (1 + 1)` aus?
    - Welcher Pfad entspricht deiner händischen Auswertung?
    - Welcher Pfad entspricht der Auswertung wie sie in Haskell stattfindet?
    - Welcher Pfad entspricht der Auswertung wie sie in Python sinngemäß stattfindet?
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
  x^n = cases((x^(n/2))^2 & "falls" n "gerade", x dot x(x^((n-1)/2))^2 & "sonst")
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
  die qLänge einer solchen Liste berechnet werden. Du möchtest nun auch
  die Längen von Listen berechnen, die Buchstaben, Booleans oder Gleitkommazahlen
  enthalten. Was stört dich am bisherigen Vorgehen? Kennst du ein Konzept
  mit dessen Hilfe du besser an dein Ziel kommst?
]

#test[
  Wie ist die Funktion ```hs lengthIntList :: IntList -> Int``` aus dem vorherigen
  Test definiert?
]

#test[
  Du hast einen Datentypen definiert und möchtest dir Werte des Typen nun
  z.B. im GHCi anzeigen lassen. Was kannst du tun, um an dieses Ziel zu kommen?
]

#test[
  Wie definieren wir Funktionen?
]

#test[
  Gebe ein Listendatentypen an, für den es nicht möglich ist, kein Element
  zu enthalten.
]


// Polymorphismus

#test[
  Wie sieht eine Datentypdefinition im Allgemeinen aus?
]

#test[
  Welchen Typ haben
  - `(:)` und `[]`,
  - `Just` und `Nothing`?
]

#test[
  Was ist parametrischer Polymorphismus?
]

#test[
  Welche Typkonstruktoren des kinds ```hs * -> *``` kennst du?
]

#test[
  Welchen kind hat ```hs Either a```?
]

#test[
  Beim Programmieren vernachlässigen redundante Syntax. 
  Gibt es einen Unterschied zwischen ```hs f 1 2``` und ```hs f(1, 2)```
]

#test[
  Welches Konzept erlaubt es uns, dass wir Funktionen auf Listen nicht für 
  jeden konkreten Typen angeben müssen?
]

#test[
  Visualisiere `[1, 2, 3]` als Baum, wie du es in der Vorlesung kennengelernt 
  hast. Zur Erinnerung: die inneren Knoten sind Funktionen und die Blätter
  Werte, die nicht weiter ausgerechnet werden können.
]

#challenge[
  - Der größte gemeinsamen Teiler (ggT) zweier Ganzzahlen kann mithilfe des
    euklidschen Algorithmus berechnet werden. Implementiere das Verfahren.
    $
    "gcd"(x, y) = cases(
      abs(x) & quad "falls" y = 0,
      gcd(y, x "mod" y) & quad "sonst"
    )
    $
  - Alternativ kann der ggT auch berechnet werden, indem wir das Produkt des
    Schnittes der Primfaktorzerlegung der beiden Zahlen betrachten, also
    $ product ("PF"(x) inter "PF"(y)) $
    wobei $"PF"$ die Menge der Primfaktoren der gegebenen Zahl (mit 
    entsprechenden Mehrfachvorkommen) beschreiben soll. Implementiere diesen 
    Ansatz.

  #hint[
    Zur Darstellung der Multimengen eignen sich sortierte Listen gut.
  ]

  #hint[
    Zur Berechnung des Schnittes können zwei sortierte Listen parallel durchlaufen
    werden. Wenn zwei gleiche Elemente zu Beginn der Liste stehen, wird eines
    der Elemente zum Ergebnis hinzugefügt. Im anderen Fall überspringen wir
    das jeweils kleinere Element der beiden.
  ]
]

#challenge[
  Die Ableitung einer Funktion $f : RR -> RR$ kann mithilfe des 
  Differenzenquotienten $(f(x+h)-f(x))/h$ für kleines $h$ approximiert werden.
  Ein andere Methode zur Berechnung der Ableitung ist symbolisches Differenzen
  und ähnelt dem, wie wir analytisch Ableitungen berechnen.
  Eine Funktion sei dargestellt durch den folgenden Typ:
  ```hs
  data Fun = X            -- x     (Variable x)
           | E            -- e     (Euler's constant)
           | Num Double   -- c     (Constant)
           | Ln Fun       -- ln    (Natural logarithm)
           | Fun :+: Fun  -- f + g (Addition)
           | Fun :-: Fun  -- f - g (Subtraction)
           | Fun :*: Fun  -- f * g (Multiplication)
           | Fun :/: Fun  -- f / g (Division)
           | Fun :<: Fun  -- f o g (Composition)
           | Fun :^: Fun  -- f ^ g (Exponentiation)

  -- Example
  f :: Fun
  f = (E :^: X) :<: (X :*: X)  -- (e^x) o (x * x) = e^(x^2)

  -- Example
  g :: Fun
  g :: let x = X
           x2 = x :*: x      
           x3 = x2 :*: x
        in x3 :+: x2 :+: x :+: Num 1.0  -- x^3 + x^2 + x + 1
  ```
  - Implementiere eine Funktion ```hs ($$) :: Fun -> Double -> Double```, die
    eine gegebene Funktion in einem gegebenen Punkt auswertet.
  - Implementiere eine Funktion ```hs derive :: Fun -> Fun```, die eine gegebene
    Funktion ableitet.
    #footnote[
      #link("https://de.wikipedia.org/wiki/Differentialrechnung#Zusammenfassung")[Zusammenfassung der Ableitungsregeln]
    ]
]

// ```hs
// ($$) :: Fun -> Double -> Double
// X         $$ x = x
// E         $$ _ = exp 1
// (Ln f)    $$ x = log (f $$ x)
// (Num x)   $$ _ = x
// (f :+: g) $$ x = f $$ x + g $$ x
// (f :*: g) $$ x = f $$ x * g $$ x
// (f :-: g) $$ x = f $$ x - g $$ x
// (f :/: g) $$ x = f $$ x / g $$ x
// (f :<: g) $$ x = f $$ (g $$ x)
// (f :^: g) $$ x = (f $$ x) ** (g $$ x)
//
//
// derive :: Fun -> Fun
// derive X         = Num 1.0
// derive E         = Num 0.0
// derive (Num _)   = Num 0.0
// derive (f :+: g) = derive f :+: derive g
// derive (f :-: g) = derive f :-: derive g
// derive (f :*: g) = let f' = derive f 
//                        g' = derive g
//                     in (f' :*: g) :+: (f :*: g')
// derive (f :/: g) = let f' = derive f 
//                        g' = derive g
//                     in ((f' :*: g) :+: (f :*: g')) :/: (g :*: g)
// derive (f :<: g) = let f' = derive f 
//                        g' = derive g
//                     in g' :*: (f' :<: g)
// derive (f :^: g) = let h = Ln f :*: g
//                     in derive h :*: (E :^: h)
// ```

#challenge[
  In Einführung in die Algorithmik hast du verschiedene Varianten des 
  `mergesort`-Algorithmus kennengelernt. Eine davon hat ausgenutzt, dass in
  einer Eingabeliste bereits aufsteigend sortierte Teillisten vorkommen können,
  um den Algorithmus zu beschleunigen.
  #footnote[
    Falls du interessiert bist: In der Haskell `base`-library wird `sort` aus 
    `Data.List` sehr ähnlich implementiert:
    #link("https://hackage.haskell.org/package/ghc-internal-9.1201.0/docs/src/GHC.Internal.Data.OldList.html#sort")[Data.List.sort].
  ] 
  Implementiere diese Variante in Haskell. 

  #text(0.8em)[
    Für den Anfang kannst du annehmen, dass die Eingabelisten vom Typ 
    ```hs [Int]``` sind. Wenn wir Typklassen behandelt haben, kannst du 
    ```hs Ord a => [a]``` nutzen.
  ]
]

// ```hs
// mergesort :: [Int] -> [Int]
// mergesort xs = mergeAll (runs xs)
//   where 
//     runs :: [Int] -> [[Int]]
//     runs []     = [[]]
//     runs (x:[]) = [[x]]
//     runs (x:xs) = 
//       let (r:rs) = runs xs 
//        in case r of
//             (y:_) | x < y -> (x:r) : rs
//             _             -> [x] : (r:rs)
//
//     merge2 :: [Int] -> [Int] -> [Int]
//     merge2 xs     []                 = xs
//     merge2 []     ys                 = ys
//     merge2 (x:xs) (y:ys) | x < y     = x : merge2 xs (y:ys)
//                          | otherwise = y : merge2 (x:xs) ys
//
//     reduce :: [[Int]] -> [[Int]]
//     reduce []       = []
//     reduce [x]      = [x]
//     reduce (x:y:xs) = merge2 x y : reduce xs
//
//     mergeAll :: [[Int]] -> [Int]
//     mergeAll [x]      = x
//     mergeAll xs = mergeAll (reduce xs)
// ```

#test[
  Du hast bereits viele Funktionen kennengelernt, die in der Haskell
  `base`-library implementiert sind. Anstatt eine konkrete Liste dieser 
  Funktionen anzugeben, möchten wir dich motivieren, folgende Dokumentationen
  verschiedener Module anzuschauen.
  - #link("https://hackage.haskell.org/package/base/docs/Prelude.html")[Prelude]
  - #link("https://hackage.haskell.org/package/base/docs/Data-List.html")[Data.List]
  Wenn du merkst, die Implementierung einer bekannten Funktion fällt dir ad hoc 
  nicht ein, nehme dir Zeit und überlege, wie du sie implementieren könntest.
]

#test[
  Hier ist eine fehlerhafte Implementierung eines Datentyps für einen 
  knotenbeschrifteten Binärbäumen.
  ```hs
  data Tree a = Empty | Node Tree a Tree
  ```
  Was ist der Fehler?
]

#test[
  In imperativen Programmierung iterieren wir über Listen oft in folgender
  Form (in Java).
  ```java
  List<Integer> a = new ArrayList<>();
  a.add(3); a.add(1); a.add(4); a.add(1); a.add(5);

  List<Integer> b = new ArrayList<>();
  for (int i = 0; i < a.size(); i++) {
    b.add(2 * a.get(i));
  }
  ```
  Wenn wir diesen Code naiv in Haskell übersetzen, könnten wir z.B.
  ```hs
  double :: [Int] -> Int -> [Int]
  double xs i | i < length xs = 2 * xs !! i : double xs (i + 1)
              | otherwise     = []
  ```
  Das wollen wir niemals so tun.
  - Wie unterscheiden sich die Laufzeiten?
  - Optimiere die Funktion `double`, sodass sie lineare Laufzeit in der Länge
    der Liste hat.
]

#test[
  Die ```hs (!!)```-Funktion ist unsicher in dem Sinne, dass sie für invalide
  Listenzugriffe einen Fehler wirft. Die Funktion 
  ```hs (!?) :: [a] -> Int -> Maybe a``` ist eine sichere Variante von 
  ```hs (!!)```. Sie macht den Fehlerfall explizit durch die Wahl des 
  Ergebnistypen. Was tut diese Funktion voraussichtlich? Implementiere diese
  Funktion.
  #footnote[Diese Funktion ist auch bereits vorimplementiert: #link("https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:-33--63-")[```hs (!?)``` in ```hs Data.List```].]
]

#pagebreak(weak: true)

= Hinweise zu Tests und Challenges

#context {
  for data in query(metadata).map(el => el.value) {
    if type(data) == dictionary and "type" in data and data.type == "hint" {
      data.value
      v(1em, weak: true)
    }
  }
}

#pagebreak(weak: true)

= Weitere Ressourcen

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

