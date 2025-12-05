#import "@preview/cetz:0.4.2"
#import "@preview/ctheorems:1.1.3": *
#import "@preview/finite:0.5.0"

#set document(
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

#show raw: set text(font: "CaskaydiaCove NF")
#set raw(syntaxes: "syntaxes/prolog.sublime-syntax")

#set text(lang: "de")
#show link: underline
#show heading.where(level: 1): set block(below: 1.25em)
#show math.equation.where(block: false): box
#show raw.where(block: false): box
#show: thmrules

#let is-type = type => {
  val => std.type(val) == dictionary and "type" in val and val.type == type
}

#let get-metadata = type => {
  query(metadata)
    .map(res => res.value)
    .filter(is-type(type))
    .map(res => res.value)
}

#let remark = thmplain(
    "remark",
    "Bemerkung",
    titlefmt: strong,
    separator: h(0.5em)
  ).with(
    numbering: (..args) => args.at(-1)
  )

#let test = thmplain(
    "test",
    "Test",
    titlefmt: strong,
    separator: h(0.5em)
  ).with(
    numbering: (..args) => args.at(-1)
  )

#let challenge = thmplain(
    "challenge",
    "Challenge",
    titlefmt: strong,
    separator: h(0.5em)
  ).with(
    numbering: (..args) => args.at(-1)
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

#let stopHere = block(
  fill: red.lighten(90%),
  inset: 1em,
  above: 2em,
  below: 2em,
  align(center)[
    #text(weight: "bold", fill: red)[
      Welcome weary traveler, you've come far. Why don't you stop here and get
      some rest before you continue your journey.
    ]

    #text(0.8em, fill: red)[
      (Mit den Inhalten der Vorlesung kannst du die Tests und Challenges aktuell
      bis hier hin lösen.)
    ]
  ]
)


#text(0.8em)[
  Dieses Dokument ist vom #datetime.today().display("[day].[month].[year]"). Die
  aktuelle Version des Dokuments kannst du im moodle oder
  #link("https://github.com/Ziharrk/DeklprogSelfTests/raw/refs/heads/main/main.pdf")[direkt von GitHub herunterladen].
]

#context {
  let (test: (_, test), challenge: (_, challenge)) = thmcounters.final().counters
  [
    Dieses Dokument enthält #test Fragen, #challenge kleinere bis
    größere Aufgaben und andere Ressourcen zum Thema Deklarative Programmierung.
    Die Inhalte dieses Dokuments sollen dir helfen, dein Verständnis über
    Haskell und Prolog zu prüfen und zu stärken.
  ]
}

Größere Aufgaben haben wir als Challenges markiert. Diese Aufgaben benötigen
öfter mehrere Konzepte und führen zusätzlich Konzepte ein, die nur für das
Lösen der Aufgabe wichtig sind. Wenn die zusätzlichen Konzepte, dir zu sehr
Schwierigkeiten bereiten, überspringe die entsprechende Frage oder Aufgabe.

Die Nomenklatur des Aufgaben ist aktuell möglicherweise noch etwas willkürlich,
da es Tests gibt, die wie Challenges wirken -- und möglicherweise sogar
andersherum.

Wenn du Anmerkungen oder weitere Ideen für Inhalte für dieses Dokument hast,
dann schreibe uns gerne über z.B. mattermost an -- oder
#link("https://github.com/Ziharrk/DeklprogSelfTests/")[erstellt ein issue oder
stellt eine PR auf GitHub].



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
  #footnote[#link("https://de.wikipedia.org/wiki/Bin%C3%A4re_Exponentiation")[Binäre Exponentiation]].
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
  die Länge einer solchen Liste berechnet werden. Du möchtest nun auch
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

#test[
  In Programmiersprachen wie Java greifen wir Daten komplexer Datentypen zu,
  indem wir auf Attribute von Objekten zugreifen oder getter-Methoden verwenden.
  Wie greifen wir auf Daten in Haskell zu?
]

// Polymorphismus

#test[
  Wie sieht eine Datentypdefinition im Allgemeinen aus?
]

#test[
  Welchen Typ haben
  - ```hs (:)``` und ```hs []```,
  - ```hs Just``` und ```hs Nothing```,
  - ```hs Left``` und ```hs Right```?
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
  Wie gewinnt man aus einem Typkonstruktor einen Typ?
]

#test[
  Visualisiere `[1, 2, 3]` als Baum, wie du es in der Vorlesung kennengelernt
  hast. Zur Erinnerung: die inneren Knoten sind Funktionen und die Blätter
  Werte, die nicht weiter ausgerechnet werden können.
]

#test[
  Ist ```hs [32, True, "Hello, world!"]``` ein valider Haskell-Wert? Warum ja oder nein?
]

#test[
  Was ist der Unterschied zwischen einem Typ und einem Typkonstruktor?
]

#test[
  Gegeben ist
  ```hs
  data Pair a b = Pair a b
  ```
  Wie unterscheidet sich der Typ von
  ```hs
  data Pair a = Pair a a
  ```
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
    - Zur Darstellung der Multimengen eignen sich sortierte Listen gut.
    - Zur Berechnung des Schnittes können zwei sortierte Listen parallel
      durchlaufen werden. Wenn zwei gleiche Elemente zu Beginn der Liste stehen,
      wird eines der Elemente zum Ergebnis hinzugefügt. Im anderen Fall
      überspringen wir das jeweils kleinere Element der beiden.
  ]
]

// ```hs
// gcd :: Int -> Int -> Int
// gcd a 0 = a
// gcd a b = gcd b (a `mod` b)
//
//
// pf :: Int -> [Int]
// pf a = go a [2..a]
//   where
//     go :: Int -> [Int] -> [Int]
//     go 1 _      = []
//     go a (p:xs) | a < p * p = [a]  -- optimization
//                 | r == 0    = p : go q (p:xs)
//                 | otherwise = go a xs
//       where (q, r) = quotRem a p  -- same as (a `div` p, a `mod` p)
//
// -- assumes both lists are sorted
// intersect :: [Int] -> [Int] -> [Int]
// intersect []     _      = []
// intersect _      []     = []
// intersect (x:xs) (y:ys)
//   | x == y    = x : intersect xs ys
//   | x < y     = intersect xs (y:ys)
//   | otherwise = intersect (x:xs) ys
//
// gcd :: Int -> Int -> Int
// gcd a b = product (pf a `intersection` pf b)
// ```

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
] <symbolic_diff>

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
// derive (Ln f)    = derive f :/: f
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

#challenge[
  Entwickle einen Datentyp ```hs Ratio```, um rationale Zahlen
  $ p/q in QQ, quad p in ZZ, q in NN, p "und" q "teilerfremd" $
  darzustellen. Implementiere die Operationen: Addition, Subtraktion,
  Multiplikation, Divison. Implementiere weiter eine Funktion, die die
  rationale Zahl als reelle Zahl mit einer festen Anzahl von Nachkommastellen
  darstellt.
]

// ```hs
// real :: Int -> Ratio Int -> String
// real k x = go 1 p ++ "." ++ go k (10 * (p `mod` q))
//   where
//     p = numerator x
//     q = denominator x
//
//     go 0 _ = ""
//     go _ 0 = ""
//     go k a = show (a `div` q) ++ go (k - 1) (10 * (a `mod` q))
// ```

#test[
  Wie können wir es hinkriegen, dass die invalide Liste ```hs [32, True, "Hello, world!"]```
  ein valider Haskell-Wert wird? Mithilfe welches Hilfstypen kriegen das hin?
]

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

#test[
  ```hs (++) :: [a] -> [a] -> [a]``` wird verwendet, um zwei Listen
  aneinanderzuhängen. Wenn wir eine Funktion induktiv über den Listentypen
  definieren wie z.B. ```hs square :: [Int] -> [Int]```, die jeden Listeneintrag
  quadrieren soll, dann können wir das wie folgt tun.
  ```hs
  square :: [Int] -> [Int]
  square []     = []
  square (x:xs) = [x * x] ++ square xs
  ```
  Die Funktion ist zwar korrekt aber nicht Haskell-idiomatisch, d.h., eine
  Person, die Erfahren im Programmieren von Haskell ist, würde dies nicht so
  schreiben. Was müssten wir an der Funktion ändern, damit sie idiomatisch ist.
]

#test[
  Die Funktion ```hs show``` kann genutzt werden, um Werte eines beliebigen
  Datentyp in eine String-Repräsentation zu überführen. Warum kann
  ```hs show``` nicht als Funktion vom Typ ```hs a -> String``` implementiert
  sein?
]


// Pattern Matching

#challenge[
  In Haskell sind Listen als einfach-verkettete Listen implementiert. Das macht
  sie ungeeignet für Operationen, die wahlfreien Zugriff in konstanter Laufzeit
  benötigen. Darüber hinaus sind Listen auch nicht mutierbar. Das führt dazu,
  dass Operationen, die eine Liste verändern, häufig lineare Laufzeit in der
  Länge der Liste haben.

  Ziel dieser Challenge ist es, eine Datenstruktur zu entwickeln, die eine
  (amortisiert) konstante ```hs append```-Operation hat. Diese ist bekannt als
  ```hs Queue```. Sie soll durch ```hs data Queue a = Q [a] [a]``` dargestellt
  werden. Die Idee ist eine Liste vorzuhalten, die eine (amortisiert) konstante
  ```hs dequeue```-Operationen erlaubt, und eine andere, die eine konstante
  ```hs enqueue```-Operationen erlaubt.

  Implementiere die Funktionen
  - ```hs empty :: Queue a```, die eine leere Queue erzeugt,
  - ```hs front :: Queue a -> a```, die das erste Element in einer queue
    zurückgibt,
  - ```hs isEmpty :: Queue a -> Bool```, die bestimmt, ob eine queue leer ist,
  - ```hs enqueue :: a -> Queue a -> Queue a```, die ein Element an das Ende
    einer queue anhängt,
  - ```hs dequeue :: Queue a -> Queue a```, die das erste Element in einer
    queue entfernt.

  Die Implementierung soll dabei folgende Invariante erfüllen: Eine queue ist
  genau dann leer, wenn die ```hs dequeue```-Liste leer ist. Diese Invariante
  kannst du z.B. mit einer Hilfsfunktion erzwingen -- oder du
  #footnote[
    Für Interessierte: Wenn sogar ```hs length xs >= length ys``` für eine
    queue ```hs Q xs ys``` gewährleistet wird, ist die queue nochmal schneller.
    Dafür muss man die Längen der Listen immer vorhalten. Mehr darüber findest
    du in
    #link("https://www.cambridge.org/core/journals/journal-of-functional-programming/article/simple-and-efficient-purely-functional-queues-and-deques/7B3036772616B39E87BF7FBD119015AB")[Simple and efficient purely functional queues and deques]
    von Chris Okasaki lesen.
  ]
]

// ```hs
// data Queue a = Q [a] [a]
//
// empty :: Queue a
// empty = Q [] []
//
// isEmpty :: Queue a -> Bool
// isEmpty (Q [] _) = True
// isEmpty _        = False
//
// front :: Queue a -> a
// front (Q (x:_) _) = x
// front _           = error "empty queue"
//
// invariant :: Queue a -> Queue a
// invariant (Q [] ys) = Q (reverse ys) []
// invariant q         = q
//
// enqueue :: a -> Queue a -> Queue a
// enqueue x (Q xs ys) = invariant (Q xs (x:ys))
//
// dequeue :: Queue a -> Queue a
// dequeue (Q []     _)  = error "empty queue"
// dequeue (Q (_:xs) ys) = invariant (Q xs ys)
// ```

#challenge[
  In den Übungsaufgaben hast du einen Suchbaum ohne Höhenbalancierung
  implementiert. Die Rotationen für einen AVL-Baum lassen sich durch das
  pattern matching in Haskell vergleichsweise elegant implementieren - erinnere
  dich z.B. an die Implementierung aus Einführung in die Algorithmik, die recht
  verbos ist.

  Die Höhe eines Teilbaums kann z.B. als weiteres Attribut im Knoten gespeichert
  werden. Eine ineffizientere Variante ist es, die Höhe mit einer Funktion
  wiederkehrend zu berechnen. Letztere Variante ist für den Anfang
  übersichtlicher.

  Implementiere eine Funktion ```hs rotate :: SearchTree a -> SearchTree a```,
  die einen Teilbaum an der Wurzel rebalanciert, sollte der Teilbaum
  unbalanciert sein. Diese Funktion kannst du dann nutzen, um die gängigen
  Operationen auf Suchbäumen anzupassen.
  #footnote[#link("https://de.wikipedia.org/wiki/AVL-Baum#Rebalancierung")[Rebalancierung eines AVL-Baum]]
]

// ```hs
// height :: SearchTree a -> Int
// height Empty        = 0
// height (Node l _ r) = max (height l) (height r) + 1
//
// balance :: SearchTree a -> Int
// balance Empty        = 0
// balance (Node l _ r) = height r - height l
//
// rotate :: SearchTree a -> SearchTree a
// rotate Empty          = Empty
// rotate t@(Node l v r) =
//   case (balance t, balance l, balance r) of
//     (2, _, -1) -> rotateL (Node l v (rotateR r))
//     (2, _, _)  -> rotateL (Node l v r)
//     (-2, _, 1) -> rotateR (Node (rotateL l) v r)
//     (-2, _, _) -> rotateR (Node l v r)
//     _          -> t
//   where
//     rotateR (Node (Node ll lv lr) v r) = Node ll lv (Node lr v r)
//     rotateR _                          = error "should not happen"
//
//     rotateL (Node l v (Node rl rv rr)) = Node (Node l v rl) rv rr
//     rotateL _                          = error "should not happen"
// ```


// Automatisches Testen

#test[
  Formuliere QuickCheck-Eigenschaften, die die Funktionen
  - ```hs isElem :: Int -> SearchTree Int -> Bool```,
  - ```hs toList :: SearchTree Int -> Int```,
  - ```hs insert :: Int -> SearchTree Int -> SearchTree Int``` und
  - ```hs delete :: Int -> SearchTree Int -> SearchTree Int```
  erfüllen sollen. ```hs isElem``` überprüft, ob eine Ganzzahl in gegebenen
  Suchbaum enthalten ist. ```hs toList``` konvertiert einen Suchbaum in eine
  Liste. ```hs insert``` fügt eine Ganzzahl in einen Suchbaum ein.
  ```hs delete``` löscht eine Ganzzahl aus einen Suchbaum.

  Wie kannst du die Suchbaum-Eigenschaft spezifizieren (dafür brauchst du
  weitere Funktionen)?
]

#test[
  QuickCheck-Eigenschaften werden mit zufällig generierten Werten getestet.
  Hin und wieder kommt es vor, dass diese Werte Vorbedingungen erfüllen müssen,
  damit wir Eigenschaften von Funktionen testen können. Wie können wir das
  erreichen?
]

#test[
  Wie können wir eine Funktionen teilweise auf Korrektheit testen -- also
  wie können wir für eine beliebige Eingabe verifizieren, dass die Ausgabe
  korrekt ist?
]

// TODO Finde ein besseres Beispiel dafür, dass es keine gute Idee ist, eine
// Implementierung mit einer Referenz-Implementierung zu vergleichen, wenn diese
// sich nicht wesentlich unterscheiden (Fehler in beiden Implementierungen).
//
// #test[
//   Gegeben sei folgendes Haskell-Programm.
//   ```hs
//   data Rose a = Rose a [Rose a]
//
//   sumRose :: Rose Int -> Int
//   sumRose (Rose x ts) = x + sumSubtrees ts
//     where
//       sumSubtrees []     = 0
//       sumSubtrees (t:ts) = sumRose t + sumSubtrees ts
//
//   prop_sum :: Rose a -> Bool
//   prop_sum t = sumRose t == sumRose' t
//     where
//       sumRose' :: Rose Int -> Int
//       sumRose' (Rose x ts) = x + sumSubtrees' ts
//         where
//           sumSubtrees' []     = 0
//           sumSubtrees' (t:ts) = sumRose' t + sumSubtrees' ts
//   ```
//   Bewerte dieses Programm hinsichtlich ```hs prop_sum```. Ist die
//   Referenz-Implementierung ```hs sumRose'``` geeignet gewählt? Wie könnte
//   ```hs sumRose``` besser getestet werden?#footnote[Die Funktion ist
//     vergleichweise einfach, sodass Referenz-Implementierungen vermutlich sehr
//     dicht an der tatsächlichen Implementierung liegen werden. Für komplexere
//     Algorithmen ist der Vergleich mit einer Referenz-Implementierung geeigneter.
//   ]
// ]


// Funktionen höherer Ordnung

#test[
  Was sind Funktionen höherer Ordnung?
]

#test[
  Wie definieren wir Lambda-Abstraktionen bzw. anonyme Funktionen?
]

#test[
  Warum ist der Typ ```hs (a -> b) -> c``` nicht identisch zum Typ
  ```hs a -> b -> c```? Welcher andere Typ ist identisch zu letzterem?
]

#test[
  Mit welchen Konzepten gehen die Linksassoziativität der Funktionsapplikation
  und die Rechtsassoziatvität des Typkonstruktors ```hs (->)``` gut Hand in
  Hand?
]

// TODO möglicherweise gibt es coolere Funktionen, die auch noch ohne folds
//      auskommen - für folds wollte ich einen Test extra haben
#test[
  Zu welchen partiell applizierten Funktionen verhalten sich folgenden
  Funktionen identisch?
  - ```hs succ :: Int -> Int``` (die Inkrementfunktion)
  - ```hs pred :: Int -> Int``` (die Dekrementfunktion)
  - ```hs length :: [a] -> Int```
  - ```hs sum :: [Int] -> Int```
  - ```hs product :: [Int] -> Int```
]

#test[
  Was ist partielle Applikation?
]

#test[
  Was ist Currying?
]

#test[
  Welche Funktionen höherer Ordnung hast du kennengelernt im Kontext der
  generischen Programmierung? Was ist das Ziel dieser Funktionen?
]

#test[
  Die Funktionen ```hs map :: (a -> b) -> [a] -> [b]``` und
  ```hs filter :: (a -> Bool) -> [a] -> [a]``` lassen sich alle mithilfe
  ```hs foldr :: (a -> r -> r) -> r -> [a] -> r``` ausdrücken. Wie erreichen wir
  dies?
]

#test[
  Gegeben seien folgende Funktionen:
  - ```hs rgbToHsv :: RGB -> HSV```, die eine Farbe von einer Darstellung
    in einen anderen konvertiert, und
  - ```hs hue :: HSV -> Float```, die den Farbwert einer Farbe im Wertebereich
    $[0°, 360°)$ im HSV-Farbraum zurückgibt.
  Du bekommst als Eingabe einen Bild, das hier als Liste von ```hs RGB```-Werten
  dargestellt ist. Jeder ```hs RGB```-Wert korrespondiert zu einem Pixel.
  Schreibe eine Funktion, die berechnet, wie viele blaue Pixel das Bild hat.
  Hier bezeichene eine Farbe als blau, wenn ihr Farbwert zwischen $200°$ und
  $250°$ (inklusiv) liegt. Nutze für die Definition der Funktion sowohl
  ```hs map``` als auch ```hs filter```.
]

#test[
  Was sind sections im Kontext von Funktionen höherer Ordnung?
]

#test[
  Welche der Faltungsfunktion auf Listen ergibt sich aus dem Verfahren zur
  Erzeugung von Faltungsfunktionen, das du für beliebige Datentypen
  kennengelernt hast?
]

#test[
  Was ist der Unterschied zwischen ```hs foldl``` und ```hs foldr```?
  Wann liefern ```hs foldl``` und ```hs foldr``` das gleiche Ergebnis?
]

#test[
  Wie gewinnen wir aus ```hs foldr``` die Identitätsfunktion auf Listen?
  In den Übungen hast du gelernt, wie man Werte anderer Typen falten kann.
  Wie gewinnt man aus diesen Funktionen die Identitätsfunktionen auf den
  jeweiligen Typen?
]

#test[
  Gegeben sind folgende Datentypen
  - ```hs data Tree a = Empty | Leaf a | Branch (Tree a) (Tree a)```,
  - ```hs data Rose a = Node [Rose a]```.
  Welche Typen haben die jeweiligen Datenkonstruktoren und wie führen wir diese
  in die Signatur der jeweiligen Faltungsfunktion über?
  Wo benötigen wir rekursive Aufrufe der jeweiligen Faltungsfunktionen?
]

#test[
  Betrachte die Funktion
  ```hs
  f :: [a] -> b
  f []     = e
  f (x:xs) = g x (f xs)
  ```
  Nach diesem induktiven Muster sind viele Funktionen auf Listen implementiert.
  Nehme als Beispiel die Funktion ```hs sum :: [Int] -> [Int]```.
  ```hs
  sum :: [Int] -> Int
  sum []     = 0
  sum (x:xs) = x + sum xs
  ```
  Dieses Muster haben wir in ```hs foldr``` abstrahiert. Wo wandern die
  jeweiligen Bestandteile der abstrakten Funktion ```hs f``` hin, wenn wir
  ```hs f``` mithilfe von ```hs foldr``` definieren. Was passiert insbesondere
  mit dem rekursiven Aufruf von ```hs f```?
]

#test[
  Wie kannst du mithilfe von Faltung viele Elemente in einen Suchbaum einfügen
  oder lösen? Implementiere
  - ```hs insertMany :: [Int] -> SearchTree Int -> SearchTree Int``` und
  - ```hs deleteMany :: [Int] -> SearchTree Int -> SearchTree Int```.
  Du kannst davon ausgehen, dass du die Einfüge- und Löschfunktion für einzelne
  Elemente bereits hast.
]

#test[
  Wir können ```hs map :: (a -> b) -> [a] -> [b]``` mithilfe von
  ```hs foldr``` wie folgt implementieren:
  #align(center)[```hs map f xs = foldr (\x ys -> f x : ys) [] xs```]
  Vereinfache den Lambda-Ausdruck mithilfe von Funktionen höherer Ordnung.
]

#test[
  Wenn wir die Listenkonstruktoren in ```hs foldr``` einsetzen, erhalten wir die
  Identitätsfunktion auf Listen, also
  #align(center)[```hs foldr (:) [] :: [a] -> [a]```.]
  Wenn wir das Gleiche mit ```hs foldl``` und angepassten ```hs (:)``` machen,
  also
  #align(center)[```hs foldl (flip (:)) [] :: [a] -> [a]```,]
  dann erhalten wir nicht die Identitätsfunktion auf Listen. Warum und was
  bekommen wir stattdessen heraus?
]

#test[
  Es gibt viele andere hilfreiche Funktionen höherer Ordnung in der Haskell
  Prelude. Eine von diesen ist ```hs zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]```.
  Sie verknüpft jeweils zwei Elemente aus den jeweiligen Listen unter der
  gegeben Funktion.
  - Implementiere ```hs zipWith``` mithilfe von ```hs map, uncurry, zip```.
  - Implementiere ```hs zip``` mithilfe von ```hs zipWith```.
  - Implementiere das Prädikat ```hs isSorted``` mithilfe von ```hs zipWith```.
]

#test[
  Das Pendant zum Falten #strike[ist das Bügeln] mit ```hs foldr``` ist
  ```hs unfoldr``` (aus ```hs Data.List```). Anstatt eine Liste von Werten zu
  falten, können wir mit ```hs unfoldr``` aus einem Wert eine Liste erzeugen.
  Die Funktion hat den Typ ```hs (b -> Maybe (a, b)) -> b -> [a]```.
  - Überlege dir anhand des Typs, wie diese Funktion implementiert sein könnte.
    Implementiere sie anschließend.
  - Können wir ```hs foldr (+) 0 [3, 1, 4, 1, 5]``` mit ```hs unfoldr```
    rückgängig machen? Das heißt, können wir die Eingabeliste rekonstruieren,
    ohne Annahmen darüber zu machen, wie die Eingabeliste entstanden ist?
  - Berechne die Binärdarstellung einer natürlichen Zahl mithilfe von
    ```hs unfoldr```. (Das LSB soll an erster Stelle der Ausgabeliste stehen.)
    Implementiere dies als Funktion ```hs bits :: Int -> [Int]```.
  - Implementiere ```hs map``` mithilfe von ```hs unfoldr```.
]

#test[
  In diesem Test wollen wir einen beliebigen Wert zu einen Baum entfalten.
  Implementiere eine Funktion ```hs unfoldTree :: (b -> Maybe (b, a, b)) -> b -> Tree a```,
  die eine Funktion nimmt, die ein Wert von Typ ```hs b``` nimmt und in eine
  Knotenbeschriftung und zwei weitere Wert vom Typ ```hs b``` aufspaltet, oder
  das Entfalten stoppt, indem ein ```hs Nothing``` zurückgegeben wird. Bäume
  sind durch den Datentyp ```hs data Tree a = Empty | Node (Tree a) a (Tree a)```
  definiert.
]

#challenge[
  Gegeben sei die Funktion Faltungsfunktion
  ```hs foldTree :: (r -> a -> r -> r) -> r -> Tree a -> r```
  für einen knotenbeschrifteten Binärbaum gegeben durch
  ```hs data Tree a = Empty | Node (Tree a) a (Tree a)```.

  Wie auch für Listen lassen sich eine Reihe von bekannten Funktionen
  auf Bäume übertragen.
  #footnote[
    Diese Funktionen lassen sich auf alle faltbaren Datentypen verallgemeinern.
    Für Interessierte: Dies wird mithilfe der #link("https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-Foldable.html")[Typklasse ```hs Foldable```]
    festgehalten -- diese Typklasse behandeln wir aber in der Vorlesung
    voraussichtlich nicht.
  ]
  Implementiere die Funktionen
  - ```hs any :: (a -> Bool) -> Tree a -> Bool``` und ```hs and :: (a -> Bool) -> Tree a -> Bool```,
  - ```hs elem :: Int -> Tree Int -> Bool``` und ```hs notElem :: Int -> Tree Int -> Bool```,
  - ```hs toList :: Tree a -> Bool```,
  - ```hs null :: Tree a -> Bool``` (überprüft, ob der Baum leer ist),
  - ```hs length :: Tree a -> Int```,
  - ```hs maximum :: Tree Int -> Int``` und ```hs minimum :: Tree Int -> Int```, und
  - ```hs sum :: Tree Int -> Int``` und ```hs product :: Tree Int -> Int```.
]

#test[
  Gegeben seien die Funktion
  ```hs
  f :: a -> b
  g :: a -> b -> c
  ```
  sowie die Kompositionsfunktion ```hs (.) :: (b -> c) -> (a -> b) -> a -> c```.

  In der Typdefinition von ```hs (.)``` scheint das erste Argument, eine
  einstellige Funktion zu sein. Ist der Ausdruck
  #align(center)[```hs g . f```]
  trotzdem typkorrekt, obwohl ```hs g``` eine zweistellige Funktion ist?
  Wenn ja, wie werden die Typvariablen -- insbesondere das ```hs c``` der
  Komposition -- unifiziert?
]

#test[
  Gegeben sei der Datentyp ```hs data Tree a = Empty | Node (Tree a) a (Tree a)```
  und die Faltungsfunktion ```hs foldTree :: r -> (r -> a -> r -> r) -> Tree a -> r```.

  Vergewissere dich, dass die Implementierung der folgenden Funktion, die alle
  Beschriftungen durch den gleichen Wert ersetzt, korrekt ist.
  ```hs
  replace :: b -> Tree a -> Tree b
  replace x = foldTree Empty (const . flip Node x)
  ```
  Zeige, dass ```hs const . flip Node x = \l y r -> Node l x r``` ist.
]

// ```hs
//   const . flip Node x               -- Definition (.)
// = \l -> const (flip Node x l)       -- Definition flip
// = \l -> const (Node l x)            -- Definition const
// = \l -> \y -> Node l x              -- Notation
// = \l y -> Node l x                  -- Eta-Expansion
// = \l y r -> Node l x r
// ```

#test[
  Welche Funktion verbirgt sich hinter ```hs foldr ((++) . f) []``` und was ist
  ihr Typ?
]

#test[
  Versuche in den folgenden Ausdrücken, Teilausdrücke schrittweise durch
  bekannte Funktionen zu ersetzen und gegebenenfalls zu vereinfachen.
  - ```hs foldr (\x ys -> f x : ys) [] (foldr (\x ys -> g x : ys) [] xs)```,
  - ```hs map (\_ -> y) xs```,
  - ```hs foldr (\x ys -> if x `mod` 2 == 1 then x - 1 : ys else ys) [] xs```,
  - ```hs foldl (\ys x -> x : ys) [] xs``` und
  - ```hs flip (curry fst) x```.
    #footnote[
      #link("https://www.youtube.com/watch?v=_oNgyUAEv0Q")["Your scientists were
      so preoccupied with whether or not they could, that they didn't stop to
      think if they should."] Jenseits solcher kleinen Verständnisfragen gilt
      weiterhin, dass wir verständlichen Code schreiben wollen. Solche Ausdrücke
      sind häufig schwieriger zu verstehen -- auch wenn es unterhaltsam ist, sich
      solche Ausdrücke auszudenken.
    ]
]

#challenge[
  Sei $m, n in NN$. Gegeben seien
  - die Identitätsfunktion auf ${0, ..., n - 1}$ mit
    $pi_0 : {0, ..., n - 1} -> {0, ..., n - 1}, x |-> x$ und
  - eine endliche Folge von Paaren $((a_i, b_i))_(i in {1, ..., m})$ mit
    $a_i in {0, ..., n - 1}, b_i in {0, ..., n - 1}$ für alle
    $i in {1, ..., m}$.

  Wir definieren $pi_(i,j)$ als
  $
  pi_(i,j) (k) = cases(
    i & quad "falls" k = j,
    j & quad "falls" k = i,
    k & quad "sonst"
  )
  $
  für alle $k in {0, ..., n - 1}$.

  Wir betrachten die Paare als Vertauschungen der Bilder der Abbildung
  $pi_0$, d.h., $ pi_i = pi_(a_i, b_i) compose pi_(i - 1) "für" i in {1, ..., m}. $
  - Implementiere eine Funktion ```hs swaps :: Int -> [(Int, Int)] -> [Int]```,
    die $pi_m$ mithilfe von Listen berechnet. Der erste Parameter bestimmt die
    Menge ${0, ..., n - 1}$ und der zweite die Folge.
  - Implementiere eine Funktion
    ```hs swaps :: Int -> [(Int, Int)] -> Int -> Int```, die $pi_m$
    mithilfe von Funktion berechnet. (Hier ist der erste Parameter unter
    Umständen redundant.)
  - Welche Vor- und Nachteile haben die jeweiligen Ansätze im Vergleich?
]

// ```hs
// swaps :: Int -> [(Int, Int)] -> [Int]
// swaps n = foldr (map . sw) [0..n - 1]
//   where
//     sw (a, b) x | x == a    = b
//                 | x == b    = a
//                 | otherwise = x
//
// swaps :: Int -> [(Int, Int)] -> Int -> Int
// swaps n = foldr (\ab p -> sw ab . p) id
//   where
//     sw (a, b) x | x == a    = b
//                 | x == b    = a
//                 | otherwise = x
// ```

#test[
  Eta-reduziere die folgende Ausdrücke:
  - ```hs sum xs = foldr (+) 0 xs```,
  - ```hs add a b = a + b``` und
  - ```hs \x ys -> (:) x ys```.
]

#test[
  Implementiere die Funktion
  ```hs insert :: Int -> a -> Map Int a -> Map Int a```,
  die ein Schlüssel-Wert-Paar in eine ```hs Map Int a``` einfügt.
  Die ```hs Map``` ist wie folgt repräsentiert ```hs type Map k v = k -> v```.
]

#test[
  Wir haben ```hs foldr :: (a -> b -> b) -> b -> [a] -> b``` als natürliche
  Faltungsfunktion kennengelernt, die einen Ausdruck erzeugt, der rechts
  geklammert ist. Zum Beispiel gilt
  #align(center)[```hs foldr (+) 0 [1, 2, 3] = 1 + (2 + (3 + 0))```.]

  Das gleiche Ziel können wir mit anderen Typen verfolgen. Implementiere
  eine Funktion ```hs foldr :: (a -> b -> b) -> b -> Tree a -> b``` für einen
  blattbeschrifteten Binärbaum ```hs data Tree a = Leaf a | Tree a :+: Tree a```,
  die den gleichen Ausdruck erzeugt. Zum Beispiel soll
  #align(center)[
    ```hs foldr (+) 0 ((Leaf 1 :+: Leaf 2) :+: Leaf 3) = 1 + (2 + (3 + 0))```
  ]
  gelten.
]

#test[
  Gegeben sei folgendes Python-Programm.
  ```py
  from dataclasses import dataclass
  from typing import Generic, TypeVar


  class Foldable():
    def foldr(self, f):
      pass

    def sum(self):
      return self.foldr(lambda x: lambda ys: x + ys)(0)

    def toList(self):
      return self.foldr(lambda x: lambda ys: [x] + ys)([])

    def __len__(self):
      return self.foldr(lambda _: lambda s: 1 + s)(0)

    def __contains__(self, y):
      return self.foldr(lambda x: lambda z: z or x == y)(False)

    # ...


  T = TypeVar('T')

  class Tree(Generic[T], Foldable):
    def foldr(self, f):
      def foldr_with_f(e):
        match self:
          case Empty():
            return e
          case Node(l, x, r):
            y = f(x)(r.foldr(f)(e))
            z = l.foldr(f)(y)
            return y
      return foldr_with_f

  @dataclass
  class Empty(Tree[T]):
    pass

  @dataclass
  class Node(Tree[T]):
    left: Tree[T]
    value: T
    right: Tree[T]


  tree = Node(Empty(), 3, Node(Node(Empty(), 7, Empty()), 4, Empty()))
  print(tree.sum())  # 14
  print(tree.toList())  # [3, 7, 4]
  print(len(tree))  # 3
  print(3 in tree, 9 in tree)  # True False
  ```
  In diesem Programm werden viele Konzepte verwendet, die du im Haskell-Kontext
  kennengelernt hast -- aber wahrscheinlich bisher nicht in Python gesehen hast.
  In diesem Test geht es darum, die diese Konzepte zu identifizieren.

  Wo findest du
  - Funktionen höherer Ordnung,
  - pattern matching,
  - algebraische Datentypen (Typkonstruktoren, Datenkonstruktoren),
  - parametrischen Polymorphismus
    #footnote[
      Typannotationen in Python sind nicht sonderlich elegant. Deshalb sind
      nur die angegeben, um den parametrischen Polymorphismus zu identifizieren
      und data classes anständig zu nutzen.
    ],
  - ad-hoc Polymorphismus (Typklassen bzw. Überladung) und
  - lokale Definitionen.

  #text(0.8em)[
    Data classes und match statements brauchst du dir jenseits dieses Tests
    nicht anschauen (wenn es dich nicht weiter interessiert). Es soll in dem
    Test nur darum gehen, die Haskell-Konzepte zu erkennen. In
    @typeclasses_in_python_remark kannst du das gleiche Programm in Java sehen.
  ]
] <typeclasses_in_python>

#test[
  In das folgende Python-Programm hat sich ein bug hineingeschlichen.
  ```py
  text = 'Ja, ja, ich back mir \'nen Kakao!'
  say_words = []

  for word in text.split():
    say_words.append(lambda sep: print(word, end=sep))

  for say_word in say_words[:-1]:
    say_word(' ')
  say_words[-1]('\n')
  ```
  Dieses Programm gibt sieben Mal "Kakao!" aus. Erkläre wie dieses Verhalten
  zustande kommt? Wie kannst du den bug beheben? Kann der gleiche Fehler in
  Haskell passieren?
]


// Abstrakte Datentypen

#test[
  Was ist ein abstrakter Datentyp? Was sind die Bestandteile eines abstrakten
  Datentyps?
]

#test[
  Wie definieren wir die Semantik der zu einem abstrakten Datentyp gehörenden
  Operationen? Wie definieren wir sie insbesondere nicht?
]

#test[
  Wieso ist das sofortige Nutzen einer Gleichheit auf einem abstrakten Datentypen
  problematisch? Was sollte man stattdessen tun?
]

#test[
  Zur Spezifikation der Semantik nutzen wir Gesetze, die bestimmen, wie
  verschiedene Operationen miteinander interagieren. Dafür benötigen wir
  verschiedene Werte oftmals unterschiedlicher Datentypen. Wo kommen diese her
  und wie sind sie quantifiziert?
]

#test[
  Welche Eigenschaften sollten die für einen abstrakten Datentypen formulierten
  Gesetze erfüllen, damit sie eine sinnvolle Semantik beschreiben?
]

#challenge[
  Gebe folgende abstrakte Datentypen an: Paar, Menge, stack, queue,
  double-ended queue, knotenbeschrifteter Binärbaum, priority queue.

  Anschließend kannst du diese auch (naiv) implementieren und deine Implementierung
  testen, indem du deine formulierten Gesetze mit QuickCheck implementierst.
]

#test[
  Als Teil eines ADTs für Arrays soll eine Operation
  ```hs reverse :: Array a -> Array a``` spezifiziert werden, die ein Array
  umdreht. Ihr Verhalten soll unter anderem durch das folgende Gesetz festgehalten sein:
  #align(center)[```hs reverse (reverse a) == a``` #h(1em) für alle Arrays ```hs a```]
  Warum ist dieses Gesetz problematisch? Wie können wir das Problem beseitigen?
]

#test[
  Als Teil eines ADTs für Array soll eine Operation ```hs at :: Array a -> Int -> a```
  spezifiziert werden, die das Element an einer Position in einem Array zurückgibt.
  Weiter soll ```hs update :: Int -> a -> Array a -> Array a``` einen Wert an
  eine Position in ein Array schreiben.

  Wie können wir spezifizieren, dass durch ein ```hs update``` nur das Element
  an der gegebenen Position verändert wird?
]

// update k x a `at` k == x
// j /= k ==> update k x a `at` j == a `at` j

#test[
  Eine Teilmenge der Operationen für eine Menge sind
  - ```hs insert :: a -> Set a -> Set a``` zum Einfügen von einem Wert in eine
    Menge und
  - ```hs size :: Set a -> Int``` zum Bestimmen der Kardinalität einer Menge.

  Gegeben ist folgendes Gesetz:
  #align(center)[
    ```hs size (insert y (insert x s)) == size s + 2``` #h(1em) für alle
    Werte ```hs x, y```, alle Mengen ```hs s```.
  ]
  Das Gesetz ist falsch. Warum und wie können es korrigieren?
]

#test[
  In ADT-Gesetzen sind Variablen allquantifiziert. Wie können wir gewährleisten,
  dass ein Wert bestimmte Bedingungen erfüllt, bevor wir ein entsprechendes
  Gesetz für solche Werte definieren?
]

#test[
  Warum benötigen wir Konstruktoren als Teil eines ADTs?
]


// Typklassen und Überladung

#test[
  Was sind Typklassen?
]

#test[
  Wie unterscheidet sich der Polymorphismus, der durch Typklassen ermöglicht
  wird, vom parametrischen Polymorphismus?
]

#test[
  In einem vorherigen Test wurdest du bereits gefragt, wieso ```hs show```
  nicht als Funktion mit dem Typ ```hs a -> String``` implementiert sein.
  Wieso wird die Funktion durch den Typ ```hs Show a => a -> String``` gerettet?
]

#test[
  Welche Typklassen kennst du? Was ermöglichen sie konkret?
]

#test[
  Eine ```hs Show```-Instanz für den Typ
  ```data Tree a = Leaf a | Tree a :+: Tree a```
  könnte wie folgt aussehen:
  ```hs
  instance Show a => Show (Tree a) where
    show (Leaf x)  = "Leaf " ++ show x
    show (l :+: r) = "(" ++ show l ++ ") :+: (" ++ show r ++ ")"
  ```
  Welche Werte vom Typ ```hs Tree a``` führen zur worst-case Laufzeit
  und welche zur best-case Laufzeit? Die Anzahl der Blätter soll hier frei
  sein. Welche Eigenschaften von ```hs (++)``` führen zu den jeweiligen
  Laufzeiten?
]

#test[
  Überlade die Operationen ```hs (+), (-), (*), abs, signum, fromInteger```
  für den Datentypen ```hs data Mat22 a = Mat22 a a a a```, der
  $(2 times 2)$-Matrizen repräsentieren soll -- ```hs abs, signum, fromInteger```
  kannst du z.B. komponentenweise implementieren.
  #footnote[
    Oft sind an Funktionen von Typklassen Bedingungen bzw. Gesetze, die erfüllt
    werden sollen, gekoppelt. Das für ```hs abs``` und ```hs signum``` wird
    durch den Vorschlag nicht erfüllt.
  ]
]

#test[
  Mit $ mat(f_(n+1), f_n; f_n, f_(n-1))^n = mat(1, 1; 1, 0)^n $ und der
  binären Exponentiation und ```hs Mat22 Integer``` aus einem vorherigen Tests
  kannst du die $n$-te Fibonacci-Zahl in logarithmischer Laufzeit in $n$
  berechnen. Implementiere das Verfahren.

  #text(0.8em)[
    Da du eine ```hs Num```-Instanz auf ```hs Mat22``` definiert hast, kannst
    du den ```hs (^)```-Operator zum binären Exponentiation nutzen.
  ]
]

#challenge[
  In dieser Challenge sollst du automatisches Differenzieren im Rückwärtsmodus
  mithilfe von (Operator-)Überladung implementieren. Dieser Ansatz des
  Differenzierens führt dabei das Differenzieren komplizierter Funktionen auf
  einfache, elementare Funktionen zurück.

  Wir verwenden folgenden Datentyp: ```hs data D a = D a a```.
  Ein Wert vom Typ ```hs D a``` enthält einen Funktionswert und seine Ableitung.

  Der Kern der Idee ist, Funktionen so zu überladen, dass sie auf
  ```hs D a```-Werte angewendet werden können. Angenommen, es sei eine Funktion
  ```hs f``` und ihre Ableitungsfunktion ```hs f'``` gegeben. Dann soll ein
  überladenes ```hs f``` wie folgt funktionieren
  ```hs
  f :: D a -> D a
  f (D gx dgdx) = D (f gx) (dgdx * f' gx)
  ```
  Der Wert ```hs gx``` ist das Ergebnis einer inneren Funktion ```hs g```,
  und ```hs dgdx``` entspricht deren Ableitung ```hs g'``` an der Stelle ```hs x```
  (bzw. $(d g)/(d x) (x)$). Die Kettenregel führt dann zu
  ```hs (f . g)' z = g' z * f' x```. Nach dem Muster kannst du nun
  Standardfunktionen überladen. Für die arithmetischen Operatoren benötigst du
  an der Stelle deren Ableitungsregeln (Summenregel, Leibnizregel, usw.)

  Implementiere die Typklasseninstanzen ```hs Num```, ```hs Fractional```
  und ```hs Floating```.
  #footnote[
    #link("https://de.wikipedia.org/wiki/Differentialrechnung#Zusammenfassung")[Zusammenfassung der Ableitungsregeln]
  ]

  Mit den folgenden Funktionen kannst du dann die erste, zweite oder dritte
  Ableitung bilden.
  #footnote[
    Für Interessierte: In @reverse_mode_ad_remark kannst du eine allgemeinere
    Funktion zum Berechnen der Ableitung sehen.
  ]
  ```hs
  d1 :: Num a => (D a -> D b) -> a -> b
  d1 f x = let (D _ d) = f (D x 1) in d

  d2 :: Num a => (D (D a) -> D (D b)) -> a -> b
  d2 f x = let (D (D _ _) (D _ d)) = f (D (D x 1) 1) in d

  d3 :: Num a => (D (D (D a)) -> D (D (D b))) -> a -> b
  d3 f x = let (D _ (D _ (D _ d))) = f (D (D (D x 1) 1) 1) in d
  ```

  #hint[
    Wenn du Anlaufschwierigkeiten hast, helfen dir möglicherweise diese ersten
    Implementierungen weiter.
    ```hs
    instance Num a => Num (D a) where
      D x1 d1 + D x2 d2 = D (x1 + x2) (d1 + d2)  -- Summenregel
      -- ...

    instance Fractional a => Fractional (D a) where
      -- ...

    instance Floating a => Floating (D a) where
      exp (D x d) = D (exp x) (d * exp x)  -- Kettenregel und exp'(x) = exp(x)
      -- ...
    ```
  ]
] <reverse_mode_ad>

// ```hs
// instance Num a => Num (D a) where
//   D x1 d1 + D x2 d2 = D (x1 + x2) (d1 + d2)
//   D x1 d1 - D x2 d2 = D (x1 - x2) (d1 - d2)
//   D x1 d1 * D x2 d2 = D (x1 * x2) (d1 * x2 + x1 * d2)
//   negate (D x d)    = D (negate x) (negate d)
//   abs (D x d)       = D (abs x) (abs d)
//   signum (D x d)    = D (signum x) (d * signum x)
//   fromInteger x     = D (fromInteger x) 0
//
// instance Fractional a => Fractional (D a) where
//   D x1 d1 / D x2 d2 = D (x1 / x2) ((d1 * x2 + x1 * d2) / (x2 * x2))
//   fromRational r    = D (fromRational r) 0
//
// instance Floating a => Floating (D a) where
//   pi = D pi 0
//   exp (D x d)   = D (exp x)   (d * exp x)
//   log (D x d)   = D (log x)   (d / x)
//   sin (D x d)   = D (sin x)   (d * cos x)
//   cos (D x d)   = D (cos x)   (d * negate (sin x))
//   tan (D x d)   = D (tan x)   (d / (cos x) ^ 2)
//   asin (D x d)  = D (asin x)  (d / sqrt (1 - x * x))
//   acos (D x d)  = D (acos x)  (d / negate (sqrt (1 - x * x)))
//   atan (D x d)  = D (atan x)  (d / (1 + x * x))
//   sinh (D x d)  = D (sinh x)  (d * cosh x)
//   cosh (D x d)  = D (cosh x)  (d * sinh x)
//   asinh (D x d) = D (asinh x) (d / sqrt (1 + x * x))
//   acosh (D x d) = D (acosh x) (d / sqrt (x * x - 1))
//   atanh (D x d) = D (atanh x) (d / (1 - x * x))
// ```

An vielen Stellen in den bisherigen Selbsttests haben wir oft einen konkreten
Typ (z.B. ```hs Int```) genutzt, für den es bestimmte Typklasseninstanzen
gibt. Das ist meistens der Fall gewesen, wenn wir Gleichheit auf Werten oder
eine Vergleichsoperation auf Werten brauchten. Schau dir die bisherigen
Selbsttests erneut an und überlege dir, wo du Typen verallgemeinern kannst.

#test[
  Welche Funktionen musst du implementieren, damit eine ```hs Eq```-Instanz
  vollständig definiert ist? Welche Gesetze sollten die Funktionen einer
  ```hs Eq```-Instanz erfüllen?
]

#test[
  Gegeben sei der Typ
  #align(center)[```hs data Tree a b c = Empty | Leaf a | Node (Tree a b c) Int c (Tree a b c)```.]
  Implementiere eine ```hs Eq```-Instanz für diesen Typen. Die Gleichheit soll
  sich so verhalten, wie die die wir durch das Ableiten bekommen würden.
  Bevor du die Instanz implementierst, überlege dir:
  - Wie viele Regeln brauchst du mindestens, um ```hs (==)``` zu definieren?
  - Benötigst du für die Implementierung Typeinschränkungen? Wenn ja, für welche
    Typen?
  - An welchen Stellen wirst du ```hs (==)``` rekursiv anwenden?
  - Die Datenkonstruktoren sind auf den rechten Seiten der Regeln nicht relevant.
    Auf welchen Typen kannst du die Gleichheit für z.B. ```hs Node```
    zurückführen, bzw. wenn du dir die rechte Seite der Regel für ```hs Node```
    anschaust, welche Typen fallen dir ein, für die diese rechte Seite auch
    eine Gleichheit definieren würde?
]

#test[
  Welche Funktionen musst du implementieren, damit eine ```hs Ord```-Instanz
  vollständig definiert ist? Welche Gesetze sollten die Funktionen einer
  ```hs Ord```-Instanz erfüllen?
]

#test[
  Gegeben sei der Typ
  #align(center)[```hs data Tree a b c = Empty | Leaf a | Node (Tree a b c) Int c (Tree a b c)```.]
  Implementiere eine ```hs Ord```-Instanz für diesen Typen. Die Ordnung soll
  sich so verhalten, wie die die wir durch das Ableiten bekommen würden.
  Bevor du die Instanz implementierst, überlege dir:
  - Spielt die Reihenfolge, in der wir die Datenkonstruktoren definieren eine
    Rolle für die Ordnung? Wenn ja, wie?
  - Wie viele Regeln brauchst du mindestens, um ```hs compare``` zu definieren?
    Wie auch bei der Typklasse ```hs Eq``` können wir eine Regel definieren,
    die alle Fälle abdeckt, in denen wir ```hs GT``` erhalten, wenn wir uns
    nur die Datenkonstruktoren anschauen. Welches Schema müssen wir für die
    anderen Regeln verwenden, damit das funktioniert?
  - Benötigst du für die Implementierung Typeinschränkungen? Wenn ja, für welche
    Typen?
  - An welchen Stellen wirst du ```hs compare``` rekursiv anwenden?
  - Die Datenkonstruktoren sind auf den rechten Seiten der Regeln nicht relevant.
    Auf welchen Typen kannst du die Ordnung für z.B. ```hs Node```
    zurückführen, bzw. wenn du dir die rechte Seite der Regel für ```hs Node```
    anschaust, welche Typen fallen dir ein, für die diese rechte Seite auch
    eine Ordnung definieren würde?

  Implementiere die Ordnung auch erneut mit ```hs (<=)```.
]

#test[
  Die Typklasse ```hs Ord``` ist wie folgt definiert:
  ```hs
  class Eq a => Ord a where
    compare              :: a -> a -> Ordering
    (<), (<=), (>), (>=) :: a -> a -> Bool
    max, min             :: a -> a -> a

    -- default definitions
    -- ...

    {-# MINIMAL compare | (<=) #-}
  ```
  Das ```hs {-# MINIMAL compare | (<=) #-}``` bedeutet, dass es genügt,
  entweder ```hs compare``` oder ```hs (<=)``` zu implementieren.
  - Gebe Standarddefinitionen für die Funktionen der Typklasse an.
  - Was ermöglicht es, eine Standarddefinition für ```hs compare``` angeben zu
    können?
  - Deine Standarddefinition von ```hs compare``` ist voraussichtlich
    ineffizient.#footnote[Die Vordefinierte ist es auch, also keine Sorge.]
    Woran liegt das? Mit Hinsicht auf Effizienz -- welche der beiden Funktionen
    würdest du implementieren, wenn du nur eine implementieren dürftest?
    #footnote[
      Auch wenn Standarddefinitionen für den Anfang hilfreich sind, um
      mit minimalem Aufwand alle Funktionen einer Typklasse zu verwenden, findet
      man häufig konkrete Implementierungen für mehr als nur die Funktionen, für
      die es notwendig ist.
    ]
]

#test[
  In nicht streng getypten Programmiersprachen haben wir oft mit impliziter
  Typkonversion zu tun. #footnote[Diese wollen wir nun für einen Moment nach Haskell
  zurückholen, um sie dann ganz schnell wieder zu vergessen.] Implementiere eine
  Funktion ```hs ifThenElse```, die als Bedingung Werte beliebiger Typen
  entgegennehmen kann. Ziel ist es, dass der folgende Ausdruck ausgewertet
  werden kann.
  ```hs
  let a = ifThenElse 0 3 4
      b = ifThenElse [5] 6 7
      c = ifThenElse Nothing 8 9
   in a + b + c  -- 19
  ```
]

#test[
  Eine Halbgruppe ist eine Struktur $(H, ast.op)$, wobei $ast.op$ eine
  assoziative, binäre Verknüpfung $ast.op : H times H -> H$ ist. Ein Monoid
  erweitert die Halbgruppe um ein neutrales Element bzgl. $ast.op$.

  Definiere Typklassen ```hs Semigroup``` und ```hs Monoid```, diese Strukturen
  implementieren. Gebe auch beispielhaft ein paar Instanzen für diese an.
]

#test[
  Wo findest du das Konzept der Typklassen in Programmiersprachen wie z.B.
  Python oder Java wieder? Gibt es z.B. ein Pendant zur ```hs Show```-Typklasse
  in diesen Programmiersprachen?
]


// Lazy Evaluation

#test[
  Was ist Lazy Evaluation?
]

#test[
  Wie werden Berechnungen in Haskell angestoßen? Wie viel wird berechnet?
]

#test[
  Gebe ein Beispiel an, das zeigt, dass die faule Auswertung
  berechnungsstärker ist?
]

#test[
  Welche praktischen Vorteile ergeben sich aus der Lazy Evaluation?
]

#test[
  Wie werden mehrfache Berechnungen in einer nicht-strikten
  Auswertungsstrategie vermieden?
]

#test[
  Gegeben sei folgendes Haskell-Ausdruck.
  ```hs
  let c = x == 0
      a = u `div` x
      b = 0
   in if c then b else a
  ```
  Der Teilausdruck ```hs a = u `div` x``` erscheint auf dem ersten Blick
  problematisch, da ```hs x``` Null sein könnte. Wieso stellt das mit
  Lazy Evaluation kein Problem dar?
]

#test[
  Eine zyklische einfach-verkettete Liste können wir in Python z.B. so
  definieren.
  ```py
  class Node:
    def __init__(self, value):
      self.value = value
      self.next = None

  one, two = Node(1), Node(2)
  one.next, two.next = two, one
  ```
  Wenn wir mit ```py one``` starten, dann korrespondiert diese verkettete Liste
  mit der unendlichen Liste ```py [1, 2, 1, 2, ...]```.

  Die Mutierbarkeit des ```hs next```-Zeigers macht das Verlinken der Knoten in
  Python möglich. Wie können wir in Haskell, trotz der Abwesenheit von
  Mutierbarkeit, zyklische Datenstrukturen umsetzen? Versuche, dein Programm
  ähnlich zum Python-Programm aussehen zu lassen.
  #footnote[
    Für Interessierte: Die Technik ist als
    #link("https://wiki.haskell.org/index.php?title=Tying_the_Knot")[Tying the Knot]
    bekannt.
  ]
]

#challenge[
  Gegeben sei der Datentyp
  #align(center)[```hs data Tree a = Empty | Node (Tree a) a (Tree a)```.]

  - Implementiere eine Funktion ```hs preorder :: Tree a -> [a]```, die
    Knotenwerte in pre-order zurückgibt. Das heißt, zuerst wird ein Knoten
    betrachtet und anschließend dessen linker und danach dessen rechter Teilbaum.
  - Implementiere einen unendlichen Baum ```hs tree :: Tree Int```, die Menge
    ${ f(i, j) | i, j in NN, i <= j }$ mit $f(i, j) = i + j + 2 i j$ darstellt.
    Die Wurzel soll den Wert $f(1,1)$ haben. Für einen beliebigen Knoten
    mit Beschriftung $f(i,j)$  soll die Wurzel des linken Teilbaums mit
    $f(i+1,j)$ beschriftet sein und die Wurzel des rechten Teilbaums mit
    $f(i,j+1)$. Falls $i > j$ erreicht wird, soll in den Baum ein
    ```hs Empty```-Knoten gesetzt werden.
  - Wende ```hs preorder``` auf ```hs tree``` an. Welches Problem haben wir
    hinsichtlich der Werte, die wir den jeweiligen Teilbäumen von ```hs tree```
    sehen und der Ergbenisliste von ```hs preorder```? Wie hängt deine
    Beobachtung mit ```hs [f i j | i <- [1..], j <- [i..]]``` zusammen?
  - Implementiere eine zweite Variante von ```hs preorder```, die statt
    ```hs (++)``` die Funktion ```hs merge``` verwendet.
  - Implementiere als Nächstes die Mengendifferenz als Funktion
    ```hs diff :: Ord a => [a] -> [a] -> [a]```. Du darfst dabei annehmen, dass
    die Eingabelisten bereits sortiert sind.
  - Was berechnet ```hs 2 : map (\x -> 2 * x + 1) ([1..] `diff` preorder tree)```?
    #footnote[
      Das Verfahren ist als #link("https://en.wikipedia.org/wiki/Sieve_of_Sundaram")[Sieb von Sundaram]
      bekannt. Die Konstruktion der oben angegebenen Menge mithilfe von
      unendlichen Bäumen ist nicht Teil des Verfahrens -- sondern Willkür des
      Verfassers.
    ]
]

// ```hs
// data Tree a = Node (Tree a) a (Tree a) | Empty
//
// tree :: Tree Int
// tree = go 1 1
//   where go i j = Node (if i < j then go (i + 1) j else Empty)
//                       (i + j + 2 * i * j)
//                       (go i (j + 1))
//
// merge :: Ord a => [a] -> [a] -> [a]
// merge xs     []                 = xs
// merge []     ys                 = []
// merge (x:xs) (y:ys) | x <= y    = x : merge xs (y:ys)
//                     | otherwise = y : merge (x:xs) (y:ys)
//
// preorder :: Tree a -> [a]
// preorder Empty        = []
// preorder (Node l x r) = x : merge (preorder l) (preorder r)
//
// diff :: Ord a => [a] -> [a] -> [a]
// diff []     _                  = []
// diff xs     []                 = xs
// diff (x:xs) (y:ys) | x == y    = diff xs (y:ys)
//                    | x > y     = diff (x:xs) ys
//                    | otherwise = x : diff xs (y:ys)
//
// primes :: [Int]
// primes = 2 : map (\x -> 2 * x + 1) ([1..] `diff` preorder tree)
// ```

#test[
  Gegeben sei der Datentyp
  #align(center)[```hs data Doubly a = Null | Node (Doubly a) a (Doubly a)```.]
  - Implementiere eine Funktion ```hs fromList :: [a] -> Doubly a```, die
    die gegebene Liste in eine doppelt-verkettete Liste umwandelt. ```hs Null```
    soll sowohl das linke als auch das rechte Ende der Liste darstellen. Von
    diesem muss es nicht möglich sein, zum anderseitig verketteten Element
    zurückzukommen.
    ```hs fromList``` soll den Knoten zurückgeben, der mit dem ersten
    Listenelement korrespondiert.
  - Weiter implementiere auch ```hs prev :: Doubly a -> Doubly a```,
    ```hs value :: Doubly a -> Maybe a``` und
    ```hs next :: Doubly a -> Doubly a```, die den vorherigen Knoten, die
    Beschriftung eines Knoten, und den nächsten Knoten zurückgeben sollen.
  - Angenommen du möchtest einen weiteren Wert in die doppelt-verkettete Liste
    einfügen, auf welches Problem stoßt du hinsichtlich Mutierbarkeit?

  Der Wert des folgenden Ausdrucks soll ```hs 8``` sein.
  ```hs
  let xs = fromList [1, 6, 1, 8, 0, 3]
   in value . prev . next . next . next . next $ xs
  ```
]

#challenge[
  Wir können endliche Automaten als unendliche Bäume darstellen.
  Betrachte z.B. den endlichen Automaten für die reguläre Sprache $a^* b^*$.
  #align(center)[
    #finite.automaton(
      (
        q0: (q0: "a", q1: "b"),
        q1: (q1: "b", q2: "a"),
        q2: (q2: "a, b")
      ),
      initial: "q0",
      final: ("q0", "q1"),
      style: (
        state: (initial: (label: none)),
        q0: (label: $q_0$),
        q1: (label: $q_1$),
        q2: (label: $q_2$)
      ),
    )
  ]
  Diesen können wir als unendlichen Baum wie folgt darstellen.
  #align(center)[
    #finite.automaton(
      (
        q0: (q1: "a", q2: "b"),

        q1: (q3: "a", q4: "b"),
        q2: (q5: "a", q6: "b"),

        q3: (q7: "a", q8: "b"),
        q4: (q9: "a", q10: "b"),
        q5: (q11: "a", q12: "b"),
        q6: (q13: "a", q14: "b"),

        q7: (),
        q8: (),
        q9: (),
        q10: (),
        q11: (),
        q12: (),
        q13: (),
        q14: ()
      ),
      initial: "q0",
      final: ("q2", "q4", "q6"),
      layout: finite.layout.custom.with(
        positions: (
          q0: (5.5, 6),

          q1: (2.5, 4),
          q2: (8.5, 4),

          q3: (1, 2),
          q4: (4, 2),
          q5: (7, 2),
          q6: (10, 2),

          q7: (0, 0),
          q8: (2, 0),
          q9: (3, 0),
          q10: (5, 0),
          q11: (6, 0),
          q12: (8, 0),
          q13: (9, 0),
          q14: (11, 0),
        )
      ),
      style: (
        transition: (
          curve: 0,
          angle: 0deg
        ),
        state: (
          initial: (label: none),
        ),

        q0: (label: $q_0$),
        q1: (label: $q_0$),
        q2: (label: $q_1$),
        q3: (label: $q_0$),
        q4: (label: $q_1$),
        q5: (label: $q_2$),
        q6: (label: $q_1$),

        q7: (label: none, stroke: none),
        q8: (label: none, stroke: none),
        q9: (label: none, stroke: none),
        q10: (label: none, stroke: none),
        q11: (label: none, stroke: none),
        q12: (label: none, stroke: none),
        q13: (label: none, stroke: none),
        q14: (label: none, stroke: none),

        q3-q7: (stroke: (dash: "dashed")),
        q3-q8: (stroke: (dash: "dashed")),
        q4-q9: (stroke: (dash: "dashed")),
        q4-q10: (stroke: (dash: "dashed")),
        q5-q11: (stroke: (dash: "dashed")),
        q5-q12: (stroke: (dash: "dashed")),
        q6-q13: (stroke: (dash: "dashed")),
        q6-q14: (stroke: (dash: "dashed"))
      )
    )
  ]
  - Konstruiere diesen Baum als ```hs asbs :: State Char``` mithilfe des Typs
    #align(center)[```hs data State a = State Bool [(a, State a)]```.] Der Boolean
    gibt an, ob der Zustand akzeptiert, und ```hs [(a, State a)]``` gibt die
    ausgehenden Transitionen an.
  - Implementiere eine Funktion ```hs accept :: Eq a => [a] -> State a -> Bool```,
    die bestimmt, ob eine Eingabe akzeptiert wird.
  - Implementiere eine Funktion ```hs language :: State a -> [[a]]```, die die
    akzeptierte Sprache des Automaten zurückgibt. (Du kannst davon ausgehen,
    dass die Sprache nicht leer ist -- wenn du Entscheidungsproblem trotzdem
    lösen möchtest, halten wir dich nicht auf.)
  - Warum funktioniert die folgende Implementierung der Funktion
    ```hs language``` nicht?
    ```hs
    language :: State a -> [[a]]
    language (State False ts) = [c:ws | (c, q) <- ts, ws <- language q]
    language (State True  ts) = [] : [c:ws | (c, q) <- ts, ws <- language q]
    ```
]

// ```hs
// data State a = State Bool [(a, State a)]
//
// ambn :: State Char
// ambn = q0
//   where
//     q0 = State False [('a', q0), ('b', q1)]
//     q1 = State True  [('a', q2), ('b', q1)]
//     q2 = State False [('a', q2), ('b', q2)]
//
// accept :: Eq a => [a] -> State a -> Bool
// accept []     (State True _) = True
// accept (x:xs) (State _ ts)   = any (accept xs) [u | (y, u) <- ts, x == y]
// accept _      _              = False
//
// language :: State a -> [[a]]
// language q = bfs [([], q)]
//   where
//     bfs []                       = []
//     bfs ((w, State final ts):qs) =
//         let ws = bfs (qs ++ [(w ++ [c], q) | (c, q) <- ts])
//          in if final then w:ws else ws
// ```

#test[
  Wieso können wir mit ```hs foldl``` auf unendlichen Listen mit keinem
  Ergebnis rechnen?
]

#test[
  ```hs scanl :: (b -> a -> b) -> b -> [a] -> [b]``` und
  ```hs scanr :: (a -> b -> b) -> b -> [a] -> [b]``` sind ähnlich zu
  ```hs foldl``` und ```hs foldr```. Beide Funktionen speichern die
  Zwischenergebnisse der jeweiligen Funktion in Listen.
  ```hs
  scanl :: (b -> a -> b) -> b -> [a] -> [b]
  scanl _ e []     = [e]
  scanl f e (x:xs) = e : scanl f (f e x) xs

  -- scanl (+) 0 [1..4] = [0, 1, 3, 6, 10]

  scanr :: (a -> b -> b) -> b -> [a] -> [b]
  scanr _ e []     = [e]
  scanr f e (x:xs) = f x y : ys
    where ys@(y:_) = scanr f e xs

  -- scanr (+) (0) [1..4] = [10, 9, 7, 4, 0]
  ```
  - Welche der beiden Funktionen kann auf unendlichen Listen arbeiten?
  - (Implementiere die Fibonacci-Folge ```hs fibs :: [Integer]``` mithilfe
    von einer der beiden Funktionen.)
]

#challenge[
  Fixpunktverfahren sind iterative Methoden, bei denen eine Funktion wiederholt
  auf einen Wert angewendet wird, bis sich ein stabiler Punkt ergibt, der sich
  durch weitere Anwendungen nicht mehr verändert.

  Dieses Berechnungsmuster wird durch die Funktion
  ```hs iterate :: (a -> a) -> a -> [a]``` in der Prelude festgehalten.
  Sie berechnet eine unendliche Liste, bestehend aus den Ergebnissen der
  wiederholten Anwendungen der übergegebenen Funktion auf den gegebenen Wert.
  Das erste Ergebnis ist der gegebene Wert, auf den die Funktion noch nicht
  angewendet wurde.

  - Implementiere ```hs iterate```.
  - Ein klassisches Beispiel für ein Fixpunktverfahren aus der Numerik ist die
    Berechnung der Wurzel mithilfe des Heron-Verfahrens. Es ist gegeben durch
    $ x_(n + 1) = 1/2 (x_n + a/x_n) $ Diese Folge nährt den Wert von $sqrt(a)$
    mit jedem Folgeglied besser an. Implementiere das Verfahren mithilfe von
    ```hs iterate```. Wähle die erste Nährung $x_(n+1)$, die
    $abs(x_(n+1) - x_n) < epsilon$ erfüllt, für ein gegebenes $epsilon > 0$.
  - Implementiere die Fibonacci-Folge als unendliche Liste
    ```hs fibs :: [Integer]``` mithilfe von ```hs iterate```. Du brauchst
    eine Hilfsfunktion, die die Elemente der Ergebnisliste von ```hs iterate```
    projeziert.
  - Solange eine Liste Inversionen enthält, d.h., es existieren $i, j$ mit
    $i < j$, sodass $a_i > a_j$ gilt, gilt eine Liste als unsortiert. Das
    schrittweise Entfernen solcher Fehlstellungen führt zu einer sortierten
    Liste.
    - Implementiere eine Funktion ```hs resolve :: Ord a => [a] -> [a]```, die
      eine Fehlstellung findet und sie auflöst, indem sie die Elemente an den
      entsprechenden Positionen tauscht.
    - Implementiere das daraus resultierende Sortierverfahren mithilfe von
      ```hs iterate```.
  - Mithilfe des Differenzenquotienten kannst du die erste Ableitung
    approximieren (oder mit der Lösung von @symbolic_diff oder
    @reverse_mode_ad). Diese benötigt man unter anderem für das
    #link("https://de.wikipedia.org/wiki/Gradientenverfahren")[Gradientenverfahren].
    Eine vereinfachte Iterationsvorschrift des Verfahren ist gegeben durch
    $ x_(k + 1) = x_k - 10^(-4) f'(x_k) quad "für alle" k in NN $
    und einem Startpunkt $x_0 in RR$. Implementiere das Verfahren mithilfe von
    ```hs iterate```. Wähle als Ergebnis die erste Nährung $x_(n+1)$, die
    $abs(x_(n+1) - x_n) < epsilon$ für ein festes $epsilon > 0$ erfüllt (z. B.
    $epsilon = 10^(-5)$).
]

// ```hs
// import Data.List (find)
// import Data.Maybe (fromJust)
//
// heron :: Double -> Double
// heron a = prec xs
//   where
//     xs = iterate (\x -> (x + a / x) / 2) a
//
//     prec (x:y:xs) | abs (x - y) < 1e-6 = y
//                   | otherwise          = prec (y:xs)
//
// sort :: Ord a => [a] -> [a]
// sort xs = fromJust (find sorted (iterate resolve xs))
//   where
//     resolve (x:y:xs) | x > y     = y : x : xs
//                      | otherwise = x : resolve (y:xs)
//     resolve xs                   = xs
//
//     sorted []                   = True
//     sorted [x]                  = True
//     sorted (x:y:xs) | x < y     = sorted (y:xs)
//                     | otherwise = False
//
// -- mit automatischen Differenzieren
// minimize :: (Fractional a, Ord a) => (D a -> D a) -> a -> a
// minimize f x0 = prec xs
//   where
//     xs = iterate (\x -> x - 1e-4 * derive f x) x0
//
//     prec (x:y:xs) | abs (x - y) < 1e-6 = y
//                   | otherwise          = prec (y:ys)
// ```

#challenge[
  Die Editierdistanz zwischen zwei Wörtern $u in Sigma^m, v in Sigma^n$ können
  wir mithilfe der folgenden Rekurrenz bestimmen:
  $
  "ed"(i, j) = cases(
    0 & quad "falls" (i, j) = (0, 0),
    i & quad "falls" j = 0,
    j & quad "falls" i = 0,,
    "ed"(i - 1, j - 1) & quad "falls" u_(i - 1) = v_(j - 1),
    min("ed"(i - 1, j - 1), "ed"(i, j - 1), "ed"(i - 1, j)) + 1 & quad "sonst"
  )
  $
  für alle $0 <= i <= m, 0 <= j <= n$.
  Um $"ed"(m,n)$ effizient auszurechen, nutzt man dynamische Programmierung.
  Das heißt, wir merken uns die Zwischenergebnisse und nutzen diese, wenn wir
  sie erneut brauchen, anstatt sie neu zu berechnen. Dieses Konzept ist auch
  als memoization bekannt.

  In Haskell können wir memoization mithilfe von Lazy Evaluation umsetzen.
  Hier ist ein unvollständiges Haskell-Programm, dass die Editierdistanz
  berechnen soll.
  ```hs
  editdist :: Eq a => [a] -> [a] -> Int
  editdist u v = table !! m !! n
    where
      (m, n) = (length u, length v)
      table = [[ed i j | j <- [0..n]] | i <- [0..m]]
  ```
  - Definiere die Funktion ```hs ed :: Int -> Int -> Int``` lokal in
    ```hs editdist```. ```hs ed``` soll hier die zwischengespeicherten
    Ergebnisse aus ```hs table``` verwenden.
  - Überlege dir wie hier laziness und memoization zusammenspielen.
  - Welche worst-case Laufzeit hat dein Problem, wenn du annimst, dass die
    Laufzeit ```hs (!!)``` konstant ist?
] <editdist>

// ```hs
// editdist :: String -> String -> Int
// editdist u v = table !! m !! n
//   where
//     (m, n) = (length u, length v)
//     table = [[ed i j | j <- [0..n]] | i <- [0..m]]
//
//     ed 0 j = j
//     ed i 0 = i
//     ed i j
//       | u !! (i - 1) == v !! (j - 1) = table !! (i - 1) !! (j - 1)
//       | otherwise = 1 + minimum [ table !! (i - 1) !! j
//                                 , table !! i !! (j - 1)
//                                 , table !! (i - 1) !! (j - 1)
//                                 ]
// ```

#challenge[
  Bevor du dich dieser Challenge stellst, bietet es sich an, sich @editdist
  anzunehmen.

  Gegeben sei ein Gitter $G in ZZ^(m times n)$. Ein Pfad durch das
  Gitter startet oben links und endet unten rechts. In jedem Schritt
  kannst du von einer Zelle in die rechtsanschließende oder darunteranschließende
  Zelle gehen. Die Pfadsumme ist die Summe aller Zellenwerte, durch die der Pfad
  führt.

  Hier ist ein Beispiel für ein solches Gitter. Der Pfad der minimalen Pfadsumme
  ist durch die Pfeile angedeutet. Für dieses Beispiel ist die minimale Pfadsumme
  $6$.
  #align(center)[
    #cetz.canvas({
      import cetz.draw: *

      grid((0, 0), (4, 3))

      let g = (
        ( 1, 2, 3, 4),
        (-8, 4, 6, 1),
        ( 5, 2, 3, 4),
      )

      for i in range(g.len()) {
        for j in range(g.at(i).len()) {
          content(
            (j, i),
            (j + 1, i + 1),
            box(
              width: 100%,
              height: 100%,
              align(center + horizon, str(g.at(g.len() - i - 1).at(j)))
            )
          )
        }
      }

      set-style(mark: (end: ")>"))
      rect((0.35, 1.8), (0.65, 2.2), fill: white, stroke: none)
      line((0.5, 2.2), (0.5, 1.8))

      rect((0.8, 1.65), (1.2, 1.35), fill: white, stroke: none)
      line((0.8, 1.5), (1.2, 1.5))

      rect((1.35, 0.8), (1.65, 1.2), fill: white, stroke: none)
      line((1.5, 1.2), (1.5, 0.8))

      rect((1.8, 0.65), (2.2, 0.35), fill: white, stroke: none)
      line((1.8, 0.5), (2.2, 0.5))

      rect((2.8, 0.65), (3.2, 0.35), fill: white, stroke: none)
      line((2.8, 0.5), (3.2, 0.5))
    })
  ]

  Implementiere eine Funktion ```hs pathsum :: (Num a, Ord a) => [[a]] -> a```,
  die die minimale Pfadsumme berechnet.

  #hint[
    Die Rekursionsvorschrift ist gegeben durch
    $
    "pathsum"(i, j) = cases(
      G_(0,0) & quad "falls" (i, j) = (0, 0),
      G_(i,0) + "pathsum"(i-1,0) & quad "falls" j = 0,
      G_(0,j) + "pathsum"(0,j-1) & quad "falls" i = 0,
      G_(i,j) + min("pathsum"(i-1,j), "pathsum"(i,j-1)) & quad "sonst"
    )
    $
    für $i in {0, ..., m - 1}, j in {0, ..., n - 1}$. Die minimale Pfadsumme
    ist dann $"pathsum"(m - 1, n - 1)$.
  ]
]

// ```hs
// pathSum :: (Num a, Ord a) => [[a]] -> a
// pathSum g = dp !! (m - 1) !! (n - 1)
//   where
//     m = length g
//     n = length (g !! 0)
//
//     dp = [[go i j | j <- [0..n - 1]] | i <- [0..m - 1]]
//
//     go 0 0 = g !! 0 !! 0
//     go i 0 = g !! i !! 0 + dp !! (i - 1) !! 0
//     go 0 j = g !! 0 !! j + dp !! 0 !! (j - 1)
//     go i j = g !! i !! j + min (dp !! (i - 1) !! j) (dp !! i !! (j - 1))
// ```


// Sequenzen

#test[
  Gegeben sei der Datentyp
  #align(center)[```hs data Direction = North | East | South | West```.]

  - Implementiere eine ```hs Enum```-Instanz für ```hs Direction```.
  - Implementiere eine ```hs Bounded```-Instanz für ```hs Direction```.
  - Implementiere die Funktionen ```hs turnLeft :: Direction -> Direction```
    und ```hs turnRight :: Direction -> Direction```, die die Himmelsrichtungen
    entsprechend ihrer Bezeichnung durchgehen.
  - Implementiere eine Funktion ```hs allDirections :: [Direction]```, die
    alle Himmelsrichtungen auflistet. Nutze dafür Funktionen, die dir durch
    die vorherigen Typklassen bereitgestellt werden.
]

#test[
  Implementiere Funktion ```hs cycleFrom :: (Enum a, Bounded a) => a -> [a]```,
  die ab einem gegebenen Wert alle Werte des Typen nicht absteigend durchläuft.
  Wenn der größte Wert erreicht ist, soll die Liste wieder beim kleinsten Wert
  des Typen beginnen.
]


// List Comprehensions

#test[
  An welches mathematische Konzept sind list comprehensions angelehnt?
]

#test[
  Aus welchen Teilen besteht eine list comprehension?
]

#test[
  Implementiere die Funktionen ```hs map```, ```hs filter``` und
  ```hs concatMap``` mithilfe von list comprehensions.
]


// Ein- und Ausgabe

#test[
  Was ist referenzielle Transparenz?
]

#test[
  Welche Rolle spielt der Typ ```hs IO a``` bzgl. Seiteneffekte?
  Was beschreibt ein Wert vom Typ ```hs IO a```?
]

#test[
  Wie können wir zwei ```hs IO```-Aktionen zu einer neuen ```hs IO```-Aktion
  kombinieren?
]

#test[
  Betrachte die ```hs IO```-Aktion ```hs act1 >> act2```.
  - Welche der beiden Aktionen wird zuerst ausgeführt?
  - Warum erscheint das bei Lazy Evaluation kontraintuitiv?
  - Welche Rolle spielt der Typ ```hs RealWorld -> (RealWorld, a)``` bei der Sequenzierung?
]

#test[
  Implementiere ein Programm, das Zahlen aus einer Datei aufsummiert, bzw.
  implementiere eine Funktion ```hs sumFile :: FilePath -> IO Int```.
  In jeder Zeile einer Datei steht eine nicht-negative Zahl.
]

#test[
  Implementiere folgendes Rate-Spiel als IO-Programm. Es soll eine Zahl erraten
  werden.
  - Du bekommst ein Orakel vom Typ ```hs a -> Ordering```, das
    dir verrät, ob dein Rateversuch kleiner als, gleich oder größer als der Wert
    ist, den das Orakel festgelegt hat.
  - In jeder Runde des Spiels ratest du eine Zahl.
  - Wenn die Zahl kleiner als die unbekannte Zahl ist, dann soll der Hinweis
    "Die gesuchte Zahl ist kleiner." ausgegeben werden. Wenn die unbekannte Zahl
    größer ist, soll ebenso eine entsprechende Nachricht ausgegeben werden.
  - Wenn die korrekte Zahl erraten wurde, bricht das Spiel ab.
  Implementiere das Spiel als Funktion ```hs game :: Read a => (a -> Ordering) -> IO ()```.

  Du kannst das Spiel gerne ausschmücken und erweitern. Zum Beispiel kannst du
  die Anzahl der Rateversuche begrenzen oder eine weitere Zahl festlegen, die
  vorzeitig das Spiel beendet und das Orakel gewinnen lässt.
]


// Funktoren, Applicatives, Monaden

#test[
  Wie lauten die ```hs Functor```-Gesetze?
]

#test[
  Die Funktor-Typkonstruktorklasse ist wie folgt definiert.
  ```hs
  class Functor f where
    fmap :: (a -> b) -> f a -> f b
  ```
  Welchen kind hat ```hs f```?
]

#test[
  Wie sind die ```hs Functor```-Instanzen für ```hs Identity```, ```hs Maybe```,
  ```hs Either e```, ```hs []``` und ```hs ((->) r)``` definiert?
]

#test[
  Wie lauten die ```hs Applicative```-Gesetze?
]

#test[
  Wie sind die ```hs Applicative```-Instanzen für ```hs Identity```, ```hs Maybe```,
  ```hs Either e```, ```hs []``` und ```hs ((->) r)``` definiert?
]

#test[
  Wie lauten die ```hs Monad```-Gesetze?
]

#test[
  Wie sind die ```hs Monad```-Instanzen für ```hs Identity```, ```hs Maybe```,
  ```hs Either e```, ```hs []``` und ```hs ((->) r)``` definiert?
]

#test[
  Die ```hs Applicative```-Typkonstruktorklasse erlaubt es uns, ```hs fmap```
  auf Funktionen mit mehreren Argumenten zu verallgemeinern. Dadurch können wir
  etwa
  #align(center)[
    ```hs (+) <$> Just 1 <*> Just 2``` #h(1em) oder #h(1em) ```hs Just (+) <*> Just 1 <*> Just 2```
  ]
  schreiben. Die Operatoren ```hs (<$>)``` und ```hs (<*>)``` funktionieren
  dabei ähnlich wie ```hs ($)``` -- mit ```hs (<$>)``` muss die Funktion nicht
  explizit in den entsprechenden ```hs Applicative``` gehoben werden.

  Ein Typ, der uns konzeptionell noch näher an die gewöhnliche
  Funktionsapplikation heranführt, ist
  #align(center)[```hs newtype Identity a = Identity { runIdentity :: a }```.]
  Implementiere ```hs Functor```-, ```hs Applicative```- und
  ```hs Monad```-Instanzen für ```hs Identity```.

  Wenn du die Instanzen definierst, solltest du feststellen, dass du im
  Wesentlichen nur den enthaltenden Wert aus der ```hs Identity``` holst,
  verarbeitest und anschließend wieder hereinpackst.
]

#test[
  Warum gilt
  #align(center)[```hs (1 +) <$> Just 1 == Just (1 +) <*> Just 1 == pure (1 +) <*> Just 1```?]
  Wie ergibt sich aus deinen Beobachtungen eine Definition für ```hs fmap```?
  Wie können wir die Berechnung für beliebige applikative Funktoren
  verallgemeinern? Wie wird dann festgelegt, was während der Berechnung
  tatsächlich passiert?
]

#test[
  Monaden sind ausdrucksstärker als applikative Funktoren, und applikative
  Funktoren sind ausdrucksstärker als Funktoren.
  - Implementiere ```hs fmap```, ```hs pure``` und ```hs (<*>)``` mithilfe von
    ```hs return``` und ```hs (>>=)```.
  - Implementiere ```hs fmap``` mithilfe von ```hs pure``` und ```hs (<*>)```.
]

#test[
  Betrachten wir die Typen von ```hs fmap``` und ```hs (>>=)```, dann sehen
  wir gewisse Ähnlichkeiten.
  #align(center)[
    ```hs fmap :: (a -> b) -> f a -> f b``` \
    ```hs (>>=) :: m a -> (a -> m b) -> m b```
  ]
  Wie können wir bereits an den Typen sehen, dass ```hs (>>=)``` die mächtigere
  Funktionen der beiden ist? Beziehe in deine Überlegungen ein, dass
  #align(center)[
    ```hs fmap f (Right x) = Left y```
  ]
  nie gelten kann. Kann ```hs fmap```, die Struktur in Abhängigkeit
  des Wertes vom Typ ```hs a``` verändern?
]

#test[
  Monaden bieten uns die Möglichkeit, Berechnungen als Folge von kleineren
  Berechnungen zu sequenzieren. Je nachdem welche Monade wir betrachten,
  beobachten wir verschiedene Effekte.
  - Welche Monade drückt eine Berechnung aus, die kein oder genau ein Ergebnis
    liefern kann?
  - Welche Monade drückt eine Berechnung aus, die kein Ergebnis oder beliebig
    viele Ergebnisse liefern kann?
  - Welche Monade drückt eine Berechnung aus, die entweder fehlschlägt oder
    erfolgreich ist? Welche kann im Fall eines Fehlschlag zusätzliche
    Information hervorbringen?
]

#test[
  Wir nutzen Monaden zur Sequenzierung von Berechnungen. Identifiziere diese
  Sequenzierung in den folgenden ```hs Monad```-Instanzen bzw. stelle fest, dass
  ```hs m``` in ```hs m >>= k``` zuerst berechnet wird, bevor die Berechnung mit
  ```hs k``` fortgesetzt wird.
  ```hs
  instance Monad Maybe where
    return x = Just x

    Nothing >>= _ = Nothing
    Just x  >>= k = k x

  instance Monad [] where
    return x = [x]

    []     >>= _ = []
    (x:xs) >>= k = k x ++ (xs >>= k)
  ```
]

#test[
  Argumentiere anhand der Gesetze, die für eine ```hs Functor```-Instanzen
  gelten sollen, dass die folgenden ```hs Functor```-Instanzen keine gültigen
  Instanzen sind. Gebe auch Beispiele an, die zeigen, dass die Gesetze nicht
  erfüllt sind.
  ```hs
  instance Functor [] where
    fmap f [] = []
    fmap f (x:xs) = f x : f x : xs

  data Tree a = Empty | Leaf a | Tree a :+: Tree a

  instance Functor Tree where
    fmap _ Empty     = Empty
    fmap f (Leaf _)  = Empty
    fmap f (l :+: r) = fmap f l :+: fmap f r
  ```
]

#challenge[
  Gegeben sei der Datentyp
  #align(center)[
    ```hs newtype ZipList a = ZipList { getZipList :: [a] }```.
    #footnote[
      Das Umwickeln eines Typen mit ```hs newtype```, für den wir bereits
      Typklasseninstanzen haben, ist ein gängiger Trick, um alternative Instanzen
      für diese Typklassen bereitzustellen.
    ]
  ]
  Das Ziel ist es, ```hs ZipList``` als $n$-stellige Generalisierung von
  ```hs zipWith``` zu verwenden:
  #align(center, block(width: 100%, ```hs f <$> ZipList xs1 <*> ... <*> ZipList xsN```))

  - Implementiere eine ```hs Functor```-Instanz für ```hs ZipList```.
  - Bevor du eine ```hs Applicative```-Instanz für ```hs ZipList```
    implementierst, überlege warum
    ```hs
    pure :: a -> ZipList a
    pure x = ZipList [x]
    ```
    keine gültige Definition ist? Welche Gesetze wären verletzt, würdest
    ```hs pure``` so definieren?
  - Implementiere eine ```hs Applicative```-Instanz für ```hs ZipList```.
  - Zeige, dass sowohl die ```hs Functor```- als auch die
    ```hs Applicative```-Instanz die üblichen geforderten Gesetze erfüllen.

  #hint[
    Das Identitätsgesetz wäre verletzt, wenn man ```hs pure``` wie gegeben
    definieren würde. Betrachte folgendes Gegenbeispiel.
    #align(center)[
      ```hs
         pure id      <*> ZipList [1, 2]
      == ZipList [id] <*> ZipList [1, 2]
      == ZipList [1]
      /= ZipList [1, 2]
      ```
    ]
    Es muss also dafür gesorgt sein, dass es genügend ```hs id```s in der linken
    ```hs ZipList``` gibt.
  ]
]

// TODO annotate steps properly and polish some steps
// ```hs
// instance Functor ZipList where
//   fmap f = ZipList . fmap f . getZipList
//
// -- Identity
// --
// --   fmap id (ZipList xs)
// -- = ZipList (fmap id (getZipList (ZipList xs)))
// -- = ZipList (fmap id xs)
// -- = ZipList xs
// -- = id (ZipList xs)
//
// -- Composition
// --
// --   fmap (f . g) (ZipList xs)
// -- = ZipList (fmap (f . g) (getZipList (ZipList xs)))
// -- = ZipList (fmap (f . g) xs)
// -- = ZipList ((fmap f . fmap g) xs)
// -- = ZipList (fmap f (fmap g xs))
// -- = ZipList (fmap f (getZipList (ZipList (fmap g xs))))
// -- = fmap f (ZipList (fmap g xs))
// -- = fmap f (ZipList (fmap g (getZipList (ZipList xs))))
// -- = fmap f (fmap g (ZipList xs))
// -- = (fmap f . fmap g) (ZipList xs)
//
// instance Applicative ZipList where
//   pure = ZipList . repeat
//
//   zfs <*> zxs = ZipList (zipWith ($) (getZipList zfs) (getZipList zxs))
//
//
// -- Identity
// --
// --   pure id <*> ZipList xs
// -- = ZipList (repeat id) <*> ZipList xs
// -- = ZipList (zipWith ($) (repeat id) xs)
// -- = ZipList (($) id x1 : ($) id x2 : ... : ($) id xn : [])
// -- = ZipList xs
//
// -- Composition
// --
// --   pure (.) <*> ZipList fs <*> ZipList gs <*> ZipList xs
// -- = ZipList (repeat (.)) <*> ZipList fs <*> ZipList gs <*> ZipList xs
// -- = ZipList (zipWith ($) (repeat (.)) fs) <*> ZipList gs <*> ZipList xs
// -- = ZipList (map (.) fs) <*> ZipList gs <*> ZipList xs
// -- = ZipList (zipWith ($) (map (.) fs) gs) <*> ZipList xs
// -- = ZipList (map (map (.) fs) gs) <*> ZipList xs
// -- = ZipList (zipWith (.) fs gs) <*> ZipList xs
// -- = ZipList ((f1 . g1) x1 : (f2 . g2) x2 : ... : (fn . gn) xn : [])
// -- = ZipList (f1 (g1 x1) : f2 (g2 x2) : ... : fn (gn xn))
// -- = ZipList (zipWith ($) fs (g1 x1 : g2 x2 : ... : gm xm))
// -- = ZipList (zipWith ($) fs (zipWith ($) gs xs))
// -- = ZipList fs <*> (ZipList (zipWith ($) gs xs))
// -- = ZipList fs <*> (ZipList gs <*> ZipList xs)
//
//
// -- Homomorphism
// --
// --   pure f <*> pure x
// -- = ZipList (repeat f) <*> ZipList (repeat x)
// -- = ZipList (zipWith ($) (repeat f) (repeat x))
// -- = ZipList (repeat (f x))
// -- = pure (f x)
//
//
// -- Interchange
// --
// --   ZipList fs <*> pure y
// -- = ZipList fs <*> ZipList (repeat y)
// -- = ZipList (zipWith ($) fs (repeat y))
// -- = ZipList (f1 y : f2 y : ... : fn y : [])
// -- = ZipList (($ y) f1 : ($ y) f2 : ... : ($ y) fn : [])
// -- = ZipList (zipWith ($) (repeat ($ y)) fs)
// -- = ZipList (repeat ($ y)) <*> ZipList fs
// -- = pure ($ y) <*> ZipList fs
// ```

#test[
  ```hs guard :: MonadZero m => Bool -> m ()``` kann genutzt werden, um eine
  Berechnung bedingt fehlschlagen zu lassen.
  #footnote[
    Für Interessierte: ```hs guard``` ist auf Basis von ```hs Alternative``` bzw.
    ```hs MonadPlus``` implementiert. ```hs MonadZero``` ist nicht Teil der
    Standardbibliothek, aber es ist definiert als Teil von ```hs MonadPlus```.
  ]
  Zum Beispiel können wir mithilfe von ```hs guard``` eine sichere Division
  definieren.
  ```hs
  safeDiv :: (Integral a, MonadZero m) => a -> a -> m a
  safeDiv a b = guard (b /= 0) >> return (a `div` b)
  ```

  - Implementiere die Funktion ```hs guard :: Bool -> Maybe ()``` mit
    ```hs Maybe ()``` als konkreten Ergebnistypen.
  - Die Typklasse ```hs MonadZero``` wird gängigerweise wie folgt definiert.
    ```hs
    class Monad m => MonadZero m where
      mzero :: m a
    ```
    Implementiere ```hs guard``` nun allgemein.
  - Berechne ```hs 1 `safeDiv` 0 :: m Int``` für ```hs Maybe``` und ```hs []```.
    Bevor das möglich ist, benötigst du entsprechende ```hs MonadZero```-Instanzen.
] <monadzero>

// ```hs
// class Monad m => MonadZero m where
//   mzero :: m a
//
// instance MonadZero Maybe where
//   mzero = Nothing
//
// instance MonadZero [] where
//   mzero = []
//
// guard :: MonadZero m => Bool -> m ()
// guard False = mzero
// guard True  = return ()
// ```

#test[
  Als motivierendes Beispiel für Monaden hast du die Auswertung eines
  arithmetischen Ausdrucks gegeben als Termstruktur kennengelernt. Dort haben
  wir die ```hs Maybe```-Monade verwendet, um fehlschlagende Berechnung
  aufzufangen. Der Typ für die arithmetischen Ausdrücke ist gegeben durch:
  ```hs
  data Exp a = Num a
             | Exp a :+: Exp a
             | Exp a :-: Exp a
             | Exp a :*: Exp a
             | Exp a :/: Exp a
  ```
  - Implementiere ```hs eval :: Exp Int -> Maybe Int``` mit dem
    ```hs Maybe```-Applicative. Warum ist die Division, ohne eine weitere
    Hilfsfunktion nicht möglich? Was müsste diese Hilfsfunktion tun, damit die
    Regel für die Division funktioniert?
    #footnote[
      Der applikativen Funktor wird dadurch nicht ausdrucksstärker! Das, was
      der applikative Funktor nicht leisten kann, wird in die Hilfsfunktion
      ausgelagert.
    ]
  - Implementiere ```hs eval :: Exp Int -> Maybe Int``` mit der
    ```hs Maybe```-Monade. Benötigst du hier die Hilfsfunktion aus der
    vorherigen Teilaufgabe? Warum ja oder nein?
  - Wie kannst du mit @monadzero ```hs eval``` zu einer Funktion
    ```hs eval :: MonadZero m => Exp Int -> m Int```
    verallgemeinern?
  - ```hs MonadZero``` erlaubt es uns, einen Fehlschlag allgemein auszudrücken.
    Allerdings können wir anhand des Fehlschlags alleine nicht feststellen,
    warum es zum Fehlschag kam. Verallgemeinere die Typkonstruktorklasse
    ```hs MonadZero``` zu einer Typkonstruktorklasse ```hs MonadFail```,
    die es erlaubt, eine Beschreibung des Fehlschags anzugeben. Sie soll
    also eine Funktion ```hs fail :: String -> m a``` definieren. Verallgemeinere
    ```hs eval``` erneut mit der neuen Typkonstruktorklasse. Gebe zusätzlich
    eine Instanz für den Typ ```hs Either String``` an -- alternativ,
    implementiere eine ```hs Functor```-, ```hs Applicative```-, ```hs Monad```-
    und ```hs MonadFail```-Instanz für den Typen
    ```hs data Result a = Failure String | Success a```.
]

// ```hs
// import Prelude hiding (MonadFail(..))
//
//
// data Exp a = Num a
//            | Exp a :+: Exp a
//            | Exp a :-: Exp a
//            | Exp a :*: Exp a
//            | Exp a :/: Exp a
//   deriving Show
//
//
// -- mit Maybe-Applicative und Hilfsfunktion
// eval :: Integral a => Exp a -> Maybe a
// eval (Num x)   = Just x
// eval (a :+: b) = (+) <$> eval a <*> eval b
// eval (a :-: b) = (*) <$> eval a <*> eval b
// eval (a :*: b) = (-) <$> eval a <*> eval b
// eval (a :/: b) = div <$> eval a <*> fail (eval b)
//   where fail (Just 0) = Nothing
//         fail m        = m
//
//
// -- Hilfsfunktion für das Anwenden der Operatoren auf monadische Werte
// liftM2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
// liftM2 f ma mb = ma >>= \a -> mb >>= \b -> return (f a b)
//
// -- mit Maybe-Monade
// eval :: Integral a => Exp a -> Maybe a
// eval (Num x)   = Just x
// eval (a :+: b) = liftM2 (+) (eval a) (eval b)
// eval (a :-: b) = liftM2 (-) (eval a) (eval b)
// eval (a :*: b) = liftM2 (*) (eval a) (eval b)
// eval (a :/: b) =
//   do
//     y <- eval b
//     if y == 0
//       then Nothing  -- mit MonadZero hier mzero hinschreiben
//       else do
//              x <- eval a
//              Just (x `div` y)
//
//
// -- mit MonadZero und guard
// eval :: (Integral a, MonadZero m) => Exp a -> m a
// eval (Num x)   = return x
// eval (a :+: b) = liftM2 (+) (eval a) (eval b)
// eval (a :-: b) = liftM2 (-) (eval a) (eval b)
// eval (a :*: b) = liftM2 (*) (eval a) (eval b)
// eval (a :/: b) =
//   do
//     y <- eval b
//     guard (y /= 0)
//     x <- eval a
//     return (x `div` y)
//
//
// class Monad m => MonadFail m where
//   fail :: String -> m a
//
// instance MonadFail (Either String) where
//   fail = Left
//
// -- mit MonadFail
// eval :: (Integral a, MonadFail m) => Exp a -> m a
// eval (Num x)   = return x
// eval (a :+: b) = liftM2 (+) (eval a) (eval b)
// eval (a :-: b) = liftM2 (-) (eval a) (eval b)
// eval (a :*: b) = liftM2 (*) (eval a) (eval b)
// eval (a :/: b) =
//   do
//     y <- eval b
//     if y == 0
//       then fail "division by zero"
//       else do
//              x <- eval a
//              return (x `div` y)
// ```

#test[
  Wie hängen die Listenmonade und list comprehensions zusammen?
  - Schreibe den folgenden Ausdruck
    ```hs
    do
      x <- [1..10]
      y <- [1..10]
      guard (x + y == 10)
      return (x, y)
    ```
    mithilfe von list comprehensions. Der Wert des ausgerechneten Ausdrucks ist
    #align(center)[
      ```hs
      [(0, 10), (1, 9), (2, 8), (3, 7), (4, 6), (5, 5),
       (6, 4), (7, 3), (8, 2), (9, 1), (10, 0)]
      ```
    ]
    Anhand des Ergebnisses kannst du dir die Semantik von ```hs guard```
    herleiten (oder schaust dir vorher @monadzero an).
  - Schreibe den folgenden Ausdruck
    ```hs
    [f | n <- [0..], let f = fib n, f `mod` 2 == 0]
    ```
    mithilfe der Listenmonade.
]

#test[
  Wie übersetzen wir
  #align(center)[```hs do x <- e1; e2``` #h(1em) und #h(1em) ```hs do e1; e2```]
  in einen äquivalente Ausdrucke mithilfe von ```hs (>>=)``` und ```hs (>>)```?

  Wie übersetzen wir diesen etwas größeren Ausdruck?
  ```hs
  do
    x <- getInt
    y <- getInt
    print (x + y)
    return (x + y)
  ```
]

#test[
  Wie übersetzen wir die Audrücke
  #align(center)[
    ```hs
    eval e1 >>= \x -> eval e2
            >>= \y -> if y == 0
                        then Nothing
                        else return (x + y)
    ```
  ]
  und
  #align(center)[
    ```hs
    getLine >> read <$> getLine
            >>= \x -> case f x of
                        Nothing -> return 0
                        Just _  -> return x
    ```
  ]
  in einen äquivalenten Ausdruck mithilfe der ```hs do```-Notation?
]

#test[
  Gegeben sei der Datentyp ```hs Tree a = Leaf a | Tree a :+: Tree a```.
  - Implementiere eine Funktion ```hs splits :: [a] -> [([a], [a])]```, die
    alle nicht-leeren Aufteilungen der Eingabeliste berechnet.

    Zum Beispiel soll ```hs splits [1..4]``` die Liste
    #align(center)[```hs [([1], [2, 3, 4]), ([1, 2], [3, 4]), ([1, 2, 3], [4])]```]
    ergeben.
  - Implementiere eine Funktion ```hs allTrees :: [a] -> [Tree a]```, die alle
    Binärbäume generiert, deren Blätter von links nach rechts die Eingabeliste
    lesen. Versuche, ```hs allTrees``` mithilfe von list comprehensions oder der
    Listenmonade zu implementieren.
]

// ```hs
// allTrees []  = []
// allTrees [x] = [Leaf x]
// allTrees xs  = [l :+: r | (ls, rs) <- splits xs
//                         , l <- allTrees ls
//                         , r <- allTrees rs
//                         ]
//
// splits :: [a] -> [([a], [a])]
// splits xs = init (tail (zip (inits xs) (tails xs)))  -- import Data.List (inits, tails)
//
// splits :: [a] -> [([a], [a])]
// splits []     = []
// splits [_]    = []
// splits [x,y]  = [([x], [y])]
// splits (x:xs) = ([x], xs) : ((\(ys, zs) -> (x:ys, zs)) <$> splits xs)
// ```

#test[
  Gegeben ist der Typ
  ```hs data Deep a b = Deep [Maybe (Either a (Deep a b))]```.
  Die Faltungsfunktion sieht im Wesentlichen so aus.
  ```hs
  foldDeep :: ([Maybe (Either a r)] -> r) -> Deep a b -> r
  foldDeep fdeep (Deep x) = fdeep (f x)
    where fold = foldDeep fdeep
  ```
  Wie können wir
  ```hs f :: [Maybe (Either a (Deep a b))] -> [Maybe (Either a r)]```
  definieren?
]

// ```hs
// foldDeep :: ([Maybe (Either a r)] -> r) -> Deep a b -> r
// foldDeep fdeep (Deep x) = fdeep (fmap . fmap . fmap fold)
//   where fold = foldDeep fdeep
// ```

#test[
  Oft kommt es vor, dass Berechnungen zustandabhängig verschiedene Ergebnisse
  liefern. Zum Beispiel merken wir uns in einer Tiefensuche durch einen
  Graph, welche Knoten bereits besucht wurden, damit die Tiefensuche sich nicht
  in einem Kreis verläuft. Allgemein können wir solche Berechnungen als
  Funktionen vom Typ ```hs s -> (a, s)``` auffassen. Sie bekommen ein Zustand
  vom Typ ```hs s``` und liefern ein Ergebnis vom Typ ```hs a``` und einen
  (möglicherweise) neuen Zustand (ebenso vom Typ ```hs s```).

  Implementiere eine Funktion ```hs sequence :: [s -> (a, s)] -> s -> ([a], s)```,
  die Liste zustandabhängiger Berechnungen nimmt und der Reihe nach ausführt.
  Es wird dabei ein erster Zustand übergegeben, der die erste Berechnung
  anstößt. Das Ergebnis soll der letzte Zustand mit allen Ergebnissen der
  Berechnungen sein.
  #footnote[
    Für Interessierte: Diese Implementierung von ```hs sequence``` ist ein
    Spezialfall für die
    #link("https://learnyouahaskell.github.io/for-a-few-monads-more.html#state")[```hs State```-Monade].
    Mit der Intuition, dass wir hier Berechnungen sequenzieren, sollte es nicht
    überraschend sein, dass ```hs s -> (a, s)``` eine Monade ist. Alternativ
    kannst du die ```hs State```-Monade implementieren und das vorimplementierte
    (oder von dir implementierte) ```hs sequence``` verwenden.
  ]

  Betrachte folgendes Beispiel:
  ```hs
  --     Zustand        Ergebnis  neuer Zustand
  fib :: (Int, Int) -> (Int     , (Int, Int)   )
  fib (f0, f1) = (f0, (f1, f0 + f1))

  fibs :: Int -> [Int]
  fibs n = fst (sequence (replicate n fib) (0, 1))
  ```
  Hier wird der erste Zustand mit den ersten beiden Fibonacci-Zahlen
  initialisiert, also $(0, 1)$. In jedem Schritt wird der Zustand mit der
  nächsten Fibonacci-Zahl aktualisiert und die kleinere Fibonacci-Zahl des
  Zustands zurückgegeben. Zuletzt projezieren wir mit ```hs fst``` das
  Ergebnis von ```hs sequence``` und verwerfen so den letzten Zustand.

  Hier ist der Datenfluss von ```hs sequence``` nochmal visualisiert:
  #align(center)[
    #cetz.canvas({
      import cetz.draw: *

      // s_0 in
      set-style(mark: (end: ")>"))
      line((-1.0, 0.35), (0.0, 0.35), name: "state0")
      content("state0.mid", padding: 0.25em, anchor: "north", [$s_0$])

      // f_1
      rect((0, 0), (1.0, 1.25))
      content((0.5, 0.625), [$f_1$])

      // y_1 out
      set-style(mark: (end: "o"))
      line((1.0, 0.85), (2.0, 0.85), name: "output1")
      content("output1.mid", padding: 0.25em, anchor: "south", [$y_1$])

      // s_1 out
      set-style(mark: (end: ")>"))
      line((1.0, 0.35), (3.0, 0.35), name: "state1")
      content("state1.mid", padding: 0.25em, anchor: "north", [$s_1$])

      // f_2
      rect((3.0, 0), (4.0, 1.25))
      content((3.5, 0.625), [$f_2$])

      // y_2 out
      set-style(mark: (end: "o"))
      line((4.0, 0.85), (5.0, 0.85), name: "output2")
      content("output2.mid", padding: 0.25em, anchor: "south", [$y_2$])

      // s_2 out with fade out
      set-style(mark: (end: ")>"))
      line((5.0, 0.35), (6.0, 0.35), name: "state2", stroke: (dash: "dotted"))
      content("state2.start", padding: 0.25em, anchor: "north", [$s_2$])
      set-style(mark: (end: none))
      line((4.0, 0.35), (5.0, 0.35))

      // s_(n-1) in with fade in
      line((7.0, 0.35), (8.0, 0.35), stroke: (dash: "dotted"))
      set-style(mark: (end: ")>"))
      line((8.0, 0.35), (9.0, 0.35), name: "stateN-1")
      content("stateN-1.start", padding: 0.25em, anchor: "north", [$s_(n-1)$])

      // f_n
      rect((9.0, 0), (10.0, 1.25))
      content((9.5, 0.625), [$f_n$])

      // y_n out
      set-style(mark: (end: "o"))
      line((10.0, 0.85), (11.0, 0.85), name: "outputN")
      content("outputN.mid", padding: 0.25em, anchor: "south", [$y_n$])

      // s_n out
      set-style(mark: (end: ")>"))
      line((10.0, 0.35), (11.0, 0.35), name: "stateN")
      content("stateN.end", padding: 0.25em, anchor: "north", [$s_n$])
    })
  ]
  Es soll also
  ```hs sequence [f1, f2, ..., fn] s0 = ([y1, y2, ..., yn], sn)``` gelten.
] <sequence_state>

#challenge[
  Im Folgenden modellieren wir einen Graph als Paar $(V, E)$ mit
  einer Knoten- und Kantenbewertung $w_v : V -> A$ und $w_e : E -> B$,
  und $E subset.eq V times V$.

  Gegeben sei der Datentyp
  #align(center)[```hs data Graph a b = Graph [(Int, a)] [(Int, b, Int)]```.]
  Hier entspricht der erste Parameter des Datenkonstruktors $w_v$ und der
  zweite $w_e$.

  - Implementiere eine Funktion ```hs succs :: Int -> Graph a b -> [Int]```,
    die zu einem gegebenen Knoten die direkten Nachfolger berechnet.
  - Implementiere eine Funktion
    ```hs reachable :: Int -> Int -> Graph a b -> Bool```, die entscheidet, ob
    zwischen zwei Knoten ein gerichteter Pfad existiert. Starte zuerst mit
    einfachen Graphen und betrachte immer komplexer werdende Graphen. Wie musst
    du (oder kannst du) deine Implementierung anpassen?
    - Nehme zuerst an, dass ein gegebener Graph immer ein Baum ist? Wenn also
      ein Pfad existiert, dann ist er eindeutig.
    - Nehme jetzt an, dass ein gegebener Graph immer azyklisch ist. Das heißt,
      falls ein Pfad existiert, muss dieser nicht mehr eindeutig sein.
      Nutze deine Erkenntnisse aus @sequence_state, um eine Liste aller
      besuchten Knoten zu verwalten.
    - Was musst du in deiner Implementierung anpassen, wenn der gegebene Graph
      auch zyklisch sein kann?
  - Je nachdem wie du ```hs reachable``` implementiert hast, könnte es denn
    Anschein erwecken, dass deine Lösung alle Knoten besucht. Wieso ist das
    nicht unbedingt der Fall? Wieso passiert das nur, wenn wir uns die Liste
    aller besuchten Knoten anschauen?
    Welche Beobachtungen machst du, wenn du ```hs reachable 0 k (yGraph k)```
    berechnest?
    ```hs
    yGraph :: Int -> Graph () ()
    yGraph k = Graph [(v, ()) | v <- [0..2 * k]]
                     (let left = [(v, (), v + 1) | v <- [1..k]]
                          right = [(v, (), v + 1) | v <- [k + 1..2 * k]]
                       in (0, (), 1) : (0, (), k + 1) : (left ++ right))
    ```
    Hier ist der Graph visualiziert.
    #align(center)[
      #cetz.canvas({
        import cetz.draw: *

        circle((0, 0), radius: 0.6, name: "v0")
        content((0, 0), [$v_0$])

        circle((2, 1), radius: 0.6, name: "v1")
        content((2, 1), [$v_1$])

        set-style(mark: (end: ")>"))
        line("v0", "v1")

        circle((4, 1), radius: 0.6, name: "v2")
        content((4, 1), [$v_2$])

        line("v1", "v2")

        circle((6, 1), radius: 0.6, stroke: none, name: "vh1")

        set-style(mark: (end: none))
        line("v2", "vh1")

        line((5.8, 1), (6.2, 1), stroke: (dash: "dotted"))

        circle((8, 1), radius: 0.6, name: "vk-1")
        content((8, 1), [$v_(k-1)$])

        set-style(mark: (end: ")>"))
        line("vh1", "vk-1")

        circle((10, 1), radius: 0.6, name: "vk")
        content((10, 1), [$v_k$])

        line("vk-1", "vk")

        circle((2, -1), radius: 0.6, name: "vk+1")
        content((2, -1), [$v_(k+1)$])

        line("v0", "vk+1")

        circle((4, -1), radius: 0.6, name: "vk+2")
        content((4, -1), [$v_(k+2)$])

        line("vk+1", "vk+2")

        circle((6, -1), radius: 0.6, stroke: none, name: "vh2")

        set-style(mark: (end: none))
        line("vk+2", "vh2")

        line((5.8, -1), (6.2, -1), stroke: (dash: "dotted"))

        circle((8, -1), radius: 0.6, name: "v2k-1")
        content((8, -1), [$v_(2k-1)$])

        set-style(mark: (end: ")>"))
        line("vh2", "v2k-1")

        circle((10, -1), radius: 0.6, name: "v2k")
        content((10, -1), [$v_(2k)$])

        line("v2k-1", "v2k")
      })
    ]

  #hint[
    Falls du es nicht geschafft hast, in @sequence_state ```hs sequence```
    zu implementieren, kannst du diese Implementierung verwenden.

    ```hs
    sequence :: [s -> (a, s)] -> s -> ([a], s)
    sequence []     s = ([], s)
    sequence (f:fs) s = let (y, s')   = f s
                            (ys, s'') = sequence fs s'
                         in (y:ys, s'')
    ```
  ]
]

// ```hs
// import Prelude hiding (sequence)
//
// data Graph a b = Graph [(Int ,a)] [(Int, b, Int)]
//   deriving Show
//
// succs :: Int -> Graph a b -> [Int]
// succs v (Graph _ es) = [w | (u, _, w) <- es, u == v]
//
// -- für Bäume gut, für DAGs langsam
// -- reachable :: Int -> Int -> Graph a b -> Bool
// -- reachable v w g = or [reachableTree u w g | u <- succs v g]
//
// -- vorimplementiert für Monaden
// sequence :: [s -> (a, s)] -> s -> ([a], s)
// sequence []     s = ([], s)
// sequence (f:fs) s = let (y, s')   = f s
//                         (ys, s'') = sequence fs s'
//                      in (y:ys, s'')
//
// reachable :: Int -> Int -> Graph a b -> Bool
// reachable v w g = fst (go v [])
//   where
//     go u us | u == w      = (True, us)
//             | u `elem` us = (False, us)
//             | otherwise   = let (rs, us') = sequence (go <$> succs u g) (u:us)
//                              in (or rs, us')
// ```

#test[
  Es kommt häufiger vor, dass zufällig generierte Werte bestimmte Eigenschaften
  erfüllen sollen. QuickCheck bietet auf Generatoren-Ebene z.B. die Funktion
  ```hs suchThat :: Gen a -> (a -> Bool) -> Gen a``` an. Diese nimmt einen
  Generator und generiert solange neue Werte, bis eine gewünschte Eigenschaft
  erfüllt ist. Diese Eigenschaft wird durch ein Prädikat formuliert.

  Implementiere ```hs suchThat```.

  #text(0.8em)[
    Es gibt ein paar QuickCheck-spezifische Fallstricke, die mit dem
    ```hs size```-Parameter der Generatoren zu tun haben, die du bei deiner
    Implementierung nicht beachten brauchst. Falls es dich interessiert, ist
    hier die
    #link("https://hackage-content.haskell.org/package/QuickCheck/docs/src/Test.QuickCheck.Gen.html#suchThat")[```hs suchThat```-Implementierung von QuickCheck].
  ]
]

// ```hs
// suchThat :: Gen a -> (a -> Bool) -> Gen a
// suchThat g p = g >>= \x -> if p x
//                              then return x
//                              else g `suchThat` p
// ```

#test[
  Wenn du folgende (etwas künstliche) Eigenschaft mithilfe von QuickCheck
  prüfst, muss im Durchschnitt jeder zweite Test verworfen werden, weil die
  Vorbedingung der Eigenschaft nicht erfüllt ist.
  ```hs
  prop_prop :: Int -> Property
  prop_prop k = k > 0 ==> True
  ```
  Um dieses Verhalten zu unterbinden, können wir die Eigenschaft leicht
  abändern.
  #footnote[
    QuickCheck hat viele solche
    #link("https://hackage-content.haskell.org/package/QuickCheck/docs/Test-QuickCheck.html")[type-level modifiers],
    die das Generatoren-Verhalten verändern.
  ]
  ```hs
  prop_prop :: Positive Int -> Property
  prop_prop (Positive k) = k > 0 ==> True
  ```
  Jetzt werden keine Tests verworfen.

  Überlege dir, wie die Lösung des Problems funktioniert und implementiere sie.
]

// ```hs
// newtype Positive a = Positive { getPositive :: a }
//
// instance (Arbitrary a, Num a) => Arbitrary (Positive a) where
//   arbitrary = Positive . (+ 1) . abs <$> arbitrary
//   -- or
//   -- arbitrary = fmap Positive (fmap abs arbitrary `suchThat` (> 0))
// ```

#challenge[
  Eigenschaften lassen sich mit QuickCheck z.B. mithilfe der Funktion
  ```hs quickCheck``` prüfen.

  Hier sind ein paar Eigenschaften.
  ```hs
  prop_comm :: Int -> Int -> Bool
  prop_comm x y = x + y == y + x

  prop_inv :: Int -> Bool
  prop_inv x = x + (-x) = 0
  ```

  - Welchen Typ muss ```hs quickCheck``` scheinbar haben, damit wir
    ```hs prop_comm``` prüfen können? Wie steht das im Konflikt mit
    ```hs prop_inv```?
  - Um ```hs quickCheck``` zu implementieren, benötigen wir die Möglichkeit,
    beliebig stellige Funktionen anwenden zu können. Das ist mithilfe von
    Typklassen möglich. Hier ist eine vereinfachte Variante der Typklasse
    ```hs Testable```, wie sie in QuickCheck zu finden ist.
    ```hs
    newtype Property = Property { unProperty :: Bool }

    class Testable a where
      property :: a -> Property
    ```
    - Implementiere ```hs Testable```-Instanzen für ```hs Bool``` und
      ```hs Property```, und anschließend eine Instanz für den Typ
      ```hs a -> b```, wobei ```hs a``` eine ```hs Arbitrary```-Instanz haben
      soll und ```hs b``` wieder ```hs Testable``` sein soll.
    - Welche Instanzen übernehmen die Rolle eines Basisfalls und welche
      Instanzen übernehmen die Rolle einer induktiven Regel?
    - Wie ist es möglich, dass wir Eigenschaften definieren können, die
      entweder ein ```hs Bool``` oder eine ```hs Property``` als Rückgabewert
      haben?
    - Als Nächstes betrachten wir eine Vereinfachung der
      ```hs Arbitrary```-Typklasse und eine Instanz für den Typ ```hs Int```,
      damit wir ```hs quickCheck``` implementieren und verwenden können.
      ```hs
      class Arbitrary a where
        arbitrary :: a

      instance Arbitrary Int where
        arbitrary = 42  -- Hier werden normalerweise zufällige Werte generiert.
      ```
      Implementiere eine Funktion ```hs quickCheck :: Testable a => a -> Bool```,
      die eine Eigenschaft nimmt und prüft.

  #text(0.8em)[
    In QuickCheck werden Generatoren genutzt, um zufällige Werte zu generieren.
    Diese nutzen alle einen Zufallszahlengenerator, der als Zustand mit
    weiteren Parametern durch alle ```hs arbitrary```-Aufrufe durchgetragen wird.
    Das haben wir uns durch die Vereinfachungen vernachlässigt.
  ]
]

// ```hs
// class Arbitrary a where
//   arbitrary :: a
//
// instance Arbitrary a where
//   arbitrary = 42
//
// instance Testable Bool where
//   property b = Property b
//
// instance Testable Property where
//   property = id
//
// instance (Arbitrary a, Testable b) => Testable (a -> b) where
//   property f = property (f arbitrary)
//
// quickCheck :: Testable a => a -> IO Bool
// quickCheck p = unProperty (property p)
// ```

#test[
  Implementiere Arrays als nicht-mutierende Datenstruktur in Python. Die
  Klasse soll so funktionieren, dass der folgende Beispielcode lauffähig ist
  und die angegebene Ausgabe erzeugt.
  ```py
  a = Array.fromList(5, [(2, 4), (3, 3)])

  b = a.update([(1, 3), (2, 2)])
  for i in range(len(a)):
    print(a[i], b[i])
  ```
  Die Ausgabe des Programms soll
  ```
  None None
  None 3
  4 2
  3 3
  None None
  ```
  sein.
]

// ```py
// class Array:
//   def __init__(self, size, data=None):
//     self.size = size
//     self.data = None if data is None else data
//
//   def fromList(size, pairs):
//     data = [None] * size
//     for index, value in pairs:
//       data[index] = value
//     return Array(size, data)
//
//   def __getitem__(self, index):
//     if not 0 <= index < self.size:
//       raise IndexError("index out of bounds")
//     return self.data[index]
//
//   def __len__(self):
//     return len(self.data)
//
//   def update(self, values):
//     data = self.data.copy()
//     for index, value in values:
//       data[index] = value
//     return Array(self.size, data)
// ```

#pagebreak(weak: true)


= Logische Programmierung

#test[
  Was ist Berechnungsprinzip von Prolog?
]

#test[
  Formuliere folgende Aussage als Prolog-Programm:
  Seien $A$ und $B$. Dann gilt $C$, wenn $A$ und $B$ gelten.
]

#test[
  Wie können wir Funktionen in Prolog umsetzen? Erkläre es mithilfe eines
  Beispiels (wie z.B. ```hs (&&) :: Bool -> Bool -> Bool```).
]

#test[
  Wie hängen die Syntax von Prolog und die der Aussagenlogik zusammen? Welche
  Symbole entsprechen sich?
]

#test[
  In Prolog können wir ebenso Termstrukturen erzeugen. Der Haskell-Typ
  ```hs data Maybe a = Nothing | Just a``` kann z.B. wie folgt in Prolog
  umgesetzt werden.
  ```SWI-Prolog
  maybe(nothing).
  maybe(just(_)).
  ```
  Übersetze mit der gleichen Idee den Datentyp
  ```hs data Tree a = Empty | Node (Tree a) a (Tree a)```.
]

#test[
  Ordne die Begriffe Atom, Fakt, Regel und Variable dem folgenden
  Prolog-Programm zu?
  ```SWI-Prolog
  semitone(c, cis). semitone(cis, d). semitone(d, dis). semitone(dis, e).
  semitone(e, f). semitone(f, fis). semitone(fis, g). semitone(g, gis).
  semitone(gis, a). semitone(a, ais). semitone(ais, b). semitone(b, c).

  minor_third(P1, Mi3) :-
    semitone(P1, Mi2), semitone(Mi2, Ma2), semitone(Ma2, Mi3).

  major_third(P1, Ma3) :- minor_third(P1, Mi3), semitone(Mi3, Ma3).

  major_chord(P1, Ma3, P5) :- major_third(P1, Ma3), minor_third(Ma3, P5).
  ```
] <major_chord>

#test[
  In @major_chord sind die Halbtonschritte als Fakten definiert. Um diese
  etwas kompakter zu schreiben, können wir ```SWI-Prolog append/3``` verwenden.
  Implementiere ```SWI-Prolog semitone/2``` mithilfe von ```SWI-Prolog append/3```.
]

// ```SWI-Prolog
// semitone(X, Y) :-
//   append(_, [X, Y|_], [c, cis, d, dis, e, f, fis, g, gis, a, ais, b, c]).
// ```

#test[
  Implementiere das Prädikat ```SWI-Prolog zip/3```, dass zwei Liste bekommt
  und eine Liste von Paaren zuliefert -- so wie du es aus Haskell kennst.
  Es soll
  #align(center)[```SWI-Prolog zip([1, 2], [3, 4, 5], [(1, 3), (2, 4)]).```]
  gelten.
  Wie gewinnst du aus deiner Implementierung das Prädikat ```SWI-Prolog unzip/3```,
  also die Umkehrfunktion ```SWI-Prolog zip/3```, wenn diese auf Listen gleicher
  Länge eingeschränkt ist.
]

#test[
  Mithilfe von ```SWI-Prolog append/3``` lassen sehr viele andere Prädikate
  auf Listen definieren. Überlege dir, wie du folgende Prädikate unter dessen
  Nutzung implementieren kannst. Die Prädikate sollen sich wie deren
  entsprechenden Haskell-Funktionen verhalten, wenn dir der Name des Prädikats
  bekannt vorkommt.
  - ```SWI-Prolog head/2``` und ```SWI-Prolog tail/2```,
  - ```SWI-Prolog last/2```,
  - ```SWI-Prolog member/2``` (entspricht ```hs elem``` in Haskell)
  - ```SWI-Prolog dups/2``` soll alle Elemente in einer Liste finden, die
    genau zweimal vorkommen. Du darfst ```SWI-Prolog not_member/2``` als
    Hilfsfunktion verwenden.

  Warum ist ```SWI-Prolog member/2``` hier zweistellig?
]

// ```SWI-Prolog
// head(X, Xs) :- append([X], _, Xs).
// tail(Xs, Ys) :- append([_], Ys, Xs).
// last(Xs, X) :- append(_, [X], Xs).
// member(X, Xs) :- append(_, [X|_], Xs).
// dups(X, Xs) :-
//   append(Ys1, [X|Ys2], Xs),
//   not_member(X, Ys1),
//   append(Ys3, [X|Ys4], Ys2).
//   not_member(X, Ys3),
//   not_member(X, Ys4).
// ```

#test[
  Warum ist ```SWI-Prolog not_member(X, Xs) :- append(_, [Y|_], Xs), X \= Y.```
  keine korrekte Implementierung des Prädikates, das testet, ob ein Element
  nicht einer Liste enthalten ist?
]

#test[
  Ein Graph sei dargestellt als eine Liste von Kanten. Die Kanten seien
  wiederum als Tupel dargestellt.

  Implementiere ein Prädikat ```SWI-Prolog reachable/3```, das bestimmt,
  ob ein Knoten von einem anderen Knoten aus erreichbar ist. Du benötigst
  voraussichtlich ein weiteres vierstelliges Hilfsprädikat.

  Dein Programm soll sich wie folgt verhalten:
  ```SWI-Prolog
  ?- reachable(1, 3, [(1, 2), (2, 3), (3, 1)]).
  true ;
  false.

  ?- reachable(1, 3, [(1, 2), (3, 2)]).
  false.
  ```
]

// ```SWI-Prolog
// not_member(_, []).
// not_member(X, [Y|Xs]) :- X \= Y, not_member(X, Xs).
//
// reachable(X, Y, G) :- reachable(X, Y, G, []).
// reachable(X, X, _, _).
// reachable(X, Y, G, V) :-
//   not_member(X, V),
//   member((X, Z), G),
//   reachable(Z, Y, G, [X|V]).
// ```

#test[
  Wie unterscheiden sich die Gleichheit ```SWI-Prolog ==/2``` in Prolog
  und die Gleichheit in Haskell?
]

#test[
  Welche Konzepte, die Haskell verwendet, stecken hinter den Anfragen
  - ```SWI-Prolog ?- just((X, Y)) = just((1, 2))``` und
  - ```SWI-Prolog ?- to(X, to(X, list(X))) = to(int, to(int, list(int)))```?
]

#pagebreak(weak: true)


= Hinweise zu Tests und Challenges

#context get-metadata("hint").join(v(1em, weak: true)) \

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
- #link("https://www.adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html")[Functors, Applicatives, And Monads In Pictures]
- #link("https://hackage.haskell.org/package/CheatSheet-1.7/src/CheatSheet.pdf")[Haskell Cheatsheet]
- #link("https://alhassy.com/PrologCheatSheet/CheatSheet.pdf")[Prolog Cheatsheet]

#pagebreak(weak: true)


= Appendix

#remark[
  In @reverse_mode_ad wurden Funktionen ```hs d1```, ```hs d2``` und ```hs d3```
  definiert. Diese ver- und entschachteln ```hs D```-Werte unterschiedlich tief.
  Mit verschiedenen Haskell-Spracherweiterungen können wir eine allgemeinere
  Funktion angeben. Mit ```hs derive @3 cos (2.0 :: Double)``` können wir jetzt
  z.B. die dritte Ableitung des Cosinus an der Stelle $2$ berechnen.
  ```hs
  {-# LANGUAGE TypeFamilies #-}
  {-# LANGUAGE DataKinds #-}
  {-# LANGUAGE UndecidableInstances #-}
  {-# LANGUAGE AllowAmbiguousTypes #-}
  {-# LANGUAGE FunctionalDependencies #-}

  import Data.Kind
  import GHC.TypeLits

  data D a = D a a

  -- Hier könnten deine Num-, Fractional- und Floating-Typklasseninstanzen
  -- stehen!

  class Derivable (n :: Nat) b where
    type Derivative n b
    derive :: (Derivative n b -> Derivative n b) -> b -> b

  instance (Wrap (NatToPeano n) b, Unwrap (DK (NatToPeano n) b) b) => Derivable n b where
    type Derivative n b = DK (NatToPeano n) b
    derive f x = unwrap (f (wrap @(NatToPeano n) x))

  data Peano = Z | S Peano

  type family NatToPeano (n :: Nat) where
    NatToPeano 0 = Z
    NatToPeano k = S (NatToPeano (k - 1))

  type family DK p a where
    DK Z     a = a
    DK (S k) a = D (DK k a)

  class Wrap n t where
    wrap :: t -> DK n t

  instance Wrap Z t where
    wrap x = x

  instance (Wrap n t, Num (DK n t)) => Wrap (S n) t where
    wrap x = D (wrap @n x) 1

  class Unwrap a b | a -> b where
    unwrap :: a -> b

  instance (Num a) => Unwrap a a where
    unwrap x = x

  instance (Unwrap a b, Num a) => Unwrap (D a) b where
    unwrap (D _ d) = unwrap d
  ```
] <reverse_mode_ad_remark>

#remark[
  In @typeclasses_in_python haben wir gesehen, wie deklarative Programmierkonzepte
  aus Haskell in Python verwendet werden können. In Java haben diese über die
  letzten Jahre auch einen Platz gefunden. Hier ist das Programm in Java (ohne
  gecurryte Funktionen und partielle Applikation).
  ```java
  import java.util.*;
  import java.util.function.*;

  interface Numeric<T> {
    T zero();
    T add(T a, T b);

    public static Numeric<Integer> integer() {
      return new Numeric<>() {
        public Integer zero() { return 0; }
        public Integer add(Integer a, Integer b) { return a + b; }
      };
    }
  }

  interface Foldable<T> {
    <R> R foldr(R initial, BiFunction<T, R, R> function);

    default List<T> toList() {
      return foldr(new LinkedList<>(), (value, list) -> {
        list.add(value);
        return list;
      });
    }

    default int length() { return foldr(0, (_, result) -> result + 1); }

    default boolean contains(T value) {
      return foldr(false, (other, result) -> result || other.equals(value));
    }

    default T sum(Numeric<T> numeric) {
      return foldr(numeric.zero(), numeric::add);
    }
  }

  sealed interface Tree<T> extends Foldable<T>
    permits Empty, Node {}

  record Empty<T>() implements Tree<T> {
    @Override
    public <R> R foldr(R initial, BiFunction<T, R, R> function) {
      return initial;
    }
  }

  record Node<T>(Tree<T> left, T value, Tree<T> right) implements Tree<T> {
    @Override
    public <R> R foldr(R initial, BiFunction<T, R, R> function) {
      R x = right.foldr(initial, function);
      R y = function.apply(value, x);
      return left.foldr(y, function);
    }
  }

  public class Main {
    public static void main(String[] args) {
      Tree<Integer> tree = new Node<>(
        new Empty<>(),
        3,
        new Node<>(new Node<>(new Empty<>(), 7, new Empty<>()), 4, new Empty<>())
      );

      System.out.println(tree.sum(Numeric.integer()));  // 14
      System.out.println(tree.toList());  // [4, 7, 3]
      System.out.println(tree.length());  // 3
      System.out.println(tree.contains(3));  // true
      System.out.println(tree.contains(9));  // false
    }
  }
  ```
] <typeclasses_in_python_remark>

