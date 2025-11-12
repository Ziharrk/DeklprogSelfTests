#import "@preview/ctheorems:1.1.3": *

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

#set text(lang: "de")
#show link: underline
#show heading.where(level: 1): set block(below: 1.25em)
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
    Haskell und Prolog zu prüfen.
  ]
}

Größere Aufgaben haben wir als Challenges markiert. Diese Aufgaben benötigen
öfter mehrere Konzepte und führen zusätzlich Konzepte ein, die nur für das
Lösen der Aufgabe wichtig sind.

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
    Zur Darstellung der Multimengen eignen sich sortierte Listen gut.
  ]

  #hint[
    Zur Berechnung des Schnittes können zwei sortierte Listen parallel durchlaufen
    werden. Wenn zwei gleiche Elemente zu Beginn der Liste stehen, wird eines
    der Elemente zum Ergebnis hinzugefügt. Im anderen Fall überspringen wir
    das jeweils kleinere Element der beiden.
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
  Gegeben sei folgendes Python-Programm.
  ```py
  from dataclasses import dataclass

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


  class Tree(Foldable):
    def foldr(self, f):
      def foldr_with_f(e):
        match self:
          case Empty():
            return e
          case Node(l, x, r):
            x = f(self.value)(self.right.foldr(f)(e))
            y = self.left.foldr(f)(x)
            return y
      return foldr_with_f

  @dataclass
  class Empty(Tree):
    pass

  @dataclass
  class Node(Tree):
    left: Tree
    value: any
    right: Tree


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
  - parametrischen Polymorphismus und
  - ad-hoc Polymorphismus (Typklassen bzw. Überladung).

  #text(0.8em)[
    Data classes und match statements brauchst du dir jenseits dieses Tests
    nicht anschauen (wenn es dich nicht weiter interessiert). Es soll in dem
    Test nur darum gehen, die Konzepte zu erkennen.
  ]
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
]

#stopHere

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
  Wieso wird die Funktion durch ```hs Show a => a -> String``` gerettet?
]

#test[
  Welche Typklassen kennst du? Was ermöglichen sie konkret in den Einzelfällen?
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

An vielen Stellen in den bisherigen Selbsttests haben wir oft einen konkreten
Typen (z.B. ```hs Int```) genutzt, für den es bestimmte Typklasseninstanzen
gibt. Das ist meistens der Fall gewesen, wenn wir Gleichheit auf Werten oder
eine Vergleichsoperation auf Werten brauchten. Schau dir die bisherigen
Selbsttests erneut an und überlege dir, wo du Typen verallgemeinern kannst.

#test[
  Welche Funktionen musst du implementieren, damit eine ```hs Eq```-Instanz
  vollständig definiert ist?
]

#test[
  Welche Funktionen musst du implementieren, damit eine ```hs Ord```-Instanz
  vollständig definiert ist?
]

// TODO Händische `compare` Implementierung für irgendwie einen Typen bzw.
//      Verständnisfrage dazu

#test[
  In nicht streng getypten Programmiersprachen haben wir oft, mit impliziter
  Typkonversion zu tun. #footnote[Diese wollen nun für einen Moment nach Haskell
  zurückholen, um sie dann ganz schnell wieder zu vergessen.] Implementiere eine
  Funktion ```hs ifThenElse```, die als Bedingung Werte beliebiger Typen
  entgegennehmen kann. Ziel ist es, dass das folgende Ausdruck ausgewertet
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
  Wieso können wir mit ```hs foldl``` auf unendlichen Listen mit keinem
  Ergebnis rechnen?
]


// Funktoren, Applicatives, Monaden

#test[
  Gegeben sei der Datentyp ```hs Tree a = Leaf a | Tree a :+: Tree a```.
  Implementiere eine Funktion ```hs allTrees :: [a] -> [Tree a]```, die alle
  Binärbäume generiert, deren Blätter von links nach rechts die Eingabeliste
  lesen. Um ```hs allTrees``` zu implementieren, implementiere zuerst eine
  Hilfsfunktion ```hs splits :: [a] -> [([a], [a])]```, die alle nicht-leeren
  Aufteilungen der Eingabelisten berechnet.
  Zum Beispiel soll ```hs splits [1..4]``` die Liste
  ```hs [([1], [2, 3, 4]), ([1, 2], [3, 4]), ([1, 2, 3], [4])]``` ergeben.
  Versuche, ```hs allTrees``` mithilfe von list comprehensions oder der
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
  ```
  Wie können wir
  ```hs f :: [Maybe (Either a (Deep a b))] -> [Maybe (Either a r)]```
  mithilfe von ```hs foldDeep fdeep :: Deep a b -> r``` definieren?
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
// - #link("https://www.adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html")[Functors, Applicatives, And Monads In Pictures]
- #link("https://hackage.haskell.org/package/CheatSheet-1.7/src/CheatSheet.pdf")[Haskell Cheatsheet]
// - #link("https://alhassy.com/PrologCheatSheet/CheatSheet.pdf")[Prolog Cheatsheet]

