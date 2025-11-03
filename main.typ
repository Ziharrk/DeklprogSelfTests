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
  - `(:)` und `[]`,
  - `Just` und `Nothing`,
  - `Left` und `Right`?
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
//     go a (p:xs) | a `mod` p == 0 = p : go (a `div` p) (p:xs)
//                 | otherwise      = go a xs
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
  `(++) :: [a] -> [a] -> [a]` wird verwendet, um zwei Listen aneinander zu hängen.
  Wenn wir eine Funktion induktiv über den Listentypen definieren wie z.B.
  ```hs square :: [Int] -> [Int]```, die jeden Listeneintrag quadrieren soll,
  dann können wir das wie folgt tun.
  ```hs
  square :: [Int] -> [Int]
  square []     = []
  square (x:xs) = [x * x] ++ square xs
  ```
  Dann ist Funktion zwar korrekt aber nicht Haskell-idiomatisch. Was müssten
  wir an der Funktion ändern, damit sie idiomatisch ist.
]

#test[
  Die Funktion ```hs show``` kann genutzt werden, um Werte eines beliebigen
  Datentyp in eine String-Representation zu überführen. Warum kann
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
  #footnote[#link("https://de.wikipedia.org/wiki/AVL-Baum#Rebalancierung")[Rebalancierung eines AVL-Baum]]

  Die Höhe eines Teilbaums kann z.B. als weiteres Attribut im Knoten gespeichert
  werden. Eine ineffizientere Variante ist die Höhe mit einer Funktion 
  wiederkehrend zu berechnen. Letztere Variante ist für den Anfang 
  übersichtlicher.

  Implementiere eine Funktion ```hs rotate :: SearchTree a -> SearchTree a```,
  die einen Teilbaum rebalanciert, sollte der Teilbaum unbalanciert sein.
  Diese Funktion kannst du dann nutzen, um die gängigen Operationen auf
  Suchbäumen anzupassen.
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
  Hin und wieder kommt es vor, dass diese Werte Vorbedingung erfüllen müssen,
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
  ```hs a -> b -> c```? Welcher andere Typ identisch zu letzterem?
]

#test[
  Mit welchen Konzepten gehen die Linksassoziativität der Funktionsapplikation
  und die Rechtsassoziatvität des Typkonstruktors ```hs ->``` Hand in Hand?
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
  Was sind sections im Kontext von Funktionen höherer Ordnung?
]

#test[
  Was ist der Unterschied zwischen ```hs foldl``` und ```hs foldr```?
  Wann liefern ```hs foldl``` und ```hs foldr``` das gleiche Ergebnis?
]

#test[
  Wie gewinnen wir aus ```hs foldr``` die Identitätsfunktion auf Listen?
  In den Übungen hast du gelernt, wie man Werte anderer Typen falten kann.
  Wie gewinne aus diesen Funktionen die Identitätsfunktionen auf den jeweiligen
  Typen?
]

// TODO Kopie aus Übung, überlege möglicherweise andere Typen?
#test[
  Gegeben sind folgende Datentypen
  - ```hs data Tree a = Empty | Leaf a | Branch (Tree a) (Tree a)```,
  - ```hs data RoseTree a = Node [RoseTree a] | Leaf a```.
  Welche Typen haben die jeweiligen Datenkonstruktoren und wie führen wir diese
  in die Signatur der jeweiligen Faltungsfunktion über?
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
  ```data Tree a = Empty | Node (Tree a) a (Tree a)```.

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
  - ```hs elem :: a -> Tree a -> Bool``` und ```hs notElem :: a -> Tree a -> Bool```,
  - ```hs toList :: Tree a -> Bool```,
  - ```hs null :: Tree a -> Bool``` (überprüft, ob der Baum leer ist),
  - ```hs length :: Tree a -> Int```,
  - ```hs maximum :: Tree Int -> Int``` und ```hs minimum :: Tree Int -> Int```, und
  - ```hs sum :: Tree Int -> Int``` und ```hs product :: Tree Int -> Int```.
]

#test[
  Welche Funktion verbirgt sich hinter ```hs foldr ((++) . f) []``` und was ist
  ihr Typ?
]


// Lazy Evaluation

// #test[
//   Wie können wir mit ```hs foldl``` auf unendlichen Listen mit keinem Ergebnis
//   rechnen?
// ]



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

