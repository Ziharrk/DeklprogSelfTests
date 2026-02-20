#import "prelude.typ": *
#show: config


= Funktionale Programmierung

== Ausdrücke und einfache Funktionen

#refs[
  - Skript: Funktionale Programmierung, Ausdrücke und Funktionen
  - #link("https://learnyouahaskell.github.io/introduction.html")[Introduction -- Learn You a Haskell for Great Good!]
  - #link("https://learnyouahaskell.github.io/starting-out.html")[Starting Out -- Learn You a Haskell for Great Good!]
    ("Ready, set, go!" und "Baby's first functions")
  - #link("https://learnyouahaskell.github.io/syntax-in-functions.html")[Syntax in Functions -- Learn You a Haskell for Great Good!]
    ("Where!?" und "Let it be")
  - #link("https://en.wikipedia.org/wiki/Pure_function")[Pure function -- Wikipedia]
]

#test(level: 2)[
  Beziehe Stellung zu der Aussage "Alles ist ein Ausdruck" in Haskell?
]

#test(level: 1)[
  Was bedeutet es, wenn eine Funktion keine Seiteneffekte hat? Warum ist die
  Abwesenheit von Seiteneffekten wünschenwert, sofern es möglich ist?
][
  #link("https://xkcd.com/1312/")[xkbd: Haskell]
]

#test(level: 1)[
  Haskell ist eine streng getypte Programmiersprache. Was bedeutet das?
]

#test(level: 1)[
  Wenn du eine Schleife in Haskell umsetzen möchtest, auf welches Konzept musst
  du dann zurückgreifen?
]

#test(level: 1)[
  Wie können wir Ausdrücke bedingt auswerten?
]

#test(level: 1)[
  Welche Vorteile und Nachteile haben streng getypte Programmiersprachen?
]

#test(level: 1, tags: (hl(),))[
  In imperativen Programmiersprachen sind Variablen Namen für Speicherzellen,
  deren Werte zum Beispiel in Schleifen verändert werden können. Als Beispiel
  betrachte die Funktionen ```py clz``` und ```py popcnt```.
  #columns(2, gutter: 1em)[
    ```py
    def clz(n):
      k = 0
      while n > 0:
        n //= 2
        k += 1
      return 64 - k
    ```
    #colbreak()
    ```py
    def popcnt(n):
      k = 0
      while n > 0:
        if n % 2 == 1:
          k += 1
        n //= 2
      return k
    ```
  ]
  In Haskell sind Variablen keine Namen für Speicherzellen. Wie können wir
  dieses Programm in Haskell umsetzen? Wo wandert das `k` hin oder wo wandert
  im Allgemeinen der Zustand hin?
] <clz_popcnt>

#test(level: 2, tags: (hl(),))[
  Um ein weiteres Glücksspiel neben Blackjack in diesem Modul vorzustellen,
  implementieren wir in diesem Test einen einarmigen Banditen. Diese soll einen
  initialen Zustand haben und als Ergebnis ein Tripel und einen neuen Zustand
  liefern. Sowohl der Zustand als auch das Tripel soll aus Ganzzahlen bestehen.

  - Überlege dir welche Signatur die Funktion haben muss. Dafür kannst du die
    Überlegungen aus @clz_popcnt fortführen.
  - Sei $s$ der initiale Zustand, dann soll das Tripel
    $ ((s + 5) mod 3, (s + 7) mod 3, (s + 11) mod 3) $
    sein und der neue Zustand $s + 11$. Implementiere eine Funktion
    ```hs slotMachine``` mit deinem überlegten Typ.
]

#test(level: 1)[
  Auf was müssen wir achten, wenn wir eine rekursive Funktion definieren?
  Die Antwort ist abhängig von dem, was die Funktion berechnen soll. Denke über
  die verschiedenen Möglichkeiten nach und gebe Beispiele an.
]

#test(level: 1)[
  Gegeben sei das folgende Haskell-Programm.
  ```hs
  even :: Int -> Bool
  even 0 = True
  even n = odd (n - 1)

  odd :: Int -> Bool
  odd 0 = False
  odd n = even (n - 1)
  ```

  - Berechne das Ergebnis von ```hs odd (1 + 1)``` händisch.
  - Wie sieht der Auswertungsgraph für den Ausdruck ```hs odd (1 + 1)``` aus?
    - Welcher Pfad entspricht deiner händischen Auswertung?
    - Welcher Pfad entspricht der Auswertung, wie sie in Haskell stattfindet?
    - Welcher Pfad entspricht der Auswertung, wie sie in Python sinngemäß
      stattfindet?
]

#test(level: 1)[
  Es wird als sauberer Programmierstil angesehen, Hilfsfunktionen, die nur für
  eine Funktion relevant sind, nicht auf der höchsten Ebene zu definieren.
  Mithilfe welcher Konstrukte kannst du diese lokal definieren?
]

#test(level: 1, tags: (hl(),))[
  Das Potenzieren einer Zahl $x$ (oder eines Elements einer Halbgruppe) mit
  einem natürlich-zahligen Exponenten $n$ ist mit $cal(O)(log n)$
  Multiplikationen möglich. Dafür betrachten wir
  $
  x^n = cases((x^(n/2))^2 & "falls" n "gerade,", x dot x(x^((n-1)/2))^2 & "sonst.")
  $
  Implementiere eine Funktion, die diese Variante des Potenzierens umsetzt.
][
  Das Verfahren ist als #link("https://de.wikipedia.org/wiki/Bin%C3%A4re_Exponentiation")[Binäre Exponentiation]
  bekannt. ```hs (^)``` ist so in Haskell implementiert (siehe
  #link("https://hackage-content.haskell.org/package/ghc-internal/docs/src/GHC.Internal.Real.html#powImpl")[```hs powImpl```]).
] <binexp>

#test(level: 1)[
  Gegeben ist folgender Ausdruck.
  ```hs
  let v = 3
      w = 5
      x = 4
      y = v + x
      z = x + y
   in y
  ```
  Welche Belegungen der Variablen werden tatsächlich berechnet, wenn wir
  ```hs y``` ausrechnen?
]

#test(level: 1)[
  Ist der folgende Ausdruck typkorrekt?
  #align(center)[```hs if 0 then 3.141 else 3141```]
]

#test(level: 1)[
  Definiere eine Funktion, die kein Ergebnis liefert.
]

#check[
  Ich bin in der Lage, ...
  - einfache Funktionen selbstständig zu definieren,
  - typkorrekte Ausdrücke zu definieren und händisch auszuwerten, und
  - erste grundlegende Konzepte der funktionalen Programmierung zu erklären wie
    - pure functions,
    - Rekursion und
    - streng getypte Programmiersprachen.
]


== Datentypen

#refs[
  - Skript: Funktionale Programmierung, Datentypen
  - #link("https://learnyouahaskell.github.io/introduction.html")[Introduction -- Learn You a Haskell for Great Good!]
  - #link("https://learnyouahaskell.github.io/starting-out.html")[Starting Out -- Learn You a Haskell for Great Good!] ("An intro to lists")
  - #link("https://learnyouahaskell.github.io/making-our-own-types-and-typeclasses.html")[Making Our Own Types and Typeclasses -- Learn You a Haskell for Great Good!] ("Algebraic data types intro")
]

#test(level: 1)[
  Wie werden algebraische Datentypen in Haskell definiert?
]

#test(level: 1)[
  Was ist charakterisierend für Aufzählungstypen, Verbundstypen und rekursive
  Datentypen? Gebe Beispiele für jeden dieser Typarten an.
]

#test(level: 1)[
  Benenne null-, ein- und zweistellige Konstruktoren aus der Haskell Prelude.
]

// TODO move to Polymorphismus?
#test(level: 1)[
  Gegeben ist der Typ ```hs IntList``` mit ```hs data IntList = Nil | Cons Int IntList```.
  Weiter kann mithilfe der Funktion
  ```hs
  lengthIntList :: IntList -> Int
  lengthIntList Nil         = 0
  lengthIntList (Cons _ xs) = 1 + lengthIntList xs
  ```
  die Länge einer solchen Liste berechnet werden. Du möchtest nun auch
  die Längen von Listen berechnen, die Buchstaben, Booleans oder Gleitkommazahlen
  enthalten. Was stört dich am bisherigen Vorgehen? Kennst du ein Konzept
  mit dessen Hilfe du mit weniger Arbeit an dein Ziel kommst?
]

#test(level: 1)[
  Wie ist die Funktion ```hs lengthIntList :: IntList -> Int``` aus dem vorherigen
  Test definiert?
]

#test(level: 1)[
  Du hast einen Datentypen definiert und möchtest dir Werte des Typen nun
  z.B. im GHCi anzeigen lassen. Was kannst du tun, um dieses Ziel zu erreichen?
]

#test(level: 1)[
  Wie definieren wir Funktionen?
]

#test(level: 1)[
  Gebe ein Listendatentypen an, für den es nicht möglich ist, kein Element
  zu enthalten.
][
  In Haskell heißt dieser Datentyp ```hs NonEmpty``` und ist definiert in
  #link("https://hackage-content.haskell.org/package/base/docs/Data-List-NonEmpty.html")[```hs Data.List.NonEmpty```].
]

#check[
  Ich bin in der Lage, ...
  - (monomorphe) algebraische Datentypen zu definieren,
  - Funktionen induktiv über den Datentypen zu definieren und
  - Listen zu verwenden und kenne wichtige Funktionen auf Listen.
]


== Polymorphismus

#refs[
  - Skript: Funktionale Programmierung, Polymorphismus
  - #link("https://learnyouahaskell.github.io/making-our-own-types-and-typeclasses.html")[Making Our Own Types and Typeclasses -- Learn You a Haskell for Great Good!] ("Type parameters", "Recursive data structures")
  - #link("https://learnyouahaskell.github.io/recursion.html")[Recursion -- Learn You a Haskell for Great Good!] ("Type parameters", "Recursive data structures")
]

#test(level: 1)[
  Wie sieht eine Datentypdefinition in Haskell im Allgemeinen aus?
]

#test(level: 1)[
  Welchen Typ haben
  - ```hs (:)``` und ```hs []```,
  - ```hs Just``` und ```hs Nothing```,
  - ```hs Left``` und ```hs Right```?
][
  Datenkonstruktoren sind "first-class values" bzw. Funktionen und haben
  entsprechend einen Typen. Das ist unter anderem von besonderer Bedeutung für
  die natürliche Faltung (die wir später kennenlernen).
]

#test(level: 1)[
  Was ist parametrischer Polymorphismus?
]

#test(level: 1)[
  Welche Typkonstruktoren des kinds ```hs * -> *``` oder ```hs * -> * -> *```
  kennst du?
]

#test(level: 1)[
  Welche kinds haben jeweils ```hs Either``` und ```hs Either a```?
]

#test(level: 1)[
  Beim Programmieren in Haskell vernachlässigen wir redundante Syntax.
  Gibt es in Haskell einen Unterschied zwischen ```hs f 1 2``` und
  ```hs f(1, 2)```?
]

#test(level: 1)[
  Welches Konzept erlaubt es uns, dass wir Funktionen auf Listen nicht für
  jeden konkreten Typen angeben müssen?
]

#test(level: 1)[
  Wie gewinnt man aus einem Typkonstruktor einen Typ?
]

#test(level: 1)[
  Visualisiere ```hs [1, 2, 3]``` als Termbaum, wie du es in der Vorlesung
  kennengelernt hast. Zur Erinnerung: Die inneren Knoten sind Funktionen und
  die Blätter Werte, die nicht weiter ausgerechnet werden können.
]

#test(level: 1)[
  Ist ```hs [32, True, "Hello, world!"]``` ein valider Haskell-Wert? Warum ja oder nein?
]

#test(level: 1)[
  Was ist der Unterschied zwischen einem Typ, einem Datenkonstrukor und einem
  Typkonstruktor?
]

#test(level: 1)[
  Gegeben ist
  ```hs
  data Pair a b = Pair a b
  ```
  Wie unterscheidet sich der Typ von
  ```hs
  data Pair a = Pair a a
  ```
]

#challenge(level: 1)[
  - Der größte gemeinsamen Teiler (ggT) zweier Ganzzahlen kann mithilfe des
    euklidschen Algorithmus berechnet werden. Implementiere das Verfahren.
    $
    "gcd"(x, y) = cases(
      abs(x) & quad "falls" y = 0\,,
      gcd(y, x "mod" y) & quad "sonst."
    )
    $
  - Alternativ kann der ggT auch berechnet werden, indem wir das Produkt des
    Schnittes der Primfaktorzerlegung der beiden Zahlen betrachten, also
    $ product ("PF"(x) inter "PF"(y)), $
    wobei $"PF"$ die Menge der Primfaktoren der gegebenen Zahl (mit
    entsprechenden Mehrfachvorkommen) beschreiben soll. Implementiere diesen
    Ansatz.
][][
  - Zur Darstellung der Multimengen eignen sich sortierte Listen gut.
  - Zur Berechnung des Schnittes können zwei sortierte Listen parallel
    durchlaufen werden. Wenn zwei gleiche Elemente zu Beginn der Liste stehen,
    wird eines der Elemente zum Ergebnis hinzugefügt. Im anderen Fall
    überspringen wir das jeweils kleinere Element der beiden.
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

#challenge(level: 1, tags: (hl(),))[
  Die Ableitung einer Funktion $f : RR -> RR$ kann mithilfe des
  Differenzenquotienten $(f(x+h)-f(x))/h$ für kleines $h$ approximiert werden.
  Ein andere Methode zur Berechnung der Ableitung ist symbolisches Differenzen
  und ähnelt dem, wie wir händisch Ableitungen berechnen.
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
  - Gebe die Funktion $sigma(x) = 1/(1+e^(-x))$ als Wert des Typs ```hs Fun```
    an.
  - Implementiere eine Funktion ```hs ($$) :: Fun -> Double -> Double```, die
    eine gegebene Funktion in einem gegebenen Punkt auswertet.
  - Implementiere eine Funktion ```hs derive :: Fun -> Fun```, die eine
    gegebene Funktion ableitet. Die Funktionen müssen nach dem Ableiten nicht
    vereinfacht werden.
][
  Hier findest du eine
  #link("https://de.wikipedia.org/wiki/Differentialrechnung#Zusammenfassung")[Zusammenfassung der Ableitungsregeln].
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

#challenge(level: 1)[
  In Franks Einführung in die Algorithmik hast du verschiedene Varianten des
  `mergesort`-Algorithmus kennengelernt. Eine davon hat ausgenutzt, dass in
  einer Eingabeliste bereits nicht-absteigend sortierte Teillisten vorkommen
  können, um den Algorithmus zu beschleunigen. Implementiere diese Variante in
  Haskell.

  Für den Anfang kannst du annehmen, dass die Eingabelisten vom Typ
  ```hs [Int]``` sind. Wenn wir Typklassen behandelt haben, kannst du
  ```hs Ord a => [a]``` nutzen -- oder du nutzt letzteres und behandelst es
  erstmal so, als wäre es ersteres.
][
  In der Haskell `base`-library wird `sort` aus `Data.List` vergleichbar
  implementiert:
  #link("https://hackage.haskell.org/package/ghc-internal-9.1201.0/docs/src/GHC.Internal.Data.OldList.html#sort")[Data.List.sort].
]

// ```hs
// mergesort :: [Int] -> [Int]
// mergesort xs = mergeAll (runs xs)
//   where
//     runs :: [Int] -> [[Int]]
//     runs []     = [[]]
//     runs [x]    = [[x]]
//     runs (x:xs)
//       | x < head r = (x:r) : rs
//       | otherwise  = [x] : (r:rs)
//       where (r:rs) = runs xs
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

#challenge(level: 1)[
  Entwickle einen Datentyp ```hs Ratio```, um rationale Zahlen
  $ p/q in QQ, quad p in ZZ, q in NN, p "und" q "teilerfremd" $
  darzustellen. Implementiere die Operationen: Addition, Subtraktion,
  Multiplikation, Divison. Implementiere weiter eine Funktion, die die
  rationale Zahl als reelle Zahl mit einer festen Anzahl von Nachkommastellen
  darstellt.

  Später kannst du auch hier die jeweiligen Typklassen verwenden, um die
  arithmetischen Operationen zu überladen.
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

#test(level: 2)[
  Wie können wir es hinkriegen, dass die invalide Liste
  ```hs [32, True, "Hello, world!"]``` ein valider Haskell-Wert wird? Mithilfe
  welches Hilfstypen kriegen das hin? (Die Liste müssen wir dafür unter
  Umständen umschreiben.)
]

#test(level: none)[
  Du hast bereits viele Funktionen kennengelernt, die in der Haskell
  `base`-library implementiert sind. Anstatt eine konkrete Liste dieser
  Funktionen anzugeben, möchten wir dich motivieren, folgende Dokumentationen
  verschiedener Module anzuschauen.
  - #link("https://hackage.haskell.org/package/base/docs/Prelude.html")[Prelude]
  - #link("https://hackage.haskell.org/package/base/docs/Data-List.html")[Data.List]
  Wenn du merkst, die Implementierung einer bekannten Funktion fällt dir ad hoc
  nicht ein, nehme dir Zeit und überlege, wie du sie implementieren könntest.
]

#test(level: 1)[
  Hier ist eine fehlerhafte Implementierung eines Datentyps für einen
  knotenbeschrifteten Binärbäumen.
  #align(center)[```hs data Tree a = Empty | Node Tree a Tree ```]
  Was ist der Fehler?
]

#test(level: 1)[
  In imperativen Programmiersprachen (hier Java) iterieren wir über Listen oft
  in folgender Form.
  ```java
  List<Integer> a = new ArrayList<>();
  a.add(3); a.add(1); a.add(4); a.add(1); a.add(5);

  List<Integer> b = new ArrayList<>();
  for (int i = 0; i < a.size(); i++) {
    b.add(2 * a.get(i));
  }
  ```
  Wenn wir diesen Code naiv in Haskell übersetzen, könnten wir z.B. folgenden
  Code erhalten.
  ```hs
  double :: [Int] -> Int -> [Int]
  double xs i | i < length xs = 2 * xs !! i : double xs (i + 1)
              | otherwise     = []
  ```
  Das wollen wir niemals so tun.
  - Was gefällt uns nicht?
  - Wie unterscheiden sich die Laufzeiten?
  - Optimiere die Funktion `double`, sodass sie lineare Laufzeit in der Länge
    der Liste hat.
  Dein Ergebnis sollte Haskell-idiomatisch sein.
]

#test(level: 1)[
  Die ```hs (!!)```-Funktion ist unsicher in dem Sinne, dass sie für invalide
  Listenzugriffe einen Fehler wirft -- also z.B. für ```hs xs !! (-1)``` oder
  ```hs xs !! k``` mit ```hs k > length xs```. Die Funktion
  ```hs (!?) :: [a] -> Int -> Maybe a``` ist eine sichere Variante von
  ```hs (!!)```. Sie macht den Fehlerfall explizit durch die Wahl des
  Ergebnistypen. Wie fängt der Ergebnistyp diesen Fehlerfall auf? Implementiere
  diese Funktion.
][
  Diese Funktion ist auch bereits vorimplementiert:
  #link("https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:-33--63-")[```hs (!?)``` in ```hs Data.List```].
]

#test(level: 2)[
  Gegeben sei der Datentyp für knotenbeschriftete Binärbäume
  #align(center)[```hs data Tree a = Empty | Node (Tree a) a (Tree a)```.]

  Mithilfe einer Pfadbeschreibung können wir uns durch so einen Baum navigieren.
  Diese Beschreibung soll durch eine Liste von Werten vom Typ
  ```hs data D = L | R``` dargestellt sein.

  Implementiere eine Funktion ```hs (!?) :: Tree a -> [D] -> Maybe a```, die
  die Beschriftung des Knotens zurückgibt, der durch die gegebene
  Pfadbeschreibung gefunden wird. Hier sind kleine Beispiele:
  - ```hs Node Empty 3 Empty !? []  = Just 3```
  - ```hs Node Empty 3 Empty !? [L] = Nothing```
  - ```hs Node Empty 3 Empty !? [R] = Nothing```
  - ```hs Node (Node Empty 1 (Node Empty 2 Empty)) 3 Empty !? [L, R] = 2```

][
  Da Datenkonstruktoren in Haskell nicht überladen werden können, können hier
  wir leider nicht ```hs Left``` und ```hs Right``` verwendet werden, solange
  die Datenkonstruktoren des ```hs Either```-Typs im scope sind.
]

#test(level: 1)[
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
  schreiben. Was müssten wir an der Funktion ändern, damit sie idiomatisch wäre.
]

#test(level: 2)[
  Die Funktion ```hs show``` kann genutzt werden, um Werte eines beliebigen
  Datentyp in eine String-Repräsentation zu überführen. Warum kann
  ```hs show``` nicht als Funktion vom Typ ```hs a -> String``` implementiert
  sein?
]

#test(level: 1, tags: (hl(),))[
  Da viele von euch Neulinge sind, wenn es um das Programmieren in Haskell geht,
  brauchen wir eine neue Sammlung von Phrasen, die wir nutzen können, um
  natürlich-sprachliche Anweisungen in Haskell zu formulieren. Finde
  "Programmierschablonen" für folgende Anweisungen:
  - "für jedes Element einer Liste, tue..."
  - "solange ein Wert etwas erfüllt, tue..."
  - "für jedes Element einer Liste, das eine Bedingung erfüllt, tue..."
  - "überprüfe ein Element existiert, das..."
  - "überprüfe für alle Elemente, ob..."
  - "betrachte alle Kombinationen zweier Listen, für jede Kombination tue..."
  Versuche so viele, wie möglich zu finden, die verschiedene Konzepte des
  Moduls aufgreifen.

  Als Beispiel für die erste Anweisung, können wir z.B. eine induktiv definierte
  Funktion implementieren, die vom Muster her so aussehen könnte
  ```hs
  f []     = e           -- Wenn wir keine Elemente mehr betrachten können,
                         -- dann e
  f (x:xs) = g x (f xs)  -- tue g in Abhängigkeit von x und (f xs)
                         -- f xs betrachtet die Elemente, die wir noch nicht
                         -- verarbeitet haben
  ```
  ```hs e``` und ```hs g``` sind hier Platzhalter für die Dinge, die wir tun
  wollen.
]

#test(level: 1)[
  Manche Funktionen lassen sich nur partiell sinnvoll definieren. Implementiere
  die Funktionen
  - ```hs head, last :: [a] -> a```, die das erste und letzte Element einer
    Liste zurückgeben,
  - ```hs init, tail :: [a] -> [a]```, die die ersten und letzten $n - 1$
    Elemente einer $n$-elementigen Liste zurückgeben,
  - ```hs (!!) :: [a] -> Int -> a```, das ein Element an einer bestimmten
    Position in einer Liste zurückgibt,
  - ```hs minimum, maximum :: Ord a => [a] -> a```, die das Minimum bzw. Maximum
    in einer Liste zurückgeben,
  - ```hs fromJust :: Maybe a -> a```, dass das Element aus einem
    ```hs Just```-Wert herausholt,
  und überlege dir, für welche Elemente des Definitionsbereichs du keine
  sinnvolle Regel angeben kannst. Überlege es dir ebenso für die Funktionen
  ```hs div, mod :: Int -> Int -> Int``` und
  ```hs read :: Read a => String -> a```, die du nicht implementieren musst.
] <test_partial_functions>

#check[
  Ich bin in der Lage, ...
  - (parametrischen) Polymorphismus zu erklären,
  - polymorphe Datentypen zu definieren, und
  - polymorphe Funktionen zu definieren.
]


== Pattern Matching

#refs[
  - Skript: Funktionale Programmierung, Pattern Matching
  - #link("https://learnyouahaskell.github.io/syntax-in-functions.html")[Syntax in Functions - Learn You a Haskell for Great Good!] ("Pattern matching", "Guards, guards!", "Case expressions")
]

#test(level: 1)[
  In Programmiersprachen wie Java greifen wir auf Daten komplexer Datentypen zu,
  indem wir auf Attribute von Objekten zugreifen oder getter-Methoden verwenden.
  Wie greifen wir auf Daten in Haskell zu?
]

#challenge(level: 2)[
  In Haskell sind Listen als einfach-verkettete Listen implementiert. Das macht
  sie ungeeignet für Operationen, die wahlfreien Zugriff in konstanter Laufzeit
  benötigen. Darüber hinaus sind Listen auch nicht mutierbar. Das führt dazu,
  dass Operationen, die eine Liste verändern, häufig lineare Laufzeit in der
  Länge der Liste haben -- mit Ausnahme der ```hs (:)```-Operation.

  Ziel dieser Challenge ist es, eine Datenstruktur zu entwickeln, die eine
  (amortisiert) konstante ```hs append```-Operation hat. Diese ist bekannt als
  ```hs Queue```. Sie soll durch ```hs data Queue a = Q [a] [a]``` dargestellt
  werden. Die Idee ist es, eine (linke) Liste vorzuhalten, die eine
  (amortisiert) konstante ```hs dequeue```-Operationen erlaubt, und eine andere
  (rechte), die eine konstante ```hs enqueue```-Operationen erlaubt. Das heißt,
  fast alle dieser Operationen benötigen konstante Laufzeit und konstant wenige
  können lineare Laufzeit haben.

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
  kannst du z.B. mit einer Hilfsfunktion erzwingen -- oder du passt bei der
  Implementierung deiner Funktionen auf. Falls es dir für den Anfang einfacher
  fällt, ignoriere die Invariante erstmal.
][
  Wenn sogar ```hs length xs >= length ys``` für eine queue ```hs Q xs ys```
  gewährleistet wird, ist die queue nochmal schneller. Dafür muss man die
  Längen der Listen immer vorhalten. Mehr darüber findest du in
  #link("https://www.cambridge.org/core/journals/journal-of-functional-programming/article/simple-and-efficient-purely-functional-queues-and-deques/7B3036772616B39E87BF7FBD119015AB")[Simple and efficient purely functional queues and deques]
  von Chris Okasaki lesen. Falls dich funktionale Datenstrukturen allgemein
  interessieren, sei dir
  #link("https://www.cs.cmu.edu/~rwh/students/okasaki.pdf")[seine
  Doktorarbeit] empfohlen.
][
  Nach $n$ ```hs enqueue```- Operationen in eine leere queue sieht diese wie
  folgt aus
  #align(center)[
    ```hs Q [] [xn, ..., x1]```.
  ]
  Wenn als Nächstes ```hs dequeue```-Operationen naiv ausgeführt werden,
  müssten wir die ```hs enqueue```-Liste jedes Mal vollständig durchlaufen.
  Wenn wir die ```hs dequeue```-Liste entsprechend verändern, können wir
  mindestens die nächsten $n$ ```hs dequeue```-Operationen effizient
  durchführen.
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

#challenge(level: 1)[
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
][
  #link("https://de.wikipedia.org/wiki/AVL-Baum#Rebalancierung")[Rebalancierung eines AVL-Baum].
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

#check[
  Ich bin in der Lage, ...
  - Pattern Matching zu nutzen.
]


== Automatisches Testen

#refs[
  - Skript: Funktionale Programmierung, Automatisches Testen
]

#test(level: 2)[
  Formuliere QuickCheck-Eigenschaften, die die Funktionen
  - ```hs isElem :: Int -> SearchTree Int -> Bool```,
  - ```hs toList :: SearchTree Int -> [Int]```,
  - ```hs insert :: Int -> SearchTree Int -> SearchTree Int``` und
  - ```hs delete :: Int -> SearchTree Int -> SearchTree Int```
  erfüllen sollen. ```hs isElem``` überprüft, ob eine Ganzzahl in gegebenen
  Suchbaum enthalten ist. ```hs toList``` konvertiert einen Suchbaum in eine
  Liste. ```hs insert``` fügt eine Ganzzahl in einen Suchbaum ein.
  ```hs delete``` löscht eine Ganzzahl aus einen Suchbaum.

  Wie kannst du die Suchbaum-Eigenschaft spezifizieren? Zum Spezifizieren der
  Eigenschaften benötigst du möglicherweise weitere Funktionen.
]

#test(level: 1)[
  QuickCheck-Eigenschaften werden mit zufällig generierten Werten getestet.
  Hin und wieder kommt es vor, dass diese Werte Vorbedingungen erfüllen müssen,
  damit wir Eigenschaften von Funktionen testen können. Wie können wir das
  erreichen?
]

#test(level: 1)[
  Wie können wir für eine beliebige Eingabe verifizieren, dass die Ausgabe
  korrekt ist? Wieso können wir mithilfe dieses Ansatzes in den meisten Fällen
  nicht die Korrektheit einer Funktion zeigen?
]

// TODO Finde ein besseres Beispiel dafür, dass es keine gute Idee ist, eine
// Implementierung mit einer Referenz-Implementierung zu vergleichen, wenn diese
// sich nicht wesentlich unterscheiden (Fehler in beiden Implementierungen).
//
// #test(level: none)[
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

#test(level: 2)[
  Welchen Nachteil hat die Prüfung von Vorbedingungen mit ```hs (==>)```? Wie
  können wir diese beheben?
]

#check[
  Ich bin in der Lage, ...
  - Eigenschaften mit QuickCheck zu spezifizieren
  - ... mit Kombinatoren wie z.B. ```hs ==>```, und
  - Vor- und Nachteile des eigenschaftsbasierten Testens zu diskutieren.
]


== Funktionen höherer Ordnung

#refs[
  - Skript: Funktionale Programmierung, Funktionen höherer Ordnung
  - #link("https://learnyouahaskell.github.io/higher-order-functions.html")[Higher Order Functions -- Learn You a Haskell for Great Good!]
]

#test(level: 1)[
  Was sind Funktionen höherer Ordnung?
]

#test(level: 1)[
  Wie definieren wir Lambda-Abstraktionen bzw. anonyme Funktionen?
]

#test(level: 1)[
  Warum ist der Typ ```hs (a -> b) -> c``` nicht identisch zum Typ
  ```hs a -> b -> c```? Welcher andere Typ ist identisch zu letzterem?
]

#test(level: 1)[
  Mit welchen Konzepten gehen die Linksassoziativität der Funktionsapplikation
  und die Rechtsassoziatvität des Typkonstruktors ```hs (->)``` gut Hand in
  Hand?
]

// TODO möglicherweise gibt es coolere Funktionen, die auch noch ohne folds
//      auskommen - für folds wollte ich einen Test extra haben
#test(level: 2)[
  Zu welchen partiell applizierten Funktionen verhalten sich folgende
  Funktionen identisch?
  - ```hs succ :: Int -> Int``` (die Inkrementfunktion)
  - ```hs pred :: Int -> Int``` (die Dekrementfunktion)
  - ```hs length :: [a] -> Int```
  - ```hs sum :: [Int] -> Int```
  - ```hs product :: [Int] -> Int```
]

#test(level: 1)[
  Sectioning ist das Erstellen einer neuen Funktion durch teilweises Anwenden
  eines Operators. Zum Beispiel entspricht ```hs (+ 1)``` der Inkrementfunktion
  und ```hs (* 2)``` der Verdopplungsfunktion. Warum ist aber ```hs (- 1)```
  keine gültige section? Welche Rolle spielt ```hs subtract``` in diesem
  Kontext?
]

#test(level: 1)[
  Was ist partielle Applikation?
]

#test(level: 1)[
  Was ist Currying?
]

#test(level: 1)[
  Welche Funktionen höherer Ordnung hast du kennengelernt im Kontext der
  generischen Programmierung? Was ist das Ziel dieser Funktionen?
]

#test(level: 1)[
  Gebe ```hs map``` und ```hs filter``` unter der Unterverwendung von
  ```hs foldr``` an.
]

#test(level: 1)[
  Gegeben seien folgende Funktionen:
  - ```hs rgbToHsv :: RGB -> HSV```, die eine Farbe von einer Darstellung
    in eine andere konvertiert, und
  - ```hs hue :: HSV -> Float```, die den Farbwert einer Farbe im Wertebereich
    $[0°, 360°)$ im HSV-Farbraum zurückgibt.
  Du bekommst als Eingabe einen Bild, das hier als Liste von ```hs RGB```-Werten
  dargestellt ist. Jeder ```hs RGB```-Wert korrespondiert zu einem Pixel.
  Schreibe eine Funktion, die berechnet, wie viele blaue Pixel das Bild hat.
  Hier bezeichene eine Farbe als blau, wenn ihr Farbwert zwischen $200°$ und
  $250°$ (inklusiv) liegt. Nutze für die Definition der Funktion sowohl
  ```hs map``` als auch ```hs filter```.
]

#test(level: 2)[
  Mit Funktionen höherer Ordnung können wir Kontrollstrukturen aus der
  imperativen Programmierung definieren. Hier ist eine mögliche Definition einer
  bedingten Wiederholung.
  ```hs
  while :: (a -> Bool) -> (a -> a) -> a -> a
  while p f x | p x       = while p f (f x)
              | otherwise = x
  ```

  In @clz_popcnt haben wir zwei Funktionen gesehen, die diese Kontrollstruktur
  verwenden. In Haskell können wir ```py clz``` mithilfe von ```hs while``` wie
  folgt definieren.
  ```hs
  clz :: Int -> Int
  clz n = snd (while cond step (n, 0))
    where
      cond (n, k) = n > 0
      step (n, k) = (n `div` 2, k + 1)
  ```
  Oder alternativ so
  ```hs
  clz :: Int -> Int
  clz = go 0
    where
      go k n | n > 0     = go (k + 1) (n `div` 2)
             | otherwise = 64 - k
  ```
  Beide Funktionsdefinitionen sind semantisch äquivalent. Argumentiere unter
  verschiedenen Aspekten, warum die eine Implementierung besser als die andere
  sein könnte -- z.B. in Hinsicht auf Lesbarkeit, Idiomatik und Wartbarkeit.
]

#test(level: 1)[
  Was sind sections im Kontext von Funktionen höherer Ordnung?
]

#test(level: 1)[
  Welche der Faltungsfunktion auf Listen ergibt sich aus dem Verfahren zur
  Erzeugung von Faltungsfunktionen?
]

#test(level: 1)[
  Was ist der Unterschied zwischen ```hs foldl``` und ```hs foldr```?
  Wann liefern ```hs foldl``` und ```hs foldr``` das gleiche Ergebnis?
]

#test(level: 1)[
  Wie gewinnen wir aus ```hs foldr``` die Identitätsfunktion auf Listen?
  In den Übungen hast du gelernt, wie man Werte anderer Typen falten kann.
  Wie gewinnt man aus diesen Funktionen die Identitätsfunktionen auf den
  jeweiligen Typen?
]

#test(level: 1)[
  Gegeben sind folgende Datentypen
  - ```hs data Tree a = Empty | Leaf a | Branch (Tree a) (Tree a)```,
  - ```hs data Rose a = Node [Rose a]```.
  Welche Typen haben die jeweiligen Datenkonstruktoren und wie führen wir diese
  in die Signatur der jeweiligen Faltungsfunktion über?
  Wo benötigen wir rekursive Aufrufe der jeweiligen Faltungsfunktionen?
]

#test(level: 2)[
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

#test(level: 1)[
  Wie kannst du mithilfe von Faltung viele Elemente in einen Suchbaum einfügen
  oder lösen? Implementiere
  - ```hs insertMany :: [Int] -> SearchTree Int -> SearchTree Int``` und
  - ```hs deleteMany :: [Int] -> SearchTree Int -> SearchTree Int```.
  Du kannst davon ausgehen, dass du die Einfüge- und Löschfunktion für einzelne
  Elemente bereits hast.
]

#test(level: 2)[
  Wir können ```hs map :: (a -> b) -> [a] -> [b]``` mithilfe von
  ```hs foldr``` wie folgt implementieren:
  #align(center)[```hs map f xs = foldr (\x ys -> f x : ys) [] xs```]
  Vereinfache den Lambda-Ausdruck mithilfe von Funktionen höherer Ordnung.
]

#test(level: 2)[
  Wenn wir die Listenkonstruktoren in ```hs foldr``` einsetzen, erhalten wir die
  Identitätsfunktion auf Listen, also
  #align(center)[```hs foldr (:) [] :: [a] -> [a]```.]
  Wenn wir das Gleiche mit ```hs foldl``` und angepassten ```hs (:)``` machen,
  also
  #align(center)[```hs foldl (flip (:)) [] :: [a] -> [a]```,]
  dann erhalten wir nicht die Identitätsfunktion auf Listen. Warum und was
  bekommen wir stattdessen heraus?
]

#test(level: 2)[
  Es gibt viele andere hilfreiche Funktionen höherer Ordnung in der Haskell
  Prelude. Eine von diesen ist ```hs zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]```.
  Sie verknüpft jeweils zwei Elemente aus den jeweiligen Listen unter der
  gegeben Funktion.
  - Implementiere ```hs zipWith``` mithilfe von ```hs map, uncurry, zip```.
  - Implementiere ```hs zip``` mithilfe von ```hs zipWith```.
  - Implementiere das Prädikat ```hs isSorted``` mithilfe von ```hs zipWith```.
]

#test(level: 3, clock: true)[
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

#test(level: 3)[
  In diesem Test wollen wir einen beliebigen Wert zu einen Baum entfalten.
  Implementiere eine Funktion ```hs unfoldTree :: (b -> Maybe (b, a, b)) -> b -> Tree a```,
  die eine Funktion nimmt, die ein Wert von Typ ```hs b``` nimmt und in eine
  Knotenbeschriftung und zwei weitere Wert vom Typ ```hs b``` aufspaltet, oder
  das Entfalten stoppt, indem ein ```hs Nothing``` zurückgegeben wird. Bäume
  sind durch den Datentyp ```hs data Tree a = Empty | Node (Tree a) a (Tree a)```
  definiert.

  Nutze die definierte Funktion, um einen Binärbaum zu erzeugen, welche die
  Zerlegungen eines diskreten Intervals darstellen soll. Entnehme die Art der
  Zerlegung dem Diagramm.
  #align(center)[
    #diagraph.raw-render(
      ```dot
      digraph {
        // node [shape=plaintext];
        edge [arrowsize=0.6];
        1 -> 2;
        1 -> 3;
      }
      ```,
      labels: (
        "1": $[l, r)$,
        "2": $[l, floor((l+r)/2))$,
        "3": $[floor((l+r)/2), r)$,
      )
    )
  ]
]

#challenge(level: 1)[
  Gegeben sei die Faltungsfunktion
  ```hs foldTree :: (r -> a -> r -> r) -> r -> Tree a -> r```
  für einen knotenbeschrifteten Binärbaum gegeben durch
  ```hs data Tree a = Empty | Node (Tree a) a (Tree a)```.

  Eine Reihe von Funktionen, die du bereits für Listen kennengelernt
  hast, lassen sich auch auf Bäume übertragen.

  Implementiere die Funktionen
  - ```hs any :: (a -> Bool) -> Tree a -> Bool``` und
    ```hs and :: (a -> Bool) -> Tree a -> Bool```,
  - ```hs elem :: Int -> Tree Int -> Bool``` und
    ```hs notElem :: Int -> Tree Int -> Bool```,
  - ```hs toList :: Tree a -> Bool```,
  - ```hs null :: Tree a -> Bool``` (überprüft, ob der Baum leer ist),
  - ```hs length :: Tree a -> Int```,
  - ```hs maximum :: Tree Int -> Int``` und ```hs minimum :: Tree Int -> Int```, und
  - ```hs sum :: Tree Int -> Int``` und ```hs product :: Tree Int -> Int```.
][
  Diese Funktionen lassen sich auf alle faltbaren Datentypen verallgemeinern.
  Dies wird mithilfe der
  #link("https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-Foldable.html")[Typklasse ```hs Foldable```]
  festgehalten.
]

#test(level: 2)[
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

#test(level: 2)[
  Gegeben sei der Datentyp ```hs data Tree a = Empty | Node (Tree a) a (Tree a)```
  und die Faltungsfunktion ```hs foldTree :: r -> (r -> a -> r -> r) -> Tree a -> r```.

  Vergewissere dich, dass die Implementierung der folgenden Funktion, die alle
  Beschriftungen durch den gleichen Wert ersetzt, korrekt ist.
  ```hs
  replace :: b -> Tree a -> Tree b
  replace x = foldTree Empty (const . flip Node x)
  ```
  Zeige, dass ```hs const . flip Node x = \l y r -> Node l x r``` ist.
][
  Es gibt Programmierende, die einen sogenannten
  #link("https://wiki.haskell.org/Pointfree")[punktfreien Stil] bevorzugen.
  Weil es teilweise unterhaltsam sein kann, sich darüber Gedanken zu machen,
  was eine äquivalente punktfreie Funktion zu einem Lambda-Ausdruck sein könnte,
  steht hier eben ```hs const . flip Node x``` statt
  ```hs \l _ r -> Node l x r```. Falls du damit herumspielen möchtest, findest
  du hier #link("https://pointfree.io/")[eine Seite], auf der du Haskell-Code in
  punktfreien Haskell-Code übersetzen kannst.
]

// ```hs
//   const . flip Node x               -- Definition (.)
// = \l -> const (flip Node x l)       -- Definition flip
// = \l -> const (Node l x)            -- Definition const
// = \l -> \y -> Node l x              -- Notation
// = \l y -> Node l x                  -- Eta-Expansion
// = \l y r -> Node l x r
// ```

#test(level: 1)[
  Welche Funktion verbirgt sich hinter ```hs foldr ((++) . f) []``` und was ist
  ihr Typ?
]

#test(level: 2)[
  Versuche in den folgenden Ausdrücken, Teilausdrücke schrittweise durch
  bekannte Funktionen zu ersetzen oder gegebenenfalls zu vereinfachen.
  - ```hs foldr (\x ys -> f x : ys) [] (foldr (\x ys -> g x : ys) [] xs)```,
  - ```hs map (\_ -> y) xs```,
  - ```hs foldr (\x ys -> if x `mod` 2 == 1 then x - 1 : ys else ys) [] xs```,
  - ```hs foldl (\ys x -> x : ys) [] xs``` und
  - ```hs flip (curry snd) x```.
][
  #link("https://www.youtube.com/watch?v=_oNgyUAEv0Q")["Your scientists were
  so preoccupied with whether or not they could, that they didn't stop to
  think if they should."] Jenseits solcher kleinen Verständnisfragen gilt
  weiterhin, dass wir verständlichen Code schreiben wollen. Solche Ausdrücke
  wie ```hs flip (curry snd) x``` sind häufig schwieriger zu verstehen -- auch
  wenn es unterhaltsam ist, sich solche Ausdrücke auszudenken.
]

// ```
//   foldr (\x ys -> f x : ys) [] (foldr (\x ys -> g x : ys) [] xs)    (Definition map über foldr)
// = foldr (\x ys -> f x : ys) [] (map g xs)                           (Definition map über foldr)
// = map f (map g xs)                                                  (Komposition von map)
// = map (f . g) xs
//
//   map (\_ -> y) xs                                                  (Definition const)
// = map (const y) xs
//
//   foldr (\x ys -> if x `mod` 2 == 1 then x - 1 : ys else ys) [] xs
// = foldr (\x ys -> x - 1 : ys) [] (filter odd xs)                    (Definition map über foldr)
// = map (\x -> x - 1) (filter odd xs)                                 (Definition pred)
// = map pred (filter odd xs)
//
//   foldl (\ys x -> x : ys) [] xs                                     (Kopfnormalform xs)
// = foldl (\ys x -> x : ys) [] (x1 : ... : xn : [])                   (Definition foldl)
// = foldl (\ys x -> x : ys) (x1 : []) (x2 : ... : xn : [])            (Definition foldl)
// = ...
// = foldl (\ys x -> x : ys) (xn : ... : x1 : []) []                   (Definition foldl)
// = xn : ... : x2 : x1 : []                                           (Definition reverse)
// = reverse xs
//
//   flip (curry snd) x                                                (Definition snd)
// = flip (curry (\(a, b) -> b)) x                                     (Definition curry)
// = flip (\a b -> b) x                                                (Definition flip)
// = (\b a -> b) x                                                     (Lambda-Applikation)
// = \a -> x
// ```


#challenge(level: 2)[
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

#test(level: 1)[
  Eta-reduziere die folgende Ausdrücke:
  - ```hs sum xs = foldr (+) 0 xs```,
  - ```hs add a b = a + b``` und
  - ```hs \x ys -> (:) x ys```.
]

#test(level: 1)[
  Implementiere die Funktion
  ```hs insert :: Int -> a -> Map Int a -> Map Int a```,
  die ein Schlüssel-Wert-Paar in eine ```hs Map Int a``` einfügt.
  Die ```hs Map``` ist wie folgt repräsentiert ```hs type Map k v = k -> v```.
]

#test(level: 3)[
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
][
  Das ```hs foldr``` für ```hs Tree a``` entspricht nicht der natürlichen
  Faltung auf diesem Typen.
]

#test(level: 2, breakable: true)[
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
  In diesem Test geht es darum, diese Konzepte im Python-Programm zu
  identifizieren.

  Wo findest du
  - Funktionen höherer Ordnung,
  - pattern matching,
  - algebraische Datentypen (Typkonstruktoren, Datenkonstruktoren),
  - parametrischen Polymorphismus,
  - ad-hoc Polymorphismus (Typklassen bzw. Überladung) und
  - lokale Definitionen.
][
  Typannotationen in Python sind nicht sonderlich elegant. Deshalb sind
  nur die angegeben, um den parametrischen Polymorphismus zu identifizieren
  und data classes anständig zu nutzen.

  Data classes und match statements brauchst du dir jenseits dieses Tests
  nicht anschauen (wenn es dich nicht weiter interessiert). Es soll in dem
  Test nur darum gehen, die Haskell-Konzepte zu erkennen. In
  @typeclasses_in_python_remark kannst du das gleiche Programm in Java sehen.
] <typeclasses_in_python>

#test(level: 2)[
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

#test(level: none)[
  Schau dir die Selbsttests dieser Sektion erneut an und versuche, geeignete in
  Python zu lösen.
]

#check[
  Ich bin in der Lage, ...
  - Funktionen höherer Ordnung zu erkennen, zu definieren und zu nutzen,
  - wichtige Funktionen höherer Ordnung zu definieren wie
    - ```hs map```, ```hs filter```, ```hs foldr``` und ```hs foldl```
    - ```hs (.)```, ```hs ($)```, ```hs flip```, ```hs curry``` und ```hs uncurry```
  - das Zusammenspiel aus Currying und partieller Applikation zu erklären,
  - natürliche Faltungsfunktionen für gegebene Datentypen zu definieren,
  - Funktionen höherer Ordnung in Python zu nutzen und damit verbundene
    Fallstricke zu erklären.
]


== Abstrakte Datentypen

#refs[
  - Skript: Funktionale Programmierung, Abstrakte Datentypen
]

#test(level: 1)[
  Was ist ein abstrakter Datentyp? Was sind die Bestandteile eines abstrakten
  Datentyps?
]

#test(level: 1)[
  Wie definieren wir die Semantik der zu einem abstrakten Datentyp gehörenden
  Operationen? Wie definieren wir sie insbesondere nicht?
]

#test(level: 1)[
  Wieso ist das sofortige Nutzen einer Gleichheit auf einem abstrakten Datentypen
  problematisch? Was sollte man stattdessen tun?
]

#test(level: 2)[
  Zur Spezifikation der Semantik nutzen wir Gesetze, die bestimmen, wie
  verschiedene Operationen miteinander interagieren. Dafür benötigen wir
  verschiedene Werte oftmals unterschiedlicher Datentypen. Wo kommen diese her
  und wie sind sie quantifiziert?
]

#test(level: 1)[
  Welche Eigenschaften sollten die für einen abstrakten Datentypen formulierten
  Gesetze erfüllen, damit sie eine sinnvolle Semantik beschreiben?
]

#test(level: 1, tags: (hl(),))[
  Die #link("https://de.wikipedia.org/wiki/Pomodoro-Technik")[Pomodoro-Technik]
  ist eine Zeitmanagement-Methode, die einen Kurzzeitwecker nutzt, um Arbeits-
  und Pauseabschnite einzuteilen. Um ein Informatik-Studierenden-Klischee zu
  bedienen, entscheidest dich den Timer selber zu bauen. Da du dich nicht
  planlos in die Implementierung stürzen möchtest, formulierst du zuerst einen
  ADT.

  Der ADT soll folgende Operationen unterstützen. Es soll möglich sein, ...
  - ... einen Timer mit einer Zeiteingabe zu starten -- die Zeiteingabe soll aus
    Stunden und Minuten bestehen,
  - die verbleibende Zeit auf dem Timer abzufragen, und
  - zu prüfen, ob der Alarm bereits ausgelöst wurde. Der Alarm wird gelöst,
    sobald die verbleibende Zeit vorüber ist.
  - Um das Vergehen von Zeit zu modellieren, soll der ADT weiter eine Funktion
    bereitstellen, die eine Sekunde vergehen lässt. Wenn keine Zeit mehr
    verblieben ist, dann hat diese Operation keine Auswirkung.

  Gebe für jede Funktion mindestens ein Gesetz an.
][
  Einen Lösungsvorschlag für diese Aufgabe findest du im Anhang dieses Dokument
  als @timer_adt_solution. Weiter findest du in @timer_adt_mistakes auch
  Anmerkungen zu Fehlern, die in der Klausur des 1. Prüfungszeitraums WS25/26
  übertragen auf diese Aufgabe gemacht wurden.
] <timer_adt>

#challenge(level: none, clock: true)[
  Gebe folgende abstrakte Datentypen an: Paar, Menge, stack, queue,
  double-ended queue, knotenbeschrifteter Binärbaum, priority queue.

  Anschließend kannst du diese auch (naiv) implementieren und deine Implementierung
  testen, indem du deine formulierten Gesetze mit QuickCheck implementierst.
]

#test(level: 2)[
  Als Teil eines ADTs für Arrays soll eine Operation
  ```hs reverse :: Array a -> Array a``` spezifiziert werden, die ein Array
  umdreht. Ihr Verhalten soll unter anderem durch das folgende Gesetz festgehalten sein:
  #align(center)[```hs reverse (reverse a) == a``` #h(1em) für alle Arrays ```hs a```]
  Warum ist dieses Gesetz problematisch? Wie können wir das Problem beseitigen?
]

#test(level: 2)[
  Als Teil eines ADTs für Array soll eine Operation ```hs at :: Array a -> Int -> a```
  spezifiziert werden, die das Element an einer Position in einem Array zurückgibt.
  Weiter soll ```hs update :: Int -> a -> Array a -> Array a``` einen Wert an
  eine Position in ein Array schreiben.

  Wie können wir spezifizieren, dass durch ein ```hs update``` nur das Element
  an der gegebenen Position verändert wird?
]

// update k x a `at` k == x
// j /= k ==> update k x a `at` j == a `at` j

#test(level: 2)[
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

#test(level: 1)[
  In ADT-Gesetzen sind Variablen allquantifiziert. Wie können wir gewährleisten,
  dass ein Wert bestimmte Bedingungen erfüllt, bevor wir ein entsprechendes
  Gesetz für solche Werte definieren?
]

#test(level: 1)[
  Warum benötigen wir Konstruktoren als Teil eines ADTs?
]

#test(level: 1)[
  Im Kontext der objektorientierten Programmierung hast du wahrscheinlich das
  Konzept der Datenkapselung kennengelernt. Dabei geht es um das Verbergen von
  Daten, sodass Zugriffe von außen nicht möglich sind. In der objektorientierten
  Programmierung wird dies z.B. durch explizite Angabe von Zugriffsarten
  für Attribute oder Methoden erreicht.

  In Haskell haben wir so etwas ```java private``` und ```java public``` nicht.
  Wie können wir aber trotzdem verhindern, dass bestimmte Operationen auf
  Werten eines Datenttypen nicht möglich sind? Welche Rolle spielen smart
  constructors in diesem Zusammenhang?
]

#test(level: 2, clock: true)[
  Kritisiere folgenden ADT für eine queue, so pingelig wie du kannst, und
  bessere ihn.
  ```hs
  empty :: Queue a
  isEmpty :: Queue a -> Bool
  enqueue :: a -> Queue a -> Queue a
  dequeue :: Queue a -> Queue a
  front :: Queue a -> a
  rear :: Queue a -> a
  clear :: Queue a -> Queue a
  duplicate :: Queue a -> Queue a
  reverseQ :: Queue a -> Queue a
  toList :: Queue a -> [a]
  fromList :: [a] -> Queue a

  isEmpty empty = True
  isEmpty (enqueue x q) = isEmpty q
  isEmpty (clear q) = True
  enqueue x empty = empty
  enqueue x (enqueue y q) = enqueue y (enqueue x q)
  dequeue empty = empty
  dequeue (enqueue x q) = q
  dequeue q = empty
  front empty = front empty
  front (enqueue x q) = x
  front q = front (dequeue q)
  front q = front (enqueue x q)
  size empty = 1
  size (enqueue x q) = size q
  size (dequeue q) = size q + 1
  reverseQ empty = empty
  reverseQ (enqueue x q) = enqueue x (reverseQ q)
  reverseQ (reverseQ q) = q
  reverseQ q = q
  duplicate empty = empty
  duplicate q = enqueue (front q) q
  clear q = q
  clear q = empty
  toList empty = []
  toList (enqueue x q) = x : toList q
  fromList [] = empty
  fromList (x:xs) = enqueue x (fromList xs)
  fromList (toList q) = empty
  toList (fromList xs) = []
  enqueue x q = q
  enqueue x q = enqueue y q
  dequeue (enqueue x empty) = enqueue x empty
  ```
]

#check[
  Ich bin in der Lage, ...
  - zu erklären, was ein abstrakter Datentyp ist und was er besteht,
  - zu erklären, was der theoretische und praktische Nutzen von abstrakten
    Datentypen ist,
  - abstrakte Datentypen anzugeben,
  - keine Implementierung für einen ADT anzugeben, wenn nicht explizit danach
    gefragt ist.
]


== Typklassen und Überladung

#refs[
  - Skript: Funktionale Programmierung, Typklassen und Überladung
  - #link("https://learnyouahaskell.github.io/types-and-typeclasses.html")[Types and Typeclasses -- Learn You a Haskell for Great Good!]
  - #link("https://learnyouahaskell.github.io/making-our-own-types-and-typeclasses.html")[Making Our Own Types and Typeclasses -- Learn You a Haskell for Great Good!] ("Typeclasses 102", "A yes-no typeclass")
]

#test(level: 1)[
  Was sind Typklassen?
]

#test(level: 1)[
  Wie unterscheidet sich der Polymorphismus, der durch Typklassen ermöglicht
  wird, vom parametrischen Polymorphismus?
]

#test(level: 1)[
  In einem vorherigen Test wurdest du bereits gefragt, wieso ```hs show```
  nicht als Funktion mit dem Typ ```hs a -> String``` implementiert sein kann.
  Wieso wird die Funktion durch den Typ ```hs Show a => a -> String``` gerettet?
]

#test(level: 1)[
  Welche Typklassen kennst du? Was ermöglichen sie konkret?
]

#test(level: 2)[
  Eine ```hs Show```-Instanz für den Typ
  ```hs data Tree a = Leaf a | Tree a :+: Tree a```
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

#test(level: 1)[
  Überlade die Operationen ```hs (+), (-), (*), abs, signum, fromInteger```
  für den Datentypen ```hs data Mat22 a = Mat22 a a a a```, der
  $(2 times 2)$-Matrizen repräsentieren soll -- ```hs abs, signum, fromInteger```
  kannst du z.B. komponentenweise implementieren.
][
  Oft sind an Funktionen von Typklassen Bedingungen bzw. Gesetze, die erfüllt
  werden sollen, gekoppelt. Diese werden durch den Implementierungsvorschlag
  von ```hs abs``` und ```hs signum``` nicht erfüllt.
] <matmath>

#test(level: 1)[
  Mit $ mat(f_(n+1), f_n; f_n, f_(n-1))^n = mat(1, 1; 1, 0)^n $ und der
  binären Exponentiation (@binexp) und ```hs Mat22 Integer``` (@matmath) aus
  vorherigen Tests kannst du die $n$-te Fibonacci-Zahl in logarithmischer
  Laufzeit in $n$ berechnen. Implementiere das Verfahren.

  #text(0.8em)[
    Da du eine ```hs Num```-Instanz auf ```hs Mat22``` definiert hast, kannst
    du den ```hs (^)```-Operator zur binären Exponentiation nutzen.
  ]
]

#test(level: 1)[
  Wieso gilt sowohl #link("https://www.youtube.com/watch?v=ZCPN9SfdH7c&t=108s")[```hs 14000605 :: Int```]
  als auch ```hs 14000605 :: Float```?
]

// https://www.haskell.org/onlinereport/haskell2010/haskellch6.html#x13-1360006.4.1
// fromInteger (42 :: Integer) :: Num a => a
// fromRational (3.1415 :: Rational) :: Fractional a => a

#challenge(level: 2, clock: true)[
  In dieser Challenge sollst du automatisches Differenzieren im Rückwärtsmodus
  mithilfe von (Operator-)Überladung implementieren. Dieser Ansatz des
  Differenzierens führt dabei das Differenzieren komplizierter Funktionen auf
  einfache, elementare Funktionen zurück.

  Wir verwenden folgenden Datentyp: ```hs data D a = D a a```.
  Ein Wert vom Typ ```hs D a``` enthält einen Funktionswert und die Ableitung
  an einer gegebenen Stelle.

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
  #align(center)[```hs (f . g)' x = g' x * f' (g x) = dgdx * f' gx```.]
  Nach dem Muster kannst du nun Standardfunktionen überladen. Für die
  arithmetischen Operatoren benötigst du an der Stelle deren Ableitungsregeln
  (Summenregel, Leibnizregel, usw.)

  Implementiere die Typklasseninstanzen ```hs Num```, ```hs Fractional```
  und ```hs Floating```.

  Mit den folgenden Funktionen kannst du z.B. die erste oder zweite Ableitung
  bilden.

  ```hs
  d1 :: Num a => (D a -> D b) -> a -> b
  d1 f x = let (D _ d) = f (D x 1) in d

  d2 :: Num a => (D (D a) -> D (D b)) -> a -> b
  d2 f x = let (D (D _ _) (D _ d)) = f (D (D x 1) 1) in d
  ```
][
  Hier ist eine #link("https://de.wikipedia.org/wiki/Differentialrechnung#Zusammenfassung")[Zusammenfassung der Ableitungsregeln].

  In @reverse_mode_ad_remark kannst du eine allgemeinere Funktion zum
  Berechnen der Ableitung sehen.
][
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
Typ (z.B. ```hs Int```) genutzt, für den es bereits vorimplementierte
Typklasseninstanzen gibt. Das ist meistens der Fall gewesen, wenn wir Gleichheit
auf Werten oder eine Vergleichsoperation auf Werten brauchten. Schau dir die
bisherigen Selbsttests gerne erneut an und überlege dir, wo du Typen
verallgemeinern kannst.

#test(level: 1)[
  Welche Funktionen musst du implementieren, damit eine ```hs Eq```-Instanz
  vollständig definiert ist? Welche Gesetze sollten die Funktionen einer
  ```hs Eq```-Instanz erfüllen?
]

#test(level: 2, clock: true, tags: (hl(),))[
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

#test(level: 2)[
  Welche Funktionen musst du implementieren, damit eine ```hs Ord```-Instanz
  vollständig definiert ist? Welche Gesetze sollten die Funktionen einer
  ```hs Ord```-Instanz erfüllen?
]

#test(level: 2, clock: true, tags: (hl(),))[
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

#test(level: 3, clock: true)[
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
    ineffizient -- die Vordefinierte ist es auch. Woran liegt das? Welche der
    beiden Funktionen würdest du implementieren, wenn du nur eine implementieren
    dürftest und es um die beste Laufzeit ginge?
][
  Standarddefinitionen sind für den Anfang hilfreich, um mit minimalem
  Aufwand alle Funktionen einer Typklasse verwenden zu können. Häufig findet
  man aber konkrete Implementierungen für mehr als nur die notwendingen
  Funktionen, da diese eine bessere Laufzeit haben.
]

#test(level: 2, clock: true)[
  In nicht streng getypten Programmiersprachen haben wir es oft mit impliziter
  Typkonversion zu tun. Implementiere eine Funktion ```hs ifThenElse```, die als
  Bedingung Werte beliebiger Typen entgegennehmen kann. Ziel ist es, dass der
  folgende Ausdruck ausgewertet werden kann.
  ```hs
  let a = ifThenElse 0 3 4
      b = ifThenElse [5] 6 7
      c = ifThenElse Nothing 8 9
   in a + b + c  -- 19
  ```
][
  Theoretisch könnten wir über eine Spracherweiterung des GHC sogar die
  #link("https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/rebindable_syntax.html")[Standardimplementierung von bedingten Ausdrücken ersetzen].
  Das wollen wir aber ganz schnell wieder vergessen, genauso wie den Inhalt
  dieses Tests, nachdem wir ihn bearbeitet haben.
]

#test(level: 2, clock: true)[
  Eine Halbgruppe ist eine Struktur $(H, ast.op)$, wobei $H$ eine Menge ist und
  $ast.op$ eine assoziative, binäre Verknüpfung $ast.op : H times H -> H$ ist.
  Ein Monoid erweitert die Halbgruppe um ein neutrales Element bzgl. $ast.op$.

  Definiere Typklassen ```hs Semigroup``` und ```hs Monoid```, die diese
  Strukturen implementieren. Gebe auch beispielhaft ein paar Instanzen für
  diese an.
][
  Diese Typklassen sind vorimplementiert in Haskell. Sie befinden sich in den
  Modulen #link("https://hackage-content.haskell.org/package/base/docs/Data-Semigroup.html")[```hs Data.Semigroup```]
  und #link("https://hackage-content.haskell.org/package/base/docs/Data-Monoid.html")[```hs Data.Monoid```].
]

#test(level: 1)[
  Wo findest du das Konzept der Typklassen in Programmiersprachen wie z.B.
  Python oder Java wieder? Gibt es z.B. ein Pendant zur ```hs Show```-Typklasse
  in diesen Programmiersprachen?
]

#test(level: 1)[
  Mit welcher Typklasse bzw. mit welcher Funktion können wir durch Strings
  repräsentierte Werte parsen?
]

#test(level: 2)[
  Gebe für die folgenden Funktionsdefinitionen den allgemeisten Typ an.
  - ```hs
    f x []     = [x]
    f x (y:ys)
      | x < y     = x : y : ys
      | otherwise = y : f x ys
    ```
  - ```hs f mmx = mmx >>= id```
  - ```hs f = (.) . (.)```
  - ```hs f = uncurry const```
]

#check[
  Ich bin in der Lage, ...
  - Typklasseninstanzen, ```hs Show```, ```hs Eq``` und ```hs Ord```, für
    Datentypen anzugeben,
  - ad-hoc Polymorphismus zu erklären und er sich von parametrischen
    Polymorphismus unterscheidet.
]


== Lazy Evaluation

#refs[
  - Skript: Funktionale Programmierung, Lazy Evaluation
]

#test(level: 1)[
  Was ist Lazy Evaluation?
]

#test(level: 1)[
  Wie werden Berechnungen in Haskell angestoßen? Wie viel wird berechnet?
]

#test(level: 1)[
  Gebe ein Beispiel an, das zeigt, dass die faule Auswertung berechnungsstärker
  ist.
]

#test(level: 1)[
  Welche praktischen Vorteile ergeben sich aus der Lazy Evaluation?
]

#test(level: 1)[
  Wie werden mehrfache Berechnungen in einer nicht-strikten Auswertungsstrategie
  vermieden?
]

#test(level: 1)[
  Gegeben sei folgender Haskell-Ausdruck.
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

#test(level: 1)[
  Wieso ist ```hs length [loop, loop, loop]``` berechenbar? ```hs loop```
  ist definiert durch
  ```hs
  loop :: a -> a
  loop = loop
  ```
]

#test(level: 2, clock: true)[
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
  ähnlich zum Python-Programm aussehen zu lassen. Verwende dafür auch den
  (polymorphen) Datentyp ```py Node```.
][
  Die Technik ist als
  #link("https://wiki.haskell.org/index.php?title=Tying_the_Knot")[Tying the Knot]
  bekannt.
]

#challenge(level: 2, clock: true)[
  Gegeben sei der Datentyp
  #align(center)[```hs data Tree a = Empty | Node (Tree a) a (Tree a)```.]

  - Implementiere eine Funktion ```hs preorder :: Tree a -> [a]```, die
    Knotenwerte in #link("https://de.wikipedia.org/wiki/Bin%C3%A4rbaum#Traversierung")[pre-order]
    zurückgibt. Das heißt, zuerst wird ein Knoten betrachtet und anschließend
    dessen linker und danach dessen rechter Teilbaum.
  - Implementiere einen unendlichen Baum ```hs tree :: Tree Int```, der die Menge
    $ { f(i, j) | i, j in NN, i <= j } "mit" f(i, j) = i + j + 2 i j $
    darstellt. Die Wurzel soll den Wert $f(1,1)$ haben. Für einen beliebigen
    Knoten mit Beschriftung $f(i,j)$  soll die Wurzel des linken Teilbaums mit
    $f(i+1,j)$ beschriftet sein und die Wurzel des rechten Teilbaums mit
    $f(i,j+1)$. Falls $i > j$ erreicht wird, soll in den Baum ein
    ```hs Empty```-Knoten gesetzt werden.
  - Wende ```hs preorder``` auf ```hs tree``` an. Welches Problem haben wir
    hinsichtlich der Werte, die wir den jeweiligen Teilbäumen von ```hs tree```
    sehen und der Ergbenisliste von ```hs preorder```? Wie hängt deine
    Beobachtung mit ```hs [f i j | i <- [1..], j <- [i..]]``` zusammen?
  - Implementiere eine Abwandlung von ```hs preorder```, die statt
    ```hs (++)``` die Funktion ```hs merge``` (aus z.B. mergesort) verwendet.
  - Implementiere als Nächstes die Mengendifferenz als Funktion
    ```hs diff :: Ord a => [a] -> [a] -> [a]```. Du darfst dabei annehmen, dass
    die Eingabelisten bereits sortiert sind.
  - Was berechnet ```hs 2 : map (\x -> 2 * x + 1) ([1..] `diff` preorder tree)```?
][
  Das Verfahren ist als #link("https://en.wikipedia.org/wiki/Sieve_of_Sundaram")[Sieb von Sundaram]
  bekannt. Die Konstruktion der oben angegebenen Menge mithilfe von
  unendlichen Bäumen ist nicht Teil des Verfahrens.
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

#test(level: 2, clock: true)[
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

#challenge(level: 2)[
  Wir können endliche Automaten als unendliche Bäume darstellen.
  Betrachte z.B. den endlichen Automaten für die reguläre Sprache $mono(a)^* mono(b)^*$.
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
    #align(center)[```hs data State a = State Bool [(a, State a)]```.] Der
    Boolean gibt an, ob der Zustand akzeptierend ist, und
    ```hs [(a, State a)]``` gibt die ausgehenden Transitionen an.
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

#test(level: 1)[
  Wieso können wir mit ```hs foldl``` auf unendlichen Listen mit keinem
  Ergebnis rechnen?
]

#test(level: 1)[
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

#challenge(level: 1, clock: true)[
  Fixpunktverfahren sind iterative Methoden, bei denen eine Funktion wiederholt
  auf einen Wert angewendet wird, bis sich ein stabiler Punkt (ein sogenannter
  Fixpunkt) ergibt, der sich durch weitere Anwendungen der Funktion nicht mehr
  verändert.

  Dieses Berechnungsmuster wird durch die Funktion
  ```hs iterate :: (a -> a) -> a -> [a]``` in der Prelude festgehalten.
  Sie berechnet eine unendliche Liste, bestehend aus den Ergebnissen der
  wiederholten Anwendungen der übergegebenen Funktion auf den gegebenen Wert.
  Das erste Ergebnis ist der gegebene Wert, auf den die Funktion noch nicht
  angewendet wurde.

  - Implementiere ```hs iterate```.
  - Ein klassisches Beispiel für ein Fixpunktverfahren aus der Numerik ist die
    Berechnung der Wurzel mithilfe des Heron-Verfahrens. Es ist gegeben durch
    $ x_0 = a, quad x_(n + 1) = 1/2 (x_n + a/x_n) $ Diese Folge nährt den Wert
    von $sqrt(a)$ mit jedem Folgeglied besser an. Implementiere das Verfahren
    mithilfe von ```hs iterate```. Wähle die erste Nährung $x_(n+1)$, die
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
    $ x_(k + 1) = x_k - 3 dot 10^(-4) dot f'(x_k) quad "für alle" k in NN $
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

#challenge(level: 1)[
  Eine Editierdistanz zwischen zwei Wörtern $u in Sigma^m, v in Sigma^n$ können
  wir mithilfe der folgenden Rekurrenz bestimmen:
  $
  "ed"(i, j) = cases(
    0 & quad "falls" (i, j) = (0, 0)\,,
    i & quad "falls" j = 0\,,
    j & quad "falls" i = 0\,,
    "ed"(i - 1, j - 1) & quad "falls" u_(i - 1) = v_(j - 1)\,,
    min("ed"(i - 1, j - 1), "ed"(i, j - 1), "ed"(i - 1, j)) + 1 & quad "sonst"
  )
  $
  für alle $0 <= i <= m, 0 <= j <= n$.
  Um $"ed"(m,n)$ effizient auszurechen, nutzt man dynamische Programmierung.
  Das heißt, wir merken uns die Zwischenergebnisse und nutzen diese, wenn wir
  sie erneut brauchen, anstatt sie neu zu berechnen und ohne die Optimalität
  des Ergebnisses zu gefährden.

  In Haskell können wir memoization mithilfe von Lazy Evaluation umsetzen. Hier
  ist ein unvollständiges Haskell-Programm, dass die Editierdistanz berechnen
  soll.
  ```hs
  editdist :: Eq a => [a] -> [a] -> Int
  editdist u v = table !! m !! n
    where
      (m, n) = (length u, length v)
      table = [[ed i j | j <- [0..n]] | i <- [0..m]]
  ```
  Die Berechnungen sind in ```hs table``` gespeichert.

  - Definiere die Funktion ```hs ed :: Int -> Int -> Int``` lokal in
    ```hs editdist```. ```hs ed``` soll hier die zwischengespeicherten
    Ergebnisse aus ```hs table``` verwenden und dadurch die Berechnungen
    anstoßen.
  - Überlege dir wie hier laziness und memoization zusammenspielen.
  - Welche worst-case Laufzeit hat deine Lösung, wenn du annimmst, dass die
    Laufzeit von ```hs (!!)``` konstant ist?
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

#challenge(level: 2, clock: true)[
  Bevor du dich dieser Challenge stellst, bietet es sich an, sich @editdist
  anzunehmen, da in dieser der technische Teil der Lösungsidee vorgestellt
  wird.

  Gegeben sei ein Gitter $G in ZZ^(m times n)$. Ein Pfad durch das Gitter
  startet oben links und endet unten rechts. In jedem Schritt kannst du von
  einer Zelle in die rechts- oder darunteranschließende Zelle gehen. Die
  Pfadsumme ist die Summe aller Zellenwerte, durch die der Pfad führt.

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
][][
  Die Rekursionsvorschrift ist gegeben durch
  $
  "pathsum"(i, j) = cases(
    G_(0,0) & quad "falls" (i, j) = (0, 0)\,,
    G_(i,0) + "pathsum"(i-1,0) & quad "falls" j = 0\,,
    G_(0,j) + "pathsum"(0,j-1) & quad "falls" i = 0\,,
    G_(i,j) + min("pathsum"(i-1,j), "pathsum"(i,j-1)) & quad "sonst"
  )
  $
  für $i in {0, ..., m - 1}, j in {0, ..., n - 1}$. Die minimale Pfadsumme
  ist dann $"pathsum"(m - 1, n - 1)$.
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

#check[
  Ich bin in der Lage, ...
  - lazy evaluation in Haskell zu erklären,
  - Funktionen auf unendliche Datenstrukturen zu definieren, und
  - unendliche Datenstrukturen mithilfe von Funktionen zu erzeugen.
]


== Sequenzen

#refs[
  - Skript: Funktionale Programmierung, Sequenzen
  - #link("https://learnyouahaskell.github.io/starting-out.html")[Starting Out -- Learn You a Haskell for Great Good!] ("Texas ranges")
]

#test(level: 1)[
  Ordne jeder der zur Typklasse ```hs Enum``` gehörenenden Funktionen eine
  äquivalente range zu, also Ausdrücke der Form ```hs [a,b..c]```.
  - ```hs enumFrom :: Enum a => a -> [a]```
  - ```hs enumFromThen :: Enum a => a -> a -> [a]```
  - ```hs enumFromTo :: Enum a => a -> a -> [a]```
  - ```hs enumFrom :: Enum a => a -> a -> a -> [a]```
]

#test(level: 1)[
  Um eine ```hs Enum```-Instanz anzugeben, benötigen wir mindestens Definitionen
  für ```hs fromEnum :: a -> Int``` und ```hs toEnum :: Int -> a```. Wie können
  wir aus denen, Definitionen für die übrigen Funktionen der Typklasse gewinnen?
]

#test(level: 1)[
  Gegeben sei der Datentyp
  #align(center)[```hs data Direction = North | East | South | West```.]

  - Implementiere eine ```hs Enum```-Instanz für ```hs Direction```, die sich
    wie die vom GHC abgeleitete Instanz verhält.
  - Implementiere eine ```hs Bounded```-Instanz für ```hs Direction```, die sich
    wie die vom GHC abgeleitete Instanz verhält.
  - Implementiere die Funktionen ```hs turnLeft :: Direction -> Direction```
    und ```hs turnRight :: Direction -> Direction```, die die Himmelsrichtungen
    entsprechend ihrer Bezeichnung durchgehen.
  - Implementiere eine Funktion ```hs allDirections :: [Direction]```, die
    alle Himmelsrichtungen auflistet. Nutze dafür Funktionen, die dir durch
    die vorherigen Typklassen bereitgestellt werden.
]

#test(level: 2)[
  Implementiere Funktion ```hs cycleFrom :: (Enum a, Bounded a) => a -> [a]```,
  die ab einem gegebenen Wert alle Werte des Typen nicht absteigend durchläuft.
  Wenn der größte Wert erreicht ist, soll die Liste wieder beim kleinsten Wert
  des Typen beginnen.
]

#check[
  Ich bin in der Lage, ...
  - zu erklären, wie die Typklassen ```hs Enum``` und ```hs Bounded``` und
    list comprehensions zusammenhängen.
]


== List Comprehensions

#refs[
  - Skript: Funktionale Programmierung, List Comprehensions
  - #link("https://learnyouahaskell.github.io/starting-out.html")[Starting Out -- Learn You a Haskell for Great Good!] ("I'm a list comprehension")
]

#test(level: 1)[
  An welches mathematische Konzept sind list comprehensions angelehnt?
]

#test(level: 1)[
  Aus welchen Teilen besteht eine list comprehension?
]

#test(level: 1)[
  Implementiere die Funktionen ```hs map```, ```hs filter``` und
  ```hs concatMap``` mithilfe von list comprehensions (sowohl in Haskell als
  auch Python).
]

#test(level: 2)[
  Übersetze die gegebenen Funktionen in eine äquivalente Funktion, die keine
  list comprehensions verwendet.
  - ```hs f xs = [x * 2 | x <- xs, x > 0]```
  - ```hs f xs ys = [x + y | x <- xs, y <- ys]```
  - ```hs f xs ys = [(x, y) | x <- xs, y <- ys, x < y]```
  - ```hs f xss = [x | xs <- xss, length xs > 2, x <- xs, even x]```
  - ```hs f xs = [(x, y) | x <- xs, y <- [1..x], even (x + y)]```
  - ```hs f xs = [(x, y) | x <- xs, let y = x * x, y `mod` 3 == 0]```
  Versuche diese Aufgabe mit und ohne Listenmonade zu lösen.
]

// ```hs
// -- f xs = [x * 2 | x <- xs, x > 0]
//
// f xs = map (*2) (filter (> 0) xs)
// f xs = do
//   x <- xs
//   guard (x > 0)
//   return (x * 2)
//
//
// -- f xs ys = [x + y | x <- xs, y <- ys]
//
// f xs ys = concatMap (\x -> map (x +) ys) xs
// f xs ys = do
//   x <- xs
//   y <- ys
//   return (x + y)
//
//
// -- f xs ys = [(x, y) | x <- xs, y <- ys, x < y]
//
// f xs ys = filter (<) (concatMap (\x -> map (x, ) ys) xs)
// f xs ys = filter (<) ((,) <$> xs <*> ys)
// f xs ys = do
//   x <- xs
//   y <- ys
//   guard (x < y)
//   return (x, y)
//
//
// -- f xss = [x | xs <- xss, length xs > 2, x <- xs, even x]
//
// f xss = concatMap (filter even) (filter (\xs -> length xs > 2) xss)
// f xss = do
//   xs <- xss
//   guard (length xs > 2)
//   x <- xs
//   guard (even x)
//   return x
//
//
// -- f xs = [(x, y) | x <- xs, y <- [1..x], even (x + y)]
//
// f xs = filter (even . uncurry (+)) (concatMap (\x -> map (x, ) [1..x]) xs)
// f xs = do
//   x <- xs
//   y <- [1..x]
//   guard (even (x + y))
//   return (x, y)
//
//
// -- f xs = [(x, y) | x <- xs, let y = x * x, y `mod` 3 == 0]
//
// f xs = filter (\(_, y) -> y `mod` 3 == 0) (map (\x -> (x, x * x)) xs)
// f xs = do
//   x <- xs
//   let y = x * x
//   guard (y `mod` 3 == 0)
//   return (x, y)
// ```

#check[
  Ich bin in der Lage, ...
  - List Comprehensions konzeptionell zu verstehen und korrekt zu anwenden,
    einschließlich ihres mathematischen Hintergrunds, und
  - Listenverarbeitungen mit List Comprehensions auszudrücken und äquivalent
    umzuformen, auch bei komplexeren Konstruktionen.
]


== Ein- und Ausgabe

#refs[
  - Skript: Funktionale Programmierung, Ein- und Ausgabe
  - #link("https://learnyouahaskell.github.io/input-and-output.html")[Input and Output -- Learn You a Haskell for Great Good!]
]

#test(level: 1)[
  Was ist referenzielle Transparenz?
]

#test(level: 1)[
  Welche Rolle spielt der Typ ```hs IO a``` bzgl. Seiteneffekte?
  Was beschreibt ein Wert vom Typ ```hs IO a```?
]

#test(level: 1)[
  Wie können wir zwei ```hs IO```-Aktionen zu einer neuen ```hs IO```-Aktion
  kombinieren?
]

#test(level: 3)[
  Betrachte die ```hs IO```-Aktion ```hs act1 >> act2```.
  - Welche der beiden Aktionen wird zuerst ausgeführt?
  - Warum erscheint das bei Lazy Evaluation kontraintuitiv?
  - Welche Rolle spielt der Typ ```hs RealWorld -> (RealWorld, a)``` bei der Sequenzierung?
  - Wieso können ```hs IO```-Berechnung in Haskell als "pure" betrachtet werden?
]

#test(level: 1)[
  Mit ```hs getLine :: IO String``` können Zeilen aus der Standardeingabe
  gelesen werden. Oft wollen wir den Wert haben, der durch die eingebene
  Zeichenkette repräsentiert wird. Wie können wir diesen Wert erhalten?

  Implementiere eine Funktion ```hs readInt :: IO Int```, die genau dies tut.
]

#test(level: 1)[
  Implementiere ein Haskell-Programm, dass eine Eingabe wörterweise liest.
  Dabei soll gezählt werden, wie viele abwechselnde Vorkommen es der Wörter
  "Liebe" und "Harry" als Teilfolge in der Eingabe gibt. Jedes der Wörter soll
  einzeln gezählt werden. Zum Beispiel soll das Programm für folgende Eingabe
  #align(center)[
    #link("https://www.youtube.com/watch?v=ZVEEJ267atw")[Liebe Harry Liebe] Liebe Voldemort
  ]
  3 ausgeben.
]

#test(level: 2)[
  Implementiere ein Programm, das Zahlen aus einer Datei aufsummiert, bzw.
  implementiere eine Funktion ```hs sumFile :: FilePath -> IO Int```.
  In jeder Zeile einer Datei steht eine nicht-negative Zahl.
]

#test(level: 2, clock: true)[
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

#check[
  Ich bin in der Lage, ...
  - referenzielle Transparenz und Seiteneffekte konzeptionell zu erklären und
    die Rolle des Typs ```hs IO a``` für die Modellierung von Effekten in
    Haskell einzuordnen,
  - IO-Aktionen korrekt zu kombinieren, zu sequenzieren und auszuwerten, auch
    im Kontext von Lazy Evaluation und der zugrunde liegenden
    Weltzustands-Semantik,
  - einfache bis mittelkomplexe IO-Programme entwerfen und implementieren,
    die Ein-/Ausgabe, Dateizugriff, Verarbeitung von Eingaben und kontrollierte
    Interaktion enthalten.
]


== Funktoren, Applicatives, Monaden

#refs[
  - Skript: Funktionale Programmierung, Funktoren und Monaden
  - #link("https://learnyouahaskell.github.io/functors-applicative-functors-and-monoids.html")[Functors, Applicative Functors and Monoids -- Learn You a Haskell for Great Good!] (ohne "Monoids")
  - #link("https://learnyouahaskell.github.io/a-fistful-of-monads.html")[A Fistful of Monads -- Learn You a Haskell for Great Good!]
  - #link("https://www.youtube.com/watch?v=C2w45qRc3aU")[The Absolute Best Intro to Monads For Software Engineers -- YouTube] (nutzt TypeScript, aber die Ideen bleiben die gleichen)

  #text(0.8em)[
    Wenn du Zeit übrig hast und es dich interessiert: Die Monoiden-Sektion ist
    interessent.
  ]
]

#test(level: 1)[
  Was ist der Unterschied zwischen Typklassen und Typkonstruktorklassen?
  Gebe Beispiele für beide an.
]

#test(level: 1)[
  Gegeben sei folgende Typkonstruktorklasse.
  ```hs
  class TCC t where
    f :: t a b -> b
  ```
  Für welche Datentypen können wir Instanzen von ```hs TCC``` angeben?
  Beantworte die Frage möglichst allgemein.
]

#test(level: 1)[
  Wie lauten die ```hs Functor```-Gesetze?
]

#test(level: 1)[
  Die Funktor-Typkonstruktorklasse ist wie folgt definiert.
  ```hs
  class Functor f where
    fmap :: (a -> b) -> f a -> f b
  ```
  Welchen kind hat ```hs f```?
]

#test(level: 1)[
  Wie sind die ```hs Functor```-Instanzen für ```hs Identity```, ```hs Maybe```,
  ```hs Either e```, ```hs []``` und ```hs ((->) r)``` definiert?
]

#test(level: 1)[
  Wie lauten die ```hs Applicative```-Gesetze?
]

#test(level: 1)[
  Wie sind die ```hs Applicative```-Instanzen für ```hs Identity```, ```hs Maybe```,
  ```hs Either e```, ```hs []``` und ```hs ((->) r)``` definiert?
]

#test(level: 1)[
  Wie lauten die ```hs Monad```-Gesetze?
]

#test(level: 1)[
  Wie sind die ```hs Monad```-Instanzen für ```hs Identity```, ```hs Maybe```,
  ```hs Either e```, ```hs []``` und ```hs ((->) r)``` definiert?
]

#test(level: 2, clock: true)[
  Die ```hs Applicative```-Typkonstruktorklasse erlaubt es uns, ```hs fmap```
  auf Funktionen mit mehreren Argumenten zu verallgemeinern. Dadurch können wir
  #align(center)[
    ```hs (+) <$> Just 1 <*> Just 2``` #h(1em) oder #h(1em) ```hs Just (+) <*> Just 1 <*> Just 2```
  ]
  schreiben. Die Operatoren ```hs (<$>)``` und ```hs (<*>)``` funktionieren
  dabei ähnlich wie ```hs ($)``` -- mit ```hs (<$>)``` muss die Funktion nicht
  explizit in den entsprechenden ```hs Applicative``` gehoben werden.

  Ein Typ, der uns konzeptionell auf Ebene der applikativen Funktoren näher an
  die gewöhnliche Funktionsapplikation heranführt, ist
  #align(center)[```hs newtype Identity a = Identity { runIdentity :: a }```.]
  Implementiere ```hs Functor```-, ```hs Applicative```- und
  ```hs Monad```-Instanzen für ```hs Identity```.

  Wenn du die Instanzen definierst, solltest du feststellen, dass du im
  Wesentlichen nur den enthaltenden Wert aus der ```hs Identity``` holst,
  verarbeitest und anschließend wieder hereinpackst.
]

#test(level: 1)[
  Gegeben sei der Datentyp
  #align(center)[```hs data NonEmpty a = a :| [a]```.]
  Gebe ```hs Functor```-, ```hs Applicative```- und ```hs Monad```-Instanzen
  für diesen Typen an.
]

#test(level: 1)[
  Um zu verifizieren, dass die ```hs Functor```-Gesetze für z.B. den Typ
  ```hs Maybe a``` gelten, müssen wir
  - das Identitätsgesetz ```hs fmap id = id``` und
  - das Kompositionsgesetz ```hs fmap f . fmap g = fmap (f . g)```
  zeigen. Wie gehen wir konkret für den gegebenen Typ vor? Wie zeigen wir
  die Gleichheit von Funktionen? Wenn die Gesetze nun für den Listendatentypen
  ```hs [a]``` zeigen wollen, was ändert sich an deinem Vorgehen?
]

#test(level: 2)[
  Warum gilt
  #align(center)[```hs (1 +) <$> Just 1 == Just (1 +) <*> Just 1 == pure (1 +) <*> Just 1```?]
  Wie ergibt sich aus deinen Beobachtungen eine Definition für ```hs fmap```?
  Wie können wir die Berechnung für beliebige applikative Funktoren
  verallgemeinern? Wie wird dann festgelegt, was während der Berechnung
  tatsächlich passiert?
]

#test(level: 2)[
  Monaden sind ausdrucksstärker als applikative Funktoren, und applikative
  Funktoren sind ausdrucksstärker als Funktoren.
  - Implementiere ```hs fmap```, ```hs pure``` und ```hs (<*>)``` mithilfe von
    ```hs return``` und ```hs (>>=)```.
  - Implementiere ```hs fmap``` mithilfe von ```hs pure``` und ```hs (<*>)```.
][
  Die Erkenntnis dieses Tests kannst du nutzen, wenn es für dich einfacher
  ist eine Monaden-Instanz anzugeben, anstatt Funktoren- und applikative
  Funktor-Instanzen anzugeben oder um Zeit zu sparen.
]

#test(level: 1)[
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

#test(level: 1)[
  Gegeben sei diese fehlerhafe Definition einer sicheren Division:
  ```hs
  safeDiv :: Int -> Int -> Maybe Int
  safeDiv x y = Just (div x) <*> Just y
  ```
  Wie können wir diese Implementierung reparieren, ohne ```hs (>>=)``` zu
  verwenden und die rechte Seite soweit wie möglich zu erhalten?
  Wie siehst du an diesem Beispiel, dass Monaden ausdrucksstärker als
  applikative Funktoren sind?
]

// ```hs
// safeDiv :: Int -> Int -> Maybe Int
// safeDiv x y = pure (div x) <*> if y == 0 then Nothing else pure y
// -- oder: safeDiv x y = pure (div x) <*> (guard (y /= 0) *> pure y)
// ```

#test(level: 2)[
  Das Sequenzieren von Berechnung haben wir bereits in einfacheren Form als
  Funktionskomposition kennengelernt. Wenn wir die Fortsetzung einer Berechnung
  im Kontext von Monaden betrachten, dann stellen wir fest, dass sich zwei
  Funktionen des Typs ```hs Monad m => a -> m b``` nicht im Sinne des
  ```hs (>>=)```-Operators hintereinanderausführen lassen.

  Finde ein geeignetes Beispiel das zeigt, dass dieses Szenario illustriert.
]

#test(level: 1)[
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

#test(level: 1)[
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

#test(level: 2)[
  Die ```hs Reader```-Monade ermöglicht es, eine gemeinsame Umgebung mit vielen
  Berechnungen zu teilen.
  ```hs
  newtype Reader r a = Reader { runReader :: r -> a }

  instance Monad (Reader r) where
    return x = Reader (\_ -> x)

    r >>= k = Reader (\s -> let x = runReader r s
                                y = runReader (k y) s
                             in y)
  ```
  Woran kannst du erkennen, dass die Berechnung ```hs r``` vor dessen
  Weiterführung mit ```hs k``` und dem Ergebnis ```hs r``` stattfindet?
] <reader_monad>

#test(level: 3, clock: true)[
  Das Pendant zur ```hs Reader```-Monade aus @reader_monad ist die
  ```hs Writer```-Monade. Eine Spezialisierung der Monade soll hier die
  ```hs ListWriter```-Monade sein. Diese Monade kann genutzt werden, um
  Zwischenergebnisse oder Logging-Informationen einer Berechnung zu speichern.
  #align(center)[
    ```hs
    newtype ListWriter w a = ListWriter { runListWriter :: (a, [w]) }
    ```
  ]
  Implementiere diese Monade. Mache dir insbesondere Gedanken darüber, wie du
  Berechnungen dieses Typens sequenzierst und wie du die Zwischenergebnisse
  von zwei Berechnungen kombinierst.
][][
  Die Implementierung dieser Monade ist sehr ähnlich zu der Implementierung
  der ```hs Reader```-Monade, wie sie in @reader_monad angegeben ist.
]

#test(level: 1)[
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

#test(level: 2, clock: true)[
  Das Verwenden von monadischen Funktionen kommt einem zum Anfang möglicherweise
  als erbitterter Kampf gegen das Typsystem vor. Diese Situationen ergeben sich
  bereits aus den scheinbar unschuldigsten Absichten. Oft fehlt dann die
  Erfahrung, um die geeignete Funktion auszuwählen bzw. sogar die Kenntnis
  darüber, dass es Funktionen gibt, die einem helfen könnten. Hier betrachten
  wir ein paar solcher Szenarien.

  - Angenommen du sollst eine Funktion ```hs putStrs :: [String] -> IO ()```
    implementieren, die eine Liste von Strings zeilenweise ausgeben soll, dann
    hast du die Möglichkeit, naiv und zielführend die Funktion per Induktion zu
    definieren. Als Nächstes könntest du die Beobachtung machen, dass du die
    gleiche Funktion für jeden String anwenden sollst, was dich an ```hs map```
    erinnern könnte. Du erhältst den folgenden typinkorrekten Code.
    ```hs
    putStrs :: [String] -> IO ()
    putStrs ss = map putStrLn ss  -- :: [IO ()]
    ```
    An der Stelle wirfst du entweder den Ansatz über den Haufen oder überlegst
    dir, wie eine Funktion vom Typ ```hs [IO ()] -> IO ()``` definiert sein
    könnte, um den obigen Code zu reparieren. Implementiere allgemeiner eine
    Funktion ```hs sequence :: Monad m => [m a] -> m [a]``` bzw. eine Funktion
    ```hs sequence_ :: Monad m => [m ()] -> m ()```, um die obige
    Implementierung von ```hs putStrs``` zu reparieren.
  - Implementiere, basierend auf der vorherigen Teilaufgabe, eine Funktion
    ```hs mapM :: (a -> m b) -> [a] -> m [b]```. (Ein entsprechendes
    ```hs mapM_``` kannst du auch implementieren.)
  - Andere hilfreiche Funktionen sind
    - ```hs replicateM :: Monad m => Int -> m a -> m [a]```, die eine monadische
      Aktion eine feste Anzahl von Malen ausführt und dann eine Liste der
      Ergebnisse ausgibt.
    - ```hs zipWithM :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m [c]```,
      die ```hs zipWith``` auf Monaden verallgemeinert,
    - ```hs join :: Monad m => m (m a) -> m a```, die eine monadische Struktur
      flacher klopft,
    - und #link("https://hackage-content.haskell.org/package/base/docs/Control-Monad.html")[viele weitere]!
    Implementiere die konkret genannten Funktionen.
  - Implementiere eine Funktion ```hs Int -> String -> IO ()```, die einen
    String beliebig häufig ausgibt.
  - Was ist ```hs zipWithM safeDiv [1, 2, 3, 4] [0, 1, 0, 1]```, wobei
    ```hs safeDiv``` wie folgt definiert sei.
    ```hs
    safeDiv :: Integral a => a -> a -> Maybe a
    safeDiv _ 0 = Nothing
    safeDiv p q = Just (p `div` q)
    ```
  - Was sind
    - ```hs join [[1, 2, 3], [4, 5], [6]]```,
    - ```hs join (Just Nothing)``` und
    - ```hs join (Left (Right True))```?
]

#challenge(level: 2, clock: true)[
  Gegeben sei der Datentyp
  #align(center)[
    ```hs newtype ZipList a = ZipList { getZipList :: [a] }```.
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
    keine gültige Definition ist? Welche Gesetze wären verletzt und warum, würde
    man ```hs pure``` so definieren?
  - Implementiere eine ```hs Applicative```-Instanz für ```hs ZipList```.
  - Zeige, dass sowohl die ```hs Functor```- als auch die
    ```hs Applicative```-Instanz die üblichen geforderten Gesetze erfüllen.
][
  Das Umwickeln eines Typen mit ```hs newtype```, für den wir bereits
  Typklasseninstanzen haben, ist ein gängiger Trick, um alternative Instanzen
  für diese Typklassen bereitzustellen.
][
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

#test(level: 3, clock: true)[
  ```hs guard :: MonadZero m => Bool -> m ()``` kann genutzt werden, um eine
  Berechnung bedingt fehlschlagen zu lassen.
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
    Zu ihr gehören die folgenden Gesetze.
    ```hs
    mzero >>= f     = mzero
    m     >>= mzero = mzero
    ```
    Damit verhält sich ```hs mzero``` wie eine "monadische Null" bzw.
    absorbierend bzgl. der Operationen ```hs (>>=)``` und ```hs (>>)```.
    Betrachte den folgenden Ausdruck.
    #block[```hs m1 >> ... >> mzero >> ... >> mn```]
    Was ist das Ergebnis dieses Ausdrucks?
  - Implementiere ```hs guard``` mit den Gedanken der vorherigen Teilaufgabe.
  - Berechne ```hs 1 `safeDiv` 0 :: m Int``` für ```hs Maybe``` und ```hs []```.
    Bevor das möglich ist, benötigst du entsprechende ```hs MonadZero```-Instanzen.
][
  ```hs guard``` ist auf Basis von ```hs Alternative``` bzw.
  ```hs MonadPlus``` implementiert. ```hs MonadZero``` ist nicht Teil der
  Standardbibliothek, aber es ist definiert als Teil von ```hs MonadPlus```.
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

#test(level: 2)[
  In @monadzero haben wir eine Typkonstruktorklasse definiert, die es uns
  erlaubt hat, einen Fehlschlag bzw. die Abwesenheit eines Ergebnisses
  auszudrücken. Diese können wir auch eine Abstraktionsebene früher einführen.
  Definiere eine weitere Typkonstruktorklasse ```hs AlternativeZero``` auf
  Ebene der applikativen Funktoren mit einer Funktion ```hs empty :: f a```.

  Folgende Ausdrücken sollen die gegebenen Werte haben:
  - ```hs guard (y /= 0) *> pure y = pure y``` für ```hs y /= 0``` und
  - ```hs guard (y /= 0) *> pure y = empty``` für ```hs y == 0```.

  ```hs (*>) :: Applicative f => f a -> f b -> f b``` ist ein Kombinator, der
  sich wie ```hs (>>)``` für applikative Funktoren verhält.
]

#test(level: 3, clock: true)[
  Als motivierendes Beispiel für Monaden hast du die Auswertung eines
  arithmetischen Ausdrucks, gegeben als Termstruktur, kennengelernt. Dort haben
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
    die es erlaubt, eine Beschreibung des Fehlschags anzugeben. Sie soll dafür
    eine Funktion ```hs fail :: String -> m a``` definieren. Verallgemeinere
    ```hs eval``` erneut mit der neuen Typkonstruktorklasse. Gebe zusätzlich
    eine Instanz für den Typ ```hs Either String``` an -- alternativ,
    implementiere eine ```hs Functor```-, ```hs Applicative```-, ```hs Monad```-
    und ```hs MonadFail```-Instanz für den Typen
    ```hs data Result a = Failure String | Success a```.
][
  Der applikativen Funktor wird durch die Verwendung von Hilfsfunktion nicht
  ausdrucksstärker! Das, was der applikative Funktor nicht leisten kann, wird
  in die Hilfsfunktion ausgelagert. Teile der Hilfsfunktion werden durch die
  Monade übernommen.
] <eval_exp>

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

#test(level: 2)[
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

#test(level: 2, clock: true)[
  Die ```hs do```-Notation erlaubt es uns, statements der Form ```hs p <- e```
  zu schreiben, wobei ```hs p``` ein beliebiges Muster sein kann. Zum Beispiel
  ist folgender Ausdruck valide und berechnenbar.
  ```hs
  do
    Just x <- [Just 1, Nothing, Just 2]
    return x
  ```
  Der Wert dieses Ausdrucks is ```hs [1, 2]```.
  - In welches Problem läufst du, wenn du diesen Ausdruck mithilfe von
    ```hs (>>=)``` und ```hs (>>)``` ausdrücken möchtest?
  - Wie kannst du das Problem beheben in diesem konkreten Fall beheben?
  - Fällt dir möglicherweise ein allgemeine Übersetzungsvorschrift für
    statements dieser Form unter der Verwendung von ```hs MonadFail``` aus
    @eval_exp ein?
][
  Der Haskell Report spezifiziert in der
  #link("https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-470003.14")[Sektion über die ```hs do```-Notation],
  wie diese übersetzt wird.
]

#test(level: 1)[
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

#test(level: 1)[
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

#test(level: 1)[
  Gegeben sei folgendes Haskell-Programm.
  ```hs
  main :: IO ()
  main = do
    putStr "Hello"
    return ()
    putStrLn ", world!"
    return ()
  ```
  Was ist die Ausgabe des Programm und warum? Wie steht das im Konflikt mit dem,
  was du aus imperativen Programmiersprachen kennst?
]

#test(level: 1)[
  Gegeben sei folgendes fehlerhafte Haskell-Programm.
  ```hs
  main :: IO ()
  main = do
    q <- read <$> getLine
    if q == 0
      then main
      else
        p <- read <$> getLine
        print (p `div` q)
  ```
  Warum ist das Programm fehlerhaft? Um den Fehler zu identifizieren, klammere
  alle validen Haskell-Teilausdrücke. (Du kannst davon ausgehen, dass alle
  Eingaben valide sind und deshalb keine weitere Fehlerbehandlung der Eingaben
  stattfinden muss.)
]

#test(level: 2, clock: true)[
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

#test(level: 2)[
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

#test(level: 3, clock: true)[
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
][
  Diese Implementierung von ```hs sequence``` ist ein Spezialfall für die
  #link("https://learnyouahaskell.github.io/for-a-few-monads-more.html#state")[```hs State```-Monade].
  Mit der Intuition, dass wir hier Berechnungen sequenzieren, sollte es nicht
  überraschend sein, dass ```hs s -> (a, s)``` eine Monade ist. Alternativ
  kannst du die ```hs State```-Monade implementieren und das vorimplementierte
  (oder von dir implementierte) Funktion ```hs sequence``` verwenden.
] <sequence_state>

#challenge(level: 2, clock: true)[
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
][
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

#check[
  Ich bin in der Lage, ...
  - Funktoren zu definieren, die die Funktor-Gesetze erfüllen und das auch
    beweisen,
  - applikative Funktoren zu definieren, die die Applicative-Gesetze erfüllen
    und das auch beweisen,
  - Monaden zu definieren, die die Monaden-Gesetze erfüllen und das auch
    beweisen,
  - Funktoren, Applicatives und Monaden zu nutzen,
  - ```hs do```-Notation zu nutzen, und
  - zwischen ```hs (>>=), (>>)``` und ```hs do```-Notation zu übersetzen.
]


== Automatisches Testdatengenerierung

#refs[
  - Skript: Funktionale Programmierung, Automatisches Testdatengenerierung
  - #link("https://dl.acm.org/doi/10.1145/351240.351266")[QuickCheck: a lightweight tool for random testing of Haskell programs]
]

#test(level: 2)[
  Es kommt häufiger vor, dass zufällig generierte Werte bestimmte Eigenschaften
  erfüllen sollen. QuickCheck bietet auf Generatoren-Ebene z.B. die Funktion
  ```hs suchThat :: Gen a -> (a -> Bool) -> Gen a``` dafür an. Diese nimmt einen
  Generator und generiert solange neue Werte, bis eine gewünschte Eigenschaft
  erfüllt ist. Diese Eigenschaft wird durch ein Prädikat formuliert.

  Implementiere ```hs suchThat```.
][
  Es gibt ein paar QuickCheck-spezifische Fallstricke, die mit dem
  ```hs size```-Parameter der Generatoren zu tun haben, die du bei deiner
  Implementierung nicht beachten brauchst. Falls es dich interessiert, ist
  hier die
  #link("https://hackage-content.haskell.org/package/QuickCheck/docs/src/Test.QuickCheck.Gen.html#suchThat")[```hs suchThat```-Implementierung von QuickCheck].
]

// ```hs
// suchThat :: Gen a -> (a -> Bool) -> Gen a
// suchThat g p = g >>= \x -> if p x
//                              then return x
//                              else g `suchThat` p
// ```

#test(level: 2)[
  Wenn du folgende (etwas künstliche) Eigenschaft mithilfe von QuickCheck
  prüfst, muss im Durchschnitt jeder zweite Test verworfen werden, weil die
  Vorbedingung der Eigenschaft nicht erfüllt ist.
  ```hs
  prop_prop :: Int -> Property
  prop_prop k = k > 0 ==> True
  ```
  Um dieses Verhalten zu unterbinden, können wir die Eigenschaft leicht
  abändern.
  ```hs
  prop_prop :: Positive Int -> Property
  prop_prop (Positive k) = k > 0 ==> True
  ```
  Jetzt werden keine Testeingaben verworfen.

  Überlege dir, wie die Lösung des Problems funktioniert und implementiere sie.
][
  QuickCheck hat viele solche
  #link("https://hackage-content.haskell.org/package/QuickCheck/docs/Test-QuickCheck.html")[type-level modifiers],
  die das Generator-Verhalten verändern.
]

// ```hs
// newtype Positive a = Positive { getPositive :: a }
//
// instance (Arbitrary a, Num a) => Arbitrary (Positive a) where
//   arbitrary = Positive . (+ 1) . abs <$> arbitrary
//   -- or
//   -- arbitrary = fmap Positive (fmap abs arbitrary `suchThat` (> 0))
// ```

Die folgenden zwei Challenges steigen tiefer in die Funktionsweise von
QuickCheck ein. Dabei werden zwar weiter viele Vereinfachungen gemacht, wir
nähern uns dennoch der tatsächlichen Implementierung von QuickCheck stark an.

#challenge(level: 3, clock: true)[
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
    ```hs Testable```, wie sie in QuickCheck zu finden ist. Hier ist Typ
    ```hs Property``` ein einfacher Wrapper um den ```hs Bool```-Typ. In
    QuickCheck ist dieser ein bisschen komplexer.
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
][
  In QuickCheck werden Generatoren genutzt, um zufällige Werte zu generieren.
  Diese nutzen alle einen Zufallszahlengenerator, der als Zustand mit
  weiteren Parametern durch alle ```hs arbitrary```-Aufrufe durchgetragen wird.
  Das behandeln wir in der @quickcheck_prng, damit diese Challenge nicht zu
  sehr ausartet.
] <quickcheck_noprng>

// ```hs
// class Arbitrary a where
//   arbitrary :: a
//
// instance Arbitrary Int where
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
// quickCheck :: Testable a => a -> Bool
// quickCheck p = unProperty (property p)
// ```

#challenge(level: 3, clock: true, breakable: true)[
  Bevor du diese Challenge bestreitest, sage @quickcheck_noprng den Kampf an!

  Während QuickCheck #link("https://dl.acm.org/doi/10.1145/2660193.2660195")[SplitMix64]
  als Pseudozufallszahlengenerator nutzt, nutzen wir ein einfacheres Verfahren,
  um solche Zahlen zu erzeugen.
  #link("https://en.wikipedia.org/wiki/Xorshift")[Xorshift] erzeugt
  Pseudozufallszahlen nach dem folgenden Schema:
  $
  x &<- x plus.circle (x << 13) \
  x &<- x plus.circle (x >> 7) \
  x &<- x plus.circle (x << 17) \
  $
  wobei $x$ eine 64-Bit Ganzzahl ist.

  - Implementiere eine Funktion ```hs xorshift64 :: Word64 -> Word64```, die das
    obige Verfahren umsetzt. Folgende imports könnten hilfreich sein. Werte vom
    Typ ```hs Word64``` können direkt mit den Bit-Operationen verwendet werden.
    Wir nutzen den Typ ```hs Word64```, weil für diesen im Gegensatz zum
    ```hs Int``` garantiert ist, dass dieser 64-Bit hat. Wir gehen danach aber
    wieder direkt zum Typ ```hs Int```.
    ```hs
    import Data.Bits (shiftL, shiftR, xor)
    import Data.Word (Word64)
    ```

  Eine zufällige Ganzzahl kann nun mithilfe der Funktion ```hs nextInt```
  generiert werden. Ein Zufallszahlengenerator kann mit der Funktion
  ```hs mkPRNG``` erzeugt werden. Dieser muss mit einer Zahl (seed)
  initialisiert werden.
  ```hs
  data PRNG = PRNG Word64
    deriving Show

  mkPRNG :: Word64 -> PRNG
  mkPRNG s = PRNG (xorshift64 s)

  nextInt :: PRNG -> (Int, PRNG)
  nextInt (PRNG s) = (fromIntegral r, PRNG r)
    where r = xorshift64 s
  ```

  Damit der Zufallsgenerator bei der Definition von ```hs arbitrary``` zur
  Verfügung steht, passen wir die Definition der zugehörigen Typklasse an.
  Weiter erhält diese auch einen Größen-Parameter, um die generierten
  Zufallswerte in ihrer Größe zu kontrollieren. Andere Definitionen aus der
  vorherigen Challenge müssen wir auch anpassen.

  ```hs
  newtype Property = Property { unProperty :: Gen Bool }

  newtype Gen a = Gen { unGen :: PRNG -> Int -> (a, PRNG) }

  class Arbitrary a where
    arbitrary :: Gen a
  ```

  - Passe deine alte Implementierung aus @quickcheck_noprng bzgl. der neuen
    Typen an.
  - Definiere ```hs Functor```-, ```hs Applicative```- und ```hs Monad```-Instanzen
    für den Typkonstruktor ```hs Gen```. (Hier passiert im Wesentlichen etwas
    Ähnliches wie in @sequence_state.)
  - Definiere eine Funktion ```hs chooseInt :: (Int, Int) -> Gen Int```, die
    eine zufällige Ganzzahl im übergebenen Intervall generiert. Um eine
    zufällige Zahl $x in ZZ$ in ein Intervall $[l, r)$ zu zwingen, können wir
    z.B. $l + x "mod" (r - l)$ rechnen. Beachte, dass ```hs mod``` auch negativ
    sein kann.
  - Definiere eine ```hs Arbitrary```-Instanz für den Typ ```hs Int```. Dabei
    soll eine generierte Ganzzahl im Intervall ```hs (-s, s)``` liegen, wobei
    ```hs s``` der übergegebene Größen-Parameter der Generator-Funktion ist.
  - Passe ```hs quickCheck``` so an, dass ein Zufallsgenerator und ein
    Größenwert an die Generator-Funktionen weitergegeben wird.
][
  Die Vereinfachungen, die wir gemacht haben:
  - einfacheren PRNG nutzen (anstatt SplitMix64)
  - in ```hs Gen``` wird in QuickCheck der PRNG gesplittet, anstatt dass
    er durchgereicht wird
  - ```hs chooseInt```, so wie vorgeschlagen, erzeugt keine gleichverteilten
    Zufallswerte in dem Intervall. Für Genaueres:
    - #link("https://stackoverflow.com/questions/2509679/how-to-generate-a-random-integer-number-from-within-a-range")[How to generate a random integer number from within a range -- StackOverflow]
    - #link("https://hackage-content.haskell.org/package/splitmix/docs/src/System.Random.SplitMix.html#nextInteger")[```hs nextInteger```] aus ```hs System.Random.SplitMax```
  - wir haben einen festen seed gewählt (in SplitMix64 wird
    #link("https://hackage-content.haskell.org/package/splitmix/docs/src/System.Random.SplitMix.Init.html#initialSeed")[initialSeed]
    basierend auf der aktuellen Uhrzeit gesetzt, damit immer neue zufällige
    Zahlen generiert werden, wenn das Programm erneut gestartet wird)
  - und vieles mehr!
] <quickcheck_prng>

// ```hs
// import Data.Word (Word64)
// import Data.Bits (shiftL, shiftR, xor)
//
// data PRNG = PRNG Word64
//   deriving Show
//
// nextInt :: PRNG -> (Int, PRNG)
// nextInt (PRNG s) = (fromIntegral z, PRNG z)
//   where z = xorshift64 s
//
// xorshift64 :: Word64 -> Word64
// xorshift64 z0 = z3
//   where
//     z1 = z0 `xor` (z0 `shiftL` 13)
//     z2 = z1 `xor` (z1 `shiftR` 7)
//     z3 = z2 `xor` (z2 `shiftL` 17)
//
// mkPRNG :: Word64 -> PRNG
// mkPRNG s = PRNG (xorshift64 s)
//
// newtype Gen a = Gen { unGen :: PRNG -> Int -> (a, PRNG) }
//
// instance Functor Gen where
//   fmap f m = Gen (\g s -> let (x, s') = unGen m g s in (f x, s'))
//
// instance Applicative Gen where
//   pure x = Gen (\g _ -> (x, g))
//
//   mf <*> mx = Gen (\g s -> let (f, g')  = unGen mf g s
//                                (x, g'') = unGen mx g' s
//                             in (f x, g''))
//
// instance Monad Gen where
//   mx >>= k = Gen (\g s -> let (x, g') = unGen mx g s
//                            in unGen (k x) g' s)
//
// newtype Property = Property { unProperty :: Gen Bool }
//
// class Testable a where
//   property :: a -> Property
//
// class Arbitrary a where
//   arbitrary :: Gen a
//
// chooseInt :: (Int, Int) -> Gen Int
// chooseInt (lo, hi) = Gen (\g _ -> let (x, g') = nextInt g
//                                       r       = hi - lo
//                                    in (lo + (x `mod` r + r) `mod` r, g'))
//
// instance Arbitrary Int where
//   arbitrary = Gen (\g s -> unGen (chooseInt (-s, s)) g s)
//
// instance Testable Bool where
//   property b = Property (return b)
//
// instance Testable Property where
//   property = id
//
// instance Testable p => Testable (Gen p) where
//   property mp = Property $ do
//     p <- mp
//     unProperty (property p)
//
// instance (Arbitrary a, Testable b) => Testable (a -> b) where
//   property f = Property $ do
//     x <- arbitrary
//     unProperty (property (f x))
//
// generate :: Gen a -> a
// generate m = fst (unGen m (mkPRNG 0x9e3779b97f4a7c15) 30)  -- random seed and size
//
// quickCheck :: Testable a => a -> Bool
// quickCheck p = generate (unProperty (property p))
// ```

#check[
  Ich bin in der Lage, ...
  - einfache ```hs Arbitrary```-Instanzen anzugeben.
]


#line()

Diese Aufgaben haben noch keinen Platz gefunden.

#test(level: 2)[
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

#challenge(level: 3, clock: true)[
  Du hörst parallel zu diesem Modul "Berechnung und Logik" und möchtest
  die Automaten-Konstruktionen für reguläre Sprachen in Haskell implementieren?
  Dann schaue nicht weiter. Du hast die korrekte Challenge gefunden. Dieser Weg
  ist lang und das erste Mal etwas steinig, also schnappe dir genügend
  Proviant und bringe genügend Zeit mit.

  In dieser Challenge wollen wir folgende Konstruktionen implementieren.
  $
  "Regulärer Ausdruck"
  stretch(->)^"Thompson-\nKonstruktion"
  epsilon"-NFA"
  stretch(->)^(epsilon"-Hüllen")
  "NFA"
  stretch(->)^"Potenzmengen-\nkonstruktion"
  "DFA"
  $

  - Definiere einen Datentypen ```hs RE```, um regulärere Ausdrücke
    darzustellen. Der reguläre Ausdruck $(mono(a b))^*|mono(c)$ könnte z.B.
    so dargestellt werden
    #align(center)[```hs Kleene (Literal 'a' :*: Literal 'b') :|: Literal 'c'```.]
    Die leere Sprache und die Sprache, die nur das leere Wort enthält, sind über
    das Beispiel nicht abdeckt.
  - Bevor wir mit den Konstruktionen beginnen, müssen wir uns überlegen, wie wir
    die Automaten darstellen wollen. Überlege dir Typen für
    nichtdeterministische endliche Automaten mit und ohne $epsilon$-Transitionen
    und deterministische endliche Automaten. Einen Zustand kannst du z.B. als
    Ganzzahl darstellen.
  - Implementiere die Thompson-Konstruktion als Funktion ```hs RE -> EpsNFA```.
    Hier ist eine wesentliche Schwierigkeit, neue Zustände zu erzeugen, da die
    Bezeichnungen verschiedener Teilautomaten miteinander kollidieren können.
    Das Problem lässt sich z.B. mithilfe eines Zählers lösen, der durch die
    Konstruktion durchgeführt wird. Eine entsprechende Hilfsfunktion könnte
    folgenden Typ haben: ```hs RE -> Int -> (EpsNFA, Int)```. Wenn du dich an
    die ```hs State```-Monade wagen möchtest, könnte dies an dieser Stelle auch
    nützlich sein, um den Zähler mitzuführen -- im Wesentlichen versteckst du
    damit das explizite Mitführen des Zählers.
  - Implementiere eine Funktion ```hs epsilonClosure :: EpsNFA -> [Int] -> [Int]```,
    die die $epsilon$-Hülle einer Menge von Zuständen berechnet.
  - Implementiere eine Funktion ```hs removeEpsilon :: EpsNFA -> NFA```, die
    alle $epsilon$-Transitionen des übergebenen nichtdeterministischen endlichen
    Automaten entfernt. Hier brauchst du dir keine Gedanken über neue Bezeichner
    für Zustände machen, da du die Zustände des alten Automaten wiederverwenden
    kannst.
  - Implementiere die Potenzmengenkonstruktion. Dafür bietet es sich an den NEA
    mit einer Tiefensuche zu durchlaufen, anstatt tabellarisch jede Teilmenge
    der Zustandsmenge entsprechend zu verbinden. So werden dann nur Zustände
    durchlaufen, die für den DEA wichtig sind.
    Hier ist eine wesentliche Schwierigkeit, die neuen Zustände zu benennen.
    Es bietet sich an, ein Wörterbuch der Form ```hs [([Int], Int)]``` zu
    verwalten, in dem die neuen Bezeichner nachgeschaut werden können.
  - Zuletzt, um zu überprüfen, ob der DEA die korrekte Sprache erkennt,
    implementiere eine Funktion ```hs member :: String -> DFA -> Bool```, die
    überprüft, ob ein Wort vom übergebenen Automaten erkannt wird.

  Wenn du bis hierhin gekommen bist, könntest du weiter den DEA mithilfe von
  Hopcrofts Algorithmus minimieren.
]

#remark[
  In der Evaluation des Moduls im Wintersemester 2025/2026 hat sich eine Person
  gewünscht, dass das Prolog-Interpreter-Projekt erweitert wird, sodass auch der
  Parser selber gebaut werden muss. Leider ist in diesem Modul zu wenig Zeit,
  um dieses Thema hinreichend zu behandeln. Das rabbit hole soll dir mit dieser
  Bemerkung allerdigns eröffnet werden.

  - Der Parser aus dem Projekt basiert auf
    #link("https://hackage.haskell.org/package/parsec")[parsec], einer
    Bibliothek bestehend aus verschiedenen monadischen Parser-Kombinatoren. Die
    wesentlichen Ideen, die diese Bibliothek nutzt, findest du in dem
    paper #link("https://www.cambridge.org/core/journals/journal-of-functional-programming/article/monadic-parsing-in-haskell/E557DFCCE00E0D4B6ED02F3FB0466093")[Monadic parsing in Haskell]
    beschrieben. Etwas weniger in die Tiefe geht einer der Co-Autoren in diesem
    Video #link("https://www.youtube.com/watch?v=dDtZLm7HIJs")[Functional Parsing].
  - Mit dieser Bibliothek lassen sich sogenannte
    #link("https://en.wikipedia.org/wiki/Recursive_descent_parser")[recursive descent parser]
    spezifizieren. Die Syntax der Kombinatoren ermöglicht es, dass
    entsprechende Grammatiken fast direkt aus der mathematischen
    Notation in ein Haskell-Programm überführt werden können.
  - Als Nächstes benötigen wir eine Grammatik für die Syntax von Prolog.
    Nicht jede Grammatik ist dafür geeignet, insbesondere müssen
    bei rekursiven Abstiegsparsern darauf achten, dass die
    Grammatik nicht linksrekursiv ist.
  - So können wir dann einen abstrakten Syntaxbaum berechnen, so wie du ihn aus
    dem Projekt kennst. Der Baum wird durch die vielen gegebenen Typen
    dargestellt.

  Je nachdem wie dich das Thema interessiert, behandelt Frank z.B. in seiner
  Vorlesung "Funktionale Programmierung" monadisches Parsen. Wenn du noch tiefer
  einsteigen willst, dann wird auch hin und wieder das Modul "Übersetzerbau"
  angeboten. Für einen tieferen Einstieg wird oft folgende Literatur empfohlen.
  - Compilers: Principles, Techniques, and Tools von Alfred V. Aho,
    Monica S. Lam, Ravi Sethi, und Jeffrey D. Ullman (auch dragon book genannt),
    und
  - Engineering a Compiler von Keith D. Cooper und Linda Torczon (das Modul
    Übersetzerbau nutzt dieses Buch)
]

#pagebreak(weak: true)

