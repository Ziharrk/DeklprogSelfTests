#import "prelude.typ": *
#show: config


= Logische Programmierung

== Prolog, Prolog!

#refs[
  - Skript: Einführung in die Logikprogrammierung, Motivation
  - Skript: Einführung in die Logikprogrammierung, Syntax von Prolog
]

#test(level: 1)[
  Wie unterscheiden sich Variablen in Haskell und Prolog?
]

#test(level: 1)[
  Was ist das Berechnungsprinzip von Prolog bzw. wie leitet Prolog aus
  gegebenen Informationen neue Erkenntnisse ab?
]

#test(level: 1)[
  Formuliere folgende Aussage als Prolog-Programm:
  Seien $A$ und $B$. Dann gilt $C$, wenn $A$ und $B$ gelten.
]

#test(level: 1)[
  Wie können wir Funktionen, wie wir sie aus Haskell kennen, in Prolog umsetzen?
  Erkläre es mithilfe eines Beispiels (wie z.B.
  ```hs (++) :: [a] -> [a] -> [a]```).
]

#test(level: 1)[
  $n$-stellige Funktionen können wir in Prolog als $(n+1)$-stellige Relation
  umsetzen. Dabei nimmt die letzte Position der Relation die Rolle des
  Ergebnisses ein. Gehen wir mit Funktionen vom Typ
  ```hs a1 -> ... an -> Bool``` besonders um?
]

#test(level: 1)[
  Wie hängen die Syntax von Prolog und die der Aussagenlogik zusammen? Welche
  Symbole entsprechen sich?
]

#test(level: 1)[
  In Prolog können wir Termstrukturen erzeugen. Der Haskell-Typ
  #align(center)[```hs data Maybe a = Nothing | Just a```]
  kann z.B. wie folgt in Prolog umgesetzt werden.
  ```SWI-Prolog
  maybe(nothing).
  maybe(just(_)).
  ```
  Übersetze mit der gleichen Idee den Datentyp
  #align(center)[```hs data Tree a = Empty | Node (Tree a) a (Tree a)```.]
]

#test(level: 1)[
  Wie viele Ergebnisse liefern die folgenden Anfragen?
  - ```SWI-Prolog ?- append(_, [X|_], [1, 2, 3, 4]).```
  - ```SWI-Prolog ?- append(_, [_,X|_], [1, 2, 3, 4]).```
  - ```SWI-Prolog ?- member(X, [1, 2, 3]), member(Y, [2, 3, 4]), X \= Y.```
  - ```SWI-Prolog ?- append(_, [X|Ys], [1, 2, 3, 4]), append(_, [Y|_], Ys).```
]

#test(level: 1)[
  Wie stehen die Begriffe Fakt, Regel, Klausel, Prädikat und Relation in
  Beziehung?
]

#test(level: 1)[
  Gebe Beispiele für Formeln an, die zeigen, dass Prolog nur eine echte
  Teilmenge der Prädikatenlogik erster Stufe umsetzt.
]

#test(level: 1)[
  Ordne die Begriffe Atom, Fakt, Regel und Variable dem folgenden
  Prolog-Programm zu?
  ```SWI-Prolog
  semitone(c, cs). semitone(cs, d). semitone(d, ds). semitone(ds, e).
  semitone(e, f). semitone(f, fs). semitone(fs, g). semitone(g, gs).
  semitone(gs, a). semitone(a, as). semitone(as, b). semitone(b, c).

  minor_third(P1, Mi3) :-
    semitone(P1, Mi2), semitone(Mi2, Ma2), semitone(Ma2, Mi3).

  major_third(P1, Ma3) :- minor_third(P1, Mi3), semitone(Mi3, Ma3).

  chord(minor(P1, Mi3, P5)) :- minor_third(P1, Mi3), major_third(Mi3, P5).
  chord(major(P1, Ma3, P5)) :- major_third(P1, Ma3), minor_third(Ma3, P5).
  ```
] <major_chord>

#test(level: 2)[
  In @major_chord sind die Halbtonschritte als Fakten definiert. Um diese
  etwas kompakter zu schreiben, können wir ```SWI-Prolog append/3``` verwenden.
  Implementiere ```SWI-Prolog semitone/2``` mithilfe von ```SWI-Prolog append/3```.
]

// ```SWI-Prolog
// semitone(X, Y) :-
//   append(_, [X, Y|_], [c, cs, d, ds, e, f, fs, g, gs, a, as, b, c]).
// ```

#test(level: 2)[
  Warum ergibt es Sinn, beim Prolog-Programmieren in Relationen statt
  Funktionen zu denken? Betrachte z.B. das Prädikat ```SWI-Prolog append/3```
  gemeinsam mit den Anfragen
  - ```SWI-Prolog ?- append(Xs, Ys, [1, 2, 3]).```,
  - ```SWI-Prolog ?- append(Xs, [2, 3], Zs).```,
  - ```SWI-Prolog ?- append([1], [2, 3], Zs).```,
  - ```SWI-Prolog ?- append([1, 2], Xs, Zs).``` und
  - ```SWI-Prolog ?- append([1, 2], Ys, [1, 2, 3]).```.
  Beschreibe, was jede dieser Anfragen berechnet. Welche der Anfragen entspricht
  der Anwendung von ```SWI-Prolog append/3``` als Funktion, so wie wir es bzgl.
  der Benennung des Prädikats erwarten würden?
]

#test(level: 1)[
  Wenn wir Haskell um die Möglichkeit erweitern könnten, mehrere Regeln
  auszuprobieren und mehrere Lösungen zu kombinieren, hätten wir trotz der
  Abwesenheit logischer Variablen bereits viele Möglichkeiten der Modellierung,
  wie wir sie in Prolog haben. Es stellt sich heraus, dass die Listenmonade
  den Nichtdeterminismus schon sehr gut abbilden kann, und erlaubt damit einen
  weiteren abstrakten Blick auf die Listenmonade - anstatt z.B. der Blick der
  imperativen Programmierung als Verschachtelung von Schleifen.

  Als kleines Beispiel betrachten wir einen fairen Münzenwurf. Wir kodieren
  die Ereignisse binär, wobei 0 für Kopf und 1 für Zahl stehen soll. Weiter ist
  auch ein Beispiel angegeben für zwei unabhängige Münzenwürfe.
  #grid(
    columns: (1fr, 1fr),
    [
      In Prolog:
      ```SWI-Prolog
      coin(0).
      coin(1).

      coin2(X, Y) :-
        coin(X),
        coin(Y).
      ```
    ],
    [
      In Haskell:
      ```SWI-Prolog
      coin :: [Int]
      coin = [0] ++ [1]

      coin2 :: [(Int, Int)]
      coin2 = do
        x <- coin
        y <- coin
        return (x, y)
      ```
    ]
  )

  Bewaffnet mit diesen Ideen, modelliere einen fairen 6-seitigen Würfel.
  Berechne alle Möglichkeiten, wie man drei Würfel werfen kann, um die
  Augenzahl 11 zu erhalten. Gebe deine Lösung sowohl in Prolog als auch in
  Haskell an. Um zu prüfen, ob die Summe von drei Zahlen einer anderen Zahl
  entspricht, kannst du ```SWI-Prolog sum3(X, Y, Z, S) :- S is X + Y + Z.```
  nutzen.
]

// ```hs
// dice :: [Int]
// dice = [1..6]
//
// eleven :: [(Int, Int, Int)]
// eleven = do
//   x <- dice
//   y <- dice
//   z <- dice
//   guard (x + y + z == 11)
//   return (x, y, z)
//
// -- or
// eleven :: [(Int, Int, Int)]
// eleven = do
//   x <- dice
//   y <- dice
//   z <- dice
//   if x + y + z == 1
//     then return (x, y, z)
//     else []
// ```

#test(level: 1)[
  Implementiere das Prädikat ```SWI-Prolog zip/3```, dass zwei Liste bekommt
  und eine Liste von Paaren zurückliefert -- so wie du es aus Haskell kennst.
  Es soll
  #align(center)[```SWI-Prolog ?- zip([1, 2], [3, 4, 5], [(1, 3), (2, 4)]).```]
  gelten.
  Wie gewinnst du aus deiner Implementierung das Prädikat ```SWI-Prolog unzip/3```,
  also die Umkehrfunktion ```SWI-Prolog zip/3```, wenn diese auf Listen gleicher
  Länge eingeschränkt ist.
]

#test(level: 2, clock: true)[
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
    Hilfsprädikat verwenden. Es ist definiert durch
    ```SWI-Prolog not_member(X, L) :- \+ member(X, L).```. Als Zwischenschritt
    kannst du ```SWI-Prolog dups/2``` so implementieren, dass geprüft wird, ob
    ein Element mindestens zweimal vorkommt.
  - ```SWI-Prolog sublist/2``` soll erfüllbar sein, wenn eine Liste in einer
    anderen ohne Lücken enthalten ist.
  - ```SWI-Prolog subsequence/2``` soll erfüllbar sein, wenn eine Liste in einer
    anderen mit möglichen Lücken enthalten ist.

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

#test(level: 1)[
  Das Prädikat ```SWI-Prolog sublist/2``` soll genau dann erfüllbar sein, wenn
  eine gegebene Liste in einer anderen gegebenen Liste enthalten ist. Eine
  mögliche Implementierung sieht wie folgt aus:
  ```SWI-Prolog
  is_prefix([], _).
  is_prefix([X|Xs], [X|Ys]) :- is_prefix(Xs, Ys).

  sublist(Xs, Ys) :- is_prefix(Xs, Ys).
  sublist(Xs, [_|Ys]) :- sublist(Xs, Ys).
  ```
  Was sind die Ergebnisse der Anfrage ```SWI-Prolog ?- sublist(Xs, [1, 2]).```?
]

#test(level: 2)[
  Warum ist ```SWI-Prolog not_member(X, Xs) :- append(_, [Y|_], Xs), X \= Y.```
  keine korrekte Implementierung des Prädikates, das testet, ob ein Element
  nicht einer Liste enthalten ist?
] <not_member>

#test(level: 2)[
  In diesem Selbsttest wollen wir die Peano-Arithmetik wiederholen.
  Implementiere folgende Prädikate:
  - ```SWI-Prolog peano/1``` soll beweisbar sein, wenn der übergebene Term
    eine Peano-Zahl ist, also z.B. die Form ```SWI-Prolog o, s(o), s(s(o)), ...```
    hat.
  - ```SWI-Prolog add/3``` soll die Addition auf Peano-Zahlen implementieren.
  - Die Multiplikation kann wie folgt definiert werden:
    ```SWI-Prolog
    mult(o, _, o).
    mult(s(X), Y, Z) :- add(U, Y, Z), mult(X, Y, U).
    ```
    Eine alternative Implementierung könnte so aussehen:
    ```SWI-Prolog
    mult(o, _, o).
    mult(s(X), Y, Z) :- mult(X, Y, U), add(U, Y, Z).
    ```
    Welcher der beiden Implementierungen ist besser? Betrachte während deiner
    Überlegungen auch folgende Anfrage ```SWI-Prolog ?- mult(s(s(o)), Y, s(s(s(s(o))))).```
    -- also sinngemäß die Anfrage $2 dot y = 4$.
  - Implementiere weiter die Prädikate ```SWI-Prolog lt/2, eq/2``` für
    Peano-Zahlen, also die $<$-Relation und Gleichheit auf Peano-Zahlen.
]

#test(level: 2, clock: true)[
  Das Sieb des Eratosthenes ist ein Algorithmus zur Bestimmung von Primzahlen.
  Dieses wollen wir nun in Prolog implementieren -- mit Peano-Zahlen natürlich.
  - Implementiere zuerst ein Prädikat ```SWI-Prolog range/3```, das eine Liste
    berechnet, die alle Ganzzahlen in einem vorgegebenen Intervall enthält.
    Zum Beispiel soll ```SWI-Prolog ?- range(s(o), s(s(s(o))), [s(o), s(s(o))]).```
    beweisbar sein.
  - Als Nächstes implementiere ein Prädikat ```SWI-Prolog filter_by_prime/3```,
    das alle Elemente einer Liste entfernt, die durch eine gegebene Primzahl
    teilbar sind.
  - Zuletzt benötigen wir noch ```SWI-Prolog primes/2```, dass alle Primzahlen
    bis zu einer angegeben Zahl berechnen soll. Implementiere dieses mithilfe
    der bereits vorbereiteten Prädikate.
  - Mit @to_nat und einem weiteren Prädikat ```SWI-Prolog map_to_nat/2``` kannst
    du die Liste der Primzahlen in eine lesbare Form bringen -- du kannst auch
    ein allgemeines ```SWI-Prolog  map/3``` mithilfe des Prädikats höherer
    Ordnung ```SWI-Prolog call``` implementieren.
]

#test(level: 1)[
  Implementiere ein Prädikat ```SWI-Prolog to_nat/2```, das eine Peano-Zahl in
  eine natürliche Zahl konvertiert. Nutze dafür ```SWI-Prolog is/2```. Wieso
  terminiert die Anfrage ```SWI-Prolog ?- to_nat(P, 3).``` nicht?
] <to_nat>

#test(level: 2, clock: true)[
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

  Hier sind die Eingabe-Graphen nochmal visualisiert.
  #grid(
    columns: (1fr, 1fr),
    gutter: 1em,
    align(center + horizon)[
      #diagraph.raw-render(
        ```dot
        digraph {
          node[shape=circle];

          1 -> 2;
          2 -> 3;
          3 -> 1;
        }
        ```,
        engine: "circo"
      )
    ],
    align(center + horizon)[
      #diagraph.raw-render(
        ```dot
        digraph {
          node[shape=circle];

          1 -> 2;
          3 -> 2;
        }
        ```
      )
    ],
    text(0.8em)[
      Eingabe-Graph der ersten Anfrage mit unendlich vielen gerichteten Pfaden
      von 1 bis 3
    ],
    text(0.8em)[
      Eingabe-Graph der zweiten Anfrage mit keinem gerichteten Pfad von 1
      nach 3
    ]
  )
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

#test(level: 1)[
  Wie unterscheiden sich die Gleichheit ```SWI-Prolog ==/2``` in Prolog
  und die Gleichheit in Haskell?
]

#test(level: 1)[
  Welche Konzepte, die Haskell verwendet, stecken hinter den Anfragen
  - ```SWI-Prolog ?- just((X, Y)) = just((1, 2))``` und
  - ```SWI-Prolog ?- to(X, to(X, list(X))) = to(int, to(int, list(int)))```?
]

#test(level: 2)[
  Eine Dur-Tonleiter hat folgendes Muster relativ zum vorherigen Ton in
  Halbtonschritten:
  #align(center)[+2, +2, +1, +2, +2, +2, +1]
  Zum Beispiel erhalten wir mit dem Grundton C, so die C-Dur Tonleiter
  #align(center)[C - D - E - F - G - A - B - C.]
  D ist zwei Halbtöne von C entfernt, E ist zwei Halbtöne von D entfernt,
  F ist einen Halbton von E entfernt und so weiter.

  Schreibe ein Prädikat ```SWI-Prolog all_major/2```, dass alle diatonischen
  Dur-Akkorde (wie in @major_chord definiert) einer Dur-Tonleiter mit gegebenen
  Grundton berechnet. Diatonisch bedeutet, dass der Akkord nur aus Tönen der
  gegebenen Tonleiter bestehen darf. Für die C-Dur Tonleiter sind die gesuchten
  Akkorde C-Dur, F-Dur und G-Dur.
]

// ```SWI-Prolog
// all_major(R, Cs) :- major_scale([R|S]), all_major(R, Cs, [R|S]).
//
// in_scale(R, major(P1, Ma3, P5)) :-
//   major_scale([R|S]),
//   member(P1, [R|S]), member(Ma3, [R|S]), member(P5, [R|S]).
//
// all_major(_, [], []).
// all_major(R, [C|Cs], [P1|S]) :-
//   C = major(P1, _, _), chord(C), in_scale(R, C), !, all_major(Cs, S, R).
// all_major(R, Cs, [_|S]) :- all_major(R, Cs, S).
// ```

#test(level: 1)[
  Entwickle ein Prädikat ```SWI-Prolog nth/3```, dass das $n$-te Element
  einer Liste zurückgibt. Zum Beispiel soll folgende Anfrage beweisbar sein.
  ```SWI-Prolog
  ?- nth([3, 1, 4, 1, 5], s(s(o)), X).
  X = 4.
  ```
]

#test(level: 3, clock: true)[
  Für die nächste Ausgabe des Mittelerde-Kuriers benötigst du noch ein
  Titelbild für den Artikel "Die Gefährten ziehen zum Schicksalsberg". Dafür
  möchtest du die Gefährten der Größe nach aufstellen. Merry und Pippin sind
  sich nicht einig darüber, wer der größere der beiden ist. Du entscheidest
  dich, beide Varianten zu fotografieren.

  Die Größen-Relation ist mithilfe des folgenden Prädikats festgehalten.
  ```SWI-Prolog
  before(X, Y, Xs) :- append(_, [X|R], Xs), append(_, [Y|_], R).

  smaller(X, Y) :- before(X, Y, [frodo, merry, pippin, sam]).
  smaller(pippin, merry).
  ```

  - Implementiere ein Prädikat ```SWI-Prolog insert/4```, dass ein Element bzgl.
    einer Ordnung in eine gegebene Liste einfügt.
    ```SWI-Prolog
    ?- insert(smaller, merry, [pippin], Fs).
    Fs = [merry, pippin] ;
    Fs = [pippin, merry].
    ```
    Falls dir Prädikate höherer Ordnung Schwierigkeiten bereiten, kannst du auch
    zuerst versuchen, eine Implementierung mit einer festen $<$-Relation
    angeben.
  - Implementiere mithilfe des ```SWI-Prolog insert```-Prädikats Sortieren durch
    Einfügen. Das Prädikat sollte dann beide Sortierungen ausgeben.
    ```SWI-Prolog
    ?- isort(smaller, [sam, pippin, frodo, merry], Fs).
    Fs = [frodo, merry, pippin, sam] ;
    Fs = [frodo, pippin, merry, sam] ;
    ```
]

// ```SWI-Prolog
// insert(_, X, [], [X]).
// insert(Lt, X, [Y|Ys], [X, Y|Ys]) :- call(Lt, X, Y).
// insert(Lt, X, [Y|Ys], [Y|Zs]) :- call(Lt, Y, X), insert(Lt, X, Ys, Zs).
//
//
// isort(Lt, Xs, Ys) :- isort(Lt, Xs, [], Ys).
//
// isort(_, [], Ys, Ys).
// isort(Lt, [X|Xs], Ys, Zs) :- insert(Lt, X, Ys, Us), isort(Lt, Xs, Us, Zs).
// ```

#check[
  Ich bin in der Lage, ...
  - das Berechnungs- und Ableitungsprinzip von Prolog informell zu erklären und
    anzuwenden, insbesondere Backtracking und Nichtdeterminismus,
  - Programme relational zu modellieren, d.h., Fakten, Regeln und Termstrukturen
    zu entwerfen und Prolog bewusst nicht funktional sondern relational
    einzusetzen,
  - Zusammenhänge zwischen Prolog und Haskell herzustellen, etwa Funktionen
    gegenüber Relationen, ```hs Bool```-Funktionen, Listenverarbeitung,
    Nichtdeterminismus (Listenmonade) und Gleichheit,
  - zentrale Prolog-Prädikate zu analysieren und nutzen, insbesondere
    ```SWI-Prolog append/3```, ```SWI-Prolog member/2```, Rekursion sowie deren
    Verhalten bei verschiedenen Anfrageformen,
]


== Elementare Programmiertechniken

#refs[
  - Skript: Logische Programmierung, Elementare Programmiertechniken
]

#test(level: 1)[
  Aus welchen Teilen besteht das generate-and-test Schema?
]

#test(level: 1)[
  Wie hängen musterorientierte Prädikate und induktiv definierte Funktionen
  miteinander zusammen?
]

#challenge(level: 1, clock: true)[
  Das Erfüllbarkeitsproblem der Aussagenlogik fragt, ob es für eine gegebene
  aussagenlogische Formel eine Belegung der Variablen mit wahr oder falsch
  gibt, sodass die Formel insgesamt wahr ist.

  Zur Erinnerung, die Syntax aussagenlogischer Formeln ist wie folgt
  beschrieben:
  - $top$ (true), $bot$ (false) und Variablen sind aussagenlogische Formeln.
  - Seien $F, G$ aussagenlogische Formeln, dann sind
    - $not F$ (Negation),
    - $F and G$ (Konjunktion) und
    - $F or G$ (Disjunktion)
    aussagenlogische Formeln.
  Die Semantik der atomaren Formeln und der Junktoren wird durch die
  entsprechenden booleschen Funktionen und eine Belegung der Variablen
  definiert.

  - Implementiere zuerst ```SWI-Prolog bool/1```, das den Wertebereich
    festlegen soll, und die booleschen Funktionen ```SWI-Prolog and/3, or/3, neg/2```.
  - Implementiere ein Prädikat ```SWI-Prolog get_vars/2```, das alle
    Bezeichner von (freien) Variablen einer gegebenen Formel zurückgibt. Eine
    Variable soll in einer Formel als Term ```SWI-Prolog var(X)```
    gekennzeichnet werden. ```SWI-Prolog X``` ist dann der Bezeichner der
    Variable. Stelle dabei sicher, dass die Liste der Bezeichner keine
    Duplikate enthält -- dafür kannst du z.B. ```SWI-Prolog list_to_set/2```
    verwenden.
  - Implementiere als Nächstes ein Prädikat ```SWI-Prolog assignment/2```,
    das alle Belegungen generiert. Eine Zuweisung soll als Liste von Tupeln
    dargestellt werden (vgl. Beispielanfragen).
  - Implementiere ein Prädikat ```SWI-Prolog eval/3```, das beweisbar ist,
    wenn die gegebene Formel erfüllbar unter der gegegeben Belegung ist.
    Hier sind ein paar Beispielanfragen:
    ```SWI-Prolog
    ?- eval(and(true, var(x)), [(x, true)], false).
    false.

    ?- eval(and(true, var(x)), [(x, B)], false).
    B = false.
    ```
  - Implementiere ein Prädikat ```SWI-Prolog sat/2```, das alle belegten
    Formeln berechnet. Hier sind ein paar weitere Beispielanfragen:
    ```SWI-Prolog
    ?- sat(or(var(x), neg(var(x))), A).
    A = [(x, true)] ;
    A = [(x, false)] ;
    false.

    ?- sat(and(var(x), or(neg(var(y)), neg(var(z)))), A).
    A = [(x, true), (y, true), (z, false)] ;
    A = [(x, true), (y, false), (z, true)] ;
    A = [(x, true), (y, false), (z, false)] ;
    false.

    ?- sat(and(var(x), neg(var(x))), A).
    false.
    ```
] <sat_solver>

// ```SWI-Prolog
// bool(true).
// bool(false).
//
// and(true, true, true).
// and(true, false, false).
// and(false, true, false).
// and(false, false, false).
//
// or(true, true, true).
// or(true, false, true).
// or(false, true, true).
// or(false, false, false).
//
// neg(true, false).
// neg(false, true).
//
// eval(B, _, B) :- bool(B).
// eval(var(X), A, B) :- member((X, B), A).
// eval(neg(X), A, B) :- eval(X, A, B1), neg(B1, B).
// eval(and(X, Y), A, B) :- eval(X, A, B1), eval(Y, A, B2), and(B1, B2, B).
// eval(or(X, Y), A, B) :- eval(X, A, B1), eval(Y, A, B2), or(B1, B2, B).
//
// get_vars_dups(var(X), [X]).
// get_vars_dups(neg(X), Vs) :- get_vars(X, Vs).
// get_vars_dups(and(X, Y), Vs) :- get_vars(X, Vs1), get_vars(Y, Vs2), append(Vs1, Vs2, Vs).
// get_vars_dups(or(X, Y), Vs) :- get_vars(X, Vs1), get_vars(Y, Vs2), append(Vs1, Vs2, Vs).
//
// get_vars(F, Vs) :- get_vars_dups(F, Ws), list_to_set(Ws, Vs).
//
// assignment([], []).
// assignment([V|Vs], [(V, B)|VBs]) :- bool(B), assignment(Vs, VBs).
//
// sat(F, A) :- get_vars(F, Vs), assignment(Vs, A), eval(F, A, true).
// ```

#test(level: 2, clock: true)[
  In diesem Test ergänzen wir den SAT-Löser aus @sat_solver um die Implikation
  und Äquivalenz. Anstatt das ```SWI-Prolog eval```-Prädikat entsprechend zu
  erweitern, wollen wir diese Terme in andere Terme übersetzen, die wir bereits
  auswerten können. Das können wir mit folgenden Regeln erreichen:
  $ (A => B) quad <=> quad (not A or B) quad quad "und" quad quad (A <=> B) quad <=> quad (A => B and B => A) $
  Implementiere dafür ein Prädikat ```SWI-Prolog desugar/2```. Als
  Konstruktoren kannst du z.B. ```SWI-Prolog impl/2``` und ```SWI-Prolog iff/2```
  verwenden.
]

// ```SWI-Prolog
// desugar(B, B) :- bool(B).
// desugar(var(X), var(X)).
// desugar(neg(A), neg(B)) :- desugar(A, B).
// desugar(and(A, B), and(B1, B2)) :- desugar(A, B1), desugar(B, B2).
// desugar(or(A, B), or(B1, B2)) :- desugar(A, B1), desugar(B, B2).
// desugar(impl(A, B), or(neg(C1), C2)) :- desugar(A, C1), desugar(B, C2).
// desugar(iff(A, B), C) :- desugar(and(impl(A, B), impl(B, A)), C).
// ```

#test(level: 2, clock: true)[
  Wir machen uns auf in die Kombinatorik und wollen ein paar nützliche Prädikate
  definieren, die uns helfen Suchräume zu durchlaufen.
  - Implementiere ein Prädikat ```SWI-Prolog varia_rep/3```, das genau dann
    beweisbar ist, wenn eine über gegebene Liste eine Variation einer anderen
    ist und aus $k$ Elementen besteht -- es soll z.B.
    ```SWI-Prolog ?- varia_rep([0, 1], 4, [1, 0, 0, 1]).``` beweisbar sein.
  - Implementiere ein Prädikat ```SWI-Prolog perms/2```, dass genau dann
    beweisbar ist, wenn zwei übergebene Listen Permutationen voneinander sind.
] <combinatorics>

#challenge(level: 2, clock: true)[
  Oft eignet sich Prolog gut, um Algorithmen für Entscheidungsprobleme, die in
  NP liegen, zu implementieren. Als Methode dafür hast du generate-and-test
  kennengelernt -- wir generieren mögliche (also korrekte oder falsche)
  Lösungen und entscheiden dann, ob sie korrekt sind.

  Ein Hamiltonkreis ist ein Kreis in einem Graph, der jeden Knoten genau einmal
  enthält. Wir wollen diese als Listen darstellen. Durch Rotation dieser Liste
  erhalten wir äquivalente Hamiltonkreise. Diese wollen wir ignorieren und
  wählen stattdessen einen kanonischen Repräsentanten, indem wir uns auf die
  Liste beschränken, die mit dem kleinsten Knoten beginnt.

  Im folgenden Graph gibt es zwei Hamiltonkreise.
  #grid(
    columns: (1fr, 1fr),
    gutter: 1em,
    align(center + horizon)[
      #diagraph.raw-render(
        ```dot
        digraph {
          node[shape=circle];
          1 -> 2 -> 3 -> 5 -> 4 -> 1 [color=red];
          3 -> 4 -> 5;
          5 -> 1;
        }
        ```,
        engine: "circo"
      )
    ],
    align(center + horizon)[
      #diagraph.raw-render(
        ```dot
        digraph {
          node[shape=circle];
          1 -> 2 [color=red];
          2 -> 3 [color=red];
          3 -> 5;
          5 -> 4;
          4 -> 1;
          3 -> 4 [color=red];
          4 -> 5 [color=red];
          5 -> 1 [color=red];
        }
        ```,
        engine: "circo"
      )
    ],
    align(center + horizon, text(0.8em)[
      Hamiltonkreis 1: $(1, 2, 3, 5, 4)$
    ]),
    align(center + horizon, text(0.8em)[
      Hamiltonkreis 2: $(1, 2, 3, 4, 5)$
    ])
  )

  Um Hamiltonkreise zu bestimmen, arbeiten wir uns von einem naiven
  Pfad-Generator zu einem, der versucht nach und nach den Suchraum durch
  vorhandene Informationen zu verkleinern (auch pruning genannt). Danach
  verifizieren wir, ob ein gegebener Pfad ein Hamiltonkreis ist. Graphen
  kannst du z.B. als Liste von Kanten darstellen.
  #align(center)[
    ```SWI-Prolog
    graph([(1, 2), (2, 3), (3, 4), (3, 5), (4, 5), (4, 1), (5, 1), (5, 4)]).
    ```
  ]

  Es lohnt sich, @combinatorics vor dieser Challenge gemacht zu haben, wenn du
  mit den naiveren Pfad-Generatoren starten möchtest.

  - Implementiere zuerst einen (polynomiellen) Verifizierer, also ein Prädikat
    ```SWI-Prolog is_hamilton/2```, das bestimmt, ob ein gegebener Pfad in einem
    gegebenen Graphen einem Hamiltonkreis entspricht.
  - Implementiere ein Prädikat ```SWI-Prolog path/2``` und verbessere sukzessiv
    mit den folgenden Ideen:
    - Prüfe jede beliebige Knotenfolge bestehend aus $abs(V)$ Knoten. Das heißt,
      der Suchraum entspricht zu Beginn $V^abs(V)$ für einen Graphen $(V, E)$.
    - Jede zwei Knoten dieser Folge müssen paarweise unterschiedlich sein.
    - Jede zwei aufeinanderfolgenden Knoten entsprechen einer Kante in dem
      Eingabegraphen. Die Knotenfolge ist also ein Pfad.
  - Implementiere zuletzt das Prädikat ```SWI-Prolog hamilton/2```, das
    Hamiltonpfade bzw. -kreise berechnet.

  Beobachte experimentell, wie sich die Laufzeit durch die einzelnen
  Pruning-Schritte verändert.
]

// ```SWI-Prolog
// graph([(1, 2), (2, 3), (3, 4), (3, 5), (4, 5), (4, 1), (5, 1), (5, 4)]).
//
// edge(V, W, G) :- member((V, W), G).
//
// nodes(G, Vs) :- setof(X, A^B^(edge(A, B, G), (X = A; X = B)), Vs).
//
// path(G, P) :-
//   nodes(G, Vs), min_list(Vs, V), length(Vs, N),
//   path_from(G, V, N, [V], P).
//
// path_from(_, _, 1, A, P) :- reverse(A, P).
// path_from(G, V, N, A, P) :-
//   N > 1,
//   edge(V, W, G),
//   \+ member(W, A),
//   M is N - 1,
//   path_from(G, W, M, [W|A], P).
//
// is_hamiltonian(G, P) :-
//   forall(append(_, [V, W|_], P), edge(V, W, G)),
//   nodes(G, Vs), sort(Vs, Ws), sort(P, Ws).
//
// hamiltonian(G, P) :-
//   path(G, P),
//   is_hamiltonian(G, P).
// ```

#check[
  Ich bin in der Lage, ...
  - einen Suchraum zu generieren,
  - den gleichen Suchraum zu durchsuchen,
  - und währenddessen (große) Teile des Suchraums, die keine Lösung enthalten
    können, kategorisch auszuschließen.
]


== Rechnen in der Logikprogrammierung

#refs[
  - Skript: Logische Programmierung, Rechnen in der Logikprogrammierung
]

Im Folgenden stehen Großbuchstaben für Variablen und Kleinbuchstaben für
atomare Ausdrücke -- wenn nicht anders im Test oder in der Challenge eingeführt.

#test(level: 1)[
  Was besagt die Abtrennungsregel (modus ponens)?
]

#test(level: 1)[
  Was ist das (einfache) Resolutionsprinzip?
]

#test(level: 1)[
  Wann ist eine Anfrage mithilfe des Resolutionsprinzips beweisbar?
]

#test(level: 1)[
  Mithilfe welcher Methode stellen wir fest, ob ein Literal zu einer linken
  Regelseite passt?
]

#test(level: 1)[
  Wie sind Terme definiert?
]

#test(level: 1)[
  Welche der folgenden Terme sind syntaktisch korrekt?
  - $X$
  - $a$
  - $X X$
  - $f(X,a)$
  - $f(f(X))$
  - $g(f(X)(Y))$
]

#test(level: 1)[
  Mithilfe welcher Methode ersetzen wir Variablen in Termen?
]

#test(level: 1)[
  Wie ist die Substitution auf Termen definiert? Was wird insbesondere durch
  eine Substitution verändert und was nicht?
]

#test(level: 1)[
  Falls du @sat_solver gemeistert hast -- an welcher Stelle deines Programms
  führst du eine Substitution durch?
]

#test(level: 1)[
  Sei $sigma = { X |-> 1, Y |-> 2 }$ eine Substitution. Welche Anwendungen
  oder Aussagen sind korrekt?
  - $sigma("add"(X, Y)) = "add"(1, 2)$
  - $sigma("eq"(X, X)) = "eq"(1, X)$
  - $sigma(f(g(X, Y), Z))$ ist nicht definiert.
]

#test(level: 1)[
  Welche der folgenden Substitutionen sind wohldefiniert?
  - $sigma = { X |-> 1 }$
  - $sigma = { X |-> X }$
  - $sigma = { f(X) |-> f(Y) }$
  - $sigma = { X |-> Y, Y |-> X }$
  - $sigma = { [X|Y] |-> [1|[]] }$
]

#test(level: 1)[
  Sind $[X]$ und $[1, 2]$ unifizierbar?
]

#test(level: 1)[
  Wende die Substitution $sigma = { X |-> 1, Y |-> f(X) }$ auf den Term
  $g(X, h(Y))$ an, ohne einen Zwischenschritt auszulassen.
]

#test(level: 1)[
  Was ist ein Unifikator? Was ist ein allgemeinster Unifikator?
]

#test(level: 1)[
  Was sind die Ergebnisse der folgenden Kompositionen von Substitutionen?
  - ${ Y |-> X } compose { Z |-> 1 }$
  - ${ Y |-> X } compose { Z |-> Y }$
  - ${ Z |-> Y } compose { Y |-> X }$
]

#test(level: 1)[
  Wie ist Unifizierbarkeit definiert?
]

#test(level: 1)[
  Wir haben eine Substitution als eine Abbildung $cal(X) -> cal(T)(cal(X))$
  definiert, wobei $cal(X)$ eine Menge von Variablen und $cal(T)(cal(X))$ die
  Menge aller Terme über $cal(X)$ sind. Angenommen eine Substitution sei eine
  Abbildung $cal(T)(cal(X)) -> cal(T)(cal(X))$. Wieso ist dann der Begriff
  der Unifizierbarkeit weniger brauchbar?
]

#test(level: 1)[
  Beim Durchführen des Unifikationsalgorithmus treten in zwei verschiedenen
  Iterationen $i, j$ mit $i < j$ die Substitutionen
  $ sigma_i = { X |-> f(Y) } "und" sigma_j = { X |-> a } $
  auf. Ist ein solcher Verlauf korrekt? Begründe deine Antwort.
  Dabei kann angenommen werden, dass $X$ nicht in $Y$ vorkommt.
]

#test(level: 1)[
  Warum gilt ${ Y |-> X } compose { Z |-> Y } = { Z |-> Y } compose { Y |-> X }$
  nicht?
]

#test(level: 1)[
  Warum ist ${ Y |-> X } compose { Y |-> 2 } = { Y |-> 2 }$?
]

#test(level: 1)[
  Warum ist die Komposition von Substitutionen im Allgemeinen nicht die
  Vereinigung, also $phi compose psi = phi union psi$?
]

#test(level: 1)[
  Wann existiert ein allgemeinster Unifikator?
]

#test(level: 1)[
  Was ist die Unstimmigkeitsmenge zweier Terme, was gibt sie an und wie ist sie
  definiert?
]

#test(level: 1)[
  Welche Unstimmigkeitsmengen sind korrekt berechnet?
  - $"ds"(f(X), f(1)) = {X, 1}$
  - $"ds"(1, 2) = {1, 2}$
  - $"ds"(g(1, 2), h(1, 2)) = emptyset$
  - $"ds"(X, Y) = emptyset$
  - $"ds"(f(X, Y), f(1, 2)) = {Y, 2}$
]

#test(level: 1)[
  Finde die allgemeinsten Unifikatoren für die folgende Terme:
  - $(X + 1) dot Y + Z$ und $((3 + Z) + 1) dot Z + 3$,
  - $f(X, Y)$ und $f([Y|R], 1)$,
  - $f([X|R], X, R)$ und $f([1, 2, 3], _, [Z|S])$, und
  - #grid(
      columns: (1fr, auto, 1fr),
      align: center + horizon,
      [
        #diagraph.raw-render(
          ```dot
          digraph {
            ranksep=0.25;
            nodesep=0.15;
            node [shape=plaintext];
            edge [arrowsize=0.6];
            1 -> 2;
            1 -> 3;
            3 -> 4;
            3 -> 5;
            5 -> 6;
            5 -> 7;
          }
          ```,
          labels: (
            "1": ```hs (:)```,
            "2": ```hs Nothing```,
            "3": ```hs (:)```,
            "4": ```hs Nothing```,
            "5": ```hs Just . (,)```,
            "6": ```hs 6```,
            "7": ```hs 7```,
          )
        )
      ],
      [und],
      [
        #diagraph.raw-render(
          ```dot
          digraph {
            ranksep=0.25;
            nodesep=0.15;
            node [shape=plaintext];
            edge [arrowsize=0.6];
            1 -> 2;
            1 -> 3;
            3 -> 4;
            3 -> 5;
          }
          ```,
          labels: (
            "1": ```hs (:)```,
            "2": $X$,
            "3": ```hs (:)```,
            "4": ```hs Nothing```,
            "5": $Y$
          )
        )
      ]
    )
] <det_mgus>

#test(level: 1)[
  Unter welchen Umständen terminiert der Unifikationsalgorithmus?
]

#test(level: 1)[
  Wie können wir überprüfen, ob eine von uns berechnete Substitution
  (z.B. als Ergebnis des Unifikationsalgorithmus) ein Unifikator ist? Wie
  können wir Evidenz gewinnen, dass die Substitution wahrscheinlich keinen
  Fehler enthält?
]

#test(level: 1)[
  Aus welcher Eigenschaft des Unifikationsalgorithmus folgt, dass ein
  berechneter Unifikator ein allgemeinster Unifikator ist?
]

#test(level: 1)[
  Seien $t_1, t_2$ Terme. Wenn $"ds"(sigma(t_1), sigma(t_2)) = emptyset$ gilt,
  was können wir über $sigma$ folgern?
]

#test(level: 1)[
  Welche Arten von Fehlschlägen können während des Unifikationsalgorithmus
  auftreten? Unter welchen Umständen treten diese auf?
]

#test(level: 1)[
  In welches Problem laufen wir, wenn wir mit einer Substitution, die sich
  aus $"ds"(t_1, t_2) = {X, f(X)}$ ergibt, naiv weiter rechnen würden, wobei
  $t_1$ und $t_2$ Terme sind?
]

#test(level: 1)[
  Was bedeutet es, wenn der Vorkommenstest (occurs check) positiv ist?
]

#test(level: 1)[
  Gebe ein Beispiel für eine Eingabe an, für das der Unifikationsalgorithmus
  exponentielle Laufzeit bzgl. der Größe der Eingabeterme hat.

  Die Größe eines Terms $abs(dot)$ wir z.B. wie folgt berechnen:
  - $abs(X) = 1$, falls $X$ Variable ist,
  - $abs(a) = 1$, falls $a$ Konstante ist und
  - $abs(f(t_1, ..., t_n)) = 1 + sum_(i=1)^n abs(t_i)$ für Terme
    $t_1, ..., t_n$ und $n$-stelligen Funktor $f$.
]

#test(level: 1)[
  Wieso kommt der Fall der exponentiellen Laufzeit in der Größe der Eingabeterme
  überhaupt zustande?
]

Zu den am häufigsten gemachten Fehlern beim Angeben eines SLD-Baums ist die
fehlerhafte Anwendung des Unifikationsalgorithmus.  Beim intuitiven Anwenden des
Algorithmus suchen wir oftmals nur die Stellen in den Ausgangstermen, an denen
Variablen auf Terme stoßen und vergessen, zuvor freie und bereits gebundene
Variablen zu ersetzen. Als Unifikator für z.B. ```SWI-Prolog f(1, Y)``` und
```SWI-Prolog f(X, [X|R])``` wird oft $ sigma = { X |-> 1, Y |-> [X|R] } $
angegeben. Das ist falsch, da
$ sigma(f(1, Y)) = f(1, [X|R]) != f(1, [1|R]) = sigma(f(X, [X|R])) $
Auf der linken Seite könnten wir den Unifikator zwar erneut anwenden, um den
korrekten Term zu erhalten, damit halten wir uns allerdings nicht an die
Definition der Unifizierbarkeit.

Wenn wir den Unifikationsalgorithmus auf die beiden Terme rigoros anwenden,
sehen wir in der ersten Iterationen, dass der Teilterm $[X|R]$ nicht mehr in
$sigma_1(t_2) = f(1,[1|R])$ vorkommt.
#align(center)[
  #grid(
    columns: (auto, auto, auto, auto, auto),
    inset: 8pt,
    stroke: 0.5pt,
    [$k$], [$sigma_k$], [$sigma_k (t_1)$], [$sigma_k (t_2)$], [$"ds"(sigma_k (t_1), sigma_k (t_2))$],
    [$0$], [$emptyset$], [$f(1,Y)$], [$f(X, [X|R])$], [${X,1}$],
    [$1$], [${X |-> 1}$], [$f(1,Y)$], [$f(1, [1|R])$], [${Y,[1|R]}$],
    [$2$], [${X |-> 1, Y |-> [1|R]}$], [$f(1,[1|R])$], [$f(1, [1|R])$], [$emptyset$],
  )
]
Der obige Fehler kommt zustande, da wir beim Berechnen der zweiten
Unstimmigkeitsmenge noch die Ausgangsterme, $t_1$ und $t_2$, anstatt der neuen
Terme, $sigma_1 (t_1)$ und $sigma_1 (t_2)$, nutzen, in denen $X$ dann
substituiert ist.

Wie können wir daran denken? Wenn eine Variable $X$ in der $k$. Iteration
gebunden wird, dann kann $X$ weder in $sigma_k (t_1)$ noch in $sigma_k (t_2)$
vorkommen. Für dieses Beispiel bedeutet das, wenn wir kurz davor sind,
$Y |-> [X|R]$ hinzuschreiben, sollten wir uns vergewissern, ob bereits gebundene
Variablen auf der rechten Seite vorkommen (hier $X$) und entsprechend ersetzen
(mit $1$). In einem anderen Fall können wir Variablen haben, die bereits an
Terme gebunden sind aber noch freie Vorkommen von Variablen haben --
betrachte z.B. $f(X, 1)$ und $f([Y|R], Y)$ mit
${Y |-> 1} compose {X |-> [Y|R]}$. Hier müssen wir andersherum schauen,
ob die neue Belegung alte Belegungen verändert. Das geht aus der linken Menge
der Definition der Komposition hervor.
$
phi compose psi
  = {v |-> phi(t) | v |-> t in psi, phi(t) != v}
  union {v |-> t | v |-> t in phi, v in.not D(psi) }.
$

Ein Unifikator $sigma$ erfüllt die Eigenschaft
$ forall v |-> t in sigma : "Vars"(t) inter D(sigma) = emptyset "oder alternativ" sigma compose sigma = sigma. $ Es dürfen also auf den rechten Seiten keine Variablen vorkommen
können, die durch den Unifikator selber gebunden werden. Solange diese
Eigenschaft nicht erfüllt ist, müssen wir solche Vorkommen durch die Belegungen
ersetzen. In @det_mgus findest du Beispiele zum Üben.

// TODO fix
//
// Der Fall "${Z |-> Y} compose {Y |-> X} = {Z |-> Y, Y |-> X}$" kann im
// Unifikationsalgorithmus nicht eintreten.
//
// Praktisch ergibt daraus, wenn zwei Terme unifizierbar sind, dann können wir
// eine Substitution (keinen Unifikator) gemäß des Unifikationsalgorithmus auf den
// Ausgangstermen definieren, in dem wir alle Unstimmigkeitsstellen in den Termen
// betrachten.
// $ phi = phi_2 compose phi_1 = { Y |-> [X|R] } compose { X |-> 1 } $
// Danach berechnen wir die Komposition von $phi$ mit sich selbst bis wir einen
// Fixpunkt erreicht haben (maximal so häufig, wie wir Belegungen haben), und
// erhalten dadurch den allgemeisten Unifikator:
// $
// sigma &= phi compose phi \
//   &= { Y |-> phi([X|R]), X |-> phi(1) } \
//   &= { Y |-> [1|R], X |-> 1 }.
// $
// Das entspricht den Anwendungen der Substitution $sigma_k$, bevor wir die
// Unstimmigkeitsmenge im Unifikationsalgorithmus berechnen.

Hier sind andere Fehler, die seltener geschehen, über die man sich trotzdem
Gedanken machen kann.
- Basierend auf dem obigen Beispiel wird als Zwischenschritt manchmal
  $ { Y |-> [X|R], X |-> 1 } = { Y |-> [1|R], X |-> 1 } $
  aufgeschrieben. Das ist falsch, denn hier besteht keine Gleichheit. Die linke
  Substitution bildet $Y$ auf $[X|R]$ ab, während die rechte $Y$ auf $[1|R]$
  abbildet. Die Terme $[X|R]$ und $[1|R]$ sind nicht gleich -- selbst wenn in
  den beiden Substitutionen $X$ auf $1$ abgebildet wird.
- In den Unifikatoren müssen alle Belegungen stehen, die benötigt werden, um
  die linke Seite einer Regel mit dem derzeitig selektierten Literal zu
  unifizieren. Das ist unabhängig davon, ob diese Belegungen am Ende zur
  Berechnung einer Lösung benötigt werden. Es geht um die korrekte Anwendungen
  eines Algorithmus und nicht darum, dass ihr logisch schließen könnt, dass
  manche Belegungen irrelevant sind.


#test(level: 1)[
  Aus welchen Komponenten setzt sich das allgemeine Resolutionsprinzip zusammen?
  Wie wird es auch genannt?
]

#test[
  SLD-Resolution-Aufgaben findest du den Übungsserien sowohl als Präsenz- und
  Hausaufgaben. Darüber findest du sie auch den Altklausuren des Moduls. Diese
  kannst du auf der #link("https://www.fs-infmath.uni-kiel.de/wiki/Protokolle_Informatik")[Webseite der Fachschaft]
  oder ggf. im aktuellen Moodle-Kurs herunterladen.
]

#test(level: 1)[
  Was legt die Selektionsfunktion der SLD-Resolution fest? Welche verwendet Prolog?
]

#test(level: 1)[
  Gegeben sei die Anfrage $"?-" A_1, ..., A_m$. Du stellst fest, dass $A_1$
  mit der linken Seite der Regel $L ":-" L_1, ..., L_n$ unifizierbar ist. Welche
  Anfrage ist in einem SLD-Resolutionsschritt daraus ableitbar.
]

#test(level: 2)[
  Wieso benennen wir Variablen einer Regel um, bevor wir eine Unifikation als
  Teil eines SLD-Resolutionsschritt durchführen?
]

#test(level: 1)[
  Wodurch ergibt sich die Struktur eines SLD-Baums?
]

// TODO fühlt sich wie eine unübersichtliche wall of text an, ein
//      algorithmischerer, stichpunktartigerer Aufschrieb ist möglicherweise
//      geeigneter
#let qq = math.op("?-")
#let cd = math.op(":-")

Ein SLD-Baum stellt einzelne SLD-Resolutionsschritte graphisch dar -- betrachte
parallel beim Lesen den unten abgebildeten Ausschnitt eines SLD-Baums. Die
Wurzel des Baumes ist mit einer gegebenen zu beweisenden Anfrage beschriftet.
Jeder Resolutionsschritt beginnt bei einem Knoten, der mit einer Anfrage $qq
L_1, ..., L_n$ ($n >= 1$) beschriftet ist -- wenn $n = 0$ ist, sind wir bereits
fertig. Basierend auf der Auswertungsstrategie von Prolog wählen wir das erste
Literal $L_1$ dieser Anfrage aus und versuchen dieses zu beweisen. Dafür
schauen wir uns gegebene (frisch umbenannte!) Klauseln der Reihe nach an. Wenn
$X$ die nächste linke Seite einer Klausel $X cd R_1, ..., R_m$ ($m >= 0$) mit
$L_1$ unifizierbar ist, d.h., es existiert $phi = "Unifikator"(L_1, X)$, dann
ersetzen wir das Literal $L_1$ mit der rechten Seite $R_1, ..., R_m$ und wenden
auf die neu gewonnene Anfrage $R_1, ..., R_m, L_2, ..., L_n$ den Unifikator an,
den wir aus der Unifikation des Literals mit der linken Seite erhalten, also
$phi (R_1, ..., R_m, L_2, ..., L_n)$. Die umbenannte Klausel geben wir neben
dem berechneten Unifikator als Teil der Kantenbeschriftung als Hilfestellung
mit an. Wenn die Anfrage bewiesen ist, also die Anfrage aus keinem Literal mehr
besteht ($qq .$), dann haben wir einen Beweis für die Anfrage gefunden und
geben das zugehörige Ergebnis $sigma$ an. Das heißt, wir berechnen die
Komposition der Unifikatoren von der Lösung bis zur Wurzel und schränken sie
danach die freien Variable der ursprünglichen Anfrage ein. Wenn $A$ die zu
beweisende Aussage ist und $phi_1, ..., phi_l$ Unifikatoren sind, die wir über
die Resolutionsschritte von $A$ nach $qq .$ erhalten haben, dann ist die Lösung
in dem Fall $sigma = (phi_l compose ... compose phi_1) |_("Vars"(A))$. Im
anderen Fall führen wir weitere SLD-Resolutionsschritte durch. Wenn die Anfrage
leer ist oder keine weiteren Klauseln zum betrachteten Literal passen,
backtracken wir. Das heißt, wir betrachten die zuvor bearbeitete Anfrage erneut
und versuchen, einen alternativen Beweis zu finden, indem wir alternative
Resolutionsschritte auf Basis von bisher nicht ausprobierten Klausuren
durchführen. So können wir z.B. zu der Zwischenanfrage $qq L_1, ..., L_n.$
zurückkommen und finden neben der Klausel $X cd R_1, ..., R_m$ möglicherweise
weitere passende Klauseln. Das ist hier angedeutet durch die rechteste Kante.
Im anderen Fall backtracken wir weiter.

Wenn das aktuelle Literal ein cut ist, dann betrachten wir im nächsten Schritt
die Anfrage ohne den cut. Wichtig ist, dass das Beweisen des cuts dazu führt,
dass sowohl für die einführende Regel des cuts als auch für alle Literale die
danach auf der linken Seite vom cut stehen keine alternativen
Resolutionsschritte durchgeführt werden, sobald wir einen Beweis für die linke
Seite gefunden haben.

#align(center)[
  #cetz.canvas({
    import cetz.draw: *

    set-style(mark: (end: ">"))

    content((0, 0), [$qq ...$], padding: 0.5em, name: "query0")
    content((0, -2), [$qq L_1, ..., L_n.$], padding: 0.5em, name: "query1")
    content((-5, -5), [$qq ...$], padding: 0.5em, name: "query2")
    content((0, -5), [$qq phi(R_1, ..., R_m, L_2, ..., L_n).$], padding: 0.5em, name: "query3")
    content((5, -5), [$qq ...$], padding: 0.5em, name: "query4")
    content((0, -7), [$qq ...$], padding: 0.5em, name: "query6")

    line("query0", "query1", stroke: (dash: "dotted"))

    line("query1", "query3", name: "step")
    content("step.mid", padding: 0.5em, anchor: "west", [$X cd R_1, ..., R_m$ \ $phi = "Unifikator"(L_1, X)$])

    bezier("query1.west", "query2.north", (-4.5, -2), stroke: (dash: "dotted"))
    bezier("query1.east", "query4.north", (4.5, -2), stroke: (dash: "dotted"))

    line("query3", "query6", stroke: (dash: "dotted"))
  })
]

#test(level: 1)[
  Gegeben sei folgendes Prolog-Programm.
  ```SWI-Prolog
  upTo(o, _, []).
  upTo(s(N), A, [A|Xs]) :- upTo(N, A, Xs).
  upTo(s(N), A, []).
  ```
  Betrachte die Anfrage ```SWI-Prolog ?- upTo(s(s(o)), a, Xs), Xs = [X, Y].```.
  Die erste Lösung ergibt sich aus der Komposition der folgenden Unifikatoren
  und dessen anschließlichen Einschränkung auf die freien Variablen der Anfrage.
  Für die erste Lösung $sigma_1$ der obigen Anfrage lauten die Unifikatoren z.B.
  - $phi_1 = {N_1 |-> s(o), A_1 |-> a, italic("Xs") |-> [a|italic("Xs")_1]}$,
  - $phi_2 = {N_2 |-> o, A_2 |-> a, italic("Xs")_1 |-> [a|italic("Xs")_2]}$,
  - $phi_3 = {italic("Xs")_2 |-> []}$ und
  - $phi_4 = {X |-> a, Y |-> a, X_4 |-> [a, a]}$.
  $phi_4 compose phi_3 compose phi_2 compose phi_1$ rigoros
  auszurechnen, ist müheselig und zeitaufwendig. In diesem Test versuchen wir,
  einen schnelleren Weg zur Berechnung einer Lösung zu ergründen, wenn wir
  die Unifikatoren bereits haben.

  - Sei $G = (V, E)$ ein gerichteter Graph mit
    $ V = union.big_(k=1)^4 D(phi_k) "und" E = {(V, W) | k in {1,...,4}, V |-> T in phi_k, W in "Vars"(T) }. $
    Stelle diesen Graph dar.
  - Wieso ist $G$ azyklisch? Wieso haben die freien Variablen der
    Anfrage keine eingehenden Kanten in dem Graphen?
  - Betrachte die Knoten $X$, $Y$ und $italic("Xs")$. Wie hängen ihre Nachfolger
    mit ihnen zusammen?
  - Wie erhalten wir $italic("Xs") |-> [a, a]$ aus dem Teilgraphen von $G$, der
    $italic("Xs")$ enthält und alle von diesem Knoten aus erreichbaren Knoten?
  - Wie ergibt sich aus der vorherigen Bertrachtung ein schnelleres
    Berechnungsverfahren?
  - Betrachten $G$ ohne die von $X$, $Y$ und $italic("Xs")$ und die von diesen
    erreichbaren Knoten. Welche Bedeutung haben diese bei der Berechnung der
    Lösung
    $sigma_1 = (phi_4 compose phi_3 compose phi_2 compose phi_1)|_{{X,Y,italic("Xs")}}$?
]

#test(level: 1)[
  Welche Auswertungsstrategie findet immer eine Lösung, falls eine existiert?
]

#test(level: 1)[
  Warum wird die Tiefensuche als Auswertungsstrategie der Breitensuche bevorzugt?
]

#test(level: 1)[
  Wie ergibt sich die Reihenfolge der Kindknoten eines Knoten in einem SLD-Baum?
]

#test(level: 1)[
  Welche Rolle spielt Backtracking in Prolog?
]

#test(level: 1)[
  Wie werden Variablen in Prolog gebunden?
]

#test(level: 1)[
  Wieso wird empfohlen, dass Klauseln für Spezialfälle vor allgemeineren
  Klauseln stehen sollten?
]

#test(level: 2)[
  Gegeben sei das Prolog-Programm.
  ```SWI-Prolog
  fruit(apple).
  fruit(banana).
  fruit(cranberry).

  salad(F1, F2, F3) :- fruit(F1), fruit(F2), fruit(F3).
  ```
  Was sind die ersten vier Lösungen, die Prolog berechnet? Warum verändern sich
  die Belegungen für ```SWI-Prolog F2``` und ```SWI-Prolog F3``` zuerst?
]

#test(level: 2)[
  Gegeben sei das Prolog-Programm:
  ```SWI-Prolog
  nth(0, [X|_], X).
  nth(N, [_|Xs], X) :- N > 0, M is N - 1, nth(M, Xs, X).
  ```

  Wir beginnen, eine Lösungen zu berechnen, wie Prolog sie berechnen würde, für
  die Anfrage ```SWI-Prolog ?- nth(1, [3, 1, 4, 1]).```.
  ```SWI-Prolog
  ?- nth(1, [3, 1, 4, 1]).
  |- (2. Regel)
  ?- 1 > 0, M is 1 - 1, nth(M, [1, 4, 1], X).
  |- (2. Regel)
  ?- 1 > 0, M is 1 - 1, M > 0, M is M - 1, nth(M, [4, 1], X).
  |-
  ...
  ```
  Was ist hier schief gelaufen?
]

#test(level: 1)[
  Begründe warum die folgende Aussage korrekt oder falsch ist:
  Beim Berechnen der Unstimmigkeitsmenge kann folgendes Ergebnis herauskommen:
  $ { g(X, Y), g(Y, X) }. $
]

#test(level: 1)[
  Gegeben sei folgendes Prolog-Programm.
  ```SWI-Prolog
  append([],    L, L     ).
  append([E|R], L, [E|RL]) :- append(R, L, RL).
  ```
  Es wird die Anfrage ```SWI-Prolog ?- append(X, Y, [1, 2]).``` gestellt. Beim
  Anwenden der zweiten Regel wurde die Substitution
  $ sigma_1 = { X |-> [E_1|R_1], Y |-> L_1, E_1 |-> 1, R L_1 |-> [2] } $
  berechnet. Ist diese Substitution ein Unifikator für
  ```SWI-Prolog append(X, Y, [1, 2])``` und
  ```SWI-Prolog append([E1|R1], L1, [E1|RL1])```, der aus dem
  Unifikationsalgorithmus entstanden ist?
]

#test(level: 1)[
  Gegeben sei folgendes Prolog-Programm.
  ```SWI-Prolog
  f(X, 2).
  ```
  Welcher Schritt der SLD-Resolution ist hier essentiell, um eine korrekte
  Lösung zu erhalten. Betrachte dafür die folgende Anfrage
  ```SWI-Prolog f(1, X).```
]

#test(level: 1)[
  Welche der folgenden Listen entsprechen syntaktisch korrekten Listen in
  Prolog. Darüber hinaus, welche der Listen entsprechen der Liste $(1, 2, 3)$.
  - ```SWI-Prolog [1, 2, 3]```
  - ```SWI-Prolog [1, [2, 3]]```
  - ```SWI-Prolog [1|[2, 3]]```
  - ```SWI-Prolog [1, 2|[3]]```
  - ```SWI-Prolog [1, 2|3]```
  - ```SWI-Prolog [1|2|3]```
  - ```SWI-Prolog [1|[2|[3|[]]]]```
]

#test(level: 2)[
  Die Komposition von Substitutionen ist definiert durch
  $
  phi compose psi
    = {v |-> phi(t) | v |-> t in psi, phi(t) != v}
    union {v |-> t | v |-> t in phi, v in.not D(psi) }.
  $
  Wieso spielen $phi(t) != v$ und $v in.not D(psi)$ keine Rolle, wenn wir den
  Unifikationsalgorithmus akribisch anwenden? Wieso spielt es aber eine Rolle,
  wenn wir z.B. ${Y |-> X} compose {X |-> Y}$ oder ${X |-> y} compose {X |-> x}$
  betrachten?
]

#test(level: 2)[
  Wieso tritt das Szenario, in dem wir z.B. $f(X, 1)$ und $f(1, X)$ unifizieren
  müssten, beim Angeben eines SLD-Baums nicht auf?
]

#check[
  Ich bin in der Lage, ...
  - Aussagen mithilfe von Modus Ponens und dem Resolutionsprinzip logisch
    abzuleiten,
  - Terme zu erkennen, Substitutionen korrekt anzuwenden und allgemeinste
    Unifikatoren zu bestimmen,
  - SLD-Resolution anzuwenden, Variablen korrekt zu binden und Backtracking
    nachzuvollziehen, und
  - Nichtdeterminismus in Prolog zu verstehen und mehrere Lösungen systematisch
    zu berechnen (SLD-Bäume mit cuts).
]


== Negation

#refs[
  - Skript: Einführung in die Logikprogrammierung, Negation
]

#test(level: 1)[
  Was verbirgt sich hinter dem Begriff "Negation als Fehlschlag"?
]

#test(level: 1)[
  Wann ist ```SWI-Prolog \+ p``` beweisbar?
]

#test(level: 2)[
  Wieso stimmt die Negation als Fehlschlag nicht mit der prädikatenlogischen
  Negation überein?
]

#test(level: 1)[
  Warum sollte ```SWI-Prolog p``` keine freien Variablen enthalten, wenn wir
  ```SWI-Prolog \+ p``` beweisen wollen? Wieso ergibt sich daraus die
  Empfehlung, dass Negationen soweit wie möglich rechts in einer Regel stehen
  sollten?
]

#test(level: 1)[
  Wie sind die Variablen ```SWI-Prolog X, Y``` in der folgenden Regel
  quantifiziert?
  ```SWI-Prolog
  p(X) :- q(X, Y).
  ```
]

#test(level: 1)[
  Nutze $forall x : p <=> not (exists x : not p)$, um ein Prädikat
  ```SWI-Prolog forall/2``` zu definieren.

  Mithilfe dieses Prädikates soll es möglich sein, folgende Anfrage auszudrücken.
  ```SWI-Prolog
  ?- forall(member(X, [1, 2, 3]), X > 0).
  ```
  Die Anfrage bedeutet, für alle $x in {1, 2, 3}$ gilt $x > 0$.

  Wie können wir mit ```SWI-Prolog forall``` die Definition von
  ```SWI-Prolog not_member``` aus @not_member reparieren?
]

#check[
  Ich bin in der Lage, ...
  - die Negation zu nutzen,
  - Fallstricke der Negation zu erklären.
]


== Der Cut-Operator

#refs[
  - Skript: Einführung in die Logikprogrammierung, Der "Cut"-Operator
]

#test(level: 1)[
  Wofür verwenden wir den Cut-Operator konzeptionell?
]

#test(level: 1)[
  Wann ist ```SWI-Prolog p``` in ```SWI-Prolog p :- q, !, r.``` beweisbar?
  Was ist insbesondere der Fall, wenn ```SWI-Prolog q``` beweisbar ist?
]

#test(level: 1)[
  Was sind die Ausgaben der jeweiligen Prolog-Programme, wenn die Anfrage
  ```SWI-Prolog ?- a(1).``` bewiesen wird.
  #grid(
    columns: (1fr, 1fr),
    [
      Programm 1:
      ```SWI-Prolog
      a(1) :- writeln(a).
      a(2) :- writeln(b).
      a(1) :- writeln(c), !.
      a(1) :- writeln(d).
      a(2) :- writeln(e).
      ```
    ],
    [
      Programm 2:
      ```SWI-Prolog
      a(1) :- writeln(a).
      a(1) :- false, !, writeln(b).
      a(2) :- writeln(c).
      a(1) :- !, writeln(d).
      a(1) :- writeln(e), !.
      a(1) :- writeln(f).
      a(2) :- writeln(g).
      ```
    ]
  )
]

#test(level: 1)[
  Wir ergibt sich aus der Semantik des Cut-Operators ein
  Fallunterscheidung-Konstrukt?
]

#test(level: 2)[
  Wie können wir mithilfe des Cut-Operators die Negation als Fehlschlag
  implementieren?
]

#test(level: 1)[
  Wie wirkt sich der Cut-Operator auf die Struktur eines SLD-Baums aus?
]

#test(level: 1)[
  Wie können wir mithilfe des Cut-Operators im folgenden Prädikat Berechnungen
  sparen?
  ```SWI-Prolog
  max(X, Y, X) :- X >= Y.
  max(X, Y, Y) :- X < Y.
  ```
]

#test(level: 1)[
  Gegeben sei folgendes Prolog-Programm.
  ```SWI-Prolog
  member(X, [X|_]).
  member(X, [_|R]) :- member(X, R).

  member_cut(X, [X|_]) :- !.
  member_cut(X, [_|R]) :- member_cut(X, R).
  ```
  Wie verändert der Cut-Operator das Verhalten in ```SWI-Prolog member_cut/2```
  von ```SWI-Prolog member/2```?
]

#check[
  Ich bin in der Lage, ...
  - zu erklären, was der Cut-Operator in Prolog macht und wie er das
    Backtracking beeinflusst,
  - die Semantik des Cut-Operators in SLD-Bäumen kenntlich zu machen,
  - anhand eines Prolog-Programms vorherzusagen, welche Lösungen durch einen Cut
    verhindert werden, und
  - den Cut gezielt einzusetzen, um unnötiges Backtracking zu vermeiden, ohne
    die logische Bedeutung eines Programms zu verändern.
]


== Prolog-Arithmetik und nicht-relationale Prädikate

#refs[
  - Skript: Einführung in die Logikprogrammierung, Programmieren mit Constraints,
    Arithmetik in Prolog (Constraint-Programmierung mit CLP-Bibliotheken
    entfällt)
]

#test(level: 1)[
  Wofür wird das Prädikat ```SWI-Prolog is/2``` verwendet?
]

#test(level: 1)[
  Welche Anfragen sind valide?
  - ```SWI-Prolog X is 42 - 3```
  - ```SWI-Prolog X is Y + 1```
  - ```SWI-Prolog Y = 1, X is Y + 1```
  - ```SWI-Prolog 32 + 10 is X```
  - ```SWI-Prolog 42 is 40 + Y```
]

#test(level: 1)[
  Warum können wir ```SWI-Prolog =/2``` nicht für Arithmetik verwenden bzw.
  wieso gilt ```SWI-Prolog 42 = 40 + 2``` nicht?
]

#test(level: 2)[
  Welche weiteren Prädikate kennst du neben ```SWI-Prolog is/2``` die auch
  Terme ausrechnen?
]

#test(level: 2)[
  Das Prädikat ```SWI-Prolog is/2``` muss in seinem zweiten Argument hinreichend
  instanziiert sein, damit wir es auswerten können. Als Faustregel kann man sich
  merken, dass Prädikate, die arithmetische Ausdrücke oder aussagenlogische
  Formeln auswerten hinreichend instanziierte Argumente benötigen. Welche
  Prädikate gibt es, neben ```SWI-Prolog is/2```, die solche Argumente
  benötigen? Warum ist die Anfrage ```SWI-Prolog ?- 42 >= X, 42 =< X.```
  nicht beweisbar?
][
  Ein Teil der hier angesprochenen Probleme werden durch die
  Constraint-Programmierung gelöst, die wir nicht behandelt haben.
]

#test(level: 1)[
  Ist ```SWI-Prolog X =:= 4 + 7.``` eine valide Anfrage in Prolog?
]

#test(level: 2)[
  Benenne Vor- und Nachteile der Prolog-Arithmetik gegenüber Arithmetik mit
  Peano-Zahlen.

  Betrachte dafür insbesondere ```SWI-Prolog ?- 4 is X + 2.``` und
  ```SWI-Prolog ?- add(X, s(s(o)), s(s(s(s(o))))).```.
]

#test(level: 1)[
  Implementiere ein Prädikat ```SWI-Prolog count_nodes/2```, dass die Anzahl
  von internen Knoten in einem Binärbaum zählt. Hier ist eine Beispielanwendung
  des Prädikats.
  ```SWI-Prolog
  ?- count_nodes(branch(empty, branch(empty, empty)), N).
  N = 2.
  ```
  Warum die Anfrage ```SWI-Prolog ?- count_nodes(T, 2).``` problematisch? Wie
  kannst du ```SWI-Prolog count_nodes``` anpassen, sodass das erwartete Ergebnis
  herauskommt?
] <count_nodes>

#test(level: 1)[
  Wann nennen wir einen Term vollständig instanziiert?
]

#test(level: 1)[
  Wann nennen wir ein Prädikat relational, multimodal bzw. multidirektional?
]

#test(level: 1)[
  Du hast Prolog lieben gelernt und liest die Dokumentation von SWI-Prolog
  gerne vor dem Schlafen gehen. Du entdeckst sonderliche Sonderzeichen vor den
  Variablen eines Prädikats und fragst dich, wofür diese stehen. Das sind
  sogenannte #link("https://www.swi-prolog.org/pldoc/man?section=argmode")[argument mode indicators]
  und deuten an, wie Informationen durch ein Prädikat fließen.

  Was bedeuten die Indikatoren des ```SWI-Prolog is/2```-Prädikats (siehe
  #link("https://www.swi-prolog.org/pldoc/man?predicate=is/2")[```SWI-Prolog is/2```-Dokumentation])?
][
  Uns ist bisher kein nicht vollständig instanziierter Ausdruck eingefallen,
  den wir unter der Verwendung von ausschließlich vorlesungsbekannten
  Prädikaten konstruieren können. Um dennoch ein Beispiel gesehen zu haben,
  warum ```SWI-Prolog Expr``` mit ```hs +``` annotiert ist und nicht mit
  ```hs ++```, betrachte folgendes Programm:
  ```SWI-Prolog
  :- arithmetic_function(mult/2).
  mult(0, _, 0) :- !.
  mult(_, 0, 0) :- !.
  mult(X, Y, Z) :- Z is X * Y.

  ?- X is mult(0, Y).
  X = 0.
  ```
  Die rechte Seite muss so also nur hinreichend instanziiert sein, damit
  ```SWI-Prolog is/2``` beweisbar ist.
]

#test(level: 2, clock: true)[
  Gegeben sei das Prädikat
  ```SWI-Prolog
  f(X, Y) :- Y is 2 * X + 1.
  ```
  das die Funktion $f(x) = 2x + 1$ in Prolog modelliert.
  - Welche Anfragen der Form ```SWI-Prolog ?- f(X, Y)```, wobei
    ```SWI-Prolog X``` und ```SWI-Prolog Y``` frei oder gebunden sein können,
    sind nicht beweisbar und warum?
  - Verbessere ```SWI-Prolog f/2``` so, dass ```SWI-Prolog f/2``` als $f^(-1)$
    verwendet werden kann. Als Hilfsprädikate kannst du die Prädikate
    ```SWI-Prolog var/1``` und ```SWI-Prolog nonvar/1``` verwenden.
  - Warum ist ```SWI-Prolog ?- f(X, Y)``` mit freien Variablen
    ```SWI-Prolog X``` und ```SWI-Prolog Y``` weiterhin problematisch?
  - Wie könnte das Prädikat
    ```SWI-Prolog
    int(0).
    int(N) :- int(N, 1).
    int(N, N).
    int(N, M) :- N is -M.
    int(N, M) :- K is M + 1, int(N, K).
    ```
    das Problem der vorherigen Teilaufgabe lösen?
] <relational_linear>

#test(level: 2)[
  Löse @relational_linear erneut, aber betrachte die Funktion $f(x) = x^2$ für
  alle $x in ZZ$.
]

#check[
  Ich bin in der Lage, ...
  - das Prädikat ```SWI-Prolog is/2``` zu verwenden, um arithmetische Ausdrücke
    auszuwerten,
  - gültige von ungültigen arithmetischen Anfragen zu unterscheiden,
  - zu erklären, warum ```SWI-Prolog =/2``` für Arithmetik ungeeignet ist und
    wann ```SWI-Prolog is/2``` verwendet werden muss,
  - alternative arithmetische Prädikate in Prolog einzusetzen
    (Peano-Arithmetik),
  - die Vor- und Nachteile der eingebauten Prolog-Arithmetik gegenüber
    Peano-Arithmetik zu erläutern
  - Prädikate relational zu definieren und Prädikate zu erkennen, die nicht
    relational sind.
]

== Prädikate höherer Ordnung

#refs[
  - Skript: Einführung in die Logikprogrammierung, Meta-Programmierung,
    Prädikate höherer Ordnung
]

#test(level: 1)[
  Erkläre, wie das Meta-Prädikat ```SWI-Prolog call``` verwendet wird. Welche
  Stelligkeit hat ```SWI-Prolog call```?
]

#test(level: 2)[
  Implementiere die Prädikate ```SWI-Prolog map/3```, ```SWI-Prolog filter/3```,
  ```SWI-Prolog foldr/4```, so wie du sie erwarten würdest. Implementiere
  darüber hinaus das Prädikat ```SWI-Prolog all/2```, dass genau dann erfüllt
  ist, wenn das ein übergegebenes Prädikat für alle Elemente einer Liste
  erfüllt ist.
]

// ```SWI-Prolog
// map(_, [], []).
// map(F, [X|Xs], [Y|Ys]) :- call(F, X, Y), map(F, Xs, Ys).
//
// filter(_, [], []).
// filter(P, [X|Xs], [X|Ys]) :- call(P, X), !, filter(P, Xs, Ys).
// filter(P, [_|Xs], Ys) :- filter(P, Xs, Ys).
//
// foldr(_, E, [], E).
// foldr(F, E, [X|Xs], R) :- foldr(F, E, Xs, S), call(F, X, S, R).
// ```

#check[
  Ich bin in der Lage, ```SWI-Prolog call``` zu nutzen.
]


== Kapselung des Nichtdeterminismus

#refs[
  - Skript: Einführung in die Logikprogrammierung, Meta-Programmierung,
    Kapselung des Nichtdeterminismus
]

#test(level: 2, clock: true)[
  Implementiere ein Prädikat ```SWI-Prolog all_trees/2```, das jeden möglichen
  blätterbeschriften Binärbaum erzeugt, deren Blätter von links nach rechts die
  Eingabeliste lesen.
  ```SWI-Prolog
  ?- all_trees([1, 2, 3], Ts).
  Ts = [
    branch(leaf(1), branch(leaf(2), leaf(3))),
    branch(branch(leaf(1), leaf(2)), leaf(3))
  ].
  ```
  - Implementiere zuerst eine Variante, in der du auf keine besonderen
    Hilfsprädikate zurückgreifst.
  - Gegeben sei folgende fehlerhafe Implementierng des Prädikats.
    ```SWI-Prolog
    all_trees([], []).
    all_trees([X], [leaf(X)]) :- !.
    all_trees(Xs, Ts) :-
      member(branch(Lt, Rt), Ts),
      append([L|Ls], [R|Rs], Xs),
      all_trees([L|Ls], Lts),
      member(Lt, Lts),
      all_trees([R|Rs], Rts),
      member(Rt, Rts).
    ```
    Auf dem ersten Blick scheint eine elegante Idee hinter dieser Definition zu
    stecken. Wieso funktioniert diese Idee allerdings nicht?
  - Korrigiere die fehlerhafte Definition unter Verwendung von
    ```SWI-Prolog findall/2``` und erhalte dabei grundsätzliche Idee.
  - Staune darüber, wie vergleichweise müheselig das Implementieren der ersten
    Variante ohne Hilfsprädikate war, und sei dankbar für
    ```SWI-Prolog findall/3```.
]

// ```SWI-Prolog
// all_trees([], []).
// all_trees([X], [leaf(X)]) :- !.
// all_trees(Xs, Ts) :-
//   findall(
//     branch(Lt, Rt),
//     (
//       append([L|Ls], [R|Rs], Xs),
//       all_trees([L|Ls], Lts), member(Lt, Lts),
//       all_trees([R|Rs], Rts), member(Rt, Rts)
//     ),
//     Ts
//   ).
// ```

#test(level: 2)[
  Implementiere ein Prädikat ```SWI-Prolog all_btrees/2```, dass alle Binärbäume
  mit einer festen internen Knotenanzahl berechnet.
][
  Es bietet sich an, @count_nodes vorher bearbeitet zu haben.
]

// TODO more findall, bagof, setof

#check[
  Ich bin in der Lage, ...
  - ```SWI-Prolog findall/3``` oder ähnliche Mechanismen einzusetzen, um
    systematisch alle möglichen Ergebnisse zu kapseln.
]

// Difference lists wurden für die Klausur ausgeschlossen.


== Logik-Puzzles

Hier sind ein paar Logik-Puzzles, die zu deiner Belustigung dienen. Der
Lerneffekt ist voraussichtlich sehr gering.

#challenge(level: 3, clock: true)[
  Das #link("https://en.wikipedia.org/wiki/Zebra_Puzzle")[Zebra-Rätsel] ist ein
  Logikpuzzle.

  So wird es auf Wikipedia wiedergegeben:
  #quote(block: true)[
    + Es gibt fünf Häuser.
    + Der Engländer wohnt im roten Haus.
    + Der Spanier hat einen Hund.
    + Kaffee wird im grünen Haus getrunken.
    + Der Ukrainer trinkt Tee.
    + Das grüne Haus ist direkt rechts vom weißen Haus.
    + Der Raucher von Old-Gold-Zigaretten hält Schnecken als Haustiere.
    + Die Zigaretten der Marke Kools werden im gelben Haus geraucht.
    + Milch wird im mittleren Haus getrunken.
    + Der Norweger wohnt im ersten Haus.
    + Der Mann, der Chesterfield raucht, wohnt neben dem Mann mit dem Fuchs.
    + Die Marke Kools wird geraucht im Haus neben dem Haus mit dem Pferd.
    + Der Lucky-Strike-Raucher trinkt am liebsten Orangensaft.
    + Der Japaner raucht Zigaretten der Marke Parliaments.
    + Der Norweger wohnt neben dem blauen Haus.
    Wer trinkt Wasser? Wem gehört das Zebra?
  ]
  Löse dieses Puzzle mithilfe von Prolog und übersetze dabei so wenig logische
  Schlüsse, die du selbst in deinem Kopf machst, in deinem Programm.
]

#challenge(level: 3, clock: true)[
  Ein Logikpuzzle, das einst viral ging, ist bekannt als "Cheryl's Geburtstag".
  Es ist das erste Puzzle aus dem paper
  #link("https://arxiv.org/abs/1708.02654")[Cheryl's Birthday].
  Löse dieses Puzzle mithilfe von Prolog und übersetze dabei so wenig logische
  Schlüsse, die du selbst in deinem Kopf machst, in deinem Programm.
]

// ```SWI-Prolog
// date(may, 15).
// date(may, 16).
// date(may, 19).
// date(june, 17).
// date(june, 18).
// date(july, 14).
// date(july, 16).
// date(august, 14).
// date(august, 15).
// date(august, 17).
//
//
// months_for_day(Day, Months) :- setof(M, date(M, Day), Months).
// days_for_month(Month, Days) :- setof(D, date(Month, D), Days).
// ambiguous(Xs) :- Xs \= [_].
//
//
// albert_statement1(Month) :-
//   days_for_month(Month, Days),
//   forall(
//     member(Day, Days),
//     (months_for_day(Day, Months), ambiguous(Months))
//   ).
//
// bernard_statement(Day) :-
//   months_for_day(Day, Months),
//   ambiguous(Months),
//   findall(
//     Month,
//     (albert_statement1(Month), date(Month, Day)),
//     [_]
//   ).
//
// albert_statement2(Month) :-
//   findall(
//     Day,
//     (bernard_statement(Day), date(Month, Day)),
//     [_]
//   ).
//
//
// solution(Month, Day) :-
//   date(Month, Day),
//   albert_statement1(Month),
//   bernard_statement(Day),
//   albert_statement2(Month).
// ```

#line()

Diese Aufgaben haben noch keinen Platz gefunden.

#challenge(level: 2)[
  Psst! Ja, du! Möchtest du ein Werkzeug zum Generieren von SLD-Bäumen?

  Erweitere den Prolog-Interpreter um eine Funktion
  ```hs graphviz :: SLDTree -> String```, die einen SLD-Baum nimmt und ihn z.B.
  mithilfe von #link("https://graphviz.com/")[Graphviz] visualisiert.

  Das Erzeugen des folgenden DOT-Codes ist vergleichsweise unkompliziert aus
  dem Projekt-Code heraus möglich. Wenn du dir mehr anzeigen lassen möchtest,
  musst du ein paar Stellen anpassen. Der ```hs SLDTree``` speichert nicht,
  welche Regel verwendet wurde, bzw. die umbenannte Regel, die für die
  Unifikation verwendet wurde. Dafür müsste der Typ entsprechend angepasst
  werden. Je mehr du dir anzeigen lassen möchtest, desto zeitaufwendiger oder
  schwieriger wird diese Challenge. Eine prototypische Implementierung, die das
  untere Beispiel umsetzt, ist zügig möglich.

  Folgender DOT-Code erzeugt den folgenden SLD-Baum. Graphviz unterstütz von
  Haus aus nicht das Setzen von mathmatischen Formeln. Entweder du verzichtest
  darauf oder du schaust dir z.B. #link("https://dot2tex.readthedocs.io/en/latest/")[dot2tex]
  oder #link("https://github.com/Robotechnic/diagraph")[diagraph] an, je nach
  dem ob du #link("https://www.latex-project.org/")[LaTeX] oder
  #link("https://typst.app/")[Typst] nutzt bzw. nutzen möchtest. Wenn du keinen
  DOT-Compiler hast, kannst du zum Debuggen deines generierten DOT-Codes
  #link("https://dreampuf.github.io/GraphvizOnline/")[diese Seite] nutzen.

  #grid(
    columns: (1fr, 1fr),
    [
      #linebreak()
      ```dot
      digraph {
        node [shape=none];
        1 [label="?- =(X, 1)."];
        2 [label="?- ."];

        1 -> 2 [label="{X1 |-> 1, X |-> 1}"];
      }
      ```
    ],
    [
      #align(center)[
        #diagraph.raw-render(
          ```dot
            digraph {
              node [shape=none];
              1 -> 2 [label="{X_1 |-> 1, X |-> 1}"]
            }
          ```,
          labels: (
            "1": [$qq =(X, 1).$],
            "2": [$qq .$]
          )
        )
      ]
    ],
  )
]

#pagebreak(weak: true)

