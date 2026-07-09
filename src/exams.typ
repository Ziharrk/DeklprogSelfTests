#import "prelude.typ": *
#show: config

= Häufige Bearbeitungsfehler in Klausuren

In diesem Abschnitt findest du aktuell Bearbeitungsfehler, die wir während der
Korrektur von Klausuren häufiger gesehen haben. Mit der Zeit sollen diese in
Selbsttests überführt werden.

== Klausur 1.PZ WS25/26

- *Aufgabe 1*
  - Partiell definierte Funktionen (wie z.B.
    ```hs init, tail, div, mod, maximum, ...```) wurden häufig nicht erkannt.
    Hier solltet ihr euch immer Fragen: Welche Eingabe könnte zu keinem Ergebnis
    führen? Das sind oft null-artige Werte wie ```hs 0``` oder ```hs []```.
    (Siehe @test_partial_functions)
  - Korrekte Substitutionen bzw. Unifikatoren können mithilfe einer Probe
    identifiziert werden. Wenn Zweifel besteht, können diese schnell auf einem
    Schmierzettel durchgeführt werden. Zwei Terme sind unifizierbar, wenn die
    Substitution durch _einmaliges_ Anwenden zwei gleiche Terme erzeugt.
  - ```SWI-Prolog ?- X = Y, Y = X``` ist im Vergleich zu
    ```SWI-Prolog ?- X >= Y, Y >= X``` beweisbar. In der ersten Anfrage geht
    es um die Unifizierbarkeit von Variablen, im zweiten zuerst darum, dass
    ```SWI-Prolog X``` und ```SWI-Prolog Y``` berechnet werden können. Zum
    Zeitpunkt der Berechnung sind diese aber noch nicht hinreichend gebunden.
- *Aufgabe 2*
  - Eine Zählvariable, so wie du sie aus imperativen Programmiersprachen kennst,
    kannst du als Parameter einer Hilfsfunktion mitführen. So etwas wie
    ```hs count = count + 1``` gibt es in Haskell nicht. (Siehe @clz_popcnt)
  - ```hs getLine``` entfernt den Zeilenumbruch am Ende einer Zeile. Technisches
    Detail, das man sich merken muss.
  - ADT: siehe @timer_adt_mistakes bzw. zuerst @timer_adt
- *Aufgabe 3*
  - In ```hs instance Functor Fiction where``` wurde häufig noch eine Typvariable ```hs a``` ergänzt,
    obwohl in den Typkonstruktorklassen der letzte Parameter eines Typs eben nicht appliziert werden darf.
  - Dasselbe gilt auch für die ```hs Monad```-Instanz.
  - In ```hs fmap f (Fake fx) = Fake (fmap f fx)``` wurde häufiger entweder
    der Konstruktor ```hs Fake``` oder ```hs fmap``` vergessen.
  - Ähnlich auch bei der ```hs Fake```-Regel für das ```hs (>>=)```.
  - In beiden Fällen sollte man die Identitätsgesetze auf Plausibilität prüfen,
    ```hs fmap id = id``` und ```hs m >>= return = m```. So sieht man, ob z.B.
    in ```hs Fake fx >>= f = fx >>= f``` ein Konstruktor fehlt oder nicht.
  - In der letzten Teilaufgabe wurden häufig ```hs Fake```-Konstruktoren
    weggelassen. Hier bietet es sich an zu raten, wenn man keine Monaden-Instanz
    angeben konnte, oder einen mit der eigenen Monaden-Instanz konsistenten
    Wert schrittweise auszurechnen.
    ```hs
    instance Monad Fiction where
      Real x  >>= f = f x              -- (1)
      Fake fx >>= f = Fake (fx >>= f)  -- (2)

      Fake (Real 42) >>= \x -> Fake (Real (x + 31))  -- (2)
    = Fake (Real 42 >>= \x -> Fake (Real (x + 31)))  -- (1)
    = Fake ((\x -> Fake (Real (x + 31))) 42)         -- Applikation
    = Fake (Fake (Real (42 + 31)))                   -- Definition (+)
    = Fake (Fake (Real 73))
    ```
    So ist die Berechnung dann im schlechtesten Fall ein Folgefehler aus
    der Definition der Monaden-Instanz.
- *Aufgabe 4*
  - "mithilfe eines Aufrufs von ```hs foldr```" bedeutet, die Definition besteht
    aus einer Regel, die so startet ```hs listToN_fold xs = foldr ... ... xs```
    (oder $eta$-reduziert).
  - ```hs []``` ist kein Fall, der im Lambda behandelt werden muss. Dafür ist
    der zweite Parameter der ```hs foldr```-Funktion da.
  - Mit der natürlichen Faltung ist das Verfahren aus der Vorlesung gemeint,
    um eine Faltungsfunktion für einen gegebenen Datentypen anzugeben -- nichts
    anders.
  - Häufig wurde eine Funktion für den ```hs Nested```-Konstruktor vergessen.
  - Variablen in Pattern werden kleingeschrieben.
  - Prüft die Stelligkeiten und Typen der Parameter der Datenkonstruktoren!
  - Schaut insbesondere nochmal nach, wie sich die Signatur der Faltungsfunktion
    aus dem Datentypen ergibt.
- *Aufgabe 5*
  - Arithmetik nicht in richtiger Reihenfolge (auf der rechten Seite muss
    alles hinreichend instanziiert zum Rechnen sein)
  - ```SWI-Prolog f(X + 1, Y div 2)``` gibt Terme an das Prädikat
    ```SWI-Prolog f```, und nicht die ausgerechneten Ergebnisse. Diese müssen
    immer mit ```SWI-Prolog is/2``` berechnet werden.
- *Aufgabe 6*
  - $sigma_1 = {X |-> Y, Y |-> 1} != {X |-> 1, Y |-> 1} = sigma_2$, denn
    $sigma_1((X, Y)) = (Y, 1) != (1, 1) = sigma_2((X, Y))$.
  - Insbesondere kann ${X |-> Y} compose {Y |-> 1}$ nicht während der
    Durchführung des Unifikationsalgorithmus auftreten.
  - Andersherum, ${Y |-> 1} compose {X |-> Y} = {Y |-> 1, X |-> 1}$ und nicht
    ${Y |-> 1, X |-> Y}$.
  - Im Kapitel #link(<how_prolog>)[Rechnen in der Logikprogrammierung] findest
    du ganz viele Selbsttests, die dich für diese Fehler sensibilisieren können,
    sodass du sie reichtzeitig erkennst.


== Klausur 2.PZ WS25/26

- *Aufgabe 1*
  - In 1.2 ist ```SWI-Prolog ?- call(length([1, 2], X)).``` oft nicht angekreut
    worden.
  - In 1.3 lassen sich Fehler vermeiden, indem man die Funktionsgleichheit
    rigoros überprüft. Das heißt, man über für jedes Element der
    Definitionsbereiche, ob auf beiden Seiten das gleiche herauskommt. Zum
    Beispiel: In 3b) sind die Definitionsbereiche für beide Funktionen ${X,Z}$.
    $ ({X |-> Y} compose {Z |-> Y})(X) = {X |-> Y}(X) = Y = {X |-> Y, Z |-> Y}(X) $
    Dasselbe machen wir für $Z$. In 4d) sehen wir z.B., dass die linke Funktion
    $Y$ nach $Y$ abbildet. Auf der rechten Seite wird $Y$ aber nach $X$ abgebildet.
  - In 1.4 können genauso nachrechnen. Unifizierbarkeit ist definiert als die
    Gleichheit zweier Terme $t_1, t_2$ unter einer Substitution $sigma$, d.h.,
    falls $sigma(t_1) = sigma(t_2)$ gilt. Zum Beispiel sei $sigma$ die erste
    Substitution, dann gilt
    $ sigma(f(g(X),Y)) = f(sigma(g(X)), sigma(Y)) = f(sigma(g(X)), g(a)) = f(g(sigma(X)), g(a)) = f(g(a), g(a)) $
    und für den anderen Term (direkter)
    $ sigma(f(Y,Z)) =  f(g(a), g(a)). $
    Damit ist $sigma$ ein Unifikator.
- *Aufgabe 2*
  - In 2.1 ist man aufgeschmissen, wenn man nicht weiß, wie
    ```hs const, (>>=), (>>)``` abbilden. Hier sind häufige falsche Bindungen
    für die jeweiligen Terme, die wir gesehen haben.
    - Zu a) ```hs [3, 2, 1]```
    - Zu b) beliebige Zahlen, ```hs id```, ```hs [1, 2, 3]```
    - Zu c) ```hs 73```
    - Zu d) ```hs []```
  - In 2.2 sind die Stunden die sich aus den Minuten ergeben oft nicht
    berücksichtigt worden.
- *Aufgabe 3*
  - Die Aufgabe hat viele kalt erwischt.
  - Hier ist eine Typklasse, die alle Fehler irgendwie zusammenfässt
    ```hs
    type Hashable :: a -> Int
    data Hashable a b where
      hash :: Hashable a b -> Int -> a
    ```
  - Als ```hs Hashable```-Instanz für Listen haben wir dann häufiger sinngemäß
    ```hs
    instance Functor Hashable where
      fmap xor []     = 0
      fmap xor (x:xs) = hash x : fmap xor xs
    ```
    gesehen.
- *Aufgabe 4*
  - Wenn in ```hs getNumbers``` ein Akkumulator verwendet, der die neue Zahl
    immer vorne anhängt, dann ist die Ergebnisliste bereits umgekehrt, sodass
    kein ```hs reverse``` später mehr notwendig gewesen ist.
  - Beim ```hs foldr``` wurden ```hs f``` und ```hs g``` häufig vertauscht.
- *Aufgabe 5*
- *Aufgabe 6*

