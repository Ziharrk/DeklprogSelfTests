#import "prelude.typ": *
#show: config

= Häufige Bearbeitungsfehler in Klausuren

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
