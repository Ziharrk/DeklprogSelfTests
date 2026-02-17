#import "prelude.typ": *
#show: config

= Häufige Bearbeitungsfehler in Klausuren

== Klausur 1.PZ WS25/26

- *Aufgabe 1*
  - Partiell definierte Funktionen häufig nicht erkannt.
    Welche Eingabe könnte zu keinem Ergebnis führen?
  - Korrekte Substitutionen können mithilfe einer Probe identifiziert werden.
    Wenn Zweifel besteht, können diese schnell auf einem Schmierzettel
    durchgeführt werden.
  - ```SWI-Prolog ?- X = Y, Y = X``` ist im Vergleich zu
    ```SWI-Prolog ?- X >= Y, Y >= X``` beweisbar. In der ersten Anfrage geht
    es um die Unifizierbarkeit von Variablen, im zweiten zuerst darum, dass
    ```SWI-Prolog X``` und ```SWI-Prolog Y``` berechnet werden können. Zum
    Zeitpunkt der Berechnung sind diese aber noch nicht hinreichend gebunden.
  - Zwei Terme sind unifizierbar, wenn die Substition durch _einmaliges_
    Anwenden zwei gleiche Terme erzeugt.
- *Aufgabe 2*
  - Eine Zählvariable, so wie du sie aus imperativen Programmiersprachen kennst,
    kannst du als Parameter einer Hilfsfunktion mitführen (siehe @clz_popcnt).
    So etwas wie ```hs count = count + 1``` gibt es in Haskell nicht.
  - ```hs getLine``` entfernt den Zeilenumbruch am Ende einer Zeile.
  - ADT: siehe @timer_adt_solution (bzw. zuerst @timer_adt)
- *Aufgabe 3*
  - In ```hs fmap f (Fake fx) = Fake (fmap f fx)``` wurde häufiger entweder
    der Konstruktor ```hs Fake``` oder ```hs fmap``` vergessen.
  - Ähnlich auch bei der ```hs Fake```-Regel für das ```hs (>>=)```.
  - In beiden Fällen sollte man die Identitätsgesetze auf Plausibilität prüfen,
    ```hs fmap id = id``` und ```hs m >>= return = m```. So sieht man, ob z.B.
    in ```hs Fake mx >>= f = mx >>= f``` ein Konstruktor fehlt oder nicht.
  - Mit korrekter ```hs (>>=)```-Implementierung sieht man dann auch, dass
    das innerste ```hs Real 42``` mit dem Ausdruck rechts im Lambda ersetzt
    wird, und ```hs Fake (Fake (Real 73))``` herauskommt. Hier ist häufiger ein
    ```hs Fake```-Konstruktor verloren gegangen.
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
  - ...
