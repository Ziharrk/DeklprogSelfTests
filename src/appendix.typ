#import "prelude.typ": *
#show: config


= Appendix

Bisher findest du hier im Anhang des Dokuments Bemerkungen zu verschiedenen
Tests oder Challenges. Sie sind aus unserer Neugier entstanden und stecken dich
möglicherweise an. Du kannst aber auch alles ignorieren, was hier steht, und
verpasst nichts.

#remark[
  In @reverse_mode_ad wurden Funktionen ```hs d1``` und ```hs d2``` definiert.
  Diese ver- und entschachteln ```hs D```-Werte unterschiedlich tief, um die
  jeweilige Ableitung zu berechnen. Ein allgemeines ```hs d``` für beliebige
  Ableitung zu definieren, stellt sich als schwierig heraus. Mit verschiedenen
  Haskell-Spracherweiterungen können wir eine allgemeinere Funktion angeben.
  Das ist in #link(git("blob/main/src/AutoDiff.hs"), raw("AutoDiff.hs")) im
  GitHub-Repository demonstriert. Mit ```hs derive @3 cos (2.0 :: Double)```
  können wir jetzt z.B. die dritte Ableitung des Cosinus an der Stelle $2$
  berechnen.
] <reverse_mode_ad_remark>

#remark[
  In @typeclasses_in_python haben wir gesehen, wie deklarative
  Programmierkonzepte aus Haskell in Python verwendet werden können. In Java
  haben diese über die letzten Jahre auch einen Platz gefunden. Hier ist das
  Programm in Java (ohne gecurryte Funktionen und partielle Applikation).
  #raw(read("Foldable.java"), block: true, lang: "java")
] <typeclasses_in_python_remark>

#pagebreak()

#remark[
  Das ist ein Lösungsvorschlag für @timer_adt.

  #line(stroke: 0.25pt)

  Beschreibe ```hs Timer``` den zu spezifierenden ADT. Dann sollen für diesen
  folgende Funktionen existieren und Gesetze gelten.

  _Konstruktoren_ \
  - ```hs new :: Int -> Int -> Timer``` erzeugt einen neuen Timer mit einer
    Zeiteingabe bestehend aus Stunden und Minuten.
  _Selektoren_ \
  - ```hs display :: Timer -> (Int, Int)``` gibt die verbleibende auf dem
    Timer an.
  _Testfunktionen_ \
  - ```hs wentOff :: Timer -> Bool``` gibt an, ob der Timer bereits klingelte
  _Operationen_ \
  - ```hs tick :: Timer -> Timer``` lässt eine Sekunde auf einem Timer
    vergehen.

  _Gesetze_ \
  Seien $h in NN_0$, $m in {0, ..., 59}$ und $t$ einer beliebiger Timer.

  - ```hs display (new h s) == (h, s)```
  - ```hs wentOff t <=> display t == (0, 0)```
  - ```hs s > 0            <=> display (tick (new h s)) == (h, s - 1)```
  - ```hs s == 0 && h > 0  <=> display (tick (new h s)) == (h - 1, 59)```
  - ```hs s == 0 && h == 0 ==> display (tick (new h s)) == (0, 0)```
] <timer_adt_solution>

#remark[
  In der Klausur des 1. Prüfungszeitraums WS25/26 gab es eine ADT-Aufgabe, in
  der eine Uhr modelliert werden sollte. Die häufigsten Fehler sind hier auf
  @timer_adt bzw. @timer_adt_solution übertragen.
  - Die Definition von Konstruktoren als Teil ```hs data```-Deklaration des ADT.
    ```hs data Timer = Timer Int Int``` ist bereits eine Implementierung. Eine
    wesentliche Eigenschaft eines ADTs ist, sich über Implementierungsdetails
    keine Gedanken gemacht werden muss (und soll). Dazu gehört die konkrete
    Darstellung des Typen, die wir mit der obigen Deklaration angeben.
    Stattdessen geben wir eine Funktion mit dem Typ ```hs Int -> Int -> Timer```
    als Konstruktor an. So ist die interne Darstellung weiterhin offen.
  Die nächsten Anmerkungen fallen in die Kategorie "Nicht unbedingt falsch,
  aber fehl am Platz". Der ADT soll die Funktionweise einer Schnittstelle
  angeben und sich nicht mit Implementierungsdetails aufhalten.
  - Es kam häufiger vor, dass Hilfsfunktionen angegeben wurden, die die
    Konsistenz des internen Zustands eines Wertes überprüfen sollten --
    möglicherweise aus Motivation heraus, dass so etwas wie z.B.
    ```hs display (new 0 73) == (0, 73)``` nicht passieren kann.
  - Unter der Annahme einer Vergleichsoperation auf Timern gab es z.B. auch
    das Gesetz ```hs t1 == t2 <=> display t1 == display t2```. Es gilt zwar,
    wann zwei Timer gleich sind, aber sagt nichts darüber aus, wie ein Timer
    funktionieren soll.
] <timer_adt_mistakes>

