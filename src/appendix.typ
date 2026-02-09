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
  Haskell-Spracherweiterungen können wir eine allgemeinere Funktion angeben. Das
  ist in #link("blob/main/AutoDiff.hs", raw("AutoDiff.hs")) im GitHub-Repository
  demonstriert. Mit ```hs derive @3 cos (2.0 :: Double)``` können wir jetzt z.B.
  die dritte Ableitung des Cosinus an der Stelle $2$ berechnen.
] <reverse_mode_ad_remark>

#remark[
  In @typeclasses_in_python haben wir gesehen, wie deklarative
  Programmierkonzepte aus Haskell in Python verwendet werden können. In Java
  haben diese über die letzten Jahre auch einen Platz gefunden. Hier ist das
  Programm in Java (ohne gecurryte Funktionen und partielle Applikation).
  #raw(read("Foldable.java"), block: true, lang: "java")
] <typeclasses_in_python_remark>

