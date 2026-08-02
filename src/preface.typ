#import "prelude.typ": *
#show: config

#heading(numbering: none, outlined: false, [Vorwort])

// #text(0.8em)[
//   Dieses Dokument ist vom #datetime.today().display("[day].[month].[year]"). Die
//   aktuelle Version des Dokuments kannst du im moodle oder
//   #link(git("releases/download/latest/main.pdf"))[direkt von GitHub herunterladen].
//   Dieses Dokument wird ständig aktualisiert.
// ]

#context {
  let (test: (test,), challenge: (challenge,)) = thmcounters.final().counters
  [
    Dieses Dokument enthält #(test + challenge) Fragen und Aufgaben
    unterschiedlicher Größe und andere Ressourcen zum Thema Deklarative
    Programmierung. Die Inhalte dieses Dokuments sollen dir helfen, dein
    Verständnis über Haskell und Prolog zu prüfen und zu stärken.
  ]
}

Die Aufgaben sind grob in drei Schwierigkeitsstufen eingeteilt:
- #nemo-make-titlefmt(1, false)("Stufe 1") #h(0.3em) Aufgaben zur Überprüfung
  grundlegender Kenntnisse. Die Lösung erfolgt durch direktes Anwenden
  bekannter Regeln oder Verfahren und erfordert kein vertieftes Nachdenken.
- #nemo-make-titlefmt(2, false)("Stufe 2") #h(0.3em) Aufgaben, bei denen
  grundlegende Kenntnisse angewendet und durch eigene Überlegungen ergänzt
  werden müssen. Es sind erste Ideen oder einfache Lösungsstrategien notwendig.
- #nemo-make-titlefmt(3, false)("Stufe 3") #h(0.3em) Komplexe Aufgaben, die ein
  vertieftes Verständnis voraussetzen. Die Lösung erfordert mehrere
  Gedankenschritte, das Verknüpfen verschiedener Inhalte sowie eigenständigen
  Lösungsstrategien.

Herausforderndere Aufgaben sind als Challenges gekennzeichet. Sie erfordern
häufig das Verständnis mehrere Konzepte und führen teilweise zusätzliche Inhalte
ein, die speziell für deren Bearbeitung relevant sind. Tests, deren Bearbeitung
voraussichtlich mehr als 10 Minuten dauern, sind mit #hi("clock", solid: false)
markiert, Challenges bei 30min.

In dieses Dokument haben es bereits sehr viele Selbsttests geschafft. Das kann
überwältigend wirken. Um dem etwas entgegenzuwirken und um dir einen ersten
Lernpfad zu zeigen, haben wir eine Auswahl von Aufgaben mit Tier-Emojis
hervorgehoben. Es ist nicht notwendig, alle Tests und Challenges zu arbeiten.
Weiter sind Aufgaben mit den #tag-level-up und #tag-deep-dive markiert. Erstere
gehen über die Lerninhalte der Vorlesung hinaus -- könnten dir aber helfen, die
eigentlichen Lerninhalte der Vorlesung nochmal besser zu verstehen. Letztere
solltest du, je nachdem wie viel Zeit du bereit zu investieren, nur dann
machen, wenn du bereits mit gutem Verständnis dabei bist. Wir hoffen, das hilft
dir bei deiner Orientierung in diesem Dokument.

Für die meisten Selbsttests wird es absehbar keine Lösungen geben. Stattdessen
möchten wir dich ermutigen, deine Lösungen mit anderen Mitstudierenden oder
Mitarbeitenden zu diskutieren, solltest du offene Fragen haben -- oder du
promptest verantwortungsvoll das LLM deiner Wahl. An jeden Abschnitt ist eine
Checkliste zur Selbstevaluation angehängt. Wenn du auf einer geeigneten
Bewertungsskala (z.B. Schulnoten) für dich feststellst, dass du weiterhin
Schwierigkeiten hast, melde dich gerne, damit wir dir helfen können.
Für manche aufwendigere Selbsttests haben wir #link(git("releases/tag/latest"))[Vorlagen und Lösungsvorschläge]
auf GitHub bereitgestellt.

Die Inhalte dieses Dokuments sind nicht vollständig und nicht fehlerfrei. Wir
machen Fehler! Es kann sein, dass Modulinhalte nicht durch Selbsttests,
Referenzen oder Selbstevaluationen abdeckt werden und es nie werden --
insbesondere fehlen derzeitig noch viele Lernziele in den Selbstevaluationen.
Betrachte dieses Dokument immer als "work in progress" und ziehe auch immer
andere Quellen zum Lernen heran.

Wenn du Anmerkungen oder weitere Ideen oder Quellen für Inhalte für dieses
Dokument hast, dann schreibe uns gerne über z.B. mattermost an -- oder
#link(git(""))[erstellt ein issue oder stellt eine PR] auf GitHub.

#pagebreak(weak: true)

#heading(numbering: none, outlined: false, [Abenteuer])

Im Folgenden erwarten dich mehrere Serien aus Tests und Challenges, die dich
in unterschiedliche Gefilde der Informatik entführen. Wir nennen sie Abenteuer
--- und wie bei jeder Expedition geht es nicht darum, stur einer Karte zu
folgen, sondern selbst zu entdecken, zu experimentieren und dabei auch mal vom
Pfad abzukommen. Jedes Abenteuer nimmt dich auf eine Reise durch vertrautes
und unbekanntes Terrain der Informatik.

Dabei soll das Programmierenlernen selbst gar nicht im Vordergrund stehen,
sondern eher zum stillen Begleiter deiner Reise werden: Weil du unterwegs
Programmierkenntnisse gebraucht hast, um voranzukommen, hast du sie ganz
nebenbei geübt und vertieft — als angenehmer Nebeneffekt deiner
Auseinandersetzung mit den eigentlichen Inhalten.

Aktuell kannst du aus den folgenden Abenteuern wählen.

- *Reguläre Sprachen* #h(1em) Begleitend zum Modul "Berechnungen und Logik"
  kannst du in diesem Abenteuer die dort gelernten und neue Konzepte in Haskell
  programmieren. In den Tests und Challenges wird auf alles Notwendige
  verwiesen oder erneut eingeführt. Wir schauen uns das Wortproblem für
  reguläre Ausdrücke und DEAs genauer an. Im Zuge dessen werden wir die
  folgende Konstruktionen implementieren.
  #align(center, pad(y: 1em, {
    cetz.canvas({
      import cetz.draw: *

      let d = 0.25

      let positions = ((2, 4), (4, 2), (2, 0), (0, 2))

      content((2, 4), [Reguläre Ausdrücke])
      content((4, 2), [$epsilon$-NEA])
      content((2, 0), [NEA])
      content((0, 2), [DEA])

      let edges = (
        ((2.0 + d, 4.0 - d), (4.0, 2.0 + d), [Thompson-Konstruktion]),
        ((4.0, 2.0 - d), (2.0 + d, d), [Elimination von $epsilon$-Transitionen]),
        ((2.0 - d, d), (0.0, 2.0 - d), [Potenzmengenkonstruktion]),
        ((0.0, 2.0 + d), (2.0 - d, 4.0 - d), [Zustandseliminierung]),
      )

      for (i, (from, to, label)) in edges.enumerate() {
        let dir = (to.at(0) - from.at(0), to.at(1) - from.at(1))
        let dir-len = calc.sqrt(dir.at(0) * dir.at(0) + dir.at(1) * dir.at(1))
        let normal = (-dir.at(1) / dir-len, dir.at(0) / dir-len)
        let mid = (from.at(0) + dir.at(0) * 0.5, from.at(1) + dir.at(1) * 0.5)
        let bow = 0.3
        let bend = (mid.at(0) + bow * normal.at(0), mid.at(1) + bow * normal.at(1))

        let anchor = if normal.at(0) > 0 { "mid-west" } else { "mid-east" }

        arc-through(from, bend, to, name: str(i), mark: (end: ">"))
        content(str(i) + ".mid", anchor: anchor, padding: 0.5, label)
      }
    })
  }))
- *Numerisches Differenzieren* #h(1em) In der Vorlesung bzw. im Skript bist du
  dem Approximieren der Ableitung durch finite Differenzen bereits beiläufig
  begegnet --- ein erster Fußabdruck auf einem viel größeren Terrain. Auf
  diesem Abenteuer brichst du tiefer auf und lernst symbolisches sowie
  automatisches Differenzieren kennen. Beide Methoden lassen sich überraschend
  elegant in Haskell umsetzen. Dabei erkundest du die Typklasse ```hs Num```
  und ihre Subtypklassen aus nächster Nähe. Am Ende deiner Reise wartet die
  Implementierung des Gradientenverfahrens
- *Lineare Gleichungssysteme* #h(1em) Das gaußsche Eliminationsverfahren kennst
  du voraussichtlich bereits aus der Schule. In diese Abenteuer schauen wir uns
  die LR-Zerlegung regulärer Matrizen an und ergründen, wie sie uns hilft,
  verschiedene Probleme der linearen Algebra zu lösen. Die Werkzeuge, die du
  dir dabei erarbeitest, wirst du unterwegs immer wieder auch für andere Zwecke
  einsetzen können.
- *Satz über rationale Nullstellen* #h(1em) Dieses Abenteuer schickt dich auf
  eine Reise von ganzzahligen Polynomen über unimodularen Matrizen zu bis hinzu
  Eigenwerten. Dabei treiben wir den Erkenntnisgewinn des Satzes über rationale
  Nullstellen auf die Spitze.

#pagebreak(weak: true)
