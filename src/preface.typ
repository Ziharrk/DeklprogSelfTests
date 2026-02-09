#import "prelude.typ": *
#show: config

#text(0.8em)[
  Dieses Dokument ist vom #datetime.today().display("[day].[month].[year]"). Die
  aktuelle Version des Dokuments kannst du im moodle oder
  #link(git("raw/refs/heads/main/main.pdf"))[direkt von GitHub herunterladen].
  Dieses Dokument wird ständig aktualisiert.
]

#context {
  let (test: (_, test), challenge: (_, challenge)) = thmcounters.final().counters
  [
    Dieses Dokument enthält #test Fragen, #challenge kleinere bis
    größere Aufgaben und andere Ressourcen zum Thema Deklarative Programmierung.
    Die Inhalte dieses Dokuments sollen dir helfen, dein Verständnis über
    Haskell und Prolog zu prüfen und zu stärken.
  ]
}

Größere Aufgaben haben wir als Challenges markiert. Diese Aufgaben benötigen
öfter mehrere Konzepte und führen zusätzlich Konzepte ein, die nur für das
Lösen der Aufgabe wichtig sind. Wenn die zusätzlichen Konzepte, dir zu sehr
Schwierigkeiten bereiten, überspringe die entsprechende Frage oder Aufgabe.
Die Nomenklatur der Aufgaben ist aktuell vielleicht noch etwas willkürlich,
da es Tests gibt, die wie Challenges wirken -- und möglicherweise sogar
andersherum. Als zusätzliche Hilfestellung ist die Schwierigkeit jeder Aufgabe
grob in drei Stufen aufgeteilt, die sich wie folgt auszeichnen sollen.
- #titlefmt(1, false)("Stufe 1") #h(0.3em) Aufgaben zur Überprüfung
  grundlegender Kenntnisse. Die Lösung erfolgt durch direktes Anwenden bekannter
  Regeln oder Verfahren und erfordert kein vertieftes Nachdenken.
- #titlefmt(2, false)("Stufe 2") #h(0.3em) Aufgaben, bei denen grundlegende
  Kenntnisse angewendet und durch eigene Überlegungen ergänzt werden müssen. Es
  sind erste Ideen oder einfache Lösungsstrategien notwendig.
- #titlefmt(3, false)("Stufe 3") #h(0.3em) Komplexe Aufgaben, die ein vertieftes
  Verständnis voraussetzen. Die Lösung erfordert mehrere Gedankenschritte, das
  Verknüpfen verschiedener Inhalte sowie eigenständigen Lösungsstrategien.
Tests, von denen wir erwarten, dass sie euch länger als 10min beschäftigen,
sind mit #hi("clock", solid: false) markiert, Challenges bei 30min.

Für die Selbsttests wird es absehbar keine Lösungen geben. Stattdessen möchten
wir dich ermutigen, deine Lösungen mit anderen Mitstudierenden oder
Mitarbeitenden zu diskutieren, solltest du offene Fragen haben -- oder du
promptest verantwortungsvoll das LLM deiner Wahl. An jeden Abschnitt ist eine
Checkliste zur Selbstevaluation angehängt. Wenn du auf einer geeigneten
Bewertungsskala (z.B. Schulnoten) für dich feststellst, dass du weiterhin
Schwierigkeiten hast, melde dich gerne, damit wir dir helfen können.

Die Inhalte dieses Dokuments sind nicht vollständig und nicht fehlerfrei. Wir
machen Fehler! Es kann sein, dass Modulinhalte nicht durch Selbsttests,
Referenzen oder Selbstevaluationen abdeckt werden und es nie werden --
insbesondere fehlen derzeitig noch viele Lernziele in den Selbstevaluationen.
Betrachte dieses Dokument immer als "work in progress" und ziehe auch immer
andere Quellen zum Lernen heran.

Wenn du Anmerkungen oder weitere Ideen oder Quellen für Inhalte für dieses
Dokument hast, dann schreibe uns gerne über z.B. mattermost an -- oder
#link(git(""))[erstellt ein issue oder
stellt eine PR auf GitHub].

#pagebreak(weak: true)
