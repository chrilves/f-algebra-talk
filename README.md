# Code source du [PSUG #64](https://www.meetup.com/fr-FR/Paris-Scala-User-Group-PSUG/events/232665213/)
## Voyage au pays des Case Class
### Algèbre abstraite pour dévelopeu.r.se concrèt.e


Ce sont les sources du 64ième meetup du [Paris Scala User Group](http://www.meetup.com/fr-FR/Paris-Scala-User-Group-PSUG).

### Présentation

La présentation est dans le fichier [slides/fr.md](https://github.com/christophe-calves/psug-2016.7/blob/master/slides/fr.md), lisible avec [reveal-md](https://github.com/webpro/reveal-md) : `cd slides && reveal-md fr.md` .

### Sources Scala

Les sources [Scala](http://www.scala-lang.org/) suivent scrupuleusement la présentation. Elles sont organisées en packages numérotés correspondants aux chapitres de la présentation.

Le code *Scala* complète à plus d'un titre la présentation:
- Les méthodes `equals` et `hasCode`, nécessaires pour simuler les `case class` et `sealed trait`, y sont implémentées. Il n'était pas judicieux de les inclure aux slides, mais je conseille très vivement leur lecture. C'est un cas non trivial d'utilisation de `fold`.
- Le code utilise massivement [ScalaCheck](https://www.scalacheck.org/) et [Discipline](https://github.com/typelevel/discipline), pour vérifier que les structures et morphismes définis respectent bien les propriétés annoncées. Le jeu de tests s'execute avec `sbt test`.
- Lancer le projet avec `sbt run` illustera l'utilisation de la *co-algèbre* `Labyrinthe` au travers d'un petit jeux.
- Pour les curieux, le fichier `coq/listealg_monoid_noniso.v`, écrit en [Coq](https://coq.inria.fr/), montrent une propriété intéressante entre listes et monoides.

Pour toutes questions ou discussion, contactez moi via [@chrilves](http://twitter.com/chrilves) ou par mail.

**Happy Hacking ;)**
