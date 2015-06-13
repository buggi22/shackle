* Compile: `sbt compile`
* Test: `sbt test`
* Package: `sbt package`
* Make available for other (local) Scala projects: `sbt publishLocal`

To use in another Scala project:
```
libraryDependencies += "shackle" %% "shackle" % "0.1_SNAPSHOT"
```
(Note: to use this, you must first have run `sbt publishLocal` from within the shackle project on the same machine)
