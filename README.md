* Compile: `sbt compile`
* Test: `sbt test`
* Package: `sbt package`
* Make available for other (local) Scala projects: `sbt publishLocal`

To use this library in another Scala project, add the following to your `build.sbt` file:
```
libraryDependencies += "shackle" %% "shackle" % "0.1-SNAPSHOT"
```
(Note: you must first run `sbt publishLocal` from within the shackle project on the same machine)
