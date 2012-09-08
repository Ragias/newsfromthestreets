name := "News from the Streets"

scalaVersion := "2.9.1"

version := "1.0"

// you can also add multiple repositories at the same time
resolvers ++= Seq(
  "Scala Tools Snapshot" at "http://scala-tools.org/repo-releases/",
  "Scala Tools Snapshot" at "http://scala-tools.org/repo-snapshots/",
  "Java.net Maven2 Repository" at "http://download.java.net/maven/2/",
  "Sonatype snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
  "Amazon" at "http://repo1.maven.org/maven2/",
  "liftmodules repository" at "https://repository-liftmodules.forge.cloudbees.com/release/"
)

  resolvers += ScalaToolsSnapshots

// if you have issues pulling dependencies from the scala-tools repositories (checksums don't match), you can disable checksums
//checksums := Nil
libraryDependencies ++= {
  val liftVersion = "2.5-SNAPSHOT" // Put the current/latest lift version here
  Seq( 
   "net.liftweb" %% "lift-widgets" % liftVersion % "compile->default",
    "net.liftweb" %% "lift-webkit" % liftVersion % "compile->default",
    "net.liftweb" %% "lift-mapper" % liftVersion % "compile->default",
    "net.liftmodules" %% "mongoauth" % "2.4-0.3" % "compile->default",
    "net.liftweb" %% "lift-mongodb-record" % liftVersion % "compile->default",
    "net.liftweb" %% "lift-textile" % liftVersion % "compile->default",
    "net.liftweb" %% "lift-mongodb" % liftVersion % "compile->default",
    "net.liftweb" %% "lift-wizard" % liftVersion % "compile->default")
}

// when using the sbt web app plugin 0.2.4+, use "container" instead of "jetty" for the context
// Customize any further dependencies as desired
libraryDependencies ++= Seq(
  "org.eclipse.jetty" % "jetty-webapp" % "8.0.4.v20111024" % "container",
  "org.scala-tools.testing" % "specs_2.9.0" % "1.6.8" % "test", // For specs.org tests
  "junit" % "junit" % "4.8" % "test->default", // For JUnit 4 testing
  "cc.co.scala-reactive" %% "reactive-web" % "0.2-SNAPSHOT",
  "javax.servlet" % "servlet-api" % "2.5" % "provided->default",
  "com.amazonaws" % "aws-java-sdk" % "1.2.9",
  "org.mindrot" % "jbcrypt" % "0.3m" % "compile->default",
  "com.h2database" % "h2" % "1.2.138", // In-process database, useful for development systems
  "com.foursquare" %% "rogue" % "1.1.7" intransitive(),
  "ch.qos.logback" % "logback-classic" % "0.9.26" % "compile->default", // Logging
  "com.fmpwizard" %% "lift-named-comet" % "0.3"
)

seq(webSettings: _*)

checksums := Nil

// If using JRebel with 0.1.0 of the sbt web plugin
//jettyScanDirs := Nil
// using 0.2.4+ of the sbt web plugin

scanDirectories in Compile := Nil

