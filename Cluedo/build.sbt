// Set the project name to the string 'My Project'
name := "cluedo"

// The := method used in Name and Version is one of two fundamental methods.
// The other method is <<=
// All other initialization methods are implemented in terms of these.
version := "1.0"

// Add a single dependency
//libraryDependencies += "org.scala-tools.testing" % "specs-library" % "1.6.5-2.8.0" % "test"


// Add multiple dependencies
libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-swing" % "2.8.1"
)
