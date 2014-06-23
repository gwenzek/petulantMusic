// scalaVersion := "2.10"

name := "PetulantMusic"

version := "1.0"

libraryDependencies ++= Seq(
    "org.scalanlp" %% "breeze" % "0.8.1",
    "org.scalanlp" %% "breeze-natives" % "0.8.1",
    "org.scalanlp" %% "breeze-viz" % "0.8"
)

libraryDependencies += "com.gwenzek.scrimage" %% "scrimage-core" % "1.4.0"

libraryDependencies += "com.intellij" % "forms_rt" % "6.0.5"

scalacOptions ++= Seq("-deprecation", "-feature")

// conflictManager := ConflictManager.strict