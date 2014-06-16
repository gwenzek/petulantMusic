scalaVersion := "2.11.0"

name := "PetulantMusic"

version := "1.0"

libraryDependencies ++= Seq(
    "org.scalanlp" %% "breeze" % "0.8.1",
    "org.scalanlp" %% "breeze-natives" % "0.8.1",
    "org.scalanlp" %% "breeze-viz" % "0.8"
)

libraryDependencies += "com.sksamuel.scrimage" %% "scrimage-core" % "1.3.21"

libraryDependencies += "com.sksamuel.scrimage" %% "scrimage-filters" % "1.3.21" changing()

libraryDependencies += "com.intellij" % "forms_rt" % "6.0.5"

scalacOptions ++= Seq("-deprecation", "-feature")