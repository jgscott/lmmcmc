name := "lmmcmc"

version := "0.0.1"

scalaVersion := "2.9.2"

resolvers ++= Seq(
  "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
)

libraryDependencies ++= Seq(
  "org.scalanlp" % "breeze-learn_2.9.2" % "0.2-SNAPSHOT" changing() withSources()
)
