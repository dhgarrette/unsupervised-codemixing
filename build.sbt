// import AssemblyKeys._ // put this at the top of the file

name := "unsupervised-codemixing"

organization := "dhg"

version := "0.0.1"

scalaVersion := "2.12.4"

resolvers ++= Seq(
  "dhg releases repo" at "http://www.dhgarrette.com/maven-repository/releases",
  "dhg snapshot repo" at "http://www.dhgarrette.com/maven-repository/snapshots",
  "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
  "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases",
  "OpenNLP repo" at "http://opennlp.sourceforge.net/maven2"
)

libraryDependencies ++= Seq(
  "dhg" % "scala-util_2.12" % "0.0.4-SNAPSHOT",
  "org.scalanlp" % "breeze_2.12" % "1.0-RC2",
  "org.scalanlp" % "breeze-natives_2.12" % "1.0-RC2",
  "org.apache.opennlp" % "opennlp-tools" % "1.5.2-incubating",
  "org.scalanlp" % "junto_2.12" % "1.6.2-SNAPSHOT",
  "org.jgrapht" % "jgrapht-jdk1.5" % "0.7.3",
  "junit" % "junit" % "4.12" % "test",
  "com.novocode" % "junit-interface" % "0.11" % "test") //switch to ScalaTest at some point...

// seq(assemblySettings: _*)

//mainClass in assembly := Some("")

//mainClass in (Compile, run) := None

// test in assembly := {}

scalacOptions ++= Seq("-deprecation", "-feature")

initialCommands in console := "import dhg.util._, scalaz._, Scalaz._"


// Code Coverage

// ScoverageKeys.highlighting := true

// ScoverageKeys.excludedPackages in ScoverageCompile := "Test;dhg.ccg.Sandbox;dhg.ccg.ccm;dhg.ccg.tag.*HmmTests;dhg.ccg.gen;dhg.ccg.dd;dhg.ccg.parse.ccm;dhg.ccg.ihmm;dhg.ccg.parse.exp.TaggerExp;dhg.ccg.prob.*ExpProbabilityDistribution;dhg.ccg.prob.AddLambdaSmoother;dhg.ccg.run.*;dhg.ccg.util.TreeViz;dhg.ccg.util.VizTree;dhg.ccg.util.DrawMatrix;dhg.ccg.z;opennlp.tools.postag;dhg.ccg.tag.Hmm;dhg.ccg.tag.HmmTagger;dhg.ccg.tag.MemmTagger;dhg.ccg.tag.*TaggerTrainer;dhg.ccg.tag.Tagger;dhg.ccg.tag.TaggerEvaluator;dhg.ccg.tag.TypeShiftingTagDictionary;dhg.ccg.tag.WeightedTagger;dhg.ccg.parse.cpcfg.exp.*"
