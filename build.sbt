name := "some_questions"

version := "1.0"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/maven-releases"

libraryDependencies ++= Seq("org.scalaz" %% "scalaz-core" % "6.0.4",
                          "com.yammer.metrics" %% "metrics-scala" % "2.1.2")
