fork in run := true

javaOptions in run ++= Seq("-XX:+UseConcMarkSweepGC", "-Xms2G", "-Xmx2G" ,"-XX:MaxPermSize=512m")