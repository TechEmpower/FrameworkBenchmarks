#!/bin/bash

#setup_util.replace_text("gemini/Docroot/WEB-INF/GeminiHello.conf", "db.ConnectString = .*:3306", "db.ConnectString = " + args.database_host + ":3306")
#setup_util.replace_text("gemini/Docroot/WEB-INF/resin.xml", "root-directory=\".*\/FrameworkBenchmarks/gemini", "root-directory=\"%s" % args.troot)
  
mkdir -p Docroot/WEB-INFclasses
ant compile
$RESIN_HOME/bin/resinctl -conf $TROOT/Docroot/WEB-INF/resin.xml start