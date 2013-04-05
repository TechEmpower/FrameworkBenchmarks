import sbt._
import Keys._
import sbtassembly.Plugin.AssemblyKeys._

object Deploy {
  val deployPackTask = TaskKey[Unit]("deploy-pack")
  val deployPack = deployPackTask <<= (assembly, jarName in assembly, target, baseDirectory) map { (_, jarName, target, base) =>
    IO.delete(target / "deploy")
    IO.createDirectory(target / "deploy")
    IO.copyFile(target / jarName, target / "deploy" / jarName)
    if (base / "server.conf" exists)
      IO.copyFile(base / "server.conf", target / "deploy" / "server.conf")
    IO.write(target / "deploy" / "start.sh",
      """#!/bin/bash
        |DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd)"
        |cd $DIR
        |java -Dfile.encoding=UTF8 -jar %s
        |""" format (jarName) stripMargin)
    target / "deploy" / "start.sh" setExecutable true
  }
  
  val deployHost = SettingKey[Option[String]]("deployHost", "default host, on which deployed files will be pushed")
  val deployDest = SettingKey[Option[String]]("deployDest", "default destination on that host")
  
  val deploySsh = InputKey[Unit]("deploy-ssh") <<= inputTask { (argTask: TaskKey[Seq[String]]) =>
    (argTask, deployPackTask, deployHost, deployDest, jarName in assembly, target) map { (args, _, deployHost, deployDest, jarName, target) => 
      val (host, dest) = if (args.size < 2) {
        (for {
          host <- deployHost
          dest <- deployDest
        } yield (host, dest)).getOrElse(sys.error("Destination was not provided on command line - and there was no default"))
      } else (args(0), args(1))
      val cmd = "rsync" +: "-avz" +: IO.listFiles(target / "deploy").map(_.toString) :+ (host+":"+dest)
      println("Copying files: " + cmd.mkString(" "))
      if (Process(cmd).! == 0) {
        val startCmd = List("ssh", host, "-x", "cd " + dest + "; (nohup ./start.sh > server.log 2>&1 &)")
        println("Starting process: " + startCmd.mkString(" "))
        Process(startCmd).!
      }
    }
  }
  
  lazy val deploySettings = Seq(deployPack, deployHost := None, deployDest := None, deploySsh)
}
