package net.cucumbersome.pascallInterpreter.io

trait Console[F[_]] {
  def readLine: F[String]
  def writeLine: F[Unit]
}
