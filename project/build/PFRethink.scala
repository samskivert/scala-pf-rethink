import sbt._

class PFRethink (info :ProjectInfo) extends DefaultProject(info) {
  override def fork = forkRun(
    "-Xbatch" :: "-XX:CICompilerCount=1" :: "-XX:+PrintCompilation" :: "-verbosegc" :: Nil)
}
