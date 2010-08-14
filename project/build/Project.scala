import sbt._

class ScalaGraphProject(info: ProjectInfo) extends DefaultProject(info)
{

  val scalatest = "org.scalatest" % "scalatest" % "1.2"

  override def compileOptions = Unchecked :: Deprecation :: Nil

  //override val mainClass = Some("aggreg.Exp")

  override def mainScalaSourcePath = "src"
  override def mainResourcesPath = "resources"
        
  override def testScalaSourcePath = "test-src"
  override def testResourcesPath = "test-resources"


}
