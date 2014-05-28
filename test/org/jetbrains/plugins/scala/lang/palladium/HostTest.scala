package org.jetbrains.plugins.scala
package lang.palladium

import org.jetbrains.plugins.scala.base.ScalaLightPlatformCodeInsightTestCaseAdapter
import org.jetbrains.plugins.scala.util.TestUtils
import java.io.File
import com.intellij.openapi.util.io.FileUtil
import com.intellij.openapi.vfs.CharsetToolkit
import com.intellij.openapi.util.text.StringUtil
import org.jetbrains.plugins.scala.lang.psi.api.ScalaFile
import org.jetbrains.plugins.scala.lang.psi.HostContextImpl

import scala.reflect.core._, Aux._

/**
 * @author kfeodorov
 * @since 28.05.14.
 */
class HostTest extends ScalaLightPlatformCodeInsightTestCaseAdapter {
  def filePath: String = TestUtils.getTestDataPath + "/parser/data/trees/Context.scala"

  def testSimple() {
    val ioFile: File = new File(filePath)
    var fileText: String = FileUtil.loadFile(ioFile, CharsetToolkit.UTF8)
    fileText = StringUtil.convertLineSeparators(fileText)
    configureFromFileTextAdapter(ioFile.getName, fileText)
    val scalaFile: ScalaFile = getFileAdapter.asInstanceOf[ScalaFile]

    val root = scalaFile.getFirstChild
    val host = new HostContextImpl(root)
    val typeDef = Defn.Def(Nil, Term.Name("Test")(false), Nil, Nil, Nil, None, Lit.Unit())
    val res = host.members(typeDef)
    assert(res.nonEmpty)
  }
}
