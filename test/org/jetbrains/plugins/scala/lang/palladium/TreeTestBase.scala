package org.jetbrains.plugins.scala
package lang.palladium

import scala.reflect.core._,  Aux._
import org.jetbrains.plugins.scala.lang.psi.api.ScalaFile
import org.jetbrains.plugins.scala.lang.psi.PalladiumTreeConverter
import PalladiumTreeConverter._
import com.intellij.openapi.util.io.FileUtil
import org.jetbrains.plugins.scala.base.ScalaLightPlatformCodeInsightTestCaseAdapter
import org.jetbrains.plugins.scala.util.TestUtils
import java.io.File
import com.intellij.openapi.vfs.CharsetToolkit
import com.intellij.openapi.util.text.StringUtil
import org.jetbrains.plugins.scala.lang.lexer.ScalaTokenTypes

/**
 * @author kfeodorov
 * @since 22.05.14
 */
abstract class TreeTestBase extends ScalaLightPlatformCodeInsightTestCaseAdapter {
  protected def folderPath: String = TestUtils.getTestDataPath + "/parser/data/trees/"

  protected def doTest() {
    import org.junit.Assert._

    val filePath = folderPath + getTestName(false) + ".scala"
    val ioFile: File = new File(filePath)
    var fileText: String = FileUtil.loadFile(ioFile, CharsetToolkit.UTF8)
    fileText = StringUtil.convertLineSeparators(fileText)
    configureFromFileTextAdapter(ioFile.getName, fileText)
    val scalaFile: ScalaFile = getFileAdapter.asInstanceOf[ScalaFile]

    PalladiumTreeConverter().convert(scalaFile.getFirstChild) match {
      case Some(res) =>
        val lastPsi = scalaFile.findElementAt(scalaFile.getText.length - 1)
        val text = lastPsi.getText
        val ref = lastPsi.getNode.getElementType match {
          case ScalaTokenTypes.tLINE_COMMENT => text.substring(2).trim
          case ScalaTokenTypes.tBLOCK_COMMENT | ScalaTokenTypes.tDOC_COMMENT => text.substring(2, text.length - 2).trim
          case _ => assertTrue("Test result must be in last comment statement.", false)
        }
        assertTrue(s"${res.toString()} was not equal to expected $ref", ref == res.toString)
      case None => assertTrue("Cannot convert", false)
    }
  }
}
