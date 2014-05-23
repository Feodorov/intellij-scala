package org.jetbrains.plugins.scala
package lang.palladium

/**
 * @author kfeodorov
 * @since 23.05.14.
 */
class TestLiterals extends TreeTestBase {
  override def folderPath: String = super.folderPath + "literals/"

  def testTrue() { doTest() }
  def testFalse() { doTest() }
  def testInt() { doTest() }
  def testLong() { doTest() }
  def testDouble() { doTest() }
  def testFloat() { doTest() }
  def testChar() { doTest() }
  def testString() { doTest() }
  def testSymbol() { doTest() }
  def testNull() { doTest() }
  def testUnit() { doTest() }
}
