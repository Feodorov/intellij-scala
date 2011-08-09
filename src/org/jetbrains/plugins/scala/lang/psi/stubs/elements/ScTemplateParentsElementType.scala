package org.jetbrains.plugins.scala
package lang
package psi
package stubs
package elements

import api.base.ScModifierList
import api.toplevel.templates.ScTemplateParents
import com.intellij.util.io.StringRef
import impl.ScTemplateParentsStubImpl
import com.intellij.psi.stubs.{StubElement, IndexSink, StubOutputStream, StubInputStream}
import com.intellij.psi.PsiElement
/**
 * User: Alexander Podkhalyuzin
 * Date: 17.06.2009
 */

abstract class ScTemplateParentsElementType[Func <: ScTemplateParents](debugName: String)
        extends ScStubElementType[ScTemplateParentsStub, ScTemplateParents](debugName) {
  def serialize(stub: ScTemplateParentsStub, dataStream: StubOutputStream) {
    val array = stub.getTemplateParentsTypesTexts
    dataStream.writeByte(array.length)
    for (s <- array) {
      dataStream.writeName(s)
    }
  }

  def createStubImpl[ParentPsi <: PsiElement](psi: ScTemplateParents, parentStub: StubElement[ParentPsi]): ScTemplateParentsStub = {
    new ScTemplateParentsStubImpl(parentStub, this, psi.typeElements.map(_.getText).toArray)
  }

  def deserializeImpl(dataStream: StubInputStream, parentStub: Any): ScTemplateParentsStub = {
    val length = dataStream.readByte
    val res = new Array[StringRef](length)
    for (i <- 0 until length) {
      res(i) = dataStream.readName
    }
    new ScTemplateParentsStubImpl(parentStub.asInstanceOf[StubElement[PsiElement]], this, res)
  }

  def indexStub(stub: ScTemplateParentsStub, sink: IndexSink) {}
}