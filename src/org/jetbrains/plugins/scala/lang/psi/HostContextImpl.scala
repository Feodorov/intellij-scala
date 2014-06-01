package org.jetbrains.plugins.scala
package lang.psi

import scala.reflect.semantic._
import scala.reflect.core._
import scala.collection.immutable.Seq
import scala.reflect.core.Member.Template
import scala.reflect.core.Aux.Self
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScTemplateDefinition
import org.jetbrains.plugins.scala.lang.psi.api.statements.{ScTypeAlias, ScFun, ScFunctionDefinition}
import com.intellij.psi.{PsiClass, PsiNamedElement, PsiElement}
import scala.reflect.semantic.SemanticProfile
import scala.reflect.core.SyntaxProfile
import scala.Some
import org.jetbrains.plugins.scala.lang.psi.api.base.ScConstructor
import scala.reflect.core
import org.jetbrains.plugins.scala.lang.resolve.ResolvableReferenceElement
import org.jetbrains.plugins.scala.lang.psi.types.{Bounds, ScType, Conformance, Any}
import org.jetbrains.plugins.scala.lang.psi.api.base.patterns.ScPattern
import org.jetbrains.plugins.scala.lang.psi.types.result.TypingContext

/**
 * @author kfeodorov
 * @since 28.05.14.
 */
class HostContextImpl(c: PalladiumTreeConverter) extends HostContext {

  override def syntaxProfile: SyntaxProfile = SyntaxProfile()

  override def semanticProfile: SemanticProfile = SemanticProfile(
    dynamics = false,
    postfixOps = false,
    reflectiveCalls = false,
    implicitConversions = false,
    higherKinds = false,
    existentials = false,
    macros = true)

  override def supertypes(tpe: Type): Seq[Type] = c.get(tpe) match {
    case Some(t: PsiClass) => t.getSupers.map(c.convert).map(_.get.asInstanceOf[Type]).toList
    case _ => Nil
  }

  override def attrs(tree: Tree): Seq[Attribute] = ???

  override def overrides(member: Member.Term): Seq[Member.Term] = ???

  override def overrides(member: Member.Type): Seq[Member.Type] = ???

  override def subclasses(tpe: Type): Seq[Template] = ???

  override def erasure(tpe: Type): Type = tpe //TODO ??? Create new ScType with the same name?

  override def linearization(tpes: Seq[Type]): Seq[Type] = tpes.map(c.get).flatten.sortWith {
    case (t1: ScType, t2: ScType) => Conformance.conforms(t1, t2, checkWeak = false)
    case _ => false
  }.map{c.convert}.flatten.map(_.asInstanceOf[Type])

  override def ctors(scope: Scope): Seq[Ctor] = c.get(scope) match {
    case Some(t: ScConstructor) => c.convert(t).map(_.asInstanceOf[Ctor]).toList
    case _ => Nil
  }

  override def <:<(tpe1: Type, tpe2: Type): Boolean = subtypingCheck(tpe1, tpe2, isWeak = false)

  override def weak_<:<(tpe1: Type, tpe2: Type): Boolean = subtypingCheck(tpe1, tpe2, isWeak = true)

  override def widen(tpe: Type): Type = c.get(tpe).map{
    case s: ScFun => s.retType
    case s: ScPattern => s.getType(TypingContext.empty).getOrAny
    case _ => Any
  }.flatMap{c.convertType}.getOrElse(tpe)

  override def members(scope: Scope): Seq[Member] = findMembersWithFilter(scope, _ => true)

  override def members(scope: Scope, name: Name): Seq[Member] = findMembersWithFilter(scope, {
    case p: PsiNamedElement => p.getName == name.value
    case _ => false
  })

  override def lub(tpes: Seq[Type]): Type = c.convertType(Bounds.lub(tpes.map(c.get).flatten.map(_.asInstanceOf[ScType]))).getOrElse(tpes.head) //TODO getOrElse(Any)

  override def glb(tpes: Seq[Type]): Type = c.convertType(Bounds.glb(tpes.map(c.get).flatten.map(_.asInstanceOf[ScType]), checkWeak = false)).getOrElse(tpes.head) //TODO getOrElse(Any)

  override def self(tpe: Type): Self = c.get(tpe).flatMap{
    case t: ScTemplateDefinition => t.selfType
    case _ => None
  }.flatMap(e => c.convertType(e).asInstanceOf[Option[Aux.Self]]).getOrElse(Aux.Self(None, None)(hasThis = true))

  override def dealias(tpe: Type): Type = c.get(tpe).flatMap{
    case t: ScTypeAlias => Option(t.getOriginalElement)
    case _ => None
  }.flatMap(e => c.convert(e).asInstanceOf[Option[Type]]).getOrElse(tpe)

  private def subtypingCheck(tpe1: Type, tpe2: Type, isWeak: Boolean): Boolean = {for {
    t1 <- c.get(tpe1)
    t2 <- c.get(tpe2)
  } yield (t1, t2)} match {
    case Some((t1: ScType, t2: ScType)) => Conformance.conforms(t1, t2, checkWeak = isWeak)
    case _ => false
  }

  private def findMembersWithFilter(scope: Scope, filter: PsiElement => Boolean) = {
    c.get(scope) match {
      case Some(t: ScTemplateDefinition) =>
        t.members.flatMap{c.convert}.map(_.asInstanceOf[Member]).toList ++ t.functions.flatMap(c.convert).map(_.asInstanceOf[Member]).toList //TODO val/var/type/inner classes
      case Some(t: ScFunctionDefinition) =>
        t.parameters.flatMap{c.convert}.map(_.asInstanceOf[Member]).toList
      case _ => Nil
    }
  }

  override def owner(tree: Tree): Scope = c.get(tree).map(_.getContext).flatMap{c.convert}.map(_.asInstanceOf[Scope]).get //TODO getOrElse(Scope.empty)?

  override def defns(ref: core.Ref): Seq[Member] = c.get(ref) match {
    case Some(t: ResolvableReferenceElement) => t.multiResolve(true).map{r => c.convert(r.getElement)}.map(_.asInstanceOf[Member]).toList
    case _ => Nil
  }
}

object HostContext {
  def apply(p: PalladiumTreeConverter) = new HostContextImpl(p)
}
