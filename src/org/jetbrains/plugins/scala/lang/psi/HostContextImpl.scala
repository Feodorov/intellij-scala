package org.jetbrains.plugins.scala
package lang.psi

import scala.reflect.semantic._
import scala.reflect.core._
import scala.collection.immutable.Seq
import scala.reflect.core.Member.Template
import scala.reflect.core.Type.Ref
import scala.reflect.core.Aux.Self
import scala.reflect.semantic.SemanticProfile
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScTemplateDefinition
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScFunctionDefinition

/**
 * @author kfeodorov
 * @since 28.05.14.
 */
class HostContextImpl(c: PalladiumTreeConverter) extends HostContext {

  override def syntaxProfile: SyntaxProfile = ???

  override def supertypes(tpe: Type): Seq[Type] = ???

  override def attrs(tree: Tree): Seq[Attribute] = ???

  override def overrides(member: Member.Term): Seq[Member.Term] = ???

  override def overrides(member: Member.Type): Seq[Member.Type] = ???

  override def linearization(tpes: Seq[Type]): Seq[Type] = ???

  override def ctors(scope: Scope): Seq[Ctor] = ???

  override def <:<(tpe1: Type, tpe2: Type): Boolean = ???

  override def widen(tpe: Type): Type = ???

  override def members(scope: Scope): Seq[Member] =
    c.get(scope) match {
      case Some(t: ScTemplateDefinition) =>
        t.members.flatMap{c.convert}.map(_.asInstanceOf[Member]).toList ++ t.functions.flatMap(c.convert).map(_.asInstanceOf[Member]).toList //TODO val/var/type/inner classes
      case Some(t: ScFunctionDefinition) =>
        t.parameters.flatMap{c.convert}.map(_.asInstanceOf[Member]).toList
      case _ => Nil
    }

  override def members(scope: Scope, name: Name): Seq[Member] = ???

  override def lub(tpes: Seq[Type]): Type = ???

  override def semanticProfile: SemanticProfile = ???

  override def glb(tpes: Seq[Type]): Type = ???

  override def self(tpe: Type): Self = ???

  override def weak_<:<(tpe1: Type, tpe2: Type): Boolean = ???

  override def defn(term: Term.Ref): Seq[Member.Term] = ???

  override def defn(tpe: Ref): Member = ???

  override def erasure(tpe: Type): Type = ???

  override def subclasses(tpe: Type): Seq[Template] = ???

  override def dealias(tpe: Type): Type = ???
}

object HostContext {
  def apply(p: PalladiumTreeConverter) = new HostContextImpl(p)
}
