package org.jetbrains.plugins.scala
package lang.psi

import scala.reflect.core._
import scala.reflect.core.Aux.{Param, TypeBounds, TypeParam}
import org.jetbrains.plugins.scala.lang.psi.api.base.{ScPatternList, ScLiteral}
import org.jetbrains.plugins.scala.lang.lexer.ScalaTokenTypes
import com.intellij.psi.PsiElement
import org.jetbrains.plugins.scala.lang.psi.api.statements.{ScFunctionDefinition, ScTypeAliasDefinition, ScVariableDefinition, ScPatternDefinition}
import scala.reflect.ClassTag
import org.jetbrains.plugins.scala.lang.psi.api.base.types.{ScTypeArgs, ScParameterizedTypeElement, ScTypeElement}
import org.jetbrains.plugins.scala.lang.psi.api.base.patterns.{ScTypedPattern, ScReferencePattern, ScParenthesisedPattern}
import org.jetbrains.plugins.scala.lang.psi.api.expr.{ScInfixExpr, ScExpression, ScUnitExpr}
import org.jetbrains.plugins.scala.lang.psi.api.statements.params._
import org.jetbrains.plugins.scala.lang.psi.types.result.TypingContext
import com.intellij.psi.tree.{IElementType, TokenSet}
import scala.collection.immutable.Seq
import com.intellij.lang.ASTNode
import scala.Some
import scala.reflect.core.Term.ApplyInfix
import org.jetbrains.plugins.scala.lang.psi.types.ScType

/**
 * @author kfeodorov
 * @since 23.05.14.
 */
class PalladiumTreeConverter {
  import MiscUtils._
  private val tree2psi = scala.collection.mutable.Map[Tree, PsiElement]()

  def get(t: Tree): Option[PsiElement] = tree2psi.get(t)

  def convertType(t: ScType): Option[Type] = None

  def convert(element: PsiElement): Option[Tree] = element match {
    //LitSuite
    case e: ScLiteral => convertLiteral(Some(e))
    case _: ScUnitExpr => Some(Lit.Unit())
    //DefnSuite
    case e: ScParameter => convertParameter(Some(e))
    case e: ScPatternDefinition => convertPatternDefn(Some(e))
    case e: ScVariableDefinition => convertVariableDefn(Some(e))
    case e: ScTypeAliasDefinition => convertTypeAliasDefn(Some(e))
    case e: ScFunctionDefinition => convertFunctionDefn(Some(e))
    case e: ScExpression => convertExpression(Some(e))
    case _ => None
  }

  private def add2mapAndReturn[T](psiOpt: Option[PsiElement], treeOpt: Option[T]): Option[T] = {
    treeOpt match {
      case Some(t: Tree) => psiOpt.foreach(p => tree2psi(t) = p)
      case _ =>
    }
    treeOpt
  }

  private def convertLiteral(element: Option[ScLiteral]): Option[Lit] = element flatMap { e =>
    val value = e.getValue
    e.getFirstChild.getNode.getElementType match {
      case ScalaTokenTypes.kTRUE => add2mapAndReturn(element, Some(Lit.Bool(value = true)))
      case ScalaTokenTypes.kFALSE => add2mapAndReturn(element, Some(Lit.Bool(value = false)))
      case ScalaTokenTypes.tINTEGER => value match {
        case v: java.lang.Integer => add2mapAndReturn(element, Some(Lit.Int(v)))
        case v: java.lang.Long => add2mapAndReturn(element, Some(Lit.Long(v)))
      }
      case ScalaTokenTypes.tFLOAT => value match {
        case v: java.lang.Double => add2mapAndReturn(element, Some(Lit.Double(v)))
        case v: java.lang.Float => add2mapAndReturn(element, Some(Lit.Float(v)))
      }
      case ScalaTokenTypes.tCHAR => add2mapAndReturn(element, Some(Lit.Char(value.asInstanceOf[java.lang.Character])))
      case ScalaTokenTypes.tMULTILINE_STRING | ScalaTokenTypes.tSTRING | ScalaTokenTypes.tWRONG_STRING=> add2mapAndReturn(element, Some(Lit.String(e.getText)))
      case ScalaTokenTypes.tSYMBOL => add2mapAndReturn(element, Some(Lit.Symbol(scala.Symbol(e.getText))))
      case ScalaTokenTypes.kNULL => add2mapAndReturn(element, Some(Lit.Null()))
      case _ => None
    }
  }

  private def convertPatternDefn(element: Option[ScPatternDefinition]): Option[Defn] = element flatMap { e =>
    sealed trait BasePat
    case class SimplePat(value: String) extends BasePat
    case class ParenthesisPat(lhs: String, rhs: String) extends BasePat

    findFirstChildByClass[ScPatternList](e).map{_.patterns.map {
      case p: ScParenthesisedPattern => //val (x: Int) = 2
        p.subpattern match {
          case Some(p: ScTypedPattern) => p.typePattern.map(_.typeElement.getText) match {
            case Some(tpe) => ParenthesisPat(p.getFirstChild.getText, tpe)
            case None => SimplePat(p.getText)
          }
          case _ => SimplePat(p.getText)
        }
      case p: ScReferencePattern => SimplePat(p.getText) //val x: Int = 2
      case p => SimplePat(p.getText)
    }} match {
      case Some(p) if p.nonEmpty =>
        add2mapAndReturn(element, Some(Defn.Val(
          mods = Nil,
          pats = p.map {
            case ParenthesisPat(lhs, rhs) => Pat.Typed(Term.Name(lhs)(isBackquoted = isBackQuoted(lhs)), Type.Name(rhs)(isBackquoted = isBackQuoted(rhs)))
            case SimplePat(name) => Term.Name(name)(isBackquoted = isBackQuoted(name))
          }.to[List],
          decltpe = findFirstChildByClass[ScTypeElement](e).map{tpe =>
            val tpeStr = tpe.getType(TypingContext.empty).getOrAny.toString
            Type.Name(tpeStr)(isBackQuoted(tpeStr))},
          rhs = convertLiteral(findFirstChildByClass[ScLiteral](e)).getOrElse(null)
        )))
      case _ => None
    }
  }

  private def convertVariableDefn(element: Option[ScVariableDefinition]): Option[Defn] = element flatMap { e =>
    findFirstChildByClass[ScPatternList](e).map{_.patterns.map(_.getText)} match {
      case Some(p) if p.nonEmpty =>
        add2mapAndReturn(element, Some(Defn.Var(
          mods = Nil,
          pats = p.map {name => Term.Name(name)(isBackquoted = isBackQuoted(name))}.to[List],
          decltpe = findFirstChildByClass[ScTypeElement](e).map{tpe =>
            val tpeStr = tpe.getType(TypingContext.empty).getOrAny.toString
            Type.Name(tpeStr)(isBackQuoted(tpeStr))},
          rhs = convertLiteral(findFirstChildByClass[ScLiteral](e))
        )))
      case _ => None
    }
  }

  private def convertTypeAliasDefn(element: Option[ScTypeAliasDefinition]): Option[Defn] = element flatMap { e =>
    add2mapAndReturn(element, Some(Defn.Type(
      mods = Nil,
      name = {
        val typeName = findByIdentifier(e, ScalaTokenTypes.tIDENTIFIER).headOption.map(_.getText).getOrElse("unknown")
        Type.Name(typeName)(isBackquoted = isBackQuoted(typeName))
      },
      tparams = findFirstChildByClass[ScTypeParamClause](e).map{ _.typeParameters.map { tparam =>
            val typeName = tparam.typeParameterText
            TypeParam.Named(
              mods = Nil,
              name = Type.Name(typeName)(isBackQuoted(typeName)),
              tparams = Nil,
              contextBounds = processContextBounds(Some(tparam)).get,
              viewBounds = processViewBounds(Some(tparam)).get,
              bounds = processBounds(Some(tparam)).get
            )
          }.to[List]
        }.getOrElse(List[TypeParam]()),
      body = processTypeElement(Some(e.aliasedTypeElement)).get
    )))
  }

  private def convertFunctionDefn(element: Option[ScFunctionDefinition]): Option[Defn] = element flatMap { e =>
    add2mapAndReturn(element, Some(Defn.Def(
      mods = List(),
      name = {
        val funcName = e.name
        Term.Name(funcName)(isBackquoted = isBackQuoted(funcName))
      },
      tparams = findFirstChildByClass[ScTypeParamClause](e).map{ _.typeParameters.map { tparam =>
          val typeName = findByIdentifier(tparam, ScalaTokenTypes.tIDENTIFIER).headOption.map(_.getText).getOrElse("unknown")
          TypeParam.Named(
            mods = Nil,
            name = Type.Name(typeName)(isBackQuoted(typeName)),
            tparams = Nil,
            contextBounds = processContextBounds(Some(tparam)).get,
            viewBounds = processViewBounds(Some(tparam)).get,
            bounds = processBounds(Some(tparam)).get
          )
        }.to[List]
      }.getOrElse(List[TypeParam]()),
      explicits = findChildrenByClass[ScParameters](e).flatMap{params => findChildrenByClass[ScParameterClause](params).filter(!_.isImplicit).
              map{_.parameters.flatMap {p => convertParameter(Some(p))}.to[List]}}.to[List],
      implicits = findChildrenByClass[ScParameters](e).flatMap{params => findChildrenByClass[ScParameterClause](params).filter(_.isImplicit).
              map{_.parameters.flatMap {p => convertParameter(Some(p))}.to[List]}}.flatten.to[List],
      decltpe = None,
      body = e.body.flatMap{b => convert(b)} match {
        case Some(t: Term) => t
        case _ => Lit.Unit()
      }
    )))
  }

  private def convertExpression(element: Option[ScExpression]): Option[Term] = element flatMap {
    case e: ScInfixExpr => add2mapAndReturn(element, Some(ApplyInfix (
        lhs = Term.Name(e.lOp.getNode.getText)(isBackquoted = isBackQuoted(e.lOp.getNode.getText)),
        op = {
          val opName = e.operation.getElement.getText
          Term.Name(opName)(isBackquoted = isBackQuoted(opName))
        },
        targs = Nil,
        args = List(Term.Name(e.rOp.getNode.getText)(isBackquoted = isBackQuoted(e.rOp.getNode.getText))) //TODO
      )))
    case _ => None
  }

  private def convertParameter(element: Option[ScParameter]): Option[Param.Named] = element flatMap { param =>
      add2mapAndReturn(element,
        Some(Param.Named(
          mods = Nil,
          name = Term.Name(param.getName)(isBackquoted = isBackQuoted(param.getName)),
          decltpe = param.paramType.map{_.typeElement}.flatMap {p => processTypeElement(Some(p))},
          default = None //TODO
        )))
  }

  private def processTypeElement(element: Option[ScTypeElement]): Option[Type] = element flatMap {
    case p: ScParameterizedTypeElement => add2mapAndReturn(element,
      Some(Type.Apply(
      tpe = Type.Name(p.typeElement.getText)(isBackquoted = isBackQuoted(p.getText)),
      args = p.typeArgList match {
        case a: ScTypeArgs => a.typeArgs.map(e => Type.Name(e.getText)(isBackquoted = isBackQuoted(e.getText))).to[List]
        case _ => List[Type]()
      }
    )))
    case p => add2mapAndReturn(element,
      Some(Type.Name(p.getText)(isBackquoted = isBackQuoted(p.getText))))
  }

  private def processBounds(element: Option[ScTypeParam]): Option[TypeBounds] = element flatMap { tparam =>
    add2mapAndReturn(element, Some(TypeBounds(
      Option(findByIdentifier(tparam, ScalaTokenTypes.tLOWER_BOUND)).flatMap{ ids => if (ids.isEmpty) None else findFirstChildByClass[ScTypeElement](tparam).flatMap {p => processTypeElement(Some(p))}},
      Option(findByIdentifier(tparam, ScalaTokenTypes.tUPPER_BOUND)).flatMap{ ids => if (ids.isEmpty) None else findLastChildByClass[ScTypeElement](tparam).flatMap {p => processTypeElement(Some(p))}}
    )))
  }

  private def processViewBounds(element: Option[ScTypeParam]): Option[Seq[Type]] = element flatMap { tparam =>
    add2mapAndReturn(element, Some(if (findByIdentifier(tparam, ScalaTokenTypes.tVIEW).nonEmpty)
      findChildrenByClass[ScTypeElement](tparam).flatMap { p => processTypeElement(Some(p))}.to[List]
    else scala.collection.immutable.Seq[Type]()))
  }

  private def processContextBounds(element: Option[ScTypeParam]): Option[Seq[Type]] = element flatMap { tparam =>
    add2mapAndReturn(element, Some(if (findByIdentifier(tparam, ScalaTokenTypes.tCOLON).nonEmpty)
      findChildrenByClass[ScTypeElement](tparam).flatMap { p => processTypeElement(Some(p))}.to[List]
    else scala.collection.immutable.Seq[Type]()))
  }
}

object PalladiumTreeConverter {
  def apply() = new PalladiumTreeConverter
}

private object MiscUtils {
  def findChildrenByClass[T: ClassTag](e: PsiElement): Array[T] = e.getChildren.collect{case x: T => x}
  def findFirstChildByClass[T: ClassTag](e: PsiElement): Option[T] = findChildrenByClass[T](e).headOption
  def findLastChildByClass[T: ClassTag](e: PsiElement): Option[T] = findChildrenByClass[T](e).lastOption
  def isBackQuoted(text: String) = text.charAt(0) == '`' && text.length > 1
  def findByIdentifier(element: PsiElement, id: IElementType): Array[ASTNode] = element.getNode.getChildren(TokenSet.create(id))
}
