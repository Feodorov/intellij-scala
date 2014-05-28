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

/**
 * @author kfeodorov
 * @since 23.05.14.
 */
object PalladiumTreeConverter {
  import MiscUtils._

  def convert(element: PsiElement): Option[Tree] = element match {
    //LitSuite
    case e: ScLiteral => convertLiteral(Some(e))
    case _: ScUnitExpr => Some(Lit.Unit())
    //DefnSuite
    case e: ScPatternDefinition => convertPatternDefn(Some(e))
    case e: ScVariableDefinition => convertVariableDefn(Some(e))
    case e: ScTypeAliasDefinition => convertTypeAliasDefn(Some(e))
    case e: ScFunctionDefinition => convertFunctionDefn(Some(e))
    case e: ScExpression => convertExpression(Some(e))
    case _ => None
  }

  private def convertLiteral(element: Option[ScLiteral]): Option[Lit] = element flatMap { e =>
    val value = e.getValue
    e.getFirstChild.getNode.getElementType match {
      case ScalaTokenTypes.kTRUE => Some(Lit.Bool(true))
      case ScalaTokenTypes.kFALSE => Some(Lit.Bool(false))
      case ScalaTokenTypes.tINTEGER => value match {
        case v: java.lang.Integer => Some(Lit.Int(v))
        case v: java.lang.Long => Some(Lit.Long(v))
      }
      case ScalaTokenTypes.tFLOAT => value match {
        case v: java.lang.Double => Some(Lit.Double(v))
        case v: java.lang.Float => Some(Lit.Float(v))
      }
      case ScalaTokenTypes.tCHAR => Some(Lit.Char(value.asInstanceOf[java.lang.Character]))
      case ScalaTokenTypes.tMULTILINE_STRING | ScalaTokenTypes.tSTRING | ScalaTokenTypes.tWRONG_STRING=> Some(Lit.String(e.getText))
      case ScalaTokenTypes.tSYMBOL => Some(Lit.Symbol(scala.Symbol(e.getText)))
      case ScalaTokenTypes.kNULL => Some(Lit.Null())
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
        Some(Defn.Val(
          mods = Nil,
          pats = p.map {
            case ParenthesisPat(lhs, rhs) => Pat.Typed(Term.Name(lhs)(isBackquoted = isBackQuoted(lhs)), Type.Name(rhs)(isBackquoted = isBackQuoted(rhs)))
            case SimplePat(name) => Term.Name(name)(isBackquoted = isBackQuoted(name))
          }.to[List],
          decltpe = findFirstChildByClass[ScTypeElement](e).map{tpe =>
            val tpeStr = tpe.getType(TypingContext.empty).getOrAny.toString
            Type.Name(tpeStr)(isBackQuoted(tpeStr))},
          rhs = convertLiteral(findFirstChildByClass[ScLiteral](e)).getOrElse(null)
        ))
      case _ => None
    }
  }

  private def convertVariableDefn(element: Option[ScVariableDefinition]): Option[Defn] = element flatMap { e =>
    findFirstChildByClass[ScPatternList](e).map{_.patterns.map(_.getText)} match {
      case Some(p) if p.nonEmpty =>
        Some(Defn.Var(
          mods = Nil,
          pats = p.map {name => Term.Name(name)(isBackquoted = isBackQuoted(name))}.to[List],
          decltpe = findFirstChildByClass[ScTypeElement](e).map{tpe =>
            val tpeStr = tpe.getType(TypingContext.empty).getOrAny.toString
            Type.Name(tpeStr)(isBackQuoted(tpeStr))},
          rhs = convertLiteral(findFirstChildByClass[ScLiteral](e))
        ))
      case _ => None
    }
  }

  private def convertTypeAliasDefn(element: Option[ScTypeAliasDefinition]): Option[Defn] = element flatMap { e =>
    Some(Defn.Type(
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
              contextBounds = processContextBounds(tparam),
              viewBounds = processViewBounds(tparam),
              bounds = processBounds(tparam)
            )
          }.to[List]
        }.getOrElse(List[TypeParam]()),
      body = processTypeElement(e.aliasedTypeElement)
    ))
  }

  private def convertFunctionDefn(element: Option[ScFunctionDefinition]): Option[Defn] = element map { e =>
    Defn.Def(
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
            contextBounds = processContextBounds(tparam),
            viewBounds = processViewBounds(tparam),
            bounds = processBounds(tparam)
          )
        }.to[List]
      }.getOrElse(List[TypeParam]()),
      explicits = findChildrenByClass[ScParameters](e).flatMap{params => findChildrenByClass[ScParameterClause](params).filter(!_.isImplicit).
              map{_.parameters.map {processParam}.to[List]}}.to[List],
      implicits = findChildrenByClass[ScParameters](e).flatMap{params => findChildrenByClass[ScParameterClause](params).filter(_.isImplicit).
              map{_.parameters.map{processParam}.to[List]}}.flatten.to[List],
      decltpe = None,
      body = e.body.flatMap{b => convert(b)} match {
        case Some(t: Term) => t
        case _ => Lit.Unit()
      }
    )
  }

  private def convertExpression(element: Option[ScExpression]): Option[Term] = element flatMap {
    case e: ScInfixExpr => Some(ApplyInfix (
        lhs = Term.Name(e.lOp.getNode.getText)(isBackquoted = isBackQuoted(e.lOp.getNode.getText)),
        op = {
          val opName = e.operation.getElement.getText
          Term.Name(opName)(isBackquoted = isBackQuoted(opName))
        },
        targs = Nil,
        args = List(Term.Name(e.rOp.getNode.getText)(isBackquoted = isBackQuoted(e.rOp.getNode.getText))) //TODO
      ))
    case _ => None
  }

  private def processTypeElement(e: ScTypeElement) = e match {
    case p: ScParameterizedTypeElement => Type.Apply(
      tpe = Type.Name(p.typeElement.getText)(isBackquoted = isBackQuoted(p.getText)),
      args = p.typeArgList match {
        case a: ScTypeArgs => a.typeArgs.map(e => Type.Name(e.getText)(isBackquoted = isBackQuoted(e.getText))).to[List]
        case _ => List[Type]()
      }
    )
    case p => Type.Name(p.getText)(isBackquoted = isBackQuoted(p.getText))
  }

  private def processBounds(tparam: ScTypeParam) = TypeBounds(
    Option(findByIdentifier(tparam, ScalaTokenTypes.tLOWER_BOUND)).flatMap{ ids => if (ids.isEmpty) None else findFirstChildByClass[ScTypeElement](tparam).map(processTypeElement)},
    Option(findByIdentifier(tparam, ScalaTokenTypes.tUPPER_BOUND)).flatMap{ ids => if (ids.isEmpty) None else findLastChildByClass[ScTypeElement](tparam).map(processTypeElement)}
  )

  private def processParam(param: ScParameter) = Param.Named(
    mods = Nil,
    name = Term.Name(param.getName)(isBackquoted = isBackQuoted(param.getName)),
    decltpe = param.paramType.map{_.typeElement}.map{processTypeElement},
    default = None //TODO
  )

  private def processViewBounds(tparam: ScTypeParam): Seq[Type] =
    if (findByIdentifier(tparam, ScalaTokenTypes.tVIEW).nonEmpty)
      findChildrenByClass[ScTypeElement](tparam).map(processTypeElement).to[List]
    else scala.collection.immutable.Seq[Type]()

  private def processContextBounds(tparam: ScTypeParam): Seq[Type] =
    if (findByIdentifier(tparam, ScalaTokenTypes.tCOLON).nonEmpty)
      findChildrenByClass[ScTypeElement](tparam).map(processTypeElement).to[List]
    else scala.collection.immutable.Seq[Type]()
}

private object MiscUtils {
  def findChildrenByClass[T: ClassTag](e: PsiElement): Array[T] = e.getChildren.collect{case x: T => x}
  def findFirstChildByClass[T: ClassTag](e: PsiElement): Option[T] = findChildrenByClass[T](e).headOption
  def findLastChildByClass[T: ClassTag](e: PsiElement): Option[T] = findChildrenByClass[T](e).lastOption
  def isBackQuoted(text: String) = text.charAt(0) == '`' && text.length > 1
  def findByIdentifier(element: PsiElement, id: IElementType): Array[ASTNode] = element.getNode.getChildren(TokenSet.create(id))
}
