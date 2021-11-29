/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package implementations

import common.Change
import transformation.TreeFactory
import analysis.Indexes

abstract class InlineLocal extends MultiStageRefactoring with ParameterlessRefactoring with TreeFactory with Indexes with common.InteractiveScalaCompiler  {

  import global._

  override type PreparationResult = ValDef

  override def prepare(s: Selection) = {

    val selectedValue = s.findSelectedOfType[RefTree] match {
      case Some(t) =>
        index.declaration(t.symbol) match {
          case Some(v: ValDef) => Some(v)
          case _ => None
        }
      case None => s.findSelectedOfType[ValDef]
    }

    def isInliningAllowed(sym: Symbol) =
      (sym.isPrivate || sym.isLocal) && !sym.isMutable && !sym.isValueParameter

    selectedValue match {
      case Some(t) if isInliningAllowed(t.symbol) =>
        Right(t)
      case Some(t) =>
        Left(PreparationError("The selected value cannot be inlined."))
      case None =>
        Left(PreparationError("No local value selected."))
    }
  }

  override def perform(selection: Selection, selectedValue: PreparationResult): Either[RefactoringError, List[Change]] = {

    trace("Selected: %s", selectedValue)

    val removeSelectedValue = {

      def replaceSelectedValue(ts: List[Tree]) = {
        ts replaceSequence (List(selectedValue), Nil)
      }

      transform {
        case tpl @ Template(_, _, stats) if stats contains selectedValue =>
          tpl.copy(body = replaceSelectedValue(stats)) replaces tpl
        case block @ BlockExtractor(stats) if stats contains selectedValue =>
          mkBlock(replaceSelectedValue(stats)) replaces block
      }
    }

    val references = index references selectedValue.symbol

    val replaceReferenceWithRhs = {

      val replacement = selectedValue.rhs match {
        // inlining `list.filter _` should not include the `_`
        case Function(vparams, Apply(fun, args)) if vparams forall (_.symbol.isSynthetic) => fun
        case t => t
      }

      trace("Value is referenced on lines: %s", references map (_.pos.lineContent) mkString "\n  ")

      transform {
        case t if references contains t => replacement
      }
    }

    if(references.isEmpty) {
      Left(RefactoringError("No references to selected val found."))
    } else {
      Right(transformFile(selection.file, topdown(matchingChildren(removeSelectedValue &> topdown(matchingChildren(replaceReferenceWithRhs))))))
    }
  }
}
