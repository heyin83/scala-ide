/*
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.refactoring
package transformation

import scala.tools.refactoring.common.TextChange

trait TreeTransformations extends Transformations with TreeFactory {

  this: common.EnrichedTrees with common.CompilerAccess =>

  import global._

  override def traverse(tree: Tree, f: Tree => Tree): Tree = {

    /**
     * Hooks into the Scala compiler's Transformer but applies only
     * one transformation and then returns that result.
     */
    object TransformOnce extends Transformer {

      /**
       * Transforms the children of the trees using `f` and creates
       * a new t with the transformed children
       */
      def once(tree: Tree) = tree match {

        case NamedArgument(name, rhs) =>
          transform(rhs) match {
            case `rhs` => tree
            case rhs =>
              NamedArgument(name, rhs)
          }

        case _: ImportSelectorTree | _: SourceLayoutTree | _: PlainText =>
          tree

        case MultipleAssignment(extractor, vals, rhs) =>
          (transform(extractor), transformTrees(vals), transform(rhs)) match {
            case (`extractor`, `vals`, `rhs`) => tree
            case (e, v, r) => MultipleAssignment(e, v.asInstanceOf[List[ValDef]], r)
          }

        case t: TypeTree if t.original != null =>
          val transformedTypeTree = super.transform(t).asInstanceOf[TypeTree]
          val transformedOriginal = f(t.original)

          // if only the original tree has been transformed, we have to create
          // a new TypeTree instance so the old and new ones are not `eq`.
          if (transformedTypeTree.eq(t) && !transformedOriginal.eq(t.original)) {
            new TypeTree().copyAttrs(t).setOriginal(transformedOriginal)
          } else {
            transformedTypeTree.setOriginal(transformedOriginal)
          }

        case t: UnApply =>
          // super does not transform t.fun
          treeCopy.UnApply(tree, transform(t.fun), transformTrees(t.args))

        case t if t.tpe != null && t.tpe.isError => t

        case t => super.transform(t)
      }

      override def transform(t: Tree) = f(t)
    }

    TransformOnce.once(tree)
  }

  def transform(f: PartialFunction[Tree, Tree]) = transformation(f)

  def filter(f: PartialFunction[Tree, Boolean]) = predicate(f)

  def replaceTree(from: Tree, to: Tree) = ↓(matchingChildren(predicate((t: Tree) => t samePosAndType from) &> constant(to)))

  implicit class TreeReplacesOtherTreeViaPosition[T <: Tree](t1: T) {
    def replaces[T2 >: T <: Tree](t2: T2): T = {
      t1 setPos t2.pos
    }
  }

  def abstractFileToTree(file: tools.nsc.io.AbstractFile): global.Tree = compilationUnitOfFile(file).get.body

  /**
   * Replace the first sequence of elements with another sequence.
   */
  implicit class AdditionalListMethods[T](l: List[T]) {
    def replaceSequence(what: List[T], replacement: List[T]): List[T] = {
      def inner(from: List[T], what: List[T], replacement: List[T]): List[T] = (from, what) match {
        case (Nil, _) => Nil
        case (xs, Nil) => xs
        case (x :: xs, y :: ys) if x == y => replacement ::: inner(xs, ys, Nil)
        case (x :: xs, _) => x :: inner(xs, what, replacement)
      }
      inner(l, what, replacement)
    }
  }

  implicit class AdditionalTreeListMethods(l: List[Tree]) {
    /**
     * Same as replaceSequence except that the positions of replaced trees
     * are copied to the new trees.
     * Note: The replacement must be smaller or of equal length than the sequence to replace
     */
    def replaceSequencePreservingPositions(seqToReplace: List[Tree], replacement: List[Tree]): List[Tree] = {
      assert(seqToReplace.length >= replacement.length)

      def inner(originalSeq: List[Tree], seqToReplace: List[Tree], replacement: List[Tree]): List[Tree] =
        (originalSeq, seqToReplace, replacement) match {
          case (Nil, _, _) => Nil
          case (xs, Nil, Nil) => xs
          case (x :: xs, y :: ys, z :: zs) if x == y =>
            (z replaces x) :: inner(xs, ys, zs)
          case (x :: xs, y :: ys, Nil) => inner(xs, ys, Nil)
          case (x :: xs, _, _) => x :: inner(xs, seqToReplace, replacement)
        }
      inner(l, seqToReplace, replacement)
    }
  }

  /**
   * Finds the "best" package for adding imports, together with the imports it already contains
   *
   * The ''best'' package for imports means the innermost package where imports are still visible
   * to all trees that might potentially need them. For example, given
   * {{{
   * package a.b.c
   * package d
   * package e
   *
   * package e1 {
   *   object E1
   * }
   *
   * package e2 {
   *   object E2
   * }
   * }}}
   * this function returns the package `a.b.c.d.e`.
   */
  val findBestPackageForImports = {

    def splitImports(p: PackageDef, stats: List[Tree]) = {
      val (imports, others) = stats partition (_.isInstanceOf[Import])
      (p, imports map (_.asInstanceOf[Import]), others)
    }

    def isPackageDef(tree: Tree) = tree.isInstanceOf[PackageDef]

    def isBestPlaceForImports(trees: List[Tree]) = {
      val (pkgDefs, otherTrees) = trees.partition(isPackageDef)
      otherTrees.nonEmpty || pkgDefs.size >= 2
    }

    transformation[Tree, (PackageDef, List[Import], List[Tree])] {
      case p @ PackageDef(_, stats) if isBestPlaceForImports(stats)  =>
        splitImports(p, stats)
    }
  }

  def shallowDuplicate[T <: Tree](orig: T): T = {
    new Transformer {
      override val treeCopy = new StrictTreeCopier
      override def transform(tree: Tree) = {
        if (tree eq orig)
          super.transform(tree)
        else
          tree
      }
    } transform orig
  }.asInstanceOf[T]

  val setNoPosition = transform {
    case t: global.Tree => t.pos = global.NoPosition; t
  }

  def addImportTransformation(importsToAdd: Seq[String]): Transformation[Tree, TextChange] = {
    def splitImports(p: PackageDef, stats: List[Tree]) = {
      val (imports, others) = stats partition (_.isInstanceOf[Import])
      (p, imports map (_.asInstanceOf[Import]), others)
    }

    def importsAsSrc(indent: String) = {
      def escaped(imp: String) = imp.split('.').map(escapeScalaKeywordsForImport).mkString(".")
      importsToAdd.map(imp ⇒ s"${indent}import ${escaped(imp)}").mkString("\n")
    }

    def insertAfter(pos: Position) = {
      val lineNumber = pos.source.offsetToLine(pos.start)
      val line = pos.source.lineToString(lineNumber)
      val indent = line.takeWhile(Character.isWhitespace)
      val insertPos = pos.source.lineToOffset(lineNumber) + line.length
      val isPkgLine = line.matches("""\s*package.*""")
      val isEmptyLine = pos.source.lineToString(lineNumber+1).trim.isEmpty
      TextChange(pos.source, insertPos, insertPos, (if (isPkgLine) "\n\n" else "\n") + importsAsSrc(indent) + (if (isEmptyLine) "" else "\n"))
    }

    def insertBefore(pos: Position) = {
      val lineNumber = pos.source.offsetToLine(pos.start)
      val line = pos.source.lineToString(lineNumber)
      val indent = line.takeWhile(Character.isWhitespace)
      val insertPos = pos.source.lineToOffset(lineNumber)
      val isEmptyLine =
        try pos.source.lineToString(lineNumber+1).trim.isEmpty
        catch {
          // WORKAROUND https://github.com/scala/scala/pull/5330
          case e: IndexOutOfBoundsException => true
        }
      TextChange(pos.source, insertPos, insertPos, importsAsSrc(indent) + (if (isEmptyLine) "\n" else "\n\n"))
    }

    /*
     * `foundPackage` is a big fat hack to remember the `PackageDef` that is
     * found in `findBestPackageForImports`. For some reason the transformation
     * is run multiple times, even though, the `once` call later should prevent
     * that. Therefore the `PackageDef` that is returned by
     * `findBestPackageForImports` is not the same as the one that is found
     * inside of it (and I don't really understand why).
     */
    var foundPackage: PackageDef = null
    val findBestPackageForImports = {
      def isPackageDef(tree: Tree) = tree.isInstanceOf[PackageDef]

      def isBestPlaceForImports(trees: List[Tree]) = {
        val (pkgDefs, otherTrees) = trees.partition(isPackageDef)
        otherTrees.nonEmpty || pkgDefs.size >= 2
      }

      transformation[Tree, Tree] {
        case p @ PackageDef(_, stats) if isBestPlaceForImports(stats) =>
          if (foundPackage == null)
            foundPackage = p
          p
      }
    }

    // first try it at the top level to avoid traversing the complete AST
    val insertLocation = findBestPackageForImports |> topdown(matchingChildren(findBestPackageForImports))

    val addImportStatement = once(insertLocation) &> transformation { case _ ⇒
      splitImports(foundPackage, foundPackage.stats) match {
        case (p @ PackageDef(Ident(nme.EMPTY_PACKAGE_NAME), _), Nil, _) ⇒
          val pos = p.pos withStart 0
          insertBefore(pos)
        case (p, Nil, _) ⇒
          p.stats match {
            case po +: _ if po.symbol.isPackageObject ⇒
              val pos = p.pos withStart 0
              insertBefore(pos)
            case _ ⇒
              insertAfter(p.pos)
          }
        case (_, imports, _) ⇒
          insertAfter(imports.last.pos)
      }
    }

    addImportStatement
  }
}
