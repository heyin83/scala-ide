package org.scalaide.internal.builder

import java.nio.file.Files
import java.io.File
import java.util.zip.ZipFile

import sbt.internal.inc.Locate
import sbt.internal.inc.PlainVirtualFileConverter
import sbt.internal.inc.Stamps
import sbt.internal.inc.classpath.ClasspathUtil

import xsbti.FileConverter
import xsbti.compile.DefinesClass

package object zinc {
  lazy val fileConverter: FileConverter = PlainVirtualFileConverter.converter
  lazy val stamper = Stamps.timeWrapBinaryStamps(fileConverter)

  private[zinc] object Locator {
    val NoClass = new DefinesClass {
      override def apply(className: String) = false
    }

    def apply(f: File): DefinesClass =
      if (f.isDirectory)
        new DirectoryLocator(f)
      else if (f.exists && ClasspathUtil.isArchive(f.toPath()))
        new JarLocator(f)
      else
        NoClass

    class DirectoryLocator(dir: File) extends DefinesClass {
      override def apply(className: String): Boolean = {
        Files.isRegularFile(Locate.classFile(dir.toPath, className))
      }
    }

    class JarLocator(jar: File) extends DefinesClass {
      lazy val entries: Set[String] = {
        val zipFile = new ZipFile(jar, ZipFile.OPEN_READ)
        try {
          import scala.jdk.CollectionConverters._
          zipFile.entries.asScala.filterNot(_.isDirectory).map { entry =>
            toClassNameFromJarFileName(entry.getName)
          }.toSet
        } finally
          zipFile.close()
      }

      private def toClassNameFromJarFileName(jarFileName: String): String = {
        val noClassAtEnd = if (jarFileName.endsWith(".class"))
          jarFileName.substring(0, jarFileName.lastIndexOf(".class"))
        else
          jarFileName
        noClassAtEnd.replaceAll("/", ".")
      }

      override def apply(className: String): Boolean =
        entries.contains(className)
    }
  }

}