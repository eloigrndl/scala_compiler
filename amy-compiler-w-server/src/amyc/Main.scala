package amyc

import utils._
import ast._
import parsing._
import analyzer._
import codegen._
import langserver._

import java.io.File

object Main extends MainHelpers {
  private def parseArgs(args: Array[String]): Context = {
    Context(new Reporter, args.toList.filter(!_.slice(0,2).equals("--")))
  }

  def main(args: Array[String]): Unit = {
    val ctx = parseArgs(args)
    
    // Check whether to launch server
    if (args.contains("--LSP")) {
      //println("Amy server is running.")
      LSP.main(args)

    } else {

      val pipeline =
        Lexer andThen
        Parser andThen
        NameAnalyzer andThen
        TypeChecker andThen
        CodeGen andThen
        CodePrinter

      val files = ctx.files.map(new File(_))

      try {
        files.find(!_.exists()).foreach { f =>
          ctx.reporter.fatal(s"File not found: ${f.getName}")
        }
        if (files.isEmpty) {
          ctx.reporter.fatal("Oo.. No input files")
        }
        else {
          pipeline.run(ctx)(files)
        }
        ctx.reporter.terminateIfErrors()
      } catch {
        case AmycFatalError(_) =>
          sys.exit(1)
      }
    }

  }
}

trait MainHelpers {
  import SymbolicTreeModule.{Program => SP}
  import NominalTreeModule.{Program => NP}

  def treePrinterS(title: String): Pipeline[(SP, SymbolTable), Unit] = {
    new Pipeline[(SP, SymbolTable), Unit] {
      def run(ctx: Context)(v: (SP, SymbolTable)) = {
        println(title)
        println(SymbolicPrinter(v._1)(true))
      }
    }
  }

  def treePrinterN(title: String): Pipeline[NP, Unit] = {
    new Pipeline[NP, Unit] {
      def run(ctx: Context)(v: NP) = {
        println(title)
        println(NominalPrinter(v))
      }
    }
  }
}
