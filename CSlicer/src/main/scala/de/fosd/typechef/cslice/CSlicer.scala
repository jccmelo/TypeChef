package de.fosd.typechef.cslice

import java.util
import java.io.File

import de.fosd.typechef.crewrite.ReachingDefinitions
import de.fosd.typechef.featureexpr.FeatureExprFactory
import de.fosd.typechef.lexer.LexerFrontend
import de.fosd.typechef.options.FrontendOptionsWithConfigFiles
import de.fosd.typechef.parser.c._
import de.fosd.typechef.typesystem._

import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer

/**
  * Created by Jean Melo on 06/03/17.
  */
object CSlicer extends EnforceTreeHelper with CDeclUse {

  def main (_args: Array[String]): Unit = {
//    if(_args.length < 2) {
//      println("Try again. Usage: <filename> <program_point>")
//      return
//    }
//    val offset = 0
//    val cfile = _args(offset)
//    val prgPoint = _args(offset + 1).toInt

//    val cfile = "CSlicer/src/main/resources/simple.c"
    val source: File = new File(getClass.getClassLoader.getResource("simple.c").getPath)
    val prgPoint = 20 // slicing criteria

    val options = new FrontendOptionsWithConfigFiles()
    options.parseOptions(Array(source.getPath))
    options.setPrintToStdOutput(false)
    val featureModel = options.getFullFeatureModel

    val lexer = new LexerFrontend()
    val tokens = CLexerAdapter.prepareTokens(lexer.run(options, true))
    val cParser = new CParser(featureModel, false)
    val parserMain = new ParserMain(cParser)

    val ast = parserMain.parserMain(tokens, SilentParserOptions, featureModel)
    prepareAST(ast)

    val typeSystem = new CTypeSystemFrontend(ast, featureModel, options) with CDeclUse
    typeSystem.checkAST(true, true)
    val declUseMap = typeSystem.getDeclUseMap
    val useDeclMap = typeSystem.getUseDeclMap

    val slice = new ListBuffer[AST]
    val sliceCxt: SliceContext = new SliceContext(ast, typeSystem, featureModel)
    val rd = new ReachingDefinitions(sliceCxt.astEnv, declUseMap, useDeclMap, FeatureExprFactory.empty, sliceCxt.functions(1))

    val relevantStmts = sliceCxt.findRelevantStatements(prgPoint) // assuming only one statement by now
    if(relevantStmts.isEmpty) {
      println("No statement found in line " + prgPoint)
      return
    }

    for (s <- relevantStmts) {
      slice.append(s)

      val res = rd.in(s).map(x => (x._1._1, x._2))
      val depNodes: List[AST] = sliceCxt.findDependenceNodes(s, res)
      val bucket: util.List[AST] = new util.ArrayList[AST]()

      depNodes.foreach(n => bucket.add(n))

      while (!bucket.isEmpty) {
        val currentDepNode = bucket.iterator().next()
        slice.append(currentDepNode)

        val statements = sliceCxt.findRelevantStatements(currentDepNode.getPositionFrom.getLine)
        for (stmt <- statements) {
          val nodes = sliceCxt.findDependenceNodes(stmt, res)
          for (n <- nodes if !bucket.contains(n)) yield bucket.add(n)
        }

        bucket.remove(currentDepNode)
      }

      printSlice(prgPoint, sliceCxt, rd, relevantStmts, slice)
    }
  }

  /**
    * Prints the program slice in the following format: line#, stmt, featureExpr.
    *
    * @param prgPoint
    * @param sliceCxt
    * @param rd
    * @param relevantStmts
    * @param slice
    */
  private def printSlice(prgPoint: Int, sliceCxt: SliceContext, rd: ReachingDefinitions, relevantStmts: List[AST], slice: ListBuffer[AST]) = {
    // print program slice in the following format: line#, stmt, featureExpr
    println("\n\nSLICING CRITERIA: '" + PrettyPrinter.print(relevantStmts.asJava.get(0)) + "' in line " + prgPoint + ".")
    println("PROGRAM SLICE:\nLine\tStmt\t\tFeatureExpr")

    val sortedSlice = slice.sortBy(_.getPositionFrom.getLine)

    for (node <- sortedSlice.toList) {
      var statement = node

      print(node.getPositionFrom.getLine + "\t\t")
      if (node.isInstanceOf[Id]) {
        statement = ASTUtil.parentAST(node, sliceCxt.astEnv)
        statement = ASTUtil.parentAST(statement, sliceCxt.astEnv)
      }

      println(PrettyPrinter.print(statement) + "\t\t" + ASTUtil.trimFeatureExpr(sliceCxt.getFeatureExpr(statement, rd)))
    }
  }
}