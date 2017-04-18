package de.fosd.typechef.cslice

import java.util

import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.crewrite._
import de.fosd.typechef.featureexpr.{FeatureExpr, FeatureExprFactory, FeatureModel}
import de.fosd.typechef.parser.c.{CFGStmt, FunctionDef, Statement, TranslationUnit, _}
import de.fosd.typechef.typesystem.{CDeclUse, CTypeSystemFrontend}

import scala.collection.mutable.ListBuffer


class SliceContext(ast: TranslationUnit, typeSystem: CTypeSystemFrontend with CDeclUse, fm: FeatureModel = FeatureExprFactory.empty) extends IntraCFG with CFGHelper {

  private [cslice] val functions: List[FunctionDef] = ASTUtil.filterASTElems[FunctionDef](ast)//.map(new Function(this, _))

  private [cslice] val astEnv = CASTEnv.createASTEnv(ast)


  def findStatements(line: Int): List[AST] = {
    var stmts: List[AST] = Nil

    for (f <- functions) {
      stmts = getAllPred(f, astEnv).map(_._1).filterNot(x => x.isInstanceOf[FunctionDef])
        //.filter(stmt => stmt.hasPosition && (stmt.getPositionFrom.getLine == line))
    }
    stmts
  }

  def findRelevantStatements(line: Int): List[AST] = {
    var stmts: List[AST] = Nil

    for (f <- functions) {
      stmts = getAllPred(f, astEnv)
        .map(_._1)
        .filterNot(x => x.isInstanceOf[FunctionDef])
        .filter(stmt => stmt.hasPosition && (stmt.getPositionFrom.getLine == line))
    }
    stmts
  }

  def getDecls(key: Id): List[Id] = {
    if (!typeSystem.getUseDeclMap.containsKey(key)) List(key)
    else typeSystem.getUseDeclMap.get(key).filter { d => astEnv.featureExpr(d) and astEnv.featureExpr(key) isSatisfiable fm }
  }

  /**
    * Gets the relevant statement(s) based on the slicing criteria
    */
  def getNodesFromProgramPoint(line: Int): util.ArrayList[Opt[Statement]] = {

    val relatedStmts: util.ArrayList[Opt[Statement]] = new util.ArrayList[Opt[Statement]]()

    for(f <- functions) {
      if (f.stmt != null) {

        val innerStmts = f.stmt.innerStatements

        for(s <- innerStmts) {
          println(ASTUtil.getASTNodeInfo(s.entry) + " | " + s.condition)

          if (s.entry.hasPosition && (s.entry.getPositionFrom.getLine == line)) {
            //println(ASTUtil.getASTNodeInfo(s.entry) + " | " + s.condition)
            relatedStmts.add(s)
          }
        }
      }
    }

    relatedStmts
  }

  def getIds(a: Any): List[Id] = {
    a match {
      case id: Id => if (!(id.name.startsWith("__builtin"))) List(id) else List()
      case gae: GnuAsmExpr => List()
      case l: List[_] => l.flatMap(x => getIds(x))
      case p: Product => p.productIterator.toList.flatMap(x => getIds(x))
      case k => List()
    }
  }

  def getFeatureExpr(node: AST, rd: ReachingDefinitions): FeatureExpr = {
    val ids: List[Id] = getIds(node)
    val listOfIdFeatExpr = rd.out(node).map{case ((x, _), f) => (x, f)}

    for (ele <- listOfIdFeatExpr) {
      for (id <- ids) {
        if(ele._1.equals(id) &&
          (ele._1.getPositionFrom.getLine == id.getPositionFrom.getLine)) {
          return ele._2
        }
      }
    }

    FeatureExprFactory.True
  }

  def hasDependence(a: AST, node: (Id, FeatureExpr)): Boolean = {
    val listIDs = getIds(a)

    if (listIDs.contains(node._1))
      true
    else
      false
  }

  def findDependenceNodes(relevantNode: AST, nodes: List[(Id, FeatureExpr)]): List[AST] = {
    val dependenceNodes = new ListBuffer[AST]

    for (node <- nodes) {
      if (hasDependence(relevantNode, node))
        dependenceNodes.append(node._1)
    }

//    println(dependenceNodes.map(id => (id, id.getPositionFrom.getLine)))

    dependenceNodes.toList
  }

  private def getEnclosingStatement(id: AST, analysis: MonotoneFW[_]): Option[AST] = {
    // retrieve the smallest valid enclosing statement
    var node: AST = id

    while(node != null) { // && !analysis.isVariable(node)) {
      node = ASTUtil.parentAST(node, astEnv)
    }

    if(node == null)
      None
    else
      Some(node)//asInstanceOf[CFGStmt])
  }

}
