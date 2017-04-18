package de.fosd.typechef.cslice

import de.fosd.typechef.crewrite.UsedDefinedDeclaredVariables
import de.fosd.typechef.featureexpr.FeatureExpr
import de.fosd.typechef.parser.c.{AST, ASTNavigation, PrettyPrinter}

/**
  * Created by jeam on 27/03/17.
  */
object ASTUtil extends ASTNavigation with UsedDefinedDeclaredVariables {

  def getASTNodeInfo(node: AST): (Int, String) =
    (node.getPositionFrom.getLine, PrettyPrinter.print(node).replace('\n', ' ').replace('\r', ' '))//node.getClass.getSimpleName)

  def trimFeatureExpr(expr: FeatureExpr): String =
    expr.toString().replaceAll("def[a-zE]*\\(([a-zA-Z0-9_]+)\\)", "$1")
}
