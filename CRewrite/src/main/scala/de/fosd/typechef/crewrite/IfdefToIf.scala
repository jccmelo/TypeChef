package de.fosd.typechef.crewrite

import de.fosd.typechef.parser.c._
import de.fosd.typechef.parser._
import org.kiama.rewriting.Rewriter._
import de.fosd.typechef.featureexpr.FeatureExpr.base
import de.fosd.typechef.featureexpr._

/**
 * strategies to rewrite ifdefs to ifs
 */

class IfdefToIf {

    val CONFIGPREFIX = "v_"

    private val rewriteStrategy = everywhere(rule {
        case Opt(f, stmt: Statement) if (!f.isTautology) =>
            Opt(base, IfStatement(featureToCExpr(f), stmt, List(), None))
        case Choice(f, One(a: Statement), One(b: Statement)) =>
            One(IfStatement(featureToCExpr(f), a, List(), Some(b)))
    })

    def rewrite(ast: AST): AST = {
        rewriteStrategy(ast).get.asInstanceOf[AST]
    }

    def featureToCExpr(feature: FeatureExpr): Expr = feature match {
        case d: DefinedExternal => Id(CONFIGPREFIX + d.feature)
        case a: And =>
            val l = a.clauses.toList
            var del = List[Opt[NArySubExpr]]()
            for (e <- l.tail)
                del = del ++ List(Opt(FeatureExpr.base, NArySubExpr("&&", featureToCExpr(e))))
            NAryExpr(featureToCExpr(l.head), del)
        case o: Or =>
            val l = o.clauses.toList
            var del = List[Opt[NArySubExpr]]()
            for (e <- l.tail)
                del = del ++ List(Opt(FeatureExpr.base, NArySubExpr("||", featureToCExpr(e))))
            NAryExpr(featureToCExpr(l.head), del)
        case Not(n) => UnaryOpExpr("!", featureToCExpr(n))
    }
}