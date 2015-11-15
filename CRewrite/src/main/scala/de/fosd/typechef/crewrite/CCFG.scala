package de.fosd.typechef.crewrite

import de.fosd.typechef.conditional.{Choice, Conditional, One, Opt}
import de.fosd.typechef.featureexpr.{FeatureExpr, FeatureExprFactory}
import de.fosd.typechef.parser.c._

import org.kiama.attribution.Attribution._

trait CCFG extends ASTNavigation with ConditionalNavigation {

    type CFGStmts = List[Opt[CFGStmt]]

    private lazy val stmtSucc: Tuple3[ASTEnv, CFGStmts, FeatureExpr] => Statement => CFGStmts = {
        paramAttr {
            case x@(env, res, ctx) =>
                s =>
                    val sn = nextASTElems(s, env).map(parentOpt(_, env)).asInstanceOf[List[Opt[Statement]]].tail
                    compStmtSucc(x)((parentAST(s, env).asInstanceOf[CompoundStatement], sn))
        }
    }

    private lazy val compStmtSucc: Tuple3[ASTEnv, CFGStmts, FeatureExpr] => Tuple2[CompoundStatement, List[Opt[Statement]]] => CFGStmts = {
        paramAttr {
            case x@(env, res, ctx) => {
                case (e, l) =>
                    var r = res

                    l.foreach {
                        x =>
                            if (ctx and env.featureExpr(x) isContradiction())
                                {}
                            else if (!isComplete(ctx)(r)) {
                                r = basicSucc(env, r, ctx)(x.entry)
                                // filter elements with an equivalent annotation
                                .foldLeft(List(): CFGStmts){
                                    (ul, nee) =>
                                        if (ul.exists{ o => o.condition equivalentTo nee.condition })
                                            ul
                                        else
                                            ul ++ List(nee)
                                }
                                // filter unsatisfiable control-flow paths, i.e., feature expression is contradiction
                                .filter{ o => o.condition and ctx isSatisfiable() }
                                r
                            }
                    }
                    r
                }
        }
    }

    private lazy val condStmtSucc: Tuple3[ASTEnv, CFGStmts, FeatureExpr] => Conditional[Statement] => CFGStmts = {
        paramAttr {
            case x@(env, res, ctx) => {
                case Choice(_, thenBranch, elseBranch) =>
                    condStmtSucc(x)(thenBranch) ++ condStmtSucc(x)(elseBranch)
                case One(c: CompoundStatement) =>
                    compStmtSucc(x)(c, c.innerStatements)
                case One(s: Statement) =>
                    basicSucc(x)(s)
            }
        }
    }

    private lazy val condExprSucc: Tuple3[ASTEnv, CFGStmts, FeatureExpr] => Conditional[Expr] => CFGStmts = {
        paramAttr {
            case x@(env, res, ctx) => {
                case _ => List()
            }
        }
    }

    private lazy val retuStmtSucc: Tuple3[ASTEnv, CFGStmts, FeatureExpr] => AST => CFGStmts = {
        paramAttr {
            case (env, res, ctx) =>
                r =>
                    findPriorASTElem[FunctionDef](r, env) match {
                        case None =>
                            List()
                        case Some(f) =>
                            val c = getCFGStmtCtx(res, ctx, env.featureExpr(f))
                            if (c.isSatisfiable())
                                res ++ List(Opt(c, f))
                            else
                                res
                    }
        }
    }

    private lazy val exprSucc: Tuple3[ASTEnv, CFGStmts, FeatureExpr] => Expr => CFGStmts =
        paramAttr {
            case x@(env, res, ctx) => {
                case CompoundStatementExpr(compoundStatement) =>
                    compStmtSucc(x)(compoundStatement, compoundStatement.innerStatements)
                case e =>
                    val c = env.featureExpr(e)
                    if (ctx and c isSatisfiable()) {
                        val uc = getCFGStmtCtx(res, ctx, c)
                        if (uc.isSatisfiable())
                            res ++ List(Opt(uc, e))
                        else
                            res
                    } else
                        res
                }
        }

    lazy val succ: ASTEnv => AST => CFGStmts = {
        paramAttr {
            env => {
                s => {
                    val c = env.featureExpr(s)
                    if (c.isSatisfiable())
                        succComp(env, List(), c)(s)
                    else
                        List()
                }
            }
        }
    }

    private lazy val basicSucc: Tuple3[ASTEnv, CFGStmts, FeatureExpr] => AST => CFGStmts =
        paramAttr {
            case x@(env, res, ctx) => {
                // loops
                case ForStatement(expr1, expr2, _, s) =>
                    if (expr1.isDefined)
                        exprSucc(x)(expr1.get)
                    else if (expr2.isDefined)
                        exprSucc(x)(expr2.get)
                    else
                        condStmtSucc(x)(s)
                case WhileStatement(expr, _) =>
                    exprSucc(x)(expr)
                case DoStatement(_, s) =>
                    condStmtSucc(x)(s)

                // conditional statements
                case IfStatement(condition, _, _, _) =>
                    condExprSucc(x)(condition)
                case ElifStatement(condition, _) =>
                    condExprSucc(x)(condition)
                case SwitchStatement(expr, _) =>
                    exprSucc(x)(expr)

                case e: CFGStmt =>
                    val c = env.featureExpr(e)
                    if (c and ctx isSatisfiable()) {
                        val cu = getCFGStmtCtx(res, ctx, c)
                        if (cu isSatisfiable())
                            res ++ List(Opt(cu, e))
                        else
                            res
                    } else
                    res
            }
        }

    private lazy val succComp: Tuple3[ASTEnv, CFGStmts, FeatureExpr] => AST => CFGStmts =
        paramAttr {
            case x@(env, res, ctx) => {
                case FunctionDef(_, _, _, stmt) => succComp(x)(stmt)
                case e@CompoundStatement(innerStatements) =>
                    var r = res
                    innerStatements.takeWhile {
                        case _ => !isComplete(ctx)(r)
                    }.foreach {
                        case Opt(_, a) => r = basicSucc(env, r, ctx)(a)
                    }

                    if (!isComplete(ctx)(r))
                        succFollowing(env, r, ctx)(e)
                    else
                        r
                case e => succFollowing(x)(e)
            }
        }

    // checks reference equality of e in a given structure
    private lazy val isPartOf: Product => Any => Boolean = {
        paramAttr {
            se => {
                case e: Product if se.asInstanceOf[AnyRef].eq(e.asInstanceOf[AnyRef]) => true
                case l: List[_] => l.exists(isPartOf(se)(_))
                case e: Product => e.productIterator.toList.exists(isPartOf(se)(_))
                case _ => false
            }
        }
    }

    private def getUnsatisfiedCtx(res: CFGStmts): FeatureExpr = {
        res.map(_.condition).fold(FeatureExprFactory.False)(_ or _).not()
    }

    private def getCFGStmtCtx(res: CFGStmts, ctx: FeatureExpr, ectx: FeatureExpr): FeatureExpr = {
        getUnsatisfiedCtx(res) and ctx and ectx
    }

    private lazy val isComplete: FeatureExpr => CFGStmts => Boolean = {
        paramAttr {
            ctx => {
                res =>
                    res.map(_.condition).fold(FeatureExprFactory.False)(_ or _) equivalentTo ctx
            }
        }
    }

    private lazy val succFollowing: Tuple3[ASTEnv, CFGStmts, FeatureExpr] => AST => CFGStmts =
        paramAttr {
            case x@(env, res, ctx) => {
                case se: ReturnStatement =>
                    retuStmtSucc(x)(se)
                case se =>
                    parentAST(se, env) match {
                        // loops
                        case ForStatement(Some(expr1), expr2, _, s) if isPartOf(se)(expr1) =>
                            if (expr2.isDefined) exprSucc(x)(expr2.get)
                            else condStmtSucc(x)(s)
                        case e@ForStatement(_, Some(expr2), expr3, s) if isPartOf(se)(expr2) =>
                            val rt = succFollowing(x)(e)
                            val rs = condStmtSucc(x)(s)
                            if (!isComplete(ctx)(rs)) {
                                val re =
                                    if (expr3.isDefined)
                                        exprSucc(x)(expr3.get)
                                    else
                                        exprSucc(x)(expr2)
                                rt ++ rs ++ re
                            } else rt ++ rs
                        case ForStatement(_, expr2, Some(expr3), s) if isPartOf(se)(expr3) =>
                            if (expr2.isDefined) exprSucc(x)(expr2.get)
                            else condStmtSucc(x)(s)
                        case ForStatement(_, expr2, expr3, s) if isPartOf(se)(s) =>
                            if (expr3.isDefined) exprSucc(x)(expr3.get)
                            else if (expr2.isDefined) exprSucc(x)(expr2.get)
                            else condStmtSucc(x)(s)

                        case e@WhileStatement(expr, s) if isPartOf(se)(expr) =>
                            val rs = condStmtSucc(x)(s)
                            val rt = succFollowing(x)(e)
                            val re =
                                if (!isComplete(ctx)(rs)) exprSucc(x)(expr)
                                else List()
                            rs ++ re ++ rt
                        case WhileStatement(expr, _) =>
                            exprSucc(x)(expr)

                        case e@DoStatement(expr, s) if isPartOf(se)(expr) =>
                            val rs = condStmtSucc(x)(s)
                            val rt = succFollowing(x)(e)
                            val re =
                                if (!isComplete(ctx)(rs)) exprSucc(x)(expr)
                                else List()
                            rs ++ re ++ rt
                        case DoStatement(expr, s) =>
                            exprSucc(x)(expr)

                        // conditional statements
                        // condition of the if statement
                        case e@IfStatement(condition, thenBranch, elifs, elseBranch) if isPartOf(se)(condition) =>
                            var r = res

                            elifs.takeWhile(_ => !isComplete(ctx)(r)).foreach {
                                case Opt(_, ElifStatement(c, _)) =>
                                    val re = condExprSucc(x)(c)

                                    re.filter {
                                        n => getUnsatisfiedCtx(r).and(n.condition).isSatisfiable()
                                    } foreach {
                                        n => r ++= List(n)
                                    }
                            }

                            if (!isComplete(ctx)(r)) {
                                if (elseBranch.isDefined) r ++= condStmtSucc(x)(elseBranch.get)
                                else r = succFollowing(env, r, ctx)(e)
                            }
                            r ++ condStmtSucc(x)(thenBranch)

                        // condition of the elif statement
                        case e@ElifStatement(condition, thenBranch) if isPartOf(se)(condition) =>
                            var r = res
                            val elifs = nextASTElems(e, env).tail.asInstanceOf[List[Opt[ElifStatement]]]

                            elifs.takeWhile(_ => !isComplete(ctx)(r)).foreach {
                                case Opt(_, ElifStatement(c, _)) =>
                                    val re = condExprSucc(x)(c)

                                    re.filter {
                                        n => getUnsatisfiedCtx(r).and(n.condition).isSatisfiable()
                                    } foreach {
                                        n => r ++= List(n)
                                    }
                            }

                            if (!isComplete(ctx)(r)) {
                                parentAST(e, env) match {
                                    case tp@IfStatement(_, _, _, None)=>
                                        r = succFollowing(env, r, ctx)(tp)
                                    case IfStatement(_, _, _, Some(elseBranch)) =>
                                        r = condStmtSucc(env, r, ctx)(elseBranch)
                                }
                            }
                            r ++ condStmtSucc(x)(thenBranch)
                        case e: ElifStatement =>
                            succFollowing(x)(e)

                        case e: CompoundStatement =>
                            val r = stmtSucc(x)(se.asInstanceOf[Statement])
                            if (!isComplete(ctx)(r))
                                succFollowing(env, r, ctx)(e)
                            else
                                r

                        case e: FunctionDef =>
                            val c = env.featureExpr(e)
                            val uc = getCFGStmtCtx(res, ctx, c)
                            var r = res
                            if (uc.isSatisfiable())
                                r = res ++ List(Opt(uc, e))
                            else
                                r = res
                            r

                        case _ => res
                    }
            }
        }
}
