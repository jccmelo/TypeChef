package de.fosd.typechef

import java.io.File
import de.fosd.typechef.parser.c._
import de.fosd.typechef.lexer.Main
import de.fosd.typechef.featureexpr.FeatureExpr
import de.fosd.typechef.typesystem.CTypeSystemFrontend
import de.fosd.typechef.typesystem.linker.CInferInterface

/**
 * Created by IntelliJ IDEA.
 * User: kaestner
 * Date: 10.04.11
 * Time: 12:56
 *
 *
 * frontend to the parser with little dependencies on other files
 *
 * produces html output
 */

object WebFrontend {

    def main(args: Array[String]) {
        if (args.size != 1 || !new File(args(0)).exists())
            println("incorrect paramter, expect C file")
        else {
            parse(new File(args(0)))
        }
    }

    def parse(file: File) {

        println("<h2>Input after macro expansion</h2><div class='output'><pre name='partiallypreprocessed'>")
        val tokenStream = new Main().run(Array(file.getAbsolutePath), true, true, null)
        println("</pre></div>")

        println("<h2>Conditional Token Stream</h2><div class='output'><div name='tokenstream'>")
        val in = CLexer.prepareTokens(tokenStream)
        for (tok <- in.tokens) {
            print('"' + tok.getText + '"')
            if (tok.getFeature != FeatureExpr.base)
                print("<sub>" + tok.getFeature + "</sub>")
            println(" * ")
        }
        println("</div></div>")

        println("<h2>Parser report</h2><pre name='parserresult'>")
        val parserMain = new ParserMain(new CParser(null, false))
        val ast = parserMain.parserMain(in, DefaultParserOptions)
        println("</pre>")
        println("<h2>AST</h2><div class='output'><div name='ast'>")
        println(ast)
        println("</div></div>")

        if (ast != null) {
            val ts = new CTypeSystemFrontend(ast.asInstanceOf[TranslationUnit]) with CInferInterface
            println("<h2>Type checking</h2><pre name='tsoutput'>")
            ts.checkAST
            println("</pre>")
            println("<h2>Module interface</h2><div class='output'><pre name='interface'>")
            println(ts.getInferredInterface().toString)
            println("</pre></div>")
        }
    }


}