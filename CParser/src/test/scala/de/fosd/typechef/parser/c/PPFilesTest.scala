package de.fosd.typechef.parser.c

import junit.framework._;
import junit.framework.Assert._
import de.fosd.typechef.featureexpr._
import de.fosd.typechef.parser._
import org.junit.Test

class PPFilesTest extends TestCase {
    def parseFile(fileName: String) {
        val inputStream = getClass.getResourceAsStream("/" + fileName)
        assertNotNull("file not found " + fileName, inputStream)
        val p = new CParser()
        val result = p.translationUnit(
            CLexer.lexStream(inputStream, fileName, "testfiles/boa/"), FeatureExpr.base)
        System.out.println(result)
        (result: @unchecked) match {
            case p.Success(ast, unparsed) => {
                assertTrue("parser did not reach end of token stream: " + unparsed, unparsed.atEnd)
                //succeed
            }
            case p.NoSuccess(msg, context, unparsed, inner) =>
                fail(msg + " at " + unparsed + " with context " + context + " " + inner)
        }

    }

    // 

    @Test
    def testEscapePi() { parseFile("boa/escape.pi") }
    @Test
    def testAliasPi() { parseFile("boa/alias.pi") }
    @Test
    def testIpPi() { parseFile("boa/ip.pi") }

}