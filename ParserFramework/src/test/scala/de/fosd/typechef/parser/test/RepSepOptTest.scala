package de.fosd.typechef.parser.test

import junit.framework.TestCase
import org.junit._
import org.junit.Assert._
import de.fosd.typechef.parser.test.parsers._
import de.fosd.typechef.parser._
import de.fosd.typechef.featureexpr.FeatureExpr
import de.fosd.typechef.featureexpr.FeatureExpr._


/**
 * specific test for the RepSepOpt problem
 *
 * repOpt (as well as any other rep combinators) fail to handle
 * lists in which elements are optional and separated by commas or
 * other separators
 *
 * such lists cause exponential effort, if annotations do not align
 * with the required separator structure
 *
 * example
 * 1 , 2 , 3 , 4
 * is easy parse with digit~repOpt(comma~digit) if annotated like
 * this
 * 1 ,_A 2_A ,_B 3_B ,_C 4_C
 * but not if annotated as
 * 1 , 2_A ,_A 3_B ,_B 4
 * the latter case is quite common, but also the former cannot be excluded
 *
 */
class RepSepOptTest extends TestCase with DigitListUtilities {

    val p = new CharDigitParser()

    def digitList = p.repSepOpt(p.number, p.comma)


    @Test def testPlainEmpty = expectDigitList(List(), List())
    @Test def testPlainOne = expectDigitList(List(t("1")), List(ol(1)))
    @Test def testPlainPair = expectDigitList(List(t("1"), t(","), t("2")), List(ol(1), ol(2)))
    @Test def testOneFeatureOne = expectDigitList(List(t("1", f1)), List(ol(1, f1)))
    @Test def testOneFeaturePair1 = expectDigitList(List(t("1"), t(",", f1), t("2", f1)), List(ol(1), ol(2, f1)))
    @Test def testOneFeaturePair2 = expectDigitList(List(t("1", f1), t(",", f1), t("2")), List(ol(1, f1), ol(2)))
    @Test def testOneFeatureTripple = expectDigitList(List(t("1", f1), t(",", f1), t("2"), t(","), t("3")), List(ol(1, f1), ol(2), ol(3)))

    @Test def testAltComma1 = expectDigitList(List(t("1"), t(",", f1), t(",", f1.not), t("2")), List(ol(1), ol(2)))
    @Test def testAltComma2 = expectDigitList(List(t("1"), t(",", f1), t(",", f1.not), t("2", f1), t("3", f1.not)), List(ol(1), ol(2, f1), ol(3, f1.not)))


    @Test def testEasylist = easyList(100)
    @Test def testHardlist = hardList(100)

    //TODO: are lists that end with a comma handled correctly?


    private def easyList(length: Int) = {
        var l = List(t("0"))
        var expected = List(ol(0))
        for (i <- 1 until length) {
            val f = FeatureExpr.createDefinedExternal("f" + i)
            l = l :+ t(",", f) :+ t(i.toString, f)
            expected = expected :+ ol(i, f)
        }

        println("in: " + l)
        expectDigitList(l, expected)
    }
    private def hardList(length: Int) = {
        var l = List(t("0"), t(","))
        var expected = List(ol(0))
        for (i <- 1 until length) {
            val f = FeatureExpr.createDefinedExternal("f" + i)
            l = l :+ t(i.toString, f) :+ t(",", f)
            expected = expected :+ ol(i, f)
        }
        l = l :+ t("0")
        expected = expected :+ ol(0)

        println("in: " + l)
        expectDigitList(l, expected)
    }


    private def ol(v: Int) = Opt(base, Lit(v))
    private def ol(v: Int, f: FeatureExpr) = Opt(f, Lit(v))
    private def expectDigitList(providedList: List[MyToken], expectedEntries: List[Opt[Lit]]) {
        val in = p.tr(providedList)
        val r = digitList(in, FeatureExpr.base)
        println("parse result: " + r)

        r match {
            case p.Success(r, rest) =>
                assertTrue("not at end " + rest, rest.atEnd)
                assertEquals(
                    expectedEntries, r
                )
            case _ => fail("unsuccessful result " + r)
        }
    }

}