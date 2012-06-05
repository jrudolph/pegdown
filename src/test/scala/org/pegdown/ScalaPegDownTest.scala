package org.pegdown

import org.specs2.mutable.Specification
import org.parboiled.common.FileUtils
import org.specs2.specification.{Fragment, Fragments}
import org.w3c.tidy.Tidy
import java.io.{StringReader, StringWriter}

class ScalaPegDownTest extends Specification {
  val pegdownTests = Seq(
    "Abbreviations",
    "AttributeWithUnderscore",
    "Autolinks",
    "Bug_in_0.8.5.1",
    "Bug_in_0.8.5.4",
    "Bug_in_1.0.0",
    "Emph_With_Linebreaks",
    "GFM_Fenced_Code_Blocks",
    "Linebreaks",
    "No Follow Links",
    "Parens_in_URL",
    "Quoted Blockquote",
    "Smartypants",
    "Special Chars",
    "Tables",
    "Wikilinks"
  ).map(prefixed("pegdown/"))

  val marukuTests =
    ("abbreviations alt blank blanks_in_code bug_def bug_table code code2 code3 data_loss easy " +
     "email entities escaping extra_dl extra_header_id extra_table1 footnotes headers hex_entities " +
     "hrule html2 html3 html4 html5 ie images images2 inline_html inline_html2 links list1 list2 list3 " +
     "list4 lists lists6 lists7 lists8 lists9 lists11 lists_after_paragraph lists_ol loss misc_sw " +
     "olist one paragraph paragraphs smartypants syntax_hl table_attributes test wrapping xml xml2 xml3 " +
     "xml_instruction")
    .split(' ').map(prefixed("Maruku/"))

  val tests: Seq[String] =
    /*pegdownTests ++*/ marukuTests

   val ms =
      tests.map { name =>
        name in {
          val  (test, check) = loadTest(name)
          ((tidy(processor.markdownToHtml(test)) must be_==(tidy(check))): Fragment)
        }
      }
  include(Fragments.create(ms: _*))


  def processor =
    new PegDownProcessor(new ScalaPegdownParser)

  def loadTest(name: String): (String, String) =
    (testFileAsString(name+".md"), testFileAsString(name+".html"))

  def testFileAsString(name: String): String =
    if (getClass.getClassLoader.getResourceAsStream(name) != null)
      FileUtils.readAllTextFromResource(name)
    else
      throw new RuntimeException("Testfile '%s' missing" format name)

  def prefixed(prefix: String)(str: String) = prefix+str

  val Tidy = new Tidy
  def tidy(html: String): String = {
    val in = new StringReader(html)
    val out = new StringWriter()
    Tidy.parse(in, out)
    out.toString()
  }
}
