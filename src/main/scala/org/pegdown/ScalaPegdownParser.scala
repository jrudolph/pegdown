package org.pegdown

import org.parboiled.scala._
import collection.JavaConverters._

import Extensions._

import ast._
import SimpleNode.Type
import org.parboiled.errors.{ErrorUtils, ParsingException}


class ScalaPegdownParser extends org.parboiled.scala.Parser with PegDownParser {
  type Nodes = Seq[Node]

  def Root: Rule1[RootNode] = rule {
    zeroOrMore(Block) ~~> asJava(new RootNode(_))
  }

  def Block: Rule1[Node] = rule {
    zeroOrMore(BlankLine) ~ (
      /* BlockQuote | Verbatim | */
      // ext ABBREVIATIONS
      /* Reference | HorizontalRule |*/ Heading | /*OrderedList | BulletList | HtmlBlock */
      // ext TABLES
      // ext DEFINITIONS
      // ext fenced code blocks
      Para | Inlines
    )
  }

  def BlockQuote: Rule1[BlockQuoteNode] = rule {
    push(new BlockQuoteNode(List[Node]().asJava))
  }
  //def Verbatim: Rule1[VerbatimNode] = rule {}

  def Heading: Rule1[HeaderNode] = rule {
    AtxHeading | SetextHeading
  }

  def Para: Rule1[ParaNode] = rule {
    NonIndentSpace ~ Inlines ~~> toSeq ~ oneOrMore(BlankLine) ~~> asJava(new ParaNode(_))
  }

  // inline text
  def Inlines = rule {
    oneOrMore(InlineOrIntermediateEndline) ~~> asJava(new SuperNode(_)) ~ optional(Endline ~ DROP)
  }

  def InlineOrIntermediateEndline: Rule1[Node] = rule {
    !Endline ~ Inline |
    Endline ~ test(Inline)
  }

  def Inline = rule {
    /*Link | */NonLinkInline
  }
  def NonLinkInline = rule {
    Str | Endline | UlOrStarLine | Space | StrongOrEmph |
    /* Image |*/ Code | /*InlineHtmlNode | */Entity | EscapedChar |
    // QUOTES |
    // SMARTS |
    Symbol
  }

  def Str = rule {
    oneOrMore(NormalChar) ~> (new TextNode(_))
  }
  def StrongOrEmph = rule {
    test(anyOf("*_")) ~ (Strong | Emph)
  }
  def Emph: Rule1[Node] = rule {
    (StrongOrEmphContent("*") | StrongOrEmphContent("_")) ~~> asJava(new EmphNode(_))
  }
  def Strong: Rule1[Node] = rule {
    (StrongOrEmphContent("**") | StrongOrEmphContent("__")) ~~> asJava(new StrongNode(_))
  }
  // don't use `rule {}` here or you will overwrite parsers
  def StrongOrEmphContent(delimiter: String): Rule1[Nodes] =
    StrongOrEmphOpen(delimiter) ~ oneOrMore(!StrongOrEmphClose(delimiter) ~ Inline) ~
    StrongOrEmphClose(delimiter)

  def StrongOrEmphOpen(delimiter: String): Rule0 =
    !CharLine(delimiter(0)) ~ delimiter ~ !Spacechar ~ NotNewline

  def StrongOrEmphClose(delimiter: String): Rule0 =
    !Spacechar ~ NotNewline ~ delimiter ~ !Alphanumeric

  def Space = rule {
    oneOrMore(Spacechar) ~ push(new TextNode(" "))
  }

  def UlOrStarLine = rule {
    (CharLine('_') | CharLine('*')) ~> (new TextNode(_))
  }

  def Code: Rule1[CodeNode] = rule {
    test("`") ~ (
      Code(Ticks(1)) |
      Code(Ticks(2)) |
      Code(Ticks(3)) |
      Code(Ticks(4)) |
      Code(Ticks(5))
    )
  }
  def Code(delimiter: Rule0): Rule1[CodeNode] = {
    delimiter ~ Sp ~ oneOrMore(
      !"`" ~ Nonspacechar |
      !delimiter ~ oneOrMore("`") |
      !(Sp ~ delimiter) ~ (
        Spacechar | (Newline ~ !BlankLine)
      )
    ) ~> (new CodeNode(_)) ~
    Sp ~ delimiter
  }


  def Entity = rule {
    "&" ~ (
      HexEntity | DecEntity | CharEntity
    ) ~ ";" ~> (new TextNode(_))
  }
  def EscapedChar = rule {
    "\\" ~ NotNewline ~ ANY ~> (new SpecialTextNode(_))
  }

  def Symbol = rule {
    SpecialChar ~> (new SpecialTextNode(_))
  }

  def AtxHeading = rule {
    AtxStart ~ optional(Sp) ~ oneOrMore(AtxInline) ~~> (_.asJava) ~ optional(Sp ~ zeroOrMore("#") ~ Sp) ~ Newline ~~>
      (new HeaderNode(_, _))
  }

  def AtxStart: Rule1[Int] = rule {
    ("######" | "#####" | "####" | "###" | "##" | "#") ~> (_.length)
  }
  def AtxInline = rule {
    !Newline ~ !(optional(Sp) ~ zeroOrMore("#") ~ Sp ~ Newline) ~ Inline
  }

  def SetextHeading: Rule1[HeaderNode] = rule {
    test(oneOrMore(NotNewline ~ ANY) ~ Newline ~ (
      nOrMore(3)("=") |
      nOrMore(3)("-")
    ) ~ Newline) ~
    (SetextHeading('=', 1) | SetextHeading('-', 2))
  }
  def SetextHeading(char: Char, level: Int): Rule1[HeaderNode] =
    oneOrMore(SetextInline) ~ Newline ~ nOrMore(3)(char.toString) ~ Newline ~~> asJava(new HeaderNode(level, _))
  def SetextInline = rule {
    !Endline ~ Inline
  }

  // links

  // line endings
  def LineBreak: Rule1[Node] = rule {
    "  " ~ NormalEndline ~ DROP ~ push(new SimpleNode(Type.Linebreak))
  }
  def Endline: Rule1[Node] = rule {
    LineBreak |
    TerminalEndline |
    NormalEndline
  }
  def NormalEndline: Rule1[Node] = rule {
    Sp ~ Newline ~ !(
      BlankLine |
      ">" |
      (AtxStart ~ DROP) |
      (zeroOrMore(NotNewline ~ ANY) ~ Newline ~
        (nOrMore(3)("=") | nOrMore(3)("-")) ~ Newline)
    ) ~
    (if (ext(HARDWRAPS)) push(new SimpleNode(Type.Linebreak)) else push(new TextNode(" ")))
  }
  def TerminalEndline: Rule1[Node] = rule {
    Sp ~ Newline ~ test(EOI) ~ push(new TextNode("\n"))
  }

  // lexical base definitions
  def NormalChar = rule {
    !SpecialChar ~ ! Spacechar ~ NotNewline ~ ANY
  }
  def SpecialChar = rule {
    anyOf("*_`&[]<>!#\\")
    // FIXME: extensions
  }
  def Alphanumeric = rule {
    Letter | Digit
  }
  def Letter = rule {
    ("a"-"z") | ("A"-"Z")
  }
  def Digit = rule {
    "0"-"9"
  }
  def HexDigit = rule {
    Digit | ("a"-"f") | ("A"-"F")
  }

  def CharLine(char: Char) =
    nOrMore(4)(char.toString) |
    Spacechar ~ oneOrMore(char.toString) ~ Spacechar

  def Ticks(n: Int) =
    nTimes(n, "`") ~ !"`"

  def HexEntity = rule {
    "#" ~ ignoreCase("x") ~ oneOrMore(HexDigit)
  }
  def DecEntity = rule {
    "#" ~ oneOrMore(Digit)
  }
  def CharEntity = rule {
    oneOrMore(Alphanumeric)
  }

  def NonIndentSpace = rule {
    "   " | "  " | " " | EMPTY
  }
  def BlankLine = rule {
    Sp ~ Newline
  }
  def Sp = rule {
    zeroOrMore(Spacechar)
  }
  def Spacechar = rule {
    anyOf(" \t")
  }
  def Nonspacechar = rule {
    !Spacechar ~ NotNewline ~ ANY
  }
  def Newline = rule {
    "\n" | ("\r" ~ optional("\n"))
  }
  def NotNewline = rule {
    !anyOf("\n\r")
  }

  def nOrMore(number: Int)(inner: Rule0): Rule0 =
    nTimes(number, inner) ~ zeroOrMore(inner)

  def test(inner: Rule): Rule0 = inner.unary_!.unary_!

  def toSeq: Node => Nodes = (n: Node) => Seq(n)
  def asJava[T](f: java.util.List[Node] => T): Nodes => T = ns => f(ns.asJava)

  def ext[T](extension: Int): Boolean = false

  def parse(source: Array[Char]): RootNode = {
    val parsingResult = ReportingParseRunner(Root).run(source)
    parsingResult.result.getOrElse {
      throw new ParsingException("Invalid JSON source:\n" + ErrorUtils.printParseErrors(parsingResult))
    }
  }
}

object SPD extends ScalaPegdownParser {
  def check[T](rule: Rule1[T])(str: String) = {
    val parsingResult = ReportingParseRunner(rule ~ EOI).run(str)
    parsingResult.result.getOrElse(throw new ParsingException("Invalid JSON source:\n" + ErrorUtils.printParseErrors(parsingResult)))
  }
  def trace[T](rule: Rule1[T])(str: String) = {
    val parsingResult = TracingParseRunner(rule ~ EOI).run(str)
    parsingResult.result.getOrElse(throw new ParsingException("Invalid JSON source:\n" + ErrorUtils.printParseErrors(parsingResult)))
  }
}