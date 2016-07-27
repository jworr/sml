package sml.test

import java.io.File
import org.scalatest._

import sml.nlp._
import sml.nlp.Phrase

/**
 * Tests the main package of sml.nlp
 */
object nlp 
{
	class PhraseTest extends FlatSpec with Matchers
	{
		"Phrase test" should "determine tokens from different sentences have no syntatic relationship" in
		{
			val sent1 = testDoc.sentenceById(1)
			val sent2 = testDoc.sentenceById(2)

			val token1 = Seq(sent1.tokenById(1))
			val token2 = Seq(sent2.tokenById(2))

			token1.hasDepRelationship(token2, testDoc) should be (false)
			token2.hasDepRelationship(token1, testDoc) should be (false)
		}

		it should "show that two regular tokens should have a relationship" in
		{
			val sent1 = testDoc.sentenceById(1)

			val token1 = Seq(sent1.tokenById(1))
			val token2 = Seq(sent1.tokenById(2))

			token1.hasDepRelationship(token2, testDoc) should be (true)
			token2.hasDepRelationship(token1, testDoc) should be (true)
		}

		it should "show that a punctuation token should have no syntatic relationships" in
		{
			val sent1 = testDoc.sentenceById(1)

			val token1 = Seq(sent1.tokenById(1))
			val token2 = Seq(sent1.tokenById(5))

			token1.hasDepRelationship(token2, testDoc) should be (false)
			token2.hasDepRelationship(token1, testDoc) should be (false)
		}

		it should "determine tokens from different sentences don't share a sentence" in
		{
			val sent1 = testDoc.sentenceById(1)
			val sent2 = testDoc.sentenceById(2)

			val token1 = Seq(sent1.tokenById(1))
			val token2 = Seq(sent2.tokenById(2))

			token1.shareSentence(token2) should be (false)
			token2.shareSentence(token1) should be (false)
		}

		it should "determine tokens from the same sentences do share a sentence" in
		{
			val sent1 = testDoc.sentenceById(1)

			val token1 = Seq(sent1.tokenById(1))
			val token2 = Seq(sent1.tokenById(2))

			token1.shareSentence(token2) should be (true)
			token2.shareSentence(token1) should be (true)
		}
	}

	class SentenceSyntax extends FlatSpec with Matchers
	{
		"Dependency Test" should "that two regular tokens have a common ancestor" in
		{
			val sent1 = testDoc.sentenceById(2)

			val token1 = sent1.tokenById(1)
			val token2 = sent1.tokenById(4)

			val result = sent1.commonAncestor(token1, token2)

			result.nonEmpty should be (true)

			result.get should be (sent1.tokenById(2))
		}

		it should "show that punctuation should have no ancestor" in
		{
			val sent1 = testDoc.sentenceById(1)

			val token1 = sent1.tokenById(5)

			sent1.ancestors(token1).size should be (1)
		}

		it should "show that a regular token should have ancestors" in
		{
			val sent1 = testDoc.sentenceById(1)

			val token1 = sent1.tokenById(1)

			sent1.ancestors(token1).size should be (2)
		}
		
		it should "show that punctuation and a regular token should have no common ancestor" in
		{
			val sent1 = testDoc.sentenceById(1)

			val token1 = sent1.tokenById(1)
			val token2 = sent1.tokenById(5)

			sent1.commonAncestor(token1, token2).nonEmpty should be (false)
		}

		it should "show that a governing token should be its own common ancestor with a decendant token" in
		{
			val sent1 = testDoc.sentenceById(2)

			val token1 = sent1.tokenById(2)
			val token2 = sent1.tokenById(4)

			val result = sent1.commonAncestor(token1, token2)

			result.nonEmpty should be (true)

			result.get should be (token1)
		}	

		it should "show that two tokens in different sentences should have no common ancestor" in
		{
			val sent1 = testDoc.sentenceById(1)
			val sent2 = testDoc.sentenceById(2)

			val token1 = sent1.tokenById(1)
			val token2 = sent2.tokenById(2)
	
			val common = sent1.commonAncestor(token1, token2)

			common.isEmpty should be (true)

			val otherCommon = sent2.commonAncestor(token1, token2)

			otherCommon.isEmpty should be (true)
		}

		it should "show that two tokens with a syntatic relationship should have common ancestor" in
		{
			val sent1 = testDoc.sentenceById(2)

			val token1 = sent1.tokenById(1)
			val token2 = sent1.tokenById(4)

			Seq(token1).hasDepRelationship(Seq(token2), testDoc) should be (true)

			sent1.commonAncestor(token1, token2).nonEmpty should be (true)
		}

		it should "show that two tokens without a syntatic relationship should have no common ancestor" in
		{
			val sent1 = testDoc.sentenceById(1)
			val sent2 = testDoc.sentenceById(2)

			val token1 = sent1.tokenById(1)
			val token2 = sent1.tokenById(5)
			val token3 = sent2.tokenById(1)

			Seq(token1).hasDepRelationship(Seq(token2), testDoc) should be (false)

			sent1.commonAncestor(token1, token2).isEmpty should be (true)
			
			Seq(token1).hasDepRelationship(Seq(token3), testDoc) should be (false)

			sent1.commonAncestor(token1, token3).isEmpty should be (true)
			sent2.commonAncestor(token1, token3).isEmpty should be (true)
		}

		it should "find children of non-leaf token" in
		{
			val sent = testDoc.sentenceById(2)
			val token = sent.tokenById(2)

			sent.children(token).toSet should be (pickTokens(sent, Seq(1,4)))
		}

		it should "find no children of leaf token" in
		{
			val sent = testDoc.sentenceById(2)
			val token = sent.tokenById(3)

			sent.children(token).toSet should be (Set())
		}

		it should "find all the descenants of a non-leaf token" in
		{
			val sent = testDoc.sentenceById(2)
			val token = sent.tokenById(2)
			sent.descendants(token) should be (pickTokens(sent, Seq(1,3,4)))
		}

		it should "find no descendants for a leaf token" in
		{
			val sent = testDoc.sentenceById(2)
			val token = sent.tokenById(3)

			sent.descendants(token) should be (Set())
		}

		it should "find no descendants for a punctuation token" in
		{
			val sent = testDoc.sentenceById(2)
			val token = sent.tokenById(5)
			sent.descendants(token) should be (Set())
		}
	}

	class SyntaxSearchTest extends FlatSpec with Matchers
	{
		//TODO make this a resource or something
		val docPath = "/home/walker/Data/ace/full_annotated/APW_ENG_20030502.0686.sgm.xml"
		val doc = if(new File(docPath).exists) parseDoc(docPath) else null

		"Syntax Search" should "find nearest subject" in
		{
			if(doc != null)
			{
				val sent = doc.sentenceById(29)
				val start = sent.tokenById(7)
				val target = sent.tokenById(1)
				
				sent.nearestSubject(start).get should be (target)
			}
		}

		it should "find nearest object" in
		{
			if(doc != null)
			{
				val sent = doc.sentenceById(18)
				val start = sent.tokenById(23)
				val target = sent.tokenById(28)
				
				sent.nearestObject(start).get should be (target)
			}
		}

		it should "find the parent" in
		{
			if(doc != null)
			{
				val sent = doc.sentenceById(31)
				val start = sent.tokenById(15)
				val target = sent.tokenById(13)
				
				sent.parent(start).get should be (target)
			}
		}
	}

	def pickTokens(sentence:Sentence, tokenIds:Iterable[Int]):Set[Token] =
	{
		tokenIds.map(i => sentence.tokenById(i)).toSet
	}
	
	val rawText = "This is a test. I hope it works."

	val xmlDoc = """<?xml version="1.0" encoding="UTF-8"?>
<?xml-stylesheet href="CoreNLP-to-HTML.xsl" type="text/xsl"?>
<root>
  <document>
    <sentences>
      <sentence id="1">
        <tokens>
          <token id="1">
            <word>This</word>
            <lemma>this</lemma>
            <CharacterOffsetBegin>0</CharacterOffsetBegin>
            <CharacterOffsetEnd>4</CharacterOffsetEnd>
            <POS>DT</POS>
            <NER>O</NER>
            <Speaker>PER0</Speaker>
          </token>
          <token id="2">
            <word>is</word>
            <lemma>be</lemma>
            <CharacterOffsetBegin>5</CharacterOffsetBegin>
            <CharacterOffsetEnd>7</CharacterOffsetEnd>
            <POS>VBZ</POS>
            <NER>O</NER>
            <Speaker>PER0</Speaker>
          </token>
          <token id="3">
            <word>a</word>
            <lemma>a</lemma>
            <CharacterOffsetBegin>8</CharacterOffsetBegin>
            <CharacterOffsetEnd>9</CharacterOffsetEnd>
            <POS>DT</POS>
            <NER>O</NER>
            <Speaker>PER0</Speaker>
          </token>
          <token id="4">
            <word>test</word>
            <lemma>test</lemma>
            <CharacterOffsetBegin>10</CharacterOffsetBegin>
            <CharacterOffsetEnd>14</CharacterOffsetEnd>
            <POS>NN</POS>
            <NER>O</NER>
            <Speaker>PER0</Speaker>
          </token>
          <token id="5">
            <word>.</word>
            <lemma>.</lemma>
            <CharacterOffsetBegin>14</CharacterOffsetBegin>
            <CharacterOffsetEnd>15</CharacterOffsetEnd>
            <POS>.</POS>
            <NER>O</NER>
            <Speaker>PER0</Speaker>
          </token>
        </tokens>
        <parse>(ROOT (S (NP (DT This)) (VP (VBZ is) (NP (DT a) (NN test))) (. .))) </parse>
        <dependencies type="basic-dependencies">
          <dep type="root">
            <governor idx="0">ROOT</governor>
            <dependent idx="4">test</dependent>
          </dep>
          <dep type="nsubj">
            <governor idx="4">test</governor>
            <dependent idx="1">This</dependent>
          </dep>
          <dep type="cop">
            <governor idx="4">test</governor>
            <dependent idx="2">is</dependent>
          </dep>
          <dep type="det">
            <governor idx="4">test</governor>
            <dependent idx="3">a</dependent>
          </dep>
        </dependencies>
        <dependencies type="collapsed-dependencies">
          <dep type="root">
            <governor idx="0">ROOT</governor>
            <dependent idx="4">test</dependent>
          </dep>
          <dep type="nsubj">
            <governor idx="4">test</governor>
            <dependent idx="1">This</dependent>
          </dep>
          <dep type="cop">
            <governor idx="4">test</governor>
            <dependent idx="2">is</dependent>
          </dep>
          <dep type="det">
            <governor idx="4">test</governor>
            <dependent idx="3">a</dependent>
          </dep>
        </dependencies>
        <dependencies type="collapsed-ccprocessed-dependencies">
          <dep type="root">
            <governor idx="0">ROOT</governor>
            <dependent idx="4">test</dependent>
          </dep>
          <dep type="nsubj">
            <governor idx="4">test</governor>
            <dependent idx="1">This</dependent>
          </dep>
          <dep type="cop">
            <governor idx="4">test</governor>
            <dependent idx="2">is</dependent>
          </dep>
          <dep type="det">
            <governor idx="4">test</governor>
            <dependent idx="3">a</dependent>
          </dep>
        </dependencies>
      </sentence>
      <sentence id="2">
        <tokens>
          <token id="1">
            <word>I</word>
            <lemma>I</lemma>
            <CharacterOffsetBegin>16</CharacterOffsetBegin>
            <CharacterOffsetEnd>17</CharacterOffsetEnd>
            <POS>PRP</POS>
            <NER>O</NER>
            <Speaker>PER0</Speaker>
          </token>
          <token id="2">
            <word>hope</word>
            <lemma>hope</lemma>
            <CharacterOffsetBegin>18</CharacterOffsetBegin>
            <CharacterOffsetEnd>22</CharacterOffsetEnd>
            <POS>VBP</POS>
            <NER>O</NER>
            <Speaker>PER0</Speaker>
          </token>
          <token id="3">
            <word>it</word>
            <lemma>it</lemma>
            <CharacterOffsetBegin>23</CharacterOffsetBegin>
            <CharacterOffsetEnd>25</CharacterOffsetEnd>
            <POS>PRP</POS>
            <NER>O</NER>
            <Speaker>PER0</Speaker>
          </token>
          <token id="4">
            <word>works</word>
            <lemma>work</lemma>
            <CharacterOffsetBegin>26</CharacterOffsetBegin>
            <CharacterOffsetEnd>31</CharacterOffsetEnd>
            <POS>VBZ</POS>
            <NER>O</NER>
            <Speaker>PER0</Speaker>
          </token>
          <token id="5">
            <word>.</word>
            <lemma>.</lemma>
            <CharacterOffsetBegin>31</CharacterOffsetBegin>
            <CharacterOffsetEnd>32</CharacterOffsetEnd>
            <POS>.</POS>
            <NER>O</NER>
            <Speaker>PER0</Speaker>
          </token>
        </tokens>
        <parse>(ROOT (S (NP (PRP I)) (VP (VBP hope) (SBAR (S (NP (PRP it)) (VP (VBZ works))))) (. .))) </parse>
        <dependencies type="basic-dependencies">
          <dep type="root">
            <governor idx="0">ROOT</governor>
            <dependent idx="2">hope</dependent>
          </dep>
          <dep type="nsubj">
            <governor idx="2">hope</governor>
            <dependent idx="1">I</dependent>
          </dep>
          <dep type="nsubj">
            <governor idx="4">works</governor>
            <dependent idx="3">it</dependent>
          </dep>
          <dep type="ccomp">
            <governor idx="2">hope</governor>
            <dependent idx="4">works</dependent>
          </dep>
        </dependencies>
        <dependencies type="collapsed-dependencies">
          <dep type="root">
            <governor idx="0">ROOT</governor>
            <dependent idx="2">hope</dependent>
          </dep>
          <dep type="nsubj">
            <governor idx="2">hope</governor>
            <dependent idx="1">I</dependent>
          </dep>
          <dep type="nsubj">
            <governor idx="4">works</governor>
            <dependent idx="3">it</dependent>
          </dep>
          <dep type="ccomp">
            <governor idx="2">hope</governor>
            <dependent idx="4">works</dependent>
          </dep>
        </dependencies>
        <dependencies type="collapsed-ccprocessed-dependencies">
          <dep type="root">
            <governor idx="0">ROOT</governor>
            <dependent idx="2">hope</dependent>
          </dep>
          <dep type="nsubj">
            <governor idx="2">hope</governor>
            <dependent idx="1">I</dependent>
          </dep>
          <dep type="nsubj">
            <governor idx="4">works</governor>
            <dependent idx="3">it</dependent>
          </dep>
          <dep type="ccomp">
            <governor idx="2">hope</governor>
            <dependent idx="4">works</dependent>
          </dep>
        </dependencies>
      </sentence>
    </sentences>
    <coreference>
      <coreference>
        <mention representative="true">
          <sentence>1</sentence>
          <start>3</start>
          <end>5</end>
          <head>4</head>
          <text>a test</text>
        </mention>
        <mention>
          <sentence>1</sentence>
          <start>1</start>
          <end>2</end>
          <head>1</head>
          <text>This</text>
        </mention>
        <mention>
          <sentence>2</sentence>
          <start>3</start>
          <end>4</end>
          <head>3</head>
          <text>it</text>
        </mention>
      </coreference>
    </coreference>
  </document>
</root>"""

	val testDoc:Document = parseDocFromString(xmlDoc, "Test Doc")
}
