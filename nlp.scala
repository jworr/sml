import scala.xml.{Node}
import scala.xml.XML.{loadFile}
import scala.collection.immutable.TreeMap
import scala.collection.mutable.HashMap
import scala.collection.Map
import grph.in_memory._
import grph.Grph

import io.baseName

/**
A library to load core nlp xml into objects
*/
object nlp
{
	class Document(documentId: String, docSentences: Seq[Sentence])
	{
		val id = documentId
		val contents = docSentences 

		//define some getters
		def docId : String = id
		def sentences : Seq[Sentence] = contents

		/*Returns the sentence specified by sentence id*/
		def sentenceById(sentenceId: Int) : Sentence = contents(sentenceId -1)
	}

	/**
	Represents a Sentence in a Document
	*/
	class Sentence(sentenceId: Int, allTokens: Seq[Token], dependencyGraph: Grph, edges: Map[(Int,Int), String])
	{
		val id = sentenceId
		val tokMap: TreeMap[Int,Token] = TreeMap(allTokens.map( (t:Token) => (t.id,t) ):_*)
		val depGraph = dependencyGraph
		val edgeTypes = edges

		def tokens: Iterable[Token] = tokMap.values

		def tokenById(tokenId: Int): Token = tokMap(tokenId)

		/**
		Returns all the tokens with the given dependency type
		*/
		def tokensWithType(depType: String): Iterable[Token] =
		{
			//get edges with the correct type, then get the end vertex of the edge, then look up the Token
			edgeTypes.filter((kv) => (kv._2 == depType)).map((kv) => (kv._1._2)).map(tokMap)
		}

		def hasDepType(token: Token, depType: String): Boolean = tokenswithType(depType).contains(token)

		def isSubject( token: Token ): Boolean = hasDepType(token, "subj")

		def isObject( token: Token ): Boolean = hasDepType(token, "obj")
		
		def isAux( token: Token ): Boolean = hasDepType(token, "aux")

		def objects: Iterable[Token] = tokensWithType("obj")

		def subjects: Iterable[Token] = tokensWithType("subj")

		def size: Int = tokens.size

		override def toString = tokens.map(_.toString).mkString(" ")
	}

	/**
	Represents an annotated Token in a Document
	*/
	class Token(tokenId: Int, _sentenceId: Int, _word: String, _lemma: String, _start: Int, _end: Int, _pos: String, _ner:String) extends Ordered[Token]
	{
		val id = tokenId
		val sentenceId = _sentenceId
		val word = _word
		val lemma = _lemma
		val start = _start
		val end = _end
		val pos = _pos
		val ner = _ner

		def isVerb = pos.contains("V")

		def isNoun = pos.contains("N")

		def isProperNoun = pos.contains("NNP")

		def isAdj = pos.contains("J")

		def isAdv = pos.contains("RB")

		def isPronoun = pos.contains("PR")

		/**
		Compares Tokens based on their id and sentence id
		*/
		override def equals(o: Any) = o match
		{
			case that: Token => that.id == this.id && that.sentenceId == this.sentenceId
			case _ => false
		}

		/**
		Tokens are hashed based on their two ids
		*/
		override def hashCode = id.hashCode + sentenceId.hashCode

		/**
		Compares Tokens based on their sentence id and their token id
		*/
		def compare(that: Token): Int = 
		{
			val comp = sentenceId.compare(that.sentenceId)
			
			//if the sentences are the same
			if (comp == 0)
				return id.compare(that.id)
			else
				return comp
		}
	
		/**
		Use the word as the token string
		*/
		override def toString = this.word.trim
	}

	/**
	Builds a graph from the dependency xml node
	*/
	def buildGraph(node: Node): (Grph, Map[(Int, Int),String]) =
	{
		//create graph
		val graph = new InMemoryGrph()
		val edgeType = new HashMap[(Int, Int),String]()
		var edge = 0

		//get all the dependencies
		for(dep <- node \ "dep")
		{
			//parse out the start, end and type information
			val start = (dep \ "governor" \ "@idx").text.toInt
			val end = (dep \ "dependent" \ "@idx").text.toInt
			val eType = (dep \\ "@type").text

			//add an edge
			graph.addDirectedSimpleEdge(start, edge, end)

			//add the edge type
			edgeType += ((start,end) -> eType)
		}
		
		return (graph, edgeType)
	}

	/**
	Returns a document parsed from an xml file
	*/
	def parseDoc(fileName: String): Document =
	{
		//load the doc as xml
		val xmlDoc = loadFile(fileName)

		//parse all the sentences
		val sentences = (xmlDoc \\ "sentences" \ "sentence").map(parseSentence)

		new Document(baseName(fileName), sentences)
	}

	/**
	Parses a sentence out of an xml tree
	*/
	def parseSentence(node: Node): Sentence =
	{
		//parse out the id
		val id = parseField(node, "@id").toInt

		//parse out all the tokens
		val toks = (node \\ "token").map( (t:Node) => parseToken(id,t) )

		//parse out the dependency graph
		val graphInfo = buildGraph((node \ "dependencies").filter((n:Node) => ((n \ "@type")(0).text == "basic-dependencies"))(0))

		new Sentence(id, toks, graphInfo._1, graphInfo._2)
	}

	/**
	Parses a token out of an xml node
	*/
	def parseToken(sentenceId: Int, node: Node): Token =
	{
		val p = (f:String) => parseField(node,f)

		new Token(p("@id").toInt, sentenceId, p("word"), 
		p("lemma"), p("CharacterOffsetBegin").toInt, 
		p("CharacterOffsetEnd").toInt, p("POS"), p("NER"))
	}

	/**
	Parses a token out of the xml, the field should only be present once
	*/
	def parseField(node: Node, field: String): String = (node \ field).text

	/**
	Main Function to run a simple test
	*/
	def main(args: Array[String])
	{
		//print out all the sentences
		for( sentence <- parseDoc(args(0)).sentences )
		{
			println(sentence)		
		}
	}
}
