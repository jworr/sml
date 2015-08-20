package sml

import scala.xml.Node
import scala.xml.XML.loadFile
import scala.collection.immutable.TreeMap
import scala.collection.mutable.HashMap
import scala.collection.Map

import sml.io.{baseName,ls,join}

/**
A library to load core nlp xml into objects
*/
object nlp
{
	val suffix = ".sgm.xml"

	class Document(val id: String, val sentences: Seq[Sentence], val corefGroups: Seq[CorefGroup])
	{
		/*Returns the sentence specified by sentence id*/
		def sentenceById(sentenceId: Int): Sentence = sentences(sentenceId -1)

		/**
		Returns the sentence the token is in
		*/
		def sentenceByToken(token:Token): Sentence = sentenceById(token.sentenceId)

		/**
		Returns the tokens associated with the mention
		*/
		def mentionTokens(mention:Mention): Iterable[Token] =
		{
			//get the sentence
			val sentence = sentenceById(mention.sentenceId)

			//look up all the tokens
			mention.span.map(i => sentence.tokenById(i))
		}

		/**
		Returns tokens from the document
		*/
		def tokens(sentenceId:Int, span:Range): Iterable[Token] =
		{
			sentenceById(sentenceId).tokensById(span)
		}

		/**
		Returns all the tokens
		*/
		def tokens:Iterable[Token] = sentences.map(_.tokens).flatten

		/**
		Returns the token that starts at the give offset
		*/
		def tokenAtOffset(offset:Int):Option[Token] = 
		{
			//TODO make more efficient
			sentences.map(_.tokenAtOffset(offset)).find(t => !t.isEmpty).getOrElse(None)
		}

		/**
		Returns the token that covers the offset
		*/
		def tokenThatCovers(offset:Int):Option[Token] =
		{
			//TODO make more efficient
			sentences.map(_.tokenCovers(offset)).find(t => !t.isEmpty).getOrElse(None)
		}

		override def toString:String = id
	}

	/**
	Represents a Sentence in a Document
	*/
	class Sentence(val id: Int, allTokens: Seq[Token], edges: Map[(Int,Int), String])
	{
		val tokMap: TreeMap[Int,Token] = TreeMap(allTokens.map( (t:Token) => (t.id,t) ):_*)
		val edgeTypes = edges

		def tokens: Iterable[Token] = tokMap.values

		def tokenById(tokenId: Int): Token = tokMap(tokenId)

		def tokensById(span:Range): Iterable[Token] = tokens.filter(t => span.contains(t.id))

		/**
		Returns all the tokens with the given dependency type
		*/
		def tokensWithType(depType: String): Iterable[Token] =
		{
			//get edges with the correct type, then get the end vertex of the edge, then look up the Token
			edgeTypes.filter((kv) => (kv._2 == depType)).map((kv) => (kv._1._2)).map(tokMap)
		}

		/**
		Returns the dependency type for the token
		*/
		def depType(token:Token): String =
		{
			edgeTypes.filter(e => (e._1._2 == token.id)).head._2
		}

		/**
		Finds the token that exactly starts with the given offset
		*/
		def tokenAtOffset(offset:Int): Option[Token] = tokens.find(t => t.start == offset)
	
		/**
		Finds the token that covers the offset
		*/
		def tokenCovers(offset:Int): Option[Token] = tokens.find(_.containsOffset(offset))

		def hasDepType(token: Token, dep: String): Boolean = depType(token) == dep

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
	class Token(val id: Int, val sentenceId: Int, val word: String, val lemma: String, val start: Int, val end: Int, val pos: String, val ner:String) 
	extends Ordered[Token]
	{
		def isVerb = pos.contains("V")

		def isNoun = pos.contains("N")

		def isProperNoun = pos.contains("NNP")

		def isAdj = pos.contains("J")

		def isAdv = pos.contains("RB")

		def isPronoun = pos.contains("PR")

		/**
		Returns true if the token contains the offset
		*/
		def containsOffset(offset:Int):Boolean = offset >= start && offset <= end

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
			val comp = this.sentenceId.compare(that.sentenceId)
			
			//if the sentences are the same
			if (comp == 0)
				return this.id.compare(that.id)
			else
				return comp
		}
	
		/**
		Use the word as the token string
		*/
		override def toString = this.word.trim
	}

	/**
	Represents a mention of an entity in a coref group
	*/
	class Mention(val sentenceId: Int, val span: Range, val head: Int)
	{
		/**
		Returns true if the token is contained in the mention's span
		*/
		def contains(token: Token): Boolean = span.contains(token.id)

		override def toString = sentenceId.toString + ": " + span.mkString(",")
	}

	/**
	Represents a group of entity mentions that refer to the same entity
	*/
	class CorefGroup(val mentions: Seq[Mention], val cannonical: Mention)
	{
		/**
		Returns true if the token is apart of a mention in the set
		*/
		def inGroup(token: Token): Boolean = !mentions.find((m) => m.contains(token)).isEmpty

		override def toString = "Coref Group:\n" + mentions.mkString("\n")
	}

	/**
	Builds a graph from the dependency xml node
	*/
	def buildGraph(node: Node): Map[(Int, Int),String] =
	{
		//create graph
		val edgeType = new HashMap[(Int, Int),String]()
		var edge = 0

		//get all the dependencies
		for(dep <- node \ "dep")
		{
			//parse out the start, end and type information
			val start = (dep \ "governor" \ "@idx").text.toInt
			val end = (dep \ "dependent" \ "@idx").text.toInt
			val eType = (dep \\ "@type").text

			//add the edge type
			edgeType += ((start,end) -> eType)
		}
		
		return edgeType
	}

	/**
	Loads all the annotated documents from a directory
	*/
	def loadDocs(dirName:String): Iterable[Document] = ls(dirName).map(parseDoc)
	
	/**
	Returns a document parsed from an xml file
	*/
	def parseDoc(fileName: String): Document =
	{
		//load the doc as xml
		val xmlDoc = loadFile(fileName)

		//parse all the sentences
		val sentences = (xmlDoc \\ "sentences" \ "sentence").map(parseSentence)

		//parse out all the coref groups
		val coref = (xmlDoc \ "root" \ "document" \ "coreference" \ "coreference").map(parseCoref)

		new Document(removeSuffix(baseName(fileName)), sentences, coref)
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
		val edges = buildGraph((node \ "dependencies").filter((n:Node) => ((n \ "@type")(0).text == "basic-dependencies"))(0))

		new Sentence(id, toks, edges)
	}

	/**
	Parses the co-reference information out of the xml node
	*/
	def parseCoref(node: Node): CorefGroup =
	{
		//parse out all the mentions
		var mentions = (node \ "mention").map(parseMention)

		var head = mentions.find((m) => m._2).get

		new CorefGroup(mentions.map(_._1), head._1)
	}

	/**
	Parse a mention out
	*/
	def parseMention(node: Node): (Mention,Boolean) =
	{
		//parse the sentence id
		val sentenceId = parseField(node, "sentence").toInt

		//parse the span
		val span = Range(parseField(node,"start").toInt, parseField(node,"end").toInt+1)

		//parse the head
		val head = parseField(node, "head").toInt

		//parse out if it is primary mention
		(new Mention(sentenceId, span, head), parseField(node, "@representative").toBoolean)
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
	Removes the file extension suffix from a doc id
	*/
	def removeSuffix(docId:String):String = 
	{
		val index = docId.indexOf(suffix)

		if(index != -1 )
			return docId.substring(0, index)
		else
			return docId
	}

	/**
	Returns the path to the file corresponding to the doc id
	*/
	def docIdToPath(path:String, docId:String):String = join(path,docId+suffix)

	/**
	Main Function to run a simple test
	*/
	def main(args: Array[String])
	{
		val doc = parseDoc(args(0))

		//print out all the sentences
		for( sentence <- doc.sentences )
		{
			println(sentence)		
		}

		//print out all the coref groups
		for( group <- doc.corefGroups )
		{
			println(group)
		}
	}
}
