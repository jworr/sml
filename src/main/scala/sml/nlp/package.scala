package sml

import scala.xml.{Node, Elem}
import scala.xml.XML.{loadFile, loadString}
import scala.collection.immutable.TreeMap
import scala.collection.mutable.HashMap
import scala.collection.Map
import scala.math.{min,max}

import sml.io.{baseName,ls,join,removeSuffix}
import sml.nlp.chunker.{Chunk, chunkSentence}

/**
A library to load core nlp xml into objects
*/
package object nlp
{
	class Document(val id: String, val sentences: Seq[Sentence], val corefGroups: Seq[CorefGroup])
	{
		/*Returns the sentence specified by sentence id*/
		def sentenceById(sentenceId: Int): Sentence = sentences(sentenceId -1)

		/**
		Returns the sentence the token is in
		*/
		def sentenceByToken(token:Token): Sentence = sentenceById(token.sentenceId)

		/**
		Gets chunks by the sentence they are associated with
		*/
		def chunksBySentenceId(sentenceId:Int):Seq[Chunk] = chunks(sentenceId-1)

		/**
		Gets chunks by the given token
		*/
		def chunkByToken(token:Token):Chunk =
		{
			val sentChunks = chunksBySentenceId(token.sentenceId)

			sentChunks.find(c => c.hasToken(token)).get
		}

		/**
		Returns all the sentences chunked
		*/
		def chunks:Seq[Seq[Chunk]] = sentences.map(_.chunks)

		/**
		Returns the tokens associated with the mention
		*/
		def mentionTokens(mention:Mention): Seq[Token] =
		{
			//get the sentence
			val sentence = sentenceById(mention.sentenceId)

			//look up all the tokens
			mention.span.map(i => sentence.tokenById(i))
		}

		/**
		 * Returns the Coref group for the token if any
		 */
		def corefGroup(token:Token):Option[CorefGroup] =
		{
			corefGroups.find(_.inGroup(token))
		}

		/**
		Returns the token identified by the sentence and token ids
		*/
		def tokenById(sentenceId:Int, tokenId:Int): Token =
		{
			sentenceById(sentenceId).tokenById(tokenId)
		}

		/**
		Returns tokens from the document
		*/
		def tokens(sentenceId:Int, span:Range): Seq[Token] =
		{
			sentenceById(sentenceId).tokensById(span)
		}

		/**
		Returns all the tokens
		*/
		def tokens:Seq[Token] = sentences.map(_.tokens).flatten

		/**
		Returns the syntatic head of the span - the highest according to the
		parse tree
		*/
		def syntaticHead(seq:Iterable[Token]):Token =
		{
			//assume all the tokens in the sequence are in the same sentence
			sentenceByToken(seq.head).syntaticHead(seq)	
		}

		/**
		Returns the window, including the given sequence, on either side of the
		sequence
		*/
		def window(subseq:Seq[Token], window:Int):Seq[Token] =
		{
			val sent = sentenceById(subseq.head.sentenceId)
			val start = max(0, subseq.head.id - window)
			val end = min(subseq.last.id + window, sent.tokens.last.id)

			tokens(sent.id, Range(start,end+1))
		}

		/**
		Returns the token that starts at the give offset
		*/
		def tokenAtOffset(offset:Int):Option[Token] = 
		{
			tokens.find(t => t.containsOffset(offset))
		}

		override def toString:String = id

		override def equals(other:Any) = other match
		{
			case that:Document => that.id == id
			case _ => false
		}

		override def hashCode = id.hashCode
	}

	/**
	An index for a document by Token (word)
	*/
	class DocumentIndex(val document:Document)
	{
		val index = document.tokens.groupBy(_.word.toLowerCase).toMap
		/**
		Return all the tokens that match the given word
		*/
		def lookup(word:String):Iterable[Token] = index.getOrElse(word, Seq())
	}

	/**
	Represents a Sentence in a Document
	*/
	class Sentence(val id: Int, allTokens: Seq[Token], edges: Map[(Int,Int), String])
	{
		val tokMap: TreeMap[Int,Token] = TreeMap(allTokens.map( (t:Token) => (t.id,t) ):_*)

		//map of token to dependency type, governor
		val dependencies = edges.map( kv => (kv._1._2, (kv._2, kv._1._1)) ).toMap

		val chunks:Seq[Chunk] = chunkSentence(this)

		def tokens: Seq[Token] = tokMap.values.toSeq

		def tokenById(tokenId: Int): Token = tokMap(tokenId)

		def tokensById(span:Range): Seq[Token] = tokens.slice(span.head-1, span.end-1)

		/**
		Returns all the tokens with the given dependency type
		*/
		def tokensWithType(depType: String): Iterable[Token] =
		{
			//get edges with the correct type, then get the end vertex of the edge, then look up the Token
			dependencies.filter(kv => kv._2._1 == depType).map( kv => kv._1 ).map(tokMap)
		}

		/**
		Returns the chunks the token sequence overlaps
		*/
		def coveredChunks(seq:Seq[Token]): Seq[Chunk] =
		{
			val indexes = coveredChunkRange(seq)
			coveredChunks(indexes.head, indexes.last)
		}
	
		/**
		Returns the chunks the indexes covers
		*/
		def coveredChunks(start:Int, end:Int): Seq[Chunk] =
		{
			return chunks.slice(start,end+1)
		}

		/**
		Returns the range of indexes the token sequence overlaps
		*/
		def coveredChunkRange(seq:Seq[Token]): Range =
		{
			val indexed = chunks.zipWithIndex
			val start = indexed.find(c => c._1.hasToken(seq.head)).get
			val end = indexed.find(c => c._1.hasToken(seq.last)).get

			return Range(start._2, end._2+1)
		}

		/**
		Returns the dependency type for the token
		*/
		def depType(token:Token): String =
		{
			if(hasDepType(token))
			{
				dependencies(token.id)._1
			}
			else
			{
				null
			}
		}

		/**
		Returns the parent of the token
		*/
		def parent(token:Token):Token =
		{
			if(dependencies.contains(token.id))
			{
				tokMap.getOrElse(dependencies(token.id)._2, null)
			}
			else
			{
				null
			}
		}

		/**
		Returns the children of the token
		*/
		def children(token:Token):Iterable[Token] =
		{
			//find all the children of the token
			for( (current,(depType, gov)) <- dependencies if tokMap.getOrElse(gov,null) == token ) yield
			{
				tokMap(current)
			}
		}

		/**
		Returns all the ancestors of the token all the way back to the root
		*/
		def ancestors(token:Token):Seq[Token] =
		{
			def helper(current:Token, partial:List[Token]):List[Token] =
			{
				if(current == null)
					return partial
				else
					return helper(parent(current), current :: partial)
			}

			return helper(token, List())
		}

		/**
		Returns the common ancestor 
		*/
		def commonAncestor(one:Token, another:Token):Option[Token] =
		{
			val prefix = ancestors(one).zip(ancestors(another)).takeWhile(p => p._1 == p._2)

			//make sure there is a prefix
			if(prefix.isEmpty)
				None
			else
				Some(prefix.last._1)
		}

		/**
		Returns the depth of the token in the dependency parse
		*/
		def depth(token:Token):Int = ancestors(token).size - 1

		/**
		Returns the token that is the syntatic head of the collection
		*/
		def syntaticHead(tokens:Iterable[Token]):Token =
		{
			tokens.minBy(t => depth(t))
		}

		/**
		Returns the maximum token id in the sentence
		*/
		def maxTokenId: Int = size

		/**
		Returns the minimum token id
		*/
		def minTokenId: Int = 1

		def hasDepType(token:Token):Boolean = token.sentenceId == id && dependencies.contains(token.id)

		def hasDepType(token: Token, dep: String): Boolean = depType(token) == dep

		def isSubject( token: Token ): Boolean = hasDepType(token, "subj")

		def isObject( token: Token ): Boolean = hasDepType(token, "obj")
		
		def isAux( token: Token ): Boolean = hasDepType(token, "aux")

		def objects: Iterable[Token] = tokensWithType("obj")

		def subjects: Iterable[Token] = tokensWithType("subj")

		def size: Int = tokens.size

		override def toString = tokens.map(_.toString).mkString(" ")

		override def equals(other:Any):Boolean = other match
		{
			case that:Sentence => that.id == id
			case _ => false
		}

		override def hashCode:Int = id.hashCode

		def toDot:String = toDot(Seq(), "")

		/**
		Returns a DOT graph representation of the dependency graph
		*/
		def toDot(groups:Iterable[Set[Token]], caption:String):String =
		{
			def strip(str:String):String = str.replaceAll("\"", "")

			//build color groups
			val colorMap = groups.zip(colors).map(p => p._1.map(i => (i, p._2))).flatten.toMap

			//build a box for each word
			val words = for(token <- tokens if dependencies.contains(token.id)) yield
			{
				val id = token.id
				val word = strip(token.word)
				val pos = token.pos
				val colorTag = if(colorMap.contains(token)) ("color=" + colorMap(token)) else ""

				s"""s$id [shape="box",label="$id: $word $pos" $colorTag];"""
			}

			//build an edge for each dependency
			val edges = for( (end, (depType, start)) <- dependencies ) yield
			{
				s"""s$start -> s$end [label="$depType"];"""
			}

			val text = strip(toString) + (if(caption.size > 0) ("\n" + strip(caption)) else "")
			
			val fullCaption = Seq(s"""label="$text";""", "labelloc=bottom;", "labeljust=left;")

			return (Seq("digraph G{") ++ words ++ edges ++ fullCaption ++ Seq("}")).mkString("\n")
		}
	}

	/**
	Represents an annotated Token in a Document
	*/
	class Token(val id: Int, val sentenceId: Int, val word: String, val lemma: String, val start: Int, val end: Int, val pos: String, val ner:String) 
	extends Ordered[Token]
	{
		def isVerb = pos.startsWith("VB")

		def isNoun = pos.startsWith("NN")

		def isProperNoun = pos.startsWith("NNP")

		def isAdj = pos.startsWith("JJ")

		def isAdv = pos.startsWith("RB")

		def isPronoun = pos.startsWith("PR")

		def simplePOS:String =
		{
			if(isVerb) "VERB"
			else if(isNoun) "NOUN"
			else if(isProperNoun) "PROPERNOUN"
			else if(isAdj) "ADJECTIVE"
			else if(isAdv) "ADVERB"
			else if(isPronoun) "PRONOUN"
			else "OTHER"
		}

		/**
		 * Returns true if the token contains alpha-numeric characters
		 */
		def alphaNum:Boolean = 
		{
			if(!blackList.contains(word))
				word.exists(c => c.isLetterOrDigit)
			else
				false
		}
		
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
		def inGroup(token: Token):Boolean = 
		{
			mentions.find((m) => m.contains(token)).nonEmpty
		}

		/**
		 * Returns true if the token is part of the cannonical mention
		 */
		def isCannonical(token:Token):Boolean = cannonical.contains(token)
		
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
	def loadDocs(dirName:String): Iterable[Document] = ls(dirName).filter(m => !m.endsWith(".swp.xml")).map(parseDoc)

	/**
	Loads all the annotated documents given with the given prefix and suffix
	*/
	def loadDocs(dir:String, prefix:String, suffix:String, docNames:Iterable[String]): Iterable[Document] = 
	{
		docNames.map(n => parseDoc(join(dir, prefix+n+suffix), prefix))
	}

	def loadDocs(dir:String, suffix:String, docNames:Iterable[String]): Iterable[Document] = loadDocs(dir, "", suffix, docNames)

	/**
	Parses the document from the given string
	*/
	def parseDocFromString(xml:String, docId:String): Document =
	{
		parseXMLDoc(loadString(xml), docId)
	}

	/**
	Returns a document parsed from an xml file
	*/
	def parseDoc(fileName:String, prefix:String): Document =
	{
		//load the doc as xml and parse it
		parseXMLDoc(loadFile(fileName), parseDocId(fileName,prefix))
	}

	def parseDoc(fileName:String): Document = parseDoc(fileName, "")

	/**
	Parses the document from the xml
	*/
	def parseXMLDoc(xml:Elem, docId:String): Document =
	{
		//parse all the sentences
		val sentences = (xml \\ "sentences" \ "sentence").map(parseSentence)

		//parse out all the coref groups
		val coref = (xml \ "root" \ "document" \ "coreference" \ "coreference").map(parseCoref)

		return new Document(docId, sentences, coref)
	}

	/**
	Strips off any prefix or suffix around the file id
	*/
	def parseDocId(fileName:String, prefix:String):String =
	{
		removeSuffix(baseName(fileName)).replaceFirst(prefix,"")
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
	List all the Penn-Treebank POS tags
	*/
	val POS = Set("CC",	"CD",	"DT",	"EX",	"FW",	"IN", "JJ", "JJR", "JJS", 
	"LS", "MD", "NN",	"NNS", "NNP", "NNPS", "PDT", "POS", "PRP", "PRP$", "RB", 
	"RBR", "RBS", "RP", "SYM", "TO", "UH", "VB", "VBD", "VBG", "VBN", "VBP", 
	"VBZ", "WDT", "WP", "WP$", "WRB")

	val blackList = Set("'s", "-LRB-", "-RRB-", "-LSB-", "-RSB-")

	/**
	Give some colors for graphviz
	*/
	val colors = Seq("green", "red", "blue", "yellow", "purple", "pink", "orange")

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
