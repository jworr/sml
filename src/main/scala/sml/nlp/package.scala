package sml

import scala.xml.{Node, Elem}
import scala.xml.XML.{loadFile, loadString}
import scala.collection.immutable.TreeMap
import scala.collection.mutable.HashMap
import scala.collection.Map
import scala.math.{min,max,abs}
import scala.util.matching.Regex

import sml.io.{baseName,ls,join,removeSuffix}
import sml.nlp.chunker.{Chunk, chunkSentence}
import sml.nlp.tree.{buildTree, ParseTree}

/**
A library to load core nlp xml into objects
*/
package object nlp
{
	class Document(val id: String, val sentences: Seq[Sentence], val corefGroups: Set[CorefGroup])
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
		def tokens:Seq[Token] = sentences.flatMap(_.tokens)

		/**
		Returns all the noun phrases in the document
		*/
		def nounPhrases:Iterable[Seq[Token]] =
		{
			sentences.flatMap(_.nounPhrases)
		}

		/**
		Returns all the verb phrases in the document
		*/
		def verbPhrases:Iterable[Seq[Token]] =
		{
			sentences.flatMap(_.verbPhrases)
		}

		/**
		 * Determines the lexical distance between two phrases
		 */
		def lexicalDistance(phrase:Seq[Token], other:Seq[Token]):Int =
		{
			//check if the phrases overlap
			if(!phrase.overlaps(other))
			{
				//determine the order of the phrases
				val ordered = Seq(phrase, other).sorted
				val first = ordered.head
				val second = ordered.last

				//if the two phrases are in the same sentence then do a simple computation
				if(phrase.shareSentence(other))
				{
					second.head.id - first.last.id
				}
				else
				{
					//the distance from the end of the first phrase to the end of 
					//its sentence
					val firstDist = sentenceByToken(first.last).size - first.last.id

					//the distance from the beginning of the second phrase to the
					//start of its sentence
					val secondDist = second.head.id

					//sum all the tokens of all the sentences in between
					val betweenDist = Range(first.last.sentenceId+1, second.head.sentenceId).map(i => sentenceById(i).size).sum

					firstDist + secondDist + betweenDist
				}
			}
			else
			{
				0
			}
		}

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
		Returns the boarders of the context
		*/
		def contextBoarders(span:Seq[Token], window:Int):(Int, Int) =
		{
			val sent = sentenceById(span.head.sentenceId)
			(max(sent.minTokenId, span.head.id - window), min(span.last.id + window, sent.maxTokenId))
		}

		/**
		Returns a the context of the span, a fix window on either side
		*/
		def context(span:Seq[Token], window:Int):(Seq[Token], Seq[Token]) =
		{
			val sent = sentenceByToken(span.head)
			val (start, end) = contextBoarders(span, window)
			val left = sent.tokensById(Range(start, span.head.id))
			val right = sent.tokensById(Range(span.last.id+1, end+1))
	
			(left, right)
		}

		/**
		Returns the window, including the given sequence, on either side of the
		sequence
		*/
		def window(subseq:Seq[Token], window:Int):Seq[Token] =
		{
			val sent = sentenceByToken(subseq.head)
			val (start, end) = contextBoarders(subseq, window)

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

		override val hashCode = id.hashCode
	}

	/**
	A document with the path to its location
	*/
	trait DocRef
	{
		/**
		 * Get the document
		 */
		def doc:Document

		def docId:String	

		/**
		 * The path to the document
		 */
		val path:String

		def asLazy:LazyDocRef = new LazyDocRef(path, docId)

		def inMemory:InMemDocRef = new InMemDocRef(doc, path)
	}

	/**
	 * Keeps a reference to a document in memory
	 */
	class InMemDocRef(override val doc:Document, override val path:String) extends DocRef
	{
		def docId:String = doc.id
	}

	/**
	 * A lazy document reference that loads the document on demand
	 */
	class LazyDocRef(val path:String, val docId:String) extends DocRef
	{
		def doc:Document = 
		{
			println(s"loading $path")
			parseXMLDoc(loadFile(path), docId)
		}
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
	In the stanford annotation the id starts at 1
	*/
	class Sentence(val id:Int, allTokens:Seq[Token], edges:Map[(Int,Int), String], val parseTree:ParseTree)
	{
		val tokMap: TreeMap[Int,Token] = TreeMap(allTokens.map( (t:Token) => (t.id,t) ):_*)

		//map of token to dependency type, governor
		val dependencies = edges.map( kv => (kv._1._2, (kv._2, kv._1._1)) ).toMap

		lazy val chunks:Seq[Chunk] = chunkSentence(this)

		def tokens: Seq[Token] = tokMap.values.toSeq

		def tokenById(tokenId: Int): Token = tokMap(tokenId)

		def tokensById(span:Range):Seq[Token] = 
		{
			if(span.nonEmpty)
				tokens.slice(span.head-1, span.end-1)
			else
				Seq()
		}

		/**
		Returns all the tokens with the given dependency type
		*/
		def tokensWithType(depType: String): Iterable[Token] =
		{
			tokens.filter(t => hasDepType(t, depType))
		}

		/**
		Returns all the noun phrases in the sentence
		*/
		def nounPhrases:Iterable[Seq[Token]] =
		{
			parseTree.nounPhrases.map(tokensById).filter(_.exists(t => t.isNoun))
		}

		/**
		Returns all the verb phrases in the sentence
		*/
		def verbPhrases:Iterable[Seq[Token]] =
		{
			parseTree.verbPhrases.map(tokensById).filter(_.exists(t => t.isVerb))
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
			chunks.slice(start,end+1)
		}

		/**
		Returns the range of indexes the token sequence overlaps
		*/
		def coveredChunkRange(seq:Seq[Token]): Range =
		{
			val indexed = chunks.zipWithIndex
			val start = indexed.find(c => c._1.hasToken(seq.head)).get
			val end = indexed.find(c => c._1.hasToken(seq.last)).get

			Range(start._2, end._2+1)
		}

		/**
		Returns the dependency type for the token
		*/
		def depType(token:Token):Option[String] =
		{
			if(hasDepType(token)) Some(dependencies(token.id)._1) else None
		}

		/**
		Returns the parent of the token
		*/
		def parent(token:Token):Option[Token] =
		{
			if(dependencies.contains(token.id))
				tokMap.get(dependencies(token.id)._2)
			else
				None	
		}

		/**
		Returns the syntatic children of the token
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
		the sequence will be root to token - the list is ordered from the top down
		*/
		def ancestors(token:Token):Seq[Token] =
		{
			def helper(current:Token, partial:List[Token]):List[Token] =
			{
				val parentTok = parent(current)
				val next = current :: partial 

				//if there is no parent then stop
				if(parentTok.isEmpty)
					next
				
				//else recurse up
				else
					helper(parentTok.get, next)
			}

			helper(token, List())
		}

		/**
		Returns the tokens syntatically inbetween the two tokens, bookended by
		the two given tokens
		*/
		def middleChildren(ancestor:Token, descendent:Token):Seq[Token] =
		{
			ancestors(descendent).dropWhile(_ != ancestor)
		}

		/**
		 * Returns all the descendents
		 */
		def descendants(token:Token):Iterable[Token] =
		{
			def helper(current:Token):Set[Token] = 
			{
				val kids = children(current)

				//include the current token's children and the children's children
				kids.toSet ++ kids.flatMap(c => helper(c)).toSet
			}

			helper(token)
		}

		/**
		Returns the common ancestor 
		*/
		def commonAncestor(one:Token, another:Token):Option[Token] =
		{
			//make sure the two tokens are in the same sentence
			if(sameSentence(one, another))
				commonAncestor(ancestors(one), ancestors(another))
			else
				None
		}

		/**
		Find the first place the two ancestor paths overlap
		*/
		def commonAncestor(one:Seq[Token], another:Seq[Token]):Option[Token] =
		{
			val prefix = one.zip(another).takeWhile(p => p._1 == p._2)

			//make sure there is a prefix
			if(prefix.isEmpty)
				None
			else
				Some(prefix.last._1)
		}

		/**
		Returns the syntatic path between the two tokens
		*/
		def path(start:Token, end:Token):Option[Seq[Token]] =
		{
			//if the two tokens are in the same sentence then find a possible
			//path between them
			if(sameSentence(start,end))
			{
				//if there is a common ancestor, then there is a syntatic
				//relationship between the two tokens
				commonAncestor(start, end) match
				{
					case an:Some[Token] =>
					{
						val common = an.get

						//if the start is the common ancestor
						if(start == common)
							Some(middleChildren(start,end))
						
						else if(end == common)
							Some(middleChildren(end,start).reverse)
						
						else
							Some(middleChildren(common,start).reverse ++ middleChildren(common,end))
					}
					case _ => None
				}
			}
			else
			{
				None
			}
		}

		/**
		 * Returns the nearsest token with the given type
		 */
		def nearestTokenWithType(seed:Token, searchType:String, subset:Set[Token]=null):Option[Token] =
		{
			val candidates = tokensWithType(searchType).filter(_ != seed).map(c => (c, syntaticDistance(seed,c))).filter(_._2.nonEmpty)

			//if a subset is specified, only consider those in it
			val sub = if(subset != null)
				candidates.filter(p => subset.contains(p._1))
			else
				candidates

			//if there are any candidates find the closest one
			if(sub.nonEmpty)
				Some(sub.minBy(_._2)._1)
			else
				None
		}
	
		/**
		 * Returns the nearest subject to the token
		 */
		def nearestSubject(seed:Token):Option[Token] = nearestTokenWithType(seed, GENERIC_SUBJECT)
		
		def nearestSubject(seed:Token, subset:Set[Token]):Option[Token] = nearestTokenWithType(seed, GENERIC_SUBJECT, subset)

		/**
		 * Returns the nearest object to the token that is descendant
		 */
		def nearestObject(seed:Token):Option[Token] = nearestObject(seed, descendants(seed).toSet)

		def nearestObject(seed:Token, subset:Set[Token]):Option[Token] = nearestTokenWithType(seed, GENERIC_OBJECT, subset)

		/**
		Returns the nearest object to the token who is not nescessarily a descendant
		*/
		def nearestSentObject(seed:Token):Option[Token] = 
		{
			val obj = nearestObject(seed)

			if(obj.isEmpty) nearestTokenWithType(seed, GENERIC_OBJECT) else obj
		}

		/**
		Returns the nearest object to the token that is also a parent or ancestor
		*/
		def nearestParentObject(seed:Token):Option[Token] = nearestTokenWithType(seed, GENERIC_OBJECT, ancestors(seed).toSet)

		/**
		 * Returns the number of tokens in the parse tree between the two
		 * tokens
		 */
		def syntaticDistance(token:Token, other:Token):Option[Int] =
		{
			val firstAn = ancestors(token)
			val secondAn = ancestors(other)

			//if either one is an ancestor of the other then compare their depth
			if(firstAn.contains(other) || secondAn.contains(token))
			{
				Some(abs(depth(token) - depth(other)))
			}
			else
			{
				val common = commonAncestor(firstAn, secondAn)

				//if there is a common ancestor measure the distance to it from
				//both tokens
				if(common.nonEmpty)
				{
					val commonDepth = depth(common.get)

					Some((depth(token) - commonDepth) + (depth(other) - commonDepth))
				}
				else
					None
			}
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
			if(tokens.size > 1)
				tokens.minBy(depth)
			else
				tokens.head
		}

		/**
		Returns the maximum token id in the sentence
		*/
		val maxTokenId: Int = size

		/**
		Returns the minimum token id
		*/
		val minTokenId: Int = 1

		/**
		 * Returns true if all the token ids are in the sentence
		 */
		def validIds(tokenIds:Range):Boolean = (minTokenId <= tokenIds.head) && (tokenIds.last <= maxTokenId)
	
		def validId(tokenId:Int):Boolean = (minTokenId <= tokenId) && (tokenId <= maxTokenId)

		def hasDepType(token:Token):Boolean = token.sentenceId == id && dependencies.contains(token.id)

		def hasDepType(token:Token, dep:String):Boolean = depType(token).getOrElse("").contains(dep)

		def isSubject(token:Token):Boolean = hasDepType(token, GENERIC_SUBJECT)

		def isObject(token:Token):Boolean = hasDepType(token, GENERIC_OBJECT)

		def isClausalComplement(token:Token):Boolean = hasDepType(token, CAUSAL_COMPLEMENT)

		def isRelativeClause(token:Token):Boolean = hasDepType(token, RELATIVE_CLAUSE)

		/**
		 * Returns true if the token plays the role of a modifier in the sentence
		 */
		def isMod(token:Token):Boolean = depType(token).getOrElse("").contains("mod")
	
		/**
		Returns true if the token is an adjectival modifier 
		*/
		def isAdjMod(token:Token):Boolean = hasDepType(token, ADJ_MOD)

		/*{
			if(hasDepType(token))
			{
				val dType = depType(token).getOrElse("")

				dType.contains("mod") || dType == "advcl"
			}
			else false
		} */
	
		/**
		Returns true if the token has a modifier
		*/
		def hasMod(token:Token):Boolean = children(token).exists(isMod)
		
		def isAux( token: Token ): Boolean = hasDepType(token, GENERIC_AUX)

		def objects: Iterable[Token] = tokensWithType(GENERIC_OBJECT)

		def subjects: Iterable[Token] = tokensWithType(GENERIC_SUBJECT)

		def size: Int = tokens.size

		override def toString = tokens.map(_.toString).mkString(" ")

		override def equals(other:Any):Boolean = other match
		{
			case that:Sentence => that.id == id
			case _ => false
		}

		override val hashCode:Int = id.hashCode

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

		def isPastParticple = pos == "VBN"

		def isNoun = pos.startsWith("NN")

		def isProperNoun = pos.startsWith("NNP")

		def isAdj = pos.startsWith("JJ")

		def isAdv = pos.startsWith("RB")

		def isPronoun = pos.startsWith("PR")

		def isConjuction = pos.startsWith("CC")

		def isPunctuation = PUNCTUATION.contains(pos)

		def isBlackListed = blackList.contains(word)

		def isQuote = quoteReg.findFirstIn(word).nonEmpty

		def simplePOS:String =
		{
			if(isVerb) "VERB"
			else if(isNoun) "NOUN"
			else if(isProperNoun) "PROPERNOUN"
			else if(isAdj) "ADJECTIVE"
			else if(isAdv) "ADVERB"
			else if(isPronoun) "PRONOUN"
			else if(isPunctuation) "PUNCTUATION"
			else "OTHER"
		}

		/**
		 * Returns true if the token contains alpha-numeric characters
		 */
		def alphaNum:Boolean = 
		{
			if(!isBlackListed)
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
		override val hashCode = id.hashCode + sentenceId.hashCode

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
	 * An implicit class for sequences of tokens, aka phrases
	 */
	implicit class Phrase(val tokens:Seq[Token]) extends Ordered[Seq[Token]]
	{
		/**
		 * Returns true if the two phrases overlap
		 */
		def overlaps(other:Seq[Token]):Boolean =
		{
			tokens.toSet.intersect(other.toSet).nonEmpty
		}

		/**
		 * Returns a Range that covers the ids of the Phrase's tokens
		 */
		def toRange:Range =
		{
			Range(tokens.head.id, tokens.last.id+1)
		}

		/**
		 * Returns true if the phrases are in the same sentence
		 */
		def shareSentence(other:Seq[Token]):Boolean =
		{
			shareSentence(other.map(_.sentenceId).toSet)
		}

		/**
		 * Returns true if the phrases are in the same sentence
		 */
		def shareSentence(sentenceIds:Set[Int]):Boolean =
		{
			tokens.map(_.sentenceId).exists(sentenceIds.contains)
		}

		/**
		 * Returns true if of one of the tokens is in a dependency tree
		 */
		def inDepTree(implicit doc:Document):Boolean =
		{
			tokens.exists(t => doc.sentenceByToken(t).hasDepType(t))
		}

		/**
		 * Returns the syntatic head of the phrase, assuming all the tokens
		 * are in the same sentence
		 */
		def syntaticHead(implicit doc:Document):Token =
		{
			//determine the sentence of the span
			val sent = doc.sentenceByToken(tokens.head)

			//find the highest token in the tree
			tokens.minBy(m => sent.depth(m))
		}

		/**
		 * Returns true if the two phrases have a dependency relationship
		 */
		def hasDepRelationship(other:Seq[Token], doc:Document):Boolean =
		{
			shareSentence(other) && inDepTree(doc) && other.inDepTree(doc)
		}

		/**
		 * Compares two phrases
		 */
		def compare(other:Seq[Token]):Int =
		{
			val sentCmp = tokens.map(_.sentenceId).min.compare(other.map(_.sentenceId).min)

			//first compare sentence ids, if there is a tie then go to tokens
			if(sentCmp == 0)
			{
				tokens.map(_.id).min.compare(other.map(_.id).min)
			}
			else
				sentCmp
		}
	}

	/**
	Returns true if the two tokens appear the same sentence
	*/
	def sameSentence(one:Token, another:Token):Boolean =
	{
		one.sentenceId == another.sentenceId
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
	def loadDocs(dirName:String):Iterable[Document] = ls(dirName).filter(m => !m.endsWith(".swp.xml")).map(parseDoc)

	/**
	Loads all the annotated documents given with the given prefix and suffix
	*/
	def loadDocs(dir:String, prefix:String, suffix:String, docNames:Iterable[String]):Iterable[Document] = 
	{
		loadDocRefs(dir, prefix, suffix, docNames).map(_.doc)
	}

	def loadDocs(dir:String, suffix:String, docNames:Iterable[String]): Iterable[Document] = loadDocs(dir, "", suffix, docNames)

	/**
	Loads documents with a reference to their path
	*/
	def loadDocRefs(dir:String, prefix:String, suffix:String, docNames:Iterable[String]):Iterable[DocRef] = 
	{
		docNames.map(n => parseDocRef(join(dir, prefix+n+suffix), prefix))
	}
	
	/**
	Parses the document from the given string
	*/
	def parseDocFromString(xml:String, docId:String):Document =
	{
		parseXMLDoc(loadString(xml), docId)
	}

	/**
	Returns a document parsed from an xml file
	*/
	def parseDocRef(fileName:String, prefix:String):DocRef =
	{
		//load the doc as xml and parse it
		new InMemDocRef(parseXMLDoc(loadFile(fileName), parseDocId(fileName,prefix)), fileName)
		//new LazyDocRef(fileName, parseDocId(fileName,prefix))
	}

	/**
	Returns a document parsed from an xml file
	*/
	def parseDoc(fileName:String, prefix:String):Document = parseDocRef(fileName, prefix).doc

	def parseDoc(fileName:String):Document= parseDoc(fileName, "")

	/**
	Parses the document from the xml
	*/
	def parseXMLDoc(xml:Elem, docId:String): Document =
	{
		//parse all the sentences
		val sentences = (xml \\ "sentences" \ "sentence").map(parseSentence)

		//parse out all the coref groups
		val coref = (xml \ "root" \ "document" \ "coreference" \ "coreference").map(parseCoref).toSet

		new Document(docId, sentences, coref)
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

		//read the parse tree
		val tree = buildTree((node \ "parse").text)

		new Sentence(id, toks, edges, tree)
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
	A regex for quotes
	*/
	val quoteReg = new Regex("^[\"\'`]+$")

	/**
	List all the Penn-Treebank POS tags
	*/
	val POS = Set("CC",	"CD",	"DT",	"EX",	"FW",	"IN", "JJ", "JJR", "JJS", 
	"LS", "MD", "NN",	"NNS", "NNP", "NNPS", "PDT", "POS", "PRP", "PRP$", "RB", 
	"RBR", "RBS", "RP", "SYM", "TO", "UH", "VB", "VBD", "VBG", "VBN", "VBP", 
	"VBZ", "WDT", "WP", "WP$", "WRB")

	val GENERIC_SUBJECT = "subj"
	val GENERIC_OBJECT = "obj"
	val GENERIC_AUX = "aux"
	val CAUSAL_COMPLEMENT = "xcomp"
	val RELATIVE_CLAUSE = "rcmod"
	val ADJ_MOD = "amod"
	
	val PUNCTUATION = Set(".", ",", ":")

	val blackList = Set("'s", "-LRB-", "-RRB-", "-LSB-", "-RSB-")

	/**
	Give some colors for graphviz
	*/
	val colors = Seq("green", "red", "blue", "yellow", "purple", "pink", "orange")

	//stopwords
	val stopwords = Set("a", "about", "above", "after", "again", "against", "all", "am", "an", "and", "any", "are", "aren't", "as", "at", "be", "because", "been", "before", "being", "below", "between", "both", "but", "by", "can't", "cannot", "could", "couldn't", "did", "didn't", "do", "does", "doesn't", "doing", "don't", "down", "during", "each", "few", "for", "from", "further", "had", "hadn't", "has", "hasn't", "have", "haven't", "having", "he", "he'd", "he'll", "he's", "her", "here", "here's", "hers", "herself", "him", "himself", "his", "how", "how's", "i", "i'd", "i'll", "i'm", "i've", "if", "in", "into", "is", "isn't", "it", "it's", "its", "itself", "let's", "me", "more", "most", "mustn't", "my", "myself", "no", "nor", "not", "of", "off", "on", "once", "only", "or", "other", "ought", "our", "ourselves","out", "over", "own", "same", "shan't", "she", "she'd", "she'll", "she's", "should", "shouldn't", "so", "some", "such", "than", "that", "that's", "the", "their", "theirs", "them", "themselves", "then", "there", "there's", "these", "they", "they'd", "they'll", "they're", "they've", "this", "those", "through", "to", "too", "under", "until", "up", "very", "was", "wasn't", "we", "we'd", "we'll", "we're", "we've", "were", "weren't", "what", "what's", "when", "when's", "where", "where's", "which", "while", "who", "who's", "whom", "why", "why's", "with", "won't", "would", "wouldn't", "you", "you'd", "you'll", "you're", "you've", "your", "yours", "yourself", "yourselves")							  
							  
	/**
	Main Function to run a simple test
	*/
	def main(args: Array[String])
	{
		val doc = parseDoc(args(0))

		//print out all the sentences
		for( (sentence,i) <- doc.sentences.zipWithIndex )
		{
			println(s"Sentence $i: ${sentence.nounPhrases.toSeq.sortBy(_.head.id).map(_.mkString(" ")).mkString("\n")}")
			//println(sentence.verbPhrases.mkString("\n"))		
		}
	}
}
