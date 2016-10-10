package sml.nlp

import scala.math.abs

import scala.util.matching.Regex
import scala.collection.mutable.ArrayBuffer

import sml.util.{tuplesToMap, intersect}

/**
A module for building and using parse trees
*/
object tree
{
	/**
	A parse tree
	*/
	class ParseTree(val root:TreeNode)
	{
		//all the clauses in the tree
		val clauses = nodes.filter(_.isClause)

		//a map of token id to the clause node
		val clauseIndex =
		{
			val pairs = for(clause <- clauses) yield
			{
				leafs(descendents(isNotClause, clause)).map(l => (l.index, clause))
			}

			tuplesToMap(pairs.flatten).mapValues(_.toSet)
		}

		//make a map of child nodes to parent
		val parents:Map[TreeNode, TreeNode] =
		{
			//for each node generate all the children edges
			val edges = for(node <- nodes) yield node match
			{
				case n:Internal => n.children.map(c => (c,n))
				case _ => Seq()
			}

			edges.flatten.toMap
		}

		//make a map of token index to node
		val index:Map[Int, Leaf] =
		{
			val entries = for(node <- nodes if node.isLeaf) yield node match
			{
				case l:Leaf => (l.index, l)
			}

			entries.toMap
		}

		/**
		Returns true if the two tokens are in the same clause
		*/
		def sameClause(token:Token, other:Token):Boolean = sameClause(token.id, other.id)

		/**
		Returns true if the two tokens are in the same clause
		*/
		def sameClause(token:Int, other:Int):Boolean =
		{
			intersect(clauseIndex.getOrElse(token, Set()), clauseIndex.getOrElse(other, Set()))
		}

		/**
		Traverse all the nodes in the tree
		*/
		def nodes:Iterable[TreeNode] = Seq(root) ++ descendents(root)
		
		/**
		 * Returns the parent node
		 */
		def parent(node:TreeNode):Option[TreeNode] =
		{
			if(parents.contains(node))	Some(parents(node)) else None
		}

		/**
		 * Returns the parent node, based on the token index
		 */
		def parent(node:Int):Option[TreeNode] = 
		{
			if(index.contains(node)) parent(index(node)) else None
		}

		/**
		 * Returns the parent node, based on the token index
		 */
		def parent(node:Token):Option[TreeNode] = parent(node.id)

		/**
		Returns all the descendents of the node with the their depth from the
		current node , with the predicate determining which nodes to keep
		*/
		def descendentsWithDepth(pred:TreeNode=>Boolean, depth:Int, node:TreeNode):Seq[(TreeNode,Int)] =
		{
			val nextDepth = depth + 1
			val next = node.children.filter(pred)
			next.map(n => (n,nextDepth)) ++ next.flatMap(descendentsWithDepth(pred, nextDepth, _))
		}
		
		/**
		Returns all the descendants of the node
		*/
		def descendentsWithDepth(node:TreeNode):Seq[(TreeNode,Int)] = 
		{
			descendentsWithDepth(x => true, 0, node)
		}

		def descendentsWithDepth(token:Token):Seq[(TreeNode,Int)] =
		{
			val node = index.get(token.id)

			if(node.nonEmpty) descendentsWithDepth(node.get) else Seq()
		}

		/**
		Returns all the descendants of the node
		*/
		def descendents(pred:TreeNode=>Boolean, node:TreeNode):Seq[TreeNode] =
		{
			descendentsWithDepth(pred, 0, node).map(_._1)
		}

		def descendents(node:TreeNode):Seq[TreeNode] = descendents(x => true, node)
		
		/**
		Returns all the noun phrases/chunks
		*/
		def nounPhrases:Iterable[Range] = qualifiedPhrases(x => x.isNounPhrase)
				
		/**
		Returns all the verb phrases/chunks
		*/
		def verbPhrases:Iterable[Range] = qualifiedPhrases(x => x.isVerbPhrase)
		
		/**
		 * Finds the nearest dependent noun phrase
		 */
		def nearestObject(head:Token):Option[Range] =
		{
			val phrases = descendentsWithDepth(head).filter(_._1.isNounPhrase)
			val closest = closestPhrase(head.id) _
		
			//if there is a noun phrase, look for the closest
			if(phrases.nonEmpty)
				Some(phrases.map(p => (nodeToRange(p._1),p._2)).reduce(closest)._1)
			
			else
				None	
		}

		/**
		Finds the nearest subject - the nearest governing noun phrase
		*/
		def nearestSubject(head:Token):Option[Range] =
		{
			nearestCousin(_.isNounPhrase, _.isVerbPhrase, head)
		}

		/**
		Finds the governing verb -- looks for the verb phrase this token
		is apart of
		*/
		def governingVerb(head:Token):Option[Range] =
		{
			def helper(node:TreeNode):Option[TreeNode] =
			{
				val par = parent(node)

				//if the current node is a verb phrase, return it
				if(node.isVerbPhrase) Some(node)

				//if there is no parent
				else if(par.isEmpty) None

				//else recurse up
				else helper(par.get)
			}

			lookupNode(helper, index.get(head.id))
		}

		/**
		Finds the dependent verb -- looks for the verb phrase that is a sibling
		of the token's phrase
		*/
		def dependentVerb(head:Token):Option[Range] =
		{
			nearestCousin(_.isVerbPhrase, _.isNounPhrase, head)
		}

		/**
		Finds the nearest cousin -- a phrase with that is siblings with the
		token's phrase's ancestors
		*/
		def nearestCousin(pred:TreeNode=>Boolean, ignore:TreeNode=>Boolean, head:Token):Option[Range] =
		{
			def helper(current:TreeNode):Option[TreeNode] =
			{
				val par = parent(current)

				//if there is no parent then return None
				if(par.isEmpty)
					None

				//if the parent is not the target type go up
				else if(ignore(par.get))
					helper(par.get)

				//else recurse up
				else
				{
					//find all the sibling phrases
					val siblings = descendents(par.get)
					val targets = siblings.filter(pred).filter(_ != current)

					//check if there is a phrase that is a sibling phrase
					//if so, return it
					if(targets.nonEmpty)
						//Some(siblings.takeWhile(_ != current).filter(_.isNounPhrase).last)
						Some(siblings.filter(pred).head)
					
					else
						helper(par.get)
				}
			}

			//lookup the node for the token
			lookupNode(helper, index.get(head.id))
		}

		/**
		Returns all the phrases of targeted internal nodes
		*/
		def qualifiedPhrases(target:TreeNode => Boolean):Iterable[Range] =
		{
			val targetPhrases = descendents(root).filter(target)

			//for each internal node, collect all the leaf nodes under it
			targetPhrases.map(nodeToRange).filter(_.nonEmpty)
		}
	}

	/**
	Look up the token with the search function
	*/
	def lookupNode(search:TreeNode=>Option[TreeNode], node:Option[TreeNode]):Option[Range] =
	{
		//if there is a node corresponding to your token
		if(node.nonEmpty)
			optionNodeToRange(search(node.get))
		else 
			None
	}

	/**
	Converts an optional TreeNode to an optional Range
	*/
	def optionNodeToRange(node:Option[TreeNode]):Option[Range] =
	{
		if(node.nonEmpty) Some(nodeToRange(node.get)) else None
	}

	/**
	 * Converts a phrasal node into a range of token indexes
	 */
	def nodeToRange(node:TreeNode):Range =
	{
		val kids = node.children.filter(_.isLeaf).map{ case l:Leaf => l.index }
				
		if(kids.nonEmpty) Range(kids.min, kids.max+1) else Range(0,0)
	}

	/**
	 * Find the closest phrase
	 */
	def closestPhrase(target:Int)(left:(Range,Int), right:(Range,Int)):(Range,Int) =
	{
		//if the two nodes are at equal depth, pick the one that is lexically
		//closer
		if(left._2 == right._2)
			Seq(left,right).minBy(p => closestDist(target, p._1))

		//else just pick the shallower one
		else
			Seq(left,right).minBy(_._2)
	}

	/**
	 * Returns the closest distance to the target in the range
	 */
	def closestDist(target:Int, values:Range):Int =
	{
		values.map(n => abs(target - n)).min
	}

	abstract class TreeNode(val nodeType:String)
	{
		def children:Seq[TreeNode]

		def isLeaf:Boolean = children.isEmpty

		def isClause:Boolean = nodeType == "S" || nodeType == "SBAR"

		def isNounPhrase:Boolean = false

		def isVerbPhrase:Boolean = false
	}

	case class Internal(nType:String, override val children:Seq[TreeNode]) 
		extends TreeNode(nType)
	{
		override def isNounPhrase:Boolean = nodeType == "NP"

		override def isVerbPhrase:Boolean = nodeType == "VP"
	}

	case class Leaf(nType:String, val content:String, val index:Int) extends TreeNode(nType)
	{
		def children = Seq()
	}

	/**
	Builds a parse tree from a string
	*/
	def buildTree(treeString:String):ParseTree =
	{
		new ParseTree(parseNode(tokenizeTreeStr(treeString)))
	}

	/**
	Parses out a node from the sequence of tree tokens
	*/
	def parseNode(tokens:Seq[TreeToken]):TreeNode =
	{
		//the first token should be a left bracket
		val start = tokens(0)
	
		if(!start.isLeft)
			throw new Exception(s"Expected Left got $start")

		//the next token should be a text token, denoting type
		val nodeType = tokens(1) 
		
		val typeText = nodeType match
		{
			case t:Text => t.text
			case _ => throw new Exception(s"Expected Text got $nodeType")
		}

		val nextNode = tokens(2)

		//either the next token is another left, in that case 
		//the node is a non-terminal, and the match bracket should be found
		if(nextNode.isLeft)
		{
			val children = groupChildren(tokens.tail.tail).filter(isNotExtraRight).filter(_.nonEmpty)

			Internal(typeText, children.map(parseNode).toSeq)
		}
		//else the token is a terminal
		else
		{
			val textNode = nextNode match
			{
				case t:Text => t
				case _ => throw new Exception(s"Expected Text with content, got $nextNode")
			}

			Leaf(typeText, textNode.text, textNode.index.get)
		}
	}

	/**
	Groups up all the children of a node
	*/
	def groupChildren(tokens:Seq[TreeToken]):Iterable[Seq[TreeToken]] =
	{
		val results = new ArrayBuffer[Seq[TreeToken]]()
		var end = 0
		var index = 0 

		//if the end is negative then there are no more children
		while(end != -1)
		{
			end = groupTokens(tokens, index)
			
			if(end != -1)
			{
				results.append(tokens.slice(index,end+1))
				index = end + 1
			}
		}
		
		results
	}

	/**
	Group up tokens according to brackets
	*/
	def groupTokens(tokens:Seq[TreeToken], start:Int):Int =
	{
		var count = 0
		var index = start
		var done = false

		//group up children nodes and parse them
		while(index < tokens.size && !done)
		{
			tokens(index) match
			{
				case t:Left => count += 1
				case t:Right => count -= 1
				case _ => {}
			}

			index += 1

			done = count == 0
		}

		if(index == start) -1 else (index -1)
	}

	/**
	Returns true if the "child" node is just the extra end
	*/
	def isNotExtraRight(child:Seq[TreeToken]):Boolean =
	{
		if(child.size == 1)
			child.head match
			{
				case t:Right => false
				case _ => true 
			}
		else
			true	
	}

	/**
	Tokenizes the string representation of the string
	*/
	def tokenizeTreeStr(treeStr:String):Seq[TreeToken] =
	{
		val regex = new Regex("([\\(]|[\\)]|[^\\(\\) ]+)")
		var index = 0

		//parse out all the tokens matching them against the correct token
		val results = for(tokenStr <- regex.findAllIn(treeStr).map(_.trim)) yield tokenStr match
		{
			case "(" => Left()
			case ")" => Right()
			case _  => Text(tokenStr)
		}

		//add in the token indexes
		val withIndex = for((token, next) <- results.sliding(2).map(s => (s.head, s.tail.head))) yield
		{
			if(token.isText && next.isRight)
			{
				index += 1
				Text(token match {case t:Text => t.text}, Some(index))
			}
			else
				token	
		}

		withIndex.toSeq
	}

	/**
	 * Does nothing
	 */
	def noFilter(n:TreeNode):Boolean = true

	/**
	Returns just the text nodes
	*/
	def leafs(nodes:Iterable[TreeNode]):Iterable[Leaf] =
	{
		for(node <- nodes if node.isLeaf) yield node match
		{
			case n:Leaf => n
		}
	}

	/**
	Returns true if the node is not a clause
	*/
	def isNotClause(t:TreeNode):Boolean = !t.isClause

	abstract class TreeToken
	{
		def isLeft:Boolean = false
		def isRight:Boolean = false
		def isText:Boolean = false
	}

	case class Left() extends TreeToken()
	{
		override def isLeft:Boolean = true
		override def toString = "("
	}

	case class Right() extends TreeToken()
	{
		override def isRight:Boolean = true
		override def toString = ")"
	}

	case class Text(val text:String, val index:Option[Int]=None) extends TreeToken()
	{
		override def isText:Boolean = true
		override def toString = text
	}
}
