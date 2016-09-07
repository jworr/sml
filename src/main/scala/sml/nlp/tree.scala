package sml.nlp

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
		def nodes:Iterable[TreeNode] =
		{
			Seq(root) ++ descendents(x => true, root)
		}

		/**
		Returns all the descendents of the node
		*/
		def descendents(filterPred:TreeNode=>Boolean,node:TreeNode):Iterable[TreeNode] =
		{
			val next = node.children.filter(filterPred)
			next ++ next.flatMap(descendents(filterPred, _))
		}
	}

	abstract class TreeNode(val nodeType:String)
	{
		def children:Set[TreeNode]

		def isLeaf:Boolean = children.isEmpty

		def isClause:Boolean = nodeType == "S" || nodeType == "SBAR"
	}

	case class Internal(nType:String, override val children:Set[TreeNode]) 
		extends TreeNode(nType)

	case class Leaf(nType:String, val content:String, val index:Int) extends TreeNode(nType)
	{
		def children = Set()
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

			Internal(typeText, children.map(parseNode).toSet)
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
