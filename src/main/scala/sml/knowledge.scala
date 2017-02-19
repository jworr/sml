package sml

import scala.collection.mutable.{HashSet, Queue}

/**
A library for traversing graph based knowledge bases over words and phrases
*/
object knowledge
{
	/**
	Defines a traverser over the knowledge base
	*/
	abstract class Traverser
	{
		/**
		Returns true if the phrase is defined in the knowledge base
		*/
		def hasPhrase(phrase:String):Boolean
		
		/**
		Returns all the phrases related to the given string
		*/
		def relatedPhrases(start:String):Set[String]
	}

	/**
	Defines the need implementation details for a knowledge base traversal
	*/
	abstract class TraverserImpl[T <: Node] extends Traverser
	{
		/**
		Wraps a paraphrase in a node
		*/
		def init( phrase:String ):Iterable[T]
		
		/**
		Returns a nodes successors
		*/
		def successors(node:T):Iterable[T]
		
		/**
		 * Returns the neighbors of the given node
		 */
		def neighbors(start:String):Iterable[T] =
		{
			Traversal[T](this, start)
		}

		/**
		Returns all the phrases related to the given string
		*/
		def relatedPhrases(start:String):Set[String] =
		{
			neighbors(start).map(_.phrase).toSet
		}
	}

	/**
	Traverses over a group of traversers and returned the union of their
	results
	*/
	class UnionTraverser(val traversers:Iterable[Traverser]) extends Traverser
	{
		/**
		Returns true if the phrase is defined in the knowledge base
		*/
		def hasPhrase(phrase:String):Boolean =
		{
			traversers.exists(_.hasPhrase(phrase))
		}
		
		/**
		Returns all the phrases related to the given string
		*/
		def relatedPhrases(start:String):Set[String] =
		{
			traversers.flatMap(_.relatedPhrases(start)).toSet
		}

		/**
		Add in another traverser
		*/
		def +(trav:Traverser):UnionTraverser = 
		{
			val extra = trav match
			{
				case u:UnionTraverser => u.traversers
				case t:Traverser => Seq(t)
			}
			
			new UnionTraverser(traversers ++ extra)
		}
	}

	/**
	A node in a knowledge graph
	*/
	abstract class Node(val phrase:String, val score:Double)
	{
		override def equals(other:Any): Boolean = other match
		{
			case that:Node=> that.phrase == phrase
			case _ => false
		}
		
		override def hashCode:Int = phrase.hashCode
	}

	/**
	 * A traversal of knowledge from a starting point
	 */
	case class Traversal[T <: Node](val traverser:TraverserImpl[T], val start:String) extends Iterable[T]
	{
		/**
		Returns an iterator over phrase
		*/
		def iterator = new Iterator[T]
		{
			val visited = new HashSet[T]()
			val fringe = new Queue[T]() ++ traverser.init(start)

			override def hasNext:Boolean = !fringe.isEmpty
		
			override def next:T = 
			{
				//get the current
				val current = fringe.dequeue

				//mark it as visited
				visited += current

				//add its children to the fringe
				fringe ++= (traverser.successors(current).filter(n => !visited(n)))
			
				//return the current value
				current
			}
		}
	}
}
