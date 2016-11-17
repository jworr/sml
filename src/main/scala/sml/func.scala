package sml

import scala.collection.mutable.ArrayBuffer

/**
Contains tools for functional programming
*/
object func
{
	/**
	Performs a Binary search on an array
	*/
	def binSearch[T](seq:Seq[T], item:T): Option[T] =
	{
		val index = java.util.Arrays.binarySearch(seq.asInstanceOf[Array[AnyRef]], item)

		//if the object was found return it
		if(index >= 0)
			Some(seq(index))
		
		//else return None
		else
			None
	}

	/**
	Weaves the two iterables together
	*/
	def intersperse[T](first:Iterable[T], second:Iterable[T]):Iterable[T] =
	{
		first.zip(second).flatMap(p => Seq(p._1, p._2))
	}

	/**
	Returns all combination pairs
	*/
	def combinations2[A](items:Iterable[A]): Iterable[(A,A)] =
	{
		items.toSeq.combinations(2).map(i => (i(0), i(1))).toIterable
	}

	/**
	Returns all combination triples
	*/
	def combinations3[A](items:Iterable[A]): Iterable[(A,A,A)] =
	{
		items.toSeq.combinations(3).map(i => (i(0), i(1), i(2))).toIterable
	}

	/**
	Returns all combination quadruples
	*/
	def combinations4[A](items:Iterable[A]): Iterable[(A,A,A,A)] =
	{
		items.toSeq.combinations(4).map(i => (i(0), i(1), i(2), i(3))).toIterable
	}
	
	/**
	Returns a all permutations of a specified length 
	*/
	def permute[A](items:Iterable[A], size:Int):Iterable[Seq[A]] = 
	{
		items.toSet.subsets(size).map(_.toSeq.permutations).flatten.toIterable
	}

	/**
	Returns all permutation pairs
	*/
	def permutations2[A](items:Iterable[A]): Iterable[(A,A)] =
	{
		permute(items,2).map(i => (i(0), i(1)))
	}

	/**
	Returns all permutation triples
	*/
	def permutations3[A](items:Iterable[A]): Iterable[(A,A,A)] =
	{
		permute(items,3).map(i => (i(0), i(1), i(2)))
	}

	/**
	Returns all permutation triples
	*/
	def permutations4[A](items:Iterable[A]): Iterable[(A,A,A,A)] =
	{
		permute(items,4).map(i => (i(0), i(1), i(2), i(3)))
	}

	/**
	Returns all pairs (combinations) of the two iterables
	*/
	def combine2[A,B](seq1:Iterable[A], seq2:Iterable[B]):Iterable[(A,B)] =
	{
		/**
		Do all pairwise combinations
		*/
		val values = for(item1 <- seq1) yield
		{
			for(item2 <- seq2) yield
			{
				(item1, item2)
			}
		}

		values.flatten
	}

	/**
	Does all combinations of all given collections - subway sandwich style
	*/
	def combine[T](items:Iterable[T]*):Iterable[Iterable[T]] =
	{
		val answer = new ArrayBuffer[Iterable[T]]()

		def helper(part:List[T], lists:Iterable[Iterable[T]])
		{
			if(lists.size > 0)
			{
				//get the current iterable to add to everything
				val current = lists.head
				val rest = lists.tail

				//for each item in the current collection, add it to all the
				//existing partial solutions
				for( item <- current ) 
				{
					helper(item :: part, rest)
				}
			}
			else
			{
				answer += part
			}
		}

		helper(List(), items)

		answer
	}

	implicit class ExtendedSeq[T](val items:Seq[T])
	{
		/**
		Groups up adjancent items that are true for the same predicate
		*/
		def adjGroup[V](pred:T => V):Iterable[Seq[T]] =
		{
			def helper(current:Seq[T], results:Vector[Seq[T]]):Vector[Seq[T]] =
			{
				//if there is nothing left then return
				if(current.isEmpty)
					return results
				
				//if there is only one thing left then return it in its own group
				else if( current.tail.isEmpty )
					return results :+ current
				
				//else split up what remains
				else
				{
					val (start,end) = current.partition(i => pred(i) == pred(current.head))
					
					return helper(end, results :+ start)
				}
			}

			helper(items, Vector())
		}
	}
}
