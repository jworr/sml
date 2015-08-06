package sml

/**
Contains tools for functional programming
*/
object func
{
	/**
	Returns all combination pairs
	*/
	def combinations2[A](items:Iterable[A]): Iterator[(A,A)] =
	{
		for(s <- items.toSet.subsets(2)) yield
		{
			(s.head, s.tail.head)
		}
	}

	/**
	Returns all permutation pairs
	*/
	def permutations2[A](items:Iterable[A]): Iterator[(A,A)] =
	{
		val results = for(s <- items.toSet.subsets(2)) yield
		{
			val first = s.head
			val second = s.tail.head

			List((first,second), (second,first))
		}

		return results.flatten
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
}
