package sml

/**
Contains tools for functional programming
*/
object func
{
	/**
	Returns all combination pairs
	*/
	def combinations2[A](items:Iterable[A]): Iterable[(A,A)] =
	{
		return items.toSeq.combinations(2).map(i => (i(0), i(1))).toIterable
	}

	/**
	Returns all combination triples
	*/
	def combinations3[A](items:Iterable[A]): Iterable[(A,A,A)] =
	{
		return items.toSeq.combinations(3).map(i => (i(0), i(1), i(2))).toIterable
	}

	/**
	Returns all combination quadruples
	*/
	def combinations4[A](items:Iterable[A]): Iterable[(A,A,A,A)] =
	{
		return items.toSeq.combinations(4).map(i => (i(0), i(1), i(2), i(3))).toIterable
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
		return permute(items,2).map(i => (i(0), i(1)))
	}

	/**
	Returns all permutation triples
	*/
	def permutations3[A](items:Iterable[A]): Iterable[(A,A,A)] =
	{
		return permute(items,3).map(i => (i(0), i(1), i(2)))
	}

	/**
	Returns all permutation triples
	*/
	def permutations4[A](items:Iterable[A]): Iterable[(A,A,A,A)] =
	{
		return permute(items,4).map(i => (i(0), i(1), i(2), i(3)))
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

		return values.flatten
	}
}
