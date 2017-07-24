package sml

import scala.collection.mutable.HashMap
import scala.math.log
import scala.util.Random

/**
Contains general untility functions
*/
object util
{
	/**
	Generates and returns a time based UUID
	*/
	def UUID:String = java.util.UUID.randomUUID.toString

	/**
	 * Computes log base 2
	 */
	def log2(n:Double):Double = log(n) / log(2)

	/**
	 * Computes the average
	 */
	def average[T](items:Iterable[T])(implicit toNum:Numeric[T]):Double =
	{
		if(items.nonEmpty)
			toNum.toDouble(items.sum) / items.size
		else
			0.0
	}

	/**
	Averages out the vectors
	*/
	def averageVecs(vecs:Iterable[Seq[Double]]):Seq[Double] =
	{
		val out = new Array[Double](vecs.headOption.map(_.size).getOrElse(0))

		for(vec <- vecs)
		{
			for((value,i) <- vec.zipWithIndex)
			{
				out(i) += value
			}
		}

		for(i <- Range(0,out.size))
		{
			out(i) /= vecs.size
		}

		out
	}

	/**
	Counts up the items in the collection
	*/
	def countItems[T](items:Iterable[T]):Map[T,Int] =
	{
		items.groupBy(x => x).mapValues(_.size)
	}

	/**
	Creates a map from an iterable of non-unique tuples
	*/
	def tuplesToMap[T,S](tuples:Iterable[(T,S)]):Map[T,Iterable[S]] =
	{
		tuples.groupBy(_._1).mapValues(_.map(_._2))
	}

	/**
	Joins a map together
	*/
	def joinMap[T,S](tuples:Iterable[(T,Iterable[S])]*):Map[T,Set[S]] =
	{
		tuples.flatten.groupBy(_._1).mapValues(_.map(_._2).reduce(_ ++ _).toSet)
	}

	/**
	Do the two sets intersect?
	*/
	def intersect[T](items:Set[T], other:Set[T]):Boolean =
	{
		items.exists(other.contains)
	}

	/**
	Pick a random element out the list
	*/
	def randomChoice[T](items:Seq[T]):T =
	{
		randomChoices(1, items).head
	}

	/**
	Pick k random elements from the list
	*/
	def randomChoices[T](k:Int, items:Seq[T]):Iterable[T] =
	{
		Random.shuffle(items).take(k)
	}

	/**
	 * A datastructure to count the number of occurances of an object i.e. a
	 * multiset
	 */
	case class Counter[T](items:Iterable[T])
	{
		val counts = 
		{
			val total = new HashMap[T,Int]()

			//total up the number of each item
			for(item <- items)
			{
				total(item) = total.getOrElse(item, 0) + 1
			}

			total.toMap
		}

		/**
		 * Returns the number of duplicates
		 */
		def duplicateCounts:Int = counts.values.sum - counts.keys.size
	}
}
