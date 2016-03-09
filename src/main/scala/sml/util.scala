package sml

import scala.collection.mutable.HashMap
import scala.math.log

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
