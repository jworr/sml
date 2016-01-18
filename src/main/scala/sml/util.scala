package sml

import scala.collection.mutable.HashMap

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
