package sml

import scala.collection.mutable.HashMap

/**
 * Implements various cache method via hashmap
 */
object cache 
{
	/**
	 * An LRU cache implemented via hashmap
	 */
	class LRU[K,V](val maxSize:Int, val gen:K => V) 
	{
		val data = new HashMap[K,(V,Long)]()

		//YES this will eventually be a problem, will address if it becomes a problem
		var current:Long = 0

		/**
		 * Gets a value from the cache
		 */
		def get(key:K):V = data.get(key) match
		{
			case v:Some[(V,Long)] => 
			{
				val value = v.get._1
				current += 1

				//increment the time stamp
				data(key) = (value,current)
		
				value
			}
			case _ =>
			{
				//generate the new value
				val newValue = gen(key)

				//add it to the cache
				put(key, newValue)

				newValue
			}
		}
		
		/**
		 * Puts a value into the cache
		 */
		def put(key:K, value:V) =
		{
			current += 1

			//if we are at max capacity then remove the oldest item
			if(size == maxSize)
			{
				data.remove(data.minBy(_._2._2)._1)
			}

			data.put(key, (value, current))
		}

		/**
		 * Returns whether or not the collection contains the item
		 */
		def contains(key:K):Boolean = data.contains(key)

		/**
		 * Returns the size of the cache
		 */
		def size:Int = data.size
	}	
}
