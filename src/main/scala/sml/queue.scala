package sml

/**
A package for a queue with a limited size
*/
object queue
{
	/**
	 * Keeps a limited number of assignments in order based on their score
	 */
	class BoundedQueue[T <% Ordered[T]](val limit:Int, items:Seq[T]=Seq()) extends Iterable[T]
	{
		val queue = items

		/**
		 * Increments the beam, maintaining its size
		 */
		def +(next:T):BoundedQueue[T] =
		{
			val newQueue = if(queue.contains(next))
			{
				queue	
			}
			else
			{
				val incremented = (next +: queue).sorted

				//make sure the beam does not grow beyond its intended size
				if(incremented.size > limit) incremented.tail else incremented
			}

			new BoundedQueue(limit, newQueue)
		}

		/**
		 * Joins two beams together, creating a single beam
		 */
		def ++(other:BoundedQueue[T]):BoundedQueue[T] =
		{
			val joined = removeDuplicates((queue ++ other.queue).sorted)
			val start = scala.math.max(joined.size - limit, 0)
			val newQueue = joined.slice(start, joined.size)

			new BoundedQueue(limit, newQueue)
		}
		
		/**
		 * Removes duplicates from the sorted sequence
		 */
		def removeDuplicates(beam:Seq[T]):Seq[T] =
		{
			if(beam.size <= 1)
			{
				beam
			}
			else
			{
				val withIndex = beam.zipWithIndex

				//get the indexes of the duplicate items	
				val dups = (for( p <- withIndex.sliding(2) if p.head._1 == p.last._1 ) yield p.head._2).toSet

				//filter out all the duplicates then drop the index
				withIndex.filter(p => !dups.contains(p._2)).map(_._1)
			}
		}

		/**
		 * Returns the highest scoring assignment
		 */
		def max:T = queue.last

		/**
		 * Returns the lowest scoring assignment
		 */
		def min:T = queue.head

		/**
		 * Returns the number of items in the queue
		 */
		override def size:Int = queue.size

		/**
		 * Return an iterator over the queue
		 */
		def iterator:Iterator[T] = queue.iterator

		/**
		Check if two queues are equal to each other
		*/
		override def equals(obj:Any):Boolean = obj match
		{
			case q:BoundedQueue[T] =>
			{
				if(size == q.size)
				{
					items.zip(q).forall(p => p._1 == p._2)
				}
				else
				{
					false
				}
			}
			case _ => false
		}

		/**
		 * Return the queue as a string
		 */
		override def toString:String = 
		{
			queue.zipWithIndex.map(p => s"${p._2}: ${p._1}").mkString("\n")
		}
	}
}
