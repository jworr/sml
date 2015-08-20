package sml

/**
Contains general untility functions
*/
object util
{
	/**
	Generates and returns a time based UUID
	*/
	def UUID:String = java.util.UUID.randomUUID.toString
}
