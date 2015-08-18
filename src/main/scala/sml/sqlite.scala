package sml

import java.sql.{DriverManager, Connection, Time, Timestamp, Date}

object sqlite
{
	/**
	Returns a connection to a sqlite database
	*/
	def connect(dbPath:String): Connection =
	{
		//load the driver class
		Class.forName("org.sqlite.JDBC")

		//return a new Connection
		DriverManager.getConnection("jdbc:sqlite:"+dbPath)
	}

	/**
	Prepares and executes a statement
	*/
	def prepareAndExecute(db:Connection, sql:String, values:Any*)
	{
		val statement = db.prepareStatement(sql)
		var i = 1
		
		//for each value bind it
		for( value <- values )
		{
			value match
			{
				case v:String => statement.setString(i,v)
				case v:Int => statement.setInt(i,v)
				case v:Double => statement.setDouble(i,v)
				case v:Float => statement.setFloat(i,v)
				case v:Time => statement.setTime(i,v)
				case v:Timestamp => statement.setTimestamp(i,v)
				case v:Date => statement.setDate(i,v)
				case v:Boolean => statement.setBoolean(i,v)
			}

			i += 1
		}

		statement.executeUpdate()
	}
}
