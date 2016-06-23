package sml

import java.sql.{DriverManager, Connection, Time, Timestamp, Date, ResultSet, PreparedStatement}

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

		//set all the parameters
		setParams(statement, values)

		//execute the statement
		statement.executeUpdate()
	}

	/**
	Prepares and executes a query
	*/
	def prepareAndQuery(db:Connection, sql:String, values:Any*): ResultSet =
	{
		val query = db.prepareStatement(sql)

		//set all the parameters
		setParams(query, values)

		//get and return the results
		return query.executeQuery()
	}

	/**
	Sets the statement parameters
	*/
	def setParams(statement:PreparedStatement, values:Iterable[Any])
	{
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
				case null => statement.setNull(i, java.sql.Types.NULL)
			}

			i += 1
		}
	}

	/**
	Converts a result set into an iterable
	*/
	implicit def iterableResults(result:ResultSet):Iterable[ResultSet] = new Iterable[ResultSet]
	{
		override def iterator = new Iterator[ResultSet]
		{
			override def hasNext = result.next
			override def next = result
		}
	}
}
