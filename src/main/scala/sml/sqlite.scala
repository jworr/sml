package sml.db

import java.sql.DriverManager
import java.sql.Connection

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
		DriverManager.getConnection(dbPath)
	}
}
