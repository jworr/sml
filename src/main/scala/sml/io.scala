package sml

import java.nio.file._
import java.io.File

object io
{
	/**
	Strips the path down to the base file name
	*/
	def baseName(path: Path): String =
	{
		val count = path.getNameCount
		val end = if (count > 0) count -1 else 0
		path.getName(end).toString()
	}

	def baseName(path: String): String = baseName(Paths.get(path))

	/**
	Return a absolute path for the given path
	*/
	def absPath(path: Path): String = path.toAbsolutePath.toString

	def absPath(path: String): String = absPath(Paths.get(path))

	/**
	Returns the path with the suffix removed
	*/
	def splitSuffix(path: Path): (String, String) = splitSuffix(path.toString)

	def splitSuffix(path: String): (String, String) =
	{
		val result = baseName(path).split("\\.", 2)
		return (join(parent(path), result(0)), result(1))
	}

	/**
	Joins two paths together
	*/
	def join(path1: String, path2: String): String = Paths.get(path1, path2).toString

	def join(path1: Path, path2: Path): String = join(path1.toString, path2.toString)

	/**
	Returns the parent path to the path
	*/
	def parent(path: String): String = parent(Paths.get(path))

	def parent(path: Path): String = path.getParent.toString

	/**
	Returns all the files in the given directory
	*/
	def ls(path:String): Iterable[String] = new File(path).listFiles.map(_.toString)
	
	def ls(path:Path): Iterable[String] = ls(path.toString)

	/*No idea why this won't work
	def join(paths: String*): String =
	{
		//println( Array(paths) )
		var tmp = Array(paths)
		other(paths:_*)
		Paths.get(paths.toList)
		return null;
		//Paths.get(Array(paths):_*).toString()
	}*/

	def main(args: Array[String])
	{
		println(baseName("/home/walker/test.txt.gz"))
		//println(join("/home", "walker", "new.txt"))
		println(absPath("."))
		println(splitSuffix("/home/walker/something.junk/test.text.gz"))
	}
}
