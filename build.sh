#! /bin/sh
dir=target

mkdir $dir

scalac -classpath ~/lib/java/grph-1.5.29-big.jar -d $dir *.scala
