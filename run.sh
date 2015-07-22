#! /bin/sh

#script to run any of the mains in sml
scala -classpath `cat classpath.txt` $@
