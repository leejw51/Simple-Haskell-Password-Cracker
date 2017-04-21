#! /bin/bash

echo "Enter a test password (Note: on my machine (2.1 GHz with 4 cores) it had trouble with just a five character password): "
read PASSWORD

NUMCORES=$(cat /proc/cpuinfo | grep 'processor[[:space:]]*:.*' | wc -l)
SHA=$(echo $PASSWORD | tr -d '\n' | sha1sum) 

GUESS=$(time ./crackpass $SHA +RTS -N$NUMCORES -l)

echo "Input password: $PASSWORD"
echo "Guessed password: $GUESS"

if [ $GUESS == $PASSWORD ] ;
then
	echo "Program succeeded."
else
	echo "Program failed."
fi
