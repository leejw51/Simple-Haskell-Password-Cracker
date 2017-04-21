HC=ghc -O2 $^ -threaded -rtsopts -eventlog

test: crackpass
	./test.sh

crackpass: crackpass.hs
	$(HC)

clean:
	rm *.o || true
	rm *.hi || true
	rm crackpass || true
	rm crackpass.eventlog || true
