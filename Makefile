Main-opt: Main.hs termops.o Grammar/Par.hs
	ghc -O3 Main.hs -g termops.o -o Main-opt

Main: Main.hs termops.o Grammar/Par.hs
	ghc Main.hs -g termops.o -o Main

Grammar/Abs.hs: Grammar/Par.hs

Grammar/Par.hs: BnfcParser/grammar.cf
	cd BnfcParser && bnfc -d -m grammar.cf && make -j
	-rm Grammar -rf
	mv BnfcParser/Grammar .

termops.o: FFI/termops.c
	cc -O3 -c $^ -o $@

.PHONY: clean
clean:
	-rm *.hi
	-rm *.o
	-rm FFI/*.hi
	-rm FFI/*.o
	-rm Main
	-rm Main-opt
	-rm BnfcParser/Grammar -rf
	-rm Grammar -rf
