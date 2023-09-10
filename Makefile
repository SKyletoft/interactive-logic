Main-opt: main.hs termops.o Grammar/Par.hs
	ghc -O3 main.hs -g termops.o -o Main-opt

Main: main.hs termops.o Grammar/Par.hs
	ghc main.hs -g termops.o -o Main

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
	-rm main
	-rm BnfcParser/Grammar -rf
	-rm Grammar -rf
