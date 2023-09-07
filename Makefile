Main: main.hs termops.o
	ghc main.hs -g termops.o -o Main

termops.o: FFI/termops.c
	cc -O3 -c $^ -o $@

.PHONY: clean
clean:
	-rm *.hi
	-rm *.o
	-rm FFI/*.hi
	-rm FFI/*.o
	-rm main
