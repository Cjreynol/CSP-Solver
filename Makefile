

main:
	ghc src/Main.hs -isrc -outputdir bin -o solver

clean:
	rm solver; cd bin; rm *.hi *.o
