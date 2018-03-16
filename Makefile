main:
	ghc src/Main.hs -Wall -isrc -outputdir bin -o solver

interactive:
	ghci -isrc Main

documentation:
	haddock -o docs -h src/*.hs src/Sudoku/*.hs

clean:
	rm solver; rm -rf bin/Sudoku bin/*.hi bin/*.o; 

cleanDocs:
	rm -f docs/*

nuke:
	make clean; make cleanDocs;

all:
	make main; make documentation;

