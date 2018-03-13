main:
	ghc src/Main.hs -isrc -outputdir bin -o solver

documentation:
	haddock -o docs -h src/*.hs

clean:
	rm solver; rm bin/*.hi bin/*.o; 

cleanDocs:
	rm -f docs/*

nuke:
	make clean; make cleanDocs;

all:
	make main; make documentation;

