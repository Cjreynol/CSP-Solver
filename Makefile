main:
	ghc src/Main.hs -XFlexibleInstances -XMultiParamTypeClasses -XFunctionalDependencies -XTypeSynonymInstances -isrc -outputdir bin -o solver

interactive:
	ghci -XFlexibleInstances -XMultiParamTypeClasses -XFunctionalDependencies -XTypeSynonymInstances -isrc Main

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

