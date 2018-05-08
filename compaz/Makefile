EXE = PazLexerTest PazParserTest Paz

paz: Paz.hs PazLexer.hs PazParser.hs PazPrinter.hs Codegen.hs Symbol.hs
	ghc -dynamic Paz.hs

test:
	python3 test.py

clean:
	rm -f *.hi *.o $(EXE)