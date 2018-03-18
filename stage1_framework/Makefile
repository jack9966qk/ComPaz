EXE = PazLexerTest PazParserTest Paz

paz: ./main.hs
	ghc ./main.hs -o ./Paz

lexer_test: ./PazLexerTest.hs
	ghc ./PazLexerTest.hs

parser_test: ./PazParserTest.hs
	ghc ./PazParserTest.hs

./main.hs: ./PazLexer.hs ./PazParser.hs ./PazPrinter.hs
./PazLexerTest.hs: ./PazLexer.hs
./PazParserTest.hs: ./PazLexer.hs ./PazParser.hs
./PazPrinter.hs: ./PazLexer.hs ./PazParser.hs

clean:
	rm -f *.hi *.o $(EXE)