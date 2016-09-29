TARGET = bin/tony
BINARIES = lex parse tony

HC = ghc
AX = alex
HP = happy

INTERMEDIATEDIR = intermediateFiles

SRCDIRS = src/:src/frontend:src/backend:src/Mark:src/optimise
HFLAGS = -outputdir $(INTERMEDIATEDIR) -i$(SRCDIRS)
AFLAGS = 
HAPPYFILES = src/frontend/Parser.hs
ALEXFILES = src/frontend/Lexer.hs
TESTEXE = bin/Tests

LPAREN :=(
RPAREN :=)

DEPS = src/frontend/Token.hs src/Grammar.hs src/backend/Asm.hs
FRONTENDFILES = src/frontend/Lexer.hs src/frontend/Parser.hs src/frontend/Analyser.hs 
BACKENDFILES = src/backend/AllocateRegisters.hs src/backend/Translate.hs src/backend/TransExpr.hs src/backend/Util.hs src/backend/TransAssign.hs src/backend/TransStat.hs src/backend/BuildFooter.hs
MARKFILES = src/Mark/MarkEval.hs src/Mark/MarkLex.hs
OPTIMISEFILES = src/optimise/Optimise.hs src/optimise/Peephole.hs

TOCOMPILE = src/Main.hs $(FRONTENDFILES) $(BACKENDFILES) $(MARKFILES) $(OPTIMISEFILES) $(DEPS)

.PHONY: all, clean, getloc

all: $(TARGET)
	$(info )
	$(info )
	$(info .    .-" +' "-.    __,  ,___,           )
	$(info .   /.'.'A_'*`.\  $(LPAREN)--|__| _,,_ ,_  )
	$(info .  |:.*'/\-\. ':|   _|  |(_||_)|_$(RPAREN)\/    )
	$(info .  |:.'.||"|.'*:|  $(LPAREN)        |  | _/     )
	$(info .   \:~^~^~^~^:/          __,  ,___,    )
	$(info .    /`-....-'\          $(LPAREN)--|__| _ |' _| _,   , )
	$(info .   /          \           _|  |(_)||$(LPAREN)_|(_|\//_) )
	$(info .   `-.,____,.-'          $(LPAREN)               _/ )
	$(info )
	$(info )
	$(info *******************************************************************)
	$(info ***** The parser binary is tony. Usage ./bin/tony [file.wacc] *****)
	$(info *******************************************************************)
	$(info ************ Compiler Usage: ./compile [file.wacc] ****************)
	$(info *******************************************************************)
	$(info )
	$(info )

bin/tony: $(TOCOMPILE) bin
	$(HC) $(HFLAGS) --make $(TOCOMPILE) -o bin/tony


test: bin/tony testParser.rb
	./testParser.rb -v examples

getloc:
	find . -name '*.hs' -o -name '*.x' -o -name '*.y' | xargs wc -l

lex: src/Lexer.hs
	$(HC) $(HFLAGS) --make src/Lexer.hs $(DEPS) -o bin/lex

src/frontend/Lexer.hs: src/frontend/lexer.x
	$(AX) src/frontend/lexer.x -o src/frontend/Lexer.hs

src/frontend/Parser.hs: src/frontend/parser.y
	$(HP) src/frontend/parser.y -o src/frontend/Parser.hs

bin:
	mkdir bin

clean:
	rm -rf $(BINARIES) $(ALEXFILES) $(HAPPYFILES) $(TESTEXE) $(INTERMEDIATEDIR)
