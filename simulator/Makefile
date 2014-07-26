SOURCES = \
	stringUtil.ml \
	lambdaman.ml lambdamanParser.mly lambdamanLexer.mll lambdamanReader.ml \
	ghost.ml ghostAiParser.mly ghostAiLexer.mll ghostAiReader.ml \
	world.ml mapReader.ml \
	simulator.ml
RESULT  = simulator

OCAMLFLAGS = -g
LDFLAGS = -g
OCAMLLDFLAGS = -g
all: byte-code

# all: native-code

OCAMLMAKEFILE = OCamlMakefile
include $(OCAMLMAKEFILE)
