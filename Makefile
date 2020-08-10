SRC= ./src/*.hs
CMP= ghc
FLG= -dynamic -odir ./bin -hidir ./bin -o
OUT= ./bin/emilian

.phony: clean

all: $(SRC)
	$(CMP) $(FLG) $(OUT) $(SRC)

run: $(OUT)
	$(OUT)

clean:
	-rm ./bin/*
