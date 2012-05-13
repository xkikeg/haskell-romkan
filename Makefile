HC = ghc
HCFLAGS = --make -O2

SRC = $(wildcard *.hs)
TGT = test

.PHONY: all clean

all: $(TGT)

$(TGT): $(TGT).hs $(SRC)
	$(HC) $(HCFLAGS) -o $@ $<

clean:
	$(RM) test *.o *.hi
