############
# Makefile for HousePrices
# Authors: Kevin Thompson, Brandon Croom, and Sterling Beason
# Last updated August 6, 2019
############

# Directory and mergedata outfile variables
DATADIR = .
MERGE_OUT = mergedata.Rout

# list variable RFILES for R gather and merge files
RFILES = $(wildcard $(DATADIR)/*.R)

# Sets OUT_FILES to a.Rout, b.Rout, etc, where a,b, etc correspond to items in the list RFILES
OUT_FILES = $(RFILES:.R=.Rout)

# default target
all: $(OUT_FILES)

# .Rout files depend on .R files. Whenever a .R file changes, make will run that file and produce new corresponding .Rout
$(DATADIR)/%.Rout: $(DATADIR)/%.R
	R CMD BATCH $<  # The pre-requisite for target .Rout files is that they are run

# 'make clean' removes out files in OUT_FILES
clean:
	rm -fv $(OUT_FILES)

# 'make cleanMerge' removes mergedata.Rout
cleanMerge:
	rm -fv $(MERGE_OUT)
