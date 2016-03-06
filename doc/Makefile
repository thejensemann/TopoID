# ======================================================================
# MAKEFILE: LaTeX, feynMF, BibTeX, ...
# VERSION: 1.1
# CREATED: 2013-03-14--15
# MODIFIED: 2013-07-14
# AUTHOR: Jens Hoff
# MAIL: jens.hoff@kit.edu
# DESCRIPTION: Manage the various calls of LaTeX, feynMF, and BibTeX.
# COMMENTS:
#   * all LaTeX, feynMF, BibTeX output is redirected into the common
#     $(LOG)-file
#   * all feynMF output is stored in the directory $(FMF)
#   * after each LaTeX call all .mp-files are updated and should be
#     reprocessed
#   * common LaTeX calls for feynMF and BibTeX are handled
# TODO:
#   * make only portions of a .tex-file (?)
# ======================================================================


# +++ file setup +++++++++++++++++++++++++++++++++++++++++++++++++++++++

# .log-file
LOG := make.log

# feynMF directory
FMF := dias

# auxiliary file extensions
AES := aux bbl blg dvi log mp nav out snm toc

# main file (first .tex-file is chosen)
#TOP :=
TOP := $(firstword $(wildcard *.tex))
TOP := $(basename $(TOP))

# directory name
#DIR := <PARENT_DIRECTORY>
DIR := $(lastword $(subst /, ,$(CURDIR)))

# re-init .log-file on every make-call
#$(shell rm -f $(LOG))


# +++ program setup ++++++++++++++++++++++++++++++++++++++++++++++++++++

LATEX  := latex
DVIPS  := dvips
PSPDF  := ps2pdf
#PSPDF  := pstopdf
PSEPS  := ps2eps
#PSEPS  := pstoeps
MPOST  := mpost
BIBTEX := bibtex

# ignore all errors
LATEY  := yes s | $(LATEX)
MPOSU  := yes s | $(MPOST)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# === target setup =====================================================

# default
def: ps

# all
all: dia bbl pdf

# aliases
dias: dia | ps
bib: bbl
tgz: tar
view: gv

PHONIES := \
  def all help \
  dvi ps pdf eps \
  fmf dia dias \
  bib bbl \
  clean clear \
  tar tgz \
  view gv

SECONDS = $(LOG)

TARGETS := \
  %.dvi %.ps %.pdf %.eps $(TOP).dvi $(TOP).ps $(TOP).pdf \
  $(FMF)/%.mp \
  %.bbl $(TOP).bbl \
  $(DIR).tgz $(DIR).tar.gz

.PHONY: $(PHONIES)
.SECONDARY: $(SECONDS)
.SILENT: $(PHONIES) $(SECONDS) $(TARGETS)

# ======================================================================


# --- help -------------------------------------------------------------

help:
	echo "Possible commands and targets are:"
	echo "  * $(PHONIES)"
	echo "  * $(TARGETS)"


# --- LaTeX processing -------------------------------------------------

# check if feynMF is used at all
cproc = \
  grep 'fmffile' $(1) > /dev/zero && \
  $(call fproc,$(FMF))

# process one LaTeX step: $(1): step, $(2): command, #(3): file
lproc = \
  echo "Calling '$(1)' on '$(3)'..." | tee -a $(LOG); \
  printf '=%.0s' {1..80} >> $(LOG); printf "\n" >> $(LOG); \
  $(2) $(3) >> $(LOG) 2>&1; \
  printf "\n\n" >> $(LOG)

# usual LaTeX command chain

dvi: $(TOP).dvi
%.dvi: %.tex
	$(call cproc,$<)
	$(call lproc,$(LATEX),$(LATEY),$<)

ps: $(TOP).ps
%.ps: %.dvi
	$(call lproc,$(DVIPS),$(DVIPS),$<)

pdf: $(TOP).pdf
%.pdf: %.ps
	$(call lproc,$(PSPDF),$(PSPDF),$<)

eps: $(TOP).eps
%.eps: %.ps
	$(call lproc,$(PSEPS),$(PSEPS),$<)


# --- feynMF and BibTeX interplay --------------------------------------

# get dia, bbl targets (for numerous LaTeX calls)
TMP := $(MAKECMDGOALS)
TMP := $(subst dias,dia,$(TMP))
TMP := $(subst bib,bbl,$(TMP))
TMP := $(subst all,bbl,$(TMP))
TMP := $(filter dia bbl,$(TMP))

# last relevant target (executes the calls)
LST := $(lastword $(TMP))

# check for BibTeX demand
BTD := $(findstring bbl,$(TMP))


# --- feynMF processing ------------------------------------------------

# check for and create feynMF directory (touch .tex-file on creation)

fproc = \
  [ ! -e $(1) ] \
    && mkdir -p $(1) \
    && echo "Created directory \"$(1)\"." \
    || exit 0
  [ ! -d $(1) ] \
    && echo "Error: \"$(1)\" not a directory!" \
    && exit 1 \
    || exit 0

fmf:
	$(call fproc,$(FMF))


# trigger generation of .mp-files
$(FMF)/%.mp: $(TOP).dvi
	$() # nothing to do


# process one MetaPost call: $(1): file, $(2): .log
mproc = \
  echo "Calling '$(MPOST)' on '$(1)'..." | tee -a $(2); \
  printf '+%.0s' {1..80} >> $(2); printf "\n" >> $(2); \
  $(MPOSU) $(1) >> $(2) 2>&1; \
  printf "\n" >> $(2)

# process loop over .mp-files: $(1): directory, $(2): .log
nproc = \
  $(foreach F,$(notdir $(wildcard $(1)/*.mp)),\
    cd $(1) && $(call mproc,$(F),../$(2)))

# produce all the diagram pictures (make version)
dia: $(FMF)/%.mp
	$(call nproc,$(FMF),$(LOG))
	printf "\n" >> $(LOG)
ifeq ($(LST),dia)
	$(call lproc,$(LATEX)-1,$(LATEY),$(TOP).tex)
ifneq ($(BTD),)
	$(call lproc,$(LATEX)-2,$(LATEY),$(TOP).tex)
endif
else
	touch $(TOP).dvi
endif


# --- BibTeX processing ------------------------------------------------

bbl: $(TOP).bbl
%.bbl: %.dvi
	$(call lproc,$(BIBTEX),$(BIBTEX),$(basename $<))
ifeq ($(LST),bbl)
	$(call lproc,$(LATEX)-1,$(LATEY),$<)
	$(call lproc,$(LATEX)-2,$(LATEY),$<)
endif


# --- cleaning & clearing ----------------------------------------------

# remove auxiliary files (recursive): *.$(AES)
rauxs = \
  for AE in $(AES); \
  do find "." -name "*.$$AE" -delete; \
  done

# clean: remove auxiliary files only
clean:
	echo "Cleaning directory..."
	$(call rauxs)
	rm -f $(SECONDS)

# clear: remove all generated files
clear:
	echo "Clearing directory..."
	$(call rauxs)
	rm -fr $(SECONDS) $(TARGETS) $(FMF)


# --- archive ----------------------------------------------------------

tar: $(DIR).tar.gz
$(DIR).tgz $(DIR).tar.gz: clean
	echo "Creating archive..."
	tar -cz -f ../$@ .
# tar -cz -f ../$@ --exclude Makefile .


# --- view -------------------------------------------------------------

gv: $(TOP).ps
	gv -watch -scale=-1000 $< &


# ----------------------------------------------------------------------
