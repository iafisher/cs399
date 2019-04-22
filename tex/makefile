BUILD=vimtex_build
PDFDIR=pdf
LATEX=pdflatex
BIBTEX=bibtex

# Make sure that the build and PDF directories exist.
$(shell mkdir -p $(BUILD) >/dev/null)
$(shell mkdir -p $(PDFDIR) >/dev/null)


all: $(PDFDIR)/research_proposal.pdf $(PDFDIR)/thesis.pdf


$(PDFDIR)/research_proposal.pdf: research_proposal.tex thesis.bib
	$(LATEX) -output-directory=$(BUILD) research_proposal.tex
	$(BIBTEX) $(BUILD)/research_proposal.aux
	# Compile twice so references work.
	$(LATEX) -output-directory=$(BUILD) research_proposal.tex
	$(LATEX) -output-directory=$(BUILD) research_proposal.tex
	cp $(BUILD)/research_proposal.pdf $(PDFDIR)


$(PDFDIR)/thesis.pdf: thesis.tex thesis.bib
	$(LATEX) -output-directory=$(BUILD) thesis.tex
	$(BIBTEX) $(BUILD)/thesis.aux
	# Compile twice so references work.
	$(LATEX) -output-directory=$(BUILD) thesis.tex
	$(LATEX) -output-directory=$(BUILD) thesis.tex
	cp $(BUILD)/thesis.pdf $(PDFDIR)


clean:
	rm -f $(BUILD)/* $(PDFDIR)/*.pdf

.PHONY: clean
