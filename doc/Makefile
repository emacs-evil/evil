.PHONY: clean

%.info: %.texi
	makeinfo --no-split $<

%.pdf: %.texi
	texi2pdf $<

clean:
	rm -f *.aux *.cp *.fn *.fns *.info *.ky *.log *.pg *.toc *.tp *.vr *.vrs
