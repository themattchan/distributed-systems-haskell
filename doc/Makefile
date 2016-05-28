new:
	lhs2TeX -o report.tex ../src/Cloud/Misc.lhs;
	latexmk -pdf report.tex

clean:
	for i in log aux ps dvi bbl blg log lof lot toc out pyg glg glo gls ist; \
		do rm -f *.$$i; done
