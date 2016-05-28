pdf:
	@(cd doc; \
	lhs2TeX -o report.tex ../src/Cloud/Misc.lhs; \
	latexmk -pdf report.tex)

clean:
	rm -f doc/*
