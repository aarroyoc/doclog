.PHONY: gen-docs
gen-docs:
	/home/aarroyoc/dev/scryer-prolog/target/release/scryer-prolog -g 'run("/home/aarroyoc/dev/scryer-prolog/src/lib", "output"),halt' main.pl

.PHONY: clean
clean:
	rm -rf output

.PHONY: upload
upload:
	netlify deploy --prod
