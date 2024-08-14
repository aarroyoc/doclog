.PHONY: gen-docs-scryer
gen-docs-scryer:
	bash doclog.sh ./scryer-prolog ./output

.PHONY: clean
clean:
	rm -rf output

.PHONY: upload
upload:
	netlify deploy --prod

.PHONY: setup
setup:
	rm -rf teruel
	git clone --depth 1 --branch v1.0.1 https://github.com/aarroyoc/teruel
	rm -rf djota
	git clone --depth 1 --branch v0.3.3 https://github.com/aarroyoc/djota
	rm -rf scryer-prolog
	git clone --depth 1 https://github.com/mthom/scryer-prolog
