.PHONY: gen-docs-scryer
gen-docs-scryer:
	bash doclog.sh scryer.config.pl

.PHONY: clean
clean:
	rm -rf output

.PHONY: upload
upload:
	netlify deploy --prod

.PHONY: setup
setup:
	git clone --depth 1 --branch v1.0.1 https://github.com/aarroyoc/teruel
	git clone --depth 1 --branch 1.0.0 https://github.com/aarroyoc/marquete
