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
	git clone --depth 1 --branch v1.0.2 https://github.com/aarroyoc/marquete

.PHONY: test
test:
	bash doclog.sh scryer-test.config.pl
