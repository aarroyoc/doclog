.PHONY: gen-docs-scryer
gen-docs-scryer:
	bash doclog.sh scryer.config.pl

.PHONY: clean
clean:
	rm -rf output

.PHONY: upload
upload:
	netlify deploy --prod
