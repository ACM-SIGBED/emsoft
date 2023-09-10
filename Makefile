PROCPROC=./scripts/procproc.ml

validate:
	@for f in articles/*.md; do $(PROCPROC) $$f; done

rewrite:
	# should give same result
	@for f in articles/*.md; do $(PROCPROC) $$f --output $$f; done

update: summaries summary-pc.csv summary-author.csv

authors:
	$(PROCPROC) articles/*.md --print-authors | sort | uniq

summary-pc.csv: FORCE
	./scripts/procproc.ml articles/*.md pc/*.md --pc-csv $@

summary-author.csv: FORCE
	./scripts/procproc.ml articles/*.md pc/*.md --author-csv $@

summaries:
	-@rm -rf by_name/*
	$(PROCPROC) articles/*.md pc/*.md --summarize-by-name by_name

www:
	-@rm -rf ./www
	@mkdir -p ./www
	@cp style.css ./www/
	$(PROCPROC) articles/*.md pc/*.md --html ./www

.PHONY: FORCE validate update rewrite authors summaries www

