PROCPROC=./scripts/procproc.ml

validate:
	@for f in articles/*.md; do $(PROCPROC) $$f; done

update: summary-pc.csv
	@for f in articles/*.md; do $(PROCPROC) $$f --output $$f; done

authors:
	$(PROCPROC) articles/*.md --print-authors | sort | uniq

summary-pc.csv:
	./scripts/procproc.ml articles/*.md pc/*.md --pc-csv $@

summaries:
	-@rm -rf by_name/*
	$(PROCPROC) articles/*.md pc/*.md --summarize-by-name by_name

