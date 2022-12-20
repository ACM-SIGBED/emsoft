PROCPROC=./scripts/procproc.ml

validate:
	@for f in articles/*.md; do $(PROCPROC) $$f; done

rewrite:
	@for f in articles/*.md; do $(PROCPROC) $$f --output $$f; done

authors:
	$(PROCPROC) articles/*.md --print-authors | sort | uniq

summaries:
	-@rm -rf names/*
	$(PROCPROC) articles/*.md --summarize-by-name names

