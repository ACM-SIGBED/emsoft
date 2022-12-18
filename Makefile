PROCPROC=./scripts/procproc.ml

validate:
	@for f in articles/*.md; do $(PROCPROC) $$f; done

rewrite:
	@for f in articles/*.md; do $(PROCPROC) $$f --output $$f; done

