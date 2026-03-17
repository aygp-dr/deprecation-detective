.PHONY: run test lint clean help scan-json scan-high

help: ## Show this help
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-20s\033[0m %s\n", $$1, $$2}'

run: ## Run the scanner on current directory
	bb run --dir . --format text

test: ## Run all tests
	bb -cp src:test -m deprecation-detective.core-test

lint: ## Check for style issues
	bb -e '(println "Lint OK - bb scripts are self-contained")'

clean: ## Clean generated files
	rm -rf .cpcache target

scan-json: ## Scan current dir, JSON output
	bb run --dir . --format json

scan-high: ## Scan current dir, high severity only
	bb run --dir . --severity high
