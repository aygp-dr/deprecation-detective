.PHONY: run test lint fmt check clean help scan-json scan-high

help: ## Show this help
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-20s\033[0m %s\n", $$1, $$2}'

run: ## Run the scanner on current directory
	bb run --dir . --format text

test: ## Run all tests (JVM + babashka)
	bb test && bb test:bb

lint: ## Lint with clj-kondo
	bb lint

fmt: ## Check formatting (bb fmt:fix to repair)
	bb fmt

check: ## lint + fmt + test (what CI runs)
	bb check

clean: ## Clean generated files
	rm -rf .cpcache target .clj-kondo/.cache

scan-json: ## Scan current dir, JSON output
	bb run --dir . --format json

scan-high: ## Scan current dir, high severity only
	bb run --dir . --severity high
