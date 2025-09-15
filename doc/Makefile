# Makefile for building CubedOS documentation using Docker

# Variables
IMAGE_NAME := cubedos-doc
OUTPUT_DIR := output
PDF_NAME := CubedOS.pdf
RUNTIME := podman # Change to 'docker' if you prefer Docker

# Default target
.PHONY: all
all: $(OUTPUT_DIR)/$(PDF_NAME)

# Build the PDF using Docker
$(OUTPUT_DIR)/$(PDF_NAME): Dockerfile CubedOS.tex
	@echo "Building CubedOS documentation..."
	@mkdir -p $(OUTPUT_DIR)
	$(RUNTIME) build -t $(IMAGE_NAME) .
	$(RUNTIME) run --rm -v $(PWD)/$(OUTPUT_DIR):/output $(IMAGE_NAME)
	@echo "Documentation built successfully: $(OUTPUT_DIR)/$(PDF_NAME)"

# Clean target
.PHONY: clean
clean:
	@echo "Cleaning output directory..."
	rm -rf $(OUTPUT_DIR)

# Force rebuild
.PHONY: rebuild
rebuild: clean all

# Help target
.PHONY: help
help:
	@echo "Available targets:"
	@echo "  all     - Build CubedOS.pdf (default)"
	@echo "  clean   - Remove output directory"
	@echo "  rebuild - Clean and rebuild"
	@echo "  help    - Show this help message"
