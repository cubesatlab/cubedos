# Use TeXLive Docker image
FROM registry.gitlab.com/islandoftex/images/texlive:latest

# Set working directory
WORKDIR /doc

# Copy source files
COPY . .

# Build the documentation using pdflatex and bibtex
RUN pdflatex CubedOS.tex && \
    bibtex CubedOS && \
    pdflatex CubedOS.tex && \
    pdflatex CubedOS.tex

# Create output directory and copy the generated PDF
RUN mkdir -p /output && cp CubedOS.pdf /output/

# Default command to copy output to mounted volume
CMD ["sh", "-c", "cp CubedOS.pdf /output/ 2>/dev/null || echo 'PDF copied to /output/CubedOS.pdf'"]
