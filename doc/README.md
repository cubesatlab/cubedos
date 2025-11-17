
# README

This folder contains the official documentation set for CubedOS. The master document is
CubedOS.tex. That document includes, directly or indirectly, all the other document components.

## Building the Documentation

### Docker/Podman Method (Recommended)

The easiest way to build the documentation is using the provided Makefile and Docker image:

```bash
make
```

This will build the PDF using a containerized LaTeX environment and place the result in `output/CubedOS.pdf`. No local LaTeX installation is required.

Additional make targets:

- `make clean` - Remove the output directory
- `make rebuild` - Clean and rebuild from scratch
- `make help` - Show available targets

By default, the Makefile uses Podman. To use Docker instead, edit the `RUNTIME` variable in the Makefile or run:

```bash
make RUNTIME=docker
```

### Manual LaTeX Method

If you prefer to build locally, first ensure you have a LaTeX system installed. The precise way to
do this depends on your system and is outside the scope of this document. Then, issue the
following commands:

```bash
pdflatex CubedOS
bibtex CubedOS
pdflatex CubedOS
pdflatex CubedOS
```

It is necessary to run the `pdflatex` command multiple times to ensure that all cross references
are resolved properly. The resulting documentation will be in `CubedOS.pdf`.

## LaTeX Resources

For a quick primer on LaTeX see the [LaTeX at
VTSU](https://www.pchapin.org/VTSU/LaTeX/LaTeX.zip) document. The [Not So Short Introduction to
LaTeX2e](http://tobi.oetiker.ch/lshort/lshort.pdf) is also an excellent resource.

We recommend using [JabRef](http://jabref.sourceforge.net/) for managing BibTeX databases. Note
that a text editor is technically all that is necessary but JabRef provides many useful services
most text editors do not.
