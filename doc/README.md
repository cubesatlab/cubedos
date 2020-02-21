
README
======

This folder contains the official documentation set for CubedOS. The master document is
CubedOS.tex. That document includes, directly or indirectly, all the other document components.

To build the documentation, first be sure you have a LaTeX system installed. The precise way to
do this depends on your system and is outside the scope of this document. Next, be sure you have
checked out the Papers project from the CubeSat Laboratory GitHub. See:

    https://github.com/cubesatlab/papers
    
This repository must be checked out as a sibling in your file system of the CubedOS repository.
This allows BibTeX to locate the references mentioned in this documentation. If you don't have
the Papers repository available, the documentation will still build, but the citations will be
broken and there will not be a reference list at the end of the document.

Finally, issue the following commands:

    > pdflatex CubedOS
    > bibtex CubedOS
    > pdflatex CubedOS
    > pdflatex CubedOS
    
It is necessary to run the `pdflatex` command multiple times to ensure that all crossreferences
are resolved properly. The resulting documentation will be in `CubedOS.pdf`.
