# The Ergonomics of Faceted Execution
My undergraduate thesis in computer science at Haverford College, April 2019.

## Abstract
This thesis reviews several challenges in the use and implementation of faceted execution, a programming-language mechanism for enforcing privacy policies. I present a proof-of-concept of a module-rewriting technique for correctly handling mutable state in [Racets], an existing implementation of faceted execution in Racket, and I discuss how static typing and abstract interpretation could improve the safety and efficiency of faceted code.

## Organization of this repository
The final PDF draft of my thesis is `thesis.pdf` at the top-level. The `tex` directory contains the LaTeX and BibTeX files for my thesis. The `code` directory contains various files of Racket code. `code/lang-as-lib/racets.rkt` is the most important code file: it holds the implementation of the proof-of-concept mentioned in the abstract.

[Racets]: https://github.com/fordsec/racets
