This is a repository for experimenting with the `LinearTypes` extension to GHC, also called *Linear Haskell.* It functions as both a Haskell library and a LaTex document (the source modules are written in Literate Haskell). 

To build the document, run:

```
latexmk -pdf document.tex
```

To build the library, run:

```
stack build
```
