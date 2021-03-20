library(formatBibtex)

## example BibTeX file that needs formatting
example_bib <- system.file("examples/example.bib", package = "formatBibtex")
print(readLines(example_bib), quote = FALSE)

## needs the package bibtex
has_bibtex <- requireNamespace("bibtex", quietly = TRUE)

## example of format_bibtex_entry()
if (has_bibtex) {
    bib <- bibtex::read.bib(example_bib)
    ## check the default words that need protection by curly braces
    (default_words <- getOption("formatBibtex.protected_words"))
    format_bibtex_entry(bib, protected_words = c(default_words, "SMEM"))
}

## example of format_bibtex_file()
if (has_bibtex) {
    output_file <- tempfile(fileext = ".bib")
    format_bibtex_file(example_bib,
                       output_file = output_file,
                       protected_words = c(default_words, "SMEM"))
    print(readLines(output_file), quote = FALSE)
}
