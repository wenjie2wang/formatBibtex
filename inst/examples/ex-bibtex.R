library(formatBibtex)

## example BibTeX file that needs formatting
example_bib <- system.file("examples/example.bib", package = "formatBibtex")
print(readLines(example_bib), quote = FALSE)

## check the default words that need protection by curly braces
format_options$get("protected_words")
## append "SMEM" to the list
format_options$append("protected_words", c("SMEM"))

## needs the package bibtex
if (requireNamespace("bibtex", quietly = TRUE)) {
    ## example of format_bibtex_entry()
    bib <- bibtex::read.bib(example_bib)
    format_bibtex_entry(bib)
    ## example of format_bibtex_file()
    output_file <- tempfile(fileext = ".bib")
    format_bibtex_file(example_bib, output_file = output_file)
    print(readLines(output_file), quote = FALSE)
}
