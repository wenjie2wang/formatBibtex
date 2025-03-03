library(formatBibtex)

## simple examples
foo <- c("iT IS_A_sIMPLe_ExamplE.", "let'S_do soMe_tesTs!")
format_string(foo, style = "title")
format_string(foo, style = "sentence")

## default words that would be protected from formatting
format_options$get("protected_words")

## e.g., protect ABCD and MCMC from being converted to lowercase
format_options$append("protected_words", c("ABCD", "MCMC"))

bar <- c("on_the_convergence properties of the ABCD_algorithm",
         "teSt: tHe cluster is Running MCMC!")
format_string(bar, style = "sentence")

## more tricky examples: protected words contain `str4split`
foo <- c("nineteenth- and twentieth-century writers",
         "well-differentiated cells with arXiv e-prints")
format_string(foo, str4split = "-| ",
              protected_words = c("arXiv", "e-prints"))

## trivial examples
format_string(NULL)
format_string(character(0))
format_string(character(3))
format_string(c(NA, "", "hello world!"))
