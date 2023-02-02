##
## formatBibtex: Format BibTeX Entries
## Copyright (C) 2021-2023 Wenjie Wang <wang@wwenjie.org>
##
## This file is part of the R package formatBibtex.
##
## The R package formatBibtex is free software: You can redistribute it and/or
## modify it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or any later
## version (at your option). See the GNU General Public License at
## <https://www.gnu.org/licenses/> for details.
##
## The R package formatBibtex is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
##

## set default options
formatBibtex_default_options <- list(
    ## TODO: add more reserved words?
    formatBibtex.lowercase_words = c(
        ## common prepositions not to be capitalized (four or fewer letters)
        "at", "by", "down", "for", "from", "in", "into", "like", "near", "of",
        "off", "on", "onto", "over", "past", "to", "upon", "with",
        ## common conjunctions not to be capitalized (four or fewer letters)
        "and", "as", "but", "for", "if", "nor", "once", "or", "so", "than",
        "that", "till", "when", "yet",
        ## referring to numbers
        "a", "an", "the"
    ),
    formatBibtex.protected_words = c(
        "ACM", "AIC", "ArXiv", "arXiv", "AUC",
        "Bayesian", "Bernstein", "BIC", "BMC", "BMJ", "BFGS", "BioRxiv",
        "C++", "Carlo", "Cox", "Cox's",
        "Doxygen",
        "ECM",  "EM", "Emacs", "e-prints",
        "Fortran",
        "Gibbs", "Gaussian", "Gauss",
        "IEEE",
        "Julia",
        "L-BFGS-B",
        "Markov", "MATLAB",  "MCMC", "MLE", "Monte",
        "NCHS",
        "Python",
        "R",
        "SAGA", "SAS", "SIAM"
    ),
    formatBibtex.punctuation = c(
        "`|'|/|\\.|\\?|!|,|;|:|<|>|\\(|\\)|\\[|\\]|\\{|\\}|[ ]"
    )
)


## set options for formatBibtex
.onLoad <- function(libname, pkgname) {
  op <- options()

  toset <- ! names(formatBibtex_default_options) %in% names(op)
  if (any(toset))
      options(formatBibtex_default_options[toset])

  invisible()
}
