##
## formatBibtex: Format BibTeX Entries
## Copyright (C) 2021-2025 Wenjie Wang <wang@wwenjie.org>
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

## set default options through functions
## reference: knitr::opts_chunk(), etc.
##' @importFrom stats setNames
.set_defaults <- function(defaults = list())
{
    x <- defaults
    ## getter
    get <- function(name = NULL, default = FALSE, drop = TRUE)
    {
        out <- if (default) {
                   defaults
               } else {
                   x
               }
        if (is.null(name)) {
            return(out)
        }
        if (drop && length(name) == 1) {
            out[[name]]
        } else {
            stats::setNames(out[name], name)
        }
    }
    ## setter
    set <- function(name, value)
    {
        x[[name]] <<- value
        invisible(x)
    }
    ## append
    append <- function(name, value)
    {
        x[[name]] <<- unique(c(x[[name]], value))
        invisible(x)
    }
    list(
        get = get,
        set = set,
        append = append
    )
}


##' Package Options
##'
##' A list of functions for getting, setting, and appending options for
##' formatting.  The available options are \code{"lowercase_words"},
##' \code{"punctuation"}, and \code{"protected_words"}.  The default values can
##' be retrieved by \code{formatBibtex_defaults$get()}.
##'
##' @return A list of functions.
##' @export
format_options <- .set_defaults(list(
    lowercase_words = c(
        ## common prepositions not to be capitalized (four or fewer letters)
        "at", "by", "down", "for", "from", "in", "into", "like", "near", "of",
        "off", "on", "onto", "over", "past", "to", "upon", "with",
        ## common conjunctions not to be capitalized (four or fewer letters)
        "and", "as", "but", "for", "if", "nor", "once", "or", "so", "than",
        "that", "till", "when", "yet",
        ## referring to numbers
        "a", "an", "the"
    ),
    punctuation = c(
        "`|'|/|\\.|\\?|!|,|;|:|<|>|\\(|\\)|\\[|\\]|\\{|\\}|[ ]"
    ),
    protected_words = c(
        "ACM", "AIC", "arXiv", "ArXiv", "AUC",
        "Bayesian", "Bernoulli",
        "Bernstein", "BERT", "BFGS", "BIC", "bioRxiv", "BioRxiv", "BMC",
        "BMC", "BMJ",
        "C++", "Carlo", "CNN", "CoRR", "Cox", "Cox's",
        "DNA", "Doxygen",
        "e-prints", "ECM", "EM", "Emacs",
        "Fortran",
        "Fourier",
        "Gauss", "Gaussian", "Gibbs",
        "IEEE", "IFAC", "IJDWM",
        "JAMA", "JDS", "Julia",
        "L-BFGS-B", "Luce",
        "Markov", "Markovian", "MATLAB", "MCMC", "MLE", "Monte",
        "NCHS", "NRL", "NURBS",
        "PAC", "Plackett", "PLOS", "Poisson", "Python",
        "R", "RNA", "ROC",
        "SAGA", "SAS", "SIAM", "siRNA", "SiRNA", "siRNAs", "SMEM", "SORT",
        "StatsRef",
        "TOG", "TOMACS", "TOMS", "Tweedie",
        "USENIX"
    )
))
