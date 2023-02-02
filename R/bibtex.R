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

##' Format BibTeX Entries in An Opinionated Way
##'
##' @description
##' This function tries to format the given BibTeX entries so that
##' \itemize{
##'
##' \item The \dQuote{title} field is written in the title case with
##'    exceptations such as common prepositions with four or fewer letters.
##'    Special words such as \dQuote{Bayesian} and \dQuote{Markov} are protected
##'    by curly braces from case changing by BibTeX sytle.
##'
##' \item The \dQuote{author} field follows a consistent fashion: \dQuote{family
##'    name, given name}.  A period will be added to single letter acronym.
##'
##' \item The \dQuote{journal} field (if any) is written in the title case.
##'
##' \item The \dQuote{pages} field should use \dQuote{--} instead of \dQuote{-}
##'   between pages.
##'
##' }
##'
##' @details
##'
##' When \code{emacs} is available in the system, the function
##' \code{format_bibtex_file()} will perform additional formatting with the help
##' of the commands \code{bibtex-reformat} and \code{bibtex-sort-buffer}.
##'
##' @name format_bibtex_entry
##'
##' @param entry A \code{bibentry} object (created by \code{utils::bibentry})
##'     representing BibTeX entries.
##' @param fields A character vector specifying the fields to format.  The
##'     available options are \code{"title"}, \code{"author"}, \code{"journal"},
##'     and \code{"pages"}.  Multiple choices can be specified.
##' @param protected_words Optional words that needs protection by curly braces
##'     from cases changing by BibTeX style.
##' @param bibtex_file A character string presenting the BibTeX file that needs
##'     formatting.
##' @param output_file A character string presenting the output BibTeX file.  By
##'     default, the input BibTeX file will be overwritten with a backup file.
##' @param backup A logical value.  If \code{TRUE}, a backup file will be
##'     created to check and tweak the formatting options.
##' @param dry_run A logical value.  If \code{TRUE}, the formatted BibTeX
##'     entries will be returned without actually (over)writing a BibTeX file to
##'     the disk.  The default value is \code{FALSE}.
##' @param ... Other arguments passed to \code{format_bibtex_entry}.
##'
##' @return A \code{bibentry} object.
##'
##' @example inst/examples/ex-bibtex.R
##'
##' @importFrom utils person bibentry
NULL


##' @rdname format_bibtex_entry
##' @export
format_bibtex_entry <-
    function(entry,
             fields = c("title", "author", "journal", "pages"),
             protected_words = getOption("formatBibtex.protected_words"),
             ...)
{
    if (! inherits(entry, "bibentry")) {
        stop("The 'entry' must be of class 'bibentry'.")
    }
    ## specify choices to make it faster
    fields <- match.arg(fields,
                        choices = c("title", "author", "journal", "pages"),
                        several.ok = TRUE)
    ## helper functions
    add_key_bibtype <- function(x) {
        x$key <- attr(x, "key")
        x$bibtype <- attr(x, "bibtype")
        x
    }
    list_has <- function(x, name) {
        if (is.null(x[[name]]))
            FALSE
        TRUE
    }
    bib_list <- unclass(entry)
    res_list <- lapply(seq_along(bib_list), function(i) {
        xi <- add_key_bibtype(bib_list[[i]])
        if (list_has(xi, "title") && "title" %in% fields) {
            xi$title <- format_string(
                xi$title, style = "title", str2ws = "[ ]+|\\n",
                str4split = "-|[ ]+|/", protect_curly_braces = TRUE,
                protected_words = protected_words
            )
        }
        if (list_has(xi, "author") && "author" %in% fields) {
            tmp <- lapply(seq_along(xi$author), function(k) {
                k_list <- unclass(xi$author[[k]])[[1]]
                k_list$given <- gsub("^([A-Z])\\.?([A-Z])\\.?$", "\\1. \\2.",
                                     gsub("^([A-Z])$", "\\1.", k_list$given))
                do.call(utils::person, k_list)
            })
            xi$author <- do.call(c, tmp)
        }
        if (list_has(xi, "journal") && "journal" %in% fields) {
            xi$journal <- format_string(
                xi$journal, style = "title", str2ws = "[ ]+|\\n",
                str4split = "[ ]+|/|-", protect_curly_braces = FALSE,
                protected_words = protected_words
            )
        }
        if (list_has(xi, "pages") && "pages" %in% fields) {
            xi$pages <- gsub("-+", "--", xi$pages)
        }
        do.call(utils::bibentry, xi)
    })
    res <- do.call(c, res_list)
    toBibtex2(res)
}



##' @rdname format_bibtex_entry
##' @export
format_bibtex_file <- function(bibtex_file,
                               output_file = bibtex_file,
                               backup = (output_file == bibtex_file),
                               dry_run = FALSE,
                               ...)
{
    if (backup) {
        backup_file <- paste0(bibtex_file, "~")
        if (file.copy(from = bibtex_file, to = backup_file,
                      overwrite = FALSE)) {
            message("Created a backup of the original bibtex file: ",
                    backup_file)
        } else {
            stop("Failed to create a backup of the original bibtex file.")
        }
    }
    if (! requireNamespace("bibtex", quietly = TRUE)) {
        stop("The 'format_bibtex_file()' needs 'bibtex' package installed",
             "to parse BibTeX file.")
    }
    bib_list <- bibtex::read.bib(bibtex_file)
    bib_out <- format_bibtex_entry(entry = bib_list, ...)
    if (dry_run) {
        return(bib_out)
    }
    writeLines(bib_out, output_file)
    if (is_emacs_available()) {
        status <- system2(
            "emacs",
            args = sprintf(paste(
                "--batch -Q %s",
                "--eval '(setq make-backup-files nil)'",
                "--eval '(setq-default fill-column 80)'",
                "--eval '(setq-default indent-tabs-mode nil)'",
                "-f bibtex-reformat",
                "-f bibtex-sort-buffer",
                "-f save-buffer"
            ), output_file),
            stdout = TRUE,
            stderr = TRUE
        )
    }
    invisible(bib_out)
}


## modified from utils:::toBibtex.bibentry
toBibtex2 <- function(object, ...)
{
    format_author <- function(author) {
        ## remove "others"
        is_others <- sapply(author, function(a) {
            identical(a$family, "others") && is.null(a$given)
        })
        author <- author[which(! is_others)]
        paste(sapply(author, function(p)
        {
            fnms <- p$family
            only_given_or_family <- (is.null(fnms) || is.null(p$given))
            fbrc <- if (only_given_or_family) {
                        c("{", "}")
                    } else if (length(fnms) > 1L ||
                               any(grepl("[[:space:]]", fnms))) {
                        c("{", "},")
                    } else {
                        c("", ",")
                    }
            gbrc <- if (only_given_or_family)
                        c("{", "}")
                    else
                        ""
            format(p, include = c("family", "given"),
                   braces = list(given = gbrc, family = fbrc))
        }), collapse = " and ")
    }
    format_bibentry1 <- function(object) {
        object <- unclass(object)[[1L]]
        rval <- paste0("@", tolower(attr(object, "bibtype")),
                       "{", attr(object, "key"), ",")
        if ("author" %in% names(object))
            object$author <- format_author(object$author)
        if ("editor" %in% names(object))
            object$editor <- format_author(object$editor)
        tmp <- sapply(names(object), function(n) {
            paste0("  ", n, " = {", object[[n]], "}")
        })
        rval <- c(rval, paste0(tmp, c(rep(",", length(object) - 1), "")),
                  "}", "")
        return(rval)
    }
    if (length(object)) {
        object$.index <- NULL
        rval <- utils::head(unlist(lapply(object, format_bibentry1)), - 1L)
    }
    else {
        rval <- character()
    }
    class(rval) <- "Bibtex"
    rval
}
