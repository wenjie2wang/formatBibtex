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

##' Format a Character Vector
##'
##' This function formats a character vector by following APA (American
##' Psychological Assoication) style.
##'
##' The available style options are title case and sentence case.  The first
##' word of the string and the first word after colon will be always
##' capitalized.  For title case (default), the function would capitalize each
##' word with a few exceptions, such as short conjunctions and prepositions.
##' For sentence case, the function would not capitalize a word unless it
##' starts a string, come after a colon, or is a proper noun.  We may specify
##' some words that need protection from any kind of case conversion.
##'
##' The function would also replace possible splitting characters, such as
##' underscores with white spaces.  Empty strings (\code{""} or \code{''}),
##' \code{NA}, \code{character(0)}, and \code{NULL} are allowed and returned as
##' they were.
##'
##' @param x A character vector that needs formatting.
##' @param style A character string for specifying case style. \code{"title"}
##'     for title case or \code{"sentence"} for sentence case.
##' @param str2ws Character expression that would be replaced with white space.
##'     By default, underscores will be replaced with white spaces.
##' @param str4split Character expression that would be used to split the string
##'     into individual words.
##' @param strict A logical value specifying whether to lower the cases for
##'     letters after the first letter.  The default value is \code{TRUE}.
##' @param arguments An optional argument list for specifying more details when
##'     replacing specified characters with whitespaces by using
##'     \code{\link[base]{gsub}}.
##' @param protect_curly_braces A logical vector of length one indicating if
##'     curly braces represent a block that needs protection.  This argument is
##'     mainly intended to format BibTeX entries.
##' @param lowercase_words Some words that should be almost always be lowercase.
##'     The default values are \code{format_options$get("lowercase_words")}.
##' @param protected_words Some words that should be kept in original case and
##'     should be not be convert to lowercase or uppercase.  The default values
##'     are \code{format_options$get("protected_words")}.
##' @param punctuation A character expression that should be considered as
##'     punctuation and should not be considered as a part of the protected
##'     words.  The function would remove these punctuation before checking
##'     whether the words need protecting.  The default values are
##'     \code{format_options$get("punctuation")}.
##' @param ... Other arguments that are not used now.
##'
##' @return A character vector of the same length as the input.
##'
##' @example inst/examples/ex-format_string.R
##'
##' @importFrom utils modifyList
##'
##' @export
format_string <-
    function(x,
             style = c("title", "sentence"),
             str2ws = "_",
             str4split = " |-",
             strict = TRUE,
             arguments = list(),
             protect_curly_braces = FALSE,
             lowercase_words = NULL,
             protected_words = NULL,
             punctuation = NULL,
             ...)
{
    ## return NULL immediately
    if (is.null(x)) return(NULL)
    ## for backward compatibility with version 0.1.0
    if (is.null(lowercase_words)) {
        lowercase_words <- getOption("formatBibtex.lowercare_words")
    }
    if (is.null(protected_words)) {
        protected_words <- getOption("formatBibtex.protected_words")
    }
    if (is.null(punctuation)) {
        punctuation <- getOption("formatBibtex.punctuation")
    }
    ## version >=0.1.1 uses format_options
    if (is.null(lowercase_words)) {
        lowercase_words <- format_options$get("lowercase_words")
    }
    if (is.null(protected_words)) {
        protected_words <- format_options$get("protected_words")
    }
    if (is.null(punctuation)) {
        punctuation <- format_options$get("punctuation")
    }
    ## preserve possible NA's
    is_any_na <- anyNA(x)
    if (is_any_na) {
        old_x <- x
        is_na_x <- is.na(x)
        x <- x[! is_na_x]
    }
    ## capitalize words
    capWords <- function(x, strict, firstOnly, add_braces)
    {
        ## function that uppers the case of the first letters
        capFirstChar <- function(s_vec, sep, strict, firstOnly, add_braces)
        {
            ## order restoring original string
            order_back <- function(n) {
                order(c(2 * seq_len(n) - 1, 2 * seq_len(n - 1)))
            }
            ## early return
            len_s <- length(s_vec)
            if (len_s == 0) return("")
            order_ind <- order_back(len_s)
            ## 1. convert to lowercase first
            ## remove any punctuation when checking
            s_vec1 <- gsub(pattern = punctuation,
                           replacement = "", x = s_vec)
            ## words that need protection
            protect_idx <- s_vec1 %in% protected_words |
                s_vec %in% protected_words
            if (protect_curly_braces)
                protect_idx <- protect_idx | grepl("\\{|\\}", s_vec)
            s_vec <- ifelse(protect_idx, s_vec, tolower(s_vec))
            ## indicator for words that always need to be capitalized
            capIdx <- if (firstOnly) {
                          c(TRUE, rep(FALSE, len_s - 1))
                      } else {
                          rep(TRUE, len_s)
                      }
            ## words that should not be capitalized
            lowercase_idx <- s_vec1 %in% lowercase_words
            capIdx[lowercase_idx] <- FALSE
            capIdx[1L] <- TRUE
            ## find possible colons
            colon_ind <- grep(":", s_vec, fixed = TRUE)
            colon_force <- colon_ind[colon_ind < len_s] + 1
            ## capitalize words after colon
            capIdx[colon_force] <- TRUE
            ## ignore protection words
            capIdx[protect_idx] <- FALSE
            ## offset for non-alpha-number
            offset_idx <- nchar(gsub(sprintf("^((%s)+)?(.*)", punctuation),
                                     "\\1", s_vec))
            words <- paste0(
            {
                tmp1 <- substring(s_vec, 1, 1 + offset_idx)
                tmp1 <- ifelse(capIdx, toupper(tmp1), tmp1)
                if (add_braces && length(colon_force) &&
                    ! any(colon_force %in% which(offset_idx > 0))) {
                    tmp0 <- tmp1[colon_force]
                    tmp1[colon_force] <- ifelse(
                        grepl("[a-zA-Z]", tmp0),
                        sprintf("{%s}", tmp0), tmp0
                    )
                }
                tmp1
            },
            {
                tmp2 <- substring(s_vec, 2 + offset_idx)
                ifelse(protect_idx | ! strict, tmp2, tolower(tmp2))
            })
            if (add_braces && any(protect_idx)) {
                tmp_idx <- protect_idx & ! grepl("\\{|\\}", words)
                words[tmp_idx] <- sprintf("{%s}", words[tmp_idx])
            }
            out <- c(words, sep)[order_ind]
            paste0(out, collapse = "")
        }
        ## check protection words contains str for splitting
        protected_words <- unique(protected_words)
        early_protected_words <- grep(
            str4split, protected_words, value = TRUE
        )
        placeholder <- "FORMATBIBTEXPROTECTEDWORD"
        x4split <- x
        if (length(early_protected_words)) {
            num_dig <- ceiling(log(length(early_protected_words), base = 10))
            placeholder <- paste0(placeholder,
                                  sprintf(sprintf("%%0%dd", num_dig),
                                          seq_along(early_protected_words)))
            for (k in seq_along(early_protected_words)) {
                x4split <- gsub(early_protected_words[k], placeholder[k],
                                x = x4split, fixed = TRUE)
            }
        }
        wordsList <- strsplit(x4split, split = str4split)
        sepList <- regmatches(x4split, gregexpr(str4split, x4split))
        for (k in seq_along(early_protected_words)) {
            for (i in seq_along(x)) {
                wordsList[[i]] <- gsub(placeholder[k],
                                       early_protected_words[k],
                                       x = wordsList[[i]],
                                       fixed = TRUE)
            }
        }
        vapply(
            seq_along(wordsList),
            function(i) {
                s_vec <- wordsList[[i]]
                sep <- sepList[[i]]
                capFirstChar(s_vec, sep, strict, firstOnly, add_braces)
            },
            FUN.VALUE = character(1),
            USE.NAMES = ! is.null(names(x))
        )
    }
    ## match case style
    style <- match.arg(style)
    ## replace `str2ws` with whitespace
    x <- do.call(
        gsub,
        modifyList(getArgs(arguments, "str2ws"),
                   list(pattern = str2ws, replacement = " ", x = x))
    )
    ## remove extra whitespace
    x <- gsub("[ ]+", " ", x)
    ## 0. if not protected
    ## 1. convert all to lowercase
    ## 2. if (sentence case)
    ##       capitalize the first word
    ##    else if (title case)
    ##       capitalize all the words
    out <- capWords(
        x,
        strict = strict,
        firstOnly = (style != "title"),
        add_braces = protect_curly_braces
    )
    ## add back possible NA's
    if (is_any_na) {
        old_x[! is_na_x] <- out
        return(old_x)
    }
    ## return
    out
}
