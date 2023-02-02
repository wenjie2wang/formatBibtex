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

## Get Possibly Specified Arguments
## @param arguments A named list
## @param x A character vector
getArgs <- function(arguments, x)
{
    if (! is.list(arguments))
        stop("The `arguments` has to be a list.", call. = FALSE)
    arguments[x]
}

## check if emacs is available
is_emacs_available <- function()
{
    system2("which", "emacs", stdout = FALSE, stderr = FALSE) == 0
}
