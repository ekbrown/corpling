#' Produce a concordance.
#'
#' Produces a concordance of a regular expression, that is,
#' a display of words as they are used in context, and returns
#' either a data frame, a character vector, or a preview,
#' as specified by \code{to_return}.
#'
#' @param text The text to be searched in, as a character vector or something
#'     coercible to it, such as a list with elements of character vectors.
#'     Tabs
#' @param pattern The Perl-compartible regular expression to search for.
#' @param to_return Specifies the output as either a data frame, a character
#'    vector, or a preview (with base R's \code{View()} function),
#'    with the options \code{"df"} (the default), \code{"char"}, or
#'    \code{"preview"}.
#' @param ignore_case Specifies whether the search should be case-sensitive or
#'     case-insensitive (the default).
#' @param locations Specifies whether, in addition to the contextualized
#'     matches, the element indexes with matches are returned.
#' @param extra_context Specifies the number of extra lines around matches
#'     to return, if more context is desired.
#'
#' @return Returns either a data frame or a character vector, or previews
#'     the results, as specified by \code{to_return}.
#'
#' @seealso Stefan Th. Gries' function \code{exact.matches()}:
#'     \url{http://www.linguistics.ucsb.edu/faculty/stgries/exact_matches.r}.
#'
#' @examples
#' concord("hello", "e")
#' concord("some sentence here", "sentence", to_return = "char")
#' concord("some sentence here", "s[eo]")
#' text <- c("first sentence here", "short paragraph here", "third line here")
#' concord(text, "e[nr]", locations = TRUE)
#' concord("some sentence here", "e(?=r)")
#' concord("some sentence here", "e(?!n)")
#' @export
concord <- function(text, pattern, to_return = "df", ignore_case = TRUE, locations = FALSE, extra_context = 0) {

  # replaces tabs with spaces in the input, so that the output doesn't have extra columns
  text <- stringr::str_replace_all(text, "\t", " ")

  # finds indexes of pattern within text
  locs <- stringr::str_locate_all(text, stringr::regex(pattern, ignore_case = ignore_case))

  # gets number of matches per element of text
  num_hits <- sapply(locs, nrow)

  # continues if any matches
  if (any(num_hits)) {

    # gets element number of elements with matches
    elements_with_hits <- seq_along(num_hits)[num_hits > 0]

    # eliminates elements w/o matches
    text_min <- text[elements_with_hits]
    num_hits <- num_hits[num_hits > 0]

    # duplicates the elements with matches, the number of times that there are matches
    text_min <- rep(text_min, num_hits)

    # gets the starting and ending positions of matches
    starts <- unlist(sapply(locs, function(x) x[, 'start']))
    ends <- unlist(sapply(locs, function(x) x[, 'end']))

    # gets the matches and preceding and following contexts, and pastes them together
    pre <- stringr::str_trim(stringr::str_sub(text_min, end = starts - 1))
    fol <- stringr::str_trim(stringr::str_sub(text_min, start = ends + 1))
    boundaries <- data.matrix(data.frame(start = starts, end = ends))
    matches <- stringr::str_trim(stringr::str_sub(text_min, start = boundaries))

    # gets index numbers with matches, as many times as there are matches
    if (locations | extra_context != 0) {
      element_num <- rep(elements_with_hits, num_hits)
    }

    # optionally, adds extra context
    if (extra_context < 0) stop("The argument 'extra_context' must be equal to or greater than '0'.")

    extra_context <- ceiling(extra_context)
    all_pre <- c()
    all_fol <- c()
    counter <- extra_context
    while (counter != 0) {

      cur_pre_num <- element_num - counter
      cur_pre_num <- sapply(cur_pre_num, function(x) if (x < 0) {0} else {x})
      temp <- sapply(cur_pre_num, function(x) text[x])
      cur_pre <- sapply(temp, function(x) if (length(x) > 0) {x} else {""})
      all_pre <- stringr::str_c(all_pre, cur_pre, sep = " ")

      cur_fol_num <- element_num + counter
      cur_fol_num <- sapply(cur_fol_num, function(x) if (x > length(text)) {0} else {x})
      temp <- sapply(cur_fol_num, function(x) text[x])
      cur_fol <- sapply(temp, function(x) if (length(x) > 0) {x} else {""})
      all_fol <- stringr::str_c(cur_fol, all_fol, sep = " ")

      counter <- counter - 1
    }

    if (extra_context > 0) {
      pre <- stringr::str_c(all_pre, pre, sep = " ")
      fol <- stringr::str_c(fol, all_fol, sep = " ")
    }

    pre <- stringr::str_trim(pre)
    fol <- stringr::str_trim(fol)

    # pastes output together
    output <- stringr::str_c(pre, matches, fol, sep = "\t")

    # optionally, adds the element number
    if (locations) {
      output <- stringr::str_c(element_num, output, sep = "\t")
    }

    # returns the output to the user
    if (to_return %in% c("df", "preview")) {
      df <- data.frame(do.call("rbind", stringr::str_split(output, "\t")))
      if (locations) {
        names(df) <- c("ELEMENT_NUM", "PRE_CONTEXT", "MATCH", "POST_CONTEXT")
      } else {
        names(df) <- c("PRE_CONTEXT", "MATCH", "POST_CONTEXT")
      }
      if (to_return == "df") {
        return(df)
      } else {
        View(df)
      }
    } else if (to_return == "char") {
      return(output)
    } else {
      stop(stringr::str_c("The argument 'to_return' must be either 'char', 'df', or 'preview'.\n"))
    }

  } # end if any matches

} # end function
