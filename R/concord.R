#' Produce a concordance.
#'
#' Produces a concordance of a regular expression and returns
#' a data frame.
#'
#' @param text The text to search in, as either a character vector or a list
#'    of character vectors.
#' @param pattern The Perl-compartible regular expression to search for.
#' @param ignore_case If \code{ignore_case = TRUE} (the default),
#'     a case-insensitive search is performed.
#' @param locations If \code{locations = TRUE}, the element indexes with
#'     matches are returned.
#' @param extra_context If positive, specifies the number of additional \strong{lines}
#'     around the lines with matches to return. Useful if not enough context is
#'     given in the lines with matches.
#' @param shorten If positive, truncates the preceding and following contexts
#'     to the number of \strong{characters} specified. Useful if too much context is
#'     given in the lines with matches, for example, in texts with (very)
#'     long paragraphs.
#'
#' @return A data frame.
#'
#' @seealso Stefan Th. Gries' function \href{http://www.linguistics.ucsb.edu/faculty/stgries/exact_matches.r}{exact.matches()}.
#'
#' @examples
#' concord("hello", "e")
#' concord("some sentence here", "sentence")
#' concord("some sentence here", "s[eo]")
#'
#' text <- c("first sentence here", "short paragraph here", "third line here")
#' concord(text, "e[nr]", locations = TRUE)
#' concord("some sentence here", "e(?=r)")
#' concord("some sentence here", "e(?!n)")
#' @export
concord <- function(text, pattern, ignore_case = TRUE, locations = FALSE, extra_context = 0, shorten = 0) {
  UseMethod("concord")
}

#' @export
concord.list <- function(text, pattern, ignore_case = TRUE, locations = FALSE, extra_context = 0, shorten = 0) {

  output <- lapply(text, function(x) concord.default(x, pattern, ignore_case, locations, extra_context, shorten))
  rep_times <- sapply(output, nrow)
  rep_times <- sapply(rep_times, function(x) if (is.null(x)) {0} else {x})

  if (is.null(names(output))) {
    names(output) <- seq_along(output)
  }

  SOURCE <- rep(names(output), rep_times)

  output <- data.frame(do.call("rbind", output))
  output <- cbind(SOURCE, output)
  rownames(output) <- seq(1, nrow(output))
  return(output)

}

#' @export
concord.default <- function(text, pattern, ignore_case = TRUE, locations = FALSE, extra_context = 0, shorten = 0) {

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
    if (extra_context > 0) {

      # extra_context <- ceiling(extra_context)
      all_pre <- c()
      all_fol <- c()
      counter <- extra_context
      while (counter > 0) {

        # gets extra preceding context
        cur_pre_num <- element_num - counter
        cur_pre_num <- sapply(cur_pre_num, function(x) if (x < 0) {0} else {x})
        temp <- sapply(cur_pre_num, function(x) text[x])
        cur_pre <- sapply(temp, function(x) if (length(x) > 0) {x} else {""})
        all_pre <- stringr::str_c(all_pre, cur_pre, sep = " ")

        # gets extra following context
        cur_fol_num <- element_num + counter
        cur_fol_num <- sapply(cur_fol_num, function(x) if (x > length(text)) {0} else {x})
        temp <- sapply(cur_fol_num, function(x) text[x])
        cur_fol <- sapply(temp, function(x) if (length(x) > 0) {x} else {""})
        all_fol <- stringr::str_c(cur_fol, all_fol, sep = " ")

        counter <- counter - 1

      } # end while loop

      pre <- stringr::str_c(all_pre, pre, sep = " ")
      fol <- stringr::str_c(fol, all_fol, sep = " ")

    } # end extra context

    pre <- stringr::str_trim(pre)
    fol <- stringr::str_trim(fol)

    # optionally, truncates the pre and post contexts
    if (shorten > 0) {
      pre_len <- stringr::str_length(pre)
      pre <- stringr::str_sub(pre, pre_len - shorten + 1, pre_len)
      fol <- stringr::str_sub(fol, 1, shorten)
    }

    # pastes output together
    output <- stringr::str_c(pre, matches, fol, sep = "\t")

    # optionally, adds the element number
    if (locations) {
      output <- stringr::str_c(element_num, output, sep = "\t")
    }

    # returns the output to the user
      df <- data.frame(do.call("rbind", stringr::str_split(output, "\t")))
      if (locations) {
        names(df) <- c("INDEX", "PRE_CONTEXT", "MATCH", "POST_CONTEXT")
      } else {
        names(df) <- c("PRE_CONTEXT", "MATCH", "POST_CONTEXT")
      }

    return(df)

  } # end if any matches

} # end function
