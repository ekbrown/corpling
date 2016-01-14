#' Produce a frequency list of ngrams.
#'
#' Produces a frequency list of ngrams and returns a data frame. Users specify
#' the number of words in the ngrams, the order of the frequency list as either
#' alphabetical or by frequency, and whether the list be in ascending or
#' descending order, among other options.
#'
#' @param text The text with the ngrams whose frequencies are to be determined,
#'     as either a character vector or something coercible to it, such as a list
#'     of character vectors.
#' @param num_wd Specifies the number of words in the ngrams, whether single words
#'     (\code{num_wd = 1}, the default), bigrams (\code{num_wd = 2}), trigrams
#'     (\code{num_wd = 3}), or even larger ngrams.
#' @param ignore_case Specifies whether the frequency list be
#'     case-insensitive (\code{ignore_case = TRUE}, the default) or case-sensitve
#'     (\code{ignore_case = FALSE}). If case-insensitive, the ngrams are converted
#'     to upper-case.
#' @param order_by Specifies whether the frequency list be ordered alphabetically
#'     (\code{order_by = "alpha"}) or by frequency (\code{order_by = "freq"}).
#' @param descending Specifies whether the frequency list be ordered in
#'     ascending order (\code{descending = FALSE}, the default) or descending
#'    order (\code{descending = TRUE}).
#' @param min_freq Specifies the minimum frequency that an ngram must have
#'     in order to be included in the frequency list. With
#'     \code{min_freq = 1} (the default), all ngrams are included.
#' @param word_char If \code{word_char = NULL} (the default), the user's locale
#'     is used to distinguish word characters (e.g., "abc") from non-word
#'     characters (e.g., ".?!"). A user's locale can be determined with
#'     \code{sessionInfo()}. If words are split that shouldn't be, users can
#'     give a character class to specify word characters. For example,
#'     \code{word_char = "[-'a-z]+"} specifies that a combination of one or
#'     more contiguous dashes, apostrophes or letters "a" to "z" be considered
#'     as words, and as a result, the sentence "It's a hard-knock life,
#'     for us!" has six words rather than more.
#'
#' @return A two-column local data frame, the first column with the
#'     ngrams and the second column with the frequencies.
#'
#' @seealso For more info about local data frames, see
#'     \url{https://cran.r-project.org/web/packages/dplyr/vignettes/data_frames.html}).
#'
#' @examples
#' text <- c("First sentence here. Short, but sweet.")
#' text <- c(text, "Second one here, maybe?")
#' text <- c(text, "Third short paragraph here!")
#' text <- c(text, "Here too; with another thought.", "Here too.")
#'
#' ngram_freq(text)
#' ngram_freq(text, ignore_case = FALSE)
#' ngram_freq(text, order_by = "freq", descending = TRUE)
#' ngram_freq(text, order_by = "freq", descending = TRUE, min_freq = 2)
#'
#' # view difference (if any, given your locale)
#' ngram_freq("It's a hard-knock life, for us!")
#' ngram_freq("It's a hard-knock life, for us!", word_char = "[-'a-z]+")
#'
#' # gets bigram frequencies
#' ngram_freq(text, num_wd = 2)
#' ngram_freq(text, num_wd = 2, order_by = "freq", descending = TRUE)
#'
#' # gets trigram frequencies
#' ngram_freq(text, num_wd = 3)
#'
#' @export
ngram_freq <- function(text, num_wd = 1, ignore_case = TRUE, order_by = "alpha", descending = FALSE, min_freq = 1, word_char = NULL) {
  UseMethod("ngram_freq")
}

#' @export
ngram_freq.list <- function(text, num_wd = 1, ignore_case = TRUE, order_by = "alpha", descending = FALSE, min_freq = 1, word_char = NULL) {
  text <- unlist(text)
  NextMethod("ngram_freq")
}


#' Method used in ngram_freq.default
#'
#' @param x input character vector
#' @param n the number of words to lead by
#'
#' @keywords internal
#' @export
move_ahead <- function(x, n) c(x[-seq_len(n)], rep(NA, n))

#' @export
ngram_freq.default <- function(text, num_wd = 1, ignore_case = TRUE, order_by = "alpha", descending = FALSE, min_freq = 1, word_char = NULL) {

  # optionally, makes freq list case insensitive
  if (ignore_case) {
    text <- stringr::str_to_upper(text)
  }

  # gets words
  if (is.null(word_char)) {
    wds <- stringr::str_extract_all(text, stringr::regex("\\w+", ignore_case = ignore_case))
  } else {
    wds <- stringr::str_extract_all(text, stringr::regex(word_char, ignore_case = ignore_case))
  }

  # gets the ngrams
  to_eval <- "lapply(wds, function(x) stringr::str_c(x, "
  for (i in seq(num_wd) - 1) {
    if (i == 0) next
    to_add <- stringr::str_c("move_ahead(x, ", i, "), ")
    to_eval <- stringr::str_c(to_eval, to_add)
  }
  to_eval <- stringr::str_c(to_eval, "sep = ' '))")

  ngrams <- eval(parse(text = to_eval))
  ngrams <- unlist(lapply(ngrams, function(x) na.omit(x)))

  df <- data.frame(ngram = ngrams)
  df <- dplyr::count(df, ngram)

  # optionally, applies minimum frequency
  if (min_freq > 1) {
    df <- dplyr::filter(df, n >= min_freq)
  }

  # orders output as specified by user
  if (order_by == "alpha" && descending == FALSE) {
    output <- dplyr::arrange_(df, "ngram")
  } else if (order_by == "alpha" && descending == TRUE) {
    output <- dplyr::arrange_(df, "dplyr::desc(ngram)")
  } else if (order_by == "freq" && descending == FALSE) {
    output <- dplyr::arrange_(df, "n")
  } else if (order_by == "freq" && descending == TRUE) {
    output <- dplyr::arrange_(df, "dplyr::desc(n)")
  } else if (!order_by %in% c("alpha", "freq")) {
    stop("The argument 'order_by' must be either 'alpha' or 'freq'.\n")
  } else if (!descending %in% c(TRUE, FALSE)) {
    stop("The argument 'descending' must be either 'TRUE' or 'FALSE'.\n")
  }

  return(output)

} # end function
