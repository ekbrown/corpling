#' Load text files.
#'
#' Loads text files and puts them into a list, one file per list element, with
#' the file name as the corresponding list element name. This function only
#' loads files with the extension \code{.txt}; files with other extensions in
#' the directory are ignored. The output of this function can be passed
#' to \code{concord()} and \code{ngram_freq()}.
#'
#' @param pathway Pathway to the directory with \code{.txt} files or the pathway to a
#'     specific \code{.txt} file, for example, \code{pathway =
#'     "/pathway/to/directory/"} or \code{pathway = "/pathway/to/file_name.txt"},
#'     where \code{"/pathway/to/directory"} and \code{"pathway/to/file_name.txt"} are
#'     replaced by the actual pathway on a user's machine. If the \code{.txt} files
#'     to be loaded are in the working directory of the R session, the user can
#'     simply specify \code{load_txt(getwd())}.
#' @param encoding Specifies the type of encoding of the \code{.txt} files, with
#'     the options \code{"UTF-8"} (the default) and \code{"latin1"}.
#' @param comment_char Specifies the comment character, if any, that is at the
#'     beginning of lines that should be ignored when loading the files. If
#'     \code{comment_char = NULL} (the default), all lines are loaded. This
#'     argument can be used to eliminate headers from corpus files before using
#'     \code{ngram_freq()} or \code{concord()}.
#' @param recursive Specifies whether the function should recursively search for
#'     \code{.txt} files, beginning at the directory given in \code{pathway}. This
#'     argument is ignored if \code{pathway} points to a specific \code{.txt} file.
#'
#' @return Returns a list with each \code{.txt} file in a separate list element,
#'     with the \code{.txt} file name as the corresponding list element name.
#'
#' @export
load_txt <- function(pathway, encoding = "UTF-8", comment_char = NULL, recursive = FALSE) {

  # determines whether the pathway is to a directory or a file
  info <- file.info(pathway)

  # gets file name(s)
  if (info$isdir) {
    file_names <- list.files(pathway, pattern = "\\.txt", full.names = TRUE, recursive = recursive, ignore.case = TRUE)
  } else if (info$isdir == FALSE) {
    file_names <- pathway
  } else {
    stop("It seems that you have specified neither a directory nor a specific file.\n")
  }

  # loads text files
  txt <- lapply(file_names, function(x) readLines(x, encoding = encoding))

  names(txt) <- basename(file_names)

  # optionally, eliminates lines that begin with comment_char
  if (is.null(comment_char)) {
    return(txt)
  } else {
    txt <- lapply(txt, function(x) grep(paste0("^", comment_char), x, value = TRUE, invert = TRUE))
    return(txt)
  }

} # end function
