% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load_txt.R
\name{load_txt}
\alias{load_txt}
\title{Load text files.}
\usage{
load_txt(pathway, encoding = "UTF-8", comment_char = NULL,
  recursive = FALSE)
}
\arguments{
\item{pathway}{Pathway to the directory with \code{.txt} files or the pathway to a
specific \code{.txt} file, for example, \code{pathway =
"/pathway/to/directory/"} or \code{pathway = "/pathway/to/file_name.txt"},
where \code{"/pathway/to/directory"} and \code{"pathway/to/file_name.txt"} are
replaced by the actual pathway on a user's machine. If the \code{.txt} files
to be loaded are in the working directory of the R session, the user can
simply specify \code{load_txt(getwd())}.}

\item{encoding}{Specifies the type of encoding of the \code{.txt} files, with
the options \code{"UTF-8"} (the default) and \code{"latin1"}.}

\item{comment_char}{Specifies the comment character, if any, that is at the
beginning of lines that should be ignored when loading the files. If
\code{comment_char = NULL} (the default), all lines are loaded. This
argument can be used to eliminate headers from corpus files before using
\code{ngram_freq()} or \code{concord()}.}

\item{recursive}{Specifies whether the function should recursively search for
\code{.txt} files, beginning at the directory given in \code{pathway}. This
argument is ignored if \code{pathway} points to a specific \code{.txt} file.}
}
\value{
Returns a list with each \code{.txt} file in a separate list element,
    with the \code{.txt} file name as the corresponding list element name.
}
\description{
Loads text files and puts them into a list, one file per list element, with
the file name as the corresponding list element name. This function only
loads files with the extension \code{.txt}; files with other extensions in
the directory are ignored. The output of this function can be passed
to \code{concord()} and \code{ngram_freq()}.
}

