#' Get Long Words
#'
#' This function takes a Corpus or vector of documents and returns words with length greater than a threshold.
#' @param x An object inheriting from Corpus, a list of character vectors, or a character vector.
#' @param length_threshold An integer. Words of length strictly greater than this threshold will be returned.
#' @return A character vector of words with length greater than \code{length_threshold}.
#' @keywords text
#' @export
#' @examples
#' docs <- c("What are you doing down there, crashy?", "I'm real good at lying to Lois.")
#' long_words <- get_long_words(docs, length_threshold=4)

get_long_words <- function(x, length_threshold=20) {

    all_words <- unique(unlist(tokenize(x)))
    lengths <- stringr::str_length(all_words)
    return(all_words[lengths > length_threshold])

}
