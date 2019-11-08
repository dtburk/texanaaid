#' Tokenize
#'
#' This function takes a Corpus or vector of documents and returns a list of character vectors of the tokens (words) in each document.
#' @param x An object inheriting from Corpus, a list of character vectors, or a character vector.
#' @return A list of character vectors of the tokens (words) in each document.
#' @keywords text
#' @export
#' @examples
#' docs <- c("What are you doing down there, crashy?", "I'm real good at lying to Lois.")
#' words <- convert_to_list_of_word_vectors(docs)

tokenize <- function(x) {
    if(inherits(x, "TextDocument")) x <- x$content
    if(all(sapply(x, inherits, "TextDocument"))) {
        x <- lapply(x, function(doc) doc$content)
    }
    if(any(sapply(x, length) > 1)) {
        x <- lapply(x, stringr::str_c, collapse=" ")
    }
    x <- lapply(x, remove_punctuation)
    x <- lapply(x, stringr::str_to_lower)
    x <- lapply(x, stringr::str_replace_all, "[0-9]+", "")
    tokenized <- stringr::str_split(x, "\\W+")
    if(length(tokenized)==1) tokenized <- unlist(tokenized)
    return(tokenized)
}
