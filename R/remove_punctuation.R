#' Faster Punctuation Removal Courtesy of \code{stringr}
#'
#' Uses \code{stringr}'s \code{str_replace_all} to remove punctuation.
#' @param x A character vector.
#' @param punct_pattern Regular expression pattern to match for punctuation. Defaults to \code{"[^\\w ]|_"}, which matches any character that is not alphanumeric or a space, including underscores.
#' @return A character vector with punctuation removed.
#' @keywords text
#' @export
#' @examples
#' docs <- c("What are you doing down there, crashy?", "I'm real good at lying to Lois.")
#' docs <- remove_punctuation(docs)

remove_punctuation <- function(x, punct_pattern="[^\\w ]|_") {
    return(stringr::str_replace_all(x, punct_pattern, ""))
}
