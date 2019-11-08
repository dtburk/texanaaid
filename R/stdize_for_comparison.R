#' Standardize to Find Duplicates
#'
#' This function standardizes case and removes all non-alphanumeric characters from a document to facilitate comparison and identification of effective duplicates.
#' @param text A character vector
#' @return A character vector with entries standardized.
#' @keywords text
#' @export

stdize_for_comparison <- function(text) {
    tolower(stringr::str_replace_all(text, "\\W", ""))
}
