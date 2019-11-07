#' Get common words
#' 
#' Get the most frequent words in a group of documents.
#' @param prob_mtrx A matrix with (named) columns for words, and rows for either documents or document groups. If each row represents a document, \code{group_membership} must be provided.
#' @param group_num An integer corresponding to the document group of interest.
#' @param n An integer specifying how many words should be returned.
#' @param group_membership An integer vector specifying the group membership of documents in \code{prob_mtrx}.
#' @export

get_common_words <- function(prob_mtrx, group_num, n, group_membership=NULL) {
    if(!is.null(group_membership)) {
        focal_docs <- prob_mtrx[group_membership == group_num, , drop=FALSE]
        focal_probs <- apply(focal_docs, 2, mean)
    } else {
        focal_probs <- prob_mtrx[group_num, ]
    }
    return(names(sort(focal_probs, decreasing=TRUE))[1:n])
}