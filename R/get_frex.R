#' Get FREX words
#' 
#' Get frequent and exclusive words for a group of documents, relative to other groups, based on a probability matrix of word occurrence.
#' @param prob_mtrx A matrix with (named) columns for words, and rows for either documents or document groups. If each row represents a document, \code{group_membership} must be provided.
#' @param group_num An integer corresponding to the document group of interest.
#' @param group_membership An integer vector specifying the group membership of documents in \code{prob_mtrx}.
#' @param wt A value between 0 and 1 specifying the weight to be given to word exclusivity in calculating FREX.
#' @param values A logical specifying whether the function should return the actual FREX values or just words in decreasing FREX order.
#' @export

get_frex <- function(prob_mtrx, group_num, group_membership=NULL, 
                     wt=0.5, values=FALSE) {
    if(!is.null(group_membership)) {
        focal_docs <- prob_mtrx[group_membership == group_num, , drop=FALSE]
        focal_probs <- apply(focal_docs, 2, mean)
    } else {
        focal_probs <- prob_mtrx[group_num, ]
    }
    overall_probs <- apply(prob_mtrx, 2, mean)
    
    focal_ecdf <- ecdf(focal_probs)
    focal_cum_probs <- focal_ecdf(focal_probs)
    
    relative_probs <- focal_probs / overall_probs
    relative_ecdf <- ecdf(relative_probs)
    relative_cum_probs <- relative_ecdf(relative_probs)
    
    frex <- 1 / ((wt/relative_cum_probs) + ((1-wt)/focal_cum_probs))
    names(frex) <- colnames(prob_mtrx)
    frex <- sort(frex, decreasing=TRUE)
    if(values) {
        return(list(words=names(frex), values=frex))
    } else {
        return(names(frex))
    }
    
}