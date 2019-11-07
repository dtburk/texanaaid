#' Save summary csv
#' 
#' Save a csv file with the highest FREX or probability words for each topic in a structural topic model
#' @param stm_model Output from a call to \code{stm}.
#' @param filename Path to write the file to.
#' @param type frex or prob.
#' @param n_words How many words per topic?
#' @export

make_summary_csv <- function(stm_model, filename, type=c("frex", "prob"), n_words=150) {
    word_type <- match.arg(type)
    words <- labelTopics(stm_model, n=n_words)[[word_type]]
    out <- data.frame(t(words))
    names(out) <- paste0("Topic ", 1:ncol(out))
    write.csv(out, file=filename, row.names=FALSE)
}