#' Topic Prevalence by Covariate
#' 
#' This function returns a data table with the mean topic prevalence for each level of a character or factor covariate.
#' @param stm_model An object of class STM.
#' @return A data table whose first column contains the levels of the covariate and subsequent columns give the average topic prevalence of each topic in documents at each level of the covariate.
#' @keywords text
#' @export

get_topic_prevalence_by_covariate <- function(stm_model, by_what, data) {
    topics_by_docs <- data.table(stm_model$theta)
    n_topics <- ncol(topics_by_docs)
    setnames(topics_by_docs, names(topics_by_docs), paste0("Topic_", 1:n_topics))
    v <- data[[by_what]]
#     if(!is.data.table(eval(parse(text="topics_by_docs")))) print("topics_by_docs is NOT a data.table")
#     else print("topics_by_docs IS a data.table")
    eval(parse(text=sprintf("topics_by_docs$%s <- v", by_what)))
    if(is.numeric(v)) eval(parse(text=sprintf("setorder(topics_by_docs, %s)", by_what)))
    topic_score_by <- eval(parse(text=sprintf("topics_by_docs[ , .(%s), by=%s]", 
                                                     paste0("Topic_", 1:n_topics, "=mean(Topic_", 1:n_topics, ")", collapse=", "),
                                              by_what)))
    return(topic_score_by)
}