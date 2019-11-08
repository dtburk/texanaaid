#' Interpret word
#'
#' Outputs information on the frequency and context of a word in documents analyzed by a topic model
#' @param word The word to be interpreted.
#' @param tkd_texts The tokenized texts used to create the document-term matrix.
#' @param doc_term_mtrx A document-term matrix summarizing the content of \code{tkd_texts}.
#' @param stemmed_texts If the texts were stemmed, this should be the stemmed and tokenized texts, otherwise \code{NULL}.
#' @param summary Logical: Should only a summary be printed, or all information returned?
#' @param custom_stem A character containing the regex pattern to match if \code{word} is a custom stem.
#' @export

interpret_word <- function(word,
                           tkd_texts,
                           doc_term_mtrx,
                           stemmed_texts=NULL,
                           summary=TRUE,
                           custom_stem=NULL) {
    if(word=="") {
        cat("Please enter a word to interpret.")
        return(NULL)
    }
    # Get indices of documents in which word appears
    vocab_idx <- which(doc_term_mtrx$dimnames$Terms==word)

    # Which documents does it appear in?
    which_docs <- with(doc_term_mtrx, i[j==vocab_idx])
    if(length(which_docs)==0) {
        cat(paste0('"', word, '" not found. Either this word doesn\'t appear in any documents, or you may need to enter it in stemmed form.'))
        return(NULL)
    }

    # In what context?
    contexts <- character(0)
    doc_of_contexts <- character(0)

    for(j in which_docs) {
        if(!is.null(stemmed_texts) & is.null(custom_stem)) {
            d <- stemmed_texts[[j]]
        } else d <- tkd_texts[[j]]
        if(is.null(custom_stem)) {
            which_words <- which(d==word)
        } else which_words <- which(str_detect(d, custom_stem))
        doc_of_contexts <- c(doc_of_contexts, rep(names(tkd_texts)[j], length(which_words)))
        contexts <- c(contexts, unlist(sapply(which_words, function(x) {
            start <- max(0, x-10)
            end <- min(length(d), x+10)
            str_c(tkd_texts[[j]][start:end], collapse=" ")
        }))
        )
    }
    contexts <- cbind(doc_of_contexts, contexts)
    colnames(contexts) <- c("Document", "Usage")

    if(summary) {
        cat(paste0('"', word, '"', " appears in ", length(which_docs), " documents.\n\n"))
        cat(paste0("Example of use: ", '"', sample(contexts[ , "Usage"], 1), '"'))
    } else return(contexts)

}
