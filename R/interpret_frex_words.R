interpret_frex_words <- function(stm_model, raw_texts, results_dir, n=10, stemmed=FALSE) {
    frex_dir <- file.path(results_dir, "Interpretation of FREX Words")
    if(!dir.exists(frex_dir)) dir.create(frex_dir)
    frex <- stm::labelTopics(stm_model, n=n)$frex
    for(topic in 1:nrow(frex)) {
        frex_per_1000 <- 1000*exp(
            stm_model$beta[[1]][[1]][
                ,
                sapply(frex[topic, ], function(x) which(stm_model$vocab==x))
            ]
        )
        topic_dir <- file.path(frex_dir, paste0("Topic ", topic))
        if(!dir.exists(topic_dir)) dir.create(topic_dir)
        for(word in 1:ncol(frex)) {
            word_file <- file.path(topic_dir, paste0(word, ".txt"))
            cat("", file=)
        }
    }
}
