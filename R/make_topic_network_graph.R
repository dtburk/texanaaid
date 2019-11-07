#' Make topic network graph
#' 
#' Make topic network graph from STM results.
#' @param stm_results Path to directory containing STM results. This directory should have the same name as the .Rdata file containing the results, and the same name as the results object contained in that file.
#' @param stm_input_file Path to .Rdata file containing R object used as input for the STM analysis. This object should be named "stm_input".
#' @param output_file Path to .png file in which to save network graph.
#' @export

make_topic_network_graph <- function(stm_results, stm_input_file, 
                                     output_file=file.path(stm_results, "topic_network.png")) {
    require(stm)
    require(igraph)
    
    load(stm_input_file)

    load(file.path(stm_results, paste0(basename(stm_results), ".Rdata")))
    k <- ncol(get(stm_results)$theta)
    vertex_labels <- labelTopics(get(stm_results), n=2)$frex
    vertex_labels <- paste(1:as.integer(k), vertex_labels[,1], vertex_labels[,2], sep="\n")
    vertex_sizes <- apply(get(stm_results)$theta, 2, mean)*20*as.integer(k)
    topics_by_geog <- get_topic_prevalence_by_covariate(get(stm_results), by_what="geog", stm_input$meta)
    topic_max <- as.numeric(apply(as.matrix(topics_by_geog)[,-1], 2, max))
    topic_median <- as.numeric(apply(as.matrix(topics_by_geog)[,-1], 2, median))
    color_by_topic <- c("red", "gold", "dodgerblue")[apply(as.matrix(topics_by_geog)[,-1], 2, which.max)]
    split_topics <- (topic_max - topic_median) < 0.015
    split_topics_geog <- as.matrix(topics_by_geog[ , which(split_topics)+1, with=FALSE])
    split_colors <- apply(split_topics_geog, 2, function(x) {
        geogs <- c(which.max(x), which(x==median(x)))
        if(setequal(geogs, c(1,2))) return("darkorange")
        if(setequal(geogs, c(1,3))) return("purple")
        if(setequal(geogs, c(2,3))) return("lawngreen")
    })
    color_by_topic[split_topics] <- split_colors
    #label_color_by_topic <- sapply(color_by_topic, function(x) complementary(x, plot=FALSE)[2])
    #label_color_by_topic[color_by_topic=="red"] <- "black"
    topic_corr <- topicCorr(get(stm_results), method="simple", cutoff=0.01)
    edge_widths <- 100*E(graph_from_adjacency_matrix(topic_corr$poscor, mode="undirected", 
                                                     weighted=TRUE, diag=FALSE))$weight
    png(filename = output_file, height=1200, width=1200)
    plot(topic_corr, vlabels=vertex_labels, vertex.label.cex=1.5, 
         vertex.color = color_by_topic, vertex.label.color="black", 
         vertex.size=vertex_sizes, vertex.frame.color=NA, edge.width=edge_widths, edge.lty=1, 
         edge.color="grey")
    dev.off()
}
