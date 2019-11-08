#' Create Standard Results Report for a Structural Topic Model
#'
#' Create standard results report for a structural topic model, including FREX and PROB words, topic prevalence plot, topic time trend plots, and most representative documents.
#' @param model An object of class STM.
#' @param report_dir Directory in which to save the report.
#' @param cov_dataset A data frame containing any document-level covariates. Must contain a variable named "year".
#' @param raw_texts A character vector containing the texts analyzed in the topic model in a human-readable format, to output most representative documents for each topic.
#' @param diff_cov The name of a (factor or character) variable in \code{cov_dataset} with which to produce a difference plot. If NULL, no plot will be produced.
#' @param diff_cov.value1 The first level of \code{diff_cov} to be used to create a difference plot.
#' @param diff_cov.value2 The second level of \code{diff_cov} to be used to create a difference plot.
#' @param representative_docs If \code{TRUE} (the default), save full texts of most representative documents. If \code{FALSE}, save only a list of titles of the most representative documents.
#' @export

create_topic_model_report <- function(model,
                                      report_dir,
                                      cov_dataset,
                                      raw_texts,
                                      diff_cov=NULL,
                                      diff_cov.value1=NULL,
                                      diff_cov.value2=NULL,
                                      representative_docs=TRUE) {

	d <- cov_dataset

	if(!dir.exists(report_dir)) dir.create(report_dir)

	m <- model
	num_topics <- ncol(m$theta)

	summ_graph_file <- file.path(report_dir, "Topic Proportions.png")
  png(filename=summ_graph_file, height=20*num_topics + 300, width=600)
  stm::plot.STM(m, labeltype="frex")
  dev.off()

  if(!is.null(diff_cov)) {
  	diff_cov_graph_file <- file.path(report_dir, paste0("Topic Prevalence by ", diff_cov, ".png"))
  	png(filename=diff_cov_graph_file, height=20*num_topics + 300, width=600)
  	fmla <- formula(paste0("~", diff_cov))
  	prep <- stm::estimateEffect(fmla, m, metadata=as.data.frame(d))
    stm::plot.estimateEffect(
      prep,
      diff_cov,
      model=m,
      method="difference",
      cov.value1=diff_cov.value1,
      cov.value2=diff_cov.value2,
      verbose.labels=FALSE,
      main=sprintf("Difference in Mean Topic Prevalence, %s minus %s",
                   diff_cov.value1, diff_cov.value2)
    )
    dev.off()
  }

  d[
    ,
    era := cut(
      year,
      breaks=c(1976, 1990, 1995, 2000, 2005, 2010, 2015),
      labels = c("Pre-1991", "1991-95", "1996-2000", "2001-05", "2006-10",
                 "2011-15"),
      include.lowest = TRUE,
      ordered_result=TRUE
    )
  ]
  topic_prevalence_by_year <- get_topic_prevalence_by_covariate(m, "era", d)
  setorder(topic_prevalence_by_year, era)
  ymax <- max(
    topic_prevalence_by_year[
      ,
      setdiff(names(topic_prevalence_by_year), "era"),
      with=FALSE
    ]
  )
  for(topic in 1:num_topics) {
    png(filename=file.path(report_dir, paste0("Time Trend, Topic ", topic, ".png")))
    plot(
      x=1:nrow(topic_prevalence_by_year),
      y=topic_prevalence_by_year[[paste0("Topic_", topic)]],
      type="o",
      main=paste0("Topic_", topic),
      ylab="Average Topic Score",
      xlab="Era",
      ylim=c(0, ymax),
      lwd=2,
      xaxt="n"
    )
    axis(
      side=1,
      at=1:6,
      labels=stringr::str_wrap(levels(topic_prevalence_by_year$era), width=15)
    )
    dev.off()
  }

  tmp <- labelTopics(m, n=150)
  summary_frex_file <- file.path(report_dir, "summary_frex.csv")
  summary_prob_file <- file.path(report_dir, "summary_prob.csv")
  make_summary_csv(m, summary_frex_file, type="frex")
  make_summary_csv(m, summary_prob_file, type="prob")

  # Write most representative abstract files
  repr <- stm::findThoughts(m, n=10)$index
  most_repr_dir <- file.path(report_dir, "Most Representative")
  if(!dir.exists(most_repr_dir)) dir.create(most_repr_dir)
  pm <- exp(m$beta[[1]][[1]])
  colnames(pm) <- m$vocab
  for(topic in 1:num_topics) {
    topic_frex <- get_frex(pm, topic)
    most_repr_topic_dir <- file.path(most_repr_dir, paste0("Topic_", topic))
    if(!dir.exists(most_repr_topic_dir)) dir.create(most_repr_topic_dir)
    if(!representative_docs) cat("Most representative documents\n=============================\n\n",
                                 file=file.path(most_repr_topic_dir, "titles.txt"))
    for(rank in 1:10) {
      idx <- repr[[topic]][rank]
      raw <- raw_texts[idx]
      if(nchar(raw) < 500) next
      ti <- stringr::str_replace(names(raw_texts)[idx], "pdf|\\.txt", "")
      ti <- paste0(rank, ".", ti)
      if(representative_docs) {
        cat(raw, file=file.path(most_repr_topic_dir, paste0(ti, ".txt")))
        print_color_coded_pdf(
          raw,
          topic_frex,
          filename=file.path(most_repr_topic_dir, paste0(ti, ".pdf"))
        )
      } else {
        cat(
          paste0(rank, ". ", ti, "\n"),
          file=file.path(most_repr_topic_dir, "titles.txt"),
          append=TRUE
        )
      }
    }
    rm(topic_frex)
  }
  rm(pm)
  rm(m)

}
