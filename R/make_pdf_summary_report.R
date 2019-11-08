#' Create Standard Summary Report for STM Results
#'
#' Create standard summary report for STM results.
#' @param stm_input_file Path to file containing objects used as input to the STM analysis.
#' @param stm_results_dir Directory containing results from STM analysis. The .Rdata file containing model output should have the same name as this directory.
#' @param report_title Title for the summary report. This is also used to name the output file, and thus should not contain special characters.
#' @param geogs Which geographies to include in the report.
#' @export

make_pdf_summary_report <- function(stm_input_file, stm_results_dir, report_title,
                                    geogs = c("EU", "UN", "US")) {

    attach(stm_input_file, name="full_reports")

    full_stm_input <- get("stm_input", as.environment("full_reports"))
    detach(full_reports)

    meta <- full_stm_input$meta

    meta[
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
    meta[ , geog := as.character(geog)]
    meta[geog=="INT", geog := "UN"]
    meta[ , geog := factor(geog)]

    full_report_meta <- meta

    if("report" %in% names(full_stm_input$meta)) {
        report_title_pattern <- "[A-Za-z]+?_\\d{4}[a-z]*"
        num_reports <- length(
            unique(
                stringr::str_extract(
                    full_report_meta$title,
                    report_title_pattern
                )
            )
        )
        full_report_meta[
            ,
            report_title := stringr::str_extract(
                full_report_meta$title,
                report_title_pattern
            )
        ]
        full_report_meta <- full_report_meta[
            ,
            .(year=year[1], geog=geog[1]),
            by=.(title=report_title)
        ]
    } else {
        num_reports <- nrow(full_report_meta)
    }

    ###########################################################################
    num_by_geog <- purrr::map_int(geogs, ~ sum(full_report_meta$geog == .))
    names(num_by_geog) <- geogs

    # num_eu <- sum(full_report_meta$geog=="EU")
    # num_un <- sum(full_report_meta$geog=="UN")
    # num_us <- sum(full_report_meta$geog=="US")
    ###########################################################################

    # Check if any eras have zero reports
    n_reports_by_era <- full_report_meta[ , .(n=.N), keyby=era]
    eras <- c("All Time Periods", as.character(n_reports_by_era$era)) # drops eras with zero reports


    # Next, make a table with a very brief topic label (or maybe just a number),
    # brief topic description, and list (in paragraph form, for space considerations)
    # of top 10 or so FREX words. Then, include either a bar chart with the very brief
    # labels, or just the topic prevalence graph created by stm. Then, include the
    # most frequent topics by geopolitical body and time period (either separately,
    # together, or both).

    results_file <- paste0(stm_results_dir, ".Rdata")
    load(file=file.path(stm_results_dir, results_file))
    stm_results <- get(stm_results_dir)

    n_topics <- ncol(stm_results$theta)
    topic_labels <- paste0("Topic_", 1:n_topics)

    # Most frequent topics by region and era
    by_region <- get_topic_prevalence_by_covariate(stm_results , "geog", meta)
    by_region[ , geog := as.character(geog)]
    by_region[geog=="INT", geog := "UN"]
    by_region[ , geog := factor(geog)]

    ###########################################################################
    topic_wts_by_geog <- purrr::map(
        geogs,
        function(g) {
            out <- as.numeric(by_region[geog==g, topic_labels, with=FALSE])
            names(out) <- topic_labels
            out
        }
    )
    names(topic_wts_by_geog) <- geogs

    # eu_topic_wts <- as.numeric(by_region[geog=="EU", topic_labels, with=FALSE])
    # names(eu_topic_wts) <- topic_labels
    # un_topic_wts <- as.numeric(by_region[geog=="UN", topic_labels, with=FALSE])
    # names(un_topic_wts) <- topic_labels
    # us_topic_wts <- as.numeric(by_region[geog=="US", topic_labels, with=FALSE])
    # names(us_topic_wts) <- topic_labels
    ###########################################################################

    ###########################################################################
    top_topics_by_geog <- purrr::map_int(
        topic_wts_by_geog,
        ~ which.max(.)
    )

    # top_eu_topic <- which.max(eu_topic_wts)
    # top_un_topic <- which.max(un_topic_wts)
    # top_us_topic <- which.max(us_topic_wts)
    ###########################################################################

    topics_by_docs <- data.table(stm_results$theta)
    setnames(topics_by_docs, names(topics_by_docs), topic_labels)
    overall_topic_wts <- apply(topics_by_docs, 2, mean)
    top_overall_topic <- which.max(overall_topic_wts)

    frex_file <- file.path(stm_results_dir, "summary_frex.csv")
    frex_words <- data.table(read.csv(file=frex_file, stringsAsFactors = FALSE))

    overall_most_prevalent_topic_top_100 <- frex_words[[top_overall_topic]][1:100]

    ###########################################################################
    top_topic_top_100_by_geog <- purrr::map(
        top_topics_by_geog,
        ~ frex_words[[.]][1:100]
    )

    # eu_top_topic_top_100 <- frex_words[[top_eu_topic]][1:100]
    # un_top_topic_top_100 <- frex_words[[top_un_topic]][1:100]
    # us_top_topic_top_100 <- frex_words[[top_us_topic]][1:100]
    ###########################################################################

    ###########################################################################
    top_topics <- c(top_overall_topic, top_topics_by_geog)

    # top_topics <- c(top_overall_topic, top_eu_topic, top_un_topic, top_us_topic)
    ###########################################################################

    topics_by_docs[ , era := meta$era]
    topics_by_docs[ , geog := meta$geog]

    by_era <- eval(
        parse(
            text=sprintf(
                "topics_by_docs[ , .(%s), by=era]",
                paste0(topic_labels, "=mean(", topic_labels, ")", collapse=", ")
            )
        )
    )

    setorder(by_era, era)

    era_matrix <- t(as.matrix(by_era[ , topic_labels, with=FALSE]))
    colnames(era_matrix) <- by_era$era
    top_by_era <- apply(era_matrix, 2, which.max)

    topics_by_docs[ , geog := as.character(geog)]
    topics_by_docs[geog=="INT", geog := "UN"]
    topics_by_docs[ , geog := as.factor(geog)]

    by_era_and_geog <- eval(
        parse(
            text=sprintf(
                "topics_by_docs[ , .(%s), by=.(era, geog)]",
                paste0(topic_labels, "=mean(", topic_labels, ")", collapse=", ")
            )
        )
    )


    all_geogs <- c("All Reports", geogs)


    era_and_geog_tab <- matrix(0, nrow=length(all_geogs), ncol=length(eras),
                               dimnames=list(all_geogs, eras))

    era_and_geog_tab["All Reports", 2:(1+length(top_by_era))] <- top_by_era
    era_and_geog_tab[ , "All Time Periods"] <- top_topics

    for(t in eras) {
        if(t=="All Time Periods") next
        for(i in seq_along(all_geogs)) {
            if(i==1) next
            tmp <- by_era_and_geog[era==t & geog==all_geogs[i], topic_labels, with=FALSE]
            era_and_geog_tab[all_geogs[i], t] <- if(nrow(tmp) == 1) {which.max(as.numeric(tmp))} else {NA}
        }
    }

    all_top_topics <- unique(as.integer(era_and_geog_tab))
    all_top_topics <- sort(all_top_topics[!is.na(all_top_topics)])


    all_top_frex <- as.matrix(frex_words)[1:100, ]
    colnames(all_top_frex) <- as.character(1:n_topics)
    rownames(all_top_frex) <- as.character(1:100)

    setorder(by_era_and_geog, geog, era)

    ###########################################################################
    wts_by_geog <- map(
        geogs,
        function(g) {
            wts <- matrix(
                0,
                nrow=n_topics,
                ncol=length(eras),
                dimnames=list(as.character(1:n_topics), eras)
            )
            wts[ , "All Time Periods"] <- topic_wts_by_geog[[g]]
            for(t in eras) {
                if(t=="All Time Periods") next
                tmp <- by_era_and_geog[era==t & geog==g, topic_labels, with=FALSE]
                wts[ , t] <- if(nrow(tmp)==1) {as.numeric(tmp)} else {NA}
            }
            wts
        }
    )
    names(wts_by_geog) <- geogs

    all_wts <- matrix(0, nrow=n_topics, ncol=length(geogs), dimnames=list(1:n_topics, geogs))
    for(g in geogs) {
        all_wts[ , g] <- wts_by_geog[[g]][ , 1]
    }


    # Tables 5-8

    # Top topic weights across time period, for all geogs
    all_geogs_mtrx <- matrix(0, nrow=n_topics, ncol=length(eras),
                             dimnames=list(as.character(1:n_topics), eras))

    all_geogs_mtrx[ , "All Time Periods"] <- overall_topic_wts
    all_geogs_mtrx[ , 2:(1+ncol(era_matrix))] <- era_matrix

    get_n_word_tables_and_topics_per_table <- function(n_topics) {
        max_topics_per_table <- 7
        if(n_topics <= max_topics_per_table) {
            return(list(n_word_tables=1, topics_per_table=n_topics))
        } else {

            n_tables_of_7 <- ceiling(n_topics/7)
            n_tables_of_6 <- ceiling(n_topics/6)
            n_tables_of_5 <- ceiling(n_topics/5)

            if(n_topics %in% c(8, 9, 10, 15)) {
                return(list(n_word_tables=n_tables_of_5,
                            topics_per_table=5))
            } else if(n_topics %in% c(11, 12, 16, 17, 18, 22, 23, 24)) {
                return(list(n_word_tables=n_tables_of_6,
                            topics_per_table=6))
            } else return(list(n_word_tables=n_tables_of_7,
                            topics_per_table=7))
        }
    }

    frex_specs <- get_n_word_tables_and_topics_per_table(n_topics)
    attach(frex_specs)

    # pandoc_cleaned_results_dir <- str_replace(file.path(getwd(), stm_results_dir),
                                              # "@", "\\\\@")
    results_dir <- stm_results_dir

    report_template <- paste(system.file(package="texanaaid"), "pdf_report_template.Rmd", sep="/")
    rmarkdown::render(report_template, output_format="pdf_document",
                      output_file=paste0(report_title, ".pdf"),
                      output_dir=results_dir, clean=TRUE)

    detach(frex_specs)
}
