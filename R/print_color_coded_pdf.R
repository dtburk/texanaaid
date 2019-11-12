#' Save a color-coded PDF
#'
#' Write text to a PDF with words color-coded according to FREX scores.
#' @param raw_text A character vector containing the text in human-readable format (with punctuation, case, etc)
#' @param frex A character vector of words sorted in decreasing order of FREX score.
#' @param filename Filepath to which output should be saved.
#' @param stemmed Are the words in \code{frex} stemmed?
#' @param colors One of \code{c("heat", "greyscale")} indicating color scheme for color coding.
#' @param pctile_cuts An integer of length 3 containing three numbers between 1 and 99 indicating the percentile cut-points for color-coding.
#' @export

print_color_coded_pdf <- function(raw_text, frex, filename, stemmed=TRUE, colors=c("greyscale", "heat"),
                                  pctile_cuts=c(95, 98, 99)) {
    # Convert frex to a named integer
    # Default cuts:
    ## 1 = 0-94th pctile
    ## 2 = 95-97th pctile
    ## 3 = 98-98th pctile
    ## 4 = 99th pctile and above
    pctiles <- seq(100, 0, length.out=length(frex))
    adj_quartiles <- as.integer(cut(pctiles, breaks=c(0, pctile_cuts, 100),
                         right=FALSE, include.lowest=TRUE))
    names(adj_quartiles) <- frex

    # Convert raw text to list of lines, each line being a character
    # vector of words
    doc <- str_split(str_wrap(raw_text, 70), "\n")[[1]]
    doc <- str_split(doc, " ")

    n_lines <- length(doc)

    col_opts <- if(match.arg(colors)=="heat") {
        rev(heat.colors(4))
    } else rev(grey.colors(4, start=0, end=0.95, gamma=4))

    process <- function(word) {
        out <- stringr::str_to_lower(remove_punctuation(word))
        if(stemmed) return(SnowballC::wordStem(out))
        else return(out)
    }

    pdf(file=filename, width=8.5, height=1+n_lines*.25)
    par(mar=c(2, 1, 2, 1))
    plot.new()
    plot.window(xlim=c(0, 80), ylim=c(1, n_lines))
    for(i in 1:n_lines) {
        plot_x <- 0
        for(j in 1:length(doc[[i]])) {
            word <- doc[[i]][j]
            if(process(word) %in% names(adj_quartiles)) w_scr <- adj_quartiles[process(word)]
            else w_scr <- 1
            text(x=plot_x, y=n_lines+1-i, labels=paste0(word, " "), pos=4, family="mono",
                 col=col_opts[w_scr])
            plot_x <- plot_x + strwidth(paste0(word, " "), family="mono")
        }
    }
    dev.off()
}
