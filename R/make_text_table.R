#' Make text table
#' 
#' Make a nicely-formatted plain text table.
#' @param col_list List of vectors containing the data for the columns of the table to be output.
#' @param headers Character vector of column headers.
#' @param which_pretty Integer vector containing column indices of columns to which \code{prettyNum} should be applied.
#' @param digits Integer vector giving the number of digits to be displayed after the decimal for each column. This value will be ignored for non-numeric columns.
#' @export

make_text_table <- function(col_list, headers, which_pretty=NULL, digits=rep(0, length(col_list))) {
    require(stringr)
    for(i in seq_along(col_list)) {
        x <- col_list[[i]]
        if(is.numeric(x)) {
            x <- round(x, digits[i])
        }
        if(i %in% which_pretty) {
            x <- prettyNum(x, big.mark=",")
        }
        pad <- max(c(nchar(x), nchar(headers[i])))
        x <- c(headers[i], str_dup("-", pad), as.character(x))
        x <- str_pad(x, width=pad)
        col_list[[i]] <- x
        rm(x, pad)
    }
    return(paste0(
        Reduce(function(x, y) paste(x, y, sep=str_dup(" ", 3)), col_list),
        collapse="\n"
    ))
}
