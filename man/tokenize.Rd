% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tokenize.R
\name{tokenize}
\alias{tokenize}
\title{Tokenize}
\usage{
tokenize(x)
}
\arguments{
\item{x}{An object inheriting from Corpus, a list of character vectors, or a character vector.}
}
\value{
A list of character vectors of the tokens (words) in each document.
}
\description{
This function takes a Corpus or vector of documents and returns a list of character vectors of the tokens (words) in each document.
}
\examples{
docs <- c("What are you doing down there, crashy?", "I'm real good at lying to Lois.")
words <- tokenize(docs)
}
\keyword{text}
