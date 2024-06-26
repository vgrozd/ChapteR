% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_delimiter.R
\name{add_delimiter}
\alias{add_delimiter}
\title{Add a delimiter comment}
\usage{
add_delimiter(
  colwidth = rstudioapi::readRStudioPreference("margin_column", NULL),
  delim = "-",
  verbose = FALSE
)
}
\arguments{
\item{colwidth}{Width of the delimiter comment, defaults to the current RStudio option}

\item{delim}{A character to repeat for the delimiter line, defaults to "-"}

\item{verbose}{Logical, print messages?}
}
\description{
Add a delimiter comment
}
