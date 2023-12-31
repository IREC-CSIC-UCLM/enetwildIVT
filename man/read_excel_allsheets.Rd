% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fun_data_processing.R
\name{read_excel_allsheets}
\alias{read_excel_allsheets}
\title{Read all sheets from an Excel file}
\usage{
read_excel_allsheets(filename, tibble = FALSE)
}
\arguments{
\item{filename}{The path to the Excel file.}

\item{tibble}{Logical indicating whether the output should be in the form of tibbles (default: \code{FALSE}).}
}
\value{
A list of data frames, with each element representing a sheet from the Excel file.
}
\description{
This function reads all sheets from an Excel file and returns them as a list of data frames.
}
\examples{
filename <- "path/to/excel/file.xlsx"
sheets <- read_excel_allsheets(filename)

}
