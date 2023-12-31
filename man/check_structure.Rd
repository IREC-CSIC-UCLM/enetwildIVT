% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fun_validation.R
\name{check_structure}
\alias{check_structure}
\title{Check Data Structure}
\usage{
check_structure(df, vocabulary)
}
\arguments{
\item{df}{The input data frame.}

\item{vocabulary}{The vocabulary containing the structure requirements.}
}
\value{
TRUE if the data structure is valid, or an error message indicating the missing rows or columns.
}
\description{
This function checks the structure of a data frame against a given vocabulary.
It verifies the presence of mandatory rows and columns in the data frame.
If any required rows or columns are missing, it returns an error message.
If the data frame structure is valid, it returns TRUE.
}
\examples{
df <- data.frame(
  A = c(1, 2, 3),
  B = c("x", "y", "z")
)
vocabulary <- list(
  structure = list(
    mandatoryRows = c("row1", "row2"),
    mandatoryCols = c("A", "C")
  )
)
result <- check_structure(df, vocabulary)
if (isTRUE(result)) {
  # Data structure is valid
  print("Data structure is valid.")
} else {
  # Missing rows or columns
  print(result)
}

}
