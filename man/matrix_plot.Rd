% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/matrix_plot.R
\name{matrix_plot}
\alias{matrix_plot}
\title{Functional and Structural Matrix Plot}
\usage{
matrix_plot(x, ...)
}
\arguments{
\item{x}{a \code{spinglass_net} object}

\item{...}{additional arguments from other methods}
}
\value{
a \code{gtable} object
}
\description{
Provide a graphical representation of the functional and structural matrices
within a \code{spinglass_net} object.
}
\examples{

data(SBM_net)

matrix_plot(SBM_net)

}
