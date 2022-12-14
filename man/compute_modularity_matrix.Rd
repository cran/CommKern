% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compute_mod_matrix.R
\name{compute_modularity_matrix}
\alias{compute_modularity_matrix}
\title{Compute modularity matrix}
\usage{
compute_modularity_matrix(net)
}
\arguments{
\item{net}{a \code{spinglass_net} object (see \code{\link{matrix_to_df}} for
more details)}
}
\value{
mod_matrix
}
\description{
Description of the compute modularity matrix function.
}
\details{
Calculates the modularity matrix, which is the difference between the
observed adjacency matrix and the expected adjacency matrix (from a null
model). This is only computed for the main component of network information,
not accounting for the guidance. For neuroimaging application, this function
would be computing the modularity matrix for the functional connectivity
aspect of the network object. The function takes in a network object and
returns the modularity matrix.
}
\seealso{
\code{\link{matrix_to_df}}
}
