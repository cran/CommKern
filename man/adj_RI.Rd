% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Adj_RI.R
\name{adj_RI}
\alias{adj_RI}
\title{Adjusted Rand Index (ARI)}
\usage{
adj_RI(a, b)
}
\arguments{
\item{a}{a vector of classifications; this must be a vector of characters,
integers, numerics, or a factor, but not a list.}

\item{b}{a vector of classifications}
}
\value{
a scalar with the adjusted Rand Index (ARI)
}
\description{
Description of the adjusted Rand Index function.
}
\details{
In information theory, the Rand Index (also called the Rand Measure) is a
measure of the similarity between two data clusterings or classifications. If
N is the set of elements and X and Y are the partition of N into n subsets,
then the Rand Index is composed of four subsets: (a) the number of pairs of
elements in N that are in the same subset in in X and the same subset in Y;
(b) the number of pairs of elements in N that are in different subsets in X
and different subsets in Y; (c) the number of pairs of elements in N that
are in the same subset in X but different subsets in Y; and (d) the number of
pairs of elements in N that are in different subsets in X but the same subset
in Y. The adjusted Rand Index is the corrected-for-chance version of the Rand
Index, which establishes a baseline by using the expected similarity of all
pairwise comparisons between clusterings specified by a random model. The ARI
can yield negative results if the index is less than the expected index.
}
\examples{
set.seed(7)
x <- sample(x = rep(1:3, 4), 12)

set.seed(18)
y <- sample(x = rep(1:3, 4), 12)

adj_RI(x,y)

}
\seealso{
\code{\link{NMI}}, \code{\link{purity}}
}
