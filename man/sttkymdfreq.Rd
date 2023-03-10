% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sttkymdfreq.R
\name{sttkymdfreq}
\alias{sttkymdfreq}
\title{Quantifies the Median Frequency of the Anteroposterior Displacement of COP.}
\usage{
sttkymdfreq(dados, sampfreq)
}
\arguments{
\item{dados}{data frame with two columns "x" and "y"}

\item{sampfreq}{number  The sampling frequency}
}
\value{
The median frequency of the y displacement for the given statokinesigram \code{FMy}.
}
\description{
Computes the median frequency of the anteroposterior displacement of the center of pressure.
}
\details{
'dados' is a data frame containing two columns named "x" and "y". The pairs (x, y) are the coordinates of the center of pressure acquired in a period of time.
}
\examples{
x <- c(1,3,7,5,9,4,3,6,8,2,8,9,4,5,7,3,4,7,9,3,2,5,3,4,8,2,9,7,4,2)
y <- c(6,3,9,1,3,7,4,9,6,1,7,3,9,7,2,6,3,4,8,1,9,3,6,8,1,6,2,9,8,3)

COP <- data.frame(x, y)

COPymdfreq <- sttkymdfreq(COP, 50)

}
\seealso{
\code{\link[stabilo]{sttkangle}}, \code{\link[stabilo]{sttkellipseplot}}
}
\author{
Jose Magalhaes de Oliveira
}
