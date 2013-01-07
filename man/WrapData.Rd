\name{WrapData}
\alias{WrapData}
\title{
Wrap raw data in an R object
}
\description{
Wrap raw data read in a R object. This object is then read in by the transform
functions and the data in it is transformed.
}
\usage{
WrapData(indata)
}
\arguments{
  \item{indata}{ the raw data set.}
}
\details{
   Object consists of the data itself and various properties for each data variable. This information 
is then used by the transformation functions to calculate the derived variable values.  
}
\value{
   An R object containing information on the data to be transformed.
}
\references{
  PMML page describing various possible data transformations: \url{http://www.dmg.org/v4-1/Transformations.html} 
}
\author{
Zementis, Inc. 
}

\seealso{
  \code{\link[pmml]{pmml}}
}
\examples{
# load the standard iris dataset, already built into R
   data(iris)

# make a wrapper object for the iris dataset to use with transformation functions
   irisBox <- WrapData(iris)

}
