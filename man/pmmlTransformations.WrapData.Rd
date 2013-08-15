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

   Object consists of the data itself and various properties for each data variable. If there is not
   enough information about the data, they are given default values; the data is assumed to be the 
   original data of data type string. The variable names are assumed to be \bold{X1}, \bold{X2}, ... 
   This information is then used by the transformation functions to calculate the derived variable 
   values. 
  
}
\value{
   An R object containing information on the data to be transformed.
}

\author{
Tridivesh Jena, Zementis, Inc. 
}

\seealso{
  \code{\link[pmml]{pmml}}
}
\examples{
# Load the standard iris dataset, already built into R
   data(iris)

# Make a wrapper object for the iris dataset to use with transformation functions
   irisBox <- WrapData(iris)

# Output only the transformations in PMML format. 
# This example will output just an empty "LocalTransformations" element as no transformations
# were performed.
   library(pmml)
   pmml(NULL,transforms=irisBox)

}
