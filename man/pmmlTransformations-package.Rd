\name{pmmlTransformations-package}
\alias{pmmlTransformations-package}
\alias{pmmlTransformations}
\docType{package}
\title{
Various data Transformation functions 
}
\description{
This package reads in raw data and allows the user to perform various transformations 
on the input data. The user can then use the derived data to construct models. A model 
constructed, together with the information about the transformation performed, can also 
be output in the PMML format, through the use of the \pkg{pmml} package.
}
\details{
The general methodology to use this package is to first wrap the data with the 'WrapData' function 
and then perform all desired transformations. The model, in PMML format, including the information 
on the transformations executed, can then be output by calling the 'pmml' function of the pmml package.More information about PMML can be found at http://www.dmg.org.
 
The 'pmml' function in this case has to be given an additional parameter, 'transform', as shown in the 
example below. 
}

\references{
  Alex Guazzelli, Wen-Ching Lin & Tridivesh Jena (2012),\emph{ PMML in Action: Unleashing the Power of 
Open Standards for Data Mining and Predictive Analytics} (Second Edition - Available on Amazon.com - 
\url{http://www.amazon.com/dp/1470003244}

  Data Mining Group home page: \url{http://www.dmg.org}

  Zementis PMML Resources page: \url{http://www.zementis.com/pmml.htm}

  PMML Transformations Generator: \url{http://www.zementis.com/PMMLTransformations/PMMLTransformations.html}
}
\keyword{ pmml }
\examples{
# load the standard iris dataset, already available in the base R package
   data(iris)

# First create the wrapper object
   irisBox <- WrapData(iris)

# perform a simple z-transformation on the first variable of the dataset, 'Sepal.Length'
# By default, the name of the transformed variable is 'derived_Sepal.Length'
# The information of the transformation is added back to the wrapped data object.
   irisBox <- ZScoreXform(irisBox,"1")

# Build a simple lm model 
   fit <- lm(Sepal.Width ~ derived_Sepal.Length + Petal.Length, data=irisBox$data)

# One may now output the model in PMML format using the command below. The PMML file will 
# now include the data transformations as well as the model. 
   library(pmml)
   fit_pmml <- pmml(fit, transform=irisBox)
}
