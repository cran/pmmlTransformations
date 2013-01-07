\name{ZScoreXform}
\alias{ZScoreXform}
\title{
Performs a z-score normalization on continuous values in accordance to the PMML element 'NormContinuous'
}
\description{
Performs a z-score normalization on data given in WrapData format
}
\usage{
ZScoreXform(boxdata, xformInfo=NA, defaultValue=NA, mapMissingTo=NA, ...)
}
\arguments{
  \item{boxdata}{wrapper object obtained by using the WrapData function on the raw data.}
  \item{xformInfo}{specification of details of the transformation.}
  \item{defaultValue}{
   the default value of the transformed variable, to be indicated in the PMML file
  }
  \item{mapMissingTo}{value to be given to the transformed variable if the value of the input variable is missing.} 
  \item{\dots}{
    further arguments passed to or from other methods. 
  }
}
\details{
 Given an input variable named 'InputVar', the name of the transformed variable 'OutputVar', and the 
desired value of the transformed variable if the input variable value is missing 'missingVal',
the ZScoreXform command including all the optional parameters is:

	xformInfo="InputVar -> OutputVar",defaultValue="defVal",mapMissingTo="missingVal"

 There are two methods in which the variables can be referred to. The first method is to use its 
column number; given the 'data' attribute of the 'boxdata' object, this would be the order at 
which the variable appears. This can be indicated in the format "column#". The second method is to 
refer to the variable by its name. \cr 
 The name of the transformed variable is optional; if the name is not provided, the transformed 
variable is given the name: "derived_" + \emph{original_variable_name} \cr 
 'missingValue', an optional parameter, is the value to be given to the output variable if the 
input variable value is missing.
  If no input variable names are provided, by default all numeric variables are transformed. Note 
that in this case a replacement value for missing input values cannot be specified.
 
}
\value{
R object containing the raw data, the transformed data and data statistics.
}
\references{
 Details on commonly used datasets, including the Iris data set: \url{http://www.dmg.org/pmml_examples}
}
\author{
Zementis, Inc.
}

\seealso{
\code{\link{WrapData}}.
}
\examples{
# load the standard iris dataset, already built into R
   data(iris)

# first wrap the data
   irisBox <- WrapData(iris)

# perform a z-transform on all numeric variables of the loaded iris dataset. These would be
# 'Sepal.Length','Sepal.Width','Petal.Length','Petal.Width'
# the 4 new derived variables will be named
# 'derived_Sepal.Length','derived_Sepal.Width','derived_Petal.Length','derived_Petal.Width'
   ZScoreXform(irisBox)

# perform a z-transform on the 1st column of the dataset (Sepal.Length)
# and give the derived variable the name 'dsl' 
   ZScoreXform(irisBox,xformInfo="column1 -> dsl")

# repeat the above operation; adding the new transformed variable to the irisBox object
   irisBox <- ZScoreXform(irisBox,xformInfo="column1 -> dsl")

# transform Sepal.Width(the 2nd column)
# the new transformed variable will be given the default name 'derived_Sepal.Width' 
   ZScoreXform(irisBox,xformInfo="column2")

# repeat the same operation as above, this time using the variable name
   ZScoreXform(irisBox,xformInfo="Sepal.Width")

# repeat the same operation as above, assign the transformed variable 'derived_Sepal.Width' 
# the value of 1.0 if the input value of the 'Sepal.Width' variable is missing. Add the new 
# information to the irisBox object 
   irisBox <- ZScoreXform(irisBox,xformInfo="Sepal.Width","mapMissingTo=1.0")

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
\keyword{utilities}
\keyword{methods}
