\name{NormDiscreteXform}
\alias{NormDiscreteXform}
\title{
Normalize discrete values in accordance to the PMML element 'NormDiscrete'
}
\description{
Define a new derived variable for each possible value of a categorical variable. Given a categorical 
variable 'catVar' with possible discrete values 'A' and 'B', this will create 2 derived variables 
'catVar_A' and 'catVar_B'. If, for example, the input value of 'catVar' is 'A' then 'catVar_A' equals 1 and 
'catVar_B' equals 0.  
}
\usage{
NormDiscreteXform(boxdata, inputVar, defaultValue=NA, mapMissingTo=NA, ...)
}
\arguments{
  \item{boxdata}{
    the wrapper object obtained by using the WrapData function on the raw data.
   }
  \item{inputVar}{
   the input variable name in the data on which the transformation is to be applied 
  }
  \item{defaultValue}{
   the default value of the transformed variable, to be indicated in the PMML file
  }
  \item{mapMissingTo}{value to be given to the transformed variable if the value of the input variable is missing.}
  \item{\dots}{
    further arguments passed to or from other methods.
  }
}
\details{
 Given an input variable, 'InputVar' and 'missingVal', the desired value of the transformed variable 
if the input variable value is missing, the NormDiscreteXform command including all 
optional parameters is in the format:

   inputVar="input_variable",defaultValue="defVal",mapMissingTo="missingVal"

 There are two methods in which the input variable can be referred to. The first method is to use its
column number; given the 'data' attribute of the 'boxdata' object, this would be the order at
which the variable appears. This can be indicated in the format "column#". The second method is to refer 
to the variable by its name. \cr 
 The output of this transformation is a set of transformed variables, one for each possible value of 
the input variable. For example, given possible values of the input variable 'val1', 'val2', ... these 
transformed variables are by default named "InputVar_val1", "InputVar_val2", ...

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
  \code{\link{WrapData}}
}
\examples{
# load the standard iris dataset, already available in R
   data(iris)

# first wrap the data
   irisBox <- WrapData(iris)

# Discretize the "Species" variable. This will find all possible values of the 'Species' variable 
# and define new variables: 
# 	"Species_setosa" such that it is 1 if "Species" equals "setosa", else 0;
# 	"Species_versicolor" such that it is 1 if "Species" equals "versicolor", else 0;
# 	"Species_virginica" such that it is 1 if "Species" equals "virginica", else 0
  irisBox <- NormDiscreteXform(irisBox,inputVar="Species")

}
\keyword{ manip }
