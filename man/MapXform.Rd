\name{MapXform}
\alias{MapXform}
\title{
Implements a map between discrete values in accordance to the PMML element 'MapValues'
}
\description{
Maps discrete values of an input variable to a discrete value of the transformed variable.
The map is given in an external table file referred to in the transform command.
}
\usage{
MapXform(boxdata, xformInfo, table, defaultValue=NA, mapMissingTo=NA, ...)
}
\arguments{
  \item{boxdata}{
   the wrapper object obtained by using the WrapData function on the raw data. 
  }
  \item{xformInfo}{specification of details of the transformation.}
  \item{table}{name of external CSV file containing the map from input to output values.}
  \item{defaultValue}{value to be given to the transformed variable if the input values do not match any given map.}
  \item{mapMissingTo}{value to be given to the transformed variable if the value of the input variable is missing.}
  \item{\dots}{
    further arguments passed to or from other methods.
  }
}

\details{
 Given a map from the combination of variables 'InVar1', 'InVar2', ... to the transformed variable 
'OutVar', where the variables have the data types 'InType1', 'InType2', ... and 'OutType', the 
 map command is in the format:

   xformInfo="[InVar1,InVar2,...->OutVar][InType1,InType2,...->OutType]",table="TableFileName",  
defaultValue="defVal",mapMissingTo="missingVal"

where 'TableFileName' is the name of the CSV file containing the map. The map can be a N to 1 map where
N is greater or equal to 1. The data types of the variables can be any of the ones defined in the PMML 
format including integer, double or string. 'defVal' is the default value of the transformed variable
and if any of the map input values are missing, 'missingVal' is the value of the transformed variable. 

 While the table file is required, the arguments 'InType', 'OutType', 'defaultValue' and 'mapMissingTo' 
are optional. The CSV file containing the table should not have any row and column identifiers, and the 
values given must be in the same order as in the map command. If the data types of the variables are not 
given, the data types of the input variables are attempted to be determined from the 'boxdata' argument. 
If that is not possible, the data type is assumed to be "string". 
}
\value{
R object containing the raw data, the transformed data and data statistics.
}
\references{
Details on commonly used datasets, including the Audit data set: \url{http://www.dmg.org/pmml_examples}
}

\seealso{
 \code{\link{WrapData}},
 \code{pmml}
}
\examples{
\dontrun{
# load the standard audit dataset, part of the base rattle package
   library(rattle)
   data(audit)

# first wrap the data
   auditBox <- WrapData(audit)

# One of the variables, 'Gender', has 2 possible values: 'Male' and 'Female'. 
# If these string values have to be mapped to a numeric value, a file has to be
# created, say 'MapGender.csv' whose content is, for example,:
#
#  Male,1
#  Female,2
#
# Transform the variable 'Gender' to a variable 'd_gender' such that:
#    if Gender = "Male" then d_gender = "1"
#    if Gender = "Female" then d_gender = "0"
#
# Give 'd_gender' the value 0 if the input variable value is missing.
   auditBox <- MapXform(auditBox,xformInfo="[Gender->d_gender][string->integer]", 
	table="MapGender.csv",mapMissingTo="0")
}

}
\keyword{ manip }
