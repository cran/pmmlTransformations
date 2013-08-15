\name{DiscretizeXform}
\alias{DiscretizeXform}
\title{
Discretizes a continuous variable to a discrete one as indicated by interval mappings.
This is in accordance to the PMML element:\cr \bold{Discretize}
}
\description{
Creates a discrete variable from a continuous one. The discrete variable value depends on which interval
the continuous variable value lies in. The mapping from intervals to discrete values is given 
in an external table file referred to in the transform command.
}
\usage{
DiscretizeXform(boxdata, xformInfo, table, defaultValue=NA, mapMissingTo=NA, ...)
}
\arguments{
  \item{boxdata}{
   the wrapper object obtained by using the WrapData function on the raw data. 
  }
  \item{xformInfo}{specification of details of the transformation.}
  \item{table}{name of external CSV file containing the map from input to output values.}
  \item{defaultValue}{value to be given to the transformed variable if the value of the input variable 
	does not lie in any of the defined intervals.}
  \item{mapMissingTo}{value to be given to the transformed variable if the value of the input variable is missing.}
  \item{\dots}{
    further arguments passed to or from other methods.
  }
}

\details{
 Given a list of intervals and the discrete value each interval is linked to, a discrete variable is defined 
with the value indicated by the interval where it lies in. If a continuous variable \bold{InVar} of data type 
\bold{InType} is to be converted to a variable \bold{OutVar} of data type \bold{OutType}, the transformation command 
is in the format:

   xformInfo = "[InVar->OutVar][InType->OutType]", table="TableFileName", \cr
   defaultValue="defVal", mapMissingTo="missingVal"

where \bold{TableFileName} is the name of the CSV file containing the interval to discrete value map. 
The data types of the variables can be any of the ones defined in the PMML format including integer, 
double or string. \bold{defVal} is the default value of the transformed variable and if any of the input
values are missing, \bold{missingVal} is the value of the transformed variable. 

While TableFileName is required, the arguments InType, OutType, defaultValue and mapMissingTo 
are optional. The CSV file containing the table should not have any row and column identifiers, and the 
values given must be in the same order as in the map command. If the data types of the variables are not 
given, the data types of the input variables are attempted to be determined from the \bold{boxData} argument. 
If that is not possible, the data types are assumed to be string.

Intervals are either given by the left or right limits, in which case the other limit is considered as 
infinite. It may also be given by both the left and right limits separated by the character ":". An example 
of how intervals should be defined in the external file are:


   rightVal1),outVal1 \cr 
   rightVal2],outVal2 \cr
   [leftVal1:rightVal3),outVal3 \cr
   (leftVal2:rightVal4],outVal4 \cr
   (leftVal,outVal5

which, given an input value \bold{inVal} and the output value to be calculated \bold{out}, means that:


	if(inVal <  rightVal1) out=outVal1 \cr
	if(inVal <= rightVal2) out=outVal2 \cr
	if( (inVal >= leftVal1) and (inVal < rightVal3) ) out=outVal3 \cr
	if( (inVal >  leftVal2) and (inVal <= rightVal4) ) out=outVal4 \cr
	if(inVal > leftVal) out=outVal5 \cr
 
}
\value{
R object containing the raw data, the transformed data and data statistics.
}

\author{
Tridivesh Jena, Zementis, Inc.
}

\seealso{
 \code{\link{WrapData}},
 \code{\link[pmml]{pmml}}
}
\examples{
\dontrun{
# Load the pmmlTransformations package
   library(pmmlTransformations)

# First wrap the data
   irisBox <- WrapData(iris)

# We wish to convert the continuous variable "Sepal.Length" to a discrete variable "dsl"
# The intervals to be used for this transformation is given in a file, "intervals.csv"
# whose content is, for example,:
#
#  5],val1
#  (5:6],22
#  (6,val2
#
# This will be used to create a disctere variable named "dsl" of dataType "string" such that:
#    if(Sepal.length <= 5) then dsl = "val1"  
#    if((Sepal.Lenght > 5) and (Sepal.Length <= 6)) then dsl = "22"
#    if(Sepal.Length > 6) then dsl = "val2" 
#
# Give "dsl" the value 0 if the input variable value is missing.
   irisBox <- DiscretizeXform(irisBox,xformInfo="[Sepal.Length -> dsl][double -> string]", 
	table="intervals.csv",mapMissingTo="0")
}

}
\keyword{ manip }
