# This file is part of the pmmlTransformations package 
#
# This part of the PMML Transformation package 
# creates the inintial objevt on which all subsequent operations are performed 
#
# Time-stamp: <2013-06-05 19:48:25 Tridivesh Jena>
#
# Copyright (c) 2013 Zementis, Inc.
#
# The pmmlTransformations package is free: you can redistribute it and/or 
# modify it under the terms of the GNU General Public License as published 
# by the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# The pmmlTransformations package is distributed in the hope that it will 
# be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# To review the GNU General Public License see <http://www.gnu.org/licenses/>
############################################################################

WrapData <-
function(indata)
{
	dataBox <- NULL
	fieldNames <- NULL
	nrows <- NULL
	ncols <- NULL
	type <- NULL
	dataType <- NULL
	origFieldName <- NULL

	dataBox$data <- indata
	dataBox$nrows <- nrow(indata)
	dataBox$ncols <- ncol(indata)
     
        fieldNames <- names(indata)
	for(i in 1:dataBox$ncols)
	{
		origFieldName <- NA
		type[i] <- "original"
		dataType[i] <- class(indata[,i])
		if(dataType[i] == "integer")
		{
		 dataType[i] <- "numeric"
		}
	}

        # mark all original field names by type=original and origFieldName=NA 
        df<-data.frame(type,dataType,origFieldName,row.names=fieldNames)
        dataBox$fieldData <- df 

	return(dataBox)
}
