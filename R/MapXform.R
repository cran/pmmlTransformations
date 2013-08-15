# This file is part of the pmmlTransformations package 
#
# This part of the PMML Transformation package handles Mapping
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

MapXform <-
function(boxdata,xformInfo,table,defaultValue=NA,mapMissingTo=NA,...)
{
	newrow <- NULL
	colnamesGiven <- FALSE
        j <- 0
	origFieldName <- NULL
	derivedFieldName <- NA
        sampleMin <- NA
        sampleMax<- NA
        xformedMin <- NA
        xformedMax <- NA
        centers <- NA
        scales <- NA
	missingValue <- NA
	dataMatrix <- NULL
	default <- NA

	newBoxData <- Initialize(boxdata)

# EXTRACT DATA FROM xformInfo 
#[a,b->c][d,d->s]
        minf <- as.character(xformInfo)

        #separate variable names and data types 
        split0 <- strsplit(minf,"]\\[")[[1]]
        split0[1] <- gsub("\\[","",split0[1])

# is dataTypes given?
        given <- length(split0)

# mapVal: a,b->c
# datType: d,d->s
        if(given == 2)
        {
         mapVal <- split0[1]
         split0[2] <- gsub("]","",split0[2])
         datType <- split0[2]
        } else
        {
         split0[1] <- gsub("]","",split0[1]) 
         mapVal <- split0[1]
         datType <- NA
        }

# mapVal: a,b-> c
        mapVal <- gsub("^[ ]*","",mapVal)
        mapVal <- gsub("[ $]*","",mapVal)

# split the variable and dataType strings
        if(grepl("[^-]->",mapVal))
        {
                st <- "->"
        } else
        {
                st <- "-->"
        }

        val <- strsplit(mapVal,st)[[1]]

# inval: a,b
        inVal <- val[1]
        valsplit <- strsplit(inVal,",")[[1]]

# inVals: "a" "b"
        inVals <- valsplit
        for(i in 1:length(inVals))
        {
         inVals[i] <- gsub("^[ ]*","",inVals[i])
         inVals[i] <- gsub("[ $]*","",inVals[i])
        }

# outVal: "c"
        outVal <- val[2]
        outVal <- gsub("^[ ]*","",outVal)
        outVal <- gsub("[ $]*","",outVal)

# if data types provided
        inDats <- NULL
        outDat <- NULL
        if(!is.na(datType))
        {
         if(grepl("[^-]->",datType))
         {
                 st <- "->"
         } else
         {
                 st <- "-->"
         }
         datsplit <- strsplit(datType,st)[[1]]
         inDat <- datsplit[1]

# inDats: "d" "d"
         inDats <- strsplit(inDat,",")[[1]]
         for(i in 1:length(inDats))
         {
          inDats[i] <- gsub("^[ ]*","",inDats[i])
          inDats[i] <- gsub("[ $]*","",inDats[i])
         }

# outDat: "s"
         outDat <- datsplit[2]
         outDat <- gsub("^[ ]*","",outDat)
         outDat <- gsub("[ $]*","",outDat)
        }

# convert double, integer to numeric for fieldData
        if(!is.null(outDat))
        {
         if((outDat == "double") || (outDat == "integer"))
         {
          outDat <- "numeric"
         }
        } else
        {
          outDat <- "string"
        }

# make variable name and data type rows to bind on data map later
        vnames <- NULL
        vdtype <- NULL
        for(i in 1:length(inVals))
        {
         vnames <- c(vnames,inVals[i])
         if(is.null(inDats[i]))
         {
          vdtype <- c(vdtype,"string")
         } else
         {
          vdtype <- c(vdtype,inDats[i])
         }
        }
        vnames <- c(vnames,outVal)

        if(is.na(outDat))
        {
         vdtype <- c(vdtype,"string")
        } else
        {
          vdtype <- c(vdtype,outDat)
        }


#EXTRACT DATA FROM table
# read data from csv file
	tabl <- as.character(table) 
        file <- scan(tabl,what=character(0),sep=",")
        ndat <- length(file)
        nrows <- length(scan(tabl,what=character(0),sep="\n"))
        numcols <- ndat/nrows
        dataMatrix <- matrix(file,nrow=nrows,byrow=TRUE)


#defaultValue=f,mapMissingTo=g
	if(!is.na(defaultValue))
	{
	  default <- as.character(defaultValue)
	}
	if(!is.na(mapMissingTo))
	{
	  missingValue <- as.character(mapMissingTo)
	}

# add variable info to the data matrix
	top <- rbind(vnames,vdtype)
	dataMatrix <- rbind(top,dataMatrix)
	rownames(dataMatrix) <- NULL

	type <- "derived"
	oFN <- NULL
	for(i in 1:length(inVals))
	{
	 oFN <- c(oFN,inVals[i])
	}
	origFieldName <- list(oFN)

	if(is.null(outDat))
	{
	 dataType <- "string"
	} else
	{
	 dataType <- outDat
	}
	fieldsMap <- list(dataMatrix) 
	transform <- "MapValues"
	derivedFieldName <- outVal

	suppressWarnings(newrow <- data.frame(type,dataType,I(origFieldName),sampleMin,sampleMax,xformedMin,xformedMax,centers,scales,I(fieldsMap),transform,default,missingValue,row.names=derivedFieldName,check.names=FALSE))

	suppressWarnings(newBoxData$fieldData <- rbind(newBoxData$fieldData,newrow))

        end <- nrow(newBoxData$fieldData)
	info <- newBoxData$fieldData[end,"fieldsMap"][[1]]

	newcol <- NULL
#new
	newmatrixcol <- NULL
	#for each row; ie piece of input data
	for(d in 1:nrow(newBoxData$data))
	{
	 data <- newBoxData$data[d,]

	# if data is missing, put in missing value replacement and go to next data row
	 if(is.na(data) || data == "")
	 {
          if(outDat == "numeric")
          {
           newcol <- rbind(newcol,as.numeric(missingValue))
	   newmatrixcol <- rbind(newmatrixcol,as.numeric(missingValue))
          } else
          {
           newcol <- rbind(newcol,missingValue)
#new
	   newmatrixcol <- rbind(newmatrixcol,missingValue)
          }
	  break 
    	 }

	 # for each mapvalue row given except the top 2 (var name and dataType)
         for( j in 3:nrow(dataMatrix))
         {
	  match <- FALSE

	  # for each input variable column except the last output variable
          for(k in 1:(ncol(dataMatrix)-1))
          {
	   if(data[dataMatrix[1,k]] == dataMatrix[j,k])
	   {
	    match <- TRUE
	   } else
	   {
	    match <- FALSE
	    break
	   }
          }
	# all input column values that in data
	  if(match)
	  {
	   if(outDat == "numeric")
	   {
	    newcol <- rbind(newcol,as.numeric(dataMatrix[j,ncol(dataMatrix)]))
#new
	    newmatrixcol <- rbind(newmatrixcol,as.numeric(dataMatrix[j,ncol(dataMatrix)]))
	   } else
	   {
	    newcol <- rbind(newcol,dataMatrix[j,ncol(dataMatrix)])
#new
	    newmatrixcol <- rbind(newmatrixcol,dataMatrix[j,ncol(dataMatrix)])
	   }
	   break
	  }
         }

	# no match found
	 if(!match && !is.na(default))
	 {
	  if(outDat == "numeric")
	  {
	   newcol <- rbind(newcol,as.numeric(default))
#new
	   newmatrixcol <- rbind(newmatrixcol,as.numeric(default))
	  } else
	  {
	   newcol <- rbind(newcol,default)
#new
	   newmatrixcol <- rbind(newmatrixcol,default)
	  }
	 }
        }

      colnames(newcol) <- dataMatrix[1,ncol(dataMatrix)]
      rownames(newcol) <- NULL

     colnames(newmatrixcol) <- colnames(newcol)

     newBoxData$data <- data.frame(newBoxData$data,newcol,check.names=FALSE)
#new
     newBoxData$matrixData <- cbind(newBoxData$matrixData,newmatrixcol)

     return(newBoxData)
}
