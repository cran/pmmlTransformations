# PMML (Predictive Model Markup Language) Transformations 
#
# Copyright (c) 2013 Zementis, Inc.
#
# This file is part of the pmmlTransformations package 
#
# The pmmlTransformations package is free: you can redistribute it and/or 
# modify it under the terms of the GNU General Public License as published 
# by the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# The pmmlTransformations package is distributed in the hope that it will 
# be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. Please see the
# GNU General Public License for details (http://www.gnu.org/licenses/).
############################################################################
#
# Author: Tridivesh Jena
#
#---------------------------------------------------------------------------

DiscretizeXform <-
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
        fieldsMap <- NA
	dataMatrix <- NULL
	default <- NA
	missingValue <- NA
        sep <- ":"

	newBoxData <- Initialize(boxdata)
#	if(!is.numeric(newBoxData$data))
#	  stop("Non-numeric matrices not yet supported for transformations")
 
# EXTRACT DATA FROM xformInfo 
#[a->c][d->s]
        minf <- as.character(xformInfo)

        #separate variable names and data types 
        split0 <- strsplit(minf,"]\\[")[[1]]
        split0[1] <- gsub("\\[","",split0[1])

# is dataTypes given?
        given <- length(split0)

# discretize: a->c
# dataTypes: d->s
        if(given == 2)
        {
         discretize <- split0[1]
         split0[2] <- gsub("]","",split0[2])
         datType <- split0[2]
        } else
        {
         split0[1] <- gsub("]","",split0[1]) 
         discretize <- split0[1]
         datType <- NA
        }

# discretize: a->c
# split the variable and dataType strings
        if(grepl("[^-]->",discretize))
        {
                st <- "->"
        } else 
        {
                st <- "-->"
        }

        val <- strsplit(discretize,st)[[1]]

# inval: a
        inVal <- gsub(" ","",val[1])

# outVal: "c"
        outVal <- gsub(" ","",val[2])

# if data types provided
        inDat <- NULL
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
         inDat <- gsub(" ","",datsplit[1])

# outDat: "s"
         outDat <- gsub(" ","",datsplit[2])
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
	vname <- NULL	
        vdtype <- NULL
        for(i in 1:length(inVal))
        {
         vname <- c(vname,inVal)
         if(is.null(inDat))
         {
          vdtype <- c(vdtype,"numeric")
         } else
         {
          vdtype <- c(vdtype,inDat)
         }
        }
        vname <- c(vname,outVal)

        if(is.na(outDat))
        {
         vdtype <- c(vdtype,"string")
        } else
        {
          vdtype <- c(vdtype,outDat)
        }

# placeholder for interval and values
	vname <- c(vname,"leftInterval","leftValue","rightInterval","rightValue")
        vdtype <- c(vdtype,"string","double","string","double")

#EXTRACT DATA FROM table
# read interval data from csv file
	tabl <- as.character(table) 
        file <- scan(tabl,what=character(0),sep=",")
        ndat <- length(file)
        nrows <- length(scan(tabl,what=character(0),sep="\n"))
        numcols <- ndat/nrows
        dataMatrix <- matrix(file,nrow=nrows,byrow=TRUE)

# add columns for left/right intervalType/value
        dataMatrix <- cbind(dataMatrix,NA)
        dataMatrix <- cbind(dataMatrix,NA)
        dataMatrix <- cbind(dataMatrix,NA)
        dataMatrix <- cbind(dataMatrix,NA)

#defaultValue=f,mapMissingTo=g
	if(!is.na(defaultValue))
	{
	  default <- as.character(defaultValue)
          if(!((defaultValue==1) || (defaultValue==0) || toupper(defaultValue)==TRUE || toupper(defaultValue)==FALSE))
          {
            stop("defaultValue must be a proper boolean value")
          }
	}

	if(!is.na(mapMissingTo))
	{
	  missingValue <- as.character(mapMissingTo)
          if(!((missingValue==1) || (missingValue==0) || toupper(missingValue)==TRUE || toupper(missingValue)==FALSE))
          {
            stop("missingValue must be a proper boolean value")
          }
	}


# add variable info to the data matrix
	top <- rbind(vname,vdtype)
	dataMatrix <- rbind(top,dataMatrix)
	rownames(dataMatrix) <- NULL

	type <- "derived"
	origFieldName <- inVal

	if(is.null(outDat))
	{
	 dataType <- "string"
	} else
	{
	 dataType <- outDat
	}

	transform <- "discretize"
	derivedFieldName <- outVal

         # for each field map row given except the top 2 (name and type)
         for( j in 3:nrow(dataMatrix))
         {
           leftValue <- NA
           rightValue <- NA
           leftInterval <- NA
           rightInterval <- NA

           if(grepl(sep,dataMatrix[j,1]))
           {
            range <- strsplit(dataMatrix[j,1],sep)[[1]]

            if(grepl("^\\[",range[1]))
            {
             leftValue <- gsub("\\[","",range[1])
             leftInterval <- "closed"
            } else if(grepl("^\\(",range[1]))
            {
             leftValue <- gsub("\\(","",range[1])
             leftInterval <- "open"
            }

            if(grepl("\\]$",range[2]))
            {
             rightValue <- gsub("\\]","",range[2])
             rightInterval <- "Closed"
            } else if(grepl("\\)$",range[2]))
            {
             rightValue <- gsub("\\)","",range[2])
             rightInterval <- "Open"
            }
# end if both left and right limits given
           } else
           {
            range <- dataMatrix[j,1]
            if(grepl("^\\[",range[1]))
            {
             leftValue <- gsub("\\[","",range[1])
             leftInterval <- "closed"
             rightInterval <- "Open"
            } else if(grepl("^\\(",range[1]))
            {
             leftValue <- gsub("\\(","",range[1])
             leftInterval <- "open"
             rightInterval <- "Open"
            } else if(grepl("\\]$",range[1]))
            {
             rightValue <- gsub("\\]","",range[1])
             leftInterval <- "open"
             rightInterval <- "Closed"
            } else if(grepl("\\)$",range[1]))
            {
             rightValue <- gsub("\\)","",range[1])
             leftInterval <- "open"
             rightInterval <- "Open"
            }
           }

	   dataMatrix[j,3] <- leftInterval
	   dataMatrix[j,4] <- leftValue
           dataMatrix[j,5] <- rightInterval
           dataMatrix[j,6] <- rightValue
          }

#	  colnames(dataMatrix) <- dataMatrix[1,]

	fieldsMap <- list(dataMatrix)
	suppressWarnings(newrow <- data.frame(type,dataType,I(origFieldName),sampleMin,sampleMax,xformedMin,xformedMax,centers,scales,I(fieldsMap),transform,default,missingValue,row.names=derivedFieldName,check.names=FALSE))


	suppressWarnings(newBoxData$fieldData <- rbind(newBoxData$fieldData,newrow))

	colnames(dataMatrix) <- dataMatrix[1,]
        type <- NULL
        for(j in 3:nrow(dataMatrix))
	{
	  if(is.na(dataMatrix[j,"leftValue"]) && (dataMatrix[j,"rightInterval"] == "Closed"))
	  {
	    type[j] <- 1
	    next
	  }
          if(is.na(dataMatrix[j,"leftValue"]) && (dataMatrix[j,"rightInterval"] == "Open"))
          {
            type[j] <- 2
	    next
          }
          if((dataMatrix[j,"leftInterval"] == "closed") && is.na(dataMatrix[j,"rightValue"]))
          {
            type[j] <- 7
	    next
          }
          if((dataMatrix[j,"leftInterval"] == "open") && is.na(dataMatrix[j,"rightValue"]))
          {
            type[j] <- 8
	    next
          }
	  if((dataMatrix[j,"leftInterval"] == "closed") && (dataMatrix[j,"rightInterval"] == "Closed"))
          {
            type[j] <- 3
	    next
          }
          if((dataMatrix[j,"leftInterval"] == "closed") && (dataMatrix[j,"rightInterval"] == "Open"))
          {
            type[j] <- 4
	    next
          }
          if((dataMatrix[j,"leftInterval"] == "open") && (dataMatrix[j,"rightInterval"] == "Closed"))
          {
            type[j] <- 5
	    next
          }
          if((dataMatrix[j,"leftInterval"] == "open") && (dataMatrix[j,"rightInterval"] == "Open"))
          {
            type[j] <- 6
	    next
          }
	}

	newcol <- NULL
	origName <- dataMatrix[1,1]
	derivedName <- dataMatrix[1,2]

         if(!is.numeric(newBoxData$data[1,1]))
           stop("Non-numeric matrices not yet supported for transformations")

	# if data is missing, put in missing value replacement and go to next data row
	# if(is.na(data[origName]) || data[origName] == "")
	# {
        #  if(outDat == "numeric")
        #  {
        #   newcol <- rbind(newcol,as.numeric(missingValue))
        #  } else
        #  {
        #   newcol <- rbind(newcol,missingValue)
        #  }
	#  next 
    	# }

#	 if(!is.na(missingValue))
#	 {
#           if(outDat == "numeric")
#           {
#            newcol <- rep(as.numeric(missingValue),nrow(newBoxData$data))
#           } else
#           {
#            newcol <- rep(missingValue,nrow(newBoxData$data))
#           }
#	 } else
#	 {
#	  newcol <- rep(NA,nrow(newBoxData$data))
#	 }

# Tridi: 9/20/13: Decided that values should be initialized to default rather than missing
         if(!is.na(default))
         {
           if(outDat == "numeric")
           {
            newcol <- rep(as.numeric(default),nrow(newBoxData$data))
           } else
           {
            newcol <- rep(default,nrow(newBoxData$data))
           }
         } else
         {
          newcol <- rep(NA,nrow(newBoxData$data))
         }


	 # for each field map row given except the top 2 (name and type)
         for( j in 3:nrow(dataMatrix))
         {
#print("ROW BEGIN")
#print(proc.time())
          if(type[j] == 1)
          {
           newcol[newBoxData$data[,origName]<=dataMatrix[j,"rightValue"]] <- dataMatrix[j,derivedName] 
          }
          if(type[j] == 2)
          {
           newcol[newBoxData$data[,origName]<dataMatrix[j,"rightValue"]] <- dataMatrix[j,derivedName]                                                                 
          }
	  if(type[j] == 3)
	  {
	   newcol[(dataMatrix[j,"leftValue"]<=newBoxData$data[,origName]) & (newBoxData$data[,origFieldName]<=dataMatrix[j,"rightValue"])] <- dataMatrix[j,derivedName]
	  }
          if(type[j] == 4)
          {
           newcol[(dataMatrix[j,"leftValue"]<=newBoxData$data[,origName]) & (newBoxData$data[,origFieldName]<dataMatrix[j,"rightValue"])] <- dataMatrix[j,derivedName]
          }
          if(type[j] == 5)
          {
           newcol[(dataMatrix[j,"leftValue"]<newBoxData$data[,origName]) & (newBoxData$data[,origFieldName]<=dataMatrix[j,"rightValue"])] <- dataMatrix[j,derivedName]
          }
          if(type[j] == 6)
          {
           newcol[(dataMatrix[j,"leftValue"]<newBoxData$data[,origName]) & (newBoxData$data[,origFieldName]<dataMatrix[j,"rightValue"])] <- dataMatrix[j,derivedName]
          }
          if(type[j] == 7)
          {
           newcol[dataMatrix[j,"leftValue"]<=newBoxData$data[,origName]] <- dataMatrix[j,derivedName]
          }
          if(type[j] == 8)
          {
           newcol[dataMatrix[j,"leftValue"]<newBoxData$data[,origName]] <- dataMatrix[j,derivedName]
          }
#print("ROW END")
#print(proc.time())
	}

      col <- as.matrix(newcol)
      colnames(col) <- dataMatrix[1,2]
      rownames(col) <- NULL

     newBoxData$data <- data.frame(newBoxData$data,col,check.names=FALSE)

#new
     if(outDat == "numeric")
     {
      newBoxData$matrixData <- cbind(newBoxData$matrixData,as.numeric(col))
      colnames(newBoxData$matrixData) <- colnames(newBoxData$data)
     } else
     {
      newBoxData$matrixData <- cbind(newBoxData$matrixData,col)
     }

     return(newBoxData)
}
