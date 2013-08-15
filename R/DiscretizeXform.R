# This file is part of the pmmlTransformations package 
#
# This part of the PMML Transformation package handles discretization 
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

	fieldsMap <- list(dataMatrix)
	suppressWarnings(newrow <- data.frame(type,dataType,I(origFieldName),sampleMin,sampleMax,xformedMin,xformedMax,centers,scales,I(fieldsMap),transform,default,missingValue,row.names=derivedFieldName,check.names=FALSE))

	suppressWarnings(newBoxData$fieldData <- rbind(newBoxData$fieldData,newrow))

	newcol <- NULL
	#for each row; ie piece of input data
	for(d in 1:nrow(newBoxData$data))
	{
	 data <- newBoxData$data[d,]
	 origName <- dataMatrix[1,1]
	 derivedName <- dataMatrix[1,2]

	# if data is missing, put in missing value replacement and go to next data row
	 if(is.na(data[origName]) || data[origName] == "")
	 {
          if(outDat == "numeric")
          {
           newcol <- rbind(newcol,as.numeric(missingValue))
          } else
          {
           newcol <- rbind(newcol,missingValue)
          }
	  next 
    	 }

	 match <- FALSE
	 partialmatch <- FALSE 
	 found <- FALSE
	 # for each field map row given except the top 2 (name and type)
         for( j in 3:nrow(dataMatrix))
         {
	   leftValue <- NULL
	   rightValue <- NULL
	   if(grepl(sep,dataMatrix[j,1]))
	   {
	    range <- strsplit(dataMatrix[j,1],sep)[[1]]

	    if(!(grepl("^\\[",range[1]) | grepl("^\\(",range[1])))
             stop("Left interval not given in proper format.")
	    if(!(grepl("\\]$",range[2]) | grepl("\\)$",range[2])))
             stop("Right interval not given in proper format.")
	    if(grepl("^\\[",range[1]))
	    {
	     leftValue <- gsub("\\[","",range[1])
	     if(data[origName] >= leftValue)
	     {
	      partialmatch <- TRUE
	     }
	    } else if(grepl("^\\(",range[1]))
	    {
	     leftValue <- gsub("\\(","",range[1])
             if(data[origName] > leftValue)
             {
              partialmatch <- TRUE
             }
	    } 

            if(partialmatch && grepl("\\]$",range[2]))
            {
	     rightValue <- gsub("\\]","",range[2])
             if(data[origName] <= rightValue)
             {
              match <- TRUE
             } 
            } else if(partialmatch && grepl("\\)$",range[2]))
            {
	     rightValue <- gsub("\\)","",range[2])
             if(data[origName] < rightValue)
             {
              match <- TRUE
             }
            }
# end if both left and right limits given
	   } else
	   {
	    range <- dataMatrix[j,1]
            if(grepl("^\\[",range[1]))
            {
	     leftValue <- gsub("\\[","",range[1])
             if(data[origName] >= leftValue)
             {
              match <- TRUE
             }
            } else if(grepl("^\\(",range[1]))
            {
	     leftValue <- gsub("\\(","",range[1])
             if(data[origName] > leftValue)
             {
              match <- TRUE
             }
            } else if(grepl("\\]$",range[1]))
            {
	     rightValue <- gsub("\\]","",range[1])
             if(data[origName] <= rightValue)
             {
              match <- TRUE
             }
            } else if(grepl("\\)$",range[1]))
            {
	     rightValue <- gsub("\\)","",range[1])
             if(data[origName] < rightValue)
             {
              match <- TRUE
             }
            } 
	   }
#end if only 1 limit given
	   if(match)
	   {
	    match <- FALSE
	    partialmatch <- FALSE
	    found <- TRUE
	    if(outDat == "numeric")
	    {
	     newcol <- rbind(newcol,as.numeric(dataMatrix[j,2]))
	    } else
	    {
	     newcol <- rbind(newcol,dataMatrix[j,2])
	    }
	   }
	  }
# end looping over all given limits
          # no match found
          if(!found && !is.na(default))
          {
           if(outDat == "numeric")
           {
            newcol <- rbind(newcol,as.numeric(default))
           } else
           {
            newcol <- rbind(newcol,default)
           }
          }
# end looping over all given data rows
         }

      colnames(newcol) <- dataMatrix[1,2]
      rownames(newcol) <- NULL

     newBoxData$data <- data.frame(newBoxData$data,newcol,check.names=FALSE)
#new
     newBoxData$matrixData <- cbind(newBoxData$matrixData,newcol)

     return(newBoxData)
}
