# This file is part of the pmmlTransformations package 
#
# This part of the PMML Transformation package assigns dummy variables 
# to categorical values 
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

NormDiscreteXform <-
function(boxdata,inputVar,defaultValue=NA,mapMissingTo=NA,...)
{
	map <- NULL
	colmn <- NULL
	newrow <- NULL
	colnamesGiven <- FALSE
        j <- 0
	sampleMin <- NA
        sampleMax<- NA
        xformedMin <- NA
        xformedMax <- NA
        centers <- NA
        scales <- NA
	default <- NA
	missingValue <- NA
	newBoxData <- Initialize(boxdata)

	dots <- list(...)
	if(length(dots) != 0)
	{
		for(coln in dots)
		{
		  # initial code to discretize only certain indicated values of a categorical variable
		  # and give each dicretized value it's own name, if preferred
		  # now ignored as it was decided to just discretize all possible values
		  # and give them all pre-determined names  
		  if(FALSE)
		  {
			coln <- as.character(coln)
			if(grepl("[^-]->",coln))
			{
				st <- strsplit(coln,"->")
			} else
			{
		   		st <- strsplit(coln,"-->")
			}

			initName <- st[[1]][1]
			initName <- gsub("\\[","",initName)
			initName <- gsub("\\]","",initName)

			toName <- st[[1]][2]
                        toName <- gsub("\\[","",toName)
                        toName <- gsub("\\]","",toName)

			fromNames <- strsplit(initName,"categories=")[[1]]
			fromName <- fromNames[1]
			fromName <- gsub(",","",fromName)
			fromName <- gsub("\\[","",fromName)
                        fromName <- gsub("\\]","",fromName)

			origName <- fromName
                        if(grepl("column",origName,ignore.case=TRUE))
                        {
                               origName <- gsub("column","",origName,ignore.case=TRUE)
                        }
                        if(grepl("^[-,_]",origName))
                        {
                               origName <- gsub("^[-,_]*","",origName)
                        }
                        if(suppressWarnings(!is.na(as.numeric(origName))))
                        {
                         colmn <- as.numeric(origName)
                         fromName <- names(newBoxData$data)[colmn]
                        }

			catName <- fromNames[2]
			catName <- gsub("\\[","",catName)
                        catName <- gsub("\\]","",catName)
			catNames <- strsplit(catName,",")[[1]]
			ncats <- length(catNames)

			toNames <- strsplit(toName,",")[[1]]
			nto <- length(toNames)

			if(!is.null(catNames))
			{
			 if(nto < ncats)
			 {
			  for(i in (nto+1):ncats)
			  {
			   name <- paste("derived_",fromName,"_",catNames[i],sep="")
			   toNames <- c(toNames,name)
			  }
			 }
			} else
			{
			 levels <- unique(newBoxData$data[fromName])[[1]]
			 for(i in 1:length(levels))
			 {
			  catNames <- c(catNames,levels[i])
			  name <- paste("derived_",fromName,"_",levels[i],sep="")
                          toNames <- c(toNames,name)
			 }
			}
		  }

		}
	}

	if(!is.na(mapMissingTo))
	{
	  missingValue <- as.character(mapMissingTo)
	}
	if(!is.na(defaultValue))
	{
	  default <- as.character(defaultValue)
	}

	# expected input format: initialName or [initialName]
	input <- as.character(inputVar)
        fromName <- gsub("\\[","",input)
        fromName <- gsub("\\]","",fromName)
	fromName <- gsub("^[ ]*","",fromName)
        fromName <- gsub("[ $]*","",fromName)
	origName <- fromName
        if(grepl("column",origName,ignore.case=TRUE))
        {
         origName <- gsub("column","",origName,ignore.case=TRUE)
        }
        if(grepl("^[-,_]",origName))
        {
         origName <- gsub("^[-,_]*","",origName)
        }
        if(suppressWarnings(!is.na(as.numeric(origName))))
        {
         colmn <- as.numeric(origName)
         fromName <- names(newBoxData$data)[colmn]
        }

	catNames <- NULL
	toNames <- NULL
	levels <- unique(newBoxData$data[fromName])[[1]]
        for(i in 1:length(levels))
        {
         catNames <- c(catNames,as.character(levels[i]))
	 # name all derived fields as [original field name]_[category name] 
	 # R doesnt like special characters in names, such as '-' as it
	 # might be confused with subtraction. Replace such characters with '_'
         name <- paste(fromName,"_",levels[i],sep="")
	 name <- gsub("-","_",name)
	 name <- gsub("\\+","_",name)
	 name <- gsub("\\*","_",name)
	 name <- gsub(":","_",name)
	 name <- gsub("'","_",name)
         toNames <- c(toNames,name)
        }
	
	for(i in 1:length(catNames))
	{
	 type <- "derived"
	 dataType <- "numeric"
	 origFieldName <- fromName
	 derivedFieldName <- toNames[i] 
	 fieldsMap <- list(as.character(catNames[i]))
 
	 transform <- "NormDiscrete"
	 newrow <- data.frame(type,dataType,origFieldName,sampleMin,sampleMax,xformedMin,xformedMax,centers,scales,I(fieldsMap),transform,default,missingValue,row.names=derivedFieldName,check.names=FALSE)
	 suppressWarnings(newBoxData$fieldData <- rbind(newBoxData$fieldData,newrow))

	 newcol <- NULL
	 #for each row; ie piece of input data
       	 for(d in 1:nrow(newBoxData$data))
       	 {
       	  col <- NULL
       	  data <- newBoxData$data[d,]
       	  if(data[fromName] == catNames[i])
       	  {
       	   col <- cbind(col,1)
       	  } else
       	  {
       	   col <- cbind(col,0)
       	  }
       	  newcol <- rbind(newcol,col)
       	 }

       	 names <- toNames[i]
      	 colnames(newcol) <- names
	 rownames(newcol) <- NULL

     	 newBoxData$data <- data.frame(newBoxData$data,newcol,check.names=FALSE)
	}
        newBoxData$fieldData[nrow(newBoxData$fieldData),"missingValue"] <- missingValue
	newBoxData$fieldData[nrow(newBoxData$fieldData),"default"] <- default

     return(newBoxData)
}
