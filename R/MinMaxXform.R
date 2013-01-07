MinMaxXform <-
function(boxdata,xformInfo=NA,defaultValue=NA,mapMissingTo=NA,...)
{
	colmn <- NULL
	newrow <- NULL
	center <- NULL
	scale <- NULL
	centers <- NA
        scales <- NA
	fieldsMap <- NA
	default <- NA
	missingValue <- NA
	colnamesGiven <- FALSE
        j <- 0

	newBoxData <- Initialize(boxdata)

	initLength <- nrow(newBoxData$fieldData)

        if(!is.na(mapMissingTo))
        {
           missingValue <- as.character(mapMissingTo)
        }
        if(!is.na(defaultValue))
        {
          default <- as.character(defaultValue)
        }

	if(is.na(xformInfo))
	{
	# if no arguments given, normalize all numeric fields between 0 and 1
		for(i in 1:newBoxData$ncols)
		{
			MIN <- 0
			MAX <- 1 
			name<-row.names(newBoxData$fieldData)[i]
			dataType <- newBoxData$fieldData[name,"dataType"]
			if(dataType == "numeric")
			{
				colmn <- cbind(colmn,newBoxData$data[,i])
				minimum <- min(newBoxData$data[,i])
				maximum <- max(newBoxData$data[,i])
				factor <- (MAX - MIN)/(maximum - minimum)
				st <- 1/factor
				dif <- maximum - (MAX/factor)
				center <- c(center,dif)
				scale <- c(scale,st)
				type<-"derived"
				dataType <- "numeric"
				origFieldName <- names(newBoxData$data)[i]
                                derivedFieldName <- paste("derived_",names(newBoxData$data)[i],sep="")
				xformedMin <- MIN
				xformedMax <- MAX
				sampleMin <- minimum
				sampleMax <- maximum
				transform <- "minmax"
                                newrow <- data.frame(type,dataType,origFieldName,sampleMin,sampleMax,xformedMin,xformedMax,centers,scales,fieldsMap,transform,default,missingValue,row.names=derivedFieldName)

                                newBoxData$fieldData <- rbind(newBoxData$fieldData,newrow)
			} 
		}
	} else
	{
		# default limits 
		MIN <- 0
		MAX <- 1

		coln <- as.character(xformInfo)
		# expected format: initName -> finalName[MIN,MAX]
		if(grepl("[^-]->",coln))
		{
			st <- strsplit(coln,"->")
		} else
		{
	   		st <- strsplit(coln,"-->")
		}

		# origName either column-number or field name
		origName <- st[[1]][1]
		st2 <- NA
		st3 <- ""
		if(!is.na(st[[1]][2]))
		{
			st2 <- strsplit(st[[1]][2],"\\[")
			finalName <- st2[[1]][1]
		}
		# finalName is name of derived field 
		
		# find MIN and MAX, if given 	
		if(!is.na(st2[[1]][2]))
		{
			st3 <- strsplit(st2[[1]][2],",")
			if(st3[[1]][1] != "")
			{
				MIN <- st3[[1]][1]
			}
		}

		endVal <- gsub("]","",st3[[1]][2])
		if(!is.na(endVal) && endVal != "")
		{
			MAX <- endVal
		}
		MIN <- as.numeric(MIN)
		MAX <- as.numeric(MAX)

		if(grepl("column",origName,ignore.case=TRUE))
		{
			origName <- gsub("column","",origName,ignore.case=TRUE)
		}
		if(grepl("^[-,_]",origName))
		{
			origName <- gsub("^[-,_]*","",origName)
		}
		colnm <- origName

		if(suppressWarnings(!is.na(as.numeric(colnm))))
		{
			coln2 <- as.numeric(colnm)
		#column number is numeric but data type of categorical below gives null
			dataType <- newBoxData$fieldData[names(newBoxData$data)[coln2],"dataType"]
			if(dataType == "numeric")
			{
				colmn <- cbind(colmn,newBoxData$data[,coln2])

				# if input was in the format: inintName -> [MIN,MAX]
                       		if(st2[[1]][1]=="" || is.na(st[[1]][2]))
                       		{
                               		finalName <- paste("derived_",row.names(newBoxData$fieldData)[coln2],sep="") 
                       		}

				minimum <- min(newBoxData$data[,coln2])
                               	maximum <- max(newBoxData$data[,coln2])

				# derive numbers so as to use the 'scale' function to normalize 
                               	factor <- (MAX - MIN)/(maximum - minimum)
                               	st <- 1/factor
                               	dif <- maximum - (MAX/factor)
                               	center <- c(center,dif)
                               	scale <- c(scale,st)
				type<-"derived"
				dataType <- "numeric"
				origFieldName <- row.names(newBoxData$fieldData)[coln2] 
       		                derivedFieldName <- finalName
				xformedMin <- MIN
                               	xformedMax <- MAX
                               	sampleMin <- minimum
                               	sampleMax <- maximum
				transform <- "minmax"
                               	newrow <- data.frame(type,dataType,origFieldName,sampleMin,sampleMax,xformedMin,xformedMax,centers,scales,fieldsMap,transform,default,missingValue,row.names=derivedFieldName)

                               	newBoxData$fieldData <- rbind(newBoxData$fieldData,newrow)
			}
		} else
		{
			i <- which(names(newBoxData$data) == colnm)
			dataType <- newBoxData$fieldData[names(newBoxData$data)[i],"dataType"]

			if(dataType == "numeric")
			{
				colmn <- cbind(colmn,newBoxData$data[,i])

				if(st2[[1]][1] == "" || is.na(st[[1]][2]))
                               	{
                                        finalName <- paste("derived_",names(newBoxData$data)[i],sep="")
                                }
				minimum <- min(newBoxData$data[,i])
     		                maximum <- max(newBoxData$data[,i])
                               	factor <- (MAX - MIN)/(maximum - minimum)
                               	st <- 1/factor
                               	dif <- maximum - (MAX/factor)
                               	center <- c(center,dif)
                               	scale <- c(scale,st)
				type <- "derived"
				dataType <- "numeric"
				origFieldName <- row.names(newBoxData$fieldData)[i] 
                                derivedFieldName <- finalName
				xformedMin <- MIN
	                        xformedMax <- MAX
                                sampleMin <- minimum
                                sampleMax <- maximum
				transform <- "minmax"
                                newrow <- data.frame(type,dataType,origFieldName,sampleMin,sampleMax,xformedMin,xformedMax,centers,scales,fieldsMap,transform,default,missingValue,row.names=derivedFieldName)

                                newBoxData$fieldData <- rbind(newBoxData$fieldData,newrow)

			}
		}
	}

	newBoxData$fieldData[nrow(newBoxData$fieldData),"missingValue"] <- missingValue
	newBoxData$fieldData[nrow(newBoxData$fieldData),"default"] <- default

	xformed <- scale(colmn,center,scale)


	begin <- initLength+1
	end <- nrow(newBoxData$fieldData)
	for(i in begin:end)
	{
		j <- j+1
		name <- row.names(newBoxData$fieldData)[i]
		newMatrix <- cbind(newBoxData$data,xformed[,j])
		newBoxData$data <- newMatrix
		colLength <- length(names(newBoxData$data))
		names(newBoxData$data)[i] <- name

		newBoxData$fieldData[i,"centers"] <- attributes(xformed)$"scaled:center"[j]
		newBoxData$fieldData[i,"scales"] <- attributes(xformed)$"scaled:scale"[j]
	} 

	return(newBoxData)
}