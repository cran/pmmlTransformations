ZScoreXform <-
function(boxdata,xformInfo=NA,mapMissingTo=NA,...)
{

	colmn <- NULL
	newrow <- NULL
	colnamesGiven <- FALSE
        j <- 0
	transform <- "zxform"
	centers <- NA
	scales <- NA
	xformedMin <- NA
        xformedMax <- NA
        sampleMin <- NA
        sampleMax <- NA
	fieldsMap <- NA 
	defaultValue <- NA
 	missingValue <- NA

        if(!is.na(mapMissingTo))
        {
          missingValue <- as.character(mapMissingTo)
        }

	newBoxData <- Initialize(boxdata)

	initLength <- nrow(newBoxData$fieldData)

	if(is.na(xformInfo))
	{
		missingValue <- NA
	# transform all numeric fields if no arguments given
		for(i in 1:newBoxData$ncols)
		{
			name<-names(newBoxData$data)[i]
			dataType <- newBoxData$fieldData[name,"dataType"]
			if(dataType == "numeric")
			{
				colmn <- cbind(colmn,newBoxData$data[,i])
				dataType <- "numeric"
				type <- "derived"
				transform <- "zxform"
				origFieldName <- names(newBoxData$data)[i]
                                derivedFieldName <- paste("derived_",names(newBoxData$data)[i],sep="")
                                newrow <- data.frame(type,dataType,origFieldName,sampleMin,sampleMax,xformedMin,xformedMax,centers,scales,fieldsMap,transform,defaultValue,missingValue,row.names=derivedFieldName)

				newBoxData$fieldData <- rbind(newBoxData$fieldData,newrow)
			} 
		}
	} else
	{
		# for each argument given
		coln <- as.character(xformInfo)
		# split to find initial and final names 	
		if(grepl("[^-]->",coln))
		{
			st <- strsplit(coln,"->")
		} else
		{
	   		st <- strsplit(coln,"-->")
		}
                st[[1]][1] <- gsub("^ *","",st[[1]][1])
                st[[1]][2] <- gsub(" *$","",st[[1]][2])
                if(!is.na(st[[1]][2]))
                {
                        derivedFieldName <- st[[1]][2]
                }
	   	colnm <- st[[1]][1]
		if(grepl("column",colnm,ignore.case=TRUE))
		{
			colnm <- gsub("column","",colnm,ignore.case=TRUE)
		}
		if(grepl("^[-,_]",colnm))
		{
			colnm <- gsub("^[-,_]*","",colnm)
		}

		# if column number, find the appropriate field 
		if(suppressWarnings(!is.na(as.numeric(colnm))))
		{
			coln2 <- as.numeric(colnm)
			dataType <- newBoxData$fieldData[names(newBoxData$data)[coln2],"dataType"]
			if(dataType == "numeric")
			{
				if(is.na(st[[1]][2]))
	                        {
					derivedFieldName <- paste("derived_",row.names(newBoxData$fieldData)[coln2],sep="")
				}
				colmn <- cbind(colmn,newBoxData$data[,coln2])
				dataType <- "numeric"
                               	type <- "derived"
				origFieldName <- row.names(newBoxData$fieldData)[coln2]
                               	newrow <- data.frame(type,dataType,origFieldName,sampleMin,sampleMax,xformedMin,xformedMax,centers,scales,fieldsMap,transform,defaultValue,missingValue,row.names=derivedFieldName)

                               	newBoxData$fieldData <- rbind(newBoxData$fieldData,newrow)
			}
		} else
		{
			i <- which(names(newBoxData$data) == colnm)
			dataType <- newBoxData$fieldData[names(newBoxData$data)[i],"dataType"]
			if(dataType == "numeric")
			{
				if(is.na(st[[1]][2]))
                               	{
                               		derivedFieldName <- paste("derived_",row.names(newBoxData$fieldData)[i],sep="")
                               	}
				colmn <- cbind(colmn,newBoxData$data[,i])
				dataType <- "numeric"
				type <- "derived"
				origFieldName <- row.names(newBoxData$fieldData)[i]

				transform <- "zxform"
                               	newrow <- data.frame(type,dataType,origFieldName,sampleMin,sampleMax,xformedMin,xformedMax,centers,scales,fieldsMap,transform,defaultValue,missingValue,row.names=derivedFieldName)
                               	newBoxData$fieldData <- rbind(newBoxData$fieldData,newrow)
			}
		}
	}
	newBoxData$fieldData[nrow(newBoxData$fieldData),"missingValue"] <- missingValue

	# use scale function 
	xformed <- scale(colmn,T,T)

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
