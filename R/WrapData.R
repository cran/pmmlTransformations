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
