Initialize <- function(inbox)
{
        xformedMin <- NULL
        xformedMax <- NULL
        sampleMin <- NULL
        sampleMax <- NULL
        centers <- NULL
        scales <- NULL
        fieldsMap <- NULL
        transform <- NULL
	defaultValue <- NULL
	missingValue <- NULL

        if(is.null(inbox$fieldData[1,"defaultValue"]))
        {
                for(i in 1:nrow(inbox$fieldData))
                {
                        defaultValue <- c(defaultValue,NA)
                        missingValue <- c(missingValue,NA)
                }
                inbox$fieldData <- cbind(inbox$fieldData,defaultValue)
                inbox$fieldData <- cbind(inbox$fieldData,missingValue)
        }

        if(is.null(inbox$fieldData[1,"xformedMax"]))
        {
                for(i in 1:nrow(inbox$fieldData))
                {
                        xformedMin <- c(xformedMin,NA)
                        xformedMax <- c(xformedMax,NA)
                        sampleMin <- c(sampleMin,NA)
                        sampleMax <- c(sampleMax,NA)
                }
                inbox$fieldData <- cbind(inbox$fieldData,sampleMin)
                inbox$fieldData <- cbind(inbox$fieldData,sampleMax)
                inbox$fieldData <- cbind(inbox$fieldData,xformedMin)
                inbox$fieldData <- cbind(inbox$fieldData,xformedMax)
        }

        if(is.null(inbox$fieldData[1,"centers"]))
        {
                for(i in 1:nrow(inbox$fieldData))
                {
                        centers <- c(centers,NA)
                        scales <- c(scales,NA)
                }
                inbox$fieldData <- cbind(inbox$fieldData,centers)
                inbox$fieldData <- cbind(inbox$fieldData,scales)
        }

        if(is.null(inbox$fieldData[1,"fieldsMap"]))
        {
                for(i in 1:nrow(inbox$fieldData))
                {
                        fieldsMap <- c(fieldsMap,NA)
                }
                inbox$fieldData <- cbind(inbox$fieldData,fieldsMap)
        }

        if(is.null(inbox$fieldData[1,"transform"]))
        {
                for(i in 1:nrow(inbox$fieldData))
                {
                        transform <- c(transform,NA)
                }
                inbox$fieldData <- cbind(inbox$fieldData,transform)
        }

        return(inbox)
}
