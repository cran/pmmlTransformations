RenameVar <-
function(boxdata,xformInfo=NA,...)
{

	i <- NULL
        j <- NULL
	colnm <- NULL 
 
	boxData <- Initialize(boxdata)

	if(is.na(xformInfo))
	{
	  warning("No field name to rename found")
	  return(boxdata)
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

          if(is.na(st[[1]][2]))
          {
            derivedFieldName <- paste("derived_",row.names(boxData$fieldData)[coln2],sep="")
          }

          # if column number, find the appropriate field 
	  if(suppressWarnings(!is.na(as.numeric(colnm))))
	  {
	    coln2 <- as.numeric(colnm)
	    dataType <- boxData$fieldData[names(boxData$data)[coln2],"dataType"]
	    if(dataType == "numeric")
	    {
	      row.names(boxData$fieldData)[coln2] <- derivedFieldName
	      names(boxData$data)[coln2] <- derivedFieldName
#new
              names(boxData$matrixData)[coln2] <- derivedFieldName
            }
         } else
	 {
	   i <- which(names(boxData$data) == colnm)
	   if(is.null(i))
	   {
	     j <- which(names(boxData$data) == colnm)
	   }

	   if(is.null(i) && is.null(j))
	   {
	     stop("field name not found.")
	   }
           if(is.null(j))
           {
            row.names(boxData$fieldData)[i] <- derivedFieldName
            names(boxData$data)[i] <- derivedFieldName
#new
            names(boxData$matrixData)[i] <- derivedFieldName
	   } else
	   {
            row.names(boxData$fieldData)[j] <- derivedFieldName
            names(boxData$data)[j] <- derivedFieldName
#new
	    names(boxData$matrixData)[j] <- derivedFieldName
	   }
	 }
       }
	
       return(boxData)
}
