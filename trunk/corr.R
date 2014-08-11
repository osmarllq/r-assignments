corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0

        ## Return a numeric vector of correlations

	#source('complete.R')

	temp <- complete(directory)
	id <- temp[(temp[,2]>threshold & !is.na(temp[,2])),1]
	
	id<-as.character(id)
	id[nchar(id)==1]<-paste('00',id[nchar(id)==1],sep='')
	id[nchar(id)==2]<-paste('0',id[nchar(id)==2],sep='')

	corr <- vector(mode='numeric',length=length(id))

	temp<-NULL
 	
	for(i in seq_along(id)){
			temp <- read.table(paste(directory,'/',id[i],'.csv',sep=''),header=TRUE,sep=',')
			corr[i] <- cor(temp[complete.cases(temp),c('sulfate','nitrate')])[1,2]
	}
	corr
}