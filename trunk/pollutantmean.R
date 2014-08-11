pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
	
        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".

        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used

        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)

	data<-NULL
	monitor <- list()
	id<-as.character(id)
	id[nchar(id)==1]<-paste('00',id[nchar(id)==1],sep='')
	id[nchar(id)==2]<-paste('0',id[nchar(id)==2],sep='')
	for(i in seq_along(id)){
		monitor[[i]] <- read.table(paste(directory,'/',id[i],'.csv',sep=''),header=TRUE,sep=',')
		data <- rbind(data,monitor[[i]])
	}
	m <- mean(data[[pollutant]],na.rm=TRUE)
	m
}