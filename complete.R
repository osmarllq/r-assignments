complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases

	data<-data.frame(id=id,nobs=0)

	id<-as.character(id)
	id[nchar(id)==1]<-paste('00',id[nchar(id)==1],sep='')
	id[nchar(id)==2]<-paste('0',id[nchar(id)==2],sep='')

	
	temp<-NULL

	for(i in seq_along(id)){
		temp <- read.table(paste(directory,'/',id[i],'.csv',sep=''),header=TRUE,sep=',')
		data[i,2] <- table(complete.cases(temp))[2]
	}
	data
}