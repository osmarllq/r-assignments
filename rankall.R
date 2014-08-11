rankall <- function(outcome, num = "best") {
## Read outcome data

data <- read.csv("outcome-of-care-measures.csv",na.strings="Not Available" )

## Check that state and outcome are valid

if(outcome=='heart attack'){ 
	x <- data[, 11] #Heart Attack
	}else if(outcome=='heart failure'){
		x <- data[, 17] #Heart Failure
		} else if(outcome=='pneumonia') {
			x <- data[, 23] #Pneumonia
			} else {
				stop('invalid outcome')
				}

## For each state, find the hospital of the given rank
rank <- data.frame(hospital=data[,'Hospital.Name'],
		state=data[,'State'],rate=x)
rank[,1] <- as.character(rank[,1])
rank[,2] <- as.character(rank[,2])

rank <- rank[order(rank[,3],rank[,2],rank[,1]),]

if(num=='worst') {
	z <- tapply(rank[!is.na(rank[,3]),1],rank[!is.na(rank[,3]),2],function(x) tail(x,1))
	} else if(num=='best') {
		z <- tapply(rank[!is.na(rank[,3]),1],rank[!is.na(rank[,3]),2],function(x) head(x,1))
		} else{
			z <- tapply(rank[!is.na(rank[,3]),1],rank[!is.na(rank[,3]),2],function(x) x[num])
			}

## Return a data frame with the hospital names and the
## (abbreviated) state name
y<-data.frame(hospital=as.vector(z),state=names(z))
return(y)
}

head(rankall("heart attack", 20), 10)
tail(rankall("pneumonia", "worst"), 3)
tail(rankall("heart failure"), 10)
