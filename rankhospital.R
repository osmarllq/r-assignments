rankhospital <- function(state, outcome, num = "best") {
## Read outcome data
data <- read.csv("outcome-of-care-measures.csv",na.strings="Not Available" )

## Check that state and outcome are valid

if(sum(data[['State']]==state)==0) stop('invalid state')

data <- data[data[['State']]==state,]

if(outcome=='heart attack'){ 
	x <- as.numeric(data[, 11]) #Heart Attack
	}else if(outcome=='heart failure'){
		x <- as.numeric(data[, 17]) #Heart Failure
		} else if(outcome=='pneumonia') {
			x <- as.numeric(data[, 23]) #Pneumonia
			} else {
				stop('invalid outcome')
				}

## Return hospital name in that state with the given rank
## 30-day death rate

rank <- data.frame(hospital=data[,'Hospital.Name'],rate=x)
rank[,1] <- as.character(rank[,1])

rank <- rank[order(rank[,2],rank[,1]),]


if(num=='worst') {
	z <- tail(rank[!(is.na(rank[,2])),1],1)
	} else if(num=='best') {
		z <- head(rank[!(is.na(rank[,2])),1],1)
		} else{
			z <- rank[num,1]
			}
return(z)

}

