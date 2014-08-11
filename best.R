best <- function(state, outcome) {
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

## Return hospital name in that state with lowest 30-day death
## rate

y <- min(x,na.rm=TRUE)
z <- as.character(data[,'Hospital.Name'][!is.na(x==y) & x==y])

if(length(z)==1){
	return(z)
	} else {
		order(z)
		return(z[1])
		}
}
