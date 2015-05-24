columnmean <- function(x, removeNA = TRUE) {
	# x is a data.frame
	nc <- ncol(x) # number of columns
	means <- numeric(nc) # initialize a vector whose length is the number of columns
	for(i in 1:nc) {
		means[i] <- mean(x[,i], na.rm = removeNA)
	}
	means
}
