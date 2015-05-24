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

        id_v <- character(length(id))
        nobs_v <- integer(length(id))
        for(i in 1:length(id)) {
                file_name <- paste(directory, "/", sprintf("%03d",id[i]), ".csv", sep="")
                #print(file_name)
                data <- read.csv(file_name)
                cc <- complete.cases(data)
                #print(cc)
                #print(nrow(data[cc,]))
                id_v[i] <- id[i]
                nobs_v[i] <- nrow(data[cc,])
        }
        my_result <- data.frame(id_v, nobs_v)
        colnames(my_result) <- c("id", "nobs")
        my_result
}
