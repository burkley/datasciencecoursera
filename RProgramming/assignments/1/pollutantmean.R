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
        ## NOTE: Do not round the result!
        
        aggregate <- vector()
        for (i in id) {
                # first must create the filenames we need.  The "ids" that are passed in
                # to this function don't automatically translate to valid file names.
                # e.g. we may get an id = 1, but the file name is "001.csv"
                #
                # also must concatenate the directory with 1) the path seperator and 
                # 2) the file name to get a valid path to the file to be opened
                filename <- paste(directory, "/", sprintf("%03d", i), ".csv", sep="")
                #print(filename)
                
                # next we open each file with read.csv()
                my_dataframe <- read.csv(filename)
                #print(class(my_dataframe))
                
                # now that we have our dataframe, we must subset the column
                # indicated by the "pollutant" argument, which we know will be
                # either "sulfate" or "nitrate"
                #
                # we also need to omit the NAs
                my_pollutant <- my_dataframe[,pollutant]
                #print(my_pollutant)
                aggregate <- c(aggregate, my_pollutant)
        }
        mean(aggregate, na.rm=TRUE)
}


