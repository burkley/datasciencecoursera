# Write a function named 'pollutantmean' that calculates the mean of a pollutant
# (sulfate or nitrate) across a specified list of monitors. The function
# 'pollutantmean' takes three arguments: 'directory', 'pollutant', and 'id'.
#
# Given a vector monitor ID numbers, 'pollutantmean' reads that monitors'
# particulate matter data from the directory specified in the 'directory'
# argument and returns the mean of the pollutant across all of the monitors,
# ignoring any missing values coded as NA.

pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        aggregate = numeric()
        for(i in id) {
                filename_function <- if(i < 10) {
                        make_filename_function(2)
                } else if (i < 100) {
                        make_filename_function(1)
                } else {
                        make_filename_function(0)
                }
                file_name <- paste(directory, "/", filename_function(i), ".csv", sep="")
                print(file_name)
                data_frame <- read.csv(file_name)
                n <- !is.na(data_frame[,pollutant])
                aggregate <- c(aggregate, data_frame[n,pollutant])
        }
        
        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)
        ## NOTE: Do not round the result!
        mean(aggregate)
}

# This function will create a function that will create a filename from an
# identifier.
# The function that is created will optionally prepend the identifier with 0's
# to match the filename patterns of the programming project.
make_filename_function <- function(num_chars_2_pad) {
        f <- function(n) {
                temp <- character(num_chars_2_pad+1)
                if(num_chars_2_pad>0) {
                        for(i in seq_along(1:num_chars_2_pad)) {
                                temp[i] <- "0"
                        }
                }
                temp[num_chars_2_pad+1] <- as.character(n)
                paste(temp, collapse="")
        }
        f
}
