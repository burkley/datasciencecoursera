corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
        
        ## Return a numeric vector of correlations
        ## NOTE: Do not round the result!
        correlations <- numeric()
        filenames <- list.files(directory, pattern="*.csv", full.names="TRUE")
        for(file in filenames) {
                data <- read.csv(file)
                cc <- complete.cases(data)
                complete_data <- data[cc,]
                num_cc <- nrow(complete_data)
                #print(sprintf("File %s, complete.cases %d", file, num_cc))
                if(num_cc > threshold) {
                        #print(sprintf("   Above threshold of %d", threshold))
                        #print(class(complete_data))
                        correlation <- cor(complete_data$sulfate, complete_data$nitrate)
                        correlations <- c(correlations, correlation)
                }
        }
        correlations
}
