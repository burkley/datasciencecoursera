best <- function(state, outcome) {
        
        valid_state <- list(c("heart attack", "heart failure", "pneumonia"),
                            c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
                              "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
                              "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"))
        if(!outcome %in% valid_state[[1]]) {
                stop("invalid outcome")
        }
        if(!state %in% state.abb) {
                stop("invalid state")
        }
        mortality <- valid_state[[2]][match(outcome, valid_state[[1]])]
        #data <- read.csv("outcome-of-care-measures.csv",
        #                 colClasses = c(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack="factor",
        #                                Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure="factor",
        #                                Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia="factor"),
        #                 na.strings="Not Available")
        data <- read.csv("outcome-of-care-measures.csv", stringsAsFactors=FALSE, na.strings="Not Available")
        logical_vector <- data$State == state
        state_data <- data[logical_vector, c("Hospital.Name", "State", mortality)]
        
        if(outcome == "heart attack") {
                ordered_state_data <- state_data[order(state_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, state_data$Hospital.Name), ]
        } else if(outcome == "heart failure") {
                ordered_state_data <- state_data[order(state_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, state_data$Hospital.Name), ]
        } else if(outcome == "pneumonia") {
                ordered_state_data <- state_data[order(state_data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, state_data$Hospital.Name), ]
        } else { # a little future proofing
                stop("invalid outcome")
        }
        ordered_state_data[1, "Hospital.Name"]
        #ordered_state_data
}
