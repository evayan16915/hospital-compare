## script of Return hospital name in that state with the given rank
## use data outcome-of-care-measures.csv
## "Sun Nov 01 21:18:35 2015"

rankhospital <- function(state, outcome, num) {
        ## Read outcome data
        if(!file.exists("progAssignment3")){
                dir.create("progAssignment3")
        }
        hospital <- read.csv("outcome-of-care-measures.csv",stringsAsFactors = FALSE)
        
        ## Check that state and outcome are valid
        validState <- c("AK","AL","AR","AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA","GU", "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MD", "ME", "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", "NH", "NJ", "NM", "NV",  "NY", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VA", "VI", "VT", "WA", "WI", "WV", "WY")
        validOutcome <- c("heart attack","heart failure","pneumonia")
        result_state <- length(grep(state, validState))
        result_outcome <- length(grep(outcome, validOutcome, fixed = TRUE))
        
        
        if(result_state == 0 )
                stop("invalid state") 
        else if(result_outcome == 0)
                stop("invalid outcome") 
        
        
        ## data clean: subset dataset/rename/classTransform/recode"not Avliable" to NA
        bestHospital <- hospital[c("Hospital.Name", "State", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")]
        names(bestHospital)[1] <- "Hospital"
        names(bestHospital)[2] <- "Location"
        names(bestHospital)[3] <- "heart_attack"
        names(bestHospital)[4] <- "heart_failure"
        names(bestHospital)[5] <- "pneumonia"
        
        bestHospital$heart_attack[bestHospital$heart_attack == "Not Available"] <- NA
        bestHospital$heart_failure[bestHospital$heart_failure == "Not Available"] <- NA
        bestHospital$pneumonia[bestHospital$pneumonia == "Not Available"] <- NA
        
        bestHospital$heart_attack <- as.numeric(bestHospital[[3]])
        bestHospital$heart_failure <- as.numeric(bestHospital[[4]])
        bestHospital$pneumonia <- as.numeric(bestHospital[[5]])
        
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        if(length(grep(outcome, "heart attack")) >0){
                
                ##subset and omit NA
                result <- subset(bestHospital, bestHospital$Location == state, select = Hospital:heart_attack )
                result <- na.omit(result)
                ##order and ranking 
                result <- result[order(result$heart_attack),]
                result$rank <- rank(result$heart_attack, ties.method = "min")
                
                ##pint
                max <- max(result$rank)
                min <- min(result$rank)
                if(num =="best") print(result[min,1])
                else if (num =="worst") print(result[max,1])
                else if (num) print(result[num,1])
                
        }  
        else if(length(grep(outcome, "heart failure"))>0){
                ##subset and omit NA
                result <- subset(bestHospital, bestHospital$Location == state, select =c("Hospital", "heart_failure"))
                result <- na.omit(result)
                ##order and ranking 
                result <- result[order(result$heart_failure),]
                result$rank <- rank(result$heart_failure, ties.method = "min")
                
                ##pint
                max <- max(result$rank)
                min <- min(result$rank)
                if(num =="best") print(result[min,1])
                else if (num =="worst") print(result[max,1])
                else if (num) print(result[num,1])
        }
        else if(length(grep(outcome, "pneumonia"))>0){
                ##subset and omit NA
                result <- subset(bestHospital, bestHospital$Location == state, select =c("Hospital", "pneumonia"))
                result <- na.omit(result)
                ##order and ranking 
                result <- result[order(result$pneumonia),]
                result$rank <- rank(result$pneumonia, ties.method = "min")
                
                ##pint
                max <- max(result$rank)
                min <- min(result$rank)
                if(num =="best") print(result[min,1])
                else if (num =="worst") print(result[max,1])
                else if (num) print(result[num,1])
        }
}