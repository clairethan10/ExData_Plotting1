library(base)
library(dplyr)


rankhospital <- function(state, outcome) {
      
      ## Read outcome data
      dat <- read.csv(file = "rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv",  colClasses = "character")
      unq_states <- base::unique(dat$State)
      disease <- c("heart failure", "heart attack" , "pneumonia")
      
      if((state %in% unq_state) & (outcome %in% disease)){
            
            #subset the data with 5 variables of interset
            subdat <- dat %>% select(Hospital.Name, State, starts_with("Hospital.30.Day.Death"))
            colnames(subdat) <- c("HospitalName", "State", "heart_attack", "heart_failure", "pneumonia")
            
            #rename all outcomes to have same prefix 
            outcome_ <- sub(pattern=" ", replacement = "_", x=outcome)
            
            #convert data to numeric 
            subdat$heart_attack <- as.numeric(subdat$heart_attack)
            subdat$heart_failure <- as.numeric(subdat$heart_failure)
            subdat$pneumonia <- as.numeric(subdat$pneumonia)
            
            rankbystate <- subdat %>% group_by(State) 
                        %>% mutate(ranking = order(order(across(all_all(outcome_)), HospitalName)))
            
            
                        
            subdat2 %>% 
                  select(HospitalName, State, all_of(outcome_)) %>% # select all variables 
                  na.omit()  %>% # remove all NAs 
                  select(-State) %>% #supress state from display 
                  arrange(across(all_of(outcome_))) %>% #sort the outcome from lowest to hightest 
                  
      }
            
}
