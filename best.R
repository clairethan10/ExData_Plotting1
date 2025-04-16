
## assignment 3 Coursera 
# install.packages("dplyr")

library(dplyr) # reading in dplyr package 
library(base) #reading in base package 

best <- function(state, outcome) {
      # read the data in 
      dat <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
      
      #list of outcome 
      disease <- c("heart attack", "heart failure", "pneumonia") # get a list of unique disease to examine
      unq_state <- base::unique(dat$State)  #get a list of unique state to examine 
      
      #list of state 
      
      if((state %in% unq_state) & (outcome %in% disease)) {
            
            #read in only var of interest 
            dat_sub <- dat %>% select(Hospital.Name, State, starts_with("Hospital.30.Day.Death"))
            
            #label the columns
            colnames(dat_sub) <- c("HospitalName", "hospitalstate", "heart_attack", "heart_failure", "pneumonia")
            
            #convert data to numeric 
            
            dat_sub$heartattack <- as.numeric(dat_sub$heartattack) 
            dat_sub$heartfailure <- as.numeric(dat_sub$heartfailure)
            dat_sub$pneumonia <- as.numeric(dat_sub$pneumonia)
            
            #rename all outcomes to have same prefix 
            outcome_ <- sub(pattern=" ", replacement = "_", x=outcome)
            
            dat_sub %>% 
                  select(HospitalName, hospitalstate, all_of(outcome_)) %>% #select only three variables from the sub data 
                  filter(hospitalstate==state) %>% #filtering the data for selected state 
                  na.omit() %>%  #remove any NAs 
                  select(-hospitalstate) %>%  #hiding hospital state from display 
                  arrange(across(all_of(outcome_)), HospitalName) %>%  # sorting the outcomes from lowest to highest and then by hospital name
                  head(1) %>% #selecting only the first row 
                  select(HospitalName) %>% #printing only the hospital name 
                  as.character() -> message 
}

# if something is wrong with this output 

if(!(state %in% unq_state) & (outcome %in% disease)) {
      return(base::cat(base::sprintf('Error in best("%s", "%s"): invalid state', state, outcome)))
}

if ((state %in% unq_state) & !(outcome %in% disease)) {
      return(base::cat(base::sprintf('Error in best("%s", "%s"): invalid state', state, outcome)))
}
if (!(state %in% unq_state) & !(outcome %in% disease)) {
      return(base::cat(base::sprintf('Error in best("%s", "%s"): invalid state', state, outcome)))
}     
return(message)
      
}
