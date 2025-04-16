library(base)
library(dplyr)

rankall <- function(outcome, num="best") {
  
  dat <- read.csv("rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv", colClasses="character")
  unq_states <- base::unique(dat$State)
  disease <- c("heart_failure", "heart_disease", "pneumonia")
  
  # subset the data containing the columns of interest
  sub_dat <- dat %>%
    select(Hospital.Name, State, starts_with("Hospital.30.Day.Death"))

  colnames(sub_dat) <- c("HospitalName", "State", "heart_attack", "heart_failure", "pneumonia")
  
  # convert the disease outcome to numeric
  sub_dat$heart_attack <- as.numeric(sub_dat$heart_attack)
  sub_dat$heart_failure <- as.numeric(sub_dat$heart_failure)
  sub_dat$pneumonia <- as.numeric(sub_dat$pneumonia)
  
  outcome_ = sub(pattern=" ", replacement="_", x=outcome)
  
  if (outcome %in% disease) {
    
    # subset the data based on outcome input
    sub_dat2 <- sub_dat %>%
      group_by(State) %>%
      arrange(across(all_of(outcome_)), HospitalName) %>%
      mutate(ranking = row_number())

    if (is.character(num)) {
      if (num == "best") {
        sub_dat2 %>%
          filter(!is.na(across(all_of(outcome_)))) %>%
          filter(ranking == 1) %>%
          select(HospitalName, State) %>%
          arrange( HospitalName, State) -> output
        return(output)
        
      } else if (num == "worst") {
        sub_dat2 %>%
          filter(!is.na(across(all_of(outcome_)))) %>%
          filter(ranking == max(ranking, na.rm=TRUE)) %>%
          select(HospitalName, State) %>%
          arrange(HospitalName, State) -> output
        return(output)
      } else {
        return(cat("Please check your num input"))
      }
    }
    
    if (is.numeric(num) & length(num) >= 1) {
      if (num >= 1 & num <= nrow(sub_dat2)) {
        sub_dat2 %>%
          filter(ranking == num) %>%
          select(HospitalName, State) %>%
          arrange( HospitalName, State ) -> output
        
        merge(x=sub_dat %>% select(State) %>% unique() %>% arrange(State), y=output, all=TRUE) -> output
        return(output)
      } else {
        return(NA)
      }
    }
    
  } else if (!(outcome %in% disease)) {
    return(base::cat(base::sprintf('Error in best("%s", num="%s"): invalid outcome', outcome, num)))
  }
  
}

