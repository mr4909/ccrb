
# load necessary packages
requiredPackages = c('dplyr') # data manipulation
# only downloads packages if needed
for(p in requiredPackages){
  if(!require(p,character.only = TRUE)) install.packages(p)
  library(p,character.only = TRUE)
}

# import data
ccrb <- read.csv("Civilian_Complaint_Review_Board__CCRB__-_Allegations_Closed.csv")
# ccrb <- read.csv("Civilian_Complaint_Review_Board__CCRB__-_Complaints_Received.csv")

# remove missing values
ccrb <- na.omit(ccrb)

# remove missing values and cases outside of NYC
ccrb <- ccrb %>% filter(Borough.Of.Incident == "Bronx"|
                        Borough.Of.Incident == "Brooklyn"|
                        Borough.Of.Incident == "Manhattan"|
                        Borough.Of.Incident == "Queens"|
                        Borough.Of.Incident == "Staten Island")

# calculate the estimated date of complaint 
ccrb$Closed.Year <- as.character(ccrb$Closed.Year)
ccrb$Closed.Year <- as.Date(paste0(ccrb$Closed.Year, '-12-30'))
ccrb$Est.Complaint.Date <- ccrb$Closed.Year - as.difftime(ccrb$Days.From.Received.Date.To.Case.Closing, unit="days")
ccrb$Est.Complaint.Year <- format(as.Date(ccrb$Est.Complaint.Date, format="%Y%m%d"),"%Y")