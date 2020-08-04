##############################
# Author: Mari Roberts
# Date: 7/24/2020
##############################

# load necessary packages
requiredPackages = c('dplyr',
                     'readr',
                     'stringr')
# only downloads packages if needed
for(p in requiredPackages){
  if(!require(p,character.only = TRUE)) install.packages(p)
  library(p,character.only = TRUE)
}

###############
# import data
###############
ccrb <- read_csv("Civilian_Complaint_Review_Board__CCRB__-_Allegations_Closed.csv",na = c("", "NA", "*", "**"))
# ccrb <- read.csv("Civilian_Complaint_Review_Board__CCRB__-_Complaints_Received.csv")

# case outcomes
ccrb.outcomes <- read_csv("Civilian_Complaint_Review_Board__CCRB__-_Complaints_Closed.csv",na = c("", "NA", "*", "**"))

###############
# clean data
###############

# remove missing values
ccrb <- na.omit(ccrb)

# summarise data
lapply(ccrb, table)

# remove white space in variable name
names(ccrb)<-str_replace_all(names(ccrb), c(" " = "." , "," = "" ))

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
ccrb$Closed.Year <- format(as.Date(ccrb$Est.Complaint.Date, format="%Y%m%d"),"%Y")

# change OMN to Other
ccrb <- ccrb %>%
  mutate(Allegation.FADO.Type = case_when((Allegation.FADO.Type=="Abuse of Authority") ~ "Abuse of Authority",
                                          (Allegation.FADO.Type=="Discourtesy") ~ "Discourtesy",
                                          (Allegation.FADO.Type=="Force") ~ "Force",
                                          (Allegation.FADO.Type=="Offensive Language") ~ "Offensive Language",
                                          (Allegation.FADO.Type=="OMN") ~ "Other"))

# clean outcomes
ccrb <- ccrb %>% filter(Allegation.Disposition != "P E N D I N G")
ccrb <- ccrb %>% 
  mutate(Allegation.Disposition.Type = 
           case_when(Allegation.Disposition=="Alleged Victim Unavailable" | 
                     Allegation.Disposition=="Alleged Victim Uncooperative" ~ "Alleged Victim Unavailable/Uncooperative",
                     Allegation.Disposition=="Complainant Unavailable" | 
                     Allegation.Disposition=="Complainant Uncooperative" ~ "Complainant Unavailable/Uncooperative",
                     Allegation.Disposition=="Substantiated (Charges)" | 
                     Allegation.Disposition=="Substantiated (Command Discipline A)" |   
                     Allegation.Disposition=="Substantiated (Command Discipline B)" | 
                     Allegation.Disposition=="Substantiated (Command Lvl Instructions)" |
                     Allegation.Disposition== "Substantiated (Formalized Training)" ~ "Substantiated",
                     Allegation.Disposition=="Administratively Closed" ~ "Administratively Closed",
                     Allegation.Disposition=="Closed - Pending Litigation" ~ "Closed - Pending Litigation",
                     Allegation.Disposition=="Complaint Withdrawn" ~ "Complaint Withdrawn",
                     Allegation.Disposition=="Exonerated" ~ "Exonerated",
                     Allegation.Disposition=="Mediated" ~ "Mediated",
                     Allegation.Disposition=="Mediation Attempted" ~ "Mediation Attempted",
                     Allegation.Disposition=="Miscellaneous" ~ "Miscellaneous",
                     Allegation.Disposition=="Officer(s) Unidentified" ~ "Officer(s) Unidentified",
                     Allegation.Disposition=="Other Misconduct" ~ "Other Misconduct",
                     Allegation.Disposition=="Unfounded" ~ "Unfounded",
                     Allegation.Disposition=="Unsubstantiated" ~ "Unsubstantiated",
                     Allegation.Disposition=="Victim Unidentified" ~ "Victim Unidentified",
                     Allegation.Disposition=="Miscellaneous - Subject Resigned" ~ "Officer Resigned",
                     Allegation.Disposition=="Miscellaneous - Subject Retired" ~ "Officer Retired",
                     Allegation.Disposition=="Miscellaneous - Subject Terminated" ~ "Officer Terminated"))


# create binary variable for force
ccrb <- ccrb %>% mutate(force_used = if_else(Allegation.FADO.Type == "Force",1,0))
ccrb$force_used <- factor(ccrb$force_used)

# remove cases prior to 2012
ccrb <- ccrb %>% filter(Closed.Year >= 2012)

# subset 2017 data (most recent year available)
ccrb.2017 <- ccrb %>% filter(Closed.Year == 2017)