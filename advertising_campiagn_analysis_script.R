library(dplyr)
library(stargazer)

# Read data
abdn = read.csv("C:/Users/mudas/OneDrive/Desktop/QMB/midterm project/Abandoned.csv", header = TRUE, na.strings = "")
rser = read.csv("C:/Users/mudas/OneDrive/Desktop/QMB/midterm project/Reservation.csv", header = TRUE, na.strings = "")

#Check for missing values
sum(is.na(abdn))
sum(is.na(rser))

#Check for Duplicates
sum(duplicated(abdn))
sum(duplicated(abdn$Caller_ID))

sum(duplicated(rser))
sum(duplicated(rser$Caller_ID))


#test/control division analyses

sum(abdn$Test_Control == "test" )
sum(abdn$Test_Control == "control")

sum(rser$Test_Control == "test" )
sum(rser$Test_Control == "control")

#segment by available state

if_state <- abdn[complete.cases(abdn['Address']),]
table(if_state$Test_Control)

summary_stats = abdn %>%
  group_by(Address, Test_Control) %>%
  summarize(Count = n())

print(summary_stats)

summary_stats_rser = rser %>%
  group_by(Address, Test_Control) %>%
  summarize(Count = n())

print(summary_stats_rser)
#The division for the Abandoned data set is relatively equal on the total as well as State level.
#The division for the Reserved data set is NOT equal on the total as well as State level.




# Matching based on different keys and create logical vectors for each condition


emailmatch = abdn$Email[complete.cases(abdn$Email)] %in% rser$Email[complete.cases(rser$Email)]
incom_match = abdn$Incoming_Phone[complete.cases(abdn$Incoming_Phone)] %in% rser$Incoming_Phone[complete.cases(rser$Incoming_Phone)]

contactmatch = abdn$Contact_Phone[complete.cases(abdn$Contact_Phone)] %in% rser$Contact_Phone[complete.cases(rser$Contact_Phone)]

incom_contact_match = abdn$Incoming_Phone[complete.cases(abdn$Incoming_Phone)] %in% rser$Contact_Phone[complete.cases(rser$Contact_Phone)]

contact_incom_match = abdn$Contact_Phone[complete.cases(abdn$Contact_Phone)] %in% rser$Incoming_Phone[complete.cases(rser$Incoming_Phone)]

# Create flags for matches

abdn$emailmatch = 0
abdn$emailmatch[complete.cases(abdn$Email)] = 1 * emailmatch

abdn$incom_match = 0
abdn$incom_match[complete.cases(abdn$Incoming_Phone)] = 1 * incom_match

abdn$contactmatch= 0
abdn$contactmatch[complete.cases(abdn$Contact_Phone)] = 1 * contactmatch

abdn$incom_contact_match= 0
abdn$incom_contact_match[complete.cases(abdn$Incoming_Phone)] = 1 * incom_contact_match

abdn$contact_incom_match= 0
abdn$contact_incom_match[complete.cases(abdn$Contact_Phone)] = 1 * contact_incom_match


# Logical selection for matching records for those who purchased 
abdn$pur = 1 * ( abdn$emailmatch | abdn$incom_match |abdn$contactmatch | abdn$incom_contact_match | abdn$contact_incom_match)  

# Create additional columns for analyses
abdn$email = 1 * complete.cases(abdn$Email)
abdn$state = 1 * complete.cases(abdn$Address)
abdn$treat = ifelse(abdn$Test_Control == "test", 1, 0)

tab = table(abdn$pur, abdn$treat)
# Adding row labels for 'Outcome'
rownames(tab) = c("Not Purchased", "Purchased")
# Adding column labels for 'Treatment'
colnames(tab) = c("Control Group", "Treatment Group")
print(tab)

#Fitering unmatchable records
unmatchable_abdn <- abdn[abdn$pur == 0, ]
head(unmatchable_abdn)

#Dropping unmatched records and selecting only those that matched (purchased)
abdn_match <- abdn[abdn$pur == 1, ]

#Cross tabulations for all records(purchased and not purchased)
tab = table(abdn$pur, abdn$treat)
rownames(tab) = c("Not Purchased", "Purchased")
# Add column labels for 'Outcome'
colnames(tab) = c("Control Group", "Treatment Group")
print(tab)


all_states = abdn$Address[!is.na(abdn$Address)] 
set.seed(123)  # Setting a seed for reproducibility
random_states = sample(all_states, 5)

cross_tabulations = list()

for (state in random_states) {
  subset_data = abdn[abdn$Address == state, ]
  cross_tabulation =  table(subset_data$pur, subset_data$treat)
  rownames( cross_tabulation ) =  c("Not Purchased", "Purchased")
  colnames( cross_tabulation ) = c("Control Group", "Treatment Group")
  cross_tabulations[[state]] = cross_tabulation
}

# Print the cross-tabulations
for (state in random_states) {
  cat("Cross-tabulation for State:", state, "\n")
  print(cross_tabulations[[state]])
  cat("\n")
}


#Cleaning dataset

# Remove multiple columns
abdnclean = abdn %>%
  select(-(2:17))

#Changing index of columns and their column names
abdnclean = abdnclean %>%
  select(1, 5, 2, 4,3:ncol(abdnclean))
colnames(abdnclean) = c("Customer_ID", "Test_Group","Outcome","State_Available",
                        "Email_Available")

#Exporting the clean data set as a csv
write.csv(abdnclean, file = "abdnclean.csv", row.names = FALSE)



#Statistical tests
# Run regression analyses

out1 = lm(Outcome ~ Test_Group, data = abdnclean)
summary(out1)


out2 = aov(Outcome ~ Test_Group, data = abdnclean)
summary(out2)


out3 = lm(Outcome ~ Test_Group + State_Available + Email_Available , data = abdnclean)
summary(out3)

out4 = lm(Outcome ~ Test_Group + State_Available + Email_Available + State_Available*Test_Group + Email_Available*Test_Group, data = abdnclean)
summary(out4)

#optional : logistic model 
logmodel = glm(Outcome ~ Test_Group + State_Available + Email_Available + State_Available*Test_Group + Email_Available*Test_Group ,family = binomial(link="logit"), data = abdnclean)
summary(logmodel)
# Generate summary table
stargazer(out1,out3,out4,logmodel, type = "text")
