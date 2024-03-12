library(readxl)
library(dplyr)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(lubridate)

df_billing <- read_excel('~/DATA 332/Patient Billing Data/Billing.xlsx')
df_patient_information <- read_excel('~/DATA 332/Patient Billing Data/Patient.xlsx')
df_patient_visit <- read_excel('~/DATA 332/Patient Billing Data/Visit.xlsx')

#Left join the patient information with the visit information and that will be left joined to the billing information
df_patient_visit_info <- left_join(df_patient_information, df_patient_visit, by = "PatientID")
df_billing_and_visit_patient_information <- left_join(df_billing, df_patient_visit_info, by = "VisitID")

#Remove the 1 row of unnecessary data
df_new_patient_information <- df_billing_and_visit_patient_information[-c(1), ]

#Because of huge lists of reasons, we group up the ones which are follow ups & monitoring together with the original visit of diagnosis
df_all_patient_information<- df_new_patient_information%>%
  dplyr::mutate(Reason = ifelse((Reason == 'UTI follow-up'), 'UTI',Reason))%>%
  dplyr::mutate(Reason = ifelse((Reason == 'Rhinitis follow-up'), 'Rhinitis',Reason))%>%
  dplyr::mutate(Reason = ifelse((Reason == 'Migraine follow-up'), 'Migraine',Reason))%>%
  dplyr::mutate(Reason = ifelse((Reason == 'Laceration follow-up')| (Reason == 'Laceration of right foot') | (Reason == 'Laceration of right calf') | (Reason == 'Laceration of left hand'),'Laceration',Reason))%>%
  dplyr::mutate(Reason = ifelse((Reason == 'Hypotension monitoring'), 'Hypotension',Reason))%>%
  dplyr::mutate(Reason = ifelse((Reason == 'Hypertension monitoring'), 'Hypertension monitoring',Reason))%>%
  dplyr::mutate(Reason = ifelse((Reason == 'Dermatitis follow-up'), 'Dermatitis',Reason))%>%
  dplyr::mutate(Reason = ifelse((Reason == 'Cyst removal follow-up'), 'Cyst removal',Reason))%>%
  dplyr::mutate(Reason = ifelse((Reason == 'Bronchitis follow-up'), 'Bronchitis',Reason))%>%
  dplyr::mutate(Reason = ifelse((Reason == 'Allergic reaction follow-up'), 'Allergic reaction',Reason))%>%
  dplyr::mutate(Reason = ifelse((Reason == 'Spotted fever rickettsiosis follow-up'), 'Spotted fever rickettsiosis',Reason))

#Turns the dates into the months (which we want) into word date 
df_all_patient_information$Month <- month(df_all_patient_information$VisitDate, label = TRUE)

#Chart the Reason for Visit segmented by Month 
df_visit_date <- df_all_patient_information %>%
  group_by(Reason, Month) %>%
  summarise(count = n())

ggplot(df_visit_date, aes(x = count, y = Reason, fill = Month)) +
  geom_bar(stat = "identity", width = 0.7) +
  labs(title = "Reason For Visit by Month",
       x = "Count",
       y = "Reason")

#Chart the reason for visit segmented by whether it was a walk-in or not.
df_all_patient_information %>%
  group_by(Reason, WalkIn) %>%
  summarise(count_by_reason = n()) %>%
  ggplot(aes(x = count_by_reason, y = Reason, fill = WalkIn)) +
  geom_bar(positions="dodge", stat="identity", width=0.75) 

#Chart the reason for visit segmented by city
df_all_patient_information %>%
  group_by(Reason, City) %>%
  summarise(count_by_city = n()) %>%
  ggplot(aes(x = count_by_city, y = Reason, fill = City)) +
  geom_bar(positions="dodge", stat="identity", width=0.75) 

#Chart the reason for visit segmented by whether the invoice was paid or not
df_all_patient_information %>%
  group_by(Reason, InvoicePaid, InvoiceNum) %>%
  summarise(count_by_InvoiceNum = n()) %>%
  ggplot(aes(x = count_by_InvoiceNum, y = Reason, fill = InvoicePaid)) +
  geom_bar(positions="dodge", stat="identity", width=0.75) 

#Chart on the walk-ins based on city
df_all_patient_information %>%
  group_by(City, WalkIn) %>%
  summarise(count_by_city = n()) %>%
  ggplot(aes(x = count_by_city, y = City, fill = WalkIn)) +
  geom_bar(positions="dodge", stat="identity", width=0.75) 
