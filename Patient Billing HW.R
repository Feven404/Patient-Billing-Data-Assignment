library(readxl)
library(dplyr)
library(tidyverse)
library(tidyr)
library(ggplot2)

df_billing <- read_excel('~/DATA 332/Patient Billing Data/Billing.xlsx')
df_patient_information <- read_excel('~/DATA 332/Patient Billing Data/Patient.xlsx')
df_patient_visit <- read_excel('~/DATA 332/Patient Billing Data/Visit.xlsx')

df_patient_visit_information <- left_join(df_patient_information, df_patient_visit, by = "PatientID")
df_billing_patient_information <- left_join(df_billing, df_patient_visit_information, by = "VisitID")

df_new_patient_visit_information <- df_patient_visit_information[-c(1), ]

df_visit_date <- df_new_patient_visit_information %>%
  mutate(visit_month = month(VisitDate)) %>%
  group_by(Reason, visit_month) %>%
  summarise(count = n())

ggplot(df_visit_date, aes(x = count, y = Reason, fill = visit_month)) +
  geom_bar(stat = "identity", width = 0.7) +
  labs(title = "Visit Date and Reason Count",
       x = "Count",
       y = "Reason")

df_new_patient_visit_information %>%
  group_by(Reason, WalkIn) %>%
  summarise(count_by_reason = n()) %>%
  ggplot(aes(x = Reason, y = count_by_reason, fill = WalkIn)) +
  geom_bar(positions="dodge", stat="identity", width=0.75) +
  theme(axis.text = element_text(angle = 55, vjust = .5, hjust = 1))

df_new_patient_visit_information %>%
  group_by(Reason, City) %>%
  summarise(count_by_city = n()) %>%
  ggplot(aes(x = Reason, y = count_by_city, fill = City)) +
  geom_bar(positions="dodge", stat="identity", width=0.75) +
  theme(axis.text = element_text(angle = 55, vjust = .5, hjust = 1))

df_billing_patient_information %>%
  group_by(Reason, InvoicePaid, InvoiceNum) %>%
  summarise(count_by_InvoiceNum = n()) %>%
  ggplot(aes(x = Reason, y = count_by_InvoiceNum, fill = InvoicePaid)) +
  geom_bar(positions="dodge", stat="identity", width=0.75) +
  theme(axis.text = element_text(angle = 55, vjust = .5, hjust = 1))

df_new_patient_visit_information %>%
  group_by(City, WalkIn) %>%
  summarise(count_by_city = n()) %>%
  ggplot(aes(x = City, y = count_by_city, fill = WalkIn)) +
  geom_bar(positions="dodge", stat="identity", width=0.75) +
  theme(axis.text = element_text(angle = 55, vjust = .5, hjust = 1))
