setwd("~/Data332_Rstudio/Patient Data")


library(tidyr)
library(tidyverse)
library(readxl)
library(dplyr)
library(kableExtra)

Visit_patient_joined <- left_join(Visit, Patient, by =c('PatientID'))
All_Joined <- left_join(Visit_patient_joined, Billing, by =c('VisitID'))

Reason_and_month_pivot<- Visit%>%
  group_by(VisitDate,Reason)%>%
  summarize(count=n())

Reason_and_month_pivot$VisitDate <- format(Visit$VisitDate, '%m')

ggplot(data=Reason_and_month_pivot, aes(x=Reason, y=VisitDate, fill= VisitDate))+
  geom_col()+
  theme(axis.text= element_text (angle = 90, vjust=5, hjust = 1))
       


ggplot(data=Visit, aes(x=Reason, y=(WalkIn), fill= WalkIn))+
  geom_col()+
  theme(axis.text= element_text (angle = 90, vjust=5, hjust = 1))




reason_and_zip <- All_Joined%>%
  group_by(Zip,Reason)%>%
  summarize(count=n())

  ggplot(data=reason_and_zip, aes(x=Zip, y=Reason))+
    geom_col()+
    theme(axis.text= element_text (angle = 90, vjust=5, hjust = 1))

total_and_reason<-All_Joined%>%
group_by(Reason,InvoiceAmt,InvoicePaid)%>%
  summarize(count=n())

ggplot(data = total_and_reason, aes(x=Reason, y= InvoiceAmt, fill=InvoicePaid))+
  geom_col()+
  theme(axis.text=element_text(angle = 90, vjust=5, hjust =1))

 

Visits_by_City<- All_Joined%>%
  group_by(InvoicePaid,City,InvoiceAmt)%>%
summarize(na.rm=TRUE)



ggplot(data=Visits_by_City, aes(x=City, y=InvoiceAmt, fill=InvoicePaid))+
  geom_col()+
  theme(axis.text = element_text(angle=90, vjust=5, hjust=1))

