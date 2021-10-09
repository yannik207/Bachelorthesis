### Bachelorarbeit ###
##1. Daten kompiliieren

# nötige Packete laden
library(readxl)
library(writexl)
library(tidyverse)    
library(BatchGetSymbols)
library(plyr)
library(lubridate)
library(corrplot)
library(RColorBrewer)
library(xlsx)
library(caret)
library(MASS)

# datan laden

initial_data <- read_xlsx("Transfermodul_Datensatz.xlsx")
founding_data <- read_csv("fouding_data.csv")


# clean founding data and merge them together
founding_data <- str_split_fixed(founding_data$`Issuer Name...2;Date of Incorporation`, ";", 2)
founding.data <- as.data.frame(founding_data)
colnames(founding_data) <- c("Unternehmensname", "founding_date")
names(initial_data)[names(initial_data) == "Gross Proceeds Inc. Overallotment Exercised"] <- "IPO_Erlös"
raw_data <- merge(x = initial_data, y = founding_data, by.x = 'Issuer Name...2', by.y = 'Unternehmensname')

# drop unnecesarry columns
drop <- c("Equity Deal Number","ISIN", "Issue Type", "Issuer Ticker Symbol", "Issuer Name...13")

raw_data = raw_data[,!(names(raw_data) %in% drop)]

# clean NA's and different errors

sapply(raw_data, function(x) sum(is.na(x)))

# some errors are still in the dataeset althought by now there are now NA's

raw_data$founding_date <- na_if(raw_data$founding_date, "NULL")
raw_data$founding_date <- na_if(raw_data$founding_date, "Unable to collect data for the field 'TR.CompanyIncorpDate' and some specific identifier(s).")

# drop new NA's

n_samples_with_NA <- nrow(raw_data)
clean_data <- na.omit(raw_data)
n_samples_without_NA <- nrow(clean_data)

round((n_samples_with_NA - n_samples_without_NA) / n_samples_with_NA, 3)

# verify although there are NA
any(is.na(clean_data))

# dummy variable wether the company went public during COVID19

orderd_data <- clean_data[order(clean_data$`Issue Date`),]
orderd_data$`Issue Date` <-as.Date(as.POSIXct(orderd_data$`Issue Date`, 'GMT'))
orderd_data$founding_date <- as.Date(as.POSIXct(orderd_data$founding_date, 'GMT'))

orderd_data$dummyCOVID <- ifelse(orderd_data$`Issue Date` < as.Date("24/02/2020", format = "%d/%m/%Y") &
                              orderd_data$`Issue Date` < as.Date("23/02/2020", format = "%d/%m/%Y"), 1, 0)


table(orderd_data$dummy)



hightechsic <- c("Software", "Integrated Telecommunications Services", "Communications Equipment", "Computer Hardware", "Electrical Components & Equipment", "Consumer Electronics", "Electric Utilities", "IT Services & Consulting", "Aerospace & Defense", "Biotechnology & Medical Research")

orderd_data$hightech_dummy<- ifelse(orderd_data$`Issuer TRBC Industry` %in% hightechsic , 1, 0)


europe <- c("Germany", "France", "Belgium", "Spain")


orderd_data$dummynation <- ifelse(orderd_data$`Issuer Nation` %in% europe, 1,0)

# section 2 describing statistics
final <- orderd_data[c(2, 10, 12, 14, 15)]

final %>% filter(dummyCOVID == 1) %>% summarise(mean = mean(Underpricing))
final %>% filter(dummyCOVID == 0) %>% summarise(mean = mean(Underpricing))

# can't find a significant difference between before and during COVID-19
# therefor I change the covid timeline and just the time between stock drop and stock recovery
data_w_covid <- orderd_data %>% select(Underpricing, `Issue Date`) %>% filter(`Issue Date` > "2020-02-24", `Issue Date` < "2020-07-30")

data_wo_covid <- orderd_data %>% select(Underpricing, `Issue Date`) %>% filter(`Issue Date` < "2020-02-24" | `Issue Date` > "2020-07-30") 

t.test(data_w_covid$Underpricing, data_wo_covid$Underpricing, alternative = "greater")


orderd_data$founding_date <- as.Date(orderd_data$founding_date)
orderd_data$age <- round(((orderd_data$`Issue Date`-orderd_data$founding_date) / 365),2)

# for the age i need a subsample
subsample <- orderd_data

subsample$age[subsample$age < 1] <- NA

subsample <- na.omit(subsample)

# afterwards i cleaned up the whole dataset and startet at the beginning with new dummyvariables for the orderd dataset, but did most of it in excel

write_xlsx(orderd_data, "/Users/yanniksa/Desktop/Uni/QM2/Bachelorthesis/orderd_data.xlsx")
write_xlsx(subsample, "/Users/yanniksa/Desktop/Uni/QM2/Bachelorthesis/subsample.xlsx")
#ordered_data_right <- read_xlsx("orderd_data.xlsx")
subsample_right <- read_xlsx("subsample.xlsx")


final_right <- ordered_data_right[c(2, 10, 12, 14, 15)]
subsample_right <- subsample_right[c(2, 10, 12, 13, 14, 15)]

# how many companies are in US and Europa
final_right %>% group_by(dummynation) %>% summarise(count = count(dummynation))

# how is the underpricing difference in europa and usa
nation_eu <- final_right %>% filter(dummynation == 1) 
nation_usa <- final_right %>% filter(dummynation == 0) 

# test wether the difference between eu and usa is significant

t.test(nation_usa$Underpricing, nation_eu$Underpricing)
# how is the underpricing in hightech industries

hightech_data <- final_right %>% filter(hightech_dummy == 1) 
non_hightech_data <- final_right %>% filter(hightech_dummy == 0)

# test wether the underpricing in hightech industries is higher than in non hightech industries
t.test(hightech_data$Underpricing, non_hightech_data$Underpricing, alternative = "greater")



# Model
M <-cor(final_right)
round(M,3)

# Model for subsample
M_age <- cor(subsample_right, method = "spearman")
round(M_age,3)

names <- c("dummyCOVID", "hightech_dummy", "dummynation")
final_right[,names] <- lapply(final_right[,names], factor)
subsample_right[,names] <- lapply(subsample_right[,names], factor)

corrplot(M, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))

Model <- lm(final_right$Underpricing ~ final_right$IPO_Erlös + final_right$hightech_dummy + final_right$dummynation + final_right$dummyCOVID, data = final_right)
summary(Model)

Model_age <- lm(subsample_right$Underpricing ~ subsample_right$IPO_Erlös + subsample_right$dummyCOVID + subsample_right$age + subsample_right$hightech_dummy + subsample_right$dummynation, data = subsample_right)
summary(Model_age)
#checking for multicollinearity
car::vif(Model)
car::vif(Model_age)

plot(final_right$Underpricing, rstandard(Model), ylab='Standardized Residuals', xlab='y') 
abline(h=0)

robust_Model <- rlm(final_right$Underpricing ~ final_right$IPO_Erlös + final_right$hightech_dummy + final_right$dummynation + final_right$dummyCOVID, data = final)

summary(robust_Model)$sigma

summary(Model)$sigma



