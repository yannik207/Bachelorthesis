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

# datan laden

initial_data <- read_xlsx("Transfermodul_Datensatz.xlsx")
founding_data <- read_csv("fouding_data.csv")


# clean founding data and merge them together
founding_data <- str_split_fixed(founding_data$`Issuer Name...2;Date of Incorporation`, ";", 2)
founding.data <- as.data.frame(founding_data)
colnames(founding_data) <- c("Unternehmensname", "founding_date")
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
n_samples_without_NA <- nrow(raw_data_wo_NA)

round((n_samples_with_NA - n_samples_without_NA) / n_samples_with_NA, 3)

# verify although there are NA
any(is.na(clean_data))

# dummy variable wether the company went public during COVID19

orderd_data <- clean_data[order(clean_data$`Issue Date`),]
orderd_data$`Issue Date` <-as.Date(as.POSIXct(orderd_data$`Issue Date`, 'GMT'))
orderd_data$founding_date <- as.Date(as.POSIXct(order_data$founding_date, 'GMT'))

orderd_data$dummyCOVID <- ifelse(orderd_data$`Issue Date` < as.Date("24/02/2020", format = "%d/%m/%Y") &
                              orderd_data$`Issue Date` < as.Date("23/02/2020", format = "%d/%m/%Y"), 1, 0)


table(orderd_data$dummy)

orderd_data$founding_date <- as.Date(orderd_data$founding_date)
orderd_data$age <- round(((orderd_data$`Issue Date`-orderd_data$founding_date) / 365),2)

orderd_data$age[orderd_data$age < 1] <- mean(orderd_data$age)

aged_data <- na.omit(orderd_data)

hightechsic <- c("Software", "Integrated Telecommunications Services", "Communications Equipment", "Computer Hardware", "Electrical Components & Equipment", "Consumer Electronics", "Electric Utilities", "IT Services & Consulting", "Aerospace & Defense", "Biotechnology & Medical Research")

orderd_data$hightech_dummy<- ifelse(orderd_data$`Issuer TRBC Industry` %in% hightechsic , 1, 0)

europe <- c("Germany", "France", "Belgium", "Spain")


orderd_data$dummynation <- ifelse(orderd_data$`Issuer Nation` %in% europe, 1,0)

final <- orderd_data[c(2, 10, 13, 14, 15, 16)]

final$age <- as.numeric(final$age)
M <-cor(final)
corrplot(M, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))

Model <- lm(final$Underpricing ~ final$`Gross Proceeds Inc. Overallotment Exercised` + final$age + final$hightech_dummy + final$dummynation + final$dummyCOVID, data = final)
summary(Model)
