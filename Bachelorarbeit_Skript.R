### Bachelorarbeit ###
##1. Daten kompiliieren

# nötige Packete laden
library(readxl)
library(timetk)
library(writexl)
library(tidyverse)    
library(BatchGetSymbols)
library(plyr)
library(lubridate)
library(xlsx)
library(caret)
library(MASS)
library(lubridate)

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


# Are the variables covid and industry independent? or de wa have a change in the industry that went public over the time?
table(ordered_data$dummyCOVID , ordered_data$`Issuer Primary SIC`)
round(prop.table(table(ordered_data$dummyCOVID, ordered_data$`Issuer Primary SIC`)),4)
chisq.test(ordered_data$dummyCOVID, ordered_data$`Issuer TRBC Industry`)
fisher.test(ordered_data$dummyCOVID, ordered_data$`Issuer TRBC Industry`, simulate.p.value=TRUE)





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
data_w_covid <- orderd_data %>% select(Underpricing, `Issue Date`, `Issuer TRBC Industry`) %>% filter(`Issue Date` > "2020-02-24", `Issue Date` < "2020-07-30")

data_wo_covid <- orderd_data %>% select(Underpricing, `Issue Date`, `Issuer TRBC Industry`) %>% filter(`Issue Date` < "2020-02-24" | `Issue Date` > "2020-07-30") 

t.test(data_w_covid$Underpricing, data_wo_covid$Underpricing, alternative = "greater")

# was there a change in the industry that went public over the time 
industry_df <- orderd_data

industry_df$`Issuer TRBC Industry`[industry_df$`Issuer TRBC Industry` %in% hightechsic] <- "hightech"
industry_df$`Issuer TRBC Industry`[industry_df$`Issuer TRBC Industry` %in% c("Investment Management & Fund Operators", 	
                                                                           "Investment Trusts",
                                                                           "Banks",
                                                                           "Diversified Financial Services",
                                                                           "Consumer Financial Services",
                                                                           "Insurance Brokers",
                                                                           "Life & Health Insurance",
                                                                           "Real Estate Development & Operations",
                                                                           "Specialty Investment Services",
                                                                           "Real Estate Services",
                                                                           "Financial & Commodity Market Operators",
                                                                           "Property & Casualty Insurance",
                                                                           "Multiline Insurance")] <- "Finanzen"


industry_df$`Issuer TRBC Industry`[industry_df$`Issuer TRBC Industry` %in% c("Generic & Specialty Pharmaceuticals",
                                                                             "Advanced Medical Equipment",
                                                                             "Drug Retailers",
                                                                             "Biotechnology & Medical Research",
                                                                             "Healthcare Facilities & Services",
                                                                             "Medical Equipment, Supplies & Distribution")] <- "Medizin"

industry_df$`Issuer TRBC Industry`[industry_df$`Issuer TRBC Industry` %in% c("Auto Vehicles, Parts & Service Retailers",
                                                                             "Heavy Machinery & Vehicles",
                                                                             "Aerospace & Defense",
                                                                             "Industrial Machinery & Equipment",
                                                                             "Auto, Truck & Motorcycle Parts",
                                                                             "Auto & Truck Manufacturers",
                                                                             "Heavy Electrical Equipment"
                                                                             )] <- "Maschinen"

industry_df$`Issuer TRBC Industry`[industry_df$`Issuer TRBC Industry` %in% c("Renewable Energy Equipment & Services",
                                                                             "Oil-related Services and Equipment",
                                                                             "Oil & Gas Transportation Services",
                                                                             "Oil & Gas Refining and Marketing",
                                                                             "Oil & Gas Exploration and Production",
                                                                             "Coal",
                                                                             "Oil & Gas Drilling",
                                                                             "Natural Gas Utilities")] <- "Energie"

industry_df$`Issuer TRBC Industry`[industry_df$`Issuer TRBC Industry` %in% c("Advertising & Marketing",
                                                                             "Non-Profit/Private Organizations/Services",
                                                                             "Personal Services",
                                                                             "Construction & Engineering",
                                                                             "Environmental Services",
                                                                             "Passenger Transportation, Ground & Sea",
                                                                             "Leisure & Recreation",
                                                                             "Construction Supplies & Fixtures",
                                                                             "Freight Transportation, Ground",
                                                                             "Broadcasting",
                                                                             "Employment Services",
                                                                             "Business Support Services",
                                                                             "Business Support Supplies")] <- "Service"
 
industry_df$`Issuer TRBC Industry`[industry_df$`Issuer TRBC Industry` %in% c("Home Furnishings Retailers",
                                                                             "Other Other Specialty Retailers",
                                                                             "Home Improvement Products & Services Retailers",
                                                                             "Department Stores",
                                                                             "Discount Stores",
                                                                             "Food Distribution & Convenience Stores",
                                                                             "Apparel & Accessories Retailers")] <- "Einzelhandel"

industry_df$`Issuer TRBC Industry`[industry_df$`Issuer TRBC Industry` %in% c("Tobacco",
                                                                             "Home Furnishings",
                                                                             "Entertainment Production",
                                                                             "Household Products",
                                                                             "Paper Products",
                                                                             "Casinos & Gaming",
                                                                             "Forest & Wood Products",
                                                                             "Apparel & Accessories",
                                                                             "Toys & Games",
                                                                             "Personal Products")] <- "Konsum"
 
industry_df$`Issuer TRBC Industry`[industry_df$`Issuer TRBC Industry` %in% c("Precious Metals & Minerals",
                                                                             "Construction Materials",
                                                                             "Commodity Chemicals",
                                                                             "Specialty Chemicals",
                                                                             "Steel",
                                                                             "Specialty Mining & Metals",
                                                                             "Non-Paper Containers & Packaging",
                                                                             "Water & Other Utilities",
                                                                             "Agricultural Chemicals",
                                                                             "Semiconductors")] <- "Materialien"

industry_df$`Issuer TRBC Industry`[industry_df$`Issuer TRBC Industry` %in% c("Recreational Products",
                                                                             "Homebuilding",
                                                                             "Food Processing",
                                                                             "Hotels, Motels & Cruise Lines",
                                                                             "Fishing & Farming",
                                                                             "Restaurants",
                                                                             "Airlines")] <- "andere"
unique(industry_df$`Issuer TRBC Industry`)


industry_df %>% filter(`Issuer TRBC Industry` == "Finanzen") %>% summarise(mean = mean(Underpricing))
industry_df %>% filter(`Issuer TRBC Industry` == "Medizin") %>% summarise(mean = mean(Underpricing))
industry_df %>% filter(`Issuer TRBC Industry` == "andere") %>% summarise(mean = mean(Underpricing))
industry_df %>% filter(`Issuer TRBC Industry` == "Materialien") %>% summarise(mean = mean(Underpricing))
industry_df %>% filter(`Issuer TRBC Industry` == "Konsum") %>% summarise(mean = mean(Underpricing))
industry_df %>% filter(`Issuer TRBC Industry` == "Einzelhandel") %>% summarise(mean = mean(Underpricing))
industry_df %>% filter(`Issuer TRBC Industry` == "Service") %>% summarise(mean = mean(Underpricing))
industry_df %>% filter(`Issuer TRBC Industry` == "Energie") %>% summarise(mean = mean(Underpricing))
industry_df %>% filter(`Issuer TRBC Industry` == "Maschinen") %>% summarise(mean = mean(Underpricing))
industry_df %>% filter(`Issuer TRBC Industry` == "hightech") %>% summarise(mean = mean(Underpricing))

industry_2016 <- industry_df %>% filter(`Issue Date` > "2016-01-13", `Issue Date` < "2016-12-30")
industry_2017 <- industry_df %>% filter(`Issue Date` > "2017-01-01", `Issue Date` < "2017-12-30")
industry_2018 <- industry_df %>% filter(`Issue Date` > "2018-01-01", `Issue Date` < "2018-12-30")
industry_2019 <- industry_df %>% filter(`Issue Date` > "2019-01-01", `Issue Date` < "2019-12-30")
industry_2020 <- industry_df %>% filter(`Issue Date` > "2020-01-01", `Issue Date` < "2020-12-30")
industry_2021 <- industry_df %>% filter(`Issue Date` > "2016-01-01", `Issue Date` < "2016-12-30")

dustry_counts_2016 <- table(industry_2016$`Issuer TRBC Industry`)
prop_16 <- dustry_counts_2016 / sum(dustry_counts_2016)

pie(prop_16, main = "2016")

dustry_counts_2017 <- table(industry_2017$`Issuer TRBC Industry`)
prop_17 <- dustry_counts_2017 / sum(dustry_counts_2017)

pie(prop_17, main = "2017")

dustry_counts_2018 <- table(industry_2018$`Issuer TRBC Industry`)
prop_18 <- dustry_counts_2018 /sum(dustry_counts_2018)

pie(prop_18, main = "2018")

dustry_counts_2019 <- table(industry_2019$`Issuer TRBC Industry`)
prop_19 <- dustry_counts_2019 / sum(dustry_counts_2019)

pie(prop_19, main = "2019")

dustry_counts_2020 <- table(industry_2020$`Issuer TRBC Industry`)
prop_20 <- dustry_counts_2020 / sum(dustry_counts_2020)

pie(prop_20, main = "2020")

dustry_counts_2021 <- table(industry_2021$`Issuer TRBC Industry`)
prop_21 <- dustry_counts_2021 / sum(dustry_counts_2021)
pie(prop_21, main = "2021", labels = names(dustry_counts_2021), srt = 10)




#checking for distribution
round(prop.table(table(industry_df$`Issuer TRBC Industry`, industry_df$dummyCOVID)),2)

#chi squared

chisq.test(industry_df$dummyCOVID, industry_df$`Issuer TRBC Industry`)
fisher.test(industry_df$dummyCOVID, industry_df$`Issuer TRBC Industry`, simulate.p.value=TRUE)





orderd_data$founding_date <- as.Date(orderd_data$founding_date)
orderd_data$age <- round(((orderd_data$`Issue Date`-orderd_data$founding_date) / 365),2)

# for the age i need a subsample
subsample <- orderd_data

subsample$age[subsample$age < 1] <- NA

subsample <- na.omit(subsample)

# afterwards i cleaned up the whole dataset and startet at the beginning with new dummyvariables for the orderd dataset, but did most of it in excel

#write_xlsx(orderd_data, "/Users/yanniksa/Desktop/Uni/QM2/Bachelorthesis/orderd_data.xlsx")
#write_xlsx(subsample, "/Users/yanniksa/Desktop/Uni/QM2/Bachelorthesis/subsample.xlsx")
ordered_data_right <- read_xlsx("orderd_data.xlsx")
subsample_right <- read_xlsx("subsample.xlsx")






# how underpricing evolved over the time
underpricing_year <- ordered_data_right %>% summarise_by_time(.date_var = `Issue Date`, .by = "month", mean = mean(Underpricing))

plot(x = underpricing_year$`Issue Date`, y = underpricing_year$mean, type = "l", main = "IPO Underpricing Zeitreihe", ylab = "Underpricing in %", xlab = "Jahr")

final_right <- ordered_data_right[c(2, 10, 12, 13, 14)]
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

# mean , max, min and boxplot underpricing
mean(final_right$Underpricing)
median(final_right$Underpricing)
boxplot(final_right$Underpricing, main = "Underpricing", ylab = "in %")
quantile(final_right$Underpricing)
min(final_right$Underpricing)
max(final_right$Underpricing)
# count hightech companies
sum(final_right$hightech_dummy)
sum(final_right$dummyCOVID)
sum(final_right$dummynation)


names <- c("dummyCOVID", "hightech_dummy", "dummynation")
final_right[,names] <- lapply(final_right[,names], factor)
subsample_right[,names] <- lapply(subsample_right[,names], factor)


Model <- lm(final_right$Underpricing ~ final_right$IPO_Erlös + final_right$hightech_dummy + final_right$dummynation + final_right$dummyCOVID, data = final_right)
summary(Model)

plot(Model, 4)
cd <- cooks.distance(Model)


#testing wether model works better without observations 905, 1093 and 1088
final_right_905 <- final_right[c(-905, -1093, -1088),]

Model_905 <- lm(final_right_905$Underpricing ~ final_right_905$IPO_Erlös + final_right_905$hightech_dummy + final_right_905$dummynation + final_right_905$dummyCOVID, data = final_right_905)
summary(Model_905)


# compare usa and eu
final_right_eu <- final_right_905 %>% filter(dummynation == 1)

final_right_usa <- final_right_905 %>% filter(dummynation == 0)

final_right_eu <- final_right_eu[,-5]

final_right_usa <- final_right_usa[,-5]

Model_eu <- lm(final_right_eu$Underpricing ~ final_right_eu$IPO_Erlös + final_right_eu$dummyCOVID + final_right_eu$hightech_dummy, data = final_right_eu)

summary(Model_eu)

Model_usa <- lm(final_right_usa$Underpricing ~ final_right_usa$IPO_Erlös + final_right_usa$dummyCOVID + final_right_usa$hightech_dummy, data = final_right_usa)

summary(Model_usa)

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



