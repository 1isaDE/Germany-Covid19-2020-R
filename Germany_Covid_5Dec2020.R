library(tidyverse)
library(data.table)
library(ggplot2)

#Loading data on covid. Csv file was downloaded from rki website
covid_rki <- fread("RKI_COVID19.csv")
head(covid_rki, n = 5)
my_dt <- covid_rki[, c("Bundesland", "Altersgruppe", "Geschlecht", "AnzahlFall",
                       "Meldedatum")]
my_dt$Meldedatum <- as.Date(my_dt$Meldedatum)

#Start date of the data set
min(my_dt$Meldedatum)

#End date of the data set
max(my_dt$Meldedatum)

#Number of confirmed cases per day in Germany
my_dt %>% 
  group_by(Meldedatum) %>%
  summarise(total_daily = sum(AnzahlFall)) %>%
  ggplot() + geom_col(aes(x = Meldedatum, y = total_daily)) +
  theme_classic() + labs(x = "Date", y = "Number of confirmed cases per day") + 
  ggtitle("Number of daily cases in Germany")

#LOOKING AT CASES FROM GENDER PERSPECTIVE
#Reading in population data including gender
populGender_dt <- fread("populationGermanyGender.csv")
populGender_dt <- populGender_dt[-c(1:5),]
names(populGender_dt) <- c("date", "male", "female", "total")
populGender_dt <- populGender_dt[-1,]

populGender2019_dt <- populGender_dt %>%
  filter(date == "31.12.2019") %>%
  melt(., id.vars = "date",  
       measure.vars = c("male", "female", "total"),
       variable.name = "gender", value.name = "popul2019")

#Number of total confirmed cases in Germany split by gender 
gender_dt <- my_dt %>% 
            group_by(Geschlecht) %>%
            summarise(total_cases = sum(AnzahlFall))
            
names(gender_dt) <- c("gender", "total_cases")
gender_dt$gender <- c("male", "undefined", "female")
genderCases <- merge(populGender2019_dt, gender_dt, by = "gender")
genderCases$date <- NULL
genderCases$total_cases <- as.numeric(genderCases$total_cases)
genderCases$popul2019 <- as.numeric(genderCases$popul2019)
genderCases$cases_prop <- genderCases$total_cases/genderCases$popul2019
genderCases

#Conclusion: Women are more slightly more susceptible to COVID-19 infection


dailyCasesGender <- my_dt %>% 
  group_by(Geschlecht, Meldedatum) %>%
  summarise(total_daily = sum(AnzahlFall)) 

names(dailyCasesGender) <- c("gender", "date", "total_daily") 
dailyCasesGender$gender <- gsub("M", "male", dailyCasesGender$gender)
dailyCasesGender$gender <- gsub("W", "female", dailyCasesGender$gender)
dailyCasesGender$gender <- gsub("unbekannt", "undefined", dailyCasesGender$gender)

genderPropDaily <-  merge(dailyCasesGender, genderCases[, c(1:2)], by = "gender", all.x = TRUE)
genderPropDaily$daily_prop_mln <- genderPropDaily$total_daily/genderPropDaily$popul2019*1000000

genderPropDaily %>%
  filter(gender != "undefined") %>%
  ggplot() + geom_line(aes(x = date, y = daily_prop_mln, color = gender)) +
  theme_classic() + labs(x = "Date", y = "Number of confirmed cases per day per 1mln people") + 
  ggtitle("Number of daily cases per 1mln people in Germany split by gender")
#NUMBER OF DAILY INFECTIONS ARE DISTRIBUTED ALMOST EQUALLY BETWEEN MEN AND WOMEN, WITH A SLIGHTLY HIGHER
#NUMBER OF CASES FOR WOMEN

genderPropDaily %>% 
  group_by(gender, date) %>%
  filter(gender != "undefined") %>%
  summarise(total_prop = sum(daily_prop_mln)) %>%
  ggplot() + geom_boxplot(aes(x = gender, y = total_prop))  +
  theme_classic() + labs(x = "Gender", y = "Number of confirmed cases per day") + 
  ggtitle("Number of daily cases per 1 mln people in Germany split by Gender")
#WOMEN AND MEN ARE AFFECTED ALMOST EQUALLY. VARIABILITY IS A BIT HIGHER FOR WOMEN.

#Reading in population data including gender
populGender_dt <- fread("populationGermanyGender.csv")
populGender_dt <- populGender_dt[-c(1:5),]
names(populGender_dt) <- c("date", "male", "female", "total")
populGender_dt <- populGender_dt[-1,]

populGender2019_dt <- populGender_dt %>%
  filter(date == "31.12.2019") %>%
  melt(., id.vars = "date",  
       measure.vars = c("male", "female", "total"),
       variable.name = "gender", value.name = "popul2019")

options(scipen=10000)
my_dt %>% 
  group_by(Altersgruppe) %>%
  summarise(total = sum(AnzahlFall)) %>%
  ggplot() + geom_col(aes(x = Altersgruppe, y = total))  +
  theme_classic() + labs(x = "Age Group", y = "Total number of confirmed cases") + 
  ggtitle("Number of confirmed cases in Germany split by age groups")

my_dt %>% 
  group_by(Altersgruppe, Meldedatum) %>%
  summarise(total = sum(AnzahlFall)) %>%
  ggplot() + geom_boxplot(aes(x = Altersgruppe, y = total))  +
  theme_classic() + labs(x = "Age Group", y = "Number of confirmed cases per day") + 
  ggtitle("Number of daily cases in Germany split by age groups")
#THE MAJORITY OF CASES ARE COMING FROM 35-59 AND 15-34 AGE GROUPS.
#?BUT NEED REFERENCE TO THE POPULATION

#Total number of cases per region per 1000 people
total_budndeslander <- my_dt %>% 
  group_by(Bundesland) %>%
  summarise(total = sum(AnzahlFall)/1000) %>%
  arrange(desc(total))
total_budndeslander

total_budndeslander %>%
  ggplot() + geom_col(aes(x = total, y = reorder(Bundesland, total))) +
  theme_classic() + labs(x = "Total number of confirmed cases per 1000 people",
                         y = "Region (Bundesland)") +
  ggtitle("Number of total confirmed cases Jan-Dec2020 per region")

#Total number of cases in Germany as of 5 Dec 2020
sum(total_budndeslander$total)*1000 

#Loading data on population in Germany. Csv file was downloaded from
#https://www-genesis.destatis.de/genesis/online?operation=statistic&levelindex=0&levelid=1607344258634&code=12411#abreadcrumb

popul_dt <- fread("populationGermany.csv")

popul_region <- popul_dt[-(1:4), ]
popul_region[2, 1] <- "Baden-Württemberg"
popul_region[17, 1] <- "Thüringen"
popul_region <- popul_region[, c(1, 11)]
names(popul_region) <- c("Bundesland", "popul2019")
popul_region <- popul_region[-1, ]
popul_region$popul2019 <- as.numeric(popul_region$popul2019) 
popul_region %>%  arrange(desc(popul2019))

#Merging 2 tables with common column Bundesland
infRate_dt <-  merge(total_budndeslander, popul_region, by = "Bundesland")
infRate_dt$total <- infRate_dt$total*1000
infRate_dt$infection_rate <- infRate_dt$total/infRate_dt$popul2019*1000 

infRate_dt %>%
  ggplot() + geom_col(aes(x = infection_rate, y = reorder(Bundesland, infection_rate))) +
  theme_classic() + labs(x = "Infection rate per 1000 people",
                         y = "Region (Bundesland)") +
  ggtitle("Infection rate for Jan-Dec2020 per region")

my_dt %>% 
  group_by(Bundesland, Meldedatum) %>%
  summarise(total_daily = sum(AnzahlFall)) %>%
  ggplot() + geom_line(aes(x = Meldedatum, y = total_daily, color = Bundesland)) +
  theme_classic()



  
  
  
  
  