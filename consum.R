setwd("~/Desktop/AAU OECON/5. semester/Empirisk projekt/Data")
library(stargazer)

consum <- read.csv("consum.csv")
consum$date <- seq(as.Date("1995/01/01"), as.Date("2019/10/01"), by = "quarter")

consum <- consum %>%
  select(Country, Value, date)


consum <- consum[-c(101:200),] 

popu <- read.csv("popu.csv")
popu$date <- seq(as.Date("1995/01/01"), as.Date("2019/10/01"), by = "quarter")

popu <- popu %>%
  select(Country, Value, date)
 
popu <- popu[-c(101:200),]
consum<- consum[-c(93:100,193:200,293:300,393:400,493:500,593:600,693:700,793:800,
                    893:900,993:1000,1093:1100,1193:1200,1293:1300,1393:1400),] 
popu <- popu[-c(93:100,193:200,293:300,393:400,493:500,593:600,693:700,793:800,
                893:900,993:1000,1093:1100,1193:1200,1293:1300,1393:1400),] 

popu$Value <- popu$Value*1000


consum$Value <- consum$Value*1000000

consum$Value <- (consum$Value/popu$Value)

ggplot(consum, aes(x = date, y = Value, by = Country)) +
  geom_line(aes(color = Country)) +
  facet_wrap(~ Country) +
  labs(title = "Household consumption over time by country",
       y = "Value", 
       x = "Year",
       caption = "Source: OECD") +
  theme(legend.position = "none")

###########


### Making list ####

list_consum <- consum %>%
  group_by(Country)

list_consum <- group_split(list_consum)

#list_of_country_codes <- c("AU", "BE", "DK", "FR", "GE", "IT", "NL","NZ", "NO", "ES", "SE", "CH", "GB", "US")

#country_names_codes <- as.data.frame(c("ISO2" = list_of_country_codes, 
                                       #"Country_name" = countrycode(list_of_country_codes, origin = 'iso2c', destination = 'country.name'),
                                      # "IOS3" = countrycode(list_of_country_codes, origin = 'iso2c', destination = 'iso3c')))


#test_consum <- consum %>%
 # filter(LOCATION %in% list_of_country_codes)

#list_consum <- setNames(list_consum,c("AU", "BE", "DK", "FR", "GE", "IT", "NL",
                                              #  "NZ", "NO", "ES", "SE", "CH", "UK", "US"))

