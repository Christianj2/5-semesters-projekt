setwd("~/Desktop/AAU OECON/5. semester/Empirisk projekt/Data")
library(stargazer)
library(gapminder)

house_price <- read.csv("huspriser.csv")
house_price$date <- seq(as.Date("1995/01/01"), as.Date("2019/10/01"), by = "quarter")

house_price <- house_price %>%
  select(Country, Value, date)


house_price <- house_price[-c(93:100,193:200,293:300,393:400,493:500,593:600,693:700,793:800,
                    893:900,993:1000,1093:1100,1193:1200,1293:1300,1393:1400),] #Sort and delete some countries

ggplot(house_price, aes(x = date, y = Value, by = Country)) +
  geom_line(aes(color = Country)) +
  facet_wrap(~ Country) +
  labs(title = "House prices over time by country",
       y = "Index", 
       x = "Year",
       caption = "Source: OECD") + 
  theme(legend.position = "none")

################

### Making list ####

list_house_price <- house_price %>%
  group_by(Country)

list_house_price <- group_split(list_house_price)

list_house_price <- setNames(list_house_price,c("AU", "BE", "DK", "FR", "GE", "IT", "NL",
                                                "NZ", "NO", "ES", "SE", "CH", "UK", "US"))

