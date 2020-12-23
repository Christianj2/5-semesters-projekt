library(dplyr)
library(zoo)
setwd("~/Desktop/AAU OECON/5. semester/Empirisk projekt/Data")

netnat <-read.csv("netnatinc.csv", sep = ",")

netnat <- netnat %>%
  select(Country, Year, Value)

annual_data <- data.frame(
  person=(netnat$Country),
  year=(netnat$Year),
  y=(netnat$Value)
)

expand_data <- function(x) {
  years <- min(x$year):max(x$year)
  quarters <- 1:4
  grid <- expand.grid(quarter=quarters, year=years)
  x$quarter <- 1
  merged <- grid %>% left_join(x, by=c('year', 'quarter'))
  merged$person <- x$person[1]
  return(merged)
}

interpolate_data <- function(data) {
  xout <- 1:nrow(data)
  y <- data$y
  interpolation <- approx(x=xout[!is.na(y)], y=y[!is.na(y)], xout=xout)
  data$yhat <- interpolation$y
  return(data)
}

expand_and_interpolate <- function(x) interpolate_data(expand_data(x))

quarterly_data <- annual_data %>% group_by(person) %>% do(expand_and_interpolate(.))

print(as.data.frame(quarterly_data))

names(quarterly_data)[3] <- "Country"
names(quarterly_data)[5] <- "Value"

netnat <- quarterly_data %>%
  select(-y, -year, -quarter)

print(as.data.frame(netnat))

netnat <- netnat[-c(93:96, 189:192, 285:288, 381:384, 477:480, 573:576, 669:672,
                    765:768, 861:864, 957:960, 1053:1056, 1149:1152, 1245:1248, 1341:1344),]

netnat <- data.frame(netnat)

netnat$date <- seq(as.Date("1995/01/01"), as.Date("2017/10/01"), by = "quarter")

popu <- read.csv("popu.csv")
popu$date <- seq(as.Date("1995/01/01"), as.Date("2019/10/01"), by = "quarter")

popu <- popu %>%
  select(Country, Value, date)

popu <- popu[-c(101:200),]
popu <- popu[-c(93:100,193:200,293:300,393:400,493:500,593:600,693:700,793:800,
                893:900,993:1000,1093:1100,1193:1200,1293:1300,1393:1400),] 

popu$Value <- popu$Value*1000


netnat$Value <- netnat$Value*1000000

netnat$Value <- (netnat$Value/popu$Value)


ggplot(netnat, aes(x = date, y = Value, by = Country)) +
  geom_line(aes(color = Country)) +
  facet_wrap(~ Country) +
  labs(title = "Net National Income per capita over time by country",
       y = "US Dollar", 
       x = "Year",
       caption = "Source: OECD") + 
  theme(legend.position = "none")

###########

### Making list ####

list_netnat <- netnat %>%
  group_by(Country)

list_netnat <- group_split(list_netnat)

list_netnat <- setNames(list_netnat,c("AU", "BE", "DK", "FR", "GE", "IT", "NL",
                                                "NZ", "NO", "ES", "SE", "CH", "UK", "US"))
