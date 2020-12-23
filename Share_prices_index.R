setwd("~/Desktop")

tester <-read.csv("shareindex.csv", sep = ",")
tester$date <- seq(as.Date("1995/03/31"), as.Date("2019/12/31"), by = "quarter")

tester <- tester %>%
  select(-LOCATION) %>%
  select(-SUBJECT) %>%
  select(-Subject) %>%
  select(-FREQUENCY) %>%
  select(-Frequency) %>%
  select(-TIME) %>%
  select(-Time) %>%
  select(-Unit.Code) %>%
  select(-Unit) %>%
  select(-PowerCode.Code) %>%
  select(-PowerCode) %>%
  select(-Reference.Period.Code) %>%
  select(-Reference.Period) %>%
  select(-Flag.Codes) %>%
  select(-Flags)

tester <- tester[-c(601:700),] 

#ggplot(data = tester, aes(x = date, y = Value, color="Lande", gro)) +
 #geom_line() + facet_wrap( ~ Country)

ggplot(tester, aes(x = date, y = Value, by = Country)) +
  geom_line(aes(color = Country)) +
  facet_wrap(~ Country) +
  labs(title = "Share prices",
       y = "Index", 
       x = "Year")

