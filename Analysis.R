#### PACKAGES ####
library(plm);library(tseries);library(CADFtest);library(car);library(lmtest);library(taRifx);library(gtools);
library(dynlm);library(dplyr);library(fastStat);library(RFGLS);library(spatstat);library(sandwich);library(ARDL);
library(countrycode);library(ggplot2);library(data.table);library(strucchange);library(xts)

#### LIST FOR ALL VARIABLES ####

list_consum

list_house_price

list_netnat

list_share_price

##### UPLOAD DATA ON PUT IT IN LIST####

#data <- cbind.data.frame(consum$Country, consum$date,log(consum$Value), log(netnat$Value), log(house_price$Value), log(share_price$Value), logu2$e_squared, dd$fitted.values)

#names(data)[1] <- "Country"
#names(data)[2] <- "date"
#names(data)[3] <- "Consumption"
#names(data)[4] <- "Income"
#names(data)[5] <- "House_price"
#names(data)[6] <- "Share_price"
#names(data)[7] <- "esquared"
#names(data)[8] <- "w"


data <- read.csv("All-variables-in-one.csv", sep = ",")

list_of_country_codes <- c("AU", "BE", "DK", "FR", "GE", "IT", "NL","NZ", "NO", "ES", "SE", "CH", "GB", "US")

Country <- countrycode(c('Australia', 'Belgium' ,'Denmark', 'France' ,'Germany', 'Italy', 'Netherlands',
                         'New Zealand', 'Norway', 'Spain', 'Sweden', 'Switzerland', 'United Kingdom', 'United States'),
                       origin = 'country.name', destination = 'iso3c')

data_list <- data %>%
  group_by(Country)

data_list <- group_split(data_list)

data_list <- setNames(data_list,Country)

##### OLS ####
All_results <- data_list %>% 
  map(~ lm(Consumption ~ Income + House_price + Share_price, data = .x))


#stargazer(All_results[1:7], type = "latex", font.size = "small", single.row = TRUE, float.env = "sidewaystable",
#          omit.stat = c("ser", "f"), title = "OLS regression")

#stargazer(All_results[8:14], type = "latex", font.size = "small", single.row = TRUE, float.env = "sidewaystable",
#          omit.stat = c("ser", "f"), title = "OLS regression")



#### OLS ANTAGELSER####

#zero mean
res <- data_list %>% 
  map(~ lm(Consumption ~ Income + House_price + Share_price, data = .x)) %>% 
  map(summary) %>%
  map("residuals") %>%
  map_dbl(mean) %>%
  round

#heteroskedadisicity

het <- data_list %>% 
  map(~ bptest((lm(Consumption ~ Income + House_price + Share_price, data = .x))))

#serial correlation

seria <- data_list %>% 
  map(~ dwtest((lm(Consumption ~ Income + House_price + Share_price, data = .x))))

#multicollinearity

multi <- data_list %>% 
  map(~ car::vif(lm(Consumption ~ Income + House_price + Share_price, data = .x)))


#normality

Normality_ols <- data_list %>% 
  map(~ lm(Consumption ~ Income + House_price + Share_price, data = .x)) %>%
  map(summary) %>%
  map("residuals") %>%
  map(~ shapiro.test(.x))

#fix for heteroskedadisicity
logu2 <- data_list %>% 
  map(~ lm(Consumption ~ Income + House_price + Share_price, data = .x)) %>%
  map("residuals")

names(logu2)
  
logu2 = do.call(cbind, logu2) %>%
  as.data.frame() %>%
  mutate(date = seq(as.Date("1995/01/01"), as.Date("2017/10/01"), by = "quarter")) %>%
  gather(-c(date), key = Country, value = residuals) %>%
  mutate(e_squared = log(residuals^2)) %>%
  dplyr::select(-residuals, -date, -Country)

varreg <- data_list %>% 
  map(~ lm(esquared ~ Income + House_price + Share_price, data = .x))

w = do.call(cbind, varreg) %>%
  as.data.frame() %>%
  map("fitted.values") %>%
  map(exp)
  
dd = do.call(cbind, w) %>%
  as.data.frame() %>%
  mutate(date = seq(as.Date("1995/01/01"), as.Date("2017/10/01"), by = "quarter")) %>%
  gather(-c(date), key = Country, value = fitted.values) %>%
  dplyr::select(-date, -Country)

fgls <- data_list %>% 
  map(~ lm(Consumption ~ Income + House_price + Share_price, weight = 1/w, data = .x))

##### FGLS ANTAGELSER ####

fgls_serial <- data_list %>% 
  map(~ lm(Consumption ~ Income + House_price + Share_price, weight = 1/w, data = .x)) %>%
  map(summary) %>%
  map("residuals") %>%
  map(~runs.test(.x))

fgls_normal <- data_list %>% 
  map(~ lm(Consumption ~ Income + House_price + Share_price, weight = 1/w, data = .x)) %>%
  map(summary) %>%
  map("residuals") %>%
  map(~ shapiro.test(.x)) #check

fgls_res <- data_list %>% 
  map(~ lm(Consumption ~ Income + House_price + Share_price, weight = 1/w, data = .x)) %>%
  map(summary) %>%
  map("residuals") %>%
  map_dbl(mean) %>%
  round

fgls_multi <- data_list %>% 
  map(~ car::vif(lm(Consumption ~ Income + House_price + Share_price, weight = 1/w, data = .x)))


#stargazer(fgls[1:7], type = "latex", font.size = "small", single.row = TRUE, float.env = "sidewaystable",
#          omit.stat = c("ser", "f"), title = "FGLS-regression")

#stargazer(fgls[8:14], type = "latex", font.size = "small", single.row = TRUE, float.env = "sidewaystable",
#          omit.stat = c("ser", "f"), title = "FGLS-regression")





#### DIFF ALL THE VARIABLES ####

# diff all the variables

data_diff <- data %>%
  select(-esquared, -w)

data_list_diff <- data_diff %>%
  group_by(Country)

data_list_diff <- group_split(data_list_diff)

data_list_diff <- setNames(data_list_diff,Country)

data_list_diff_2 <- map(data_list_diff, ~ (.x %>%
                                             mutate_at(vars(-Country, -date), ~ (.x %>% (function(x){c(NA,diff(x))}))) %>%
                                             .[-1,]))

#### FIRST DIFFERENCE OLS ####

All_results_diff <- data_list_diff_2 %>% 
  map(~ lm(Consumption ~ Income + House_price + Share_price, data = .x))

#stargazer(All_results_diff[1:7], type = "latex", font.size = "small", single.row = TRUE, float.env = "sidewaystable",
#          omit.stat = c("ser", "f"))

#stargazer(All_results_diff[8:14], type = "latex", font.size = "small", single.row = TRUE, float.env = "sidewaystable",
#          omit.stat = c("ser", "f"))

#### FIRST DIFFERENCE OLS ANTAGELSER ####

#Zero conditional mean

res_diff <- data_list_diff_2 %>% 
  map(~ lm(Consumption ~ Income + House_price + Share_price, data = .x)) %>% 
  map(summary) %>%
  map("residuals") %>%
  map_dbl(mean) %>%
  round

#heteroskedadisicity

het_diff <- data_list_diff_2 %>% 
  map(~ bptest((lm(Consumption ~ Income + House_price + Share_price, data = .x))))

#serial correlation

seria_diff <- data_list_diff_2 %>% 
  map(~ dwtest((lm(Consumption ~ Income + House_price + Share_price, data = .x))))

#multicollinearity

multi_diff <- data_list_diff_2 %>% 
  map(~ car::vif(lm(Consumption ~ Income + House_price + Share_price, data = .x)))

#Normality

normality_diff <- data_list_diff_2 %>% 
  map(~ lm(Consumption ~ Income + House_price + Share_price, data = .x)) %>%
  map(summary) %>%
  map("residuals") %>%
  map(~ shapiro.test(.x))

##### PLOTS ####

plot_df_consumption <- ggplot(bind_rows(data_list_diff_2), aes(date, Consumption , colour=Country, by = Country)) +
  geom_line() +
  facet_wrap(~ Country) + 
  labs(y = "Consumption", 
       x = "") +
  theme(legend.position = "none")

plot_df_income <- ggplot(bind_rows(data_list_diff_2), aes(date, Income , colour=Country, by = Country)) +
  geom_line() +
  facet_wrap(~ Country) + 
  labs(y = "Income", 
       x = "") +
  theme(legend.position = "none")

plot_df_house_price <- ggplot(bind_rows(data_list_diff_2), aes(date, House_price , colour=Country, by = Country)) +
  geom_line() +
  facet_wrap(~ Country) +
  labs(y = "House Price", 
       x = "År") +
  theme(legend.position = "none")

plot_df_share_price <- ggplot(bind_rows(data_list_diff_2), aes(date, Share_price , colour=Country, by = Country)) +
  geom_line() +
  facet_wrap(~ Country) + 
  labs(y = "Share Price", 
       x = "År",
       caption = "Source: OECD") +
  theme(legend.position = "none")

#arranged <- ggarrange(plot_df_consumption, plot_df_income, plot_df_house_price, plot_df_share_price)

#ggsave("samlet-plot.pdf",arranged,width = 40,height = 40,units = "cm",path ="~/Desktop")

#### ERROR CORRECTION MODEL ####

data_list_t <- map(data_list, ~ (.x %>%
                                 mutate_at(vars(-Country, -date), ~ (.x %>%
                                                                       (function(x){zoo(x, order.by = data_list$AUS$date)}))))) #Transform to time-series data

ecm_model <- data_list_t %>%
  map(~ (dynlm(d(Consumption) ~ d(Income) + d(House_price) + d(Share_price) +
                 L(Consumption, 1) + L(Income, 1) + L(House_price, 1) + L(Share_price, 1), data = .x)))

#stargazer(ecm_model[1:7], type = "latex", font.size = "scriptsize", single.row = TRUE, float.env = "sidewaystable",
#          omit.stat = c("ser", "f"))

#stargazer(ecm_model[8:14], type = "latex", font.size = "scriptsize", single.row = TRUE, float.env = "sidewaystable",
#          omit.stat = c("ser", "f"))

##### ECM ANTAGELSER #####

ecm_model_serial <- data_list_t %>%
  map(~ (dynlm(d(Consumption) ~ d(Income) + d(House_price) + d(Share_price) +
                 L(Consumption, 1) + L(Income, 1) + L(House_price, 1) + L(Share_price, 1), data = .x))) %>%
  map(~ dwtest(.)) #ITA & BEL - serial correlation

ecm_model_bp <- data_list_t %>%
  map(~ (dynlm(d(Consumption) ~ d(Income) + d(House_price) + d(Share_price) +
                 L(Consumption, 1) + L(Income, 1) + L(House_price, 1) + L(Share_price, 1), data = .x))) %>%
  map(~ bptest(.)) #NOR & FRA - hetero

ecm_model_res <- data_list_t %>%
  map(~ (dynlm(d(Consumption) ~ d(Income) + d(House_price) + d(Share_price) +
                 L(Consumption, 1) + L(Income, 1) + L(House_price, 1) + L(Share_price, 1), data = .x))) %>%
  map(summary) %>%
  map("residuals") %>%
  map_dbl(mean) %>%
  round #zero mean

ecm_model_multi <- data_list_t %>%
  map(~ (dynlm(d(Consumption) ~ d(Income) + d(House_price) + d(Share_price) +
                 L(Consumption, 1) + L(Income, 1) + L(House_price, 1) + L(Share_price, 1), data = .x))) %>%
  map(~ car::vif(.x)) #Ser fine ud

ecm_model_normal <- data_list_t %>%
  map(~ (dynlm(d(Consumption) ~ d(Income) + d(House_price) + d(Share_price) +
                 L(Consumption, 1) + L(Income, 1) + L(House_price, 1) + L(Share_price, 1), data = .x))) %>%
  map(summary) %>%
  map("residuals") %>%
  map(~ jarque.bera.test(.x)) #ikke normal - ITA, NLD, NZL, ESP


#### ARDL OG ECM MED AUTO.ARDL PAKKE ####

ardl_list <- data_list_t %>%
  map(~ auto_ardl(Consumption ~ Income + House_price + Share_price, data = .x, max_order = 5)) %>%
  map("best_model") %>%
  map(summary)

uecm_list <- data_list_t %>%
  map(~ auto_ardl(Consumption ~ Income + House_price + Share_price, data = .x, max_order = 5)) %>%
  map("best_model") %>%
  map(~uecm(.x))

recm_list <- data_list %>%
  map(~ auto_ardl(Consumption ~ Income + House_price + Share_price, data = .x, max_order = 5)) %>%
  map("best_model") %>%
  map(recm)

bound_test_ardl <- data_list %>%
  map(~ auto_ardl(Consumption ~ Income + House_price + Share_price, data = .x, max_order = 5)) %>%
  map("best_model") %>%
  map(bounds_f_test, case = 2)

bound_test_ecm <- uecm_list %>%
  map(bounds_f_test, case =3, alpha = 0.01)


multipliers_test_ardl <- data_list %>%
  map(~ auto_ardl(Consumption ~ Income + House_price + Share_price, data = .x, max_order = 5)) %>%
  map("best_model") %>%
  map(multipliers)

#### NY METODE




#data_list_diff_2 <- map(data_list_diff, ~ (.x %>%
#  mutate_at(vars(-Country, -date), ~ (.x %>% (function(x){c(NA,diff(x))}))) %>%
#  mutate_at(vars(-Country, -date), ~ (.x %>% (function(x){xts(x, order.by = date)}))) %>%
#  .[-1,]))