library(urca)

setwd("~/Desktop/AAU OECON/5. semester/Empirisk projekt/Data")

data_ardl <- read.csv("ardl.csv")

data_ardl <- data_ardl %>%
  slice(1:92) #92 obs pr land.

ts_Consumption <- ts(data_ardl$Consumption, start = c(1995,1), end = c(2017,4), frequency = 4)
ts_Income <- ts(data_ardl$Income, start = c(1995,1), end = c(2017,4), frequency = 4)
ts_House_price <- ts(data_ardl$House_price, start = c(1995,1), end = c(2017,4), frequency = 4)
ts_Share_price <- ts(data_ardl$Share_price, start = c(1995,1), end = c(2017,4), frequency = 4)

#### ECM ######

cusom <- dynlm(ts_Consumption ~ L(ts_Consumption, 1) + ts_Income + L(ts_Income,1) + ts_House_price + L(ts_House_price,1) +
                 ts_Share_price + L(ts_Share_price,1), data = test)

lagged <- function(x, k) {
  if (k>0) {
    return (c(rep(NA, k), x)[1 : length(x)] );
  }
  else {
    return (c(x[(-k+1) : length(x)], rep(NA, -k)));
  }
}

lag_con <- ts(lagged(ts_Consumption,1), start =c(1995,1), end =c(2017,4), freq = 4)
lag_inc <- ts(lagged(ts_Income,1), start =c(1995,1), end =c(2017,4), freq = 4)
lag_house <- ts(lagged(ts_House_price,1), start =c(1995,1), end =c(2017,4), freq = 4)
lag_share <- ts(lagged(ts_Share_price,1), start =c(1995,1), end =c(2017,4), freq = 4)
d_con <- ts_Consumption - lag_con
d_inc <- ts_Income - lag_inc
d_house <- ts_House_price - lag_house
d_share <- ts_Share_price - lag_share

mudata=cbind(d_con, d_inc, d_house, d_share, lag_con, lag_inc, lag_house, lag_share)
mudata=na.omit(mudata)

#model_check <- lm(ts_Consumption~lag_con + ts_Income + lag_inc + ts_House_price + lag_house + ts_Share_price + lag_share)

struc <- d_con ~ lag_con + d_inc + lag_inc + d_house + lag_house + d_share + lag_share

stab <- efp(struc, type = "Rec-CUSUM", data = mudata)
plot(stab)

#### REC OG OLS CUSOM PLOT ####

#rec_aus <- efp(struc, type = "Rec-CUSUM", data = mudata)
#ols_aus <- efp(struc, type = "OLS-CUSUM", data = mudata)

#rec_bel <- efp(struc, type = "Rec-CUSUM", data = mudata)
#ols_bel <- efp(struc, type = "OLS-CUSUM", data = mudata)

#rec_dnk <- efp(struc, type = "Rec-CUSUM", data = mudata)
#ols_dnk <- efp(struc, type = "OLS-CUSUM", data = mudata)

#rec_fra <- efp(struc, type = "Rec-CUSUM", data = mudata)
#ols_fra <- efp(struc, type = "OLS-CUSUM", data = mudata)

#rec_deu <- efp(struc, type = "Rec-CUSUM", data = mudata)
#ols_deu <- efp(struc, type = "OLS-CUSUM", data = mudata)

#rec_ita <- efp(struc, type = "Rec-CUSUM", data = mudata)
#ols_ita <- efp(struc, type = "OLS-CUSUM", data = mudata)

#rec_nld <- efp(struc, type = "Rec-CUSUM", data = mudata)
#ols_nld <- efp(struc, type = "OLS-CUSUM", data = mudata)

#rec_nzl <- efp(struc, type = "Rec-CUSUM", data = mudata)
#ols_nzl <- efp(struc, type = "OLS-CUSUM", data = mudata)

#rec_nor <- efp(struc, type = "Rec-CUSUM", data = mudata)
#ols_nor <- efp(struc, type = "OLS-CUSUM", data = mudata)

#rec_esp <- efp(struc, type = "Rec-CUSUM", data = mudata)
#ols_esp <- efp(struc, type = "OLS-CUSUM", data = mudata)

#rec_swe <- efp(struc, type = "Rec-CUSUM", data = mudata)
#ols_swe <- efp(struc, type = "OLS-CUSUM", data = mudata)

#rec_che <- efp(struc, type = "Rec-CUSUM", data = mudata)
#ols_che <- efp(struc, type = "OLS-CUSUM", data = mudata)

#rec_gbr <- efp(struc, type = "Rec-CUSUM", data = mudata)
#ols_gbr <- efp(struc, type = "OLS-CUSUM", data = mudata)

#rec_usa <- efp(struc, type = "Rec-CUSUM", data = mudata)
#ols_usa <- efp(struc, type = "OLS-CUSUM", data = mudata)

par(mfrow=c(3,5))
plot(rec_aus, main = "Australia"); plot(rec_bel, main = "Belgium"); plot(rec_dnk, main = "Denmark"); plot(rec_fra, main = "France"); plot(rec_deu, main = "Germany"); plot(rec_ita, main = "Italy"); plot(rec_nld, main = "Netherlands"); plot(rec_nzl, main = "New Zealand"); plot(rec_nor, main = "Norway"); plot(rec_esp, main = "Spain"); plot(rec_swe, main = "Sweden"); plot(rec_che, main = "Switzerland"); plot(rec_gbr, main = "United Kingdom"); plot(rec_usa, main = "USA")


par(mfrow=c(3,5))
plot(ols_aus, main = "Australia"); plot(ols_bel, main = "Belgium"); plot(ols_dnk, main = "Denmark"); plot(ols_fra, main = "France"); plot(ols_deu, main = "Germany"); plot(ols_ita, main = "Italy"); plot(ols_nld, main = "Netherlands"); plot(ols_nzl, main = "New Zealand"); plot(ols_nor, main = "Norway"); plot(ols_esp, main = "Spain"); plot(ols_swe, main = "Sweden"); plot(ols_che, main = "Switzerland"); plot(ols_gbr, main = "United Kingdom"); plot(ols_usa, main = "USA")
plot(ols_aus)

#### ARDL ####

cusom <- dynlm(ts_Consumption ~ L(ts_Consumption, 1) + ts_Income + L(ts_Income,1) + ts_House_price + L(ts_House_price,1) +
                 ts_Share_price + L(ts_Share_price,1), data = test)

lagged <- function(x, k) {
  if (k>0) {
    return (c(rep(NA, k), x)[1 : length(x)] );
  }
  else {
    return (c(x[(-k+1) : length(x)], rep(NA, -k)));
  }
}

lag_con <- ts(lagged(ts_Consumption,1), start =c(1995,1), end =c(2017,4), freq = 4)
lag_inc <- ts(lagged(ts_Income,1), start =c(1995,1), end =c(2017,4), freq = 4)
lag_house <- ts(lagged(ts_House_price,1), start =c(1995,1), end =c(2017,4), freq = 4)
lag_share <- ts(lagged(ts_Share_price,1), start =c(1995,1), end =c(2017,4), freq = 4)

mudata=cbind(ts_Consumption, ts_Income, ts_House_price, ts_Share_price, lag_con, lag_inc, lag_house, lag_share)
mudata=na.omit(mudata)

model_check <- lm(ts_Consumption~lag_con + ts_Income + lag_inc + ts_House_price + lag_house + ts_Share_price + lag_share)

struc <- ts_Consumption ~ lag_con + ts_Income + lag_inc + ts_House_price + lag_house + ts_Share_price + lag_share

stab <- efp(struc, type = "OLS-CUSUM", data = mudata)
plot(stab)
