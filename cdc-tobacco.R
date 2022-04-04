# Meta --------------------------------------------------------------------
## Title:         ECON 470 HW 3
## Author:        Genia Kim
# Preliminaries -----------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, 
               gdata, scales, ivreg)

#Load data
cdc_tax_data <- readRDS("~/R/cdc-tobacco/cdc-tax-data.rds")

#Summarize the data
#1. Present a line graph showing the average number of packs sold per capita from 1970 to 2018.

graph1 <- cdc_tax_data %>%
  ungroup() %>%
  group_by(Year) %>%
  ggplot(aes(x=Year, y= sales_per_capita, group=1)) +
  stat_summary(fun="mean", geom="line", na.rm=TRUE) +
  labs(
    x="Year",
    y="Sales per Capita",
    title="Average Cigarette Pack Sales Per Capita"
  )+
  scale_x_continuous(breaks = seq(1970, 2020,5))
graph1

#2. Present a bar graph showing the proportion of states with a change in their cigarette tax in each year from 1970 to 1985.
graph2 <- cdc_tax_data %>%
  arrange(state, Year) %>%
  group_by(state) %>%
  mutate(
    change = tax_state - lag(tax_state)
  ) %>%
  select(state,Year,tax_state,change)%>%
  filter(Year <= 1985) %>%
  mutate(p_change = ifelse(change>0,1,0)) %>%
  replace(is.na(.), 0) %>%
  ggplot(aes(x=Year, y=p_change)) +
  stat_summary(fun = "mean", geom="bar") +
  labs(
    x="Year",
    y="Proportion of States",
    title="States with a Change in Cigarette Taxes by Year"
  ) +
  #  scale_x_continuous(breaks = seq(1970,1985)) 
  scale_y_continuous(breaks = seq(0, 0.3,.05), labels = scales::number_format(accuracy = 0.01))
graph2

# 3. Plot the average tax (in 2012 dollars) on cigarettes from 1970 to 2018.
graph3 <- cdc_tax_data %>%
  mutate(tax_dollar_cpi=tax_dollar*(229.59392/index)) %>%
  ggplot(aes(x=Year, y=tax_dollar_cpi)) +
  stat_summary(fun = "mean", geom="line") +
  labs(
    x="Year",
    y="Tax per pack ($)",
    title="Average Tax (in 2012 Dollars) on Cigarettes 1970-2018"
  )+
  scale_x_continuous(breaks = seq(1970, 2020,5)) +
  scale_y_continuous(breaks = seq(0, 3,.25), labels = scales::number_format(accuracy = 0.01))
graph3


#4. Plot the average price of a pack of cigarettes from 1970 to 2018. Over what period did prices appear to increase the most?
graph4 <- cdc_tax_data %>%
  mutate(pack_price12=cost_per_pack*(229.59392/index)) %>%
  ggplot(aes(x=Year, y=pack_price12)) +
  stat_summary(fun = "mean", geom="line") +
  labs(
    x="Year",
    y="Cost per Pack",
    title="Average Price of a Pack of Cigarettes (2012 dollars) 1970-2018"
  )+
  scale_x_continuous(breaks = seq(1970, 2020,5)) +
  scale_y_continuous(breaks = seq(0, 10), labels = scales::number_format(accuracy = 0.01))
graph4

#answer question, federal tax= big jump in taxes

#5. Identify the 5 states with the highest increases in cigarette prices (in dollars) over the time period. 
#Plot the average number of packs sold per capita for those states from 1970 to 2018.
high5 <- cdc_tax_data %>%
  mutate(pack_price12=cost_per_pack*(229.59392/index)) %>%
  filter(Year <= "1970" | Year >= "2018") %>%
  arrange(state, Year) %>%
  group_by(state) %>%
  mutate(
    change = pack_price12 - lag(pack_price12)
  ) %>%
  arrange(desc(change)) %>%
  group_by(state)%>%
  filter(state!='District of Columbia')

top5 <- high5[1:5, 1]
top5

g5 <- cdc_tax_data %>%
  ungroup() %>%
  group_by(state) %>%
  select(state, Year, sales_per_capita)

top5 <- unlist(top5)

graph5 <- g5[is.element(g5$state, top5),] %>%
  ungroup() %>%
  group_by(state) %>%
  ggplot(aes(x=Year, y=sales_per_capita, color = state)) +
  stat_summary(fun = "mean", geom="line") +
  labs(
    x="Year",
    y="Sales per Capita",
    title="Average Cigarette Sales per Capita for States with Greatest Price Increase"
  )+
  scale_x_continuous(breaks = seq(1970, 2020,10))
#  scale_y_continuous(breaks = seq(0, 10), labels = scales::number_format(accuracy = 0.01))
graph5


#6. Identify the 5 states with the lowest increases in cigarette prices over the time period. 
#Plot the average number of packs sold per capita for those states from 1970 to 2018.
low5<- high5 %>%
  na.omit()

bottom5<- high5[46:50, 1]
bottom5
bottom5 <- unlist(bottom5)

graph6 <- g5[is.element(g5$state, bottom5),] %>%
  ungroup() %>%
  group_by(state) %>%
  ggplot(aes(x=Year, y=sales_per_capita, color = state)) +
  stat_summary(fun = "mean", geom="line") +
  labs(
    x="Year",
    y="Sales per Capita",
    title="Average Cigarette Sales per Capita for States with Lowest Increase"
  )+
  scale_x_continuous(breaks = seq(1970, 2020,10))
#  scale_y_continuous(breaks = seq(0, 10), labels = scales::number_format(accuracy = 0.01))
graph6


#7. Compare the trends in sales from the 5 states with the highest price
#increases to those with the lowest price increases.
list <- c(top5,bottom5)
graph7 <- g5[is.element(g5$state, list),] %>%
  ungroup() %>%
  mutate(price = ifelse(is.element(state,top5),"High Increase","Low Increase"))%>%
  group_by(price) %>%
  ggplot(aes(x=Year, y=sales_per_capita, group=price, color = price)) +
  stat_summary(fun = "mean", geom="line") +
  labs(
    x="Year",
    y="Sales per Capita",
    title="Trends in Sales by Price Change (1970-2018)"
  )+
  scale_x_continuous(breaks = seq(1970, 2020,10))+
  scale_y_continuous(breaks = seq(0, 150, 25))
graph7



# Estimate ATEs
# Now let’s work on estimating a demand curve for cigarettes. 
#Specifically, we’re going to estimate the price elasticity of demand for cigarettes. 
#When explaining your findings, try to limit your discussion just to a couple of sentences.


#1. Focusing only on the time period from 1970 to 1990, regress log sales
#on log prices to estimate the price elasticity of demand over that period. Interpret your results.

data <- cdc_tax_data %>%
  filter("1970" <= Year & Year <= "1990") %>%
  mutate(ln_sales=log(sales_per_capita),
         ln_price_cpi = log(cost_per_pack*(229.59392/index)),
         ln_price = log(cost_per_pack),
         tax_cpi = tax_state*(229.59392/index),
         total_tax_cpi = tax_dollar*(229.59392/index),
         ln_state_tax = log(tax_cpi)
  )
ols <- lm(ln_sales ~ ln_price_cpi, data = data)
summary(ols)


#2. Again limiting to 1970 to 1990, regress log sales on log prices using 
#the total (federal and state) cigarette tax (in dollars) as an instrument for log prices. 
#Interpret your results and compare your estimates to those without an instrument. Are they different? If so, why?

ivs <- ivreg(ln_sales ~ ln_price_cpi | total_tax_cpi, data=data)
summary(ivs)


#3. Show the first stage and reduced-form results from the instrument.

#-outcome of interest and instrument: reduced form 

step1 <- lm(ln_price ~ total_tax_cpi, data=data)
pricehat <- predict(step1)
step2 <- lm(ln_sales ~ pricehat, data=data)
summary(step2)


#4. Repeat questions 1-3 focusing on the period from 1991 to 2015.

data2 <- cdc_tax_data %>%
  filter("1991" <= Year & Year <= "2015") %>%
  mutate(ln_sales=log(sales_per_capita),
         ln_price_cpi = log(cost_per_pack*(229.59392/index)),
         ln_price = log(cost_per_pack),
         tax_cpi = tax_state*(229.59392/index),
         total_tax_cpi = tax_dollar*(229.59392/index),
         ln_state_tax = log(tax_cpi))

ols2 <- lm(ln_sales ~ ln_price_cpi, data = data2)
summary(ols2)

ivs2 <- ivreg(ln_sales ~ ln_price_cpi | total_tax_cpi, data=data2)
summary(ivs2)

step1_2 <- lm(ln_price ~ total_tax_cpi, data=data2)
pricehat <- predict(step1_2)
step2_2 <- lm(ln_sales ~ pricehat, data=data2)
summary(step2_2)

#5. Compare your elasticity estimates from 1970-1990 versus those from 1991-2015. 
#Are they different? If so, why?





rm(list=c("cdc-tax-data", "data",
          "data2", "g5", "high5", "low5"))
save.image("cig.RData")

