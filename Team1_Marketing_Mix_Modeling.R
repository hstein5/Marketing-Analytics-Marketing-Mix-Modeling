# Load all required packages
library(data.table)
library(dplyr)
library(stringr)
library(tidyverse)
library(cluster)
library(factoextra)
library(psych)
library(plyr)
library(arules)
library(reshape)
library(Metrics)
library(lmtest)

setwd("C:/Users/harsh/Desktop/MSBA/Spring/Marketing Analytics/Projects/Project 4")

# Load the datasets

transactions <- fread("transaction_table_supp.csv")
product <- fread("product_table_supp.csv")
seasonality <- fread("seasonality.csv")
holiday <- fread("holiday.csv")
promo <- fread("promo_ad.csv")

total_product = fread("product_table.csv")
total_transactions = fread("transaction_table.csv")


# 1.Data Exploration

# Category level transactional statistics - Beer in Pernalonga stores
total_product = total_product[total_product$category_desc_eng == "BEER WITH ALCOHOL"]
beer_idlist = total_product$prod_id

# Time period for total transactions is aligned with time period for transactions 
total_transactions = total_transactions[total_transactions$prod_id %in% beer_idlist]

total_transactions[, beer_sale:= sum(tran_prod_sale_amt)]
total_transactions[, beer_net_sale:= sum(tran_prod_paid_amt)]
total_transactions[, beer_sale_qty:= sum(tran_prod_sale_qty)]
total_transactions[, beer_avg_discount_amt:= -sum(tran_prod_discount_amt)/sum(tran_prod_sale_qty)]

total_transactions = unique(total_transactions[,c('beer_sale', 'beer_net_sale','beer_sale_qty','beer_avg_discount_amt')])

write.csv(total_transactions,"category_summary.csv", quote = FALSE, row.names = FALSE)

# Product level transactional statistics - For each beer product in Pernalonga stores
transactions[, mean_unit_price:= mean(prod_unit_price), by = c('prod_id')]
transactions[, median_unit_price:= median(prod_unit_price), by = c('prod_id')]
transactions[, min_unit_price:= min(prod_unit_price), by = c('prod_id')]
transactions[, max_unit_price:= max(prod_unit_price), by = c('prod_id')]
transactions[, total_quantity_sold:= sum(tran_prod_sale_qty), by = c('prod_id')]
transactions[, product_total_sales:= sum(tran_prod_sale_amt), by = c('prod_id')]
transactions[, product_total_net_sales:= sum(tran_prod_paid_amt), by = c('prod_id')]
transactions[, avg_discount_amt:= sum(tran_prod_discount_amt)/sum(tran_prod_sale_qty), by='prod_id']
transactions[, pct_sales_category:= product_total_net_sales/total_transactions$beer_sale, by='prod_id']
transactions[, pct_net_sales_category:= product_total_net_sales/total_transactions$beer_net_sale, by='prod_id']
transactions[, pct_sales_qty_category:= product_total_net_sales/total_transactions$beer_sale_qty, by='prod_id']

transactions_a = unique(transactions[,c('prod_id', 'mean_unit_price', 'median_unit_price', 'min_unit_price',
                                        'max_unit_price','total_quantity_sold', 'product_total_sales', 'product_total_net_sales',
                                        'avg_discount_amt', 'pct_sales_category', 'pct_net_sales_category', 'pct_sales_qty_category')])

write.csv(transactions_a,"product_summary.csv", quote = FALSE, row.names = FALSE)


# 1.1 Promotion Ads schedule
# First, we want to compute the days difference between Ads to discover how the promotion ads were scheduled.
promo$tran_wk = as.Date(promo$tran_wk)
promo$prod_assoc = as.integer(promo$prod_assoc)
daydifference = unique(promo[,c('tran_wk')])  
daydifference$lag_wk = unique(promo[,c('tran_wk')]) 
daydifference[,c('lag_wk')] = shift(daydifference[,c('lag_wk')], n=1,  type = "lag") 
daydifference[, daydiff:= difftime(tran_wk,lag_wk, tz, units = "days")] 

# The time difference between each Ad is 7 days. So, we can confirm that the promotion ads were distributed on a weekly basis. 
unique(daydifference$daydiff)

# Then, we'd like to learn if the promotion ads were distributed in each week between 2016 and 2017
unique(promo$tran_wk)
length(unique(promo$tran_wk))
min(promo$tran_wk)
max(promo$tran_wk)

# There are 106 unique tran_wk in promo, which is greater than the 105 weeks time difference between
# the earliest and latest date in the table. Thus, we can confirm the ads were distributed each week. 
difftime(min(promo$tran_wk), max(promo$tran_wk), tz, units = c("weeks"))

# 2.Data Preparation

# 2.1 Reach For TV & Radio
# Given the GRP and reach function for TV and Radio, we first computed the reach for each vehicle in each week.
# 2.1.1 Alpha of TV and Radio
# TV - 8 week half-life, Radio - 4 week half-life 
# calculate the alpha of TV and Radio based on half-life of each vehicle
a_TV = 1 - 0.5^(1/8)
a_Radio = 1 - 0.5^(1/4)

# 2.1.2 Adstock & Reach for TV
# Compute weekly adstock and reach for TV. The earlist distribution date of TV ads is 2016-06-05
TV = promo[promo$vehicle == "TV"]
wk = data.table(promo$tran_wk)
setnames(wk, 'V1', 'tran_wk') 
TV = unique(merge(wk, TV, by = 'tran_wk', all = TRUE))
TV[ ,ad_stock:= 0]
TV[ ,reach:= 0]
TV[is.na(TV)] <- 0
# Adstock - Adstock(t) = aGRP(t) + (1-a)Adstock(t-1)
# Reach for TV: Reach = 0.95(1-e^(-0.020GRP))
for (i in 2:length(TV$tran_wk)) {
  TV[i, 'ad_stock'] = a_TV * TV[i, amount] + (1-a_TV) * TV[i-1, 'ad_stock'] 
  TV[i, 'reach'] = 0.95 * (1 - exp(-0.02 * TV[i, 'ad_stock']))
} 

# 2.1.2 Adstock & Reach for Radio
# Compute weekly adstock and reach for Radio. The earlist distribution date of TV ads is 2016-06-05
Radio = promo[promo$vehicle == "Radio"]
wk = data.table(promo$tran_wk)
setnames(wk, 'V1', 'tran_wk')  
Radio = unique(merge(wk, Radio, by = 'tran_wk', all = TRUE))
Radio[,ad_stock:= 0]
Radio[,reach:= 0]
Radio[is.na(Radio)] <- 0
# Reach for Radio: Reach = 0.90(1-e^(-0.025GRP))
for (i in 2:length(Radio$tran_wk)) {
  Radio[i, 'ad_stock'] = a_Radio * Radio[i, amount] + (1-a_Radio) * Radio[i-1, 'ad_stock'] 
  Radio[i, 'reach'] = 0.95 * (1 - exp(-0.02 * Radio[i, 'ad_stock'])) 
} 
# Change column name of reach to "TV" and "Reach", so we can use them as model input variables
setnames(TV, 'reach', 'TV')  
setnames(TV, 'ad_stock', 'TV_adstock') 
setnames(Radio, 'reach', 'Radio') 
setnames(Radio, 'ad_stock', 'Radio_adstock') 

TV_Radio = merge(TV, Radio, by = 'tran_wk', all = TRUE)

# 2.2 Weekly Transactional Data
# 2.2.1 Change fisrt day of week
# The ad was given every Sunday, so we changed the first day of each week to Sunday to include the sales data
# within 7 days of the ad distribtion date.
library(lubridate)
transactions[, tran_wk:= floor_date(as.Date(tran_dt), "weeks", week_start = 7)]

# 2.2.2 Sale Volume and Average Discount
transactions[, weekly_sale_qty:= sum(tran_prod_sale_qty), by=c('prod_id','tran_wk')]
transactions[, weekly_avg_disc:= 1-sum(tran_prod_paid_amt)/sum(tran_prod_sale_amt), by=c('prod_id','tran_wk')]
transactions[, weekly_store_sales:= sum(tran_prod_paid_amt), by=c('prod_id','tran_wk','store_id')]

# 2.2.2 Weekly Shelf Price
transactions[, weekly_shelf_price:= mean(prod_unit_price), by=c('prod_id','tran_wk')]
# By checking weekly unit sale price across stores, we found the brand adopted uniform prices for 
# the three products. That said, the unit sale price at a given week is fixed and not impacted by store location.
transactions[, weekly_unique_shelf_price:= length(unique(prod_unit_price)), by=c('prod_id','tran_wk')]
max(transactions$weekly_unique_shelf_price)

# 2.3 Seasonality & Holiday
# 2.3.1 Include seasonality factor in the model input data
seasonality$tran_wk = as.Date(seasonality$tran_wk)
weekly_data = unique(transactions[, c('prod_id','weekly_sale_qty','weekly_avg_disc','weekly_shelf_price','tran_wk')])
weekly_data = merge(weekly_data, seasonality, by = 'tran_wk', all = TRUE)

# 2.3.2 Include Holiday factor in the model input data - each holiday will be used as a seperate variable in the model
holiday$tran_wk = as.Date(holiday$tran_wk)
weekly_data = merge(weekly_data, holiday, by = 'tran_wk', all = TRUE)

# 2.4 Marketing Vehicles
# Now we want to include all marketing vehicles in our model input data
# Only flyer and store display were not applied to all three products. Thus, we merge them to the
# model input data separately from the other vehicles.
# 2.4.1 Flyer
Flyer = promo[promo$vehicle == "Flyer"][, c('tran_wk','amount','prod_assoc')]
setnames(Flyer, 'amount', 'Flyer') 
setnames(Flyer, 'prod_assoc', 'prod_id') 
weekly_data = merge(weekly_data, Flyer, by = c('tran_wk','prod_id'), all = TRUE)

# 2.4.2 Store Display
Store_Display = promo[promo$vehicle == "Store Display"][, c('tran_wk','amount','prod_assoc')]
setnames(Store_Display, 'amount', 'Store_Display') 
setnames(Store_Display, 'prod_assoc', 'prod_id') 
weekly_data = merge(weekly_data, Store_Display, by = c('tran_wk','prod_id'), all = TRUE)

# 2.4.3 Additional vehicles
# Paid Search
Paid_Search = promo[promo$vehicle == "Paid Search"][, c('tran_wk','amount')]
setnames(Paid_Search, 'amount', 'Paid_Search') 
# Web Dislay
Web_Display = promo[promo$vehicle == "Web Display"][, c('tran_wk','amount')]
setnames(Web_Display, 'amount', 'Web_Display') 
# Email
Email = promo[promo$vehicle == "Email"][, c('tran_wk','amount')]
setnames(Email, 'amount', 'Email') 
# TV & Radio
TV_Radio = TV_Radio[, c('tran_wk', 'TV', 'Radio')]
additional = merge(TV_Radio, Paid_Search, by = 'tran_wk', all = TRUE)
additional = merge(additional, Web_Display, by = 'tran_wk', all = TRUE)
additional = merge(additional, Email, by = 'tran_wk', all = TRUE)

model_input = merge(weekly_data, additional, by = 'tran_wk', all = TRUE)
model_input = model_input[model_input$weekly_sale_qty>=0]
model_input[is.na(model_input)] <- 0

#We have data from 2015 that we need to remove - there is only one day worth of data from 2015 and that an skew our y variable (quantity). Also, there are limited promotions in this week
last_week_2015 <- min(model_input$tran_wk)

model_input <- model_input[model_input$tran_wk!= last_week_2015,]

#On running the models we found there are multicollinearity issues with the data - XMAS, PrXMAS and New Year are multicollinear with e-mail
#After comparing sales in final weeks of 2016 and 2017 against the previous weeks, we found that PrXMAS and NEWYEAR see a surge in sales, but XMAS does not. Hence, we remove XMAS from the model

#Remove XMAS and then remodel for the 3 products
model_input$holiday <- ifelse(model_input$holiday=="XMAS",0,model_input$holiday)

#Now run a model for each individual product predict weekly sales quantity, and then decompose it into the DueTos

# 3. Duetos 138936951
model_51 = model_input[prod_id == 138936951]
dim(model_51)
# transform y
model_51[, max_sales_qty := max(weekly_sale_qty) * 1.2] # assume that the maximum sales quantity is 20% more than historical maximum
model_51[, sales_trfm := log(weekly_sale_qty / (max_sales_qty - weekly_sale_qty))] 
model_51[is.na(model_51)] <- 0 


#building regressions. Using sales qty as y and all other promotion ads as Xs
formula <- sales_trfm ~ weekly_avg_disc + weekly_shelf_price + Flyer +  Web_Display + Paid_Search + Email+ TV + Radio + seas_index + holiday  # no Store Display for 138936951
lm <- lm(formula, data = model_51)
summary(lm)

#use logit function to bound the prediction, and present the interaction more clearly, since audience might exosed many different media such as radio and tv
model_51$pred <- predict(lm, newdata = model_51)
model_51[, pred := max_sales_qty * exp(pred) / (exp(pred) + 1)]

model_51$weekly_shelf_price[model_51$tran_wk == min(model_51$tran_wk)]

# decomposit DueTo
# baseline is base price + seasonailty + holiday, set the price first appeared in the time period as the base price
model_pred <- copy(model_51)
model_pred[, weekly_shelf_price := model_51$weekly_shelf_price[model_51$tran_wk == min(model_51$tran_wk)]]

model_pred[, c('weekly_avg_disc', 'Email', 'Flyer', 'Paid_Search', 'Web_Display', 'TV', 'Radio')] <- 0
model_51$base <- predict(lm, newdata = model_pred)
model_51[, base := max_sales_qty * exp(base) / (exp(base) + 1)]

# calculate Due_to_Price by calculating difference between the estimated sales qty using real price in the period and 
#estimated sales qty with the base price in place of the weekly price while all other other values are the same
model_pred <- copy(model_51)
model_pred[, weekly_shelf_price := model_51$weekly_shelf_price[model_51$tran_wk == min(model_51$tran_wk)]]# replace weekly price with base price, with other attributes fixed
model_51$due_to_base_price <- predict(lm, newdata = model_pred)
model_51[, due_to_base_price := max_sales_qty * exp(due_to_base_price) / (exp(due_to_base_price) + 1)]
model_51[, due_to_Price := pred - due_to_base_price]
model_51[, due_to_base_price := NULL]

# same as calculation for Due_to_Price, set base discount to 0
model_pred <- copy(model_51)
model_pred[, weekly_avg_disc := 0] # set discount to 0
model_51$due_to_weekly_discount <- predict(lm, newdata = model_pred)
model_51[, due_to_weekly_discount := max_sales_qty * exp(due_to_weekly_discount) / (exp(due_to_weekly_discount) + 1)]
model_51[, due_to_weekly_discount := pred - due_to_weekly_discount]
model_51

# Flyer + Email + Web_Display + Paid_Search + TV + Radio.
for (media in c('Flyer', 'Email', 'Web_Display', 'Paid_Search', 'TV', 'Radio')) {
  model_pred <- copy(model_51)
  model_pred[, c(media) := 0] # set media to 0
  col_name = paste('due_to', media, sep = '_')
  model_51[, c(col_name) := predict(lm, newdata = model_pred)]
  model_51[, c(col_name)] <- model_51$max_sales_qty * exp(model_51[, .SD, .SDcols = c(col_name)]) / (exp(model_51[, .SD, .SDcols = c(col_name)]) + 1)
  model_51[, c(col_name)] <- model_51$pred - model_51[, .SD, .SDcols = c(col_name)]
}

# Dibias DueTos by scaling to bring sum of duetos equal to actual sales  - more in detail in the report
model_51[, sum := base + due_to_Price + due_to_weekly_discount + due_to_Flyer + due_to_Email + due_to_Web_Display + due_to_Paid_Search + due_to_TV + due_to_Radio]
model_51[, .(sum, weekly_sale_qty)]

for (col in c('base', 'due_to_Price', 'due_to_weekly_discount', 'due_to_Flyer', 'due_to_Email', 'due_to_Web_Display', 'due_to_Paid_Search', 'due_to_TV', 'due_to_Radio')) {
  model_51[, c(col)] <- model_51[, .SD, .SDcols = c(col)] / model_51$sum * model_51$weekly_sale_qty
}
colnames(model_51)


ggplot(model_51, aes(x = tran_wk, y = base)) + geom_line()
ggplot(model_51, aes(x = tran_wk, y = weekly_sale_qty)) + geom_line()

write.csv(model_51, '138936951.csv', row.names = FALSE)


# use Dubin-Watson test to check autocorrelation 
# DW = 1.8188, p-value = 0.1623
model_51$residual = model_51$sales_trfm - model_51$pred
dwtest(model_51$sales_trfm ~ model_51$residual)

# Model Evaluation
rmse(model_51$weekly_sale_qty,model_51$pred)
mape(model_51$weekly_sale_qty,model_51$pred)
mae(model_51$weekly_sale_qty,model_51$pred)

# Model diagnostic with VIF - Check for multicollinearity

#All below 5 (highest is 3.1)
VIF(lm)

#Dueto 138936952 
model_52 = model_input[prod_id == 138936952]

dim(model_52)
ggplot(model_52, aes(x = tran_wk, y = weekly_sale_qty)) + geom_line()


# transform y
model_52[, max_sales_qty := max(weekly_sale_qty) * 1.2] # Assumption here : set max sales as 20% more than maximum from past records
model_52[, sales_trfm := log(weekly_sale_qty / (max_sales_qty - weekly_sale_qty))] # It is bounded and replace 1 with max_sales_qty*1.2 to ensure it is positive inside logarithm

# format
model_52[is.na(model_52)] <- 0
colnames(model_52)
formula <- sales_trfm ~ weekly_shelf_price + weekly_avg_disc + Flyer + Email + Web_Display + Paid_Search + Store_Display +  TV + Radio + seas_index + holiday 

lm <- lm(formula, data = model_52)
summary(lm)

model_52$pred <- predict(lm, newdata = model_52)
model_52[, pred := max_sales_qty * exp(pred) / (exp(pred) + 1)]

# base= base price + seasonailty + holiday
model_52_pred <- copy(model_52)
model_52_pred[, weekly_shelf_price := model_52$weekly_shelf_price[model_52$tran_wk == min(model_52$tran_wk)]]

model_52_pred[, c('weekly_avg_disc', 'Email', 'Flyer', 'Paid_Search', 'Web_Display', 'Store_Display', 'TV', 'Radio')] <- 0
model_52$base <- predict(lm, newdata = model_52_pred)
model_52[, base := max_sales_qty * exp(base) / (exp(base) + 1)]

# Due To Price
model_52_pred <- copy(model_52)
model_52_pred[, weekly_shelf_price := model_52$weekly_shelf_price[model_52$tran_wk == min(model_52$tran_wk)]]
model_52$due_to_base_price <- predict(lm, newdata = model_52_pred)
model_52[, due_to_base_price := max_sales_qty * exp(due_to_base_price) / (exp(due_to_base_price) + 1)]
model_52[, due_to_Price := pred - due_to_base_price]
model_52[, due_to_base_price := NULL]

# Due To weekly_discount
model_52_pred <- copy(model_52)
model_52_pred[, weekly_avg_disc := 0] # set weekly_discount to 0
model_52$due_to_weekly_discount <- predict(lm, newdata = model_52_pred)
model_52[, due_to_weekly_discount := max_sales_qty * exp(due_to_weekly_discount) / (exp(due_to_weekly_discount) + 1)]
model_52[, due_to_weekly_discount := pred - due_to_weekly_discount]
model_52

# Flyer + Email + Web_Display + Store_Display + Paid_Search + TV + Radio
for (media in c('Flyer', 'Email', 'Web_Display', 'Store_Display', 'Paid_Search', 'TV', 'Radio')) {
  model_52_pred <- copy(model_52)
  model_52_pred[, c(media) := 0] # set to 0
  col_name = paste('due_to', media, sep = '_')
  model_52[, c(col_name) := predict(lm, newdata = model_52_pred)]
  model_52[, c(col_name)] <- model_52$max_sales_qty * exp(model_52[, .SD, .SDcols = c(col_name)]) / (exp(model_52[, .SD, .SDcols = c(col_name)]) + 1)
  model_52[, c(col_name)] <- model_52$pred - model_52[, .SD, .SDcols = c(col_name)]
}

# Dibias DueTos by scaling to bring sum of duetos equal to actual sales  - more in detail in the report
model_52[, sum := base + due_to_Price + due_to_weekly_discount + due_to_Flyer + due_to_Email + due_to_Web_Display + due_to_Store_Display + due_to_Paid_Search + due_to_TV + due_to_Radio]
model_52[, .(sum, weekly_sale_qty)]
cor(model_52[, .(sum, pred, weekly_sale_qty)])

for (col in c('base', 'due_to_Price', 'due_to_weekly_discount', 'due_to_Flyer', 'due_to_Email', 'due_to_Web_Display', 'due_to_Paid_Search', 'due_to_TV', 'due_to_Radio')) {
  model_52[, c(col)] <- model_52[, .SD, .SDcols = c(col)] / model_52$sum * model_52$weekly_sale_qty
}



model_52[, .(pred, sum)]

ggplot(model_52, aes(x = tran_wk, y = base)) + geom_line()
ggplot(model_52, aes(x = tran_wk, y = weekly_sale_qty)) + geom_line()

write.csv(model_52, '138936952.csv', row.names = FALSE)

# use DWtest to calculate autocorrelation
model_52$residual = model_52$sales_trfm - model_52$pred
# DW = 2.0073, p-value = 0.4953
dwtest(model_52$sales_trfm ~ model_52$residual) 

# Model Evaluation
rmse(model_52$weekly_sale_qty,model_52$pred)
mape(model_52$weekly_sale_qty,model_52$pred)
mae(model_52$weekly_sale_qty,model_52$pred)

# Model diagnostic with VIF - Check for multicollinearity

#All below 5 (highest is 3.1)
VIF(lm)

#Dueto 138936953
model_53 = model_input[prod_id == 138936953]

dim(model_53)
ggplot(model_53, aes(x = tran_wk, y = weekly_sale_qty)) + geom_line()

# transform y
model_53[, max_sales_qty := max(weekly_sale_qty) * 1.2] # Assumption: 20% more than historical maximum
model_53[, sales_trfm := log(weekly_sale_qty / (max_sales_qty - weekly_sale_qty))] # It is bounded and replace 1 with max_sales_qty*1.2 to ensure it is positive inside logarithm

# format
model_53[is.na(model_53)] <- 0
colnames(model_53)
formula <- sales_trfm ~ weekly_shelf_price + weekly_avg_disc + Flyer + Email + Web_Display + Paid_Search + Store_Display + TV + Radio + seas_index + holiday

lm <- lm(formula, data = model_53)
summary(lm)

model_53$pred <- predict(lm, newdata = model_53)
model_53[, pred := max_sales_qty * exp(pred) / (exp(pred) + 1)]

# base: base price + seasonailty + holiday
model_53_pred <- copy(model_53)
model_53_pred[, weekly_shelf_price := model_53$weekly_shelf_price[model_53$tran_wk == min(model_53$tran_wk)]]

model_53_pred[, c('weekly_avg_disc', 'Email', 'Flyer', 'Paid_Search', 'Web_Display', 'Store_Display', 'TV', 'Radio')] <- 0
model_53$base <- predict(lm, newdata = model_53_pred)
model_53[, base := max_sales_qty * exp(base) / (exp(base) + 1)]

# Due To Price
model_53_pred <- copy(model_53)
model_53_pred[, weekly_shelf_price := model_53$weekly_shelf_price[model_53$tran_wk == min(model_53$tran_wk)]]
# use base price as weekly price, and other remain the same
model_53$due_to_base_price <- predict(lm, newdata = model_53_pred)
model_53[, due_to_base_price := max_sales_qty * exp(due_to_base_price) / (exp(due_to_base_price) + 1)]
model_53[, due_to_Price := pred - due_to_base_price]
model_53[, due_to_base_price := NULL]

# Due To weekly_discount
model_53_pred <- copy(model_53)
model_53_pred[,weekly_avg_disc := 0] # set weekly_discount to 0
model_53$due_to_weekly_discount <- predict(lm, newdata = model_53_pred)
model_53[, due_to_weekly_discount := max_sales_qty * exp(due_to_weekly_discount) / (exp(due_to_weekly_discount) + 1)]
model_53[, due_to_weekly_discount := pred - due_to_weekly_discount]
model_53

# Flyer + Email + Web_Display + Store_Display + Paid_Search + TV + Radio
for (media in c('Flyer', 'Email', 'Web_Display', 'Store_Display', 'Paid_Search', 'TV', 'Radio')) {
  model_53_pred <- copy(model_53)
  model_53_pred[, c(media) := 0] # make promo = 0
  col_name = paste('due_to', media, sep = '_')
  model_53[, c(col_name) := predict(lm, newdata = model_53_pred)]
  model_53[, c(col_name)] <- model_53$max_sales_qty * exp(model_53[, .SD, .SDcols = c(col_name)]) / (exp(model_53[, .SD, .SDcols = c(col_name)]) + 1)
  model_53[, c(col_name)] <- model_53$pred - model_53[, .SD, .SDcols = c(col_name)]
}

# Dibias DueTos by scaling to bring sum of duetos equal to actual sales  - more in detail in the report
model_53[, sum := base + due_to_Price + due_to_weekly_discount + due_to_Flyer + due_to_Email + due_to_Web_Display + due_to_Store_Display + due_to_Paid_Search + due_to_TV + due_to_Radio]


model_53[sum < 0, base := base - sum + 0.1]

model_53[, sum := base + due_to_Price + due_to_weekly_discount + due_to_Flyer + due_to_Email + due_to_Web_Display + due_to_Store_Display + due_to_Paid_Search + due_to_TV + due_to_Radio]

cor(model_53[, .(sum, pred, weekly_sale_qty)])

for (col in c('base', 'due_to_Price', 'due_to_weekly_discount', 'due_to_Flyer', 'due_to_Email', 'due_to_Web_Display', 'due_to_Paid_Search', 'due_to_TV', 'due_to_Radio')) {
  model_53[, c(col)] <- model_53[, .SD, .SDcols = c(col)] / model_53$sum * model_53$weekly_sale_qty
}

ggplot(model_53, aes(x = tran_wk, y = base)) + geom_line()
ggplot(model_53, aes(x = tran_wk, y = weekly_sale_qty)) + geom_line()
ggplot(model_53, aes(x = tran_wk, y = pred)) + geom_line()

model_53[which.max(base)]

write.csv(model_53, '138936953.csv', row.names = FALSE)

# Use DWtest to calculate autocorrelation
model_53$residual = model_53$sales_trfm - model_53$pred
# DW = 1.9115, p-value = 0.3055
dwtest(model_53$sales_trfm ~ model_53$residual) 

# Model Evaluation
rmse(model_53$weekly_sale_qty,model_53$pred)
mape(model_53$weekly_sale_qty,model_53$pred)
mae(model_53$weekly_sale_qty,model_53$pred)

# Model diagnostic with VIF - Check for multicollinearity

#All below 5 (highest is 4.6)
VIF(lm)

#Final duetos for all 3 products

final_duetos <- bind_rows(model_51,model_52,model_53)
write.csv(final_duetos, 'final_duetos.csv', row.names = FALSE)

