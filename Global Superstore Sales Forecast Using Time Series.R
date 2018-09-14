#####################   Global Mart : Sales and Demand forecast   ######################

# Load the required libraries
library(dplyr)
library(forecast)  
library(tseries)
library(graphics)
library(ggplot2)

# Load data into data frame
global_superstore <- read.csv("Global Superstore.csv")
str(global_superstore)
summary(global_superstore)
sum(duplicated(global_superstore))               # No duplicates are present
sum(is.na(global_superstore))                    # 41296 NAs
sapply(global_superstore, function(x) sum(is.na(x)))     # All NAs in Postal.Code column

# Removinf column with NA values
sum(is.na(global_superstore$Postal.Code))/nrow(global_superstore) # 80% values NA in column
# Remove column = Postal.Code since it has maximum NAs
global_superstore$Postal.Code <- NULL
sum(is.na(global_superstore))                    # NAs none

# Standardising Date columns and creating new dimensions
date_cols <- c("Order.Date", "Ship.Date")
global_superstore[, date_cols] <- lapply(global_superstore[, date_cols], function(x) as.Date(x, "%d-%m-%Y"))
global_superstore$Order.Month <- as.numeric(format(global_superstore$Order.Date, "%m"))
global_superstore$Order.Year <- as.numeric(format(global_superstore$Order.Date, "%Y"))
summary(global_superstore)

########################### Data Preparation ########################################

# Taking the columns of interest together and creating a subset
cols_interest <- c("Segment", "Market", "Order.Month", "Order.Year", "Sales", "Quantity", "Profit")
global_superstore_subset <- select(global_superstore, cols_interest)

# Group the subset data by Segment, Market
global_superstore_groups <- split(global_superstore_subset,
                                  list(global_superstore_subset$Segment,
                                       global_superstore_subset$Market))

# Function to aggregate Sales,Quantity and Profit of each group by Month, Year

prepare_data <- function(df){
  new_df <- df %>%
    group_by(Segment, Market, Order.Month, Order.Year) %>%
    summarise_at(c("Sales", "Quantity", "Profit"), sum, na.rm=T)  %>%
    mutate_if(is.factor, as.character) %>%
    ungroup() %>%
    mutate(MonthNum = row_number()) %>%
    select(Market, Segment, MonthNum, Sales, Quantity, Profit) %>%
    as.data.frame()

  return(new_df)
}

# Function to calculate Coefficient of Variation

calculate_coeffOfvariation <- function(df){
  market <- df$Market[1]
  segment <- df$Segment[1]
  coeff_var <- sd(df$Profit)/mean(df$Profit)*100
  return(data.frame(Market = market, Segment = segment, Coeff.Of.Variation = coeff_var))
}

resulting_df <- data.frame(Market = character(),
                           Segment = character(),
                           MonthNum = integer(),
                           Sales = double(),
                           Quantity = double(),
                           Profit = double())
profit_CoeffOfVariation_df <- data.frame(Market = character(),
                                         Segment = character(),
                                         Coeff.Of.Variation = double())

for (group in global_superstore_groups) {
  group_df <- prepare_data(group)
  resulting_df <- rbind(resulting_df, group_df)

  profit_CoeffOfVariation_df <- rbind(profit_CoeffOfVariation_df, calculate_coeffOfvariation(group_df))
}

arrange(profit_CoeffOfVariation_df, Coeff.Of.Variation)


# Plot TotalProfit from the aggregated dataset
resulting_df %>%
  group_by(Market, Segment) %>%
  summarise(TotalProfit=sum(Profit),
            TotalSales = sum(Sales),
            TotalQuantity = sum(Quantity)) %>%
  ggplot(aes(x=Market, y=TotalProfit, fill=Segment)) +
    geom_bar(stat="identity", position="dodge")


# Looking at the plot, we find
# EU has made the highest profit in the Consumer segment
# APAC too has the highest total profit in the Consumer segment
# These two are the most consistent profitable groups.

eu_consumer_data <- filter(resulting_df, Market == "EU" & Segment == "Consumer") %>%
  select(MonthNum, Sales, Quantity)

apac_consumer_data <- filter(resulting_df, Market == "APAC" & Segment == "Consumer") %>%
  select(MonthNum, Sales, Quantity)

###################################################################################



############################# Model building  #####################################

######################   1. EU : Consumer Segment  ##############################
eu_consumer_total_rows <- nrow(eu_consumer_data)                  # 48 rows
eu_consumer_train_data <- eu_consumer_data[1:(eu_consumer_total_rows-6), ] # Training data
eu_consumer_test_data <- eu_consumer_data[(eu_consumer_total_rows-5):eu_consumer_total_rows, ] # Test data

######################   1a. EU : Consumer Sales     ###############################

# Prepare timeseries for Sales
eu_consumer_sales_timeser <- ts(eu_consumer_train_data$Sales)
plot(eu_consumer_sales_timeser)   # Plot the time series of EU sales

### Decompose timeseries to see the components

# Add frequency=12 to be able to decompose
eu_consumer_sales_timeser_freq12 <- ts(eu_consumer_train_data$Sales, frequency = 12)
eu_consumer_sales_timeser_decompose <- decompose(eu_consumer_sales_timeser_freq12)
plot(eu_consumer_sales_timeser_decompose)

# Decomposition of the pattern shows that
# 1. Trend is linearly increasing
# 2. Seasonality is also present as a sine curve

plot(eu_consumer_sales_timeser)

# Smoothen the timeseries
w <- 1
eu_consumer_sales_timeser_smooth <- stats::filter(eu_consumer_sales_timeser,
                                           filter=rep(1/(2*w+1), (2*w+1)),
                                           method="convolution",
                                           sides=2)
diff <- eu_consumer_sales_timeser_smooth[w+2] - eu_consumer_sales_timeser_smooth[w+1]

for(i in seq(w,1,-1)){
  eu_consumer_sales_timeser_smooth[i] <- eu_consumer_sales_timeser_smooth[i+1]-diff
}
n <- length(eu_consumer_sales_timeser)

timevals <- eu_consumer_train_data$MonthNum
timevals_test <- eu_consumer_test_data$MonthNum
diff <- eu_consumer_sales_timeser_smooth[n-w] - eu_consumer_sales_timeser_smooth[n-w-1]
for (i in seq(n-w+1,n)) {
  eu_consumer_sales_timeser_smooth[i] <- eu_consumer_sales_timeser_smooth[i-1]+diff
}
lines(eu_consumer_sales_timeser_smooth, col="red", lwd=2)

eu_consumer_sales_timeser_smoothdf <- as.data.frame(cbind(timevals, as.vector(eu_consumer_sales_timeser_smooth)))
colnames(eu_consumer_sales_timeser_smoothdf) <- c("MonthNum","Sales")
eu_consumer_sales_timeser_smoothdf$MonthNum <- as.numeric(eu_consumer_sales_timeser_smoothdf$MonthNum)
eu_consumer_sales_timeser_smoothdf$Sales <- as.numeric(eu_consumer_sales_timeser_smoothdf$Sales)
str(eu_consumer_sales_timeser_smoothdf)


#### Fitting an advanced linear regression model to extract trend
lmfit <- lm(eu_consumer_sales_timeser_smoothdf$Sales ~ sin(0.5*eu_consumer_sales_timeser_smoothdf$MonthNum) *
              poly(eu_consumer_sales_timeser_smoothdf$MonthNum,2) +
              cos(0.5*eu_consumer_sales_timeser_smoothdf$MonthNum) *
              poly(eu_consumer_sales_timeser_smoothdf$MonthNum,2) +
              sin(0.05*eu_consumer_sales_timeser_smoothdf$MonthNum)*eu_consumer_sales_timeser_smoothdf$MonthNum,
            data=eu_consumer_sales_timeser_smoothdf)
summary(lmfit)
accuracy(lmfit)

## Predict the trend
trend <- predict(lmfit, data.frame(x=timevals))
lines(timevals, trend, col="blue", lwd=2) # Fitting the predicted trend

# Performing a manual arima for stationary component analysis

resi <- eu_consumer_sales_timeser - trend
plot(resi, col="red")

#Calculating ACF and PACF and plotting graph
acf(resi)
acf(resi, type = "partial")  ## PACF plot
par("mar")
par(mar=c(1,1,1,1))

## Auto arima model

armafit <- auto.arima(resi)
tsdiag(armafit)
par(mar=c(5.1,4.1,4.1,2.1))
armafit           # ARIMA(0,0,0) with zero mean

# Auto arima
autoarima <- auto.arima(eu_consumer_sales_timeser)
autoarima
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")
accuracy(autoarima)         # MAPE=47.22186

# Forecast EU :Consumer Sales with Holt's Method of smoothening
eu_consumer_sales_forecast <- HoltWinters(eu_consumer_sales_timeser, beta = F, gamma = F)
eu_consumer_sales_forecast  
plot(eu_consumer_sales_forecast)
##Smoothing parameters:
#alpha: 0.3213469
#beta : FALSE
#gamma: FALSE


# Forecasting sales for the next 6 months
eu_consumer_sales_forecast_6months <- predict(eu_consumer_sales_forecast, n.ahead = 6)
eu_consumer_sales_forecast_6months
plot(eu_consumer_sales_forecast_6months)

accuracy(eu_consumer_sales_forecast_6months, eu_consumer_test_data$Sales) #MAPE=33.31919

# Use forecast to get next 6 months Sales
forecast(eu_consumer_sales_timeser, h=6)
forecast(autoarima, h=6)

######################################################################################

#############################   1b. EU Consumer Quantity START ######################

# Prepare timeseries for Quantity
eu_consumer_quantity_timeser <- ts(eu_consumer_train_data$Quantity)
plot(eu_consumer_quantity_timeser)

# Decompose timeseries to see the components
# Add frequency=12 to be able to decompose
eu_consumer_quantity_timeser_freq12 <- ts(eu_consumer_train_data$Quantity, frequency = 12)
eu_consumer_quantity_timeser_decompose <- decompose(eu_consumer_quantity_timeser_freq12)
plot(eu_consumer_quantity_timeser_decompose)

# Decomposition of the pattern shows that
# 1. Trend is linearly increasing
# 2. Seasonality is also present as a sine curve

plot(eu_consumer_quantity_timeser)

# Smoothening the timeseries
w <- 1
eu_consumer_quantity_timeser_smooth <- stats::filter(eu_consumer_quantity_timeser,
                                                  filter=rep(1/(2*w+1), (2*w+1)),
                                                  method="convolution",
                                                  sides=2)
diff <- eu_consumer_quantity_timeser_smooth[w+2] - eu_consumer_quantity_timeser_smooth[w+1]
for(i in seq(w,1,-1)){
  eu_consumer_quantity_timeser_smooth[i] <- eu_consumer_quantity_timeser_smooth[i+1]-diff
}
n <- length(eu_consumer_quantity_timeser)

timevals <- eu_consumer_train_data$MonthNum

diff <- eu_consumer_quantity_timeser_smooth[n-w] - eu_consumer_quantity_timeser_smooth[n-w-1]

for (i in seq(n-w+1,n)) {
  eu_consumer_quantity_timeser_smooth[i] <- eu_consumer_quantity_timeser_smooth[i-1]+diff
}
lines(eu_consumer_quantity_timeser_smooth, col="red", lwd=2)  #Plot the smoothened line

eu_consumer_quantity_timeser_smoothdf <- as.data.frame(cbind(timevals, as.vector(eu_consumer_quantity_timeser_smooth)))
colnames(eu_consumer_quantity_timeser_smoothdf) <- c("MonthNum","Quantity")
eu_consumer_quantity_timeser_smoothdf$MonthNum <- as.numeric(eu_consumer_quantity_timeser_smoothdf$MonthNum)
eu_consumer_quantity_timeser_smoothdf$Quantity <- as.numeric(eu_consumer_quantity_timeser_smoothdf$Quantity)
str(eu_consumer_quantity_timeser_smoothdf)

### Fitting an advanced linear regression to extract the trend
lmfit <- lm(eu_consumer_quantity_timeser_smoothdf$Quantity ~ sin(0.5*eu_consumer_quantity_timeser_smoothdf$MonthNum) *
              poly(eu_consumer_quantity_timeser_smoothdf$MonthNum,2) +
              cos(0.5*eu_consumer_quantity_timeser_smoothdf$MonthNum) *
              poly(eu_consumer_quantity_timeser_smoothdf$MonthNum,2) +
              sin(0.05*eu_consumer_quantity_timeser_smoothdf$MonthNum)*eu_consumer_quantity_timeser_smoothdf$MonthNum,
            data=eu_consumer_quantity_timeser_smoothdf)
summary(lmfit)
accuracy(lmfit)         #MAPE=10.62404


trend <- predict(lmfit, data.frame(x=timevals))
lines(timevals, trend, col="blue", lwd=2)  #Plot a smoothened curve

#Perform a manual arima for stationary component

resi <- eu_consumer_quantity_timeser - trend
plot(resi, col="red")  # Plot the stationary component

## Calculate the ACF and PACF and plot them for ARMA fitting
acf(resi)
acf(resi, type = "partial")  #PACF
par("mar") # 5.1 4.1 4.1 2.1
par(mar=c(1,1,1,1))

# Perform an ARMA fit
armafit <- auto.arima(resi)
tsdiag(armafit)
par(mar=c(5.1,4.1,4.1,2.1))
armafit # ARIMA(0,0,0) with zero mean

# Auto arima
autoarima <- auto.arima(eu_consumer_quantity_timeser)
autoarima
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")
accuracy(autoarima)  #MAPE=42.97133

# Forecast EU.Consumer Quantity
eu_consumer_quantity_forecast <- HoltWinters(eu_consumer_quantity_timeser, beta = F, gamma = F)
eu_consumer_quantity_forecast
plot(eu_consumer_quantity_forecast)
# Forecast for next 6 months
eu_consumer_quantity_forecast_6months <- predict(eu_consumer_quantity_forecast, n.ahead = 6)
eu_consumer_quantity_forecast_6months
plot(eu_consumer_quantity_forecast_6months)

accuracy(eu_consumer_quantity_forecast_6months, eu_consumer_test_data$Quantity) #MAPE=32.1775

# Use forecast to get next 6 months Quantity
forecast(eu_consumer_quantity_timeser, h=6)
forecast(autoarima, h=6)

####################################################################################

#################################   1. EU Consumer END   ##################################

##############################   2. APAC Consumer  ##################################

apac_consumer_total_rows <- nrow(apac_consumer_data) # 48 rows
apac_consumer_train_data <- apac_consumer_data[1:(apac_consumer_total_rows-6), ]
apac_consumer_test_data <- apac_consumer_data[(apac_consumer_total_rows-5):apac_consumer_total_rows, ]

###########################  2a. APAC Consumer Sales #############################

# Prepare timeseries for Sales
apac_consumer_sales_timeser <- ts(apac_consumer_train_data$Sales)
plot(apac_consumer_sales_timeser)

# Decompose timeseries to see the components
# Add frequency=12 to be able to decompose
apac_consumer_sales_timeser_freq12 <- ts(apac_consumer_train_data$Sales, frequency = 12)
apac_consumer_sales_timeser_decompose <- decompose(apac_consumer_sales_timeser_freq12)
plot(apac_consumer_sales_timeser_decompose)

# Decomposition of the pattern states that
# 1. Trend is linearly increasing
# 2. Seasonality is also present as sine curve

plot(apac_consumer_sales_timeser)

# Smoothen the timeseries
w <- 1
apac_consumer_sales_timeser_smooth <- stats::filter(apac_consumer_sales_timeser,
                                                  filter=rep(1/(2*w+1), (2*w+1)),
                                                  method="convolution",
                                                  sides=2)
diff <- apac_consumer_sales_timeser_smooth[w+2] - apac_consumer_sales_timeser_smooth[w+1]
for(i in seq(w,1,-1)){
  apac_consumer_sales_timeser_smooth[i] <- apac_consumer_sales_timeser_smooth[i+1]-diff
}
n <- length(apac_consumer_sales_timeser)

timevals <- apac_consumer_train_data$MonthNum

diff <- apac_consumer_sales_timeser_smooth[n-w] - apac_consumer_sales_timeser_smooth[n-w-1]
for (i in seq(n-w+1,n)) {
  apac_consumer_sales_timeser_smooth[i] <- apac_consumer_sales_timeser_smooth[i-1]+diff
}
lines(apac_consumer_sales_timeser_smooth, col="red", lwd=2) #Fit a smoothened line

apac_consumer_sales_timeser_smoothdf <- as.data.frame(cbind(timevals, as.vector(apac_consumer_sales_timeser_smooth)))
colnames(apac_consumer_sales_timeser_smoothdf) <- c("MonthNum","Sales")
apac_consumer_sales_timeser_smoothdf$MonthNum <- as.numeric(apac_consumer_sales_timeser_smoothdf$MonthNum)
apac_consumer_sales_timeser_smoothdf$Sales <- as.numeric(apac_consumer_sales_timeser_smoothdf$Sales)
str(apac_consumer_sales_timeser_smoothdf)

#Fitting an Advanced linear regression model to extract the trend
lmfit <- lm(apac_consumer_sales_timeser_smoothdf$Sales ~ sin(0.5*apac_consumer_sales_timeser_smoothdf$MonthNum) *
              poly(apac_consumer_sales_timeser_smoothdf$MonthNum,2) +
              cos(0.5*apac_consumer_sales_timeser_smoothdf$MonthNum) *
              poly(apac_consumer_sales_timeser_smoothdf$MonthNum,2) +
              sin(0.05*apac_consumer_sales_timeser_smoothdf$MonthNum)*apac_consumer_sales_timeser_smoothdf$MonthNum,
            data=apac_consumer_sales_timeser_smoothdf)
summary(lmfit)
accuracy(lmfit)        #MAPE=9.697995


trend <- predict(lmfit, data.frame(x=timevals))
lines(timevals, trend, col="blue", lwd=2)   ## Fit the trend

#Perform manual arima
resi <- apac_consumer_sales_timeser - trend
plot(resi, col="red")

# Calculate the ACF and PACF
acf(resi)
acf(resi, type = "partial")
par("mar")
par(mar=c(1,1,1,1))

# Fit an ARMA model
armafit <- auto.arima(resi)
tsdiag(armafit)
par(mar=c(5.1,4.1,4.1,2.1))
armafit            # ARIMA(0,0,0) with zero mean

# Auto arima fitting
autoarima <- auto.arima(apac_consumer_sales_timeser)
autoarima
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")
accuracy(autoarima)          #MAPE=47.38695

# Forecast APAC Consumer Sales
apac_consumer_sales_forecast <- HoltWinters(apac_consumer_sales_timeser, beta = F, gamma = F)
apac_consumer_sales_forecast
plot(apac_consumer_sales_forecast)

# Forecast APAC sales for next 6 months
apac_consumer_sales_forecast_6months <- predict(apac_consumer_sales_forecast, n.ahead = 6)
apac_consumer_sales_forecast_6months
plot(apac_consumer_sales_forecast_6months)

accuracy(apac_consumer_sales_forecast_6months, apac_consumer_test_data$Sales)  #MAPE=17.03751

# Use forecast to get next 6 months Sales
forecast(apac_consumer_sales_timeser, h=6)
forecast(autoarima, h=6)

###################################################################################

#########################   2b. APAC Consumer Quantity ################################

# Prepare timeseries for Quantity
apac_consumer_quantity_timeser <- ts(apac_consumer_train_data$Quantity)
plot(apac_consumer_quantity_timeser)

# Decompose timeseries to see the components
# Add frequency=12 to be able to decompose
apac_consumer_quantity_timeser_freq12 <- ts(apac_consumer_train_data$Quantity, frequency = 12)
apac_consumer_quantity_timeser_decompose <- decompose(apac_consumer_quantity_timeser_freq12)
plot(apac_consumer_quantity_timeser_decompose)

# Decomposition of pattern states that
# 1. Trend is linearly increasing
# 2. Seasonality is also present as sine curve

plot(apac_consumer_quantity_timeser)

# Smoothen the timeseries
w <- 1
apac_consumer_quantity_timeser_smooth <- stats::filter(apac_consumer_quantity_timeser,
                                                     filter=rep(1/(2*w+1), (2*w+1)),
                                                     method="convolution",
                                                     sides=2)
diff <- apac_consumer_quantity_timeser_smooth[w+2] - apac_consumer_quantity_timeser_smooth[w+1]
for(i in seq(w,1,-1)){
  apac_consumer_quantity_timeser_smooth[i] <- apac_consumer_quantity_timeser_smooth[i+1]-diff
}
n <- length(apac_consumer_quantity_timeser)

timevals <- apac_consumer_train_data$MonthNum
diff <- apac_consumer_quantity_timeser_smooth[n-w] - apac_consumer_quantity_timeser_smooth[n-w-1]
for (i in seq(n-w+1,n)) {
  apac_consumer_quantity_timeser_smooth[i] <- apac_consumer_quantity_timeser_smooth[i-1]+diff
}
lines(apac_consumer_quantity_timeser_smooth, col="red", lwd=2)

apac_consumer_quantity_timeser_smoothdf <- as.data.frame(cbind(timevals, as.vector(apac_consumer_quantity_timeser_smooth)))
colnames(apac_consumer_quantity_timeser_smoothdf) <- c("MonthNum","Quantity")
apac_consumer_quantity_timeser_smoothdf$MonthNum <- as.numeric(apac_consumer_quantity_timeser_smoothdf$MonthNum)
apac_consumer_quantity_timeser_smoothdf$Quantity <- as.numeric(apac_consumer_quantity_timeser_smoothdf$Quantity)
str(apac_consumer_quantity_timeser_smoothdf)

#Fitting an advanced linear regression model to extract the trend
lmfit <- lm(apac_consumer_quantity_timeser_smoothdf$Quantity ~ sin(0.5*apac_consumer_quantity_timeser_smoothdf$MonthNum) *
              poly(apac_consumer_quantity_timeser_smoothdf$MonthNum,2) +
              cos(0.5*apac_consumer_quantity_timeser_smoothdf$MonthNum) *
              poly(apac_consumer_quantity_timeser_smoothdf$MonthNum,2) +
              sin(0.05*apac_consumer_quantity_timeser_smoothdf$MonthNum)*apac_consumer_quantity_timeser_smoothdf$MonthNum,
            data=apac_consumer_quantity_timeser_smoothdf)
summary(lmfit)
accuracy(lmfit)    #MAPE=8.358348


trend <- predict(lmfit, data.frame(x=timevals))
lines(timevals, trend, col="blue", lwd=2)   ### Plot the trend

#Perform manual arima
resi <- apac_consumer_quantity_timeser - trend
plot(resi, col="red")  #Residual plot

#Calculate ACF and PACF
acf(resi)
acf(resi, type = "partial")
par("mar") # 5.1 4.1 4.1 2.1
par(mar=c(1,1,1,1))

#Arma fit
armafit <- auto.arima(resi)
tsdiag(armafit)
par(mar=c(5.1,4.1,4.1,2.1))
armafit               # ARIMA(0,0,0) with zero mean

# Auto arima
autoarima <- auto.arima(apac_consumer_quantity_timeser)
autoarima
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")
accuracy(autoarima)       #MAPE=45.7269

# Forecast APAC Consumer Quantity
apac_consumer_quantity_forecast <- HoltWinters(apac_consumer_quantity_timeser, beta = F, gamma = F)
apac_consumer_quantity_forecast
plot(apac_consumer_quantity_forecast)  #Holt's method of smoothening

apac_consumer_quantity_forecast_6months <- predict(apac_consumer_quantity_forecast, n.ahead = 6)
apac_consumer_quantity_forecast_6months
plot(apac_consumer_quantity_forecast_6months)

accuracy(apac_consumer_quantity_forecast_6months, apac_consumer_test_data$Quantity)  #MAPE=19.17355

# Use forecast to get next 6 months Quantity
forecast(apac_consumer_quantity_timeser, h=6)
forecast(autoarima, h=6)

#########################################################################################

#############################################END#################################