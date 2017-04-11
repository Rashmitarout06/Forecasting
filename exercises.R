#Exercise 2.8 

#Question 1: For each of the following series (from the fma package), make a graph of the data. 
#If transforming seems appropriate, do so and describe the effect.
#install.packages("fma")
library(fma)

# a.Monthly total of people on unemployed benefits in Australia (January 1956-July 1992).

#Using  data(package="fma") we get the list of datsets present in the fma package. 
#From the list we find that "dole" is the dataset for Unemployment benefits in Australia

plot(dole)
lambda <- BoxCox.lambda(dole)
plot(BoxCox(dole, lambda ),ylab="Number of People",xlab="Years")

# b.Monthly total of accidental deaths in the United States (January 1973-December 1978).

#From the list we find that "usdeaths" is the dataset for accidental deaths in the United States.

plot(usdeaths)
usdeathsComp <- decompose(usdeaths)
usdeathsAccidental <- usdeaths - usdeathsComp$seasonal
plot(usdeathsAccidental ,ylab="Number of People",xlab="Years")

# c.Quarterly production of bricks (in millions of units) at Portland, Australia (March 1956-September 1994).

#From the list we find that "usdeaths" is the dataset for Quarterly clay brick production at Australia.

plot(bricksq)
bricksqComp <- decompose(bricksq)
bricksProd <- bricksq - bricksqComp$seasonal
plot(bricksProd ,ylab="Number of Bricks in millions",xlab="Years")

#Question 2: Use the Dow Jones index (data set dowjones) to do the following:

#a.Produce a time plot of the series.

plot(dowjones)

#b.Produce forecasts using the drift method and plot them.

predictions <- rwf(dowjones, drift=TRUE)
plot(predictions)

#c.Show that the graphed forecasts are identical to extending the line drawn between the first and last observations.

x0 = 1
x1 = 78
y0 = 110.94
y1 = 121.23
segments(x0, y0, x1, y1)

#d.Try some of the other benchmark functions to forecast the same data set. Which do you think is best? Why?
#prediction from mean

prediction_1 <- meanf(dowjones)
plot(prediction_1)

#prediction from naive method

prediction_2 <- naive(dowjones)
plot(prediction_2)

#forecasts from seasonal naive method

prediction_3 <- snaive(dowjones)
plot(prediction_3)

#Question 3:Consider the daily closing IBM stock prices (data set ibmclose).

#a.Produce some plots of the data in order to become familiar with it.

plot(ibmclose)

#b.Split the data into a training set of 300 observations and a test set of 69 observations.

train_ibmdata <- window(ibmclose, start=c(1), end=c(300)) 
test_ibmdata <- window(ibmclose, start=c(300))

#c.Try various benchmark methods to forecast the training set and compare the results on the test set. Which method did best?

mean_ibmdata <- meanf(train_ibmdata, h=70)
accuracy(mean_ibmdata, test_ibmdata)


naive_ibmdata <- naive(train_ibmdata, h=70)
accuracy(naive_ibmdata, test_ibmdata)


drift_ibmdata <- rwf(train_ibmdata, h=70, drift=TRUE)
accuracy(drift_ibmdata, test_ibmdata)

#We know the best model is chosen from the above results which has relatively 
#lesser values of ME,RMSE,MAE,MPE,MAPE,MASE. 
#Looking at the accuracy measures we can see that the drift method performs the best
#out of the three benchmark models to forecast.



#Question 4: Consider the sales of new one-family houses in the USA, Jan 1973 - Nov 1995 (data set hsales).

#a.Produce some plots of the data in order to become familiar with it.

plot(hsales)

plot(decompose(hsales))

#b.Split the hsales data set into a training set and a test set, where the test set is the last two years of data.

train_hsales <- window(hsales, start=c(1973,1), end=c(1993,12))
test_hsales <- window(hsales, start=c(1993,12))

#c.Try various benchmark methods to forecast the training set and compare the results on the test set. Which method did best?

mean_hsales <- meanf(train_hsales, h=24)
accuracy(mean_hsales, test_hsales)


naive_hsales <- naive(train_hsales, h=24)
accuracy(naive_hsales, test_hsales)


snaive_hsales <- snaive(train_hsales, h=24)
accuracy(snaive_hsales, test_hsales)


drift_hsales <- rwf(train_hsales, h=24, drift=TRUE)
accuracy(drift_hsales, test_hsales)

##We know the best model is chosen from the above results which has relatively 
#lesser values of ME,RMSE,MAE,MPE,MAPE,MASE. 
#Looking at the accuracy measures we can see that the Seasonal naive method performs the best
#out of the four benchmark models to forecast.


#Exercise 4.10

#Question 1: Electricity consumption was recorded for a small town on 12 randomly chosen days. 
#The following maximum temperatures (degrees Celsius) and consumption (megawatt-hours) were recorded for each day. 

#Day	1	    2	    3	    4	    5	    6	    7	    8	    9	    10	  11	  12
#Mwh	16.3	16.8	15.5	18.2	15.2	17.5	19.8	19.0	17.5	16.0	19.6	18.0
#temp	29.3	21.7	23.7	10.4	29.7	11.9	9.0	  23.4	17.8	30.0	8.6	  11.8

#a.Plot the data and find the regression model for Mwh with temperature as an explanatory variable. 
#Why is there a negative relationship?

mwh <- c(16.3,	16.8,	15.5,	18.2,	15.2,	17.5,	19.8,	19.0,	17.5,	16.0,	19.6,	18.0)
temp <- c(29.3,	21.7,	23.7,	10.4,	29.7,	11.9,	9.0,	23.4,	17.8,	30.0,	8.6,	11.8)

elec_df <- data.frame(mwh, temp)

mwh <- ts(mwh)
temp <- ts(temp)

par(mfrow=c(2,1))
plot(mwh,ylab="consumption (megawatt-hours)",xlab="Days") 
plot(temp,ylab="temperatures (degrees Celsius)",xlab="Days")

elec_lm <- lm(mwh~temp)
par(mfrow=c(2,2))
plot(elec_lm)

summary(elec_lm)

#One of the possible reason for the negative relationship is that households consume less
#power when the temperature is warm outside. With warmer temperatures there is less need to consume
#energy to keep the house warm and it also increases the possibilities to go outside and enjoy the weather.
#Thus reducing the energy consumption on those days.

#b.Produce a residual plot. Is the model adequate? Are there any outliers or influential observations?

par(mfrow=c(1,1))
plot(fitted(elec_lm), residuals(elec_lm), xlab="Predicted scores", ylab="Residuals")

#The model is adequate enough as the majority of residuals lie between 1 and -1. Yes there is one outlier.

#c.Use the model to predict the electricity consumption that you would expect for a day with 
#maximum temperature 10 degrees and a day with maximum temperature 35 degrees. Do you believe these predictions?

new_pred <- data.frame(temp = c(10, 35))
predict(elec_lm, new_pred)

par(mfrow=c(1,1))
plot(elec_df$mwh, elec_df$temp,xlab="consumption (megawatt-hours)", ylab="temperatures (degrees Celsius)")

#d.Give prediction intervals for your forecasts.

forecast(elec_lm, new_pred)

#Question 2: The following table gives the winning times (in seconds) for the 
#men's 400 meters final in each Olympic Games from 1896 to 2012 (data set `olympic`).

#a.Update the data set `olympic` to include the winning times from the last few Olympics.

olympic_df <- data.frame(olympic)

olympic_updated <- data.frame(Year = c(2000, 2004, 2008, 2012, 2016), time = c(43.84, 44.00, 43.75, 43.94, 43.03))

olympic_new <- rbind(olympic_df, olympic_updated)

#b.Plot the winning time against the year. Describe the main features of the scatterplot.

plot(olympic_new$Year, olympic_new$time, xlab = "Year", ylab = "winning times (in seconds)")

#We see that there has been a downward trend throughout the period. The biggest time reduction was achieved 
#after the first year of olympics.The winning times have been decreasing over the years.

#c.Fit a regression line to the data. Obviously the winning times have 
#been decreasing, but at what *average* rate per year?

Olympic_lm <- lm(time~Year, olympic_new)

summary(Olympic_lm)

#Winning times have been decreased at a rate of -0.065 per year.

#d.Plot the residuals against the year. What does this indicate about the suitability of the fitted line?

plot(fitted(Olympic_lm), residuals(Olympic_lm),xlab="Predicted scores", ylab="Residuals")

#The residuals show a downward trend over time. A non-random pattern in the residuals
#indicates that the predictor variables of the model is not capturing some explanatory information. 

#e.Predict the winning time for the men's 400 meters final in the 2000, 2004, 2008 and 2012 Olympics. 
#Give a prediction interval for each of your forecasts. What assumptions have you made in these calculations?

old_df <- data.frame(olympic)
new_data <- data.frame(Year = c(2000, 2004, 2008, 2012))

forecast(Olympic_lm, new_data)


#f.Find out the actual winning times for these Olympics (see www.databaseolympics.com). 
#How good were your forecasts and prediction intervals?

actual <- c(43.84, 44.00, 43.75, 43.94)
preds <-forecast(Olympic_lm, new_data)

accuracy(preds, actual)

#Question 3: 

#Execise 5.8

#Question 1:The data below (data set fancy) concern the monthly sales figures of a shop 
#which opened in January 1987 and sells gifts, souvenirs, and novelties. The shop is 
#situated on the wharf at a beach resort town in Queensland, Australia. 
#The sales volume varies with the seasonal population of tourists. There is a large influx 
#of visitors to the town at Christmas and for the local surfing festival, held every March since 1988.
#Over time, the shop has expanded its premises, range of products, and staff.

#a.Produce a time plot of the data and describe the patterns in the graph. 
#Identify any unusual or unexpected fluctuations in the time series.

plot(fancy)

# We can see a clear seasonal pattern in the data. There is a spike in sales in the peak 
# Christmas season and another smaller bump in sales during March. The seasonal
# fluctuations and random fluctuations seem to increase with the level of the 
# time series.

#b.Explain why it is necessary to take logarithms of these data before fitting a model.

# The increasing seasonal fluctuations in the data means that it is necessary to take
# logarithms in order to get an additive model.

#c.Use R to fit a regression model to the logarithms of these sales data with a 
#linear trend, seasonal dummies and a "surfing festival" dummy variable.

fancy_log <- log(fancy)
dummy_data = rep(0, length(fancy))
dummy_data[seq_along(dummy_data)%%12 == 3] <- 1
dummy_data[3] <- 0
dummy_data <- ts(dummy_data, freq = 12, start=c(1987,1))
fancy_newdata <- data.frame(fancy_log,dummy_data)

sales_fit <- tslm(fancy_log ~ trend + season + dummy_data, data=fancy_newdata)

future_data <- data.frame(dummy_data = rep(0, 12))
future_data[3,] <- 1

forecast(sales_fit, newdata=future_data)

#d.Plot the residuals against time and against the fitted values.
#Do these plots reveal any problems with the model?

plot(residuals(sales_fit), type='p')

plot(as.numeric(fitted(sales_fit)), residuals(sales_fit), type='p')

#The residuals plotted against the fitted values show no pattern and vary from -0.03 to
# 0.03. Such a plot shows unbiased and homoscedastic residuals.
# The residuals plotted against time also vary from -0.03 to 0.03. There is a trend for
# the residuals to increase from 1991 to 1994. The residuals appear random prior to this period.

#e.Do boxplots of the residuals for each month. Does this reveal any problems with the model?

boxplot(resid(sales_fit) ~ cycle(resid(sales_fit)))

# The residuals for the second half of the year show greater variance (paticularly August,
# September and October) than the first half of the year, which suggests that our model 
# may not be capturing some information relevant to this time period.

#f.What do the values of the coefficients tell you about each variable?

summary(sales_fit)

#The value of the coefficients show how much the model thinks each month contributes
# to the conditional mean of the model. We can see that as the year progresses the size 
# of the coefficient increases. All months have positive coefficents and are statistically
# significant except for March, this is unsuprising given that the dummy variable occurs
# in this month.

#g.What does the Durbin-Watson statistic tell you about your model?

# dwtest(sales_fit, alt="two.sided")

#Durbin-Watson test

#data:  sales_fit
#DW = 0.88889, p-value = 1.956e-07
#alternative hypothesis: true autocorrelation is not 0

#The Durbin-Watson test shows that there is some autocorrelation remaining in the 
# residuals. This means there is some information remaining in the residuals that can 
# be exploited to obtain better forecasts.

#h.Regardless of your answers to the above questions, use your regression model to predict 
#the monthly sales for 1994, 1995, and 1996. Produce prediction intervals for each of your forecasts.

future_data <- data.frame(dummy_data = rep(0, 36))

pred_sales <- forecast(sales_fit, newdata=future_data)
pred_sales

#i.Transform your predictions and intervals to obtain predictions and intervals for the raw data.

sales_df <- as.data.frame(pred_sales)
sales_df <- exp(sales_df)
sales_df

#j.How could you improve these predictions by modifying the model?

#We could consider using a dynamic-regression model which works better when we have 
# autocorrelation remaining in the residuals.

#Question 2:The data below (data set texasgas) shows the demand for 
#natural gas and the price of natural gas for 20 towns in Texas in 1969.

#a.Do a scatterplot of consumption against price. The data are clearly not linear. 

texasgas_df <- (texasgas)
plot(texasgas_df$price, texasgas_df$consumption , xlab = "Price", ylab = "Consumption")

#b.Can you explain why the slope of the fitted line should change with P?

# The data is not linear so the slope needs to change in order to capture that information
# in our model.

#c.Fit the three models and find the coefficients, and residual variance in each case.

# First model - basic linear regression

texasgas_fit <- lm(consumption ~ exp(price), texasgas_df)

summary(texasgas_fit)

# Residual variance
(summary(texasgas_fit)$sigma)**2 
# 1101.359 is the residual variance.

#Second model - piecewise linear regression

texasgas_lin <- lm(consumption ~ price, texasgas_df)
library(segmented)
segmented.mod <- segmented(texasgas_lin, seg.Z = ~price, psi=60)

slope(segmented.mod)

# We see that the slope for B1 is -3.147 and B2 for it's -0.308. 

# Residual variance
(summary(segmented.mod)$sigma)**2 

# 167.8511. The residual variance is nearly ten times smaller than the basic linear model fitted above.

# Third model - polynomial regression

texasgas_poly <- lm(consumption ~ poly(price, 2), texasgas_df)

# Residual variance
(summary(texasgas_poly)$sigma)**2

# 206.5276 - As was the case with piecewise regression, a polynomial fit also greatly reduces residual variance.

#d.For each model, find the value of R2 and AIC, 
#and produce a residual plot. Comment on the adequacy of the three models.

# First model - basic linear regression

summary(texasgas_fit)

AIC(texasgas_fit)

resiplot_1 <- residuals(texasgas_fit)
plot(texasgas_fit$fitted.values, resiplot_1, ylab='residuals', xlab='fitted values',main='linear regression')
abline(0,0)

# Second model - piecewise linear regression

summary(texasgas_lin)

AIC(texasgas_lin)

resiplot_2 <- residuals(segmented.mod)
plot(segmented.mod$fitted.values, resiplot_2, ylab='residuals', xlab='fitted values', main='piecewise linear regression')
abline(0,0)

# Third model - polynomial regression.

summary(texasgas_poly)

AIC(texasgas_poly)

resiplot_3 <- residuals(texasgas_poly)
plot(texasgas_poly$fitted.values, resiplot_3, ylab='residuals', xlab='fitted values',main='polynomial linear regression')
abline(0,0)

# We see that all three of the residual plots show heteroskedasticity. 
# The linear regression model residual plot shows huge problems, nearly all the values are predicted to be
# around 71 and most of these predictions are wildly inaccurate. 
# The piecewise model has a slightly larger R^2 and a slightly lower AIC than the polynomial model so it is
# the best model of the three.

#e.For prices 40, 60, 80, 100, and 120 cents per 1,000 cubic feet, compute the forecasted 
# per capita demand using the best model of the three above.

# We just found that the Piecewisw model is the best among all the three models.

new_data <- data.frame(price=c(40, 60, 80, 100, 120))
predict(segmented.mod, new_data)

#f.Compute 95% prediction intervals. Make a graph of these prediction intervals and discuss their interpretation.

texasgas_new <- seq(min(new_data), max(new_data), length.out=5)
intervals <- predict(segmented.mod, new_data, interval="predict")
intervals

plot(consumption ~ price, data = texasgas_df, type = 'n')

polygon(c(rev(texasgas_new), texasgas_new), c(rev(intervals[ ,3]), intervals[ ,2]), col = 'pink', border = NA)

# We can see that the prediction intervals are fairly wide meaning we only have a rough
# idea of how much energy will be demanded.

#g.What is the correlation between P and P2? Does this suggest any general problem to 
# be considered in dealing with polynomial regressions---especially of higher orders?









