# TIME-SERIES-ANALYSIS-AND-PREDICTION-OF-BITCOIN-PRICING-TREND

# PROBLEM STATEMENT
With the rise of crypto currency markets the interest in creating automated trading strategies, or trading
bots, has grown. Developing algorithmic trading strategies however requires intensive backtesting to
ensure profitable performance. It follows that access to high resolution historical trading data is the
foundation of every successful algorithmic trading strategy. This project is aimed towards analysis and
prediction of Bitcoin pricing trend using time series analysis and various methods such as ARIMA,
fbprophet, Etc.

# METHODOLOGY
The data obtained is of the last 8 years approx i.e 2015-2022 of the most famous
cryptocurrency "Bitcoin", having attributes such as date itself, its opening bid price, high and
low of the days, close price and the volume of trade and our plan of approach are as follows-
● Adding an attribute of "percent change" on each day (as compared to last day close price
of-coarse)
● Load the required libraries and Check for null values
● Visualization of the BTC and its highest and lowest values across years
● At what days of the week it shows uptrend & downtrend more often (if it shows any such
specific trend at all)
● When it showed a dramatic bullish trend and the possible potential reason behind it.

# DATASET
Dataset link: https://finance.yahoo.com/quote/BTC-USD/history?p=BTC-USD

#CONCLUSION
We observed from our dataset that after 2020, the Bitcoin trend started to increase. The trend
normally peaks on Wednesday and generally troughs on Tuesday and Thursday. In terms of
months, the trend rises from March to April and troughs in May to June.
After making our dataset stationary and time series analysis ready, we used a variety of models,
including fbprophet with the accuracy of 87%, S naive, ETS, and ARIMA, we found that the
ARIMA model best fits our dataset, according to the Ljung Box test of fit.


