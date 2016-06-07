The PullStockMarketData.R script can be used to pull down a stock price and generates a 
training data set and a test data set. You must set the stock symbol on three different places in the script
in the "Parameters" section, once on line 9 and twice on line 16. You may set any date range (always setting 
the final date one day ahead) and indicate how many test observations you would like there to be. It will then 
pull a large number of market indicators (S&P500, price of gold, and london stock exchange) as well as stock 
features such as the MACD, BIAS, and moving averages. You may customize this easily to pull in different attributes.

The TestStrategy.R file can be used to test different model's performance on predicting the stock price using
the training and test data sets created with the above script. Choose the strategy which right now contains 3 
different models: logit regression, support vector machines, and quadratic discriminant analysis using 
preselected variables. On lines 38 - 43 you may choose between these models or add in your own model. The other 
parameters that need to be set are "equity", which represents your initial equity, "fee" which represents the 
percentage of equity that is lost on each trade, and "strat.strat" which is the strategy. You may choose "fixed" 
(generate the model once on the initial training data) or "dynamic", which will regenerate the model each day by 
creating a new model. Results will print.
