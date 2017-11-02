# ZillowAnalysis
An MSA project to practice modelling with regression techniques. The primary goal was to perform a linear regression on data captured from Zillow about a single property. This is located in the ZillowSingleHomeAnalysis. Going beyond this, added functionality was added toward the goal of capturing more data from zillow using the Jefferson County maps API to find addresses to search under the Zillow API. 

# How to follow along
For the main analysis, the following files were used: ZillowSingleHomeAnalysis.R, Functions.R, MSPNHSUS.csv. 

In order to use the file yourself you will need to comment out source('zillowAPIcredentials.R') and uncomment the following # set_zillow_web_service_id(ZWSID). Replace ZWSID with your own Zillow API key. This project stored that value in a file with was git ignored.

With those loaded into R it is possible to follow along with the regression process used. Two different regression models were created (one using last sale price and one not using it). 

# Other functions and files
Functions.R has a number of other functions. There are known bugs for Deep Searching Zillow and for searching for Updated Details. These are likely due to bad data being returned from the server for some properties. This may arise when addresses maintained by Jefferson County don't match the Zillow database. Due to time constraints on the project deadline, these bugs have not yet been fixed. They do not affect the single house analysis.

Due to function issues, the regression analysis in ZillowAnalysis.R is not complete. However, it can easily be transfered over from the single home analysis file. 

There is some data stored in my-db.sqlite file. This data can be accessed from the main R files. However, please not that SQLite does not support date formats. Therefore dates should be converted from R date format to character strings or else they may behave strangely. 
