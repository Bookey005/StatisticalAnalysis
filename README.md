# StatisticalAnalysis
***I used Demo_R.csv for addressing the statiscal analysis part of the question, I saved the codes as an r source file

***Please ensure to set the working directory and installing all required packages before running the solution file(OPABISI_Task2.R).

I read in my data and performed data inspection
i then check for missing values and outliers on the dataset
created a dataframe to filter each country from the overall dataset


Using the describe function, i performed the descriptive statistical analysis on the overall dataset and each country respectively
For comparison, grouping by country i did the mean & standard deviation for the factors selected based on my objective 
(Population growth and factors contributing to its increase)


Performed normality checks using the shapiro.test function, histogram and density plot on the countries selected for analysis
So i used the cor function for correlation analysis on each of the countries and corrplot for displaying the correlation matrix for each of them
Then finally used correlation function to compute the correlation coefficient details alongside the p-values for the countries individually.


Based on the country of choice, i ensured the correlation analysis for the countries still exists, 
Then i performed my regression analysis on the factors that best fits the objectives for each of the countries
After which i summarised, visualised each output and carried out Multicolinearity test, making sure all required assumptions are met for the analysis.


I selected the least country from the mean computed for the time-series analysis
Using the holt-winters and Auto-arima approach, i performed a non-seasonal time-series analysis on the country data selected.
The analysis includes decomposition, exponential smoothing, forecast prediction and its improvement as well as the histogram plot for the forecasted errors


I performed Anova testing on my overall dataset by grouping per country and created a categorical level for my independent variable(Natural increase) into low or high category
using the summarize function i got the average for both dependent and independent factors then performed my classification
Then i applied all the ANOVA assumptions rule for confirmation whether or not Natural increase has a significant impact on population growth.
