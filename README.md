By analyzing pre-bubble trends, fitting an appropriate model, and making forecasts up to the present, we can answer an important question: had any trends before the bubble began simply continued in an ordinary fashion where would housing prices be now?

### Data

The S&P/Case-Shiller Home Price Index (with January 2000 as baseline) from 1991 to 2002 exhibits a gradually upward trend as well as seasonal fluctuations:

![](https://raw.githubusercontent.com/ryan-quigley/ryan-quigley.github.io/master/images/index_plot-1.png)

In order to make forecasts of pre-bubble trends, the beginning of the housing bubble must be pinned down. 
There appears to be dramatic change in the rate of increase of housing prices around 1998, which may indicate 
the beginning of the housing bubble; however, many economic factors contribute to a bubble, and there is likely to be 
disagreement among economists on a specific start date. This [Shiny app](https://ryan-quigley.shinyapps.io/home-price-forecasts/) 
specifically was developed specifically for this purpose: let the user choose the start date of the bubble, and the forecasts will 
be automatically generated.

### Shiny App

The default model in the Shiny app is a SARIMA(1,1,0)(0,1,1) with period = 12, but I have provided options for customizing the 
model yourself: check the "Customize the model" checkbox and see how the results change in the various tabs.

### Conclusions

The resulting forecasts are highly dependent on the start date of the housing bubble. 
For example, a start date during 1997 or 1998 would lead us to believe that current housing prices are 
higher than they would have been without the bubble; whereas, a start date after 1999 suggests that housing prices have 
been corrected and current prices are lower than expected. Note: making forecasts so far ahead inevitably leads to prediction 
intervals that are extremely wide, so it would not be unreasonable to draw opposite conclusions to those above for the given 
start dates.
