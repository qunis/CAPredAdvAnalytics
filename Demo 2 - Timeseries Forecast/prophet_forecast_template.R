## Forecasting using Facebook's prophet library

library(tidyverse)
library(prophet)

# read data

df <- read_csv2('data/timeseries2.csv') %>% 
    group_by(time) %>% 
    summarise(value = sum(value)) %>% 
    rename(ds = time, y = value) 

# add prophet sample code here
# see https://cran.r-project.org/web/packages/prophet/vignettes/quick_start.html
# ...





###

# Output to file
write.csv2(forecast, 'forecast.csv')
