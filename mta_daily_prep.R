# prep script for the hourly MTA data file 
library(tidyverse)
# read the data file 
dd_df <- read.csv("Daily_Traffic_on_MTA_Bridges_&_Tunnels_20251030.csv")

# data read is good - now change the header names 
cnames <- c("Date", "ID", "Drct", "EZPass", "Cash")
colnames(dd_df) <- cnames   # update column names 

# cooool ... 
# change EZPass and Cash to numerics 
dd_df %>%
  mutate(EZPass = as.numeric(gsub(",", "", EZPass))) %>%
  mutate(Cash = as.numeric(gsub(",", "", Cash))) -> d_df

print( " ** Daily MTA file is good to go as d_df **")

