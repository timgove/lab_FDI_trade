#################################
#### Lab FDI and Trade Data
#### May 20, 2026 
#### Kyle Handley 
#### Version 1.2 
 


# clear environment
rm(list = ls())

 
# new packages we need for Census and BEA

#install.packages("censusapi")
library(censusapi)
#install.packages('bea.R')
library(bea.R)

library(fredr)
library(tidyverse)

## Part 3: Census Trade Data ----

# YOU MUST SPEND SOME TIME READING MANUAL HERE #

#Trade data details https://www.census.gov/data/developers/data-sets/international-trade.html



#naics basis
imports_naics<-getCensus(
  name = "timeseries/intltrade/imports/naics", # this says where to look there are dozens of options
  vars = c("GEN_VAL_MO"), #this is the variable you want: here General Imports by Month
  time = "from 2016",
  CTY_CODE="1220", #this is Canada, Census uses it's own 4 character codes
  CTY_CODE="2010", #this is China
  show_call = TRUE #useful to check or for replication later on different system
)
head(imports_naics)

# note there is also a cumulative import and expor value with suffix YR
imports_naics<-getCensus(
  name = "timeseries/intltrade/imports/naics",
  vars = c("GEN_VAL_YR","GEN_VAL_MO","YEAR"),
  time = "from 2023",
  CTY_CODE="1220",
  CTY_CODE="2010"
)
 

#to really save time, we want the cumulative value, for the month of December
#GEN_VAL_YR is cumulative imports for consumption by month
#the end of the year value for this is annual total, month=12
imports_naics<-getCensus(
  name = "timeseries/intltrade/imports/naics",
  vars = c("GEN_VAL_YR","YEAR"),
  time = "from 2016",
  CTY_CODE="1220",
  CTY_CODE="2010",
  MONTH = "12", ## this setting here only gets us year end values #
)

## I want a make a graph of the top 10 import partners for any given year

## so we will use the method above, but we want all countries
## we also need to screen out some regional codes again
## We also want to get the country names because the numericacodes 
# are not meaningful to non-specialists

imports_cty_yr<-getCensus(
  name = "timeseries/intltrade/imports/naics",
  vars = c("GEN_VAL_YR","YEAR","CTY_CODE","CTY_NAME"),
  time = "from 2000",
  MONTH = "12",
  show_call = TRUE
)

exports_cty_yr<-getCensus(
  name = "timeseries/intltrade/exports/naics",
  vars = c("ALL_VAL_YR","YEAR","CTY_CODE","CTY_NAME"),
  time = "from 2000",
  MONTH = "12",
  show_call = TRUE
)

head(imports_cty_yr)

head(exports_cty_yr)

#filter region and other aggregation codes#
#takea  look at the data you can see why I do this#
imports_cty_yr_clean <- imports_cty_yr %>%
  filter(!(substr(CTY_CODE, 1, 1) == "0" | substr(CTY_CODE, 2, 2) == "X" | substr(CTY_CODE,1,1)=="-"))

exports_cty_yr_clean <- exports_cty_yr %>%
  filter(!(substr(CTY_CODE, 1, 1) == "0" | substr(CTY_CODE, 2, 2) == "X" | substr(CTY_CODE,1,1)=="-"))

#also, the values for year and imports are not numeric
#we want to change that and convert to billions of dollars#
imports_cty_yr_clean <- imports_cty_yr_clean %>%
  mutate(GEN_VAL_YR = as.numeric(GEN_VAL_YR)/1000000000,
         YEAR = as.numeric(YEAR))

exports_cty_yr_clean <- exports_cty_yr_clean %>%
  mutate(ALL_VAL_YR = as.numeric(ALL_VAL_YR)/1000000000,
         YEAR = as.numeric(YEAR))
# Check for possible introduction of NAs due to conversion errors
sum(is.na(imports_cty_yr_clean$GEN_VAL_YR))

sum(is.na(exports_cty_yr_clean$ALL_VAL_YR))
#a different way to sort top 10 than we did for BEA
#here we use slice_max
top10_data <- imports_cty_yr_clean %>%
  group_by(YEAR) %>%
  slice_max(order_by = GEN_VAL_YR, n = 10, with_ties = FALSE) %>%
  arrange(YEAR, desc(GEN_VAL_YR)) 

top10_data_exports <- exports_cty_yr_clean %>%
  group_by(YEAR) %>%
  slice_max(order_by = ALL_VAL_YR, n = 10, with_ties = FALSE) %>%
  arrange(YEAR, desc(ALL_VAL_YR)) 
# View the top 10 data
print(top10_data)

print(top10_data_exports)
#this assigns every country to a ranking
#the ranking can change over time so it will depend on the year
#how a graph ultimately looks.
top10_data<-top10_data%>%
  group_by(YEAR) %>%
  arrange(-GEN_VAL_YR, CTY_NAME) %>%
  mutate(rank = row_number()) %>%
  ungroup()

top10_data_exports<-top10_data_exports%>%
  group_by(YEAR) %>%
  arrange(-ALL_VAL_YR, CTY_NAME) %>%
  mutate(rank = row_number()) %>%
  ungroup()
#set the year
yrplot<-2023
ggplot(top10_data%>%filter(YEAR==yrplot),aes(group = CTY_NAME, y = rank)) +
  geom_tile(aes(x = GEN_VAL_YR/2, width=GEN_VAL_YR, height=.5, color = CTY_NAME, fill = CTY_NAME),show.legend = FALSE) +
  geom_text(aes(x = GEN_VAL_YR, y = rank, label = CTY_NAME), nudge_x=50, show.legend = FALSE) +
  scale_y_reverse(breaks = 1:10, minor_breaks = NULL)+
  labs(x = "Import Value (billions USD)", y = "Ranking by Imports", title = paste("Top 10 Countries for",yrplot)) +
  theme_minimal()

ggplot(top10_data_exports%>%filter(YEAR==yrplot),aes(group = CTY_NAME, y = rank)) +
  geom_tile(aes(x = ALL_VAL_YR/2, width=ALL_VAL_YR, height=.5, color = CTY_NAME, fill = CTY_NAME),show.legend = FALSE) +
  geom_text(aes(x = ALL_VAL_YR, y = rank, label = CTY_NAME), nudge_x=50, show.legend = FALSE) +
  scale_y_reverse(breaks = 1:10, minor_breaks = NULL)+
  labs(x = "Exports Value (billions USD)", y = "Ranking by Expots", title = paste("Top 10 Countries for",yrplot)) +
  theme_minimal()

## Part 5: Animated plot by year ----


# some bonus material you can fiddle around with

#you will need to install figski or gganimate if you don't have them#
library(gifski)
library(ggplot2)
library(gganimate)

p<-ggplot(top10_data,aes(group = CTY_NAME, y = rank)) +
  geom_tile(aes(x = GEN_VAL_YR/2, width=GEN_VAL_YR, height=.5, color = CTY_NAME, fill = CTY_NAME),show.legend = FALSE) +
  geom_text(aes(x = GEN_VAL_YR, y = rank, label = CTY_NAME), nudge_x=50, show.legend = FALSE) +
  scale_y_reverse(breaks = 1:10, minor_breaks = NULL)+
  labs(x = "Import Value (billions USD)", y = "Ranking by Imports", title = 'Top 10 Source Countries for U.S. Imports: Year {closest_state}') +
  theme_minimal()

p_exports<-ggplot(top10_data_exports,aes(group = CTY_NAME, y = rank)) +
  geom_tile(aes(x = ALL_VAL_YR/2, width=ALL_VAL_YR, height=.5, color = CTY_NAME, fill = CTY_NAME),show.legend = FALSE) +
  geom_text(aes(x = ALL_VAL_YR, y = rank, label = CTY_NAME), nudge_x=50, show.legend = FALSE) +
  scale_y_reverse(breaks = 1:10, minor_breaks = NULL)+
  labs(x = "Exports Value (billions USD)", y = "Ranking by Exports", title = 'Top 10 Source Countries for U.S. Exports: Year {closest_state}') +
  theme_minimal()

animated_plot <- p +
  transition_states(YEAR, transition_length = 2, state_length = 2, wrap = FALSE) +
  #transition_components(time=YEAR)+
  ease_aes('linear')

animated_plot_exports <- p_exports +
  transition_states(YEAR, transition_length = 2, state_length = 2, wrap = FALSE) +
  #transition_components(time=YEAR)+
  ease_aes('linear')

# Create and save the animation using gifski
anim <- animate(animated_plot, nframes = 100, fps = 10, width = 800, height = 600, start_pause=10, end_pause = 10, renderer = gifski_renderer())
anim_save("top10_countries_over_time.gif", animation = anim)

anim <- animate(animated_plot_exports, nframes = 100, fps = 10, width = 800, height = 600, start_pause=10, end_pause = 10, renderer = gifski_renderer())
anim_save("top10_countries_over_time_exports.gif", animation = anim)