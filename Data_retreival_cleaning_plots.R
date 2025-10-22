
library(WDI)
library(dplyr)
library(ggplot2)
library(tidyr)
library(corrplot)
library(countrycode)
library(plm)
library(sandwich)
library(lmtest)
library(broom)
library(ggrepel)
library(scales)
library(tidyverse)
library(bslib)
set.seed(2025)
##1 We will first load the dataset

indicators <- c(
  gdp = "NY.GDP.PCAP.KD",
  unemployment = "SL.UEM.TOTL.ZS",
  inflation = "FP.CPI.TOTL.ZG",
  life_exp = "SP.DYN.LE00.IN",
  labor_force = "SL.TLF.CACT.ZS"
)
raw_data<-WDI(country = "all",indicator = indicators,start=2015,end=2023,extra=TRUE)
head(raw_data)
print("We have successfully loaded the data")
## now we will clean it and make it suitable for plotting, we need to add a column of continent which we will add using iso3c and remove rows that contain NA values
df <- raw_data%>%
  filter(!is.na(iso3c), region != "Aggregates") %>%
  rename(
    Country = country, Year = year, GDP = gdp,
    Unemployment = unemployment, Inflation = inflation,
    LifeExp = life_exp, LaborForce = labor_force,
    Region = region, Income = income, iso3 = iso3c
  ) %>%
  select(Country, iso3, Region, Income, Year, GDP, Unemployment, Inflation, LifeExp, LaborForce) %>%
  mutate(Continent = countrycode(iso3, "iso3c", "continent")) %>%
  filter(!(is.na(GDP) & is.na(Unemployment) & is.na(Inflation)))
##The above code did not know about the iso3c code of CHI and XKX, therefore i will manually setting them
row_data[1,]
df[df$iso3=='XKX',]$Continent="Europe"
df[df$iso3=='CHI',]$Continent="Europe"
df <- df %>%
  group_by(Country) %>%
  mutate(across(c(GDP, Unemployment, Inflation, LifeExp, LaborForce), 
                ~ifelse(is.na(.x), mean(.x, na.rm=TRUE), .x))) %>%
  ungroup()

df$COVID <- ifelse(df$Year >= 2020, "Post-2020", "Pre-2020")
df$logGDP=log(df$GDP+1)
latest_yr <- max(df$Year, na.rm = TRUE)
df_latest <- df %>% filter(Year == latest_yr)
df_latest$logGDP <- log(df_latest$GDP + 1)

write.csv(df, "cleaned_data_2.csv", row.names = FALSE)

cat("Data cleaned:", nrow(df), "rows |", n_distinct(df$Country), "countries\n\n")

vrs <- c("GDP", "Unemployment", "Inflation", "LifeExp", "LaborForce")
summary_stats <- df %>%
  select(Country, Year, all_of(vrs)) %>%
  pivot_longer(cols = all_of(vrs), names_to = "indicator", values_to = "value") %>%
  group_by(indicator) %>%
  summarise(
    n = sum(!is.na(value)),
    mean = mean(value, na.rm=TRUE),
    sd = sd(value, na.rm=TRUE),
    median = median(value, na.rm=TRUE),
    min = min(value, na.rm=TRUE),
    max = max(value, na.rm=TRUE)
  )

print(summary_stats)
write_csv(summary_stats, "summary_stats_global.csv")

###################################################################################################
#plotting the curves
