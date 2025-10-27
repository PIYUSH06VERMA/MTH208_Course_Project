
#load necessary libraries

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
raw_data[1,]
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

#1. Preston Curve (GDP vs LIFE EXPECTANCY)

cat("PRESTON CURVE\n")

p1 <- ggplot(df_latest, aes(x = GDP, y = LifeExp)) +
  geom_point(aes(color = Continent, size = GDP), alpha = 0.65) +
  geom_smooth(method = "loess", se = TRUE, color = "black", linewidth = 1.2) +
  geom_text_repel(data = df_latest %>% filter(GDP > 60000 | GDP < 1000 | LifeExp > 84),
                  aes(label = Country), size = 2.8, max.overlaps = 15) +
  scale_size_continuous(range = c(1, 8), guide = "none") +
  labs(title = "Preston Curve: Wealth and Longevity",
       subtitle = sprintf("Global data for %d", latest_yr),
       x = "GDP per capita", y = "Life Expectancy (years)", color = "Continent") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"))


print(p1)
ggsave("plot1_preston_curve.png", p1, width = 10, height = 7, dpi = 300)

# Preston by region
p2 <- ggplot(df_latest, aes(x = logGDP, y = LifeExp)) +
  geom_point(aes(color = Region), alpha = 0.7, size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 0.8) +
  facet_wrap(~Region, scales = "free") +  # Free both axes for better scaling
  labs(
    title = "Preston Curve by Region",
    x = "LOG GDP",
    y = "Life Expectancy"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold")
  )

print(p2)

ggsave("plot2_preston_regional.png", p2, width = 12, height = 8, dpi = 300)

#Regression Analysis: Preston Curve Model
preston_lm <- lm(LifeExp ~ logGDP + Unemployment + Inflation + LaborForce, data = df_latest)
preston_robust <- coeftest(preston_lm, vcov = vcovHC(preston_lm, type = "HC1"))
print(preston_robust)
write.csv(tidy(preston_lm), "preston_results.csv", row.names = FALSE)

#2. PHILLIPS CURVE (Inflation vs Unemployment)

cat("\n PHILLIPS CURVE\n")

p3 <- ggplot(df_latest, aes(x = Unemployment, y = Inflation)) +
  geom_point(aes(color = Continent, size = GDP), alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "red", linewidth = 1.2) +
  geom_text_repel(data = df_latest %>% filter(Inflation > 15 | Unemployment > 20),
                  aes(label = Country), size = 3, max.overlaps = 10) +
  scale_size_continuous(range = c(1, 8), guide = "none") +
  labs(title = "Phillips Curve: Inflation-Unemployment Tradeoff",
       subtitle = sprintf("Cross-section %d", latest_yr),
       x = "Unemployment (%)", y = "Inflation (%)", color = "Continent") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"))

print(p3)
ggsave("plot3_phillips_cross.png", p3, width = 10, height = 7, dpi = 300)

cat("\n PHILLIPS CURVE\n")
p3 <- ggplot(df_latest, aes(x = Unemployment, y = Inflation)) +
  geom_point(aes(color = Continent, size = GDP), alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "red", linewidth = 1.2) +
  geom_text_repel(data = df_latest %>% filter(Inflation > 15 | Unemployment > 20),
                  aes(label = Country), size = 3, max.overlaps = 10) +
  scale_size_continuous(range = c(1, 8), guide = "none") +
  coord_cartesian(xlim = c(0, 30), ylim = c(-5, 100)) +  # Limit axes
  labs(title = "Phillips Curve: Inflation-Unemployment Tradeoff",
       subtitle = sprintf("Cross-section %d", latest_yr),
       x = "Unemployment (%)", y = "Inflation (%)", color = "Continent") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"))
print(p3)
ggsave("plot3_phillips_cross.png", p3, width = 10, height = 7, dpi = 300)


ggsave("plot3_phillips_cross.png", p3, width = 10, height = 7, dpi = 300)

# Phillips over time
top_countries <- df %>% filter(Year == latest_yr) %>% arrange(desc(GDP)) %>% 
  slice(1:8) %>% pull(Country)
df_top <- df %>% filter(Country %in% top_countries)


df_india <- df %>% filter(Country == "India")

p4 <- ggplot(df_india, aes(x = Unemployment, y = Inflation)) +
  geom_path(aes(color = Year), linewidth = 1.2, arrow = arrow(length = unit(0.18, "cm"))) +
  geom_point(aes(color = Year), size = 3) +
  geom_text_repel(aes(label = Year), size = 3, color = "black") +
  scale_color_viridis_c(option = "plasma", direction = -1) +
  labs(
    title = "Phillips Curve Evolution for India (2015–2023)",
    subtitle = "Color gradient and arrows show temporal progression",
    x = "Unemployment Rate (%)",
    y = "Inflation Rate (%)",
    color = "Year"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "bottom"
  )

print(p4)




# Panel regression
pdata <- pdata.frame(df, index = c("Country", "Year"))
pdata$logGDP <- log(pdata$GDP + 1)
phillips_fe <- plm(Inflation ~ Unemployment + logGDP + LaborForce, data = pdata, 
                   model = "within", effect = "twoways")
phillips_se <- vcovHC(phillips_fe, type = "HC1", cluster = "group")
print(coeftest(phillips_fe, vcov = phillips_se))
write.csv(tidy(phillips_fe), "phillips_results.csv", row.names = FALSE)

#3. INFLATION PATTERNS (Global & Regional)


cat("\nINFLATION PATTERNS\n")

# Global inflation trend
global_infl <- df %>%
  group_by(Year) %>%
  summarise(Mean = mean(Inflation, na.rm = TRUE), Median = median(Inflation, na.rm = TRUE),
            SD = sd(Inflation, na.rm = TRUE)) %>%
  mutate(Lower = Mean - SD, Upper = Mean + SD)

p5 <- ggplot(global_infl, aes(x = Year)) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "blue", alpha = 0.3) +
  geom_line(aes(y = Mean), color = "blue", linewidth = 1.5) +
  geom_line(aes(y = Median), color = "darkred", linewidth = 1, linetype = "dashed") +
  geom_vline(xintercept = 2020, linetype = "dotted", color = "red", linewidth = 1) +
  annotate("text", x = 2020.5, y = max(global_infl$Upper), label = "COVID-19", 
           hjust = 0, color = "red", size = 4) +
  labs(title = "Global Inflation Trends (2015-2023)",
       subtitle = "Mean (solid) and Median (dashed) with SD band", y = "Inflation (%)") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))
print(p5)
ggsave("plot5_global_inflation.png", p5, width = 10, height = 6, dpi = 300)





#Regional inflation
regional_infl <- df %>%
  group_by(Region, Year) %>%
  summarise(Avg_Inflation = mean(Inflation, na.rm = TRUE), .groups = "drop")

p6 <- ggplot(regional_infl, aes(x = Year, y = Avg_Inflation, color = Region)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  geom_vline(xintercept = 2020, linetype = "dashed", color = "blue") +
  labs(title = "Regional Inflation Divergence", y = "Average Inflation (%)") +
  scale_color_brewer(palette = "Set2") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"))

print(p6)
ggsave("plot6_regional_inflation.png", p6, width = 11, height = 7, dpi = 300)


# Inflation heatmap
top30 <- df %>% filter(Year == latest_yr) %>% arrange(desc(GDP)) %>% 
  slice(1:30) %>% pull(Country)
heatmap_data <- df %>% filter(Country %in% top30) %>% select(Country, Year, Inflation)

p8 <- ggplot(heatmap_data, aes(x = Year, y = reorder(Country, Inflation), fill = Inflation)) +
  geom_tile(color = "white", linewidth = 0.5) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 2, limits = c(-5, 20)) +
  labs(title = "Inflation Heatmap: Top 30 Economies", x = "Year", y = "", fill = "Inflation (%)") +
  theme_minimal(base_size = 10) +
  theme(axis.text.y = element_text(size = 8))

print(p8)
ggsave("plot8_inflation_heatmap.png", p8, width = 10, height = 12, dpi = 300)

#4. COVID-19 IMPACT ANALYSIS


cat("\nCOVID-19 IMPACT\n")

# Pre vs Post comparison
covid_compare <- df %>%
  group_by(COVID) %>%
  summarise(GDP = mean(GDP, na.rm = TRUE), Unemployment = mean(Unemployment, na.rm = TRUE),
            Inflation = mean(Inflation, na.rm = TRUE), N = n())

print(covid_compare)
write.csv(covid_compare, "covid_comparison.csv", row.names = FALSE)

# 2019 vs 2020 shock
covid_shock <- df %>%
  filter(Year %in% c(2019, 2020)) %>%
  select(Country, Year, GDP, Unemployment, Continent) %>%
  pivot_wider(names_from = Year, values_from = c(GDP, Unemployment)) %>%
  mutate(GDP_drop = ((GDP_2020 - GDP_2019) / GDP_2019) * 100,
         Unemp_rise = Unemployment_2020 - Unemployment_2019) %>%
  filter(!is.na(GDP_drop), !is.na(Unemp_rise))

p9 <- ggplot(covid_shock, aes(x = GDP_drop, y = Unemp_rise)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(aes(color = Continent, size = GDP_2019), alpha = 0.6) +
  geom_text_repel(data = covid_shock %>% filter(abs(GDP_drop) > 8 | abs(Unemp_rise) > 4),
                  aes(label = Country), size = 2.8, max.overlaps = 15) +
  scale_size_continuous(range = c(1, 8), guide = "none") +
  labs(title = "COVID-19 Economic Shock (2019-2020)",
       x = "GDP Change (%)", y = "Unemployment Change (pp)", color = "Continent") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"))

print(p9)
ggsave("plot9_covid_shock.png", p9, width = 10, height = 7, dpi = 300)


# COVID timeline
covid_ts <- df %>%
  filter(Year >= 2018) %>%
  group_by(Year) %>%
  summarise(GDP = mean(GDP, na.rm = TRUE), Unemployment = mean(Unemployment, na.rm = TRUE),
            Inflation = mean(Inflation, na.rm = TRUE)) %>%
  pivot_longer(cols = c(GDP, Unemployment, Inflation), names_to = "Indicator", values_to = "Value")

p10 <- ggplot(covid_ts, aes(x = Year, y = Value)) +
  geom_line(linewidth = 1.2, color = "darkblue") +
  geom_point(size = 3, color = "darkblue") +
  geom_vline(xintercept = 2020, linetype = "dashed", color = "red") +
  annotate("rect", xmin = 2020, xmax = 2021, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "red") +
  facet_wrap(~Indicator, scales = "free_y", ncol = 1) +
  labs(title = "COVID-19 Timeline: Key Indicators", subtitle = "Global averages (2018-2023)", y = "Value") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

print(p10)
ggsave("plot10_covid_timeline.png", p10, width = 10, height = 9, dpi = 300)


# Regional COVID impact


regional_covid <- df %>%
  filter(Year %in% c(2019, 2020)) %>%
  group_by(Region, Year) %>%
  summarise(
    GDP = mean(GDP, na.rm = TRUE),
    Unemployment = mean(Unemployment, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = Year, values_from = c(GDP, Unemployment)) %>%
  mutate(
    GDP_change = ((GDP_2020 - GDP_2019) / GDP_2019) * 100,
    Unemp_change = Unemployment_2020 - Unemployment_2019
  )

# Adjust scaling so both series are comparable
scale_factor <- max(abs(regional_covid$GDP_change), na.rm = TRUE) / 
  max(abs(regional_covid$Unemp_change), na.rm = TRUE)

p11 <- ggplot(regional_covid, aes(x = reorder(Region, GDP_change))) +
  geom_col(aes(y = GDP_change), fill = "#2C7BB6", alpha = 0.8) +
  geom_point(aes(y = Unemp_change * scale_factor), color = "#D7191C", size = 3) +
  geom_text(aes(y = Unemp_change * scale_factor, 
                label = round(Unemp_change, 1)), 
            color = "#D7191C", hjust = -0.2, size = 3) +
  geom_hline(yintercept = 0, color = "grey40") +
  scale_y_continuous(
    name = "GDP Change (%)",
    sec.axis = sec_axis(~./scale_factor, name = "Unemployment Change (percentage points)")
  ) +
  coord_flip() +
  labs(
    title = "Regional COVID-19 Economic Impact (2019–2020)",
    subtitle = "GDP change shown as bars; Unemployment change shown as red dots",
    x = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.title.y = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 10, color = "grey30"),
    axis.title.x.right = element_text(color = "#D7191C"),
    axis.title.x.left = element_text(color = "#2C7BB6")
  )

print(p11)



#5. CORRELATION AMONG ALL THE FACTORS
cor_data <- df_latest %>% select(GDP, Unemployment, Inflation, LifeExp, LaborForce) %>% na.omit()
cor_matrix <- cor(cor_data)

corrplot(cor_matrix, method = "color", type = "upper", tl.col = "black", tl.srt = 45,
         addCoef.col = "black", title = sprintf("Correlation Matrix (%d)", latest_yr),
         mar = c(0,0,2,0), number.cex = 1.2)

png("plot12_correlation_matrix.png", width = 900, height = 800, res = 120)
corrplot(cor_matrix, method = "color", type = "upper", tl.col = "black", tl.srt = 45,
         addCoef.col = "black", title = sprintf("Correlation Matrix (%d)", latest_yr),
         mar = c(0,0,2,0), number.cex = 1.2)
dev.off()

write.csv(cor_matrix, "correlation_matrix.csv")


#6.SUMMARY FOR ALL REGIONS
regional_summary <- df %>%
  filter(Year == latest_yr) %>%
  group_by(Region) %>%
  summarise(Countries = n(), GDP = round(mean(GDP, na.rm = TRUE), 0),
            Inflation = round(mean(Inflation, na.rm = TRUE), 2),
            Unemployment = round(mean(Unemployment, na.rm = TRUE), 2),
            LifeExp = round(mean(LifeExp, na.rm = TRUE), 1)) %>%
  arrange(desc(GDP))

print(regional_summary)
write.csv(regional_summary, "regional_summary.csv", row.names = FALSE)
