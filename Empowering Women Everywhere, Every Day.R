install.packages("tidyverse") # Collection of R packages related to design philosophy, grammar, and data structures
install.packages("dplyr") # Data management
install.packages("ggplot2") # Data visualization
install.packages("WDI") 

library(tidyverse)
library(dplyr)
library(ggplot2)
library(WDI)
?WDI

# World Bank Indicators 
# SH.STA.ANVC.ZS = Pregnant women receiving prenatal care (%)
# SH.STA.MMRT = Maternal mortality ratio (modeled estimate, per 100,000 live births)
# SH.STA.STNT.ZS = Prevalence of stunting, height for age (% of children under 5)


SH_1 <- WDI(
  country = "all",
  indicator = c("SH.STA.ANVC.ZS", "SH.STA.MMRT", "SH.STA.STNT.ZS"),
  start = 1960,
  end = 2022,
  extra = FALSE,
  cache = NULL,
  latest = NULL,
  language = "en"
)



# Average and Median
SH_1 %>%
  arrange(desc(year)) %>%
  filter(country == "Ethiopia")

ETH <- SH_1 %>%
  arrange(desc(year)) %>%
  filter(country == "Ethiopia")

mean(ETH$SH.STA.ANVC.ZS, na.rm = TRUE) # na.rm = TRUE, when values in variables are missing.
sort(ETH$SH.STA.ANVC.ZS) 
median(ETH$SH.STA.ANVC.ZS, na.rm = TRUE) # na.rm = TRUE, when values in variables are missing.
hist(ETH$SH.STA.ANVC.ZS, main = "Pregnant women receiving prenatal care (%)")

SH_1 %>%
  group_by(country) %>%
  summarise(mean_SH.STA.MMRT = mean(SH.STA.MMRT, na.rm = TRUE),
            median_SH.STA.MMRT = median(SH.STA.MMRT, na.rm = TRUE))

SH_1 %>%
  filter(country %in% c("Ethiopia", "Ghana", "Sub-Saharan Africa")) %>%
  group_by(country) %>%
  summarise(mean_SH.STA.MMRT = mean(SH.STA.MMRT, na.rm = TRUE),
            median_SH.STA.MMRT = median(SH.STA.MMRT, na.rm = TRUE))

SH_1 %>%
  filter(country == "Ethiopia") %>%
  ggplot(aes(SH.STA.MMRT)) +
  geom_histogram()

SH_1 %>%
  filter(country == "Ethiopia") %>%
  summarize(mean_SH.STA.MMRT = mean(SH.STA.MMRT, na.rm = TRUE),
            median_SH.STA.MMRT = median(SH.STA.MMRT, na.rm = TRUE))



# Quartiles, splitting up the data into 4 equal parts
quantile(ETH$SH.STA.MMRT, na.rm = TRUE) # the first 25% of the data is between 401,00 and 505,25. The second 25% of the data is between 505,25 and 659,50. This means that the second quartile splits the data into 2, with 50% of the data below it and 50%  above it, so it is the same as the median.
# Calculating the quantiles of SH.STA.MMRT 
quantile(ETH$SH.STA.MMRT, na.rm = TRUE, probs = seq(0,1,0.2)) # With the seq function we can take the lowest number "0", the highest number "1", and the number we want to jump by "0.2"
quantile(ETH$SH.STA.MMRT, na.rm = TRUE, probs = seq(0,1,0.1)) # With the seq function we can take the lowest number "0", the highest number "1", and the number we want to jump by "0.1"



# Variance and Standard deviation 
SH_1 %>% 
  group_by(country) %>% 
  summarize(var_a1 = var(SH.STA.ANVC.ZS, na.rm = TRUE),
            sd_a1 = sd(SH.STA.ANVC.ZS, na.rm = TRUE))

SH_1 %>% 
  group_by(country) %>% 
  filter(country %in% c("Ethiopia", "Ghana", "Sub-Saharan Africa")) %>%
  summarize(var_a1 = var(SH.STA.ANVC.ZS, na.rm = TRUE),
            sd_a1 = sd(SH.STA.ANVC.ZS, na.rm = TRUE))

SH_1 %>%
  filter(country == "Ethiopia") %>%
  ggplot(aes(SH.STA.ANVC.ZS)) +
  geom_histogram()

SH_1 %>%
  filter(country == "Ethiopia") %>%
  summarize(var_a1 = var(SH.STA.ANVC.ZS, na.rm = TRUE),
            sd_a1 = sd(SH.STA.ANVC.ZS, na.rm = TRUE))


# Calculating the total SH.STA.ANVC.ZS per country
SH_1 %>%
  group_by(country) %>%
  summarize(total_SH.STA.ANVC.ZS = sum(SH.STA.ANVC.ZS, na.rm = TRUE))

Total_SH_1 <- SH_1 %>%
  group_by(country) %>%
  summarize(total_SH.STA.ANVC.ZS = sum(SH.STA.ANVC.ZS, na.rm = TRUE))

Total_SH_1 %>%
  head() 

Total_SH_1 %>%
  tail()



# Computing the first and third quartiles and IQR of total_SH.STA.ANVC.ZS
quantile(Total_SH_1$total_SH.STA.ANVC.ZS, 0.25)
quantile(Total_SH_1$total_SH.STA.ANVC.ZS, 0.75)
q1 <- quantile(Total_SH_1$total_SH.STA.ANVC.ZS, 0.25)
q3 <- quantile(Total_SH_1$total_SH.STA.ANVC.ZS, 0.75)
iqr <- q3-q1

q3-q1
q3+1.5*iqr
q1-1.5*iqr

# Calculating the lower and upper data points that are substantially different from the others 
lower <- q1-1.5*iqr
upper <- q3+1.5*iqr

# Filtering Total_SH_1 to find data points substantially different from the others 
Total_SH_1 %>%
  filter(total_SH.STA.ANVC.ZS < lower | total_SH.STA.ANVC.ZS> upper)


