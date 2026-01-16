#loading csv file
getwd()
file_path ='./Countries Population.csv' #relative path
dfCountries = read.csv(file= file_path)

library(dplyr)
#sorting of the data based on population 
dfCountries_ascending = arrange(dfCountries, Total.Population.2017) 
dfCountries_descending = arrange(dfCountries, - Total.Population.2017) 

#population more than 10000000
dfpopulation = filter(dfCountries, Total.Population.2017 >10000000)  
vector_country = c(dfpopulation$Country.Name) 
vector_country

#data frame dfBigAndSmall
dfBigAndSmall =filter(dfCountries, (Total.Population.2017 >100000000) | (Total.Population.2017 < 2000000))
dfBigAndSmall

#levels
Incomelevels = factor(
  dfCountryMaster$IncomeGroup,
  levels = c("Low income", "Lower middle income", 
             "Upper middle income", "High income"))
Incomelevels 

#loading countries region csv
getwd()
file_path_regions ='./Countries Region Mapping.csv' #relative path
dfCountriesRegion = read.csv(file= file_path_regions)
dfCountriesRegion 

head(dfCountriesRegion)

#merging of three datasets into dfCountryMaster
getwd()
file_path_indicators ='./Countries Indicators.csv' #relative path
dfCountriesIndicators = read.csv(file= file_path_indicators)
dfCountriesIndicators

dfCountryMaster = merge(dfCountries, dfCountriesRegion , by = "Country.Code", all = TRUE)
dfCountryMaster = merge(dfCountryMaster, dfCountriesIndicators, by = "Country.Code", all = TRUE)

head(dfCountryMaster)

#Summarize dfCountryMaster countries by region.
summary_region =  dfCountryMaster %>%
  group_by(Region) %>%
  summarise(
    Number_of_Countries = n_distinct(Country.Code)
  )
summary_region

#Summarize dfCountryMaster countries by region and income
summary_region_income =  dfCountryMaster %>%
  group_by(Region, IncomeGroup) %>%
  summarise(
    Number_of_Countries = n_distinct(Country.Code)
  )
summary_region_income

#Summarize dfCountryMaster countries by region. Result to have the following columns in it.
#1, Number of countries.
#2. Total population in millions.
#3. Average of GDP per capita
#4. Countries with low income.
#5. Median GDP per capita
#6. minimum and maximum mortality rate under 5.

dfCountryMaster$GDP.per.capita.2017 <- as.numeric(
  dfCountryMaster$GDP.per.capita.2017)


summary_region_full <- dfCountryMaster %>%
  group_by(Region) %>%
  summarise(
    Number_of_Countries = n_distinct(Country.Code),
    Total_Population_Millions = sum(Total.Population.2017, na.rm = TRUE) / 1000000,
    Avg_GDP_per_capita = mean(GDP.per.capita.2017, na.rm = TRUE),
    Low_Income_Countries = sum(IncomeGroup == "Low income", na.rm = TRUE),
    Median_GDP_per_capita = median(GDP.per.capita.2017, na.rm = TRUE),
    Min_Under5_Mortality = min(Under.5.Mortality.Rate.2017, na.rm = TRUE),
    Max_Under5_Mortality = max(Under.5.Mortality.Rate.2017, na.rm = TRUE)
  )

#Write the above result in csv.
write.csv(summary_region_full, "Region Summary Output.csv")

#Histogram of gdp per capita
gdp_plot1 <- ggplot(
  dfCountryMaster %>% filter(!is.na(GDP.per.capita.2017)),
  aes(x = GDP.per.capita.2017)
) +
  geom_histogram(
    binwidth = 50,
    color = "black",
    fill = "skyblue"
  ) +
  ggtitle("Histogram of GDP per Capita") +
  xlab("GDP per Capita") +
  ylab("Frequency")
  
gdp_plot1

#Plot of income group by region.
ggplot(dfCountryMaster, aes(x = Region, fill = IncomeGroup)) +
  geom_bar() +
  ggtitle("Income Group by Region") +
  xlab("Region") +
  ylab("Number of Countries")
  
#Plot of mortality rate under 5 by region
ggplot(dfCountryMaster %>% filter(!is.na(Under.5.Mortality.Rate.2017)), aes(x = Region, y = Under.5.Mortality.Rate.2017)) +
  geom_boxplot() +
  ggtitle("Under-5 Mortality Rate by Region") +
  xlab("Region") +
  ylab("Mortality Rate")

#Scatter plot of mortality rate under 5 against GDP per capita.
ggplot(dfCountryMaster%>% filter(!is.na(GDP.per.capita.2017)),
       aes(x = GDP.per.capita.2017,
           y = Under.5.Mortality.Rate.2017)) +
  geom_point() +
  ggtitle("Mortality Rate vs GDP per Capita") +
  xlab("GDP per Capita") +
  ylab("Under-5 Mortality Rate")

#Plot of mortality rate under 5 against GDP and region.
ggplot(dfCountryMaster%>% filter(!is.na(GDP.per.capita.2017)),
       aes(x = GDP.per.capita.2017,
           y = Under.5.Mortality.Rate.2017,
           color = Region)) +
  geom_point() +
  ggtitle("Mortality Rate vs GDP per Capita by Region") +
  xlab("GDP per Capita") +
  ylab("Under-5 Mortality Rate")
  





       