library(tidyverse)
library(readxl)
library(haven)
library(ggplot2)
library(economiccomplexity)

## Question 1: Definitions
# Define the following seven terms in your own words and describe how they relate to one another: RCA, Diversity, Average Ubiquity, ECI, PCI, Proximity, Density, COI and COG. 
# 

 
## Question 2: Examine Your Country
# Choose a country for the rest of the problem set. Browse your country’s data in the Explore and Country Profile functionalities of the Atlas. If your country has a city in the Metroverse, please include information from one city in the Metroverse. Write a paragraph (200 words most) on your country’s export basket, its trade partners, the evolution of its comparative advantages and its complexity profile. Do not simply copy the automatically generated text in the Country Profile.
# 
countryname <- "Peru"
countryiso <- "PER"

dev309 <- read_dta("Dev309_Database.dta")

Database_Codebook <- read_excel("Database_Codebook.xlsx") %>% 
  filter(is.na(`Variable Name`) == F)

perdev <- dev309 %>% 
  filter(wb_countryname == countryname,
         year > 1970)


# Question 3: Browse your Country’s Data
# Use the same country as in Question 2. Using the statistical package of your choice, generate a list of the following:
#   •	10 Largest Products by Export Value
# •	10 Products with Highest RCA
# •	10 Products with Highest Global market share
# •	10 Products with Highest PCI
# 


## Question 4: Diversity, Ubiquity, and Complexity
# Create scatter plots of diversity vs. average ubiquity in your country’s most recent year of complete data, as well as diversity vs. ECI for the same year. What do you observe? How does your country rank vis-à-vis other countries? What does this mean?
#   


##   Question 5: Your Country’s Strategic Position
# Reproduce the “rock song chart” (COI vs. ECI controlling for Natural Resource exports/rents and GDP per capita). What’s the position of your country in the chart? What does this say about the policy stance that the country should take on the matter of economic development strategy?
#   


##   Question 6: Are Your Monkeys Jumping?
#   Regress a metric of the number of products added to the export basket of a country as a function of the country’s earlier position in the product space (COI) in a previous year. What’s the slope of the relationship? Did your country add more or less products than expected? Note that too little “jumpiness” may be evidence that the problem is the “monkeys”, not the “trees around them” – that is, if there is ample space for adjacent diversification, yet diversification has not happened in your country, it suggests that something else is preventing agents from exploiting such opportunities. 
# 


## Question 7: Are Green Growth Opportunities on Your Country’s Horizon?
#   Look at the Green Growth Dashboard to see if your country is competitive in products needed in a decarbonizing world (strategy 1 of the Green Growth lecture). Look at the wind, solar, and hydropower potential of your country to see if your country is potentially competitive in renewable energy sources (strategy 2 of the Green Growth lecture). Similarly, you can check if your country has unexploited minerals  critical to the green transition (optional!). Feel free to pull in other databases or sources as needed (and share on the Slack if you find anything useful).
# Does your country (or certain areas of your country) have a clear opportunity in either strategy 1 or strategy 2? You can compare your country to other countries in the region or to the world. You are not expected to do any data analysis in this question aside from looking up figures. Limit your answer to 200 words.
