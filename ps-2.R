
## Loading Libraries
packages <- c(
  #FOR BASIC DATA HANDLING
  'plyr', #load this first so dplyr in tidyverse takes priority
  'tidyverse',
  'dplyr',
  'janitor', #clean names
  'tidyr', #reshaping
  'readxl',
  #FOR EXPORTING AND IMPORTING
  'foreign',
  'haven',
  #FOR REG RESULTS
  'modelsummary', #summarizing reg results
  'sandwich', #robust estimators
  'lmtest', #diagnostic checking
  'car',
  'broom',
  'outreg',
  'stargazer', #exporting to latex
  #FOR NETWORK
  'igraph',
  'centiserve',
  #FOR PLOTS AND AESTHETICS
  'ggpubr',
  'ggrepel', #text labels to not overlap
  'ggnewscale', #multiple scales
  'treemapify', #treemaps
  'mekko', #mekko charts (variable width bar charts),
  'qgraph', #q graphs
  'RColorBrewer',
  'pals', #color palettes
  'viridis', #special color palettes
  'ggthemes', #themes
  'cowplot', #themes and other plot configs
  'gridExtra', #putting multiple plots into grids
  'gt', #table for heatmap
  'webshot2', #for saving gt table as png
  'scales',
  'patchwork',
  'ggbump', #putting multiple plots into grids  
  #FOR COMPLEXITY
  'economiccomplexity') 
to_install <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(to_install)>0) install.packages(to_install, 
                                          repos='http://cran.us.r-project.org')
lapply(packages, require, character.only=TRUE)

countryname <- "Peru"
countryiso <- "PER"

dev309 <- read_dta("Dev309_Database.dta")
products <- read_dta("dataverse_files/country_partner_sitcproduct2digit_year.dta")

Database_Codebook <- read_excel("Database_Codebook.xlsx") %>% 
  filter(is.na(`Variable Name`) == F)

perdev <- dev309 %>% 
  filter(wb_countryname == countryname,
         year > 1970)

## Pick Year
yr <- 2019

## Loading Data

sitc_data <- read_dta("dataverse_files/sitc_product.dta")




# # Constructing RCA matrix
# df_rca <- df %>%
#   filter(year==2019) %>%
#   select(location_code, sitc_product_code, export_rca) %>%
#   pivot_wider(names_from="sitc_product_code", values_from="export_rca")
# 
# row_name = df_rca$location_code
# rca <- df_rca %>% select(-location_code) %>% as.matrix()
# rownames(rca) = row_name
# 
# # Converting to Mcp matrix
# rca[rca<1] <- 0
# rca[rca>=1] <- 1
# mcp <- rca
# rm(rca)

mcp <- balassa_index(
  df_exp,
  country="location_code",
  product="sitc_product_code",
  value="export_value"
)

complexity <- complexity_measures(
  mcp
)

eci <- complexity$complexity_index_country
pci <- complexity$complexity_index_product

proximity <- proximity(mcp, compute="both")
proximity_country <- proximity$proximity_country
proximity_product <- proximity$proximity_product

co <- complexity_outlook(mcp, proximity_product, pci)
coi <- co$complexity_outlook_index
cog <- co$complexity_outlook_gain


## Question 1: Definitions
# Define the following seven terms in your own words and describe how they relate to one another: RCA, Diversity, Average Ubiquity, ECI, PCI, Proximity, Density, COI and COG. 
# 

 
## Question 2: Examine Your Country
# Choose a country for the rest of the problem set. Browse your country’s data in the Explore and Country Profile functionalities of the Atlas. If your country has a city in the Metroverse, please include information from one city in the Metroverse. Write a paragraph (200 words most) on your country’s export basket, its trade partners, the evolution of its comparative advantages and its complexity profile. Do not simply copy the automatically generated text in the Country Profile.
# 



# Question 3: Browse your Country’s Data
# Use the same country as in Question 2. Using the statistical package of your choice, generate a list of the following:
#   •	10 Largest Products by Export Value
# •	10 Products with Highest RCA
# •	10 Products with Highest Global market share
# •	10 Products with Highest PCI
# 
## Pick Year
yr <- 2019

## Loading Data

sitc_data <- read_dta("dataverse_files/sitc_product.dta")

df <- read_dta("dataverse_files/country_sitcproduct4digit_year.dta") %>% 
  join(.,sitc_data,by = "sitc_product_code") %>% select(-2) %>% 
  filter(level == "4digit")

df_exp <- df %>%
  filter(year==yr) 

df_peru <- df_exp %>% 
  filter(location_code == countryISO)

# perform calculations
largest_exports <- df_peru %>% arrange(.,desc(export_value)) %>% head(.,10) %>% 
  select(sitc_product_name_short_en,export_value)

highest_RCA <- df_peru %>% arrange(.,desc(export_rca)) %>% head(.,10) %>% 
  select(sitc_product_name_short_en,export_rca)

market_share <- df_exp %>% group_by(sitc_product_code) %>% summarise(global_export = sum(export_value))
df_peru <- join(df_peru,market_share,by = "sitc_product_code") 
df_peru <- df_peru %>% mutate(market_percent = export_value/global_export)
highest_share <- df_peru %>% arrange(.,desc(market_percent)) %>% head(.,10) %>% 
  select(sitc_product_name_short_en,market_percent)

highest_PCI <- df_peru %>% arrange(.,desc(pci)) %>% head(.,10) %>% 
  select(sitc_product_name_short_en,pci)

#answers
largest_exports
highest_RCA
highest_share
highest_PCI

## Question 4: Diversity, Ubiquity, and Complexity
# Create scatter plots of diversity vs. average ubiquity in your country’s most recent year of complete data, as well as diversity vs. ECI for the same year. What do you observe? How does your country rank vis-à-vis other countries? What does this mean?
#

#create data values
products_status <- df_exp %>% mutate(status = ifelse(export_value>0,1,0))
ubiquity_code <- products_status %>% group_by(sitc_product_code) %>% summarise(ubiquity = mean(status))
diversity_code <- products_status %>% group_by(location_code) %>% summarise(diversity = sum(status))
df_exp <- df_exp %>% left_join(.,ubiquity_code,by = "sitc_product_code") %>% left_join(.,diversity_code,by = "location_code")
df_plot <- df_exp %>% filter(export_value>0, location_code != "ANS") %>% group_by(location_code) %>% summarise(diversity = mean(diversity),ubiquity = mean(ubiquity))

# Create a color variable to differentiate Peru from other countries
df_plot$color <- ifelse(df_plot$location_code == "PER", "red", "darkgrey")

#plot 1
ggplot(df_plot, aes(x = diversity, y = ubiquity, color = color, label = location_code)) +
  geom_point() +
  scale_color_identity() +
  labs(x = "Diversity", y = "Ubiquity",title = "Diversity vs. Ubiquity for Countries") +
  geom_text(hjust = 0, vjust = 0, size = 3) +
  theme_minimal()

#plot 2
df_plot2 <- df_exp %>% filter(export_value>0, location_code != "ANS") %>% group_by(location_code) %>% summarise(diversity = mean(diversity),eci = mean(sitc_eci))
df_plot2$color <- ifelse(df_plot$location_code == "PER", "red", "darkgrey")

ggplot(df_plot2, aes(x = diversity, y = eci, color = color, label = location_code)) +
  geom_point() +
  scale_color_identity() +
  labs(x = "Diversity", y = "ECI",title = "Diversity vs. ECI for Countries") +
  geom_text(hjust = 0, vjust = 0, size = 3) +
  theme_minimal()

##   Question 5: Your Country’s Strategic Position
# Reproduce the “rock song chart” (COI vs. ECI controlling for Natural Resource exports/rents and GDP per capita). What’s the position of your country in the chart? What does this say about the policy stance that the country should take on the matter of economic development strategy?
df_rocksong <- eci %>%
  as.table() %>%
  as.data.frame() %>%
  as_tibble() %>%
  rename(country=Var1,
         eci=Freq) %>%
  left_join(database %>%
              filter(year==2019) %>%
              rename(country=countrycodeiso) %>%
              select(country, wdi_ny_gdp_pcap_kd, wdi_ny_gdp_totl_rt_zs, atl_sitc_coi),
            by="country") %>%
  rename(gdppc = wdi_ny_gdp_pcap_kd,
         natrent = wdi_ny_gdp_totl_rt_zs) %>%
  filter(!is.na(gdppc) & !is.na(natrent)) %>%
  mutate(resid = residuals(lm(eci ~ gdppc + natrent))) %>%
  rename(eci_controlled = resid) %>%
  left_join(coi %>%
              as.table() %>%
              as.data.frame() %>%
              as_tibble() %>%
              rename(country=Var1,
                     coi=Freq),
            by="country") %>%
  mutate(coi_norm = (coi-mean(coi))/sd(coi))

df_rocksong %>%
  mutate(peru = ifelse(country=="PER",1,0)) %>%
  ggplot(aes(x=eci_controlled,y=atl_sitc_coi,color=as.factor(peru))) +
  geom_text(aes(label=country)) +
  theme_classic() +
  theme(legend.position="none") +
  scale_color_manual(values=c("gray","red"))


##   Question 6: Are Your Monkeys Jumping?
#   Regress a metric of the number of products added to the export basket of a country as a function of the country’s earlier position in the product space (COI) in a previous year. What’s the slope of the relationship? Did your country add more or less products than expected? Note that too little “jumpiness” may be evidence that the problem is the “monkeys”, not the “trees around them” – that is, if there is ample space for adjacent diversification, yet diversification has not happened in your country, it suggests that something else is preventing agents from exploiting such opportunities. 
# 


## Question 7: Are Green Growth Opportunities on Your Country’s Horizon?
#   Look at the Green Growth Dashboard to see if your country is competitive in products needed in a decarbonizing world (strategy 1 of the Green Growth lecture). Look at the wind, solar, and hydropower potential of your country to see if your country is potentially competitive in renewable energy sources (strategy 2 of the Green Growth lecture). Similarly, you can check if your country has unexploited minerals  critical to the green transition (optional!). Feel free to pull in other databases or sources as needed (and share on the Slack if you find anything useful).
# Does your country (or certain areas of your country) have a clear opportunity in either strategy 1 or strategy 2? You can compare your country to other countries in the region or to the world. You are not expected to do any data analysis in this question aside from looking up figures. Limit your answer to 200 words.
