## Setting Directory ------------------------------------------------------------------------------------------------
setwd("/Users/Jana/Documents/Study/Masters/DEV 309/devpolstrat")


## Loading Libraries ------------------------------------------------------------------------------------------------
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
  'modelr', #to add predicted value to dataframe
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


## Setting basic parameters -----------------------------------------------------------------------------------------
countryname <- "Peru"
countryiso <- "PER"
yr <- 2021

rca_thres <- 1


## Loading datasets -------------------------------------------------------------------------------------------------
database <- read_dta("input/Dev309_Database.dta")
df <- read_dta("input/country_sitcproduct4digit_year.dta")

top50_2016 <- database %>%
  filter(year==2016) %>%
  arrange(-wdi_ny_gdp_mktp_kd) %>%
  mutate(rank = row_number()) %>%
  filter(rank<=50) %>%
  pull(countrycodeiso)

sitc_data <- read_dta("input/sitc_product.dta")


## Preparing data ---------------------------------------------------------------------------------------------------

df_exp <- df %>%
  filter(year==yr) %>%
  select(location_code, sitc_product_code, export_value, export_rca, pci, sitc_eci)

df_peru <- df_exp %>% 
  filter(location_code == countryiso) %>%
  left_join(sitc_data, by="sitc_product_code")


# Q3: Browse your Country’s Data ------------------------------------------------------------------------------------
# Use the same country as in Question 2. Using the statistical package of your choice, generate a list of the following:
#   •	10 Largest Products by Export Value
# •	10 Products with Highest RCA
# •	10 Products with Highest Global market share
# •	10 Products with Highest PCI

largest_exports <- df_peru %>% arrange(.,desc(export_value)) %>% head(.,10) %>% 
  select(sitc_product_name_short_en,export_value)

highest_rca <- df_peru %>% arrange(.,desc(export_rca)) %>% head(.,10) %>% 
  select(sitc_product_name_short_en,export_rca)

market_share <- df_exp %>% group_by(sitc_product_code) %>% summarise(global_export = sum(export_value))

df_peru1 <- df_peru %>%
  left_join(market_share,by = "sitc_product_code") %>%
  mutate(market_percent = export_value/global_export)

highest_share <- df_peru1 %>% arrange(.,desc(market_percent)) %>% head(.,10) %>% 
  select(sitc_product_name_short_en,market_percent)

highest_pci<- df_peru1 %>% arrange(.,desc(pci)) %>% head(.,10) %>% 
  select(sitc_product_name_short_en,pci)

largest_exports %>% write_csv("output/largest_exports.csv")
highest_rca %>% write_csv("output/highest_rca.csv")
highest_share %>% write_csv("output/highest_share.csv")
highest_pci %>% write_csv("output/highest_pci.csv")


## Question 4: Diversity, Ubiquity, and Complexity ------------------------------------------------------------------
# Create scatter plots of diversity vs. average ubiquity in your country’s most recent year of complete data, as well as diversity vs. ECI for the same year. What do you observe? How does your country rank vis-à-vis other countries? What does this mean?

product_rca <- df_exp %>%
  mutate(rca_bin = ifelse(export_rca>=rca_thres,1,0))
ubiquity_code <- product_rca %>%
  group_by(sitc_product_code) %>%
  summarise(ubiquity = sum(rca_bin,na.rm=TRUE))
diversity_code <- product_rca %>%
  group_by(location_code) %>%
  summarise(diversity = sum(rca_bin,na.rm=TRUE))

df_exp %>%
  left_join(.,ubiquity_code,by = "sitc_product_code") %>%
  left_join(.,diversity_code,by = "location_code") %>%
  filter(export_rca>=rca_thres, location_code != "ANS") %>%
  group_by(location_code) %>%
  summarise(diversity = mean(diversity),ubiquity = mean(ubiquity)) %>%
  mutate(color = ifelse(location_code == "PER", "red", "darkgrey")) %>%
  ggplot(aes(x = diversity, y = ubiquity, color = color, label = location_code)) +
  geom_smooth(method=lm, se=FALSE, color="black", linewidth=0.2) +
  #geom_point() +
  scale_color_identity() +
  labs(x = "Diversity", y = "Average Ubiquity of RCA>1 Products",
       title = paste0("Diversity vs. Ubiquity for Countries, ",yr)) +
  geom_text(hjust = 0, vjust = 0, size = 3) +
  theme_classic()
ggsave("output/q4a.png",width=6,height=6,dpi=500)

df_exp %>%
  left_join(.,diversity_code,by = "location_code") %>%
  filter(export_rca>=rca_thres, location_code != "ANS") %>%
  group_by(location_code) %>%
  summarise(diversity = mean(diversity),eci = mean(sitc_eci)) %>%
  mutate(color = ifelse(location_code == "PER", "red", "darkgrey")) %>%
  ggplot(aes(x = diversity, y = eci, color = color, label = location_code)) +
  geom_smooth(method=lm, se=FALSE, color="black", linewidth=0.2) +
  #geom_point() +
  scale_color_identity() +
  labs(x = "Diversity", y = "ECI",title = paste0("Diversity vs. ECI for Countries, ",yr)) +
  geom_text(hjust = 0, vjust = 0, size = 3) +
  theme_classic()
ggsave("output/q4b.png",width=6,height=6,dpi=500)

## Question 5: Your Country’s Strategic Position --------------------------------------------------------------------
# Reproduce the “rock song chart” (COI vs. ECI controlling for Natural Resource exports/rents and GDP per capita). What’s the position of your country in the chart? What does this say about the policy stance that the country should take on the matter of economic development strategy?
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
  scale_color_manual(values=c("gray","red")) +
  geom_vline(xintercept=0) +
  geom_hline(yintercept=0) +
  xlab("ECI controlled for GDP per Capita and Natural Resource Rents") +
  ylab("COI")
ggsave("output/q5.png",width=6,height=6,dpi=500)


## Question 6: Are Your Monkeys Jumping? ----------------------------------------------------------------------------
# Regress a metric of the number of products added to the export basket of a country as a function of the country’s earlier position in the product space (COI) in a previous year. What’s the slope of the relationship? Did your country add more or less products than expected? Note that too little “jumpiness” may be evidence that the problem is the “monkeys”, not the “trees around them” – that is, if there is ample space for adjacent diversification, yet diversification has not happened in your country, it suggests that something else is preventing agents from exploiting such opportunities. 
yr1 <- 2006
yr2 <- 2016

# Net number of new products (including products losing RCA)
df_rca_net <- df %>%
  mutate(export_rca_bin = ifelse(export_rca>=rca_thres,1,0)) %>%
  select(location_code, year, sitc_product_code, export_rca_bin) %>%
  group_by(location_code,year) %>%
  summarise(number_rca_prod = sum(export_rca_bin,na.rm=TRUE)) %>%
  filter(year %in% c(yr1,yr2)) %>%
  mutate(year = ifelse(year==yr1,"pre","post")) %>%
  pivot_wider(names_from=year, values_from=number_rca_prod) %>%
  mutate(new_prod = post-pre) %>%
  select(location_code,new_prod)

# Gross number of new products (only products gaining RCA)
df_rca_gross <- df %>%
  mutate(export_rca_bin = ifelse(export_rca>=rca_thres,1,0)) %>%
  select(location_code, year, sitc_product_code, export_rca_bin) %>%
  filter(year %in% c(yr1,yr2)) %>%
  mutate(year = ifelse(year==yr1,"pre","post")) %>%
  pivot_wider(names_from=year, values_from=export_rca_bin) %>%
  mutate(new_prod = ifelse(pre==0 & post==1,1,0)) %>%
  group_by(location_code) %>%
  summarise(new_prod=sum(new_prod,na.rm=TRUE))

# Regressing
df_monkeyjump <- df_rca_gross %>%
  rename(country=location_code) %>%
  left_join(database %>%
              filter(year==yr1) %>%
              rename(country=countrycodeiso) %>%
              select(country, atl_sitc_eci,atl_sitc_coi),
            by="country") %>%
  add_predictions(lm(new_prod ~ atl_sitc_eci + atl_sitc_coi, data=.))

df_rca_gross %>%
  rename(country=location_code) %>%
  left_join(database %>%
              filter(year==yr1) %>%
              rename(country=countrycodeiso) %>%
              select(country, atl_sitc_eci,atl_sitc_coi),
            by="country") %>%
  lm(new_prod ~ atl_sitc_eci + atl_sitc_coi, data=.)

#Plotting
df_monkeyjump %>%
  filter(country %in% top50_2016) %>% #top50 countries only
  filter(!is.na(pred)) %>%
  select(country,new_prod,pred) %>%
  pivot_longer(-country,values_to="new_prod",names_to="variable") %>%
  left_join(df_monkeyjump %>% mutate(resid = new_prod-pred) %>% select(country,resid), by="country") %>%
  mutate(variable = ifelse(variable=="new_prod","Actual","Predicted")) %>%
  ggplot(aes(y=reorder(country,resid),x=new_prod,color=variable)) +
  geom_hline(yintercept="PER",color="orange") +
  geom_point() +
  geom_line(aes(group = country)) +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = "gray",
                                        size = 0.1),
        legend.position="top") +
  scale_color_manual(values=c("red3","chartreuse3")) +
  ylab("Country, Ordered by Actual Number of New Products Added versus Predicted") +
  xlab("Number of New Products (RCA>1)") +
  ggtitle("Number of New Products Added 2006-2016: Actual vs Predicted") +
  labs(colour="")
ggsave("output/q6.png",width=6,height=10,dpi=500)
