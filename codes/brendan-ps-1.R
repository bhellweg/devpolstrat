library(tidyverse)
library(readxl)
library(haven)
library(ggplot2)

dev309 <- read_dta("Dev309_Database.dta")

Database_Codebook <- read_excel("Database_Codebook.xlsx") %>% 
  filter(is.na(`Variable Name`) == F)

## Country Selection

country <- 'Peru'
countryISO <- 'PER'

ethdev <- dev309 %>% 
  filter(wb_countryname == country,
         year > 1970)

## Problem 1

# Plot time series graphs for at least 40 years of data until 2022 that show:
# •	GDP weo_ngdpd
# •	GDP per capita weo_ngdpdpc
# •	Population 	weo_lp
# •	GDP growth wdi_ny_gdp_mktp_kd_zg
# •	GDP per capita growth wdi_ny_gdp_pcap_kd_zg
# •	Population growth wdi_sp_pop_grow
# •	Dependency Ratio, young wdi_sp_pop_dpnd_yg
# •	Dependency Ratio, old wdi_sp_pop_dpnd_ol
# •	Adjusted Dependency Ratio wdi_sp_pop_dpnd
# •	GINI Coefficient (Income) wdi_si_pov_gini
# 
# How do these graphs compare to one another? Have they been accelerating? Decelerating? Are there significant instances of volatility?
  
plot1 <- ethdev %>% 
  ggplot(.,
         aes(x = year, y = weo_pppgdp))+
  geom_line()+
  ggtitle(label = "GDP of Peru",subtitle = "Gross Domestic Process, Current Prices, PPP")

plot2 <- ethdev %>% 
  ggplot(.,
         aes(x = year, y = weo_ngdpdpc))+
  geom_line()+
  ggtitle(label = "GDP per capita of Peru",subtitle = "Current Prices, PPP")

plot3 <- ethdev %>% 
  ggplot(.,
         aes(x = year, y = weo_lp))+
  geom_line()+
  ggtitle(label = "Population",subtitle = "Peru")

 plot4 <- ethdev %>% 
  ggplot(.,
         aes(x = year, y = wdi_ny_gdp_mktp_kd_zg))+
  geom_line()+
  ggtitle(label = "GDP Growth Rate of Peru",subtitle = "Current Prices, PPP")

 plot10 <- ethdev %>% 
   ggplot(.,
          aes(x = year, y = wdi_ny_gdp_pcap_kd_zg))+
   geom_line()+
   ggtitle(label = "GDP Per Capita Growth Rate",subtitle = "Current Prices, PPP, Peru")
 
plot5 <- ethdev %>% 
  ggplot(.,
         aes(x = year, y = wdi_sp_pop_grow))+
  geom_line()+
  ggtitle(label = "Population Growth Rate",subtitle = "Peru")

plot6 <- ethdev %>% 
  ggplot(.,
         aes(x = year, y = wdi_sp_pop_dpnd_ol))+
  geom_line()+
  ggtitle(label = "Old Age Dependency Ratio",subtitle = "Peru")

plot7 <- ethdev %>% 
  ggplot(.,
         aes(x = year, y = wdi_sp_pop_dpnd_yg))+
  geom_line()+
  ggtitle(label = "Young Dependency Ratio",subtitle = "Peru")

plot8 <- ethdev %>% 
  ggplot(.,
         aes(x = year, y = wdi_sp_pop_dpnd))+
  geom_line()+
  ggtitle(label = "Adjusted Dependency Ratio",subtitle = "Peru")

plot9 <- ethdev %>% 
  ggplot(.,
         aes(x = year, y = wdi_si_pov_gini))+
  geom_point()+
  ggtitle(label = "Gini Coefficient (Income)",subtitle = "Peru")

plot1
plot2
plot3
plot4
plot5
plot6
plot7
plot8
plot9
plot10


# Transform the data into a format suitable for plotting for share of employment in agriculture / industry / services
ethiopia_emp_long <- tidyr::gather(ethdev %>% select(.,c(year,
                                                          Agriculture = wdi_sl_agr_empl_zs,
                                                          Industry = wdi_sl_ind_empl_zs,
                                                          Services = wdi_sl_srv_empl_zs))
                                    , Industry, Percent, -year)

ggplot(ethiopia_emp_long, aes(x = year, y = Percent, fill = Industry)) +
  geom_bar(stat = "identity") +
  labs(title = "Percent Workers in Each Industry by Year for Peru",
       x = "Year",
       y = "Percent") +
  scale_fill_manual(values = c("#69b3a2", "#404080", "#ff6767")) + # Specify colors for each industry
  theme_minimal()



# Transform the data into a format suitable for plotting for share of GDP in agriculture / industry / services
ethiopia_gdp_long <- tidyr::gather(ethdev %>% select(.,c(year,
                                                          Agriculture = eiu_agrp,
                                                          Industry = eiu_indp,
                                                          Services = eiu_serp))
                                    , Industry, Percent, -year)

ggplot(ethiopia_gdp_long, aes(x = year, y = Percent, fill = Industry)) +
  geom_bar(stat = "identity") +
  labs(title = "Percent of GDP in Each Industry by Year for Peru",
       x = "Year",
       y = "Percent") +
  scale_fill_manual(values = c("#69b3a2", "#404080", "#ff6767")) + # Specify colors for each industry
  theme_minimal()

#	Relative productivity (output per person) of agriculture vs. industry/manufacturing vs. services
ethiopia_vpw_long <- tidyr::gather(ethdev %>% select(.,c(year,
                                                         Agriculture = wdi_nv_agr_empl_kd,
                                                         Industry = wdi_nv_ind_empl_kd,
                                                         Services = wdi_nv_srv_empl_kd))
                                   , Industry, Percent, -year)

ggplot(ethiopia_vpw_long, aes(x = year, y = Percent, color = Industry)) +
  geom_line(stat = "identity", size = 1) +
  labs(title = "Output per Worker by Year by Industry, Peru",
       x = "Year",
       y = "Percent") +
  scale_color_manual(values = c("#69b3a2", "#404080", "#ff6767")) + # Specify colors for each industry
  theme_minimal()


## 3: 
# import relevant packages + install missing if needed
library(data.table)
library(strucchange)
library(forecast)
library(tseries)
library(plyr)
library(dplyr)
library(tidyverse)
library(haven)



my_data <- dev309

# calculate GDP per capita growth for desired country 
gdp_c <- my_data[, c('countrycodeiso', 'year', 'weo_ngdp_r', 'pwt_pop')] 
gdp_c <- gdp_c[gdp_c$countrycodeiso=='ETH',] #replace VNM with your country code 
gdp_c$gdp_c <- gdp_c$weo_ngdp_r / gdp_c$pwt_pop 
gdp_c <- gdp_c %>%  
  mutate(gdp_c_g = (gdp_c - lag(gdp_c))/lag(gdp_c)*100) 
gdp_c <- na.omit(gdp_c) 

# convert to time series object 
gdp_c_g <- ts(gdp_c$gdp_c_g, start=c(min(gdp_c$year), 1), end=c(max(gdp_c$year), 1), frequency=1) 

# calculate and plot structural breaks, along with confidence intervals 
bp.gdp_c_g <- breakpoints(gdp_c_g ~ 1) 
plot(gdp_c_g, main = "GDP Change Over Time & Discontinuities") 
lines(bp.gdp_c_g) 
ci.gdp_c_g <- confint(bp.gdp_c_g) 
ci.gdp_c_g 
lines(ci.gdp_c_g)

##4: 

urb_2021 <- my_data[, c('countrycodeiso', 'year', 'wdi_ny_gdp_pcap_kd', 'wdi_sp_urb_totl_in_zs')] 
urb_2021 <- urb_2021[urb_2021$year==2021,] 
urb_2021 <- na.omit(urb_2021) 
urb_2021$gdp_c <- log(urb_2021$wdi_ny_gdp_pcap_kd) 

plot(urb_2021$gdp_c, urb_2021$wdi_sp_urb_totl_in_zs, xlab="Log GDP per Capita", ylab="Urbanization Rate",  
     col=ifelse(urb_2021$countrycodeiso == "PER", "red", "black"),
     main = "GDP Per Capita vs Urbanization", sub = "Peru, 2021")  
temp <- urb_2021[urb_2021$countrycodeiso == "PER",] 
#try changing 'pos' as needed to make the label readable 
text(x = temp$gdp_c, y = temp$wdi_sp_urb_totl_in_zs, labels = temp$countrycodeiso, pos = 4, col="red") 
abline(lm(urb_2021$wdi_sp_urb_totl_in_zs ~ urb_2021$gdp_c))

## Replication

gov_2021 <- my_data[, c('countrycodeiso', 'year', 'wdi_ny_gdp_pcap_kd', 'eiu_pgce')] 
gov_2021 <- gov_2021[gov_2021$year==2021,] 
gov_2021 <- na.omit(gov_2021) 
gov_2021$gdp_c <- log(gov_2021$wdi_ny_gdp_pcap_kd) 

plot(gov_2021$gdp_c, gov_2021$eiu_pgce, xlab="Log GDP per Capita", ylab="Government % GDP",  
     col=ifelse(gov_2021$countrycodeiso == "PER", "red", "black"),
     main = "GDP Per Capita vs Government Spend", sub = "Ethiopia, 2021")  
govtemp <- gov_2021[gov_2021$countrycodeiso == "PER",] 
#try changing 'pos' as needed to make the label readable 
text(x = govtemp$gdp_c, y = govtemp$eiu_pgce, labels = govtemp$countrycodeiso, pos = 4, col="red") 
abline(lm(gov_2021$eiu_pgce ~ gov_2021$gdp_c))

##5

urb_1985 <- my_data[, c('countrycodeiso', 'year', 'wdi_ny_gdp_pcap_kd', 'wdi_sp_urb_totl_in_zs')] 
urb_1985 <- urb_1985[urb_1985$year==1985,] 
urb_1985 <- na.omit(urb_1985) 
urb_1985$gdp_c <- log(urb_1985$wdi_ny_gdp_pcap_kd) 
fit_1985 <- lm(urb_1985$wdi_sp_urb_totl_in_zs ~ urb_1985$gdp_c + I(urb_1985$gdp_c^2)+ I(urb_1985$gdp_c^3)) 
urb_1985$predicted <- predict(fit_1985) 
urb_1985 <- urb_1985[order(urb_1985$gdp_c),] 

urb_2021 <- my_data[, c('countrycodeiso', 'year', 'wdi_ny_gdp_pcap_kd', 'wdi_sp_urb_totl_in_zs')] 
urb_2021 <- urb_2021[urb_2021$year==2021,] 
urb_2021 <- na.omit(urb_2021) 
urb_2021$gdp_c <- log(urb_2021$wdi_ny_gdp_pcap_kd) 
fit_2021 <- lm(urb_2021$wdi_sp_urb_totl_in_zs ~ urb_2021$gdp_c + I(urb_2021$gdp_c^2) + I(urb_2021$gdp_c^3)) 
urb_2021$predicted <- predict(fit_2021) 
urb_2021 <- urb_2021[order(urb_2021$gdp_c),] 

#below, swap out VNM with your countrycodeiso. Also play with “pos” to make the labels readable 
plot(urb_1985$gdp_c, urb_1985$predicted, type="l", col="red", xlab="Log GDP per Capita", ylab="Urbanization Rate", ylim = c(10,80),xlim = c(3,12),
     main = "Growth in Urbanization & GDP",sub = "Peru, 1985 to 2021") 
lines(urb_2021$gdp_c, urb_2021$predicted, type="l", col="green") 
temp_1985 <- urb_1985[urb_1985$countrycodeiso=='PER',] 
points(temp_1985$gdp_c, temp_1985$wdi_sp_urb_totl_in_zs, col="red") 
text(x = temp_1985$gdp_c, y = temp_1985$wdi_sp_urb_totl_in_zs, labels = "PER 1985", pos = 2, col="red") 
temp_2021 <- urb_2021[urb_2021$countrycodeiso=='PER',] 
points(temp_2021$gdp_c, temp_2021$wdi_sp_urb_totl_in_zs, col="green") 
text(x = temp_2021$gdp_c, y = temp_2021$wdi_sp_urb_totl_in_zs, labels = "PER 2021", pos = 4, col="green") 
arrows(temp_1985$gdp_c, temp_1985$wdi_sp_urb_totl_in_zs, temp_2021$gdp_c, temp_2021$wdi_sp_urb_totl_in_zs)

## Replication

gov_1985 <- my_data[, c('countrycodeiso', 'year', 'wdi_ny_gdp_pcap_kd', 'eiu_pgce')] 
gov_1985 <- gov_1985[gov_1985$year==1985,] 
gov_1985 <- na.omit(gov_1985) 
gov_1985$gdp_c <- log(gov_1985$wdi_ny_gdp_pcap_kd) 
govfit_1985 <- lm(gov_1985$eiu_pgce ~ gov_1985$gdp_c + I(gov_1985$gdp_c^2)+ I(gov_1985$gdp_c^3)) 
gov_1985$predicted <- predict(govfit_1985) 
gov_1985 <- gov_1985[order(urb_1985$gdp_c),] 

gov_2021 <- my_data[, c('countrycodeiso', 'year', 'wdi_ny_gdp_pcap_kd', 'eiu_pgce')] 
gov_2021 <- gov_2021[gov_2021$year==2021,] 
gov_2021 <- na.omit(gov_2021) 
gov_2021$gdp_c <- log(gov_2021$wdi_ny_gdp_pcap_kd) 
govfit_2021 <- lm(gov_2021$eiu_pgce ~ gov_2021$gdp_c + I(gov_2021$gdp_c^2) + I(gov_2021$gdp_c^3)) 
gov_2021$predicted <- predict(govfit_2021) 
gov_2021 <- gov_2021[order(gov_2021$gdp_c),] 

#below, swap out VNM with your countrycodeiso. Also play with “pos” to make the labels readable 
plot(gov_1985$gdp_c, gov_1985$predicted, type="p", col="black", xlab="Log GDP per Capita", ylab="Government % GDP", ylim = c(8,22),xlim = c(3,12),
     main = "Growth in Government & GDP",sub = "Peru, 1985 to 2021") 
lines(gov_2021$gdp_c, gov_2021$predicted, type="l", col="green") 
govtemp_1985 <- gov_1985[gov_1985$countrycodeiso=='PER',] 
points(govtemp_1985$gdp_c, govtemp_1985$eiu_pgce, col="red") 
text(x = govtemp_1985$gdp_c, y = govtemp_1985$eiu_pgce, labels = "PER 1985", pos = 2, col="red") 
govtemp_2021 <- gov_2021[gov_2021$countrycodeiso=='PER',] 
points(govtemp_2021$gdp_c, govtemp_2021$eiu_pgce, col="green") 
text(x = govtemp_2021$gdp_c, y = govtemp_2021$eiu_pgce, labels = "PER 2021", pos = 4, col="green") 
arrows(govtemp_1985$gdp_c, govtemp_1985$eiu_pgce, govtemp_2021$gdp_c, govtemp_2021$eiu_pgce)

