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
  'sjlabelled',
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

peers_name <- c("Costa Rica","Colombia","Cuba")
peers_iso <- c("CRI","COL","CUB")


## Loading Data
database <- read_dta("input/Dev309_Database.dta") %>%
  select(wb_countryname,
         countrycodeiso,
         wbfs_region,
         year,
         pwt_hc,
         pwt_cn,
         pwt_ck,
         pwt_ctfp,
         pwt_cwtfp,
         pwt_rnna,
         pwt_rkna,
         pwt_rtfpna,
         pwt_rwtfpna,
         wdi_ny_gdp_mktp_kn,
         wdi_ny_gdp_mktp_kd,
         wdi_ny_gdp_mktp_kd_zg,
         wdi_ny_gdp_mktp_pp_kd,
         wdi_ny_gdp_pcap_kd,
         wdi_ny_gdp_pcap_kd_zg,
         wdi_ny_gdp_pcap_pp_kd,
         wdi_se_sec_durs,
         wdi_si_pov_gini)

schooling <- read_csv("input/mean-years-of-schooling-long-run.csv")

region_names <- database %>%
  mutate(across(c(wbfs_region), 
                sjlabelled::as_label, .names = '{col}_label')) %>%
  select(wbfs_region,wbfs_region_label) %>%
  filter(!is.na(wbfs_region)) %>%
  distinct()

## Checking income-based peers
peersinc_iso <- database %>%
  filter(year==2021) %>%
  select(countrycodeiso,wdi_ny_gdp_pcap_kd) %>%
  arrange(-wdi_ny_gdp_pcap_kd) %>%
  mutate(rank=rank(desc(wdi_ny_gdp_pcap_kd))) %>%
  filter(rank %in% c((rank[countrycodeiso=="PER"]-5):(rank[countrycodeiso=="PER"]+5))) %>%
  pull(countrycodeiso)

peersinc_name <- database %>%
  filter(year==2021) %>%
  select(wb_countryname,countrycodeiso,wdi_ny_gdp_pcap_kd) %>%
  arrange(-wdi_ny_gdp_pcap_kd) %>%
  mutate(rank=rank(desc(wdi_ny_gdp_pcap_kd))) %>%
  filter(rank %in% c((rank[countrycodeiso=="PER"]-5):(rank[countrycodeiso=="PER"]+5))) %>%
  pull(wb_countryname)


## Checking top 100 countries (by GDP)

top50gdp <- database %>%
  filter(year==2021) %>%
  select(countrycodeiso,year,wdi_ny_gdp_mktp_kd) %>%
  mutate(rank_gdp = rank(desc(wdi_ny_gdp_mktp_kd))) %>%
  filter(rank_gdp<=50) %>%
  pull(countrycodeiso)

top75gdp <- database %>%
  filter(year==2021) %>%
  select(countrycodeiso,year,wdi_ny_gdp_mktp_kd) %>%
  mutate(rank_gdp = rank(desc(wdi_ny_gdp_mktp_kd))) %>%
  filter(rank_gdp<=75) %>%
  pull(countrycodeiso)


## How developed is the country?
database %>%
  filter(countrycodeiso %in% top75gdp) %>%
  left_join(region_names, by="wbfs_region") %>%
  mutate(d_peru = ifelse(countrycodeiso=="PER",1,0)) %>%
  filter(year==2021) %>%
  select(countrycodeiso,wbfs_region_label,d_peru,wdi_ny_gdp_pcap_kd) %>%
  mutate(rank_gdppc = rank(desc(wdi_ny_gdp_pcap_kd))) %>%
  filter(rank_gdppc<=75) %>%
  ggplot(aes(y=reorder(paste0(rank_gdppc,". ",countrycodeiso),wdi_ny_gdp_pcap_kd),x=wdi_ny_gdp_pcap_kd)) +
  geom_bar(stat="identity",width=0.1) +
  geom_point(aes(color=as.factor(wbfs_region_label)),shape=16,size=3) +
  geom_hline(yintercept="48. PER",color="gray",alpha=0.2,linewidth=3) +
  scale_shape_manual(values=c(1,16)) +
  scale_color_manual(values=brewer.pal(n = 8, name = "Set2")) +
  scale_x_continuous(expand = c(0, 0)) +
  theme_classic() +
  theme(legend.position = c(0.8, 0.2),
        plot.title = element_text(face="bold"),
        axis.text.y = element_text(hjust = 0)) +
  guides(shape = FALSE) +
  labs(colour="Region", subtitle="Ranked by GDP Per Capita, Top 75 Largest GDP Only") +
  xlab("") +
  ylab("") +
  ggtitle("GDP per Capita (Constant 2015 USD), 2011")
ggsave("output/incomepc_ringline.png",width=6,height=10,dpi=500)

database %>%
  filter(countrycodeiso %in% top75gdp) %>%
  left_join(region_names, by="wbfs_region") %>%
  mutate(d_peru = ifelse(countrycodeiso=="PER",1,0)) %>%
  filter(year==2021) %>%
  select(countrycodeiso,wbfs_region_label,d_peru,wdi_ny_gdp_pcap_kd) %>%
  mutate(rank_gdppc = rank(desc(wdi_ny_gdp_pcap_kd))) %>%
  filter(rank_gdppc<=75) %>%
  ggplot(aes(y=reorder(paste0(rank_gdppc,". ",countrycodeiso),wdi_ny_gdp_pcap_kd),x=wdi_ny_gdp_pcap_kd)) +
  geom_bar(stat="identity",width=0.1) +
  geom_point(aes(color=as.factor(wbfs_region_label)),shape=16,size=3) +
  geom_hline(yintercept="48. PER",color="gray",alpha=0.3,linewidth=3) +
  scale_shape_manual(values=c(1,16)) +
  scale_color_manual(values=brewer.pal(n = 8, name = "Set2")) +
  scale_x_continuous(expand = c(0, 0)) +
  theme_classic() +
  theme(legend.position = c(0.8, 0.2),
        plot.title = element_text(face="bold"),
        axis.text.y = element_text(hjust = 0)) +
  guides(shape = FALSE) +
  labs(colour="Region", subtitle="Top 75 Largest GDP Only") + 
  xlab("") +
  ylab("") +
  ggtitle("GDP per Capita (Constant 2015 USD), 2021")
ggsave("output/incomepc_global_ringline.png",width=6,height=10,dpi=500)

database %>%
  #filter(countrycodeiso %in% top75gdp) %>%
  left_join(region_names, by="wbfs_region") %>%
  mutate(d_peru = ifelse(countrycodeiso=="PER",1,0)) %>%
  filter(wbfs_region==3) %>%
  filter(year==2021) %>%
  filter(!is.na(wdi_ny_gdp_pcap_kd)) %>%
  select(countrycodeiso,wbfs_region_label,d_peru,wdi_ny_gdp_pcap_kd) %>%
  mutate(rank_gdppc = rank(desc(wdi_ny_gdp_pcap_kd))) %>%
  filter(rank_gdppc<=75) %>%
  ggplot(aes(y=reorder(paste0(rank_gdppc,". ",countrycodeiso),wdi_ny_gdp_pcap_kd),x=wdi_ny_gdp_pcap_kd)) +
  geom_bar(stat="identity",width=0.1) +
  geom_point(color="chartreuse4",shape=16,size=3) +
  geom_hline(yintercept="22. PER",color="gray",alpha=0.3,linewidth=3) +
  scale_shape_manual(values=c(1,16)) +
  scale_color_manual(values=brewer.pal(n = 8, name = "Set2")) +
  scale_x_continuous(expand = c(0, 10)) +
  theme_classic() +
  theme(legend.position = c(0.8, 0.2),
        plot.title = element_text(face="bold"),
        axis.text.y = element_text(hjust = 0)) +
  guides(shape = FALSE, color = FALSE) +
  labs(colour="Region", subtitle="Latin America and The Carribean") +
  xlab("") +
  ylab("") +
  ggtitle("GDP per Capita (Constant 2015 USD), 2011")
ggsave("output/incomepc_regional_ringline.png",width=6,height=6,dpi=500)

database %>%
  left_join(region_names, by="wbfs_region") %>%
  filter(countrycodeiso %in% top75gdp) %>%
  mutate(d_peru = ifelse(countrycodeiso=="PER",1,0)) %>%
  filter(year %in% c(2011,2021)) %>%
  filter(wdi_ny_gdp_pcap_kd>3000) %>%
  select(countrycodeiso,year,wbfs_region_label,d_peru,wdi_ny_gdp_pcap_kd) %>%
  mutate(year = paste0("gdppc_",year)) %>%
  pivot_wider(names_from=year,values_from=wdi_ny_gdp_pcap_kd) %>%
  mutate(gdppc_2011_rank = rank(desc(gdppc_2011))) %>%
  mutate(gdppc_2021_rank = rank(desc(gdppc_2021))) %>%
  ggplot(aes(x=gdppc_2011_rank,y=gdppc_2021_rank)) +
  geom_text(aes(label=countrycodeiso, color=as.factor(d_peru)),size=3) +
  scale_x_reverse() +
  scale_y_reverse() +
  theme_classic() +
  theme(panel.grid.major = element_line(),
        plot.title = element_text(face="bold")) +
  geom_abline(intercept = 0, slope = 1, size = 0.5) +
  guides(color = FALSE) +
  scale_color_manual(values=c("gray70","red")) +
  xlab("Global Ranking of GDP Per Capita in 2011") +
  ylab("Global Ranking of GDP Per Capita in 2021") +
  ggtitle("Change in GDP Per Capita Ranking, 2011-2021")
ggsave("output/incomepc_rankchange_scatter.png",width=6,height=6,dpi=500)
  
  
## How fast is it growing?
database %>%
  filter(countrycodeiso %in% top75gdp) %>%
  left_join(region_names, by="wbfs_region") %>%
  mutate(d_peru = ifelse(countrycodeiso=="PER",1,0)) %>%
  filter(year>2000 & year<=2021) %>%
  select(countrycodeiso,year,d_peru,wdi_ny_gdp_pcap_kd_zg) %>%
  group_by(countrycodeiso) %>%
  mutate(med_growth = median(wdi_ny_gdp_pcap_kd_zg,na.rm=TRUE)) %>%
  group_by(year) %>%
  mutate(rank_med_growth = rank(desc(med_growth))) %>%
  filter(rank_med_growth<=75) %>%
  ggplot(aes(y=reorder(paste0(rank_med_growth,". ",countrycodeiso),med_growth),x=wdi_ny_gdp_pcap_kd_zg,color=as.factor(d_peru))) +
  geom_boxplot(notch=FALSE) +
  geom_hline(yintercept="15. PER",color="gray",alpha=0.2,linewidth=3) +
  guides(color = FALSE) +
  scale_color_manual(values=c("gray70","red")) +
  theme_classic() +
  theme(legend.position = c(0.8, 0.2),
        plot.title = element_text(face="bold"),
        axis.text.y = element_text(hjust = 0)) +
  xlim(c(-15,15)) +
  xlab("GDP Per Capita Growth Rate (%)") +
  ylab("") +
  labs(colour="Region", subtitle="Ranked by Median GDP Per Capita Growth Rate, Top 75 Largest GDP Only") +
  ggtitle("Boxplot of GDP Per Capita Growth Rates 2000-2021")
ggsave("output/gdppcgrowth_boxplot.png",width=6,height=10,dpi=500)

database %>%
  filter(countrycodeiso %in% top75gdp) %>%
  left_join(region_names, by="wbfs_region") %>%
  filter(wbfs_region==3) %>%
  mutate(d_peru = ifelse(countrycodeiso=="PER",1,0)) %>%
  filter(year>2000 & year<=2021) %>%
  select(countrycodeiso,year,d_peru,wdi_ny_gdp_pcap_kd_zg) %>%
  group_by(countrycodeiso) %>%
  mutate(med_growth = median(wdi_ny_gdp_pcap_kd_zg,na.rm=TRUE)) %>%
  group_by(year) %>%
  mutate(rank_med_growth = rank(desc(med_growth))) %>%
  filter(rank_med_growth<=75) %>%
  ggplot(aes(y=reorder(paste0(rank_med_growth,". ",countrycodeiso),med_growth),x=wdi_ny_gdp_pcap_kd_zg,color=as.factor(d_peru))) +
  geom_boxplot(notch=FALSE) +
  geom_hline(yintercept="15. PER",color="gray",alpha=0.2,linewidth=3) +
  guides(color = FALSE) +
  scale_color_manual(values=c("gray70","red")) +
  theme_classic() +
  theme(legend.position = c(0.8, 0.2),
        plot.title = element_text(face="bold"),
        axis.text.y = element_text(hjust = 0)) +
  xlim(c(-15,15)) +
  xlab("GDP Per Capita Growth Rate (%)") +
  ylab("") +
  labs(colour="Region", subtitle="Ranked by Median GDP Per Capita Growth Rate, Top 75 Largest GDP Only") +
  ggtitle("Boxplot of GDP Per Capita Growth Rates 2000-2021")
ggsave("output/gdppcgrowth_regional_boxplot.png",width=6,height=4,dpi=500)

## How volatile is growth?
database %>%
  filter(countrycodeiso %in% c("PER",peersinc_iso,peers_iso)) %>%
  mutate(d_peru = ifelse(countrycodeiso=="PER",1,0)) %>%
  filter(year>2000 & year<=2021) %>%
  select(countrycodeiso,year,d_peru,wdi_ny_gdp_pcap_kd_zg) %>%
  ggplot(aes(x=year,y=wdi_ny_gdp_pcap_kd_zg,color=countrycodeiso,alpha=as.factor(d_peru))) +
  geom_line() +
  geom_point() +
  theme_classic() +
  theme(plot.title = element_text(face="bold")) +
  guides(alpha = FALSE) +
  labs(colour="", subtitle="Selected Peer Countries of Peru") +
  xlab("") +
  ylab("") +
  ggtitle("GDP Capita Growth Rates (%)")
ggsave("output/gdppcgrowth_line.png",width=7,height=4,dpi=500)

database %>%
  filter(countrycodeiso %in% c("PER",peersinc_iso,peers_iso)) %>%
  mutate(d_peru = ifelse(countrycodeiso=="PER",1,0)) %>%
  filter(year>2000 & year<=2021) %>%
  group_by(countrycodeiso) %>%
  summarise(gdppc_gr_sd = sd(wdi_ny_gdp_pcap_kd_zg,na.rm=TRUE), d_peru=mean(d_peru)) %>%
  ggplot(aes(x=reorder(countrycodeiso,gdppc_gr_sd),y=gdppc_gr_sd,fill=as.factor(d_peru))) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("gray70","red")) +
  theme_classic() +
  theme(plot.title = element_text(face="bold"),
        legend.position="none") +
  labs(fill="", subtitle="Selected Peer Countries of Peru") +
  xlab("") +
  ylab("") +
  ggtitle("Standard Deviation of GDP Capita Growth Rates (%)")
ggsave("output/gdppcgrowth_sd_bar.png",width=7,height=4,dpi=500)

database %>%
  filter(countrycodeiso %in% c("PER",peersinc_iso,peers_iso)) %>%
  left_join(region_names, by="wbfs_region") %>%
  mutate(d_peru = ifelse(countrycodeiso=="PER",1,0)) %>%
  filter(year>2000 & year<=2021) %>%
  select(countrycodeiso,d_peru,wdi_ny_gdp_pcap_kd_zg) %>%
  group_by(countrycodeiso) %>%
  mutate(gdppc_gr_sd = sd(wdi_ny_gdp_pcap_kd_zg,na.rm=TRUE)) %>%
  mutate(med_growth = median(wdi_ny_gdp_pcap_kd_zg,na.rm=TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x=reorder(countrycodeiso,gdppc_gr_sd),y=wdi_ny_gdp_pcap_kd_zg,color=as.factor(d_peru))) +
  geom_boxplot(notch=FALSE) +
  guides(color = FALSE) +
  scale_color_manual(values=c("gray70","red")) +
  theme_classic()

## Is inequality up or down?
database %>%
  filter(countrycodeiso %in% c("PER",peersinc_iso,peers_iso)) %>%
  left_join(region_names, by="wbfs_region") %>%
  mutate(d_peru = ifelse(countrycodeiso=="PER",1,0)) %>%
  filter(year==2011 | year==2021) %>%
  select(countrycodeiso, d_peru, year, wdi_si_pov_gini) %>%
  group_by(countrycodeiso) %>%
  mutate(gini_avg=mean(wdi_si_pov_gini)) %>%
  filter(!is.na(gini_avg)) %>%
  ggplot(aes(y=reorder(countrycodeiso,gini_avg),x=wdi_si_pov_gini)) +
  geom_hline(yintercept="PER",color="gray",alpha=0.2,linewidth=7) +
  geom_point(aes(color=as.factor(year)),size=3) +
  geom_line(aes(group = countrycodeiso,color=as.factor(year))) +
  scale_color_manual(values=c("red3","chartreuse3")) +
  theme_classic() +
  theme(plot.title = element_text(face="bold")) +
  labs(color="", subtitle="Selected Peer Countries of Peru") +
  xlab("") +
  ylab("") +
  ggtitle("Gini Coefficient")
ggsave("output/gini_line bar.png",width=7,height=4,dpi=500)


## What about human and physical capital accumulation?
# Human Capital
database %>%
  filter(countrycodeiso %in% c("PER",peersinc_iso,peers_iso)) %>%
  left_join(region_names, by="wbfs_region") %>%
  mutate(d_peru = ifelse(countrycodeiso=="PER",1,0)) %>%
  filter(year==2011 | year==2019) %>%
  select(countrycodeiso, d_peru, year, pwt_hc) %>%
  group_by(countrycodeiso) %>%
  mutate(pwt_hc_avg=mean(pwt_hc)) %>%
  filter(!is.na(pwt_hc_avg)) %>%
  ggplot(aes(y=reorder(countrycodeiso,pwt_hc_avg),x=pwt_hc)) +
  geom_hline(yintercept="PER",color="gray",alpha=0.2,linewidth=7) +
  geom_point(aes(color=as.factor(year)),size=3) +
  geom_line(aes(group = countrycodeiso,color=as.factor(year))) +
  scale_color_manual(values=c("chartreuse3","red3")) +
  theme_classic() +
  theme(plot.title = element_text(face="bold")) +
  labs(color="", subtitle="Selected Peer Countries of Peru") +
  xlab("") +
  ylab("") +
  ggtitle("PWT Human Capital Index")
ggsave("output/hcindex_line bar.png",width=7,height=4,dpi=500)

# Human Capital index
database %>%
  filter(countrycodeiso %in% c("PER",peersinc_iso,peers_iso)) %>%
  left_join(region_names, by="wbfs_region") %>%
  mutate(d_peru = ifelse(countrycodeiso=="PER",1,0)) %>%
  select(countrycodeiso, d_peru, year, pwt_hc) %>%
  group_by(countrycodeiso) %>%
  mutate(pwt_hc_2001 = pwt_hc[year==2001]) %>%
  mutate(pwt_hc_by2001 = pwt_hc/pwt_hc_2001*100) %>%
  filter(year>=2001 & year<=2019) %>%
  ggplot(aes(x=year,y=pwt_hc_by2001,color=countrycodeiso,alpha=as.factor(d_peru))) +
  geom_line() +
  geom_point() +
  theme_classic() +
  theme(plot.title = element_text(face="bold")) +
  labs(color="", subtitle="Selected Peer Countries of Peru") +
  guides(alpha=FALSE) +
  xlab("") +
  ylab("") +
  ggtitle("PWT Human Capital Index (2001=100)")
ggsave("output/hcindex_index_line.png",width=7,height=4,dpi=500)

# Capital stock
database %>%
  filter(countrycodeiso %in% c("PER",peersinc_iso,peers_iso)) %>%
  left_join(region_names, by="wbfs_region") %>%
  mutate(d_peru = ifelse(countrycodeiso=="PER",1,0)) %>%
  filter(year==2011 | year==2019) %>%
  select(countrycodeiso, d_peru, year, pwt_cn) %>%
  group_by(countrycodeiso) %>%
  mutate(pwt_cn_avg=mean(pwt_cn)) %>%
  filter(!is.na(pwt_cn_avg)) %>%
  ggplot(aes(y=reorder(countrycodeiso,pwt_cn_avg),x=pwt_cn)) +
  geom_hline(yintercept="PER",color="gray",alpha=0.2,linewidth=7) +
  geom_point(aes(color=as.factor(year)),size=2) +
  geom_line(aes(group = countrycodeiso,color=as.factor(year))) +
  scale_color_manual(values=c("chartreuse3","red3")) +
  theme_classic() +
  theme(plot.title = element_text(face="bold")) +
  labs(color="", subtitle="Selected Peer Countries of Peru") +
  xlab("") +
  ylab("") +
  ggtitle("Capital Stock at Current PPP (in 2017 USD mn)")
ggsave("output/capitalstock_line bar.png",width=7,height=4,dpi=500)

# Capital stock index
database %>%
  filter(countrycodeiso %in% c("PER",peersinc_iso,peers_iso)) %>%
  left_join(region_names, by="wbfs_region") %>%
  mutate(d_peru = ifelse(countrycodeiso=="PER",1,0)) %>%
  select(countrycodeiso, d_peru, year, pwt_cn) %>%
  group_by(countrycodeiso) %>%
  mutate(pwt_cn_2001 = pwt_cn[year==2001]) %>%
  mutate(pwt_cn_by2001 = pwt_cn/pwt_cn_2001*100) %>%
  filter(year>=2001 & year<=2019) %>%
  ggplot(aes(x=year,y=pwt_cn_by2001,color=countrycodeiso,alpha=as.factor(d_peru))) +
  geom_line() +
  geom_point() +
  theme_classic() +
  theme(plot.title = element_text(face="bold")) +
  labs(color="", subtitle="Selected Peer Countries of Peru") +
  guides(alpha=FALSE) +
  xlab("") +
  ylab("") +
  ggtitle("Capital Stock at Current PPP (2001=100)")
ggsave("output/capitalstock_index_line.png",width=7,height=4,dpi=500)

# Capital stock per GDP
database %>%
  filter(countrycodeiso %in% c("PER",peersinc_iso,peers_iso)) %>%
  left_join(region_names, by="wbfs_region") %>%
  mutate(d_peru = ifelse(countrycodeiso=="PER",1,0)) %>%
  filter(year==2011 | year==2019) %>%
  select(countrycodeiso, d_peru, year, pwt_cn, wdi_ny_gdp_mktp_pp_kd) %>%
  mutate(pwt_cn = (pwt_cn*10^6)/wdi_ny_gdp_mktp_pp_kd*100) %>%
  group_by(countrycodeiso) %>%
  mutate(pwt_cn_avg=mean(pwt_cn)) %>%
  filter(!is.na(pwt_cn_avg)) %>%
  ggplot(aes(y=reorder(countrycodeiso,pwt_cn_avg),x=pwt_cn)) +
  geom_hline(yintercept="PER",color="gray",alpha=0.2,linewidth=7) +
  geom_point(aes(color=as.factor(year)),size=2) +
  geom_line(aes(group = countrycodeiso,color=as.factor(year))) +
  scale_color_manual(values=c("chartreuse3","red3")) +
  theme_classic() +
  theme(plot.title = element_text(face="bold")) +
  labs(color="", subtitle="Selected Peer Countries of Peru") +
  xlab("") +
  ylab("") +
  ggtitle("Capital Stock (% of GDP)")
ggsave("output/capitalstock_pergdp_linebar.png",width=7,height=4,dpi=500)
  

## What about TFP growth?
database %>%
  filter(countrycodeiso %in% c("PER",peersinc_iso,peers_iso)) %>%
  left_join(region_names, by="wbfs_region") %>%
  mutate(d_peru = ifelse(countrycodeiso=="PER",1,0)) %>%
  filter(year==2011 | year==2019) %>%
  select(countrycodeiso, d_peru, year, pwt_rtfpna) %>%
  group_by(countrycodeiso) %>%
  mutate(pwt_rtfpna_avg=mean(pwt_rtfpna)) %>%
  filter(!is.na(pwt_rtfpna_avg)) %>%
  ggplot(aes(y=reorder(countrycodeiso,pwt_rtfpna_avg),x=pwt_rtfpna)) +
  geom_hline(yintercept="PER",color="gray",alpha=0.2,linewidth=7) +
  geom_point(aes(color=as.factor(year)),size=2) +
  geom_line(aes(group = countrycodeiso,color=as.factor(year))) +
  scale_color_manual(values=c("chartreuse3","red3")) +
  theme_classic() +
  theme(plot.title = element_text(face="bold")) +
  labs(color="", subtitle="Selected Peer Countries of Peru") +
  xlab("") +
  ylab("") +
  ggtitle("TFP at Constant National Prices")
ggsave("output/tfp_index_linebar.png",width=7,height=4,dpi=500)

# TFP index
database %>%
  filter(countrycodeiso %in% c("PER",peersinc_iso,peers_iso)) %>%
  left_join(region_names, by="wbfs_region") %>%
  mutate(d_peru = ifelse(countrycodeiso=="PER",1,0)) %>%
  select(countrycodeiso, d_peru, year, pwt_rtfpna) %>%
  group_by(countrycodeiso) %>%
  mutate(pwt_rtfpna_2001 = pwt_rtfpna[year==2001]) %>%
  mutate(pwt_rtfpna_by2001 = pwt_rtfpna/pwt_rtfpna_2001*100) %>%
  filter(year>=2001 & year<=2019) %>%
  ggplot(aes(x=year,y=pwt_rtfpna_by2001,color=countrycodeiso,alpha=as.factor(d_peru))) +
  geom_line() +
  geom_point() +
  theme_classic() +
  theme(plot.title = element_text(face="bold")) +
  labs(color="", subtitle="Selected Peer Countries of Peru") +
  guides(alpha=FALSE) +
  xlab("") +
  ylab("") +
  ggtitle("TFP at Constant National Prices (2001=100)")
ggsave("output/tfp_index_line.png",width=7,height=4,dpi=500)


