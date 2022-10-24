# Texas data ####
library(tidyverse)
library(zoo)
library(igraph)
# source("gp_on_graph_functions.R")

# https://dshs.texas.gov/coronavirus/additionaldata.aspx
# https://demographics.texas.gov/Data/TPEPP/Estimates/
# https://www.census.gov/data/datasets/time-series/demo/popest/2020s-national-total.html#par_textimage_1810472256


# 1. Import Datasets ----
# 1.1 Covid Case per county #
covid_tx = readxl::read_xlsx("./covid_data/TX_COVID19_by_County_122321.xlsx", skip = 2) %>% drop_na()
raw_name = names(covid_tx)

# get date information
date_ls = str_match_all(raw_name[-1], "\\d+")
date_name20 = sapply(1:300,function(i){
  temp = date_ls[[i]]
  paste0("2020-", temp[1,1], "-", temp[2,1])
})
date_name21 = sapply(301:length(date_ls), function(i){
  temp = date_ls[[i]]
  paste0("2021-", temp[1,1], "-", temp[2,1])
})

names(covid_tx) = c("county", date_name20, date_name21)

# 1.2 County Population #
pop_tx = read_csv("./covid_data/2018_txpopest_county.csv") %>% 
  mutate(FIPS = str_c("48", FIPS)) %>%
  dplyr::select(FIPS, county, starts_with("jan1_2019_p")) %>%
  rename(population = jan1_2019_pop_est)

pop_census = read_csv("./covid_data/NA-EST2021-POP.xlsx")

# 1.3 region information: covid policy related #
regions_tx = readxl::read_xlsx("./covid_data/PHR_MSA_County_masterlist.xlsx",
                               sheet = 1) 
PHR_tx = regions_tx %>%
  mutate(FIPS = str_c("48", `FIPS #`)) %>% 
  rename(county = `County Name`, 
         PHR = `Public Health Region (11)`, 
         HSR = `Health Service Region (8)`) %>%
  dplyr::select(FIPS, county, PHR, HSR) %>% na.omit()


identical(covid_tx$county[-255], PHR_tx$county)
identical(PHR_tx$FIPS, pop_tx$FIPS) #pop_tx has 48509 State of Texas

# 1.4 Joining the three imported datasets.
# covid_tx + pop_tx + PHR_tx
# a) left_join(covid_tx, PHR_tx, by county)
#  + put FIPS = 48509 for total 
# b) join pop_tx
join1 = covid_tx %>% left_join(PHR_tx, by = "county")
tx_all = join1 %>% left_join(pop_tx, by = "FIPS")

# 1.5 
# keep county.x only
# create date column by pivot_longer
tx_allcases = tx_all %>% pivot_longer(cols = starts_with("20"),
                                      names_to = "date",
                                      values_to = "cases") %>%
  mutate(date = as.Date(date, "%Y-%m-%d")) %>%
  rename(county = county.x) %>%
  dplyr::select(-county.y)

# may write tx_allcases as a csv file :write_csv()
write_csv(tx_allcases, file = "TX_covidcase.csv")

# https://www.dshs.texas.gov/chs/hprc/counties.shtm
# TX counties are designated as Metropolitan or Non-Metropolitan (Rural or Urban)
#  by the US Office of Buget and Management (OMB).
# https://www.census.gov/programs-surveys/metro-micro.html

# 2. Neighboring Graph

adj_county = read_tsv("./covid_data/county_adjacency.txt", col_names = F)
names(adj_county) = c("From", "FromFIPS", "To", "ToFIPS")
adj_county = adj_county %>% fill(From) %>% fill(FromFIPS) #fill NA from top to bottom

# separate county and state
To_seplist = str_split(adj_county$To, ", ")
To_sepMat = do.call(rbind, To_seplist)
From_sepMat = do.call(rbind, str_split(adj_county$From, ", "))
adj_county = adj_county %>% 
  mutate(FromState = From_sepMat[,2], FromCounty = From_sepMat[,1],
         ToState = To_sepMat[,2], ToCounty = To_sepMat[,1]) %>%
  dplyr::select(-From, -To)


# get TX county only
adj_TXcounty = adj_county %>% filter(FromState == "TX" & ToState == "TX")
temp1 = adj_TXcounty %>% left_join(pop_tx, by = c("FromFIPS" = "FIPS"))
temp2 = temp1 %>% left_join(pop_tx, by = c("ToFIPS" = "FIPS")) %>% 
  rename(FromPop = population.x, ToPop = population.y) %>% 
  dplyr::select(-starts_with("county."))
adj_TXcounty_Weight = temp2 %>% mutate(weight1 = sqrt(FromPop*ToPop),
                                       weight2 = FromPop*ToPop)

# full graph:
edges_TX = adj_TXcounty %>% dplyr::select(FromFIPS, ToFIPS) %>%
  rename(from = FromFIPS, to = ToFIPS)
g_tx = graph_from_data_frame(edges_TX, directed = FALSE)
g_tx = simplify(g_tx)

save(tx_allcases, g_tx, file = "TX_Covid.RData")



