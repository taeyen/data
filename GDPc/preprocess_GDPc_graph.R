library(tidyverse)
library(readxl)
library(igraph)
"./gdp_data/GCP_Release_1.xlsx" #GDP data
"./gdp_data/cc-est2018-alldata.csv" #population
"./gdp_data/county_adjacency2010.csv" #county_adjacency
"./gdp_data/sf12010countydistance100miles" #county distance
"./gdp_data/CAGDP9/CAGDP9__ALL_AREAS_2001_2018.csv"

raw1 = read_csv("./gdp_data/CAGDP9/CAGDP9__ALL_AREAS_2001_2018.csv",
                na = c("(NA)", "(D)"))
names_raw1 = names(raw1)

# 1. GeoFIPS column includes GeoName,
#     which causes all other columns mismatch its names and 2018 column has only NA
# "GeoFIPS\", \"GeoName", if GeoName has comma, county and state.
# want to have GeoFIPs, county, state columnes
# 2. Once the data match to names, 
#    I want to keep GeoFIPs, GeoName, Region, 2001-2018, 
#    and "All industry total"(which is supposed to be "Description", not "Industry Classification") row

# 1. cut the non-necessary rows
# delete US column, keep all industry total only
raw2 <- raw1 %>% 
  filter(IndustryClassification == "All industry total", GeoFIPS != "00000\" ,\"United States") %>%
  separate(GeoFIPS, into = c("FIPS", "Location"), sep = "\" ,\"" ) %>% 
  separate(Location, c("County", "State"), sep = ", ") %>%
  mutate(FIPS_num = as.numeric(FIPS)) %>%
  filter(is.na(State) != TRUE) %>%
  dplyr::select(-c(GeoName, Region, TableName, LineCode, IndustryClassification, Description, "2018"))

oldnames = names(raw2)
names(raw2) = c(oldnames[1:3], paste0("GDP", 2001:2018), "FIPS_num")
all_gdp = raw2
all_gdp_long <- all_gdp %>% gather(key = "Year", value = "GDP", -c(FIPS, County, State))

# 2. GDPc

# GDP per Capita Data ####
#pop_files = paste0("./gdp_data/co-est", 2012:2015, "-alldata.csv")

pop2018 = read_csv("./gdp_data/cc-est2018-alldata.csv") # YEAR = 5:8 indicating 07/01/2012:2015
# 1:11 07/01/2008:2018

pop_00 = read_csv("./gdp_data/co-est00int-tot.csv")
pop_18 = read_csv("./gdp_data/co-est2018-alldata.csv")
names(pop_00)
names(pop_18)[1:18] # keep 1:18

# 1. SUMLEV only 50, STATE 2 digits + COUNTY 3 digits FIPS
# 00: POP est 2001-2010, 2010 census
# 18: ESTBase 2010, POP est 2010-2018 (compare 2010 census, ESTbase, est 2010)

# download from website.
#https://www2.census.gov/programs-surveys/popest/datasets/2000-2010/intercensal/county/
#co-est00int-alldata-01.csv 01:56
#https://www2.census.gov/programs-surveys/popest/datasets/2010-2018/counties/asrh/cc-est2018-alldata-01.csv
#https://www2.census.gov/programs-surveys/popest/datasets/2010-2018/counties/asrh/cc-est2018-alldata.csv
web_path = "https://www2.census.gov/programs-surveys/popest/datasets/2000-2010/intercensal/county/"

csv_filenames = c(paste0("co-est00int-alldata-0", 1:9, ".csv"),
                  paste0("co-est00int-alldata-", 10:56, ".csv"))
pop_1 = read_csv("https://www2.census.gov/programs-surveys/popest/datasets/2000-2010/intercensal/county/co-est00int-alldata-01.csv")
pop_2 = read_csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2018/counties/asrh/cc-est2018-alldata-01.csv")
  
pop_1 %>% filter(AGEGRP == 0) 

tmpdf1 = pop_1[1:19,8:10]
colSums(tmpdf1) == pop_1[20, 8:10]
pop_2[1:20,]
# want to get age group 0 which indicates total, and TOT_POP

# Currents ####
# sapply(pop_data, dim) # the number of rows are different

gdp_data_cd = read_xlsx("./gdp_data/GCP_Release_1.xlsx", sheet = 1) #Current Dollar GDP
gdp_data_real = read_xlsx("./gdp_data/GCP_Release_1.xlsx", sheet = 2) #Real GDP
names(gdp_data_cd)[6:9] = paste0("GDP", 2012:2015)
names(gdp_data_real)[6:9] = paste0("GDP", 2012:2015)

gdp_data_cd = gdp_data_cd[!is.na(gdp_data_cd$Postal),]
gdp_data_real = gdp_data_real[!is.na(gdp_data_real$Postal),]

drop.cols <- c('LineCode', 'IndustryName')
gdp_all_industry_cd <- gdp_data_cd %>% filter(LineCode == 1) %>% 
  dplyr::select(-one_of(drop.cols)) %>%
  mutate(GDP2012 = as.numeric(GDP2012), GDP2013 = as.numeric(GDP2013),
         GDP2014 = as.numeric(GDP2014), GDP2015 = as.numeric(GDP2015), FIPS = as.numeric(FIPS))
gdp_all_industry_real <- gdp_data_real %>% filter(LineCode == 1) %>% 
  dplyr::select(-one_of(drop.cols)) %>%
  mutate(GDP2012 = as.numeric(GDP2012), GDP2013 = as.numeric(GDP2013),
         GDP2014 = as.numeric(GDP2014), GDP2015 = as.numeric(GDP2015), FIPS_num = as.numeric(FIPS))

#YEAR: 5:8 = 2012:2015, AGEGRP == 0 = total
population <- pop2018 %>% filter(YEAR %in% 5:8) %>% filter(AGEGRP == 0) %>% 
  mutate(FIPS = paste0(STATE, COUNTY), FIPS_num = as.numeric(FIPS)) %>% 
  dplyr::select(FIPS_num, YEAR, TOT_POP) %>% spread(YEAR, TOT_POP)
names(population)[2:5] <- paste0('TOT_POP', 2012:2015)
# 1-> 2008, 2->2009, 3->2010, 4->2011
# 5->2012, 8->2015 9->2016, 10->2017, 11->2018

popul_11yr <- pop2018 %>% filter(AGEGRP == 0) %>% 
  mutate(FIPS = paste0(STATE, COUNTY), FIPS_num = as.numeric(FIPS)) %>% 
  dplyr::select(FIPS_num, YEAR, TOT_POP) %>% spread(YEAR, TOT_POP)
names(popul_11yr)[-1] <- paste0('TOT_POP', 2008:2018)

gdp_pop_data <- left_join(gdp_all_industry_real, population, by = "FIPS_num")
gdp_pop_data <- gdp_pop_data %>% 
  mutate(GDPc2012 = GDP2012/TOT_POP2012, GDPc2013 = GDP2013/TOT_POP2013,
         GDPc2014 = GDP2014/TOT_POP2014, GDPc2015 = GDP2015/TOT_POP2015)

#all_gdp or all_gdp_long
gdp_pop11_data <- left_join(all_gdp, popul_11yr, by = "FIPS_num")
gdp_pop11_data <- gdp_pop11_data %>% 
  mutate(GDPc2008 = GDP2008/TOT_POP2008, GDPc2009 = GDP2009/TOT_POP2009,
         GDPc2010 = GDP2010/TOT_POP2010, GDPc2011 = GDP2011/TOT_POP2011,
         GDPc2012 = GDP2012/TOT_POP2012, GDPc2013 = GDP2013/TOT_POP2013,
         GDPc2014 = GDP2014/TOT_POP2014, GDPc2015 = GDP2015/TOT_POP2015,
         GDPc2016 = GDP2016/TOT_POP2016, GDPc2017 = GDP2017/TOT_POP2017,
         GDPc2018 = GDP2018/TOT_POP2018)

# Generate Graph by using County Adjacency####
# Choose the county only having the  population
gdp_pop_data[is.na(gdp_pop_data$TOT_POP2012),]
naFIPS <- gdp_pop_data[is.na(gdp_pop_data$TOT_POP2012),1] #it seems special locations for GDP, and the FIPS codes don't exist in the adjancy data
gdp_pop_data <- gdp_pop_data[!is.na(gdp_pop_data$TOT_POP2012),]

# dropping NA

gdp_pop11_data <- gdp_pop11_data[!is.na(gdp_pop11_data$TOT_POP2008),] 


adj_data <- adj_data %>%mutate(strcnt = fipscounty %>% str_count()) %>%  
  mutate(fipscounty_char = ifelse(strcnt == 4, str_pad(fipscounty, 5, side = "left", pad = "0"), fipscounty)) %>%
  mutate(fipsneighbor_char = ifelse(strcnt == 4, str_pad(fipsneighbor, 5, side = "left", pad = "0"), fipsneighbor))


# genrate graph
adj_df <- data.frame(from = adj_data$fipscounty_char, to = adj_data$fipsneighbor_char)
g_all <- graph_from_data_frame(adj_df, directed = F, vertices = NULL)
g_all <- simplify(g_all)
fips_adj_list <- adj_data[,c(2,4)] %>% filter(fipscounty <= 56045, fipsneighbor <= 56045)
g_gdp <- graph_from_data_frame(data.frame(from = fips_adj_list$fipscounty, to = fips_adj_list$fipsneighbor), 
                               directed = F, vertices = NULL)
g_gdp <- simplify(g_gdp)


#save(gdp_pop_data, g_gdp, adj_data, g_ma, mid_atlantic, g_all, gdp_data, file = "gdp_graph.Rdata")

names(counties)[7] = "FIPS"
gdp_data <- left_join(gdp_pop_data, counties, by = "FIPS")

gdp_data %>%
  ggplot(aes(long, lat, fill = GDPc2012)) +
  geom_polygon(color = NA) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  labs(fill = "GDP per capita 2012")

# Graph by its distance ####
# Gaussian geometric graph
# c*exp(-\|x_i - x_j\|^2/\tau^2)
tau2 = 0.1
distance_100 = read.csv("./gdp_data/sf12010countydistance100miles.csv") 

# California ----
# use tau = 1st Quantile, median
dis_ca_ind = which((distance_100$county1 %in% cal$FIPS_num) &
        (distance_100$county2 %in% cal$FIPS_num))
distance_ca <- distance_100[dis_ca_ind,] %>%
  mutate(wij = exp(-mi_to_county^2/50^2), connected = wij > 0.5,
         wij2 =  exp(-mi_to_county^2/70^2), connected2 = wij2 > 0.5)
         
edgelist_ca <- distance_ca %>% filter(connected == TRUE) %>% 
  dplyr::select(county1, county2) 
edgelist_ca2 <- distance_ca %>% filter(connected2 == TRUE) %>% 
  dplyr::select(county1, county2) 

# Mid-Atlantic
dis_ma_ind = which((distance_100$county1 %in% mid_atlantic$FIPS) &
                     (distance_100$county2 %in% mid_atlantic$FIPS))
distance_ma <- distance_100[dis_ma_ind,] %>%
  mutate(wij = exp(-mi_to_county^2/45^2), connected = wij > 0.5,
         wij2 =  exp(-mi_to_county^2/67^2), connected2 = wij2 > 0.5)

edgelist_ma1 <- distance_ma %>% filter(connected == TRUE) %>% 
  dplyr::select(county1, county2) 
edgelist_ma2 <- distance_ma %>% filter(connected2 == TRUE) %>% 
  dplyr::select(county1, county2) 

get_fullgraph = function(node_name, edgelist){
  #node_name: node name vector which corresponding to elements in edgelist
  #           it should be character
  #edgelist: two column matrix which has from , to (undirected graph returen)
  #return graph
  n = length(node_name)
  mat = matrix(0, ncol = n, nrow = n)
  rownames(mat) = colnames(mat) = node_name
  for(i in 1:nrow(edgelist)){
    str1 = as.character(edgelist[i,1])
    str2 = as.character(edgelist[i,2])
    mat[str1, str2] = 1
  }
  g = graph_from_adjacency_matrix(mat, mode = "undirected", diag = FALSE)
  return(g)
}


g_ca_dis1 = get_fullgraph(as.character(cal$FIPS_num), edgelist = edgelist_ca)
g_ca_dis2 = get_fullgraph(as.character(cal$FIPS_num), edgelist = edgelist_ca2)
g_ma_dis1 = get_fullgraph(as.character(mid_atlantic$FIPS), edgelist = edgelist_ma1)
g_ma_dis2 = get_fullgraph(as.character(mid_atlantic$FIPS), edgelist = edgelist_ma2)





# =================== #
# Sub Graphs #
# Mid-Atlantic ####
# NY(34), NJ(36), PA(42), DE(10), MD(24), DC(11), WV(54), VA(82)
mid_atlantic <- gdp_pop_data %>% filter(Postal %in% c("NY", "NJ", "PA", "DE", "MD", "DC", "VA", "WV"))
mid_atl_adj <- adj_data %>% filter(fipscounty %in% mid_atlantic$FIPS) %>% filter(fipsneighbor_char %in% mid_atlantic$FIPS)
g_ma <- graph_from_data_frame(data.frame(from = mid_atl_adj$fipscounty, to = mid_atl_adj$fipsneighbor), 
                               directed = F, vertices = NULL)
g_ma <- igraph::simplify(g_ma)

mid_atlantic11 <- gdp_pop11_data %>% filter(State %in% c("NY", "NJ", "PA", "DE", "MD", "DC", "VA", "WV"))
mid_atl11_adj <- adj_data %>% filter(fipscounty %in% mid_atlantic11$FIPS) %>% 
  filter(fipsneighbor_char %in% mid_atlantic11$FIPS)

g_ma11 <- graph_from_data_frame(data.frame(from = mid_atl11_adj$fipscounty, 
                                           to = mid_atl11_adj$fipsneighbor), 
                              directed = F, vertices = NULL)
g_ma11 <- igraph::simplify(g_ma11)

deg_ma <- degree(g_ma, mode = "all")
#idx 258, FIPS 51770 Roanoke is not connected to others, but it's in the roanoke county


ma_va = mid_atlantic
ma_va11 = mid_atlantic11
g_mav = g_ma
g_mav11 = g_ma11

save(ma_va, g_mav, ma_va11, g_mav11, file = "MA_va.RData")

print(load("MA_va.RData"))

cal <- gdp_pop_data %>% filter(Postal == "CA")
cal_adj <- adj_data %>% filter(fipscounty_char %in% cal$FIPS) %>% filter(fipsneighbor_char %in% cal$FIPS)
g_ca <- graph_from_data_frame(data.frame(from = cal_adj$fipscounty, to = cal_adj$fipsneighbor), 
                              directed = F, vertices = NULL)
g_ca <- igraph::simplify(g_ca)

save(gdp_pop_data, g_gdp, adj_data, g_ma, mid_atlantic, g_all, gdp_data,
     cal, g_ca, eL_gdp, eL_ma, eL_ca,
     g_ma_dis1, g_ma_dis2, g_ca_dis1, g_ca_dis2,
     file = "gdp_graph.Rdata")

