# 1-year averaging
data_1yr <- read_csv(paste0(robbie_datasets, "one_yr_avg_data.csv")) %>%
  mutate(family_SES = as.factor(family_SES),
         sex = as.factor(sex),
         geokode = as.factor(geokode),
         civst = as.factor(civst),
         place_birth = as.factor(place_birth),
         residence = as.factor(residence)) %>%
  dplyr::filter(residence != "Unknown") %>%
  droplevels() %>%
  tidyr::drop_na(civst) %>%
  group_by(nr) %>% mutate(sum=sum(ck)) %>% dplyr::filter(sum==1)

# 5-year averaging
data_5yrs <- read_csv(paste0(robbie_datasets, "five_yrs_avg_data.csv")) %>%
  mutate(family_SES = as.factor(family_SES),
         sex = as.factor(sex),
         geokode = as.factor(geokode),
         civst = as.factor(civst),
         place_birth = as.factor(place_birth),
         residence = as.factor(residence)) %>%
  dplyr::filter(residence != "Unknown") %>%
  droplevels() %>%
  tidyr::drop_na(civst) %>%
  group_by(nr) %>% mutate(sum=sum(ck)) %>% dplyr::filter(sum==1)

# 10-year averaging
data_10yrs <- read_csv(paste0(robbie_datasets, "ten_yrs_avg_data.csv"))%>%
  mutate(family_SES = as.factor(family_SES),
         sex = as.factor(sex),
         geokode = as.factor(geokode),
         civst = as.factor(civst),
         place_birth = as.factor(place_birth),
         residence = as.factor(residence)) %>%
  dplyr::filter(residence != "Unknown") %>%
  droplevels() %>%
  tidyr::drop_na(civst) %>%
  group_by(nr) %>% mutate(sum=sum(ck)) %>% dplyr::filter(sum==1)

# Copies of columns for INLA
data_1yr$birth_year3 <- data_1yr$birth_year2 <- data_1yr$birth_year
data_1yr$age_diag2 <- data_1yr$age_diag
data_5yrs$birth_year3 <- data_5yrs$birth_year2 <- data_5yrs$birth_year
data_5yrs$age_diag2 <- data_5yrs$age_diag
data_10yrs$birth_year3 <- data_10yrs$birth_year2 <- data_10yrs$birth_year
data_10yrs$age_diag2 <- data_10yrs$age_diag

# Rename columns to work for code regardless of length of averaging
colnames(data_1yr) = gsub("_1yr", "", colnames(data_1yr))
colnames(data_5yrs) = gsub("_5yr", "", colnames(data_5yrs))
colnames(data_10yrs) = gsub("_10yr", "", colnames(data_10yrs))

# subtract EC from PM2.5 to make non-traffic PM2.5
data_1yr$PM25 = data_1yr$PM25 - data_1yr$EC
data_5yrs$PM25 = data_5yrs$PM25 - data_5yrs$EC
data_10yrs$PM25 = data_10yrs$PM25 - data_10yrs$EC

# Center and scale each pollutant
data_1yr$PM25 = as.vector(scale(data_1yr$PM25))
data_1yr$NOX = as.vector(scale(data_1yr$NOX))
data_1yr$NO2 = as.vector(scale(data_1yr$NO2))
data_1yr$CO = as.vector(scale(data_1yr$CO))
data_1yr$EC = as.vector(scale(data_1yr$EC))
data_1yr$O3 = as.vector(scale(data_1yr$O3))

data_5yrs$PM25 = as.vector(scale(data_5yrs$PM25))
data_5yrs$NOX = as.vector(scale(data_5yrs$NOX))
data_5yrs$NO2 = as.vector(scale(data_5yrs$NO2))
data_5yrs$CO = as.vector(scale(data_5yrs$CO))
data_5yrs$EC = as.vector(scale(data_5yrs$EC))
data_5yrs$O3 = as.vector(scale(data_5yrs$O3))

data_10yrs$PM25 = as.vector(scale(data_10yrs$PM25))
data_10yrs$NOX = as.vector(scale(data_10yrs$NOX))
data_10yrs$NO2 = as.vector(scale(data_10yrs$NO2))
data_10yrs$CO = as.vector(scale(data_10yrs$CO))
data_10yrs$EC = as.vector(scale(data_10yrs$EC))
data_10yrs$O3 = as.vector(scale(data_10yrs$O3))

# load neighborhood ses
load(paste0(parish.ses.folder,'sogn_udd_2009b_1aR'))
parish_ses = sogn_udd_2009b_1aR %>%
  rowwise() %>% 
  mutate(parish_ses = sum(ses23,ses4,ses567,ses89, na.rm=TRUE))
sogn_udd_2009b_1aR = NULL

# load look-up for each participant's parish code
parish_lookup = haven::read_dta(paste0(parish.ses.folder,'ALS_geo_adr_1989_2013_1989_5_2.dta'))

# merge parish code at index date for each participant, then merge parish ses info 
data_1yr = left_join(data_1yr, parish_lookup, by=c('nr','lobnr','ck'))
data_1yr = left_join(data_1yr, parish_ses, by=c('sognecode'))
data_5yrs = left_join(data_5yrs, parish_lookup, by=c('nr','lobnr','ck'))
data_5yrs = left_join(data_5yrs, parish_ses, by=c('sognecode'))
data_10yrs = left_join(data_10yrs, parish_lookup, by=c('nr','lobnr','ck'))
data_10yrs = left_join(data_10yrs, parish_ses, by=c('sognecode'))

# recode nr to go from 1...N for R-STAN
data_1yr$nr <- match(data_1yr$nr, unique(data_1yr$nr))
data_5yrs$nr <- match(data_5yrs$nr, unique(data_5yrs$nr))
data_10yrs$nr <- match(data_10yrs$nr, unique(data_10yrs$nr))