data_1yr <- read_csv(paste0(robbie_datasets, "one_yr_avg_data.csv")) %>%
  mutate(family_SES = as.factor(family_SES),
         sex = as.factor(sex),
         geokode = as.factor(geokode),
         civst = as.factor(civst),
         place_birth = as.factor(place_birth),
         residence = as.factor(residence)) %>%
  dplyr::filter(residence != "Unknown") %>%
  droplevels() %>%
  drop_na(civst) %>%
  group_by(nr) %>% mutate(sum=sum(ck)) %>% filter(sum==1)

data_1yr$nr <- match(data_1yr$nr, unique(data_1yr$nr))

summary(data_1yr)

data_1yr$birth_year3 <- data_1yr$birth_year2 <- data_1yr$birth_year
data_1yr$age_diag2 <- data_1yr$age_diag

data_5yrs <- read_csv(paste0(robbie_datasets, "five_yrs_avg_data.csv")) %>%
  mutate(family_SES = as.factor(family_SES),
         sex = as.factor(sex),
         geokode = as.factor(geokode),
         civst = as.factor(civst),
         place_birth = as.factor(place_birth),
         residence = as.factor(residence)) %>%
  dplyr::filter(residence != "Unknown") %>%
  droplevels() %>%
  drop_na(civst) %>%
  group_by(nr) %>% mutate(sum=sum(ck)) %>% filter(sum==1)

data_5yrs$nr <- match(data_5yrs$nr, unique(data_5yrs$nr))

summary(data_5yrs)

data_5yrs$birth_year3 <- data_5yrs$birth_year2 <- data_5yrs$birth_year
data_5yrs$age_diag2 <- data_5yrs$age_diag

data_10yrs <- read_csv(paste0(robbie_datasets, "ten_yrs_avg_data.csv"))%>%
  mutate(family_SES = as.factor(family_SES),
         sex = as.factor(sex),
         geokode = as.factor(geokode),
         civst = as.factor(civst),
         place_birth = as.factor(place_birth),
         residence = as.factor(residence)) %>%
  dplyr::filter(residence != "Unknown") %>%
  droplevels() %>%
  drop_na(civst) %>%
  group_by(nr) %>% mutate(sum=sum(ck)) %>% filter(sum==1)

data_10yrs$nr <- match(data_10yrs$nr, unique(data_10yrs$nr))

summary(data_10yrs)

data_10yrs$birth_year3 <- data_10yrs$birth_year2 <- data_10yrs$birth_year
data_10yrs$age_diag2 <- data_10yrs$age_diag

colnames(data_1yr) = gsub("_1yr", "", colnames(data_1yr))
colnames(data_5yrs) = gsub("_5yr", "", colnames(data_5yrs))
colnames(data_10yrs) = gsub("_10yr", "", colnames(data_10yrs))

# subtract EC from PM2.5 to make non-traffic PM2.5
data_1yr$PM25 = data_1yr$PM25 - data_1yr$EC
data_5yrs$PM25 = data_5yrs$PM25 - data_5yrs$EC
data_10yrs$PM25 = data_10yrs$PM25 - data_10yrs$EC