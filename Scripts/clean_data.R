### CLEAN UP UN AND FB DATA ###

# Load libraries
library(data.table)
library(tidyverse)


### UN Data ###

# Import UN data
mort <- read_csv('./Data/mortality.csv') 
pop <- read_csv('./Data/population.csv') 

# For columns 3-23, delete the space and class as numeric
for (j in 3:23) {
  mort[[j]] <- as.numeric(gsub(' ', '', mort[[j]]))
  pop[[j]] <- as.numeric(gsub(' ', '', pop[[j]]))
}

# Make sure both data frames have all and only the same columns 
mort <- mort %>% select(-Total)
pop <- pop %>% 
  mutate(Age_95 = Age_95 + Age_100) %>%
  select(-Age_100)

# Fix Time column in mort
mort <- mort %>% mutate(Time = as.integer(gsub(' -.*', '', Time)))

# Tidy data, shift to midpoint
mort <- mort %>% 
  gather(Age, Deaths, -Time, -Location) %>%
  mutate(Age = 2.5 + as.numeric(gsub('Age_', '', Age)), # Shift to midpoint
         Deaths = Deaths / 25,  # Five-year totals, five-year buckets
         Idx = paste(Location, Time, Age, sep = '.')) %>%
  as.data.table(.)
pop <- pop %>% 
  gather(Age, Population, -Time, -Location) %>%
  mutate(Age = 2.5 + as.numeric(gsub('Age_', '', Age)),
         Population = Population / 5,
         Idx = paste(Location, Time, Age, sep = '.')) %>%
  as.data.table(.)

# Merge data, calculate mortality rate
un_dat <- merge(mort, pop[, .(Idx, Population)], by = 'Idx'
  )[, Idx := NULL
# Some observations have impossible death totals
  ][Deaths > Population, Deaths := Population
# Calculate mortality rate
  ][, Mortality_Rate := Deaths / Population
# Fix country names 
  ][Location == 'Antigua and Barbuda', Location := 'Antigua'
  ][Location == 'Bolivia (Plurinational State of)', Location := 'Bolivia'
  ][Location == 'Brunei Darussalam', Location := 'Brunei'
  ][Location == 'Cabo Verde', Location := 'Cape Verde'
  ][Location == 'China, Hong Kong SAR', Location := 'Hong Kong'
  ][Location == 'China, Macao SAR', Location := 'Macau'
  ][Location == 'China, Taiwan Province of China', Location := 'Taiwan'
  ][Location == 'Congo', Location := 'Republic of the Congo'
  ][Location == "Lao People's Democratic Republic", Location := 'Laos'
  ][Location == 'Micronesia (Fed. States of)', Location := 'Federated States of Micronesia'
  ][Location == 'Republic of Korea', Location := 'South Korea'
  ][Location == 'Republic of Moldova', Location := 'Moldova'
  ][Location == 'Russian Federation', Location := 'Russia'
  ][Location == 'Saint Lucia', Location := 'St. Lucia'
  ][Location == 'TFYR Macedonia', Location := 'Macedonia'
  ][Location == 'United Republic of Tanzania', Location := 'Tanzania'
  ][Location == 'United States of America', Location := 'United States'
  ][Location == 'United States Virgin Islands', Location := 'US Virgin Islands'
  ][Location == 'Venezuela (Bolivarian Republic of)', Location := 'Venezuela'
  ][Location == 'Viet Nam', Location := 'Vietnam']


### Instagram Data ###

# Import Facebook data
in_dat <- fread('./Data/in_dat.csv')

# Remove zeros
total <- in_dat[, sum(Users), by = Country]
keep <- total[V1 >= 0, Country]
in_dat <- in_dat[Country %in% keep]
# Fix country names
in_dat[Country == 'The Gambia', Country := 'Gambia'
       ][Country == 'Czech Republic', Country := 'Czechia'
         ][Country == 'The Bahamas', Country := 'Bahamas']
# Harmonize countries
overlap <- intersect(un_dat$Location, in_dat$Country)
un_dat <- un_dat[Location %in% overlap]
in_dat <- in_dat[Country %in% overlap]
# Anchor all countries with 0 users of age 100
anchor <- data.table(
  Country = in_dat[, unique(Country)],
  Age = 100,
  Users = 0
)
in_dat <- rbind(in_dat, anchor)


# Export

saveRDS(in_dat, './Data/in_dat1.rds')
saveRDS(un_dat, './Data/un_dat.rds')
