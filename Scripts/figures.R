### FIGURES ###

# Load libraries
library(data.table)
library(tidyverse)
library(mgcv)
library(maps)
library(ggsci)
library(RColorBrewer)

# Import data
un_dat <- readRDS('./Data/un_dat.rds')
in_dat <- readRDS('./Data/in_dat1.rds')


### Figure 1: Modelling Pipeline ###
# Build death model
mr_mod <- gam(Mortality_Rate ~ ti(Age) + ti(Time) + 
                ti(Age, Time), family = betar(),
              data = un_dat[Age >= 13 & Location == 'India'])

# Build Instagram model
in_mod <- gam(Users ~ s(Age, bs = 'cr', k = 10), 
              family = gaussian(link = 'log'),
              data = in_dat[Country == Country])

# Build population model
pop_mod <- gam(Population ~ ti(Age, k = 10) + ti(Time, k = 10) + 
                 ti(Age, Time, k = 10), family = gaussian(link = 'log'),
               data = un_dat[Age >= 13 & Location == 'India'])

# Build grid
df <- crossing(
  Assumption = c('Shrinking', 'Growing'),
        Time = 2018:2100,
         Age = 18:100
) %>% as.data.table(.)
# Predict mortality rate and population data
df[, mr_hat := predict(mr_mod, df, type = 'response')
  ][, pop_hat := predict(pop_mod, df, type = 'response')
  # Predict baseline FB data
  ][Time == 2018, in_hat := 
  predict(in_mod, df[Time == 2018], type = 'response')
  # No such thing as markets with penetration rate > 1
  ][in_hat > pop_hat, in_hat := pop_hat]
# Extend predictions under the shrinking assumption
for (year in 2019:2100) {
  survivors <- df[Assumption == 'Shrinking' & Time == year - 1,
                  in_hat * (1 - mr_hat)]
  df[Assumption == 'Shrinking' & Time == year, 
     in_hat := lag(survivors, default = 0)]
}
# Extend predictions under the growing assumption
baseline <- df[Assumption == 'Growing' & Time == 2018, in_hat]
df[Assumption == 'Growing' & Time > 2018, 
  in_hat := baseline * 1.13^(Time - 2018), by = Time
# But no penetration rates > 1
  ][in_hat > pop_hat, in_hat := pop_hat]
# Calculate Facebook mortalities
df[, IN_Deaths := mr_hat * in_hat]

# Figure 1A: Observed vs. predicted mortality rates
ggplot() + 
  geom_point(data = un_dat %>% filter(Location == 'India',
                                          Time == 2030,
                                           Age >= 18), 
             aes(Age, Mortality_Rate * 1000)) + 
  geom_line(data = df %>% filter(Assumption == 'Shrinking', Time == 2030), 
            aes(Age, mr_hat * 1000),
            size = 0.75, color = 'blue') + 
  labs(title = 'Projected Mortality: India, 2030',
           y = 'Deaths Per Thousand') +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))
ggsave('./Results/Figures/Fig_1A.2.pdf')

# Figure 1B: Observed vs. predict FB users
ggplot() + 
  geom_point(data = in_dat %>% filter(Country == 'India', Age < 65),
             aes(Age, Users / 1000)) + 
  geom_line(data = df %>% filter(Assumption == 'Shrinking', Time == 2018, 
                                 Age %in% 18:64), 
            aes(Age, in_hat / 1000), size = 0.75, color = 'blue') + 
  labs(title = 'Instagram Users: India, 2018',
       y = 'Users (Millions)') + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))
ggsave('./Results/Figures/Fig_1B.2.pdf')

# Figure 1C: FB mortalities by age
ggplot(df[Assumption == 'Shrinking' & Time == 2030 & Age >= 30], 
            aes(Age, IN_Deaths)) + 
  geom_line(size = 0.75, color = 'blue') + 
  labs(title = 'Projected Instagram Mortalities: India, 2030',
       y = 'Profiles (Thousands)') + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))
ggsave('./Results/Figures/Fig_1C.pdf')


### Figure 2: Ribbon plot, Scenario A ###
# By continent
countries <- fread('./Data/countries.csv')
colors <- data.table(
  Continent = c('Asia', 'North America', 'Europe', 
                'South America', 'Africa', 'Oceania'),
  Color = pal_d3()(6)
)

# Load data
df <- readRDS('./Results/Models/global_in_models2.rds')
df <- df[Status == 'Dead' & Assumption == 'Shrinking', ]
df[, CumSum := cumsum(Profiles / 1000), by = Country]
df <- merge(df, countries, by = 'Country')
df[, CumSum := sum(CumSum), by = .(Continent, Year)]
df <- distinct(df[, .(Continent, Year, CumSum)])

# We want biggest continents first
most_dead <- df %>%
  filter(Year == 2100) %>%
  inner_join(colors, by = 'Continent') %>%
  arrange(desc(CumSum))
df[, Continent := factor(Continent, levels = most_dead$Continent)]

# Plot 
ggplot(df, aes(Year, CumSum, group = Continent, fill = Continent)) + 
  geom_area(alpha = 0.9) +
  labs(title = 'Global Accumulation of Dead Profiles:\nScenario A',
       x = 'Year',
       y = 'Dead Profiles (Millions)') +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = most_dead$Color)
ggsave('./Results/Figures/Fig_2.2.pdf')

### Figure 3: Ribbon plot, Scenario B ###
# Load data
df <- readRDS('./Results/Models/global_in_models2.rds')
df <- df[Status == 'Dead' & Assumption == 'Growing', ]
df[, CumSum := cumsum(Profiles / 1000), by = Country]
df <- merge(df, countries, by = 'Country')
df[, CumSum := sum(CumSum), by = .(Continent, Year)]
df <- distinct(df[, .(Continent, Year, CumSum)])

# We want biggest countries first
most_dead <- df %>%
  filter(Year == 2100) %>%
  inner_join(colors, by = 'Continent') %>%
  arrange(desc(CumSum))
df[, Continent := factor(Continent, levels = most_dead$Continent)]

# Plot 
ggplot(df, aes(Year, CumSum, group = Continent, fill = Continent)) + 
  geom_area(alpha = 0.9) +
  labs(title = 'Global Accumulation of Dead Profiles:\nScenario B',
       x = 'Year',
       y = 'Dead Profiles (Millions)') +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = most_dead$Color)
ggsave('./Results/Figures/Fig_3.2.pdf')


### Figure 4: World Maps ###
# Cumulative global numbers under shrinking scenario
df <- readRDS('./Results/Models/global_in_models2.rds')
df <- df[Status == 'Dead' & Year %in% c(2050, 2100), ]
df[, CumSum := cumsum(Profiles * 1000), by = Country]

# Harmonize country names
world <- as.data.table(map_data('world'))
fix <- setdiff(df$Country, world$region)
df[Country == 'United States', Country := 'USA'
  ][Country == 'United Kingdom', Country := 'UK'
  ][Country == 'Czechia', Country := 'Czech Republic'
  ][Country == 'Republic of the Congo', Country := 'Republic of Congo'
  ][Country == 'St. Lucia', Country := 'Saint Lucia'
  ][Country == 'Réunion', Country := 'Reunion'
  ][Country == 'US Virgin Islands', Country := 'Virgin Islands'
  ][Country == 'Federated States of Micronesia', Country := 'Micronesia']
world[region == 'Trinidad', region := 'Trinidad and Tobago'
  ][region == 'Tobago', region := 'Trinidad and Tobago'
  ][region == 'Saint Vincent', region := 'Saint Vincent and the Grenadines'
  ][region == 'Grenadines', region := 'Saint Vincent and the Grenadines'
  ][subregion == 'Hong Kong', region := 'Hong Kong'
  ][subregion == 'Macao', region := 'Macau']

# Merge datasets
world <- world %>% rename(Country = region)
one <- merge(df[Assumption == 'Shrinking' & Year == 2050], world, 
             by = 'Country', all.y = TRUE
  )[, Assumption := 'Shrinking'
  ][, Year := 2050]
two <- merge(df[Assumption == 'Shrinking' & Year == 2100], world, 
             by = 'Country', all.y = TRUE
  )[, Assumption := 'Shrinking'
  ][, Year := 2100]
three <- merge(df[Assumption == 'Growing' & Year == 2050], world, 
               by = 'Country', all.y = TRUE
  )[, Assumption := 'Growing'
  ][, Year := 2050]
four <- merge(df[Assumption == 'Growing' & Year == 2100], world, 
              by = 'Country', all.y = TRUE
  )[, Assumption := 'Growing'
  ][, Year := 2100]
df <- rbind(one, two, three, four)
df <- df[Country != 'Antarctica']
df[, Assumption := as.factor(ifelse(Assumption == 'Shrinking', 
                                    'Scenario A', 'Scenario B'))] 

# Plot
ditch_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)
ggplot(df, aes(long, lat, fill = CumSum, group = group)) + 
  geom_polygon() +
  coord_equal() + 
  scale_fill_distiller(trans = 'log10', palette = 'Spectral',
                       name = 'Accumulated\nProfiles',
                       breaks = c(1e3, 1e5, 1e7),
                       labels = c('1k', '100k', '10m')) +
  labs(title = 'Global Distribution of Dead Instagram Profiles') + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  ditch_axes + 
  facet_grid(Assumption ~ Year)
ggsave('./Results/Figures/Fig_4.2.pdf')
