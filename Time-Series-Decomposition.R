library(fpp3)

## GDP --------------------------------------------------------------------------

global_economy %>%
  filter(Country == "Australia") %>%
  autoplot(GDP)

global_economy %>%
  filter(Country == "Australia") %>%
  autoplot(GDP / Population)


## Print retail adjusted by CPI --------------------------------------------------

print_retail <- aus_retail %>%
  filter(Industry == "Newspaper and book retailing") %>%
  group_by(Industry) %>% # Just to keep the key in there
  index_by(Year = year(Month)) %>%
  summarise(Turnover = sum(Turnover))

aus_CPI <- global_economy %>% 
  filter(Code == "AUS") %>%
  select(CPI)

print_retail_adj <- print_retail %>%
  left_join(aus_CPI, by = "Year") %>%
  mutate(Adj_turnover = Turnover / CPI*100) %>%
  pivot_longer(c(Turnover, Adj_turnover),
    names_to = "Type", values_to = "Turnover"
  ) %>%
  select(Turnover,-Industry) 

# Plot both on same graph
print_retail_adj %>%
  ggplot(aes(x = Year, y = Turnover, col=Type)) +
  geom_line() +
  labs(title = "Turnover: Australian print media industry",
       y = "$AU")

# Use faceting
print_retail_adj %>%
  mutate(name = factor(Type,
                       levels=c("Turnover","Adj_turnover"))) %>%
  ggplot(aes(x = Year, y = Turnover)) +
  geom_line() +
  facet_grid(name ~ ., scales = "free_y") +
  labs(title = "Turnover: Australian print media industry",
       y = "$AU")

# Return to slides to comment

## Australian food retail --------------------------------------------------------

food <- aus_retail %>%
  filter(Industry == "Food retailing") %>%
  summarise(Turnover = sum(Turnover)) #summed over states

food %>% autoplot(Turnover) +
  labs(y = "Turnover ($AUD)")

food %>% autoplot(sqrt(Turnover)) +
  labs(y = "Square root turnover")

food %>% autoplot(log(Turnover)) +
  labs(y = "Log turnover")

# Tell i
food %>%
  features(Turnover, features = guerrero)

food %>% autoplot(box_cox(Turnover, 0.0524)) +
  labs(y = "Box-Cox transformed turnover")



## US retail employment ----------------------------------------------------------

us_retail_employment <- us_employment %>%
  filter(year(Month) >= 1990, Title == "Retail Trade") %>%
  select(-Series_ID)

us_retail_employment %>%
  autoplot(Employed) +
  xlab("Year") + ylab("Persons (thousands)") +
  ggtitle("Total employment in US retail")

dcmp <- us_retail_employment %>%
  model(stl = STL(Employed))

components(dcmp)

us_retail_employment %>%
  autoplot(Employed, color='gray') +
  autolayer(components(dcmp), trend, color='red') + #pull out the trend
  xlab("Year") + ylab("Persons (thousands)") +
  ggtitle("Total employment in US retail")

components(dcmp) %>% autoplot() + xlab("Year")

components(dcmp) %>% gg_subseries(season_year)

us_retail_employment %>%
  autoplot(Employed, color='gray') +
  autolayer(components(dcmp), season_adjust, color='blue') +
  xlab("Year") + ylab("Persons (thousands)") +
  ggtitle("Total employment in US retail")



#### MA -----------
us_retail_employment_ma <- us_retail_employment %>%
  mutate(
    `12-MA` = slider::slide_dbl(Employed, mean,
                                .before = 5, .after = 6, .complete = TRUE),
    `2x12-MA` = slider::slide_dbl(`12-MA`, mean,
                                  .before = 1, .after = 0, .complete = TRUE)
  )

autoplot(us_retail_employment_ma, Employed, color = "gray") +
  autolayer(us_retail_employment_ma, vars(`2x12-MA`),
            color = "#D55E00") +
  labs(y = "Persons (thousands)",
       title = "Total employment in US retail")

#### Classical decomposition

us_retail_employment %>%
  model(classical_decomposition(Employed, type = "additive")) %>%
  components() %>%
  autoplot() + xlab("Year") +
  ggtitle("Classical additive decomposition of total US retail employment")

##### STL decomposition

# Let's start with the basic model 

us_retail_employment %>%
  model(STL(Employed)) %>%
  components() %>% 
  autoplot() +
  ggtitle("STL decomposition: US retail employment")

# We saw this before
# Let's play around with some parameters and let's see what happens

# Let's control the seasonal component
# season(window=13) - Default setting - no science behind

# set season(window=91) equivalent to "periodic"
# set season(window=13)
# set season(window=9)
# set season(window=3)

us_retail_employment %>%
  model(STL(Employed ~ season(window="periodic"))) %>%
  components() %>% 
  autoplot() +
  ggtitle("STL decomposition: US retail employment")

# Let's control the trend 

# Default is complex automated algorithm depends on size of 
# season and noise - works fairly well so we usually leave it alone
# set trend(window=5) - notice the remainder
# set trend(window=99, 999)

# Default setting for monthly data is 21
us_retail_employment %>%
  model(STL(Employed ~ season(window=9))) %>%
  components() %>% 
  autoplot() +
  ggtitle("STL decomposition: US retail employment")

# Robust option - make this true and any outliers should 
# show up im remainder
us_retail_employment %>%
  model(STL(Employed ~ season(window=9) + trend(window=21), robust=TRUE)) %>%
  components() %>% 
  autoplot() +
  ggtitle("STL decomposition: US retail employment")

#
