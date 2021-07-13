
library(fpp3)
?fpp3 
global_economy
# Back to notes

## tsibble objects --------------------------------------------------------------------

z <- tibble(Month = paste(2019, month.abb[1:5]), 
            Observation = c(50, 23, 34, 30, 25))

z %>%
  mutate(Month = yearmonth(Month)) %>%
  as_tsibble(index = Month)

# Back to notes

## Explore the OLYMPICS tsibble ----------------------------------------------------------------

olympic_running %>% distinct(Length)
olympic_running$Year %>% range()

# We'll talk about autoplot soon but let's have a quick look
olympic_running %>%
  filter(Length=="100") %>%
  autoplot(Time)

# Just to show you what you can do 
ggplot(olympic_running, aes(x=Year, y = Time, colour = Sex)) +
  geom_line() +
  geom_point(size = 1) +
  facet_wrap(~ Length, scales = "free_y", nrow = 2) + 
  theme_minimal() + 
  scale_color_brewer(palette = "Dark2") + 
  theme(legend.position = "bottom", legend.title = element_blank()) +
  labs(title = "Olympic running times",
       y="Seconds")


# Back to slides

## Prison ------------------------------------------------------------------

library(readr)
prison <- readr::read_csv("prison_population.csv") %>%
  mutate(Quarter = yearquarter(date)) %>%
  select(-date) %>%
  as_tsibble(
    index = Quarter,
    key = c(state, gender, legal, indigenous)
  ) %>% relocate(Quarter)
prison


prison %>% distinct(state)
prison %>% distinct(gender)
prison %>% distinct(legal)
prison %>% distinct(indigenous)

prison %>% distinct(state, gender)

prison$state %>% unique
prison$state %>% unique %>% as.matrix()

8*2*2*2

# Back to slides

# Examples of tsibbles in packages

## PBS -----------------------------------------------------------------------------------------------------------------
# Exploring a rich tsibble

PBS
PBS %>% View()
PBS %>% distinct(ATC1_desc)
PBS %>% distinct(ATC2_desc)

# Back to slides

PBS %>%
  filter(ATC2=="A10") %>%
  select(Cost)

# Back to slides

PBS %>%
  filter(ATC2 == "A10")

PBS %>%
  filter(ATC2 == "A10") %>% 
  filter_index(~"1991 Jul")

# Back to slides

# Let's sum Cost of A10 across Concession and Type, i.e., total A10 cost  
a10 <- PBS %>%
  filter(ATC2 == "A10") %>%
  select(Month, Concession, Type, Cost) %>%
  summarise(total_cost = sum(Cost)) %>%
  mutate(total_cost = total_cost / 1e6) 

a10

# Back to slides - will show autoplot() later

a10 %>% autoplot() 
# Note - points/obs are joint by lines 

a10 %>% ggplot(aes(x=Month, y=total_cost)) +
  geom_point()


a10 %>% autoplot(total_cost)

a10 %>% autoplot(total_cost) +
  labs(title = "Antidiabetic drug sales",
       y = "$ million")



## ANSETT -----------------------------------------------------------------------------------------------------------------------------
# This is available in tsibbledata - recall it loads when you load fpp3
ansett

ansett %>% autoplot()

attributes(ansett)

# [1W] Weekly data
# 4 Variables
# 2 Keys: Airports and Class
# 1 Time Index
# 1 Passengers 

ansett %>% distinct(Class) 
ansett %>% distinct(Airports)

# 10 Airports * 3 Class = 30 unique time series

# Let's filter some particular series

melsyd <- ansett %>%
  filter(Airports=="MEL-SYD") %>%
  select(-Airports) 
  

melsyd # Have a look at what we have
melsyd %>% autoplot()

# So we now have all Melbourne to Sydney travel 
# for all three classes


# Let's filter and plot the Economy class series
melsyd %>%
  filter(Class=="Economy") %>%
  mutate(Passengers = Passengers/1000) %>% 
  autoplot(Passengers) +
  labs(title = "Ansett airlines economy class",
       subtitle = "Melbourne-Sydney", 
       y = "Passengers ('000)")

# back to slides

# SHOW THIS EXAMPLE
# Making a tsibble out of a ts object
?lynx
attributes(lynx)
lynx %>% as_tsibble()
lynx %>% as_tsibble() %>% autoplot(value)

# Explore the pelt tsibble
help(pelt)
?pelt #https://en.wikipedia.org/wiki/Hudson%27s_Bay_Company
attributes(pelt)
# A bit of a plotting trick/lesson that may be useful later on
pelt %>% autoplot(Lynx)

pelt %>%
  pivot_longer(2:3, names_to = "key")  %>% 
  ggplot(aes(x=Year, y=value, colour=key)) +
  geom_line()
# Note this is annual data - talk about population cycle
# Lynx eating Hare 

# Add some axis labels and title
pelt %>%
  pivot_longer(2:3, names_to = "key")  %>% 
  ggplot(aes(x=Year, y=value, colour=key)) +
  geom_line() + ggtitle("Lynx eating Hare") +
  ylab("Animals")


## AUS ECONOMY
# A nice trick with distinct
global_economy %>% distinct(Code, Country) 
global_economy$Year %>% unique() %>% range()
global_economy %>% distinct(Year) %>% range()

aus_economy <- global_economy %>%
  filter(Code == "AUS")

aus_economy

aus_economy %>%
  autoplot(GDP) +
  scale_y_log10()


# Your turn
aus_production %>%
  autoplot(Bricks) +
  labs(title = "Australian clay brick production",
       y= "Units (millions)")

# MAX TEMPERATURE IN MELBOURNE ------------------------------------------------------------

vic_elec

vic_elec %>%
  autoplot(Temperature)

# Index by day - notice now: Groups @ Day
vic_elec %>%
  index_by(Day = date(Time))

# Let's summarise over Groups - hence daily summaries
maxtemp <- vic_elec %>%
  index_by(Day = date(Time)) %>%
  summarise(
    Temperature = max(Temperature),
    Demand = sum(Demand),
    Holiday = any(Holiday)
  )

maxtemp %>%
  autoplot(Temperature) +
  ylab("Max temperature")

maxtemp %>%
  ggplot(aes(x = Day, y = Temperature)) +
  geom_point() + ylab("Max temperature")

# An alternative plot
maxtemp %>%
  ggplot(aes(x = Day, y = 1)) +
  geom_tile(aes(fill = Temperature)) +
  scale_fill_gradient2(
    low = "navy",
    mid = "yellow",
    high = "red", midpoint=28) +
  ylab("") + scale_y_discrete(expand=c(0,0))


# Season plots and subseries plots ------------------------------------------------------------

## PBS AGAIN

a10 %>% gg_season(total_cost, labels = "both") +
  labs(y= "$ million",
        title="Seasonal plot: antidiabetic drug sales")

a10 %>%
  gg_subseries(total_cost) +
  labs(y= "$ million",
       title="Seasonal plot: antidiabetic drug sales")


## AUSTRALIAN BEER PRODUCTION ------------------------------------------------------------


beer <- aus_production %>%
  select(Quarter, Beer) %>%
  filter(year(Quarter) >= 1992)
beer %>% autoplot(Beer)

beer %>% gg_season(Beer, labels="right")

beer %>% gg_subseries(Beer)

# Your turn
tourism %>% distinct(Region)

dat <- filter(tourism,
                  Region == "Snowy Mountains"|Region == "Gold Coast",
                  Purpose == "Holiday")

# Or in the same way
dat <- filter(tourism,
                  Region %in% c("Snowy Mountains","Gold Coast"),
                  Purpose == "Holiday")

dat %>% autoplot()

dat %>% gg_season(Trips,labels="right")
# Very different patterns between the two 

dat %>% gg_subseries(Trips,labels="right")

# Back to slides

# Australian state tourism
holidays <- tourism %>%
  filter(Purpose == "Holiday") %>%
  group_by(State) %>%
  summarise(Trips = sum(Trips))

# log scale
holidays %>% autoplot(Trips) +
  scale_y_log10()+
  ylab("thousands of trips") + xlab("Year") +
  ggtitle("Australian domestic holiday nights")

# let's filter the top three
tourism %>%
  filter(Purpose == "Holiday") %>%
  group_by(State) %>%
  filter(State %in% c("New South Wales", "Victoria", "Queensland")) %>%
  summarise(Trips = sum(Trips)) %>% autoplot(Trips)

# Show these but then go back to slides to comment 
holidays %>% gg_season(Trips) +
  ylab("thousands of trips") +
  ggtitle("Australian domestic holiday nights")

holidays %>%
  gg_subseries(Trips) + ylab("thousands of trips") +
  ggtitle("Australian domestic holiday nights")


# Time Series patterns
as_tsibble(fma::elec) %>%
  filter(index >= 1980) %>%
  autoplot(value) + xlab("Year") + ylab("GWh") +
  ggtitle("Australian electricity production")

as_tsibble(fma::hsales) %>%
  autoplot(value) +
  ggtitle("Sales of new one-family houses, USA") +
  xlab("Year") + ylab("Total sales")

as_tsibble(fma::ustreas) %>%
  autoplot(value) +
  ggtitle("US Treasury Bill Contracts") +
  xlab("Day") + ylab("price")

pelt %>%
  autoplot(Lynx) +
  ggtitle("Annual Canadian Lynx Trappings") +
  xlab("Year") + ylab("Number trapped")

# New ones 

us_employment %>%
  filter(Title == "Retail Trade", year(Month) >= 1980) %>%
  autoplot(Employed/1e3) +
  ggtitle("Retail employment, USA") + ylab("Million people")

# Trend, cycle, seasonality (seasonal pattern changing - we will study this next week)

# Google, Apple, Facebook, Amazon
gafa_stock %>%
  filter(Symbol == "AMZN", year(Date) >= 2014) %>%
  autoplot(Close) +
  ggtitle("Amazon closing stock price") +
  xlab("Day") + ylab("$")

# No seasonality in stock prices - especially if market is efficient
# Seems no trend or cycle because only looking at 1-year
# Change year(Date) >= 2014 to see what happens

# BAck to slides to explore Lynx

# Lag plots and autocorrelation

new_prod <- aus_production %>% 
  filter(year(Quarter) >= 1992)

new_prod %>%
  select(Quarter, Beer) %>%
  filter(year(Quarter) >= 1992)
new_prod %>% autoplot(Beer)

new_prod %>% gg_lag(Beer)
new_prod %>% gg_lag(Beer, geom='point')

new_prod %>% ACF(Beer, lag_max = 9)
new_prod %>% ACF(Beer, lag_max = 9) %>% autoplot()
new_prod %>% ACF(Beer) %>% autoplot()


## GOOGLE STOCK PRICE
google_2015 <- gafa_stock %>%
  filter(Symbol == "GOOG", year(Date) == 2015) %>%
  select(Date, Close)

google_2015 %>% autoplot(Close)

google_2015 %>%
  ACF(Close, lag_max =100) %>% autoplot()

google_2015 <- google_2015 %>%
  mutate(trading_day = row_number()) %>%
  update_tsibble(index=trading_day, regular=TRUE)

google_2015 %>%
  ACF(Close, lag_max=100) %>%
  autoplot()

## Google stock price longer

google_longer <- gafa_stock %>%
  filter(Symbol == "GOOG", year(Date) >= 2014) %>%
  select(Date, Close) %>%
  mutate(trading_day = row_number()) %>%
  update_tsibble(index=trading_day, regular=TRUE)

google_longer %>%
  autoplot(Close)

google_longer %>%
  ACF(Close, lag_max=100) %>%
  autoplot()

# Aus monthly electricity -------------

elec2 <- as_tsibble(fma::elec) %>%
  filter(year(index) >= 1980)
elec2 %>% autoplot(value)

elec2 %>% ACF(value, lag_max=48) %>%
  autoplot()

# Your turn --------

aus_production %>%
  autoplot(Bricks)+
  ggtitle("Australian clay brick production") +
  xlab("Year") + ylab("million units")

aus_production %>%
  gg_lag(Bricks, geom = 'point')

aus_production %>%
  ACF(Bricks, geom = 'point') %>% 
  autoplot()

# Lynx ACF ----------------------------
# Mention updating packages - Regularly 


pelt %>% autoplot(Lynx)

pelt %>% gg_lag(Lynx, geom = 'point')

pelt %>% 
  ACF(Lynx) %>% 
  autoplot()

# Back to slides -- you'll see plenty of examples in 
# the tutes. Some of you may have seen some already

## WHITE NOISE ------------------------------------------

seq_len(36)
rnorm(36)

set.seed(1)
wn <- tsibble(t = seq_len(3600), y = rnorm(3600), 
              index = t) #rnorm(n, mean = 0, sd = 1)
autoplot(wn)+ggtitle("White noise")
ACF(wn) %>% autoplot()

set.seed(6) 
wn <- tsibble(t = seq_len(36), y = rnorm(36), 
              index = t)
autoplot(wn)+ggtitle("White noise")
ACF(wn) %>% autoplot()

set.seed(1)
wn <- tsibble(t = seq_len(3600), y = rnorm(3600,mean=100,sd=100), 
              index = t) #rnorm(n, mean = 0, sd = 1)
autoplot(wn)+ggtitle("White noise")
ACF(wn) %>% autoplot()

set.seed(6) 
wn <- tsibble(t = seq_len(3600), y = rnorm(3600), 
              index = t)
autoplot(wn)+ggtitle("White noise")
ACF(wn) %>% autoplot()


## PIGS ----------------------------------------------

pigs <- aus_livestock %>%
  filter(State == "Victoria",
         Animal == "Pigs",
         year(Month) >= 2014)
pigs %>% autoplot(Count/1e3) +
  xlab("Year") + ylab("Thousands") +
  ggtitle("Number of pigs slaughtered in Victoria")

pigs %>% ACF(Count) %>% autoplot()

## DIFFERENCING GOOGLE CLOSING PRICE ------------------

dgoog <- gafa_stock %>%
  filter(Symbol == "GOOG", year(Date) >= 2018) %>%
  mutate(trading_day = row_number()) %>%
  update_tsibble(index=trading_day, regular=TRUE) %>%
  mutate(diff = difference(Close))

dgoog %>% autoplot(Close)
dgoog %>% ACF(Close) %>% autoplot()

dgoog %>% autoplot(diff)
dgoog %>% ACF(diff) %>% autoplot()


