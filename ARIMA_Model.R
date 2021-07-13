library(fpp3)

# Australia Gas production
aus_production %>% autoplot(Gas)
# Fails stationarity in all three ways


# GOOGLE STOCK PRICE 2018 ----------------
google_2018 <- gafa_stock %>%
  filter(Symbol == "GOOG", year(Date) == 2018) %>%
  mutate(trading_day = row_number()) %>%
  update_tsibble(index = trading_day, regular = TRUE)

google_2018 %>% autoplot(Close)
google_2018 %>% ACF(Close) %>% autoplot()
# Typical of non-stationary data

# Let's look at the differences from period to period
# Can do this on the fly
google_2018 %>% autoplot(difference(Close)) 

# ACF
google_2018 %>% ACF(difference(Close)) %>% autoplot()
# Not only stationary - it looks like WN


### WWW usage: 2nd differencing ----- 
# A time series of the numbers of users connected to the Internet 
# through a server every minute.

wwwusage <- as_tsibble(WWWusage)
wwwusage %>% autoplot(value)
wwwusage %>% autoplot(difference(value))
wwwusage %>% autoplot(difference(value, differences=2))

## A10 drugs: seas diff --------
#(antidiabetic) drugs sold in Australia

a10 <- PBS %>%
  filter(ATC2 == "A10") %>%
  summarise(Cost = sum(Cost)/1e6)

a10 %>% autoplot(Cost)

a10 %>% autoplot(log(Cost))

a10 %>% autoplot(
  log(Cost) %>% difference(12)
)

## H02 drugs: seas diff --------
#Australian corticosteroid drug sales

h02 <- PBS %>%
  filter(ATC2 == "H02") %>%
  summarise(Cost = sum(Cost)/1e6)

h02 %>% autoplot(Cost)

h02 %>% autoplot(log(Cost))

h02 %>% autoplot(
  log(Cost) %>% difference(12)
)

h02 %>% autoplot(
  log(Cost) %>% difference(12) %>% difference(1)
)


# GOOGLE STOCK PRICE 2018: KPSS ----------------

google_2018 %>%
  autoplot(Close)

google_2018 %>%
  features(Close, unitroot_kpss)

# Let's look at the differences

google_2018 %>%
  autoplot(difference(Close))

google_2018 %>%
  features(difference(Close), unitroot_kpss)

# Returns directly the number of differences
# at a 5%
google_2018 %>%
  features(Close, unitroot_ndiffs)

### WWW usage: KPSS ----------------
wwwusage <- as_tsibble(WWWusage)
wwwusage %>% autoplot(value)
wwwusage %>% autoplot(difference(value))
wwwusage %>% autoplot(difference(value, differences=2))

wwwusage %>% features(value, unitroot_kpss)
wwwusage %>% features(difference(value), unitroot_pp)

wwwusage %>% features(difference(value), unitroot_ndiffs)


## A10 drugs

a10 <- PBS %>%
  filter(ATC2 == "A10") %>%
  summarise(Cost = sum(Cost)/1e6)

a10 %>% autoplot(Cost)

a10 %>% autoplot(log(Cost))

a10 %>% autoplot(
  log(Cost) %>% difference(12)
)

## H02 drugs: Seas Diff ----------------

h02 <- PBS %>%
  filter(ATC2 == "H02") %>%
  summarise(Cost = sum(Cost)/1e6)

h02 %>% autoplot(Cost)

h02 %>% autoplot(log(Cost))

h02 %>% autoplot(
  log(Cost) %>% difference(12)
)

h02 %>% autoplot(
  log(Cost) %>% difference(12) %>% difference(1)
)

# Seasonal strength - compare seasonal component to remainder 
h02 %>% model(STL(log(Cost))) %>% components() %>% autoplot()


# Just showing passing a list into features()
# and also where the seasonal strength comes from
# i.e., an STL decomposition
h02 %>% mutate(log_sales = log(Cost)) %>%
  features(log_sales, list(unitroot_nsdiffs, feat_stl))

h02 %>% mutate(log_sales = log(Cost)) %>%
  features(log_sales, feat_stl)

# Seasonal diffs
h02 %>% mutate(log_sales = log(Cost)) %>%
  features(log_sales, unitroot_nsdiffs)
# First order diffs
h02 %>% mutate(d_log_sales = difference(log(Cost), 12)) %>%
  features(d_log_sales, unitroot_ndiffs)


## Australian tourism --------------------------

# Total trips across all purposes and all regions
total_trips <- tourism %>%
  summarise(Trips = sum(Trips))

total_trips %>% autoplot(Trips)

# Looks clearly seasonal but let's ask anyway
total_trips %>% features(Trips, unitroot_nsdiffs)

total_trips %>% features(difference(Trips,4), unitroot_ndiffs)

total_trips %>%
  autoplot(Trips %>% difference(4))
# Do we now need a first order difference?

total_trips %>%
  ACF(Trips %>% difference(4)) %>%
  autoplot()

total_trips %>% features(difference(Trips,4), unitroot_ndiffs)
# yes we need a first order difference

total_trips %>%
  autoplot(Trips %>% difference(4) %>% difference(1))

total_trips %>%
  ACF(Trips %>% difference(4) %>% difference(1)) %>%
  autoplot()


# Back to slides to introduce backshift notation

## AR Simulations ----

# You will be doing this by writing a function
# Hopefully some of you have done it already
# Here we use arima.sim()

set.seed(1)
n=1000
ar1 <- tsibble(idx = seq_len(n), 
               sim = arima.sim(list(ar = -0.8), n = n), 
               index = idx)

ar1 %>% autoplot(sim)

set.seed(1)
ar1 <- tsibble(idx = seq_len(n), 
               sim = arima.sim(list(ar = 0.95), n = n), 
               index = idx)
ar1 %>% autoplot(sim)

# How do we estimate
ar1 %>%
  model(ARIMA(sim~pdq(1,0,0))) %>%
  report()

# Back to slides

n=100
# AR model with constant
set.seed(1)
ar1 <- tsibble(idx = seq_len(n), 
               sim = 10 + arima.sim(list(ar = -0.8), n = n), 
               index = idx)
ar1 %>% autoplot(sim)


# How do we estimate
ar1 %>%
  model(ARIMA(sim~1+pdq(1,0,0))) %>%
  report()

set.seed(1)
n=100 
ar2 <- tsibble(idx = seq_len(n), 
               sim = 20 + arima.sim(list(ar = c(1.3, -0.7)), n = n), 
               index = idx)

ar2 %>%
  model(ARIMA(sim~1+pdq(2,0,0))) %>%
  report()

ar2 %>%
  model(ARIMA(sim~pdq(2,0,0))) %>%
  report()

ar2 %>% autoplot(sim) + ylab("") + ggtitle("AR(2)")


p1 <- tsibble(idx = seq_len(100), sim = 10 + arima.sim(list(ar = -0.8), n = 100), index = idx) %>%
  autoplot(sim) + ylab("") + ggtitle("AR(1)")

p2 <- tsibble(idx = seq_len(100), sim = 20 + arima.sim(list(ar = c(1.3, -0.7)), n = 100), index = idx) %>%
  autoplot(sim) + ylab("") + ggtitle("AR(2)")

library(patchwork)
p1|p2

# MA simulations ------

#Similarly for MA processes

p1 <- tsibble(idx = seq_len(100), sim = 20 + arima.sim(list(ma = 0.8), n = 100), index = idx) %>%
  autoplot(sim) + ylab("") + ggtitle("MA(1)")
p2 <- tsibble(idx = seq_len(100), sim = arima.sim(list(ma = c(-1, +0.8)), n = 100), index = idx) %>%
  autoplot(sim) + ylab("") + ggtitle("MA(2)")

p1|p2

#Demonstrating ACFs for AR(1) and MA(1) -------
set.seed(10)
ar1 <- tsibble(idx = seq_len(1000), sim = 10 + arima.sim(list(ar = 0.8), n = 1000), index = idx) 
ar1 %>% gg_tsdisplay(sim, plot_type='partial') 

set.seed(1)
ma1 <- tsibble(idx = seq_len(1000), sim = 20 + arima.sim(list(ma = 0.8), n = 1000), index = idx)
ma1 %>% gg_tsdisplay(sim, plot_type='partial') 
0.8/(1+0.8^2)

## Understanding ARIMA models ----

# You can experiment with these lines to
# generate data and see what the forecasts
# converge to. Useful for slide entitled
#'Understanding ARIMA models'

set.seed(104)
n=200
n_sim=n

# Use the following pairings
d=0; c=0;  # converge to zero
d=0; c=10; # converge to mean(y)

d=1; c=0; n_sim=n-1; # converge to constant (RW + ARMA)

d=2; c=0; n_sim=n-2; # converge to trend (2 unit roots + ARMA)

order=c(1,d,1)
ar=c(0.6)
ma=c(0.8)
h=30

y <- tsibble(idx = seq_len(n), sim = c + arima.sim(list(order = order, ar=ar, ma=ma), n = n_sim) , index = idx)
y %>% autoplot(sim)
y %>% model(ARIMA(sim~1+pdq(order))) %>% 
  forecast(h=h) %>% autoplot(tail(y,200))

# Simulating from a random walk with drift
set.seed(104)
d=1; c=.1; # converge to trend (RW with Drift + ARMA)
y<-tsibble(idx = seq_len(n), sim = cumsum(c(0,rnorm(n-1)+c)), index = idx)
y %>% autoplot(sim)
y %>% model(ARIMA(sim~1+pdq(order))) %>% 
  forecast(h=h) %>% autoplot(tail(y,200))


## EGYPTIAN EXPORTS ------

egypt <-global_economy %>%
  filter(Code == "EGY")
  
egypt %>% autoplot(Exports) +
  labs(y = "% of GDP", title = "Egyptian Exports")

# Data is stationary - tested
egypt %>%
  features(Exports,unitroot_ndiffs)

# But data is not WN
egypt %>% ACF(Exports) %>% autoplot()

fit <- global_economy %>% 
  filter(Code == "EGY") %>%
  model(ARIMA(Exports))
report(fit)
gg_tsresiduals(fit)

augment(fit) %>%
  features(.innov, ljung_box, lag = 10, dof = 4)

fit %>% 
  forecast(h=60) %>%
  autoplot(global_economy) +
  labs(y = "% of GDP", title = "Egyptian Exports")

# Picking an ARIMA model
egypt %>% ACF(Exports) %>% autoplot()
egypt %>% PACF(Exports) %>% autoplot()

egypt %>%
  gg_tsdisplay(Exports, plot_type = "partial")

fit1 <- global_economy %>%
  filter(Code == "EGY") %>%
  model(ARIMA(Exports ~ pdq(4,0,0)))
report(fit1)

# Let's have a look at the residuals
fit1 %>% gg_tsresiduals()

fit2 <- global_economy %>%
  filter(Code == "EGY") %>%
  model(ARIMA(Exports))
report(fit2)

# Let's have a look at these residuals
fit2 %>% gg_tsresiduals()

# How? Notice the AICc values


## CAF EXPORTS -----

global_economy %>%
  filter(Code == "CAF") %>%
  autoplot(Exports) +
  labs(title="Central African Republic exports",
       y="% of GDP")
# Is this stationary?

# Remember signs of non-stationarity
global_economy %>%
  filter(Code == "CAF") %>%
  gg_tsdisplay(Exports, plot_type='partial')

# Let's difference
global_economy %>%
  filter(Code == "CAF") %>%
  gg_tsdisplay(difference(Exports), plot_type='partial')

# Very messy ACF and PACF - very usual - forget about theory
caf_fit <- global_economy %>%
  filter(Code == "CAF") %>%
  model(arima210 = ARIMA(Exports ~ pdq(2,1,0)),
        arima013 = ARIMA(Exports ~ pdq(0,1,3)),
        stepwise = ARIMA(Exports),
        fullsearch = ARIMA(Exports, 
                           stepwise=FALSE, 
                           trace = TRUE))
?ARIMA

caf_fit

glance(caf_fit) %>% arrange(AICc) %>% select(.model:BIC)

caf_fit %>%
  select(fullsearch) %>%
  gg_tsresiduals()

caf_fit %>%
  select(fullsearch) %>%
  augment() %>%
  features(.innov, ljung_box, lag = 10, dof = 3)

caf_fit %>%
  forecast(h=10) %>%
#  filter(.model=='fullsearch') %>%
  autoplot(global_economy)

caf_fit %>% tidy()
# Back to slides

## US Consumption ------------------------------------------------------------------

us_change 
# Quarterly changes in these variables

us_change %>% autoplot(Consumption) +
  labs(y="Quarterly percentage change", title="US consumption")
# Is it stationary - already differenced data 

us_change %>% gg_tsdisplay(Consumption, plot_type = 'partial')
# Indicates possible AR(3)
# But let's fit an AR(2) model
fit <- us_change %>%
  model(arima = ARIMA(Consumption ~ pdq(2,0,0) + PDQ(0,0,0)))

augment(fit) %>% gg_tsdisplay(.innov, plot_type = 'partial')

# Let's correct
fit <- us_change %>%
  model(arima = ARIMA(Consumption ~ pdq(3,0,0) + PDQ(0,0,0)))
fit %>% report()
augment(fit) %>% gg_tsdisplay(.innov, plot_type = 'partial')

# Alternatively
us_change %>% gg_tsdisplay(Consumption, plot_type = 'partial')
fit <- us_change %>%
  model(arima = ARIMA(Consumption ~ pdq(0,0,3) + PDQ(0,0,0)))
fit %>% report()
augment(fit) %>% gg_tsdisplay(.innov, plot_type = 'partial')

# I can let ARIMA choose
fit <- us_change %>%
  model(arima = ARIMA(Consumption ~ PDQ(0,0,0)))
fit %>% report()
# It chooses based on AICc

# I can let ARIMA choose - only some bits
fit <- us_change %>%
  model(arima = ARIMA(Consumption  ~ pdq(d=0) + PDQ(0,0,0)))
fit %>% report()
# It chooses the rest based on AICc

# I can restrict the constant
fit <- us_change %>%
  model(arima = ARIMA(Consumption  ~ 0 + pdq(d=0) + PDQ(0,0,0)))
fit %>% report()
# It chooses the rest based on AICc

# The rest is the same - just use the forecast function
fit <- us_change %>%
  model(arima = ARIMA(Consumption ~ PDQ(0,0,0), stepwise = FALSE, 
                      approximation = FALSE))

fit %>% report()
fit %>% tidy()
fit %>% gg_tsresiduals()

fit %>% forecast(h=10) %>%
  autoplot(us_change)

fit %>% forecast(h=10) %>%
  autoplot(tail(us_change, 80))

# Returns to the mean of the historical data
fit %>% forecast(h=50) %>%
  autoplot(us_change)

## MINKS ------------

mink <- as_tsibble(fma::mink)
mink %>% autoplot(value) +
  labs(y="Minks trapped (thousands)",
       title = "Annual number of minks trapped")

mink %>% gg_tsdisplay(value, plot_type = "partial")
# Remember we are looking at the last significant one
# Back to slides

# Let's choose a model
mink %>% model(ARIMA(value~pdq(4,0,0))) %>% report()

mink %>% model(ARIMA(value)) %>% report()
# Not the best model - look at AICc of my manual one
# Also I need cycles - hence p>=2
mink %>% model(ARIMA(value)) %>% forecast(h=20) %>% autoplot(mink)

# Let's make it work harder
mink %>% model(ARIMA(value, stepwise=FALSE)) %>% report()
# Will look at every possibility up to p+q<(max_order)

fit <- mink %>%
  model(
    ar4 = ARIMA(value ~ pdq(4,0,0)),
    auto = ARIMA(value),
    best = ARIMA(value, stepwise=FALSE, approximation=FALSE)
  )

fit
glance(fit)
fit %>% select(best) %>% report()
fit %>% select(best) %>% gg_tsresiduals()
fit %>% select(best) %>% forecast(h=20) %>% autoplot(mink)

?ARIMA
# Scroll down to specials
# Can look at more models
mink %>% 
  model(ARIMA(value, stepwise=FALSE, order_constraint = p+q+P+Q<=6)) %>% 
  report()

# Can look harder by not making any approximations 

mink %>% 
  model(ARIMA(value, stepwise=FALSE, order_constraint = p+q+P+Q<=12, 
              approximation = FALSE, trace=TRUE)) %>% 
  report()

mink %>% 
  model(ARIMA(value~pdq(p=0:6,q=0:6), stepwise=FALSE, 
              order_constraint = p+q+P+Q<=12, 
              approximation = FALSE, trace=TRUE)) %>% 
  report()


# Commonly switching off stepwise will find a reasonable model

mink %>% 
  model(ARIMA(value, stepwise=FALSE)) %>% 
  forecast(h=100) %>% 
  autoplot(mink)

# Switch to slides 

## WWW usage -----------------------------------------------------------------------

web_usage <- as_tsibble(WWWusage)
web_usage %>% gg_tsdisplay(value, plot_type = 'partial')

web_usage %>% gg_tsdisplay(difference(value), plot_type = 'partial')

fit <- web_usage %>%
  model(arima = ARIMA(value ~ pdq(3, 1, 0))) %>%
  report()

web_usage %>%
  model(auto = ARIMA(value ~ pdq(d=1))) %>%
  report()

web_usage %>%
  model(auto2 = ARIMA(value ~ pdq(d=1),
       stepwise = FALSE, approximation = FALSE)) %>%
  report()

gg_tsresiduals(fit)

augment(fit) %>%
  features(.innov, ljung_box, lag = 10, dof = 3)

fit %>% forecast(h = 10) %>% autoplot(web_usage)


## Electrical Equipment --------------------------------------------------------

elecequip <- as_tsibble(fpp2::elecequip)
dcmp <- elecequip %>%
  model(STL(value ~ season(window = "periodic"))) %>%
  components() %>%
  select(-.model)
dcmp %>% as_tsibble %>%
  autoplot(season_adjust) + xlab("Year") +
  ylab("Seasonally adjusted new orders index")

dcmp %>% gg_tsdisplay(difference(season_adjust), plot_type = 'partial')

fit <- dcmp %>%
  model(
    arima310 = ARIMA(season_adjust ~ pdq(3,1,0) + PDQ(0,0,0)),
    arima410 = ARIMA(season_adjust ~ pdq(4,1,0) + PDQ(0,0,0)),
    arima014 = ARIMA(season_adjust ~ pdq(0,1,4) + PDQ(0,0,0)),
    arima311 = ARIMA(season_adjust ~ pdq(3,1,1) + PDQ(0,0,0))
  )

glance(fit)

fit <- dcmp %>%
  model(
    arima = ARIMA(season_adjust ~ PDQ(0,0,0))
    )
fit %>% report()

fit <- dcmp %>%
  model(
    arima = ARIMA(season_adjust ~ PDQ(0,0,0), stepwise = FALSE)
  )
fit %>% report()
# Be warned

fit <- dcmp %>%
  model(
    arima = ARIMA(season_adjust ~ PDQ(0,0,0), stepwise = FALSE, 
                  approximation = FALSE)
  )
fit %>% report()


gg_tsresiduals(fit)

augment(fit) %>%
  features(.innov, ljung_box, lag = 24, dof = 4)

fit %>% forecast %>% autoplot(dcmp)

## GDP --------------------------------------------------------------------------

global_economy

# Pick a couple of countries - see logs
global_economy %>%
  filter(Country=="Australia") %>%
  autoplot(log(GDP))

global_economy %>%
  filter(Country=="United States") %>%
  autoplot(log(GDP))

# Find a suitable ARIMA model for each one of the 263 countries
fit <- global_economy %>%
  model(
    ARIMA(log(GDP))
  )
# Has not worked for some countries because of no data being present
fit
fit %>% tail()

fit %>%
  filter(Country == "Australia") %>%
  report()
fit %>%
  filter(Country == "Australia") %>%
  gg_tsresiduals()
# Some tiny correlations

fit %>%
  filter(Country == "Australia") %>%
  augment() %>%
  features(.innov, ljung_box, dof=2, lag=10)
# Push it out to 15 and you get some significant dynamics

fit %>%
  filter(Country == "Australia") %>%
  forecast(h=10) %>%
  autoplot(global_economy) 
# fairly wide prediction intervals
+ scale_y_log10()


### US Monthly Electricity -----------------------------------------------------

usmelec <- as_tsibble(fpp2::usmelec) %>%
  rename(Month = index, Generation = value)

usmelec %>% autoplot(
  Generation
)

usmelec %>% autoplot(
  log(Generation)
)

usmelec %>% autoplot(
  log(Generation) %>% difference(12)
)

usmelec %>% gg_tsdisplay(
  log(Generation) %>% difference(12), plot_type = "partial"
  )
# You could possible work with this

usmelec %>% autoplot(
  log(Generation) %>% difference(12) %>% difference()
)

usmelec %>% gg_tsdisplay(
  log(Generation) %>% difference(12) %>% difference(),
  plot_type = "partial")

usmelec %>%
  model(arima = ARIMA(log(Generation) ~ pdq(0,1,3) + PDQ(0,1,1))) %>%
  report()

usmelec %>%
  model(arima = ARIMA(log(Generation))) %>%
  report()

fit <- usmelec %>%
  model(arima = ARIMA(log(Generation)))
gg_tsresiduals(fit)

augment(fit) %>%
  features(.innov, ljung_box, lag = 24, dof = 5)
# residuals are not great

# Don't run takes too long - see model below
usmelec %>%
  model(arima = ARIMA(log(Generation), 
                      stepwise = FALSE, 
                      approximation = FALSE)
        ) %>%
  report()

fit <- usmelec %>%
  model(arima = ARIMA(log(Generation)~pdq(1,1,1)+PDQ(2,1,2)))
gg_tsresiduals(fit)

usmelec %>%
  model(arima = ARIMA(log(Generation))) %>%
  forecast(h = "3 years") %>%
  autoplot(usmelec)

usmelec %>%
  model(arima = ARIMA(log(Generation))) %>%
  forecast(h = "3 years") %>%
  autoplot(filter(usmelec, year(Month) >= 2005))


## US Leisure and Hospitality -------------

leisure <- us_employment %>%
  filter(Title == "Leisure and Hospitality", year(Month) > 2000) %>%
  mutate(Employed = Employed/1000) %>% select(Month, Employed)
autoplot(leisure, Employed) +
  labs(title = "US employment: leisure and hospitality",
       y="Number of people (millions)")

leisure %>%
  gg_tsdisplay(difference(Employed, 12),
               plot_type='partial', lag=36) +
  labs(title="Seasonally differenced", y="")
# Clearly non-stationary

leisure %>%
  gg_tsdisplay(difference(Employed, 12) %>% difference(),
               plot_type='partial', lag=36) +
  labs(title = "Double differenced", y="")

# Lets think about models
# Start with the seasonal component


fit_leisure <- leisure %>%
  model(
    arima012011 = ARIMA(Employed ~ pdq(0,1,2) + PDQ(0,1,1)),
    arima210011 = ARIMA(Employed ~ pdq(2,1,0) + PDQ(0,1,1)),
    auto = ARIMA(Employed, stepwise = FALSE, approx = FALSE)
  )
fit_leisure %>% pivot_longer(everything(), names_to = "Model name",
                     values_to = "Orders")

glance(fit_leisure) %>% arrange(AICc) %>% select(.model:BIC)
fit_leisure %>% select(auto) %>%  tidy()
fit_leisure %>% select(auto) %>% gg_tsresiduals()
fit_leisure %>% select(auto) %>% augment() %>% 
  features(.innov, ljung_box, lag=24, dof=4)

fit_leisure %>% select(auto) %>% report()

forecast(fit_leisure, h=36) %>%
  filter(.model=='auto') %>%
  autoplot(leisure) +
  labs(title = "US employment: leisure and hospitality",
       y="Number of people (millions)")

# Question: where does the trend come from?

## h02 drugs ----------------------------------------------------------------------

h02 <- PBS %>%
  filter(ATC2 == "H02") %>%
  summarise(Cost = sum(Cost))

h02 %>% autoplot(Cost)

## Models using logs

h02 %>% autoplot(log(Cost))
# You may choose not to take logs
h02 %>% features(Cost, features = guerrero) %>% pull(lambda_guerrero)

# Clearly seasonal
h02 %>% gg_tsdisplay(difference(log(Cost),12), lag_max = 36, plot_type='partial')
# Debatable whether I should take another difference.
# Stick with d=0, D=1 for the moment

# My best guess
# Easier to look at the PACF for both seas and non-seas
fit <- h02 %>%
  model(best = ARIMA(log(Cost) ~ pdq(3,0,0) + PDQ(2,1,0)))
report(fit)
gg_tsresiduals(fit, lag_max=36)
# not too bad

augment(fit) %>%
  features(.innov, ljung_box, lag = 36, dof = 6)
# Change lag to 24 - 36 is very long 
# Spikes are small and far away
# So overall happy with this

# Go to slide 137 to show Plausible alternative model 

# Best of the alternative plausible models
fit <- h02 %>%
  model(best = ARIMA(log(Cost) ~ 0 + pdq(3,0,1) + PDQ(0,1,2)))
report(fit)
gg_tsresiduals(fit, lag_max=36)
# better I think 
augment(fit) %>%
  features(.innov, ljung_box, lag = 24, dof = 6)
# Still failing at lag 36
# How about lag 24

# Letting R choose
fit <- h02 %>% model(auto = ARIMA(log(Cost), stepwise = FALSE))
# Better than my model with stepwise=TRUE
# Turn off and see what happens
report(fit)
gg_tsresiduals(fit, lag_max=36)
# Probably a bit worse with only 6 parameters again
augment(fit) %>%
  features(.innov, ljung_box, lag = 36, dof = 6)
# Still failing at lag 36
# How about lag 24

# Getting R to work really hard now 
# DO NOT RUN
fit_h02 <- h02 %>%
  model(best = ARIMA(log(Cost), stepwise = FALSE,
                 approximation = FALSE,
                 order_constraint = p + q + P + Q <= 9))
# This will take a while now
# You should be experimenting with these in IA4 and GA4
# Start early because these will take a while
# Remember you may not be able to deal with all the autocorrelations
# Evaluate your significant spikes and where they are

# Remember every model is a crude approximation of reality
# No data comes from a model unless you are simulating data

fit_h02 %>% report()
gg_tsresiduals(fit_h02, lag_max=36)
augment(fit_h02) %>%
  features(.innov, ljung_box, lag = 36, dof = 9)
# Success :-)

# However we have a problem? Go back to slide 148

# The forecasts
fit_h02 %>% forecast %>% autoplot(h02) +
  labs(y="H02 Expenditure ($AUD)")


## Example: Australian population -------------
# Non-seasonal
aus_economy <- global_economy %>% filter(Code == "AUS") %>%
  mutate(Population = Population/1e6)

aus_economy %>% autoplot(Population)

aus_economy %>%
  slice(-n()) %>% # take out the last row
  stretch_tsibble(.init = 10) %>% # stretch out starting with 10 obs
  # 48 folds - 48 .ids
  model(ets = ETS(Population),
        arima = ARIMA(Population)
  ) %>%
  forecast(h = 1) %>%
  accuracy(aus_economy) %>%
  select(.model, ME:RMSSE)

# Forecasts
aus_economy %>%
  model(ETS(Population)) %>%
  forecast(h = "5 years") %>%
  autoplot(aus_economy) +
  labs(title = "Australian population",
       y = "People (millions)")

# Let's zoom in a little
aus_economy %>%
  model(ETS(Population)) %>%
  forecast(h = "5 years") %>%
  autoplot(aus_economy %>% filter(Year > 1990)) +
  labs(title = "Australian population",
       y = "People (millions)")


## Example: Cement production ------------
# Seasonal data

cement <- aus_production %>%
  select(Cement) %>%
  filter_index("1988 Q1" ~ .)

cement %>% autoplot(Cement) 
# Seasonality changing 

# We are doing a train-test split 
# Best to do a cv stretch_tsibble
# You will do that in your assignment 
# but it will take longer - so be prepared - start early

train <- cement %>% filter_index(. ~ "2007 Q4")
fit <- train %>%
  model(
    arima = ARIMA(Cement),
    ets = ETS(Cement)
  )

fit %>%
  select(arima) %>%
  report()
gg_tsresiduals(fit %>% select(arima), lag_max = 16)

fit %>%
  select(arima) %>%
  augment() %>%
  features(.innov, ljung_box, lag = 16, dof = 6)


fit %>%
  select(ets) %>%
  report()
gg_tsresiduals(fit %>% select(ets), lag_max = 16)

fit %>%
  select(ets) %>%
  augment() %>%
  features(.innov, ljung_box, lag = 16, dof = 6)

# Both models seem to be very suitable

fit %>%
  forecast(h = "2 years 6 months") %>%
  accuracy(cement) %>%
  select(-ME, -MPE, -ACF1)
# ARIMA does slightly better 
# Remember only 10 obs 
# So do not get too excited about it
# But I still go with the ARIMA


fit %>%
  select(arima) %>%
  forecast(h="3 years") %>%
  autoplot(cement) +
  labs(title = "Cement production in Australia",
       y="Tonnes ('000)")

fit %>%
  select(ets) %>%
  forecast(h="3 years") %>%
  autoplot(cement) +
  labs(title = "Cement production in Australia",
       y="Tonnes ('000)")

# Possibly ARIMA looks a bit too optimistic
# ETS wider intervals are possibly better

# If we are interested in intervals
fit %>%
  forecast(h = "2 years 6 months") %>%
  accuracy(cement, measures=interval_accuracy_measures, level=95)

fit %>%
  forecast(h = "2 years 6 months") %>%
  accuracy(cement, measures=interval_accuracy_measures, level=80)

# If we are interested in the whole distributions
fit %>%
  forecast(h = "2 years 6 months") %>%
  accuracy(cement, measures=distribution_accuracy_measures)

