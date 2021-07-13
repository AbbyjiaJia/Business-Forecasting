library(fpp3)

# Algerian Exports -------------

algeria_economy <- global_economy %>%
  filter(Country == "Algeria")
algeria_economy %>% autoplot(Exports)

fit <- algeria_economy %>%
  model(
    ANN = ETS(Exports ~ error("A") + trend("N") + season("N")),
    MNN = ETS(Exports ~ error("M") + trend("N") + season("N")),
    autoNN = ETS(Exports ~ trend("N") + season("N"))
  )

fit %>% report()
fit %>% select(ANN) %>% report()
fit %>% select(MNN) %>% report()

fit %>% tidy()
fit %>% glance()

fit %>%
  select(autoNN) %>%
  report(fit)

# Drop the automated
fit <- fit %>%
  select(Country, ANN,MNN)

fit %>%
  components() %>% 
  autoplot()

fit %>% 
  components() %>%
  left_join(fitted(fit), by = c("Country", ".model", "Year"))

fit %>%
  forecast(h = 5) %>%
  autoplot(algeria_economy) +
  ylab("Exports (% of GDP)") + xlab("Year")


# Australian population ------------

# Filter Australia and put on a sensible scale
aus_economy <- global_economy %>%
  filter(Code == "AUS") %>%
  mutate(Pop = Population/1e6)

aus_economy
aus_economy %>% tail()

aus_economy %>% autoplot(Pop)

fit <- aus_economy %>%
  model(AAN = ETS(Pop ~ error("A") + trend("A") + season("N")))
report(fit)

# beta = 0.3266366 
# Anything above 0.2 is quite high 
# So this changes relatively quickly
# This captures the change in the slope towards the end

components(fit) %>% autoplot()
# Level tracks data - usually a smoothed version of data
# Always positive but changes rapidly towards the end
# Like a derivative of the data

components(fit) %>%
  left_join(fitted(fit), by = c("Country", ".model", "Year"))

# Point out initial level and slope
# All subsequent levels and slopes are computed 
# by the equations

# Recall fitted(t) = level(t-1) + slope(t-1)

# Let's have a look at the forecasts
# Note that it tracks the slope well at the end
fit %>%
  forecast(h = 10) %>%
  autoplot(aus_economy) +
  ylab("Population") + xlab("Year")

# Experimenting with the two special cases alpha=beta=0 and alpha=1 beta=0 
fit <- aus_economy %>%
  model(a_0_b_0 = ETS(Pop ~ error("A") + trend("A", alpha=0.0001, beta=0.0001) + season("N")),
        a_1_b_0 = ETS(Pop ~ error("A") + trend("A", alpha=0.999, beta=0.0001) + season("N")),
        a_free_b_free = ETS(Pop ~ error("A") + trend("A") + season("N"))
  )

fit %>% components() %>% autoplot() 
fit %>% tidy()


# Let's do the multiplicative version
aus_economy %>%
  model(
    AAN = ETS(Pop ~ error("A") + trend("A") + season("N")),
    MAN = ETS(Pop ~ error("M") + trend("A") + season("N"))
    ) %>%
  forecast(h=10) %>% 
  autoplot(aus_economy, alpha=0.6,) 
# Prediction intervals are different

# Damped
aus_economy %>% autoplot()

fit <- aus_economy %>%
  model(damped = ETS(Pop ~ error("A") + trend("Ad") + season("N"))) 
report(fit)
# Maybe comment on the 0.98

aus_economy %>%
  model(holt = ETS(Pop ~ error("A") + trend("Ad") + season("N"))) %>%
  forecast(h = 300) %>% #Inrease h=100, 300
  autoplot(aus_economy)

fit <- aus_economy %>%
  filter(Year <= 2010) %>% # Notice the filter here
  model(
    ses = ETS(Pop ~ error("A") + trend("N") + season("N")),
    holt = ETS(Pop ~ error("A") + trend("A") + season("N")),
    damped = ETS(Pop ~ error("A") + trend("Ad") + season("N"))
  )

fit

tidy(fit) # will return parameters
glance(fit)

accuracy(fit)
# models have different number of parameters hence not comparable

fit %>% forecast(h=7) %>% accuracy(aus_economy)
# But only 7 obervations


# Eggs ----------

prices %>% filter(!is.na(eggs)) %>% autoplot(eggs)
# Eggs in real terms 

# doing it on log scale so that forecasts do not go -ve
# only controlling/changing trend
# letting R figure out the rest 

fit <- prices %>% filter(!is.na(eggs)) %>%
  model(
    ses = ETS(log(eggs) ~ trend("N")),
    holt = ETS(log(eggs) ~ trend("A")),
    damped = ETS(log(eggs) ~ trend("Ad"))
  )

# Notice/amazing how quickly models are estimated.
# You should appreciate after this week's tutes what optimisation does

fit 

fit %>%
  forecast(h=100) %>%
  autoplot(prices %>% filter(!is.na(eggs)), level=NULL)

# Notice curved/exponential on the original scale
# But would be linear on the log scale
# There is also the bias adjustment

# Looking at forecasts Holt's seems best
fit %>% glance()

fit %>% tidy()

# Let's look closer at the "best" model

fit %>%
  select(holt) %>%
  report()

# beta very small to slope is not changing on the log-scale
# but becomes curved in the original scale
fit %>% 
  select(holt) %>% 
  components() %>% 
  autoplot()

fit %>%
  select(holt) %>%
  gg_tsresiduals()
# They look pretty good 

# Helpful for number of parameters
fit %>% 
  select(holt) %>% 
  tidy()

fit %>% select(holt) %>%gg_tsresiduals()
# Filter by rows
fit %>%
  augment() %>%
  filter(.model=="holt") %>%
  features(.innov, ljung_box, dof=4, lag=10)

# Select by columns 
fit %>%
  select(holt) %>% 
  augment() %>%
  features(.innov, ljung_box, dof=4, lag=10)

# We can select models automatically 
prices %>% filter(!is.na(eggs)) %>% model(ETS(log(eggs)))
prices %>% filter(!is.na(eggs)) %>% model(ETS(eggs))


# J07 ------

j07 <- PBS %>%
  filter(ATC2 == "J07") %>%
  summarise(Cost = sum(Cost))

j07 %>% autoplot(Cost)

j07 %>%
  model(ETS(Cost ~ error("A") + trend("N") + season("A"))) %>%
  forecast(h=36) %>%
  autoplot(j07, level=NULL)

j07 %>%
  model(ETS(Cost ~ error("M") + trend("N") + season("M"))) %>%
  forecast(h=36) %>%
  autoplot(j07, level=NULL)

## Aus holidays ---------

# Tourism by region and purpose

aus_holidays <- tourism %>%
  filter(Purpose == "Holiday") %>%
  summarise(Trips = sum(Trips))

aus_holidays %>% autoplot(Trips)
# Data: seasonal, upward trend towards the end 

# Let's estimate 2 models 
# 
fit <- aus_holidays %>%
  model(
    additive = ETS(Trips ~ error("A") + trend("A") + season("A")),
    multiplicative = ETS(Trips ~ error("M") + trend("A") + season("M"))
  )

fit
fit %>% tidy()

fc <- fit %>% forecast()

fc %>%
  autoplot(aus_holidays, level = NULL) + xlab("Year") +
  ylab("Overnight trips (thousands)")
# Fairly similar point forecasts

#fc %>%
#  autoplot(aus_holidays) + xlab("Year") +
#  ylab("Overnight trips (thousands)")
# Prediction intervals not so similar 
# multi are wider 

components(fit) %>% autoplot()
# Of course the additive error and seasonality are different

# Let's pull out only multi version
fit %>% 
  select(multiplicative) %>% 
  components() %>% 
  autoplot()
# Not a huge difference in this case
# multi is a more flexible

# Back to slides - and see side by side....

# Gas production ----

aus_production 

aus_production %>% autoplot(Gas)
# Crazy seasonal variation
# Clearly multiplicative seasonality

fit <- aus_production %>%
  model(
    hw = ETS(Gas ~ error("M") + trend("A") + season("M")),
    hwdamped = ETS(Gas ~ error("M") + trend("Ad") + season("M")),
  )

fit
fit %>% tidy()
fit %>% glance() # Give you more summary stats - will get back to this
# What is the best method?
# Based on the fit not much difference between them
# Slightly lower MAE
# Let's go with no damping 

fit %>%
  select(hw) %>%
  gg_tsresiduals()
# Residuals not exactly homoscedastic 
# - see the difference between the beginning and end
# Some autocorrelations remaining 
# Fairly normal
# Not the perfect model but prepared to live with it

# Let's look at the LB test

fit %>% 
  select(hw) %>% 
  augment() %>% 
  features(.innov, ljung_box, dof=8, lag=24)
  
fit %>%
  forecast(h=36) %>%
  autoplot(aus_production, level=NULL)

# Maybe cut sample short - but be warned in general not a 
# good idea to throw data away

# Let's try something else
# This model comes with a warning - will show soon why
fit <- aus_production %>%
  model(
    hw2 = ETS(Gas ~ error("A") + trend("A") + season("M"))
    )
# Works better than that others in this case

fit %>% gg_tsresiduals()
# Residual clearly heteroscedastic

fit %>%
  forecast(h=36) %>%
  autoplot(aus_production)

## National populations ----

global_economy 
# Pop in millions so make a more appropriate scale
# Notice key Country 263 
# Let's select and estimate a model for each country

fit <- global_economy %>%
  mutate(Pop = Population / 1e6) %>%
  model(ets = ETS(Pop))
# So we are fitting every possible model to every country 
# and selecting the one with the lowest AICc
# Of course there is no seasonal component here

fit

fit %>% forecast(h = 5)
# This is amazingly fast right? 


## Example: Australian holiday tourism ----

tourism # 304 series split by region, state, purpose 

# Let's just do the holiday ones - 76 series

holidays <- tourism %>%
  filter(Purpose == "Holiday")


fit <- holidays %>% model(ets = ETS(Trips))
# It now thinks also about seasonal models - so it works a little harder

fit

fit %>%
  filter(Region == "Snowy Mountains") %>%
  report()
# Most popular snow fields in Australia 

fit %>%
  filter(Region == "Snowy Mountains") %>%
  components() %>%
  autoplot()

# Let's have a look at the dable - decomposition table
fit %>%
  filter(Region == "Snowy Mountains") %>%
  components() 


# Forecast all the series at the same time and pull out 
# the Snowy Mountains
fit %>%
  forecast() %>%
  filter(Region == "Snowy Mountains") %>%
  autoplot(holidays) +  #, show_gap=FALSE
  xlab("Year") + ylab("Overnight trips (thousands)")

# Pull out Snowy Mountains first and then forecast
# So this will be quicker for us
fit %>%
  filter(Region == "Snowy Mountains") %>%
  forecast() %>%
  autoplot(holidays) +
  xlab("Year") + ylab("Overnight trips (thousands)")



# Tourism example - Sum over regions -------
# So only one model

aus_holidays <- tourism %>%
  filter(Purpose == "Holiday") %>%
  summarise(Trips = sum(Trips))

aus_holidays %>% autoplot()

fit <- aus_holidays %>% model(ETS(Trips))
report(fit)
# Small gamma

components(fit) %>% autoplot() 

residuals(fit)  # innovations resids attached to the model 
# for additive they are one and the same
residuals(fit, type = "response")
fit %>% augment()
fit %>% gg_tsresiduals()

# If model was 
fit1 <- aus_holidays %>% model(ETS(Trips~error("A")))
report(fit1)

fit1 %>% augment()
fit1 %>% gg_tsresiduals()

fit %>% forecast(h='4 years') %>% autoplot(aus_holidays)
fit1 %>% forecast(h='4 years') %>% autoplot(aus_holidays)

fit2 <- aus_holidays %>% model(ETS(Trips~trend("A")))
fit2 %>% report()
fit2 %>% gg_tsresiduals()
fit2 %>% forecast(h='4 years') %>% autoplot(aus_holidays)

fit %>% glance()
fit1 %>% glance()
fit2 %>% glance()

# Sometimes you may need to be subjective and overide choices


## H02 ----
#Corticosteroid

PBS

h02 <- PBS %>%
  filter(ATC2 == "H02") %>%
  summarise(Cost = sum(Cost))
# We end up with a single series

h02 %>%
  autoplot(Cost)
# Possibly a multiplicative season
# No need for transformation for these - multiplicative components deal with this

h02 %>% model(ETS(Cost)) %>% report()
# Fits all 15 models - 6 additive and 9 multiplicative - show slide 86
h02 %>% model(ETS(Cost)) %>% components() %>% autoplot()
h02 %>% model(ETS(Cost)) %>% forecast() %>% autoplot(h02)

# Let's compare some models
# Recall if you leave component out it is chosen by AICc

fit <- h02 %>% model(
  ETS_auto = ETS(Cost),
  ETS_AAA = ETS(Cost ~ error("A") + trend("A") + season("A")),
  ETS_damped = ETS(Cost ~ error("A") + trend("Ad")),
  ETS_forbidden = ETS(Cost ~ error("A") + trend("Ad") + season("M"))
  )

fit
fit %>% accuracy()
fit %>% glance()
fit %>% tidy() 
fit %>% tidy() %>% pivot_wider(names_from=.model,values_from=estimate) 

fit %>% 
  select(ETS_auto, ETS_AAA) %>% 
  forecast(h="5 years") %>% 
  autoplot(h02)

# Summary
# Really useful class of models
# Fully automated - probably most commonly used around the world
# Can deal with trend, seasons, heteroscedasticity
# But they do have limitations
# Cannot capture and project cycles

# Pelt
pelt %>% autoplot(Lynx)

pelt %>% 
  model(ETS(Lynx))

pelt %>% 
  model(ETS(Lynx)) %>% 
  forecast(h=20) %>% 
  autoplot(pelt)



        