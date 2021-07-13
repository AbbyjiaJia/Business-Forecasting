library(fpp3)

## ---- GDP ----------------------------------------------------------------

gdppc <- global_economy %>%
  mutate(GDP_per_capita = GDP / Population) %>%
  select(Year, Country, GDP, Population, GDP_per_capita)
gdppc

gdppc %>%
  filter(Country == "Sweden") %>%
  autoplot(GDP_per_capita) +
  labs(title = "GDP per capita for Sweden", y = "$US")

fit <- gdppc %>%
  model(trend_model = TSLM(GDP ~ trend()))

fc <- fit %>% forecast(h = "3 years")

fc %>%
  filter(Country == "Sweden") %>%
  autoplot(global_economy) +
  labs(title = "GDP per capita for Sweden", y = "$US")


## ---- Facebook -------------------------------------------------------


gafa_stock %>%
  filter(Symbol == "FB", Date >= ymd("2018-01-01")) %>%
  mutate(trading_day = row_number()) %>%
  update_tsibble(index = trading_day, regular = TRUE) %>% 
  autoplot(Close) +
  labs(title = "Facebook closing stock price in 2018",
       y = "Closing price ($USD)")

gafa_stock %>%
  filter(Symbol == "FB") %>%
  mutate(trading_day = row_number()) %>%
  update_tsibble(index = trading_day, regular = TRUE) %>% 
  autoplot(Close) +
  labs(title = "Facebook closing stock price in 2018",
       y = "Closing price ($USD)")



## ---- Bricks ------------------------------------------------------------

aus_production %>% 
  autoplot(Bricks)

aus_production %>% tail()

aus_production %>% 
  filter(!is.na(Bricks)) %>% 
  tail()

brick_fit <- aus_production %>%
  filter(!is.na(Bricks)) %>%
  model(
    Seasonal_naive = SNAIVE(Bricks),
    Naive = NAIVE(Bricks),
    Drift = RW(Bricks ~ drift()),
    Mean = MEAN(Bricks)
  )

brick_fc <- brick_fit %>%
  forecast(h = "5 years")

brick_fc %>%
  autoplot(aus_production, level = NULL) +
  labs(
    title = "Clay brick production for Australia",
    y = "Millions of bricks"
  ) +
  guides(colour = guide_legend(title = "Forecast"))

brick_fit %>% 
  select(Seasonal_naive) %>% 
  gg_tsresiduals()

brick_fc %>% autoplot(aus_production)

# Have a look at the distributional forecasts anyway - which leads to the next topic
brick_fc %>% 
  filter(.model=="Seasonal_naive") %>% 
  autoplot(aus_production) 
  
brick_fc %>% 
  filter(.model=="Seasonal_naive") %>% 
  autoplot()

brick_fc %>% 
  filter(.model=="Seasonal_naive") %>% 
  autoplot(level=c(58,80,95,99)) 

brick_fc %>% hilo()
brick_fc %>% hilo(level=95)


## ---- FACEBOOK -------------------------------------------------------------------

fb_stock <- gafa_stock %>%
  filter(Symbol == "FB") %>%
  mutate(trading_day = row_number()) %>%
  update_tsibble(index = trading_day, regular = TRUE)

fb_stock %>% autoplot(Close) +
  labs(
    title = "Facebook closing stock price",
    y = "$US"
  )

fit <- fb_stock %>% model(NAIVE(Close))

# Augment a mable oject to add stuff to it
augment(fit)

augment(fit) %>%
  ggplot(aes(x = trading_day)) +
  geom_line(aes(y = Close, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted"))

augment(fit) %>%
  filter(trading_day > 1100) %>%
  ggplot(aes(x = trading_day)) +
  geom_line(aes(y = Close, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted"))

augment(fit) %>%
  autoplot(.resid) + 
  labs(y = "$US",
       title = "Residuals from naïve method")

augment(fit) %>%
  ggplot(aes(x = .resid)) +
  geom_histogram(bins = 150) +
  ggtitle("Histogram of residuals")

augment(fit) %>%
  features(.resid, ljung_box, lag=10, dof=0)


# Specify, estimate and forecast
fb_stock %>%
  model(
    Mean = MEAN(Close),
    Naive = NAIVE(Close),
    Drift = RW(Close ~ drift())
  ) %>%
  forecast(h = 42) %>%
  autoplot(fb_stock, level = NULL) +
  labs(
    title = "Facebook closing stock price",
    y = "$US"
  ) +
  guides(colour = guide_legend(title = "Forecast"))

fit <- fb_stock %>% model(NAIVE(Close))

augment(fit) %>%
  filter(trading_day > 1100) %>%
  ggplot(aes(x = trading_day)) +
  geom_line(aes(y = Close, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted"))

augment(fit) %>%
  autoplot(.resid) +
  labs(
    y = "$US",
    title = "Residuals from naïve method"
  )

augment(fit) %>%
  ggplot(aes(x = .resid)) +
  geom_histogram(bins = 150) +
  labs(title = "Histogram of residuals")

augment(fit) %>%
  ACF(.resid) %>%
  autoplot() +
  labs(title = "ACF of residuals")

gg_tsresiduals(fit)

augment(fit) %>%
  features(.resid, ljung_box, lag = 10, dof = 0)


fc <- fb_stock %>%
  model(
    Mean = MEAN(Close),
    Naive = NAIVE(Close),
    Drift = RW(Close ~ drift())
    ) %>%
  forecast(h = 42)

fc %>% autoplot(fb_stock,level=NULL)

## BEER -------------------------------------------

recent <- aus_production %>% filter(year(Quarter) >= 1992)
fit <- recent %>% model(SNAIVE(Beer))
fit %>% forecast() %>% autoplot(recent)

gg_tsresiduals(fit)

Box.test(augment(fit)$.resid, lag=10, fitdf=0, type="Lj")

## FOOD RETAILING --------------------------------------

food <- aus_retail %>%
  filter(Industry == "Food retailing") %>%
  summarise(Turnover = sum(Turnover))

food %>% autoplot(Turnover)

# Considering no transformation

fit <- food %>%  model(SNAIVE(Turnover))
fit %>% gg_tsresiduals()

# Considering log transformation
# food %>% autoplot(log(Turnover))
fit <- food %>% model(SNAIVE(log(Turnover)))
fit %>% gg_tsresiduals()
# Still terrible because of trend but variance much better behaved

fc <- fit %>%
  forecast(h = "3 years") 

fc %>% autoplot(food)

fc %>% autoplot(filter(food, year(Month) > 2010))

# Example to understand difference between .resid and .innov
fit <- food %>% model(NAIVE(log(Turnover))) 
fit %>% augment() %>% select(.resid) %>% autoplot()
fit %>% augment() %>% select(.innov) %>% autoplot()
fit %>% gg_tsresiduals()


## EGG PRICES --------------------------------------

eggs <- prices %>%
  filter(!is.na(eggs)) %>%
  select(eggs)
eggs %>%
  autoplot(eggs) +
  labs(
    title = "Annual egg prices",
    y = "$US (adjusted for inflation)"
  )

fit <- eggs %>%
  model(rwdrift = RW(log(eggs) ~ drift()))
fit
fc <- fit %>%
  forecast(h = 50)
fc

fc %>% autoplot(eggs) +
  labs(
    title = "Annual egg prices",
    y = "US$ (adjusted for inflation)"
  )

fc %>%
  autoplot(eggs, level = 80, point_forecast = lst(mean, median)) +
  labs(
    title = "Annual egg prices",
    y = "US$ (adjusted for inflation)"
  )


## US RETAIL EMPLOYMENT ---------------------

us_retail_employment <- us_employment %>%
  filter(year(Month) >= 1990, Title == "Retail Trade") %>%
  select(-Series_ID)

us_retail_employment %>% autoplot(Employed)

us_retail_employment %>%
  model(STL(Employed)) %>%
  components() %>% 
  autoplot()

# Let's save this and remove model as I'm going to fit models to the 
# various parts of the decomposition 
dcmp <- us_retail_employment %>%
  model(STL(Employed)) %>%
  components() %>%
  select(-.model)

dcmp

dcmp %>%
  model(NAIVE(season_adjust)) %>%
  forecast() %>%
  autoplot(dcmp) +
  ggtitle("Naive forecasts of seasonally adjusted data")
# Looks pretty good

# Let's add back in the seasonality to get forecasts 
# on the original seasonal data
# fit a model to each part using decomposition_model

us_retail_employment %>%
  model(stlf = decomposition_model(
    STL(Employed ~ trend(window = 7), robust = TRUE), # decomposition
    NAIVE(season_adjust) # how to forecast seasonally adjusted 
  )) %>%
  forecast() %>%
  autoplot(us_retail_employment)
# Pretty good forecasts but will not cope with trend
# We will deal better with the season_adj series


## BEER PRODUCTION ---------------------

recent_production <- aus_production %>%
  filter(year(Quarter) >= 1992)

train <- recent_production %>%
  filter(year(Quarter) <= 2007)

train %>% tail()

test <- recent_production %>%
  filter(year(Quarter) >= 2008)

test

# More general and useful sometimes
test <- recent_production %>%
  slice((n() - 9):n())

recent_production %>%
  autoplot(Beer) + 
  geom_line(aes(y=Beer, colour="red"), data = test) + 
  geom_point()
# Add - to understand disjointed
# + geom_point()

beer_fit <- train %>%
  model(
    Mean = MEAN(Beer),
    Naive = NAIVE(Beer), # if you have weird characters use backticks - tidyverse
    Seasonal_naive = SNAIVE(Beer),
    Drift = RW(Beer ~ drift())
  )
beer_fc <- beer_fit %>%
  forecast(h = 10)

beer_fc %>% filter(.model=="Mean")%>% autoplot(recent_production)
beer_fc %>% autoplot()
beer_fc %>% filter(.model=="Mean")%>% autoplot()
beer_fc %>% autoplot(level= NULL)
beer_fc %>% autoplot(test, level= NULL)
beer_fc %>% autoplot(recent_production, level= NULL)
beer_fc %>% autoplot(recent_production, level= NULL)+
  guides(colour=guide_legend(title = "Forecasts"))

accuracy(beer_fit)
accuracy(beer_fc, test)

## CROSS-VALIDATION: FACEBOOK ----------------------------

# Setup tsibble 
fb_stock <- gafa_stock %>%
  group_by(Symbol) %>%
  mutate(trading_day = row_number()) %>%
  update_tsibble(index=trading_day, regular=TRUE) %>%
  ungroup() %>%
  filter(Symbol == "FB") %>% 
  select(Close)

fb_stock


# notice its size
fb_stock %>% stretch_tsibble(.init = 3, .step = 1) 
# .id tells me which window I am in

# Just showing slide
fb_stock %>% slide_tsibble(.size  = 3, .step = 1) 


# Get rid of the last window because I have nothing to evaluate after that 
fb_stock %>% stretch_tsibble(.init = 3, .step = 1) %>% tail()
fb_stock$trading_day %>% range()

fb_stock %>% stretch_tsibble(.init = 3, .step = 1) %>% filter(.id != max(.id))


fb_stretch <- fb_stock %>%
  stretch_tsibble(.init = 3, .step = 1) %>%
  filter(.id != max(.id))

fit_cv <- fb_stretch %>%
  model(RW(Close ~ drift()))

fit_cv # mable

# Now I want to generate the forecasts
fc_cv <- fit_cv %>%
  forecast(h=1, level=NULL)

fc_cv # fable
# For each .id I am forecasting one step ahead

fc_cv %>% accuracy(fb_stock)
# test-sample

## BEER PRODUCTION WITH CROSS-VALIDATION -------------

beer_cv <- aus_production %>%
  filter(year(Quarter) >= 1992) %>%
  select(Beer) %>%
  stretch_tsibble(.init = 4, .step = 1) %>%
  filter(.id <= max(.id)-4) # because I will do 4-steps ahead forecasting

beer_cv %>% tail()

aus_production %>%
  filter(year(Quarter) >= 1992) %>%
  select(Beer) %>% tail()

beer_fit_cv <- beer_cv %>%
  model(
    Mean = MEAN(Beer),
    `Naive` = NAIVE(Beer),
    `Seasonal naive` = SNAIVE(Beer),
    Drift = RW(Beer ~ drift())
  )

beer_fit_cv # 67 training sets - 4 models

beer_fc_cv <- beer_fit_cv %>%
  forecast(h = 4)

beer_fc_cv

# Let's plot to have a look 
beer_fc_cv %>%
  filter(.id %in% seq(1,10)) %>% 
  autoplot(beer_cv ,level = NULL) +
  ggtitle("Rolling forecasts for quarterly beer production") +
  xlab("Year") + ylab("Megalitres") +
  guides(colour=guide_legend(title="Forecast"))

# accuracy is smart enough to do right comparisons  
accuracy(beer_fc_cv, aus_production) %>% select(.model,MAE,RMSE,MAPE, MASE)

# Not much more code than before 
# fable is brilliant with this type of thing
# You'll do more in future assignments

# One train-test set
accuracy(beer_fc, recent_production) %>% select(.model,MAE,RMSE,MAPE, MASE)





