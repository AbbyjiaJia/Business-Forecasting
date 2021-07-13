library(fpp3)
library(lubridate)

options(
  digits = 3,
  width = 60,
  ggplot2.continuous.colour="viridis",
  ggplot2.continuous.fill = "viridis",
  ggplot2.discrete.colour = c("#D55E00", "#0072B2","#009E73", "#CC79A7", "#E69F00", "#56B4E9", "#F0E442"),
  ggplot2.discrete.fill = c("#D55E00", "#0072B2","#009E73", "#CC79A7", "#E69F00", "#56B4E9", "#F0E442")
)


## US change ------------------------------------------------------------------

# Percentage changes of these variables
us_change

us_change %>%
  pivot_longer(c(Consumption, Income), names_to="Series") %>%
  autoplot(value) +
  labs(y="% change")

# How about a scatterplot
us_change %>%
  ggplot(aes(x=Income, y=Consumption)) +
    labs(y = "Consumption (quarterly % change)",
         x = "Income (quarterly % change)") +
    geom_point() + geom_smooth(method="lm", se=FALSE)

fit_cons <- us_change %>%
  model(lm = TSLM(Consumption ~ Income)) # linear model for time series
# it has inbuilt functions it recognises

report(fit_cons)
# Back to slides


us_change %>%
  pivot_longer(-Quarter, names_to="Measure", values_to="Change") %>%
  ggplot(aes(x = Quarter, y = Change, colour = Measure)) +
  geom_line() +
  facet_grid(vars(Measure), scales = "free_y") +
  labs(y="") +
  guides(colour="none")

# GGally package - Di Cook ----------------
us_change %>%
  GGally::ggpairs(columns = 2:6) 


my_fn <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point() + 
    geom_smooth(method=lm, se=FALSE )
}

us_change %>%
  GGally::ggpairs(columns = 2:6,lower = list(continuous = my_fn)) 
  
# Multiple regression model ---------------
fit_consMR <- us_change %>%
  model(lm = TSLM(Consumption ~ Income + Production + Savings + Unemployment ))
report(fit_consMR)

augment(fit_consMR) %>%
  ggplot(aes(x = Quarter)) +
  geom_line(aes(y = Consumption, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  labs(y = NULL,
    title = "Percent change in US consumption expenditure"
  ) +
  scale_colour_manual(values=c(Data="black",Fitted="#D55E00")) +
  guides(colour = guide_legend(title = NULL))

augment(fit_consMR) %>%
  ggplot(aes(x=.fitted, y=Consumption)) +
    geom_point() +
    labs(y="Fitted (predicted values)",
         x="Data (actual values)",
         title ="Percentage change in US consumption expenditure") +
    geom_abline(intercept=0, slope=1)

fit_consMR %>% gg_tsresiduals()


# Australian beer production --------------

recent_production <- aus_production %>% filter(year(Quarter) >= 1992)
recent_production %>% 
  autoplot(Beer) +
  labs(y="Megalitres",title ="Australian quarterly beer production")

# Remember the downward trend in Q4 
recent_production %>% gg_subseries(Beer)

fit_beer <- recent_production %>%
  model(TSLM(Beer ~ trend() + season()))
report(fit_beer)
# Back to slides to comment

augment(fit_beer) %>%
  ggplot(aes(x = Quarter)) +
  geom_line(aes(y = Beer, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  labs(y="Megalitres",title ="Australian quarterly beer production") +
  scale_colour_manual(values = c(Data = "black", Fitted = "#D55E00"))

# Comment 
augment(fit_beer) %>%
  ggplot(aes(x=Beer, y=.fitted, colour=factor(quarter(Quarter)))) +
    geom_point() +
    labs(y="Fitted", x="Actual values",
         title = "Quarterly beer production") +
    scale_colour_brewer(palette="Dark2", name="Quarter") +
    geom_abline(intercept=0, slope=1)

fit_beer %>% gg_tsresiduals()

fit_beer %>%
  forecast() %>% 
  autoplot(recent_production)

fit_beer %>%
  forecast() %>% 
  autoplot(recent_production %>% filter(year(Quarter)>=2009))

# Beer with fourier terms -----------------
fourier_beer <- recent_production %>%
  model(TSLM(Beer ~ trend() + fourier(K=2)))
glance(fourier_beer)
report(fourier_beer)

recent_production %>% 
  model(
    f1 = TSLM(Beer ~ trend() + fourier(K=1)),
    f2 = TSLM(Beer ~ trend() + fourier(K=2)),
    season = TSLM(Beer ~ trend() + season())
  ) %>%
  glance()


## Aus cafe with Fourier terms --------

aus_cafe <- aus_retail %>% filter(
  Industry == "Cafes, restaurants and takeaway food services",
  year(Month) %in% 2004:2018
  ) %>% summarise(Turnover = sum(Turnover))

aus_cafe %>% 
  autoplot(Turnover)
aus_cafe %>% 
  autoplot(log(Turnover))

fit <- aus_cafe %>% 
  model(K1 = TSLM(log(Turnover) ~ trend() + fourier(K = 1)),
        K2 = TSLM(log(Turnover) ~ trend() + fourier(K = 2)),
        K3 = TSLM(log(Turnover) ~ trend() + fourier(K = 3)),
        K4 = TSLM(log(Turnover) ~ trend() + fourier(K = 4)),
        K5 = TSLM(log(Turnover) ~ trend() + fourier(K = 5)),
        K6 = TSLM(log(Turnover) ~ trend() + fourier(K = 6)))
glance(fit) %>% select(.model, r_squared, adj_r_squared, AIC, AICc, BIC)

augment(fit) %>%
  filter(.model %in% c("K1","K2","K3","K4","K5","K6")) %>%
  ggplot(aes(x=Month, y=Turnover)) +
  geom_line() +
  geom_line(aes(y=.fitted, col=.model)) +
  facet_wrap(vars(.model), ncol = 2) +
  guides(colour = FALSE, fill = FALSE, level = FALSE) +
  geom_label(
    aes(x = yearmonth("2007 Jan"), y = 4250,
        label = paste0("AICc = ", format(AICc))),
    data = glance(fit)
  ) +
  labs(title= "Total monthly eating-out expenditure",
       y="$ billions")


## Boston Marathon ------

# https://www.runnersworld.com/uk/training/motivation/a773110/qa-kathrine-switzer/
# K.N. Switzer
# Roberta (Bobbi) Gibb

boston_marathon %>% distinct(Event)
boston_marathon %>% filter(Event=="Women's pioneer era")

marathon <- boston_marathon %>%
  filter(Event == "Men's open division") %>%
  select(-Event) %>%
  mutate(Minutes = as.numeric(Time)/60)

marathon %>% tail()

marathon %>% 
  autoplot(Minutes) +
  labs(y="Winning times in minutes") 

fit_trends <- marathon %>%
  model(
    # Linear trend
    linear = TSLM(Minutes ~ trend()),
    # Exponential trend
    exponential = TSLM(log(Minutes) ~ trend()),
    # Piecewise linear trend
    piecewise = TSLM(Minutes ~ trend(knots = c(1950, 1980))),
    better = TSLM(Minutes ~ trend(knots = c(1919,1930,1950,1980)))
  )
# Quick because no nonlinear optim
# Regression has closed form solutions

fit_trends

glance(fit_trends) %>%
  select(.model, r_squared, adj_r_squared, AIC,AICc, BIC, CV)
# Notice exponential very different - computed on the log scale so cannot compare

fc_trends <- fit_trends %>% 
  forecast(h = 10)

marathon %>%
  autoplot(Minutes) +
  geom_line(data = fitted(fit_trends),
            aes(y = .fitted, colour = .model)) +
  autolayer(fc_trends, alpha = 0.5, level = 95) +
  labs(y = "Minutes",
       title = "Boston marathon winning times")

# Piecewise forecasts look reasonable
# PIs probably inflated by variance at the beginning of the sample
# Could probably truncate

# Check residuals 

fit_trends %>% select(linear) %>% 
  gg_tsresiduals()

fit_trends %>% select(better) %>%
  gg_tsresiduals()


# US consumption quarterly changes -------------

us_change

2^4

fit_all <- us_change %>%
  model(
    TSLM(Consumption ~ Income + Production + Unemployment + Savings),
    TSLM(Consumption ~ Production + Unemployment + Savings),
    TSLM(Consumption ~ Income + Unemployment + Savings),
    TSLM(Consumption ~ Income + Production + Savings),
    TSLM(Consumption ~ Income + Production + Unemployment),
    TSLM(Consumption ~ Income + Production),
    TSLM(Consumption ~ Income + Unemployment),
    TSLM(Consumption ~ Income + Savings),
    TSLM(Consumption ~ Production + Unemployment),
    TSLM(Consumption ~ Production + Savings),
    TSLM(Consumption ~ Unemployment + Savings),
    TSLM(Consumption ~ Income),
    TSLM(Consumption ~ Production),
    TSLM(Consumption ~ Unemployment),
    TSLM(Consumption ~ Savings),
    TSLM(Consumption ~ 1 ),
  )

fit_all %>% 
  glance() %>% 
  select(.model, adj_r_squared, AICc, BIC, CV) %>%
  arrange(CV)
#AICc
# Back to slides


fit_consBest <- us_change %>%
  model(
    TSLM(Consumption ~ Income + Production + Unemployment + Savings),
  )

fit_consBest %>% report()

future_scenarios <- scenarios(
  Increase = new_data(us_change, 4) %>%
    mutate(Income=1, Savings=0.5, Unemployment=0, Production=0),
  Decrease = new_data(us_change, 4) %>%
    mutate(Income=-1, Savings=-0.5, Unemployment=0, Production=0),
  names_to = "Scenario")

fc <- forecast(fit_consBest, new_data = future_scenarios)

us_change %>% autoplot(Consumption) +
  labs(y="% change in US consumption") +
  autolayer(fc) +
  labs(title = "US consumption", y = "% change")

