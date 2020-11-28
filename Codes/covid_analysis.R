#######################
## Analysis of       ##
##  Covid-19 cases   ##
##    and            ##
##  Death due to     ## 
##  Covid-19 on a    ## 
##  given day        ##
##                   ##
##                   ##
##      NO. 3        ##
##                   ##
##  Analysis of      ##
##    the data       ##
##                   ##
#######################



# Clear memory
rm(list=ls())

# Packages to use
library(tidyverse)
# For scaling ggplots
require(scales)
# Estimate piecewise linear splines
#install.packages("lspline")
library(lspline)
# Estimate robust SE
#install.packages("estimatr")
library(estimatr)
# Compare models with robust SE
#install.packages("texreg")
library(texreg)
# For different themes
#install.packages(ggthemes)
library(ggthemes)
library(dplyr)
# Call the data from github
my_data <- "/Users/steve_j/Documents/CEU /data_analysis/DA2assignment1/data /covid_data_clean.csv"
df <- read_csv( my_data )



####
# 
# Quick check on all HISTOGRAMS
df %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~key, scales = "free") +
  geom_histogram()

summary( df )


######
# Plotting basic scatterplots to make a decision about the variable transformation 
#
# Basic regression: 
# Number of death per capita = alpha + beta * number of cases per capita 
#     
#
# Checking distributions by applying different variable transformations
#
# 1) Registered number of deaths - Registered number of cases: level-level model without scaling
ggplot( df , aes(x = CasePerCap, y = DeathsPerCap)) +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "Registered covid cases/capita",y = "Registered covid deaths/capita")

# 2) Registered number of deaths/capita - ln(Registered number of cases/capita): log-transformation applied for registered cases/capita
ggplot( df , aes(x = CasePerCap, y = DeathsPerCap)) +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "Cases/capita (for 1M population, ln scale )",y = "Registered Covid Deaths/capita (for 1M population)") +
  scale_x_continuous( trans = log_trans(),  breaks = c(1,2,5,10,20,50,100,200,500,1000,10000) )

# 3) ln(Registered number of deaths/capita) - Registered number of cases/capita: log-transformation applied for registered deaths/capita
ggplot( df , aes(x = CasePerCap, y = DeathsPerCap))  +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "Cases/capita (for 1M population, ln scale )",y = "Deaths/capita (for 1M population, ln scale )") +
  scale_y_continuous( trans = log_trans(),  breaks = c(1,2,5,10,20,50,100,200,500,1000,10000) )


# 4) ln(Registered number of deaths/capita) - ln(Registered number of cases/capita): log-transformation applied for registered deaths/capita and registered cases/capita
ggplot( df , aes(x = CasePerCap, y = DeathsPerCap))  +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "Cases/capita (for 1M population, ln scale )", y = "Deaths/capita (for 1M population, ln scale )") +
  scale_x_continuous( trans = log_trans(),  breaks = c(1,2,5,10,20,50,100,200,500,1000,10000) )+
  scale_y_continuous( trans = log_trans(),  breaks = c(1,2,5,10,20,50,100,200,500,1000,10000)  )


####
# Conclusions:
#   1) Using level-level approach does not yield a linear pattern.
#   2) If the Cases/capita variable is transformed (level-log), yields a more linear-like pattern. However there is still non-linearities in data. Would need to use 'approximation' when interpreting the possible associations
#   3) The log transformation of Deaths/capita does not make sense on its own. It yields a non-linear relationship that is hard to interpret. 
#   4) Performing log transformation on both variables makes the most sense. It provides a pattern, that is close to linear and enables a meaningful interpretation. 
#   
#
#    Log-log transformation would be of best use due to the following:     
#
#     - Substantive: it allows for a  meaningful interpretation 
#     - Statistical: better approximation makes distribution is close to linear, thus it's easier to fit a proper model
#     



# Take Log of Case/capita and log Deaths/capita
df <- df %>% mutate( ln_CPC = log( CasePerCap ),
                     ln_DPC = log( DeathsPerCap ) )
# dropping negative ln observations to enable easier interpretation 

library(dplyr)
df <- filter(df, df$ln_CPC > 0, df$ln_DPC > 0)


######
# Exploring potential models:
# 
#     reg1: ln_DPC = alpha + beta * ln_CPC
#     reg2: ln_DPC = alpha + beta_1 * ln_CPC + beta_2 * ln_CPC^2
#     reg4: ln_DPC = alpha + beta_1 * ln_CPC * 1(ln_CPC < 50) + beta_2 * ln_CPC * 1(ln_CPC >= 50)
#     reg5: ln_DPC = alpha + beta * ln_CPC, weights: population (weighted-ols)

###
# Two ways to handle polynomials: 
#
#  Adding powers of the explanatory variable to the data frame:
df <- df %>% mutate( ln_CPC_sq = ln_CPC^2,
                     ln_CPC_cb = ln_CPC^3 )

# Regressions 
# Using lm_robust to tackle issues associated heteroscedasticity 

# First model:
reg1 <- lm_robust( ln_DPC ~ ln_CPC , data = df , se_type = "HC2" )
summary(reg1) 
# Summary statistics
summary( reg1 )
# Visual inspection:
ggplot( data = df, aes( x = ln_CPC, y = ln_DPC ) ) + 
  geom_point( color='blue') +
  geom_smooth( method = lm , color = 'red' )

# Second model with a polynomial 

# Quadratic 
reg2 <- lm_robust( ln_DPC ~ ln_CPC + ln_CPC_sq , data = df )
summary(reg2)
ggplot( data = df, aes( x = ln_CPC, y = ln_DPC ) ) + 
  geom_point( color='blue') +
  geom_smooth( formula = y ~ poly(x,2) , method = lm , color = 'red' )
 
# Cubic
reg3 <- lm_robust( ln_DPC ~ ln_CPC + ln_CPC_sq + ln_CPC_cb , data = df)
summary(reg3)
ggplot( data = df, aes( x = ln_CPC, y = ln_DPC ) ) + 
  geom_point( color='blue') +
  geom_smooth( formula = y ~ poly(x,3) , method = lm , color = 'red' )

# Third regression with piece wise linear spline:
# defining the cutoff for cases/capita 
cutoff <- 50 

# log transforming the cutoff
cutoff_ln<- log( cutoff )

# Use simple regression with the lspline function
?lspline
reg4 <- lm_robust(ln_DPC ~ lspline( ln_CPC , cutoff_ln ) , data = df )
summary( reg4 )
ggplot( data = df, aes( x = ln_CPC, y = ln_DPC ) ) + 
  geom_point( color='blue') +
  geom_smooth( formula = y ~ lspline(x,cutoff_ln, cutoff1) , method = lm , color = 'red' )


# Weighted-OLS: use reg4 setup and weight with population
reg5 <- lm_robust(ln_DPC ~ ln_CPC, data = df , weights = Population)
summary( reg5 )

ggplot(data = df, aes(x = ln_CPC, y = ln_DPC )) +
  geom_point(data = df, aes(size=df$Population),  color = 'blue', shape = 16, alpha = 0.6,  show.legend=F) +
  geom_smooth(aes(weight = df$Population), method = "lm", color='red')+
  scale_size(range = c(1, 15)) +
  labs(x = "ln(Cases/capita) ",y = "ln(Death/capita)")
  


#####
# Creating model summary with texreg
data_out <- "/Users/steve_j/Documents/CEU /data_analysis/DA2assignment1/"
htmlreg( list(reg1 , reg2 , reg3 , reg4),
         type = 'html',
         custom.model.names = c("CasesPerCap - linear","CasesPerCap - quadratic","CasesPerCap - PLS",
                                "GDP/capita - weighted linear"),
         caption = "Modelling life expectancy and different wealth measures of countries",
         file = paste0( data_out ,'model_comparison.html'), include.ci = FALSE)


######
# Based on model comparison our chosen model is reg1 - ln_CPC ~ ln_DPC
#   Substantive: - level-log interpretation works properly for countries
#                - magnitude of coefficients are meaningful
#   Statistical: - simple model, easy to interpret
#                - Comparatively high R2 and captures variation well


######
# Residual analysis.


# Get the predicted y values from the model
df$reg1_y_pred <- reg1$fitted.values
# Calculate the errors of the model
df$reg1_res <- df$ln_DPC - df$reg1_y_pred 

# Find countries with largest negative errors
df %>% top_n( -5 , reg1_res ) %>% 
  select( Country , ln_DPC , reg1_y_pred , reg1_res )

# Find countries with largest positive errors
df %>% top_n( 5 , reg1_res ) %>% 
  select( Country , ln_DPC , reg1_y_pred , reg1_res )


#################################
## Testing hypothesis
# The test hypothesis is the following $H_0: \beta = 0, \, H_A: \beta \neq 0$ or not in our model. 
# The estimated t-statistics is `r round(reg1$statistic[2],2)`, with p-value: `r reg1$p.value[2]`. 
# Choosing a significance level of p = 0.05. 
summary( reg1 )
# Thus we reject the $H_0$, which means the number of daily recorded deaths per capita due to Covid 19 is not uncorrelated with daily number to recorded cases of Covid 19 per capita.



