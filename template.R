############################################################
# Created       : November 17, 2023
# By            : Thomas de Graaff
# For           : Regional and Urban Economics
# Last edited   : November 17, 2023
############################################################

# If you need to install these package comment out command below
# install.packages("tidyverse")
# install.packages("stargazer")
# install.packages("sjPlot")

# Use packages
library(stargazer)
library(tidyverse)
library(sjPlot) # for plotting with ggplot2 from tidyverse package

# Read in Data
data <- read_csv(file = "monocentric_2016.csv")

# Labels of data are as follows
                      # pc4 = "Zipcode",
                      # pc4name = "Name of zipcode",
                      # mun = "municipality",
                      # munname = "municipality name",
                      # pricem2 = "price per m2",
                      # distcbd = "distance to cbd",
                      # shhistdistr = "share of historical distict",
                      # popdens = "population density",
                      # shforeign = "share of foreigners",
                      # shlandinfr = "share of land being infrastructure",
                      # shlandres = "share land for residential use",
                      # shlandmanuf = "share land for manufacturing",
                      # shlandother = "share land for other use",
                      # shopenspace = "share land being open space",
                      # shyoungp = "share of young people",
                      # sheldery = "share of elderly people",
                      # hhsize = "household size"
#

# Get descriptive histogram
hist(data$pricem2)

# Get descriptives
summary(data)

# make scatterplot between pricem2 and hhsize (base R)
plot(data$pricem2 ~ data$hhsize)

# First regression with prices per m2 regressed on household size
model_1 <- lm(pricem2 ~ hhsize, data = data)
plot_model(model_1, type = "pred", terms = c("hhsize"))

# Create new variable for Leeuwarden dummy
data$leeuwarden <- data$munname == "Leeuwarden"
data$leeuwarden <- as.factor(data$leeuwarden) # coerce into factor

# Regression with interaction terms
model_2 <- lm(pricem2 ~ hhsize + leeuwarden + hhsize:leeuwarden, data = data)
summary(model_2)

# Interaction plotting with ggplot2
plot_model(model_2, type = "pred", terms = c("hhsize", "leeuwarden")) 
# And combine with schatterplot
plot_model(model_2, type = "pred", terms = c("hhsize", "leeuwarden")) + 
    geom_point(data = data, aes(x = hhsize, y = pricem2), inherit.aes = FALSE)

# Interaction plotting with base R

plot(data$hhsize, data$pricem2,
     pch = 20,
     col = "steelblue",
     main = "Interaction between household size and Leeuwarden",
     xlab = "Household size",
     ylab = "Housing price per m2",
     cex.main=1.2)

coef_m2 <- model_2$coefficients 

abline(coef = c(coef_m2[1], coef_m2[2]), 
       col = "red",
       lwd = 2)

abline(coef = c(coef_m2[1] + coef_m2[3], coef_m2[2] + coef_m2[4]), 
       col = "purple",
       lwd = 2)
