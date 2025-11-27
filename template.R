############################################################
# Created       : November 17, 2023
# By            : Thomas de Graaff
# For           : Regional and Urban Economics
# Last edited   : November 17, 2023
############################################################

# If you need to install these package comment out command below
install.packages("tidyverse")
install.packages("stargazer")
install.packages("sjPlot")

# Use packages
library(stargazer)
library(tidyverse)
library(sjPlot) # for plotting with ggplot2 from tidyverse package

# Read in Data
data <- read_csv(file = "Datasets/monocentric_2016.csv")

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

# Create new variable for Eindhoven
data$Eindhoven <- data$munname == "Eindhoven"
data$Eindhoven <- as.factor(data$Eindhoven) # coerce into factor

# Regression with interaction terms
model_2 <- lm(pricem2 ~ hhsize + Eindhoven + hhsize:Eindhoven, data = data)
summary(model_2)

# Interaction plotting with ggplot2
plot_model(model_2, type = "pred", terms = c("hhsize", "Eindhoven")) 
# And combine with schatterplot
plot_model(model_2, type = "pred", terms = c("hhsize", "Eindhoven")) + 
    geom_point(data = data, aes(x = hhsize, y = pricem2), inherit.aes = FALSE)

# Interaction plotting with base R

plot(data$hhsize, data$pricem2,
     pch = 20,
     col = "steelblue",
     main = "Interaction between household size and Eindhoven",
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













target_city <- "Eindhoven"

mun_col  <- "munname"
land_col <- "pricem2"
dist_col <- "distcbd"
pop_col  <- "popdens" 

needed <- c(mun_col, land_col, dist_col)
miss <- needed[!needed %in% names(data)]
if (length(miss) > 0) stop("Missing columns: ", paste(miss, collapse = ", "))

# small cleaning and flags
data <- data[!is.na(data[[land_col]]) & !is.na(data[[dist_col]]), ]
data$is_eindhoven <- ifelse(data[[mun_col]] == target_city, 1, 0)
cat("Rows in", target_city, ":", sum(data$is_eindhoven == 1), "\n")

# Winsor cap function
cap01 <- function(x) {
  q <- quantile(x, probs = c(0.01, 0.99), na.rm = TRUE)
  pmin(pmax(x, q[1]), q[2])
}

data$land_c <- cap01(data[[land_col]])
data$dist_c <- cap01(data[[dist_col]])

# Create logs (small epsilon to avoid log(0))
eps <- 1e-6
data$log_land <- log(data$land_c + eps)
data$log_dist <- log(data$dist_c + eps)

# Log population density if available and positive
has_pop <- pop_col %in% names(data)
if (has_pop) {
  data$log_popdens <- ifelse(data[[pop_col]] > 0, log(data[[pop_col]] + eps), NA)
}


# Variables to include (edit to add other control names)
vars <- c("land_c","log_land","dist_c","log_dist","log_popdens") 
vars <- intersect(vars, names(data))              # keep only present ones

# Helper to make one-row summary for a numeric vector
summ_one <- function(x, name) {
  xnum <- as.numeric(x)
  m <- mean(xnum, na.rm = TRUE)
  s <- sd(xnum, na.rm = TRUE)
  mn <- min(xnum, na.rm = TRUE)
  p25 <- quantile(xnum, 0.25, na.rm = TRUE)
  med <- median(xnum, na.rm = TRUE)
  p75 <- quantile(xnum, 0.75, na.rm = TRUE)
  mx <- max(xnum, na.rm = TRUE)
  cv <- ifelse(!is.na(m) & m != 0, s / m, NA)
  # geometric mean: for logged vars we can back-transform exp(mean(log_var))
  geom_mean <- NA
  backtrans <- NA
  if (grepl("^log_", name)) {
    # mean of log variable -> back-transform gives a multiplicative "typical" value
    geom_mean <- exp(m)
    backtrans <- geom_mean
  } else if (all(xnum > 0, na.rm = TRUE)) {
    # positive-level variable -> geometric mean via exp(mean(log(x)))
    geom_mean <- exp(mean(log(xnum), na.rm = TRUE))
  }
  data.frame(
    variable = name,
    mean = m,
    sd = s,
    cv = cv,
    min = mn,
    p25 = p25,
    median = med,
    p75 = p75,
    max = mx,
    geom_mean = geom_mean,
    backtrans_from_log = backtrans,
    stringsAsFactors = FALSE
  )
}

# Build table
rows <- lapply(vars, function(v) summ_one(data[[v]], v))
tab <- do.call(rbind, rows)

# Round numbers to 3 decimals for printing
numcols <- setdiff(names(tab), "variable")
tab[numcols] <- round(tab[numcols], 3)

# Add a short plain-language interpretation column (ELI5)
interpret <- sapply(seq_len(nrow(tab)), function(i) {
  r <- tab[i, ]
  if (startsWith(as.character(r$variable), "log_")) {
    paste0("Typical (original units) ≈ ", r$backtrans_from_log,
           "; median log = ", r$median)
  } else {
    paste0("Median = ", r$median, "; geom. mean = ", r$geom_mean)
  }
})
tab$short_note <- interpret

# Print nicely and save
print(tab, row.names = FALSE)



model_loglevel <- lm(log_land ~ dist_c * is_eindhoven, data = data)
summary(model_loglevel)

model_loglevelpop <- lm(log_land ~ dist_c * is_eindhoven + log_popdens, data = data)
summary(model_loglevelpop)
                     
