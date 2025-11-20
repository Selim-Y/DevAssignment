# First we need to make sure that some packages (=places where e.g. commands are 
#saved) are available, for this, we need to install them
#for this, we create a variable called check, (see in your Enviroment 
#what value it takes after having run the line), it takes TRUE if the package is 
#already installed and FALSE if not
check <- require('survey')
#next, we have a loop that has a condition (!check, this condition is TRUE if check is FALSE and FALSE if check is TRUE)
#i.e. if it is already installed, the loop will not do anything, if it is not instaled, it will install it
#Depending on your version, you might need to enter YES in the console to confirm that you want to install it
if(!check) {
  install.packages('survey')
}
#just because it is installed does not mean that it is loaded into your current session (the one you run this file in), therefore we execute the following code
library(survey)  
#to learn about the package
?survey

#now to the next package that will allow us to load the files we have easily into R
check <- require('readstata13')
#if not installed, install the package, this needs to be confirmed by typing in yes in the command console, after having run the following line
if(!check) {
  install.packages('readstata13')
}
library('readstata13')  
#to learn about the package
?readstata13

#some housekeeping
rm(check)

# now finally: Read in data

#for this: we create the filepath either per hand, see the line below, 
#or we use the point-and-shoot-option (often easier if you donot feel comfortable with R or coding yet)

# statafile <- 'u:/interim files/Tanzania 2012.dta' #if you want to use this, delete the # at the beginning and type in the correct path
statafile <- file.choose() #allows for clicking on it in a window, no need to have the path ready
#load it
tzdata2018 <- read.dta13(statafile,nonint.factors = T)

# Remove observations with missing data 
tzdata2018 <- na.omit(tzdata)
#create population-weight (popwt) as a variable
tzdata2018$popwt <- with(tzdata,hhsize*hhweight)

#incorporate the surveydesign: for this, use the information about strata, weights and ID from the data
tzdesign2018 <- svydesign(id = ~CLUSTER, strata = ~STRATUM, weights = ~popwt, data = tzdata)
#

#This is the end for the first round of the survey (either 2012 and 2018), 
#now you will have to repeat the code for the other round, however, no need to load (or install) the packages again, good luck!




library(dplyr)
library(haven)
library(data.table)


dt1 <- as.data.table(read_dta("Datasets/Tanzania_2012.dta"))
dt2 <- as.data.table(read_dta("Datasets/Tanzania_2018.dta"))
setnames(dt1, tolower(names(dt1))); setnames(dt2, tolower(names(dt2)))
dt <- rbindlist(list(dt1, dt2), fill = TRUE)
df <- as.data.frame(dt, stringsAsFactors = FALSE)


cons <- "cons"
year <- "year"
std_var <- "povline"
food_var <- "food_povline"


# build person weight
hh_wt <- "hhweight"
hh_sz <- "hhsize"
if (!is.na(hh_wt) && !is.na(hh_sz)) df$person_wt <- df[[hh_wt]] * df[[hh_sz]] else if (!is.na(hh_wt)) df$person_wt <- df[[hh_wt]] else if ("person_wt" %in% names(df)) df$person_wt <- df$person_wt else df$person_wt <- 1

# build area_group
area_var <- "area"
region_var <- "region"
df$area_group <- NA_character_
if (!is.na(region_var)) df[grepl("dar", tolower(as.character(df[[region_var]]))), "area_group"] <- "Dar es Salaam"
if (!is.na(area_var)) {
  isUrban <- grepl("urban|town|city", tolower(as.character(df[[area_var]])))
  isRural <- grepl("rural|village", tolower(as.character(df[[area_var]])))
  df$area_group[is.na(df$area_group) & isUrban] <- "Other urban"
  df$area_group[is.na(df$area_group) & isRural] <- "Rural"
}
df$area_group[is.na(df$area_group)] <- "Rural"

measures <- function(cons_v, wt_v, z) {
  if (is.na(z)) return(c(h = NA_real_, g = NA_real_, sg = NA_real_))
  poor <- cons_v < z
  S <- sum(wt_v, na.rm = TRUE)
  if (S <= 0) return(c(h = NA_real_, g = NA_real_, sg = NA_real_))
  h <- sum(wt_v * poor, na.rm = TRUE) / S
  gap <- pmax(0, (z - cons_v) / z)
  g <- sum(wt_v * gap, na.rm = TRUE) / S
  sg <- sum(wt_v * gap^2, na.rm = TRUE) / S
  c(h = h, g = g, sg = sg)
}

compute_by_group <- function(df, z, group_col = NULL, povlabel = "standard", level_name = NULL) {
  if (is.null(group_col)) {
    m <- measures(df$cons, df$person_wt, z)
    return(data.frame(level = "National", group = "All", povline = povlabel,
                      headcount = m["h"], poverty_gap = m["g"], sq_poverty_gap = m["sg"],
                      stringsAsFactors = FALSE))
  }
  vals <- unique(df[[group_col]])
  out <- lapply(vals, function(g) {
    sub <- df[df[[group_col]] == g, , drop = FALSE]
    m <- measures(sub$cons, sub$person_wt, z)
    data.frame(level = ifelse(is.null(level_name), group_col, level_name),
               group = as.character(g), povline = povlabel,
               headcount = m["h"], poverty_gap = m["g"], sq_poverty_gap = m["sg"],
               stringsAsFactors = FALSE)
  })
  do.call(rbind, out)
}


cons <- "cons"    
year <- "year"
std_var <- "povline"
food_var <- "food_povline"
region_var <- if ("region" %in% names(df)) "region" else NA
area_col <- "area_group"

yrs <- sort(unique(df[[year]]))
results <- list()
for (y in yrs) {
  sub <- df[df[[year]] == y, , drop = FALSE]
  z_std  <- if (length(unique(na.omit(sub[[std_var]])))>0) unique(na.omit(sub[[std_var]]))[1] else NA_real_
  z_food <- if (length(unique(na.omit(sub[[food_var]])))>0) unique(na.omit(sub[[food_var]]))[1] else NA_real_
  results[[length(results)+1]] <- compute_by_group(sub, z_std, NULL, "standard")
  results[[length(results)+1]] <- compute_by_group(sub, z_std, area_col, "standard", "Area")
  if (!is.na(region_var)) results[[length(results)+1]] <- compute_by_group(sub, z_std, region_var, "standard", "Region")
  results[[length(results)+1]] <- compute_by_group(sub, z_food, NULL, "food")
  results[[length(results)+1]] <- compute_by_group(sub, z_food, area_col, "food", "Area")
  if (!is.na(region_var)) results[[length(results)+1]] <- compute_by_group(sub, z_food, region_var, "food", "Region")
}
res <- do.call(rbind, results)
res$year <- rep(yrs, each = nrow(res)/length(yrs))

res[, c("headcount","poverty_gap","sq_poverty_gap")] <- lapply(res[, c("headcount","poverty_gap","sq_poverty_gap")], as.numeric)
write.csv(res, "output/poverty_measures.csv", row.names = FALSE)
print(head(res, 30))