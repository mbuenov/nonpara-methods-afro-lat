# Loading packages recommended for this project
library(boot)
library(MASS)
library(GoFKernel)
library(sm)
library(pracma)
library(np)
library(splines)
library(SemiPar)
library(gt)
library(xtable)
library(ggbraid)
library(tidyr)
library(RColorBrewer)
library(patchwork)

# Loading other libraries
library(tidyverse)
library(haven)
library(foreign)
library(janitor)
library(labelled)
library(stargazer)

#### Loading data ###########

### Loading Data

gss_small <- read_csv("data/GSSsmall.csv")

val_labels(gss_small) <- NULL

### Tidying

gss_tidy <- gss_small %>% 
  mutate(hispanic = na_if(hispanic, 99)) %>% 
  mutate(hispanic = na_if(hispanic, 0)) %>%
  mutate(hispanic = na_if(hispanic, 98)) %>%
  mutate(hispanic = recode(hispanic,
                           '1' = 0,
                           '50' = 0))  %>%
  mutate(hispanic = recode(hispanic,
                           '2' = 1,
                           '3' = 1,
                           '4' = 1,
                           '5' = 1,
                           '6' = 1,
                           '7' = 1,
                           '8' = 1,
                           '9' = 1,
                           '10' = 1,
                           '11' = 1,
                           '15' = 1,
                           '16' = 1,
                           '20' = 1,
                           '21' = 1,
                           '22' = 1,
                           '23' = 1,
                           '24' = 1,
                           '25' = 1,
                           '30' = 1,
                           '31' = 1,
                           '35' = 1,
                           '40' = 1,
                           '41' = 1,
                           '42' = 1,
                           '45' = 1,
                           '46' = 1,
                           '47' = 1))

table(gss_tidy$hispanic)


### Race
# 1 = white, 2 = Black, 3 = other

table(gss_tidy$race)

### Recoding partyid
# 0 = Democrat, 1 = Republican, Other is NA

gss_tidy <- gss_tidy %>%
  mutate(partyid = na_if(partyid, 3)) %>%
  mutate(partyid = na_if(partyid, 7)) %>%
  mutate(partyid = na_if(partyid, 8)) %>%
  mutate(partyid = na_if(partyid, 9)) %>%
  mutate(partyid = recode(partyid,
                          '0' = 0,
                          '1' = 0,
                          '2' = 0,
                          '3' = 0,
                          '4' = 1,
                          '5' = 1,
                          '6' = 1))


### Recoding polviews
# 1 = Extremely Liberal, 4 = Moderate, 7 = Extremely Conservative

unique(gss_tidy$polviews)


### Filtering NAs and recoding region

### Region: 1 = Northeast, 2 = Midwest, 3 = South, 4 = West

gss_tidy <- gss_tidy %>% 
  dplyr::select(race, region, partyid, hispanic, coninc, polviews) %>% 
  filter(!is.na(hispanic)) %>% 
  filter(!is.na(partyid)) %>% 
  filter(!is.na(polviews)) %>% 
  filter(!is.na(coninc)) %>% 
  mutate(region = recode(region,
                         '1' = 1,
                         '2' = 1,
                         '3' = 2,
                         '4' = 2,
                         '5' = 3,
                         '6' = 3,
                         '7' = 3,
                         '8' = 4,
                         '9' = 4)) %>% 
  mutate(polviews_dist_mean = 
           (polviews - mean(polviews))) %>% 
  mutate(income_thou = coninc/1000)

gss_tidy <- as.data.frame(gss_tidy)

colSums(!is.na(gss_tidy)) 


### Summary Table

names(gss_tidy)

stargazer(as.data.frame(gss_tidy), summary = TRUE)


# Making a new Black subset

gss_black <- gss_tidy %>% 
  filter(race == 2)



# Making a new Hispanic subset

gss_hisp <- gss_tidy %>% 
  filter(hispanic == 1) %>% 
  filter(race != 3 )

### Sub-setting the data

unique(gss_tidy$hispanic)
table(gss_tidy$race)

# White Hispanics
gss_wh <- subset(gss_tidy, c(hispanic == 1 & race == 1))
gss_wh <- as.data.frame(gss_wh)

# Black Hispanics
gss_bh <- subset(gss_tidy, c(hispanic == 1 & race == 2))
gss_bh <- as.data.frame(gss_bh)


# White non-Hispanics
gss_wn <- subset(gss_tidy, c(hispanic == 0 & race == 1))
gss_wn <- as.data.frame(gss_wn)


# Black non-Hispanics
gss_bn <- subset(gss_tidy, c(hispanic == 0 & race == 2))
gss_bn <- as.data.frame(gss_bn)


### Means in income across groups

mean(gss_bn$income_thou)
mean(gss_wn$income_thou)
mean(gss_bh$income_thou)
mean(gss_wh$income_thou)

mean(gss_bn$coninc)
mean(gss_wn$coninc)
mean(gss_bh$coninc)
mean(gss_wh$coninc)


# White Hispanics Plots
plot(density(gss_wh$income_thou, bw="SJ"), # ideal binwidth = 4.46
     xlab="White Hispanics Income ($ thousands)", main="", ylim=c(0, .025))
lines(density(gss_wh$income_thou, bw="nrd0"), col="blue")
lines(density(gss_wh$income_thou, bw="ucv"), col="red")
legend("topright", legend = c("SJ", "NRD", "CV"), col=c("black", "blue", "red"), cex= 1,
       lty=1)

hist(gss_wh$income_thou,
     xlab="White Hispanics Income ($ thousands)", main="",
     col = "white",
     font.main = 1)

stargazer(gss_wh, summary = TRUE,
          title = "White Hispanics Income Summary Statistics")

# Black Hispanics Plots
plot(density(gss_bh$income_thou, bw="SJ"), # ideal binwidth = 7.31
     xlab= "Black Hispanics Income ($ thousands)", main="", ylim=c(0, .025))
lines(density(gss_bh$income_thou, bw="nrd0"), col="blue")
lines(density(gss_bh$income_thou, bw="ucv"), col="red")
legend("topright", legend = c("SJ", "NRD", "CV"), col=c("black", "blue", "red"), lty=1, cex = 1)

hist(gss_bh$income_thou,
     xlab= "Black Hispanics Income ($ thousands)", main="",
     col = "white")

stargazer(gss_bh, summary = TRUE,
          title = "Black Hispanics Income Summary Statistics")

# White non-Hispanic Plots
plot(density(gss_wn$income_thou, bw="SJ"),
     xlab="White non-Hispanics Income ($ thousands)", main="", ylim=c(0, .02))
lines(density(gss_wn$income_thou, bw="nrd0"), col="blue")
lines(density(gss_wn$income_thou, bw="ucv"), col="red")
legend("topright", legend = c("SJ", "NRD", "CV"), col=c("black", "blue", "red"), lty=1, cex = 1)

hist(gss_wn$income_thou,
     xlab="White non-Hispanics Income ($ thousands)", main="",
     col = "white")

stargazer(gss_wn, summary = TRUE,
          title = "White non-Hispanics Income Summary Statistics")

# Black non-Hispanics Plot
plot(density(gss_bn$income_thou, bw="SJ",
             cex.main = .5,
             cex.sub = .5,  
             cex.lab = .5,    
             cex.axis = .5),
     xlab="Black non-Hispanics Income ($ thousands)", main="", ylim=c(0, .03))
lines(density(gss_bn$income_thou, bw="nrd0"), col="blue")
lines(density(gss_bn$income_thou, bw="ucv"), col="red")
legend("topright", legend = c("SJ", "NRD", "CV"), col=c("black", "blue", "red"), lty=1, cex = 1)

hist(gss_bn$income_thou,
     xlab="Black non-Hispanics Income ($ thousands)", main="",
     col = "white")

stargazer(gss_bn, summary = TRUE,
          title = "Black non-Hispanics Income Summary Statistics")


### Numerical integration - mu_hat

denout <- density(gss_wh$income_thou, bw = "SJ")

f <- function(y){
  y*approxfun(denout)(y) 
}

summary(denout$x)  # xmin = -13.03, xmax = 192.09
# fall outside the data range below
summary(gss_wh$income_thou) # xmin = 0.3505, xmax = 178.7125 

### Integrating to find mu_hat

wh_muhat_int <- integrate(f = f, lower = -13.03, upper = 192.09) 
# 44.64594 with absolute error < 0.0018
wh_muhat <- 44.64594
wh_muhat_error <- "< 0.0018"

# vs. the mean of the variable
wh_truemu <- mean(gss_wh$income_thou) # 44.60438

mu_hat <- 44.64594

####

# Numerical integration - sigmasq_hat

denout <- density(gss_wh$income_thou, bw = "SJ")

# defining a function

f2 <- function(y){
  (y - mu_hat)^2 * approxfun(denout)(y) 
}

# Integrating to find sigma^2_hat

wh_sig2hat_int <- integrate(f = f2, lower = -13.03, upper = 192.09) 
# 1603.47 with absolute error < 0.17

wh_sig2hat <- 1603.47
wh_sig2hat_error <- "< 0.17"

# vs. the variance of the variable
wh_truesig2 <- var(gss_wh$income_thou) # 1584.079
```


```{r}
#| label: manual-int-df-wh


### Saving results into vectors for df

wh_mu_diff <- wh_muhat - wh_truemu 
wh_sig_diff <- wh_sig2hat - wh_truesig2
wh_method <- "Sheather-Jones"
wh_samplesize <- 871

# Vector with manual integration results
wh_manual_integration <- c(wh_muhat,
                           wh_muhat_error, 
                           wh_truemu, 
                           wh_mu_diff, 
                           wh_sig2hat, 
                           wh_sig2hat_error,
                           wh_truesig2, 
                           wh_sig_diff, 
                           wh_method, 
                           wh_samplesize)


# Numerical integration - mu_hat

denout <- density(gss_bh$income_thou, bw = "SJ")

f <- function(y){
  y*approxfun(denout)(y) 
}

summary(denout$x)  # xmin = -21.56, xmax = 188.35
# fall outside the data range below
summary(gss_bh$income_thou) # xmin = 0.3695, xmax = 166.4190 

# Integrating to find mu_hat

bh_muhat_int <- integrate(f = f, lower = -21.56, upper = 188.35) # 33.53353 with absolute error < 0.003
bh_muhat <- 33.53353
bh_muhat_error <- "< 0.003"

# vs. the mean of the variable
bh_truemu <- mean(gss_bh$income_thou) # 33.50242

mu_hat <- 33.53353

####

# Numerical integration - sigmasq_hat

denout <- density(gss_bh$income_thou, bw = "SJ")

f2 <- function(y){
  (y - mu_hat)^2 * approxfun(denout)(y) 
}

bh_sig2hat_int <- integrate(f = f2, lower = -21.56, upper = 188.35) # 839.3324 with absolute error < 0.046
bh_sig2hat <- 839.3324
bh_sig2hat_error <- "< 0.046"

# vs. the variance of the variable
bh_truesig2 <- var(gss_bh$income_thou) # 796.0166


### Saving results into vectors for df

bh_mu_diff <- bh_muhat - bh_truemu 
bh_sig_diff <- bh_sig2hat - bh_truesig2
bh_method <- "Sheather-Jones"
bh_samplesize <- 76

# Vector with manual integration results

bh_manual_integration <- c(bh_muhat, 
                           bh_muhat_error, 
                           bh_truemu, 
                           bh_mu_diff, 
                           bh_sig2hat, 
                           bh_sig2hat_error,
                           bh_truesig2, 
                           bh_sig_diff, 
                           bh_method, 
                           bh_samplesize)


# Numerical integration - mu_hat

denout <- density(gss_wn$income_thou, bw = "sj")

f <- function(y){
  y*approxfun(denout)(y) 
}

summary(denout$x) 
# this summary provides the upper and lower limits for the integration
# x-min: -2.724, x-max: 181.787
summary(gss_wn$income_thou)
# the values found through the density estimation above 
# must lie outside the range of our sample data for this    
# integration to be solvable
# min:  0.3505, max: 178.7125

# Integrating to find mu_hat

integrate(f = f, lower =  -2.724, 
          upper = 181.787, 
          rel.tol = .Machine$double.eps^.05) 
# 56.66262 with absolute error < 8.8

# sj method requires increased tolerance for the 
# White Non-Hispanic Sample, 
# as the algorithm does not converge beyond 100 subdivisions. 
# Increasing subdivisions to 251 produces a roundoff error 
# Given the large n, concerns over accuracy with 
# increased tolerance are minimal, 
# but repeating analyses with the cross-val 
# or NRD method is preferrable

# vs. the mean of the variable
wn_truemu <- mean(gss_wn$income_thou) # 56.59982

mu_hat <- 56.66262

####

# Numerical integration - sigma_hat

denout <- density(gss_wn$income_thou, bw = "sj")

f2 <- function(y){
  (y - mu_hat)^2 * approxfun(denout)(y) 
}

integrate(f = f2, 
          lower = -2.724, 
          upper = 181.787, 
          rel.tol = .Machine$double.eps^.05) 
# 2258.007 with absolute error < 34

# vs. the variance of the variable
wn_truesig2 <- var(gss_wn$income_thou) # 2028.442


### Repeating calculations using other methods

# Numerical integration - mu_hat

denout <- density(gss_wn$income_thou, bw = "nrd0")

f <- function(y){
  y*approxfun(denout)(y) 
}

summary(denout$x) 
# this summary provides the upper and lower limits for the integration
# x-min: -2.116, x-max: 181.179
summary(gss_wn$income_thou)
# the values found through the density estimation above 
# must lie outside the range of our sample data for this    
# integration to be solvable

# Integrating to find mu_hat

integrate(f = f, lower =  -15.20, upper = 194.26)
# 56.6508 with absolute error < 0.00088
# the nrd0 produces reliable results while keeping 
# analyses under a reasonable error tolerance

wn_muhat <- 56.6508
wn_muhat_error <- "< 0.00088"
wn_method <- "nrd0"

# Numerical integration - sigma_hat

f2 <- function(y){
  (y - mu_hat)^2 * approxfun(denout)(y) 
}

integrate(f = f2, lower =  -15.20, upper = 194.26) 
# 2199.699 with absolute error < 0.076

wn_sig2hat <- 2199.699
wn_sig2hat_error <- "< 0.076"

# vs. the variance of the variable
wn_truesig2 <- var(gss_wn$income_thou) # 2028.442

wn_mu_diff <- wn_muhat - wn_truemu
wn_sig_diff <- wn_sig2hat - wn_truesig2
wn_samplesize <- 11759

# Vector with manual integration results
wn_manual_integration <- c(wn_muhat, 
                           wn_muhat_error, 
                           wn_truemu, 
                           wn_mu_diff, 
                           wn_sig2hat, 
                           wn_sig2hat_error,
                           wn_truesig2, 
                           wn_sig_diff, 
                           wn_method, 
                           wn_samplesize)

# Numerical integration - mu_hat

denout <- density(gss_bn$income_thou, bw = "SJ")

f <- function(y){
  y*approxfun(denout)(y) 
}

summary(denout$x) 

# Integrating to find mu_hat

integrate(f = f, 
          lower =  -6.18 , 
          upper = 185.24) # 33.97187 with absolute error < 0.00091

bn_muhat <- 33.97187
bn_muhat_error <- "< 0.00091"

# vs. the mean of the variable
bn_truemu <- mean(gss_bn$income_thou) # 33.9398

mu_hat <- 33.97187

####

# Numerical integration - sigmasq_hat

denout <- density(gss_bn$income_thou, 
                  bw = "SJ")

f2 <- function(y){
  (y - mu_hat)^2 * approxfun(denout)(y) 
}

integrate(f = f2, 
          lower =  -6.18 , 
          upper = 185.24) 
# 1019.407 with absolute error < 0.038

bn_sig2hat <- 1019.407
bn_sig2hat_error <- "< 0.038"

# vs. the variance of the variable
bn_truesig2 <- var(gss_bn$income_thou) # 1014.201

# Vector with manual integration results
bn_mu_diff <- bn_muhat - bn_truemu
bn_sig_diff <- bn_sig2hat - bn_truesig2
bn_samplesize <- 2476
bn_method <- "Sheather-Jones"

bn_manual_integration <- c(bn_muhat, 
                           bn_muhat_error, 
                           bn_truemu, 
                           bn_mu_diff, 
                           bn_sig2hat, 
                           bn_sig2hat_error, 
                           bn_truesig2, 
                           bn_sig_diff,
                           bn_method, 
                           bn_samplesize)


# Creating a df

# Summary statistics names
stats_titles <- c("Mu Estimate", 
                  "Mu Error", 
                  "Sample Mean", 
                  "Mean Estimate-Sample Difference", 
                  "Sigma Sq.", 
                  "Sigma Sq. Error", 
                  "Sample Variance", 
                  "Variance Estimate-Sample Difference", 
                  "Binsize method", 
                  "Sample size")

group_titles <- c("White Hispanic",
                  "Black Hispanic",
                  "White non-Hispanic",
                  "Black non-Hispanic")

# Tidy summary statistics names
stats_tidy <- c('muhat', 
                'muhat_error', 
                'sample_mu', 
                'mu_diff', 
                'sig2hat', 
                'sig2hat_error', 
                'truesig2', 
                'sig2_diff', 
                'method', 
                'sample_size')

group_names <- c("white_hisp", 
                 "black_hisp", 
                 "white_nonhisp", 
                 "black_nonhisp")

# Short df - columns are the 4 groups
manual_int_df_short <- cbind(values = stats_titles,
                             wh_manual_integration, 
                             bh_manual_integration, 
                             wn_manual_integration, 
                             bn_manual_integration)
manual_int_df_short

# Long df - columns are the statistics
manual_int_df_long <- rbind(wh_manual_integration, 
                            bh_manual_integration, 
                            wn_manual_integration, 
                            bn_manual_integration)
colnames(manual_int_df_long) <- stats_tidy
rownames(manual_int_df_long) <- group_names

# Converting to tibble
manual_int_df_long <- as_tibble(manual_int_df_long,
                                .rows = 4)
rownames(manual_int_df_long) <- group_names


# All stats values being read as strings, need to convert some vars to numeric
names(manual_int_df_long)

numeric_stats <- c("muhat", 
                   "sample_mu", 
                   "mu_diff", 
                   "sig2hat", 
                   "truesig2" , 
                   "sig2_diff", 
                   "sample_size")

# Changing numeric stats, rounding to 3 decimals, adding commas to thousands
manual_int_table <- manual_int_df_long %>% 
  mutate_at(numeric_stats, as.numeric) %>% 
  mutate(across(numeric_stats, round, 3)) %>% 
  mutate(across(numeric_stats, prettyNum, big.mark = ",")) %>% 
  mutate("group" = group_names) %>% 
  relocate(group, 1)


# Displaying table
xtable(manual_int_table)
gt(manual_int_table)

# Transposing for publication ease
manual_int_table_short <- as.tibble(t(manual_int_table[-1])) %>% 
  mutate("Statistic" = stats_titles) %>% 
  relocate(Statistic, 1) %>% 
  rename("White Hispanic" = V1,
         "Black Hispanic" = V2,
         "White non-Hispanic" = V3,
         "Black non-Hispanic" = V4)

# Displaying table
xtable(manual_int_table_short)
gt(manual_int_table_short)


### HYPOTHESIS TESTING #########################################

# Plotting options
sm.options(
  alpha = 0.9,
  lwd = c(2,2),
  lty = c(2, 1),
  col = c("#018571", "#a63603"),
  col.band = "#fec44f",
  col.palette = terrain.colors(12)
)


# Making a new subset
gss_hisp <- gss_tidy %>% 
  filter(hispanic == 1) %>% 
  filter(race != 3 )


# Hypothesis testing
set.seed(352)
comp1 <- sm.density.compare(gss_hisp$income_thou, group = gss_hisp$race,  
                            model = "equal", 
                            bw = "SJ", 
                            nboot = 10000,
                            xlab = "Income in Thousand $")
legend("topright", comp1$levels, col = comp1$col, lty = comp1$lty, lwd = comp1$lwd)
title(main = "Density Hypothesis Test", 
      sub = "(White Hispanic and Black Hispanic densities)", 
      ylab = "Density")


# Making a new subset

gss_white <- gss_tidy %>% 
  filter(race == 1)


# Hypothesis testing

set.seed(352)
comp2 <- sm.density.compare(gss_white$income_thou, 
                            group = gss_white$hispanic,
                            model = "equal", 
                            bw = "SJ", 
                            nboot = 10000, 
                            xlab = "Income in Thousand $")
legend("topright",
       comp2$levels, 
       col = comp2$col, 
       lty = comp2$lty, 
       lwd = comp2$lwd)
title(main = "Density Hypothesis Test", 
      sub = "(White Hispanic and White non-Hispanic densities)", 
      ylab = "Density")

# Making a new subset

gss_black <- gss_tidy %>% 
  filter(race == 2)


# Hypothesis testing

set.seed(352)
comp3 <- sm.density.compare(gss_black$income_thou, 
                            group = gss_black$hispanic,  
                            model = "equal", 
                            bw = "SJ", 
                            nboot = 10000,
                            xlab = "Income in Thousand $")
legend("topright",
       comp3$levels, 
       col = comp3$col, 
       lty = comp3$lty, 
       lwd = comp3$lwd)
title(main = "Density Hypothesis Test", 
      sub = "(Black Hispanic and Black non-Hispanic densities)", 
      ylab = "Density")

### Political views viz

groups <- c("White Hispanic", "White Non-Hispanic", "Black non-Hispanic", "Black Hispanic")

polv <- c(4.014, 4.184, 3.811, 3.684)

pol_dist <- c(-0.075, 0.095, -0.277, -0.404)

abs_pol_dist <- abs(c(-0.075, 0.095, -0.277, -0.404))

pol_mat <- cbind(groups, polv, pol_dist)

pol_mat <- as_data_frame(pol_mat)

knitr::kable(pol_mat)

polview_dist_plot <- pol_mat %>% ggplot(aes(groups, abs_pol_dist, fill = groups)) + 
  geom_col(width = .4) +
  ylab(NULL) + 
  xlab(NULL) +
  labs(title = "Group Mean Distance from Mean \nSample Political View",
       subtitle = "Data: General Social Survey") +
  scale_fill_brewer(palette = "Set2") +
  scale_x_discrete(
    labels = c("Black \nHispanic",
               "Black \nnon-\nHispanic",
               "White \nHispanic",
               "White \nnon-\nHispanic") 
  ) +
  theme_light() +
  theme(legend.position = "none",
        axis.text.y = element_text(size = 8))

polview_mean_plot <- pol_mat %>% ggplot(aes(groups, polv, fill = groups)) + 
  geom_bar(stat = "identity", width = .4) +
  ylab("More Liberal          More Conservative ") +
  xlab(NULL) +
  labs(title = "Mean Political View per group",
       subtitle = "Data: General Social Survey") +
  scale_fill_brewer(palette = "Set2") +
  scale_x_discrete(
    labels = c("Black \nHispanic",
               "Black \nnon-\nHispanic",
               "White \nHispanic",
               "White \nnon-\nHispanic") 
  ) +
  theme_light() +
  theme(legend.position = "none",
        axis.text.y = element_text(size = 8))



polview_mean_plot / polview_dist_plot
polview_mean_plot + polview_dist_plot

#### HYPOTHESIS TESTING - POL VIEWS ############################

# Hypothesis testing

set.seed(352)
viewscomp1 <- sm.density.compare(gss_hisp$polviews, group = gss_hisp$race,
                                 model = "equal", 
                                 bw = "SJ", 
                                 nboot = 10000,
                                 xlab = "Political views")
title(main = "Density Hypothesis Test", 
      sub = "(White Hispanic and Black Hispanic densities)", 
      ylab = "Density")
legend("topright",
       viewscomp1$levels, 
       col = viewscomp1$col, 
       lty = viewscomp1$lty, 
       lwd = viewscomp1$lwd)
# Not statistically significant - p = 0.084

# Hypothesis testing

set.seed(352)
viewscomp2 <- sm.density.compare(gss_black$polviews, group = gss_black$hispanic, 
                                 model = "equal", 
                                 bw = "SJ", 
                                 nboot = 10000,
                                 xlab = "Political views")
title(main = "Density Hypothesis Test", 
      sub = "(Black Hispanic and Black Non-Hispanic densities)", 
      ylab = "Density")
legend("topright",
       viewscomp2$levels, 
       col = viewscomp2$col, 
       lty = viewscomp2$lty, 
       lwd = viewscomp2$lwd)
# Test of equal densities:  p-value =  0.036 
# Statistically significant p < 0.05

# Making a new subset

gss_hisp <- gss_tidy %>% 
  filter(hispanic == 1) %>% 
  filter(race != 3 )


# Hypothesis testing

set.seed(352)
party_comp1 <- sm.density.compare(gss_hisp$partyid, group = gss_hisp$race, 
                                  model = "equal", 
                                  bw = "SJ", 
                                  nboot = 10000,
                                  xlab = "Party identification")
title(main = "Density Hypothesis Test", 
      sub = "(White Hispanic and Black Hispanic densities)", 
      ylab = "Density")
legend("topright",
       party_comp1$levels, 
       col = party_comp1$col, 
       lty = party_comp1$lty, 
       lwd = party_comp1$lwd)

# Hypothesis testing

set.seed(352)
party_comp2 <- sm.density.compare(gss_black$partyid, group = gss_black$hispanic,  
                                  model = "equal", 
                                  bw = "SJ", 
                                  nboot = 10000, 
                                  xlab="Party identification")
title(main = "Density Hypothesis Test", 
      sub = "(Black Hispanic and Black non-Hispanic densities)", 
      ylab = "Density")
legend("topright",
       party_comp2$levels, 
       col = party_comp2$col, 
       lty = party_comp2$lty, 
       lwd = party_comp2$lwd)


## Conclusions

gss_hisp <- gss_hisp %>% 
  select(race, polviews, income_thou)

# Plotting the densities separately

plot(density(gss_wh$income_thou),
     main = 'White Hisp. Income')
plot(density(gss_bh$income_thou),
     main = 'Black Hisp. Income' )
plot(density(gss_wn$income_thou),
     main = 'White non-Hisp. Income')
plot(density(gss_bn$income_thou),
     main = 'Black non-Hisp. Income' )








