################################################################################
##' @title Summarise simulated time series data
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2017-03-17
##' 
##' @log Add a log here
################################################################################

#rm(list=ls(all=TRUE))

setwd("/Users/elahi/github/coding")

##### Assign variables #####
# Mean response
y_mu = 0
# Standard deviation of response
y_sd = 1
# Number of years
n_yrs = 30
# Linear trend
linear_trend = 0
# Standard deviation of trend
linear_trend_sd = 0.25
# Time
x = seq(1:number_yrs)
# Intercept
intercept = 0
# Slope
slope = 0.25

## Random variation for each time point
w <- rnorm(n_yrs, mean = 0, sd = y_sd) 
## Get time series
y <- slope * x + intercept + w

### Plot one series
png(filename = "figs/plot_time_series_1.png", height = 2, width = 2, units = "in", res = 600)
par(mar = c(2,2,1,1))
plot(x, y, type = "l", xaxt = "n", yaxt = "n", 
     xlab = "", ylab = "", 
     ylim = c(-15, 15), lwd = 0.75)
title(ylab = "Response", line = 0.5, cex.lab = 1.2)
title(xlab = "Time", line = 0.5, cex.lab = 1.2)
dev.off()

### Plot multiple series
n_series <- 10
colors <- seq(1, n_series, 1) 
library(RColorBrewer)
darkcols <- brewer.pal(8, "Dark2")

png(filename = "figs/plot_time_series_multiple.png", height = 2, width = 2, units = "in", res = 600)
par(mar = c(2,2,1,1))
plot(x, y, type = "l", xaxt = "n", yaxt = "n", 
     xlab = "", ylab = "", 
     ylim = c(-15, 15), lwd = 0.75)
title(ylab = "Response", line = 0.5, cex.lab = 1.2)
title(xlab = "Time", line = 0.5, cex.lab = 1.2)

set.seed(211)
for(i in 2:9){
  slope.i <- rnorm(1, 0, 0.25)
  intercept.i <- rnorm(1, 0, 2)
  ## Random variation for each time point
  y_sd.i <- max(0.25, rnorm(1, 1, 0.5))
  w.i <- rnorm(n_yrs, mean = 0, sd = y_sd.i) 
  ## Get time series
  y.i <- slope.i * x + intercept.i + w.i
  ## Plot it
  lines(x, y.i, type = "l", col = darkcols[i], lwd = 0.75)
}
dev.off()


##### LOAD PACKAGES, DATA #####
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(broom)

sim_df <- read.csv("data/sim_df.csv") %>% tbl_df() %>% select(-X)

# RELEVEL SCENARIOS
sim_df <- sim_df %>% 
  mutate(scenario = factor(scenario, 
                           levels = c("Stable", "Linear", 
                                      "Phase_shift", "Oscillations")))

# How many simulations total?
n_sims = 400
i = 1
i_vec <- rep(i,30)
for(i in 2:n_sims){
  i_new <- rep(i, 30)
  i_vec <- c(i_vec, i_new)
}

sim_df$sim_total <- i_vec

# Select randomly a few time series
n = 10
set.seed(10)
ts_vec <- round(runif(n, min = 1, max = n_sims))

dat <- sim_df[sim_df$sim_total %in% ts_vec, ]

library(RColorBrewer)
theme_set(theme_bw(base_size = 12))

with(dat, plot(year, y))

dat %>% 
  ggplot(aes(year, y, group = sim_total)) + 
  ylab("coral cover (%)") + xlab("year") + 
  scale_y_continuous(limits = c(0, 60)) + 
  theme(legend.position = "none") + 
  geom_line(alpha = 0.8, aes(color = sim_total), 
            size = 1.25) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


plot_box


plot_dat <- grand_means

## Get colors for scenarios
scenarios <- unique(plot_dat$scenario)
cb_pal_scenario <- c("#D55E00", "#E69F00", "#56B4E9", "#009E73")
names(cb_pal_scenario) <- scenarios
col_scale_scenario <- scale_colour_manual(name = "Model Scenario", values = cb_pal_scenario)
fill_scale_scenario <- scale_fill_manual(name = "Model Scenario", values = cb_pal_scenario)

##### OASIS Z PLOT #####

plot_dat %>% 
  ggplot(aes(med_z, cv, fill = scenario, shape = scenario)) + 
  xlab("Median z-score of coral cover") + 
  ylab("Temporal variability in coral cover (CV)") + 
  annotate("rect", xmin = -2, xmax = -1, ymin = 100, ymax = 150,
           alpha = 0, color = "gray", linetype = "solid") +
  annotate("rect", xmin = -2, xmax = -1, ymin = 50, ymax = 100,
           alpha = 0, color = "gray") +
  annotate("rect", xmin = -2, xmax = -1, ymin = 0, ymax = 50,
           alpha = 0, color = "gray") +
  annotate("rect", xmin = -1, xmax = -0, ymin = 50, ymax = 100,
           alpha = 0, color = "gray") +
  annotate("rect", xmin = 0, xmax = 1, ymin = 50, ymax = 100,
           alpha = 0, color = "gray") +
  annotate("rect", xmin = 1, xmax = 2, ymin = 0, ymax = 50,
           alpha = 0, color = "gray") +
  annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 50,
           alpha = 0, color = "gray") +
  annotate("rect", xmin = -1, xmax = 0, ymin = 0, ymax = 50,
           alpha = 0, color = "gray") +
  annotate("rect", xmin = -1, xmax = -2, ymin = 0, ymax = 50,
           alpha = 0, color = "gray") +
  geom_point(alpha = 0.75) + 
  theme(legend.position = c(1,1), legend.justification = c(1.25, 1.25)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  scale_fill_manual(name = "Model Scenario", values = cb_pal_scenario) + 
  scale_shape_manual(name = "Model Scenario", values = c(21,22,23,25))

#ggsave("figs_sims/oasis_z_plot.pdf", height = 5, width = 6)

##### PICK REPRESENTATIVE SITES #####

##' (stable positive)
##' (phase shift)
##' (stable negative)
##' (non-linear decline)

plot_dat %>% 
  ggplot(aes(med_z, cv, color = scenario)) + 
  xlab("Median z-score of coral cover") + 
  ylab("Temporal variability in coral cover (CV)") + 
  annotate("rect", xmin = -2, xmax = -1, ymin = 100, ymax = 150,
           alpha = 0, color = "gray", linetype = "solid") +
  annotate("rect", xmin = -2, xmax = -1, ymin = 50, ymax = 100,
           alpha = 0, color = "gray") +
  annotate("rect", xmin = -2, xmax = -1, ymin = 0, ymax = 50,
           alpha = 0, color = "gray") +
  annotate("rect", xmin = -1, xmax = -0, ymin = 50, ymax = 100,
           alpha = 0, color = "gray") +
  annotate("rect", xmin = 0, xmax = 1, ymin = 50, ymax = 100,
           alpha = 0, color = "gray") +
  annotate("rect", xmin = 1, xmax = 2, ymin = 0, ymax = 50,
           alpha = 0, color = "gray") +
  annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 50,
           alpha = 0, color = "gray") +
  annotate("rect", xmin = -1, xmax = 0, ymin = 0, ymax = 50,
           alpha = 0, color = "gray") +
  annotate("rect", xmin = -1, xmax = -2, ymin = 0, ymax = 50,
           alpha = 0, color = "gray") +
  geom_point(alpha = 0.75) + 
  theme(legend.position = c(1,1), legend.justification = c(1.25, 1.25)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  scale_color_manual(name = "Model Scenario", values = cb_pal_scenario) + 
  geom_text(data = plot_dat,  
            aes(label = sim_total), size = 2.5, check_overlap = FALSE, 
            vjust = 0, nudge_y = 3)

## Manually select time-series for each box
## (choosing one haphazardly for each scenario in the middle of boxes)
box1 = c(229, 121, 395)
box2 = c(226, 135, 384)
box3 = c(113, 373)
box4 = c(124, 302)
box5 = c(5, 246, 176, 326)
box6 = c(50, 201, 116, 342)
box7 = c(78, 237, 195, 324)
box8 = c(92, 216, 137, 314)

## Get needed items for the loop
box_list <- list(box1, box2, box3, box4, 
                 box5, box6, box7, box8)

box_names <- c("box1", "box2", "box3", "box4", 
               "box5", "box6", "box7", "box8")

box_names_length <- length(box_list)
box_results <- vector("list", box_names_length)

for(i in 1:box_names_length){
  
  box_i <- box_list[[i]]
  box_name_i <- box_names[i]
  box_i_df <- sim_df[sim_df$sim_total %in% box_i, ]
  box_i_df$box <- box_name_i
  
  plot_box <- box_i_df %>% 
    ggplot(aes(year, y)) + 
    ylab("coral cover (%)") + xlab("year") + 
    scale_y_continuous(limits = c(0, 60)) + 
    theme(legend.position = "none") + 
    geom_line(aes(year, y_mean), alpha = 1, size = 1, 
              data = mean_cover_df, color = "black", linetype = "dashed") + 
    geom_line(alpha = 0.8, aes(group = sim2, color = scenario), 
              size = 1.25) + 
    col_scale_scenario + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  
  box_results[[i]] <- plot_box
}

##### USE GRID ARRANGE TO CREATE PANEL B #####

hlay <- rbind(c(1,NA,NA,NA),
              c(2,3,4,NA),
              c(5,6,7,8))

pdf("figs_sims/box_time_series_panels.pdf", 
    height = 5, width = 7)
grid.arrange(grobs = box_results, layout_matrix = hlay)
dev.off()

