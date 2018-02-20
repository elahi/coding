## Comparing methods in meta-analysis
## Robin Elahi
## 20 Feb 2018

library(metafor)

##### Studies on the Effects of Elevated CO2 Levels on Woody Plant Mass #####

### co2 experiments
head(dat.curtis1998)

### load data
dat <- get(data(dat.curtis1998))

### calculate log ratio of means and corresponding sampling variances

## to calculate LRR I need (for each treatment): mean, standard deviation, sample size
dat <- escalc(measure="ROM", m1i=m1i, sd1i=sd1i, n1i=n1i, m2i=m2i, sd2i=sd2i, n2i=n2i, data=dat)
head(dat)

### meta-analysis of log ratio of means using a random-effects model
res <- rma(yi, vi, method="DL", data=dat)
res
plot(res)
forest(res)

### average ratio of means with 95\% CI
predict(res, transf=exp, digits=2)

### meta-analysis for plants grown under nutrient stress
res <- rma(yi, vi, method="DL", data=dat, subset=(xtrt=="FERT" & level=="LOW"))
predict(res, transf=exp, digits=2)

### meta-analysis for plants grown under low light conditions
res <- rma(yi, vi, method="DL", data=dat, subset=(xtrt=="LIGHT" & level=="LOW"))
predict(res, transf=exp, digits=2)

##### Comparing metafor with Bayesian approach #####

## From:
## https://mvuorre.github.io/post/2016/2016-09-29-bayesian-meta-analysis/
## https://github.com/mvuorre/mvuorre.github.io/blob/master/post/2016/2016-09-29-bayesian-meta-analysis.Rmd


library(lme4)
library(brms)
library(tidyverse)

## See:
## http://www.metafor-project.org/doku.php/tips:rma_vs_lm_lme_lmer?s%5B%5D=lme4

dat.molloy2014
#' ni = sample size
#' ri = raw correlation coefficients (these will be transformed to "ZCOR" - fisher's r to z transformed correlation coefficient)

## From Matti's Rmd file
?escalc
dat <- escalc(measure="ZCOR", ri=ri, ni=ni, data=dat.molloy2014)
dat
dat <- dat[,-c(5:10)]
dat <- as.data.frame(dat)
dat$yi <- as.numeric(dat$yi)
dat$vi <- as.numeric(dat$vi)
names(dat) <- c("study", "year", "ni", "ri", "yi", "vi")
dat$study <- paste0(dat$study, " (", dat$year, ")")
dat$sei <- as.numeric(sqrt(dat$vi))
dat$study <- as.character(dat$study)
# dat$order <- as.integer(reorder(dat$study, dat$yi))
# Major pain in reordering
dat$study[5] <- "Christensen et al. (1995)"
tmp <- dat[5,]
dat[5,] <- dat[4,]
dat[4,] <- tmp

ggplot(dat, aes(x=yi, y=study)) +
  geom_segment(aes(x = yi-sei*2, xend = yi+sei*2, y=study, yend=study)) +
  geom_point()

ma_out <- rma(data = dat, yi = yi, sei = sei, slab = dat$study)
summary(ma_out)
plot(summary(ma_out))

## Fit stan model
## Note that I am setting the weights somehow - need to understand this explicitly
brm_out <- brm(yi | se(sei) ~ 1 + (1|study), 
               prior = set_prior("uniform(0, 1000)", class = "sd"),
               data = dat, iter = 5000, warmup = 2000, cores = 4)

## Plotting Stan results
library(viridis)
ma_tau <- confint(ma_out)$random[2,]
ma_mu <- confint(ma_out, fixed = T)$fixed
posterior <- as.data.frame(brm_out, pars = c("sd", "b_"))
names(posterior) <- c("mu", "tau")
gridExtra::grid.arrange(
  ggplot(posterior, aes(x=mu)) +
    geom_histogram(binwidth=.001) +
    geom_vline(xintercept = as.numeric(ma_mu), lty = 2) +
    scale_x_continuous(expression(mu), limits = c(0, 0.5)) +
    scale_y_continuous(expand = c(0, 0)) + 
    theme(panel.grid = element_blank()),
  ggplot(posterior, aes(x=tau)) +
    geom_histogram(binwidth=.001) +
    geom_vline(xintercept = as.numeric(ma_tau), lty = 2) +
    scale_x_continuous(expression(tau), limits = c(0, 0.5)) +
    scale_y_continuous(expand = c(0, 0)) + 
    theme(panel.grid = element_blank()),
  ggplot(posterior, aes(x=mu, y=tau)) + 
    stat_density_2d(geom = "raster", aes(fill = ..density..), contour = FALSE) +
    geom_vline(xintercept = as.numeric(ma_mu)[1], lty = 2) +
    geom_hline(yintercept = as.numeric(ma_tau)[1], lty = 2) +
    scale_fill_viridis(na.value = "black") +
    coord_cartesian(xlim = c(0,0.3), ylim = c(0, 0.3)) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(x = expression(mu), y = expression(tau)),
  ggplot(dat, aes(x=yi, y=study)) +
    geom_segment(aes(x = yi-sei*2, xend = yi+sei*2, y=study, yend=study)) +
    geom_point(),
  ncol = 2
)
  