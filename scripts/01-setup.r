## ---- Source Functions ----------------------------------------
## Source all the required and related function
source("scripts/00-function.r")

## ----LoadingPackages--------------------------------------------
## Load Required packages
pkgs <- c("pls", "Renvlp", "simrel", "pander", "tidyverse", "ggpubr",
          "reshape2", "glmnet", "doParallel", "parallel", "gridExtra",
          "ggridges")
for (pkg in pkgs) {
  require(pkg, quietly = T, warn.conflicts = F, character.only = T)
}

## ----MakingDesign----------------------------------------
## Here we have different levels of simulation parameters
## p: 20 and 250
## eta: 0, 0.4, 0.8, 1.2
## gamma: 0.2, 0.9
## R2: 0.8, 0.4
opts <- list(
  n      = rep(100, 2),
  p      = c(20, 250),
  m      = rep(4, 2),
  relpos = c("1, 2, 3, 4", "5, 6, 7, 8"),
  eta    = c(0, 0.4, 0.8, 1.2),
  gamma  = c(0.2, 0.9),
  R2     = rep(0.8, 2),
  ypos   = rep("1, 2, 3, 4", 2)
)

## ---- Prepare Design ----------------------------------------
## Create design table where each row represent a complete set of 
## simulate parametes. Extracting these rows using `get_design` function
## gives a ready-to-use list of simrel parameters which then be passed to
## `simulate` function for simulation
design <- simrel::mbrdsim(opts, 4)[["Design"]] %>%
  as_tibble() %>%
  rowwise() %>%
  mutate(q = p) %>%
  ungroup() %>%
  mutate_at(vars(-relpos, -ypos), map, parse_parm) %>%
  mutate_at(vars(relpos, ypos), map, parse_parm, in_list = TRUE) %>% 
  mutate_at(vars(n, p, m, eta, gamma, R2, q), as.numeric) %>%
  mutate(type = "multivariate",
         n = as.integer(n),
         p = as.integer(p),
         m = as.integer(m))

## The design table contains lists which will be difficult for print.
## `design_chr` is a character representation of those list so that
## we can use `design_chr` for printing purpose instead of `design` itself
design_chr <- design %>% mutate_if(is.list, map_chr, list2chr)

## ---- Estimation method and saving design ----------------------------------------
## Here we are using following estimation methods for comparison
## PCR: Principal Components Regression
## PLS1: Partial Least Squares where each response is modeled separately
## PLS2: Partial Least Squares where all responses are modeled together
## Xenv: Envelope estimation in Predictor Space
## Senv: Estimation of Simulteneous Envelope
## Ridge: Estimation using Ridge Regression
## Lasso: Estimation using Lasso Regression
mthds <- c('PCR', 'PLS1', 'PLS2', 'Xenv', 'Senv') %>% name_it()
## ---- Save opts, methods, design table as design.rdata ----
save(opts, mthds, design, design_chr, file = "scripts/robj/design.rdata")
