## ---- Loading Packages ----
library(tidyverse)
library(pls)
library(simrel)
library(Renvlp)

## ---- Source Functions ----
source('scripts/00-function.r')

## ---- Simulation ----
sobj_1 <- simrel(
  n = 100, p = 10, m = 3, ntest = 200, q = c(3, 3, 4), eta = 0,
  gamma = 0.8, relpos = list(c(1, 5), c(2), c(3, 4)),
  ypos = list(c(1, 2, 3)), R2 = c(0.3, 0.3, 0.3),
  type = "multivariate"
)

sobj_2 <- simrel(
  n = 100, p = 10, m = 3, ntest = 200, q = 10, eta = 0,
  gamma = 0.8, relpos = list(1:5),
  ypos = list(1:3), R2 = 0.9,
  type = "multivariate")


errors <- list(
  senv_mdl_1 = coef_errors(sobj_1, 'Senv', scale = FALSE, u = 3),
  senv_mdl_2 = coef_errors(sobj_2, 'Senv', scale = FALSE, u = 1),
  pls_mdl_1 = coef_errors(sobj_1, 'PLS2', scale = FALSE),
  pls_mdl_2 = coef_errors(sobj_2, 'PLS2', scale = FALSE),
  xenv_mdl_1 = coef_errors(sobj_1, 'Xenv', scale = FALSE),
  xenv_mdl_2 = coef_errors(sobj_2, 'Xenv', scale = FALSE)
)

plts <- lapply(errors, err_plot, params = c('ypos', 'q', 'R2', 'relpos'))
plts <- lapply(plts, function(plt) {
  plt + coord_cartesian(ylim = c(0, 1.7)) +
  theme(
    legend.position = c(0.5, 0.1),
    legend.direction = "horizontal",
    legend.background = element_blank()
  )
})

do.call(gridExtra::grid.arrange, plts)
