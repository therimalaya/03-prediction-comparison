---
title: "A Report on Comparison of Multivariate Estimators"
author: "Raju Rimal"
output: 
  bookdown::html_document2:
    self_contained: no
    code_folding: 'hide'
    highlight: 'tango'
    code_download: yes
    theme: 'cosmo'
params:
  design:
    label: "Design:"
    value: 16
    input: slider
    min: 1
    max: 32
    step: 1
  replication:
    label: "Replication:"
    value: 5
    input: numeric
    min: 1
  indiv:
    label: "Average over response?"
    choices: {'yes': TRUE, 'no': FALSE}
    value: FALSE
    inline: yes
---

```{r setup, include = FALSE}
knitr::opts_chunk$set(
  comment = "#> ",
  message = FALSE,
  warning = FALSE,
  error = FALSE,
  out.width = '100%',
  fig.retina = 2,
  dev = 'svg'
)
library(DT)
```


# Functions and Scripts
```{r}
source("scripts/00-function.r")
source("scripts/01-setup.r")
```

# The Design
```{r}
design %>%
  rownames_to_column("Design") %>% 
  DT::datatable(rownames = FALSE, 
                options = list(pageLength = 16)) %>% 
  DT::formatStyle(1:ncol(design), fontFamily = "Source Code Pro") %>% 
  DT::formatStyle('Design', target = 'row', backgroundColor = styleEqual(
    seq.int(nrow(design)),
    ifelse(params$design == seq.int(nrow(design)), "yellow", "inherit")
  ))
```


# Global Parameters
```{r}
DESIGN <- params$design
N_REP <- params$replication
INDIV <- params$indiv
NEED_PC <- DESIGN %in% which(with(design, p > n))
```

# New Functions
```{r}
rep_sim <- function(design, which, n_rep, seed=NULL) {
  rep_seed <- formatC(seq.int(n_rep), flag="0", width=2)
  seed_extra <- paste0(rep_seed, seed)
  sim <- function(design, dgn, seed=NULL) {
    dgn_seed <- formatC(dgn, flag="0", width=2)
    set.seed(as.numeric(paste0(dgn_seed, seed)))
    design %>% 
      get_design(dgn) %>% 
      simulate()
  }
  out <- lapply(seq.int(n_rep), function(r) sim(design, which, seed = seed_extra[r]))
  tibble(
    Replication = seq.int(n_rep),
    SimObj = out
  )
}
get_relpos_plot <- function(sim_obj, design){
  sobj <- sim_obj %>% filter(Design == design)
  pop_dta <- sobj %>% 
    mutate_at('SimObj', map, ggsimrelplot, which=3L, use_population = TRUE) %>% 
    mutate_at('SimObj', map, 'data') %>% 
    unnest()
  sample_dta <- sobj %>% 
    mutate_at('SimObj', map, ggsimrelplot, which=3L, use_population = FALSE) %>% 
    mutate_at('SimObj', map, 'data') %>% 
    unnest()
  
  dta <- pop_dta %>% 
    inner_join(sample_dta, by = c('Design', 'Replication', 'comp', 'response'),
               suffix = c('_population', '_sample')) %>% 
    gather(key, value, -Design:-comp, -response) %>% 
    extract(key, c('key', 'type'), "(.*)_(.*)") %>% 
    spread(key, value)
  
  plt <- ggplot(dta, aes(comp, lambda)) +
    stat_summary(geom = "bar", fun.y = mean, aes(fill = type), position= "dodge", alpha = 0.15) +
    facet_grid(response ~ .) +
    stat_summary(geom = "point", fun.y = mean, aes(y = covariance, color = type, shape = type)) +
    stat_summary(geom = "line", fun.y = mean, aes(y = covariance, color = type, linetype = type, group = type)) +
    scale_fill_brewer(palette = "Set1") +
    scale_color_brewer(palette = "Set1") +
    labs(x = "Components", y = "Absolute covariance between Components and Response / eigenvalues") +
    theme(legend.title = element_blank(),
          legend.position = c(1, 1),
          legend.justification = c(1.2, 1.2),
          legend.direction = 'horizontal',
          legend.background = element_blank(),
          legend.spacing.x = unit(0.2, 'cm'),
          text = element_text(family = "mono"))
  
  plot(plt)
}
```

# Simulate the data
```{r}
sobj <- map_df(seq.int(nrow(design)), ~rep_sim(design, .x, 5), .id = "Design")
```


# Fit differnt methods
```{r, cache=TRUE}
mthds <- c("CPPLS", "Xenv", "Senv", "PCR", "PLS1", "PLS2")
mthds <- name_it(mthds, tolower(mthds))
fit <- map_df(mthds, function(x) {
  NEED_PC <- all(NEED_PC, x %in% c("Senv", "Xenv", "Yenv"))
  rep_sim(design, DESIGN, N_REP) %>% 
    mutate(
      coef_error = map(SimObj, coef_errors, x, scale = TRUE, 
                       need_pc = NEED_PC, ncomp = 10),
      pred_err = map(coef_error, "prediction_error"))
}, .id = "Method")
```


# Extract Prediction Error from the fitted models
```{r}
dta <- fit %>% 
  unnest(pred_err) %>% 
  filter(Method %in% c("cppls", "xenv", "pls2", "senv", "pcr"))
```


# Minimum prediction Error
```{r}
conditionally <- function(fun){
  function(first_arg, ..., execute){
    if(execute) return(fun(first_arg, ...))
    else return(first_arg)
  }
}
group_by_iff <- conditionally(group_by)

dta_min <- dta %>% 
  group_by(Method, Tuning_Param) %>% 
  group_by_iff(Response, execute = INDIV, add = TRUE) %>%
  summarize(Pred_Error = mean(Pred_Error)) %>%
  ungroup() %>% 
  group_by(Method) %>% 
  group_by_iff(Response, execute = INDIV, add = TRUE) %>%
  summarize(
    Tuning_Param = Tuning_Param[which.min(Pred_Error)],
    Pred_Error = min(Pred_Error)
  ) %>% 
  mutate(label = paste0(format(Method, width = 6), 
                        ":", formatC(Pred_Error, digits = 3, format = "f"))) %>% 
  mutate(label = paste0(label, "(", Tuning_Param, ")")) %>% 
  arrange(Pred_Error)
```


# Prediction Error Plot
```{r}
v_just <- seq.int(1, by = 2, length.out = n_distinct(dta_min$Method))
if ("Response" %in% names(dta_min)) {
  v_just <- rep(v_just, each = max(dta_min$Response))
}
```


```{r}
plt <- ggplot(dta, aes(factor(Tuning_Param), Pred_Error, color = Method)) +
  geom_point(position = position_jitter(width = 0.1), alpha = 0.15, size = rel(3))
if (INDIV) {
  plt <- plt + facet_grid(Response ~ ., labeller = label_both)
} else {
  plt <- plt + stat_summary(aes(group = interaction(Method, Replication)), 
                            geom = "line", fun.y = mean, alpha = 0.3)
}
plt <- plt + stat_summary(aes(group = Method), geom = "line", fun.y = mean, size = rel(0.8)) +
  stat_summary(geom = "point", fun.y = mean)
plt <- plt + scale_x_discrete(breaks = 0:10) +
  labs(x = "Number of Components", y = "Prediction Error") +
  theme_gray(base_size = 16) +
  theme(legend.position = "bottom") +
  geom_text(x = Inf, y = Inf, aes(label = label),
            data = dta_min, show.legend = FALSE,
            vjust = v_just, size = 6,
            hjust = 1, family = "mono") +
  geom_point(data = dta_min, shape = 21, size = rel(2), fill = "white")
```


# Print the plot
```{r, fig.width=9, fig.asp=0.8}
plot(plt)
```

