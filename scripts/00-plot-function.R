coef_plot <- function(method = "Senv", design = 9, replication = 1:10, ncomp = 1:5) {
  require(tidyverse)

  METHOD <- method
  DESIGN <- design
  REPLICATE <- replication
  NCOMP <- ncomp
  PATH <- "scripts/robj/coef-error/"
  FNAME <- paste0(PATH, "dgn-", DESIGN, "-", tolower(METHOD), ".Rdata")
  DGN <- "scripts/robj/design.rdata"

  load(FNAME)
  load(DGN)

  dgn_lbl <- design_chr %>%
    slice(DESIGN) %>%
    select(p, eta, gamma, R2) %>%
    pmap_chr(function(p, eta, gamma, R2) paste0("p:", p, " eta:", eta, " gamma:", gamma, " R2:", R2))

  dta <- out[REPLICATE] %>%
    map_df("coefficients", .id = "Replication") %>%
    gather(Components, Estimated, -c(1:3, ncol(.))) %>%
    gather(Coef_Type, Coef, True, Estimated) %>%
    mutate_at(vars(Predictor, Components),
              ~gsub("[()A-Za-z]+", "", .x)) %>%
    mutate_at("Predictor", ~ifelse(.x == "", 0, .x)) %>%
    mutate_at(vars(Predictor, Components), as.numeric) %>%
    group_by(Predictor, Response, Components, Coef_Type) %>%
    summarise(Coef = mean(Coef))

  plt <- dta %>%
    filter(Components %in% NCOMP) %>%
    ggplot(aes(Predictor, Coef)) +
    geom_line(aes(group = Coef_Type,
                  linetype = Coef_Type,
                  color = Coef_Type)) +
    facet_grid(Response ~ Components,
               labeller = labeller(Components = label_both)) +
    labs(y = "Coefficients") +
    theme(legend.title = element_blank(),
          legend.position = "bottom",
          plot.subtitle = element_text(family = "mono")) +
    ggtitle(paste0("Method:", METHOD, ", ", "Design:", DESIGN,
                   ", Replicate:", deparse(REPLICATE)),
            dgn_lbl)

  if (max(dta$Predictor) < 50) {
    plt <- plt + geom_point(shape = 21, size = 1, stroke = 0.2,
               aes(fill = Coef_Type))
  }

  return(plt)
}
err_plot <- function(method = "Senv", design = 1, error_df = pred_error) {
  require(tidyverse)

  METHOD = method
  DESIGN = design
  dta <- error_df
  dta <- dta %>% mutate(Response = paste0("Y", Response))
  names(dta)[4] <- "Error"
  error_type <- if (deparse(substitute(error_df)) == "pred_error")
    "Prediction" else "Estimation"

  load("scripts/robj/design.rdata")
  
  
  dgn_lbl <- design_chr %>%
    slice(DESIGN) %>%
    select(p, eta, gamma, R2) %>%
    pmap_chr(function(p, eta, gamma, R2) paste0("p:", p, " eta:", eta, " gamma:", gamma, " R2:", R2))

  lbl <- dta %>%
    filter(Design == DESIGN, Method == METHOD) %>%
    group_by(Response, Tuning_Param) %>%
    summarize_at(4, mean) %>%
    group_by(Response) %>%
    summarize(Tuning_Param = Tuning_Param[which.min(Error)],
              Error = min(Error)) %>%
    mutate(label = paste0(Response, " = ", round(Error, 3), " (", Tuning_Param, ")")) %>%
    arrange(Error)

  plt <- dta %>%
    filter(Design == DESIGN, Method == METHOD) %>%
    ggplot(aes(as.factor(as.integer(Tuning_Param)), Error, fill = Response)) +
    geom_boxplot(alpha = 0.2, color = "grey70", size = 0.3) +
    geom_point(aes(color = Response),
               position = position_jitterdodge(jitter.width = 0.2, jitter.height = 0.2),
               alpha = 0.1) +
    stat_summary(fun.y = mean, geom = "line", size = 1,
                 aes(group = Response, color = Response)) +
    stat_summary(fun.y = mean, geom = "point",  size = 2, shape = 21,
                 aes(group = Response, fill = Response)) +
    labs(x = "Number of Components", y = paste(error_type, "Error"),
         color = "Response", fill = "Response") +
    geom_text(data = lbl, aes(label = label, color = Response),
              x = Inf, y = Inf, hjust = 1, vjust = seq(2, 8, 2),
              family = "mono") +
    ggtitle(paste0(paste(error_type, "Error Plot:: "), "Method:", METHOD, " Design: ", DESIGN),
            subtitle = dgn_lbl) +
    theme_light() +
    theme(plot.subtitle = element_text(family = "mono"))

  return(plt)
}
plot_all <- function(method = "Senv", design = 21) {
  METHOD <- method
  DESIGN <- design

  pred_error <- readRDS("scripts/robj/prediction-error.rds")
  est_error <- readRDS("scripts/robj/estimation-error.rds")

  thm <- theme(legend.position = "bottom")
  p1 <- err_plot(METHOD, DESIGN, est_error) + thm
  p2 <- err_plot(METHOD, DESIGN, pred_error) + thm
  p3 <- coef_plot(METHOD, DESIGN) + thm

  plt <- gridExtra::arrangeGrob(p1, p2, p3,
                                layout_matrix = matrix(c(1, 2, 3, 3), nrow = 2, byrow = TRUE)
  )
  return(plt)
}
