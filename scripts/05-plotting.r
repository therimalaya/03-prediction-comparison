source("04-analysis.r")

## ---- Effect Plots ----
effect_label <- function(mdl, interaction = 0) {
    eff_lbl <- attr(terms(mdl), "term.labels")
    colon_count <- stringr::str_count(eff_lbl, ":")
    selected_eff <- eff_lbl[colon_count == interaction]
    return(selected_eff)
}
ggsave2 <- function(plot_list, filename, width, height) {
  pdf(file = filename, width = width , height = height, onefile = TRUE)
  invisible(lapply(plot_list, print))
  dev.off()
}

## Prediction Model -----------
eff_idx <- 1:5
pred_plot_lbl <- map(eff_idx - 1, ~effect_label(pred_mdl, .x))
names(pred_plot_lbl) <- paste0("pred_eff", eff_idx)

walk(eff_idx, function(idx){
  lbl <- pred_plot_lbl[[paste0('pred_eff', idx)]]
  plt1 <- map(lbl, eff_plot, pred_mdl)
  plt2 <- map(lbl, eff_plot2, pred_mdl)
  ggsave2(plt1, paste0("plots/prediction-model/separated/eff-", idx, ".pdf"),
          width = round(7 * (idx)^(3/5)), height = round(5 * (idx)^(1/3)))
  ggsave2(plt2, paste0("plots/prediction-model/averaged/eff-", idx, ".pdf"),
          width = round(7 * (idx)^(3/5)), height = round(5 * (idx)^(1/3)))
})


## Estimation Model -----------
eff_idx <- 1:5
est_plot_lbl <- map(eff_idx - 1, ~effect_label(est_mdl, .x))
names(est_plot_lbl) <- paste0("est_eff", eff_idx)

walk(eff_idx, function(idx){
  lbl <- est_plot_lbl[[paste0('est_eff', idx)]]
  plt1 <- map(lbl, eff_plot, est_mdl)
  plt2 <- map(lbl, eff_plot2, est_mdl)
  ggsave2(plt1, paste0("plots/estimation-model/separated/eff-", idx, ".pdf"),
          width = round(7 * (idx)^(3/5)), height = round(5 * (idx)^(1/3)))
  ggsave2(plt2, paste0("plots/estimation-model/averaged/eff-", idx, ".pdf"),
          width = round(7 * (idx)^(3/5)), height = round(5 * (idx)^(1/3)))
})
