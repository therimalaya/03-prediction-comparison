## Source the collection script in 03-collection.r ----
## This gives design, mthds, pred_error, est_error and all the
## required function in our environment
source("scripts/03-collection.r")

## Multivariate model with prediction error --------
pred_dta <- design_chr %>%
  select_if(function(x) n_distinct(x) > 1) %>%
  mutate(Design = as.character(1:n())) %>%
  mutate_at(vars(p, gamma, relpos, eta), as.factor) %>%
  right_join(pred_error, by = "Design") %>%
  mutate_if(is.character, as.factor) %>%
  mutate_at("p", as.factor) %>%
  mutate(Response = paste0("Y", Response))

pred_dta_min <- pred_dta %>%
  group_by(p, gamma, eta, relpos, Replication, Method, Response) %>%
  summarize(Pred_Error = min(Pred_Error)) %>%
  spread(Response, Pred_Error)

pred_mdl <- lm(cbind(Y1, Y2, Y3, Y4) ~ p * gamma * eta * relpos * Method,
               data = pred_dta_min)

## pred_aov <- map(paste0("Y", 1:4), function(y){
##     dta <- pred_dta_min %>%
##         select_at(y) %>%
##         rename_at(y, str_remove, "[0-9]")
##     mdl <- lm(Y ~ p * gamma * eta * relpos * Method, data = dta)
##     return(mdl)
## })
## pred_aov_anova <- lapply(pred_aov, anova)



## anova(pred_mdl)
## pred_eff_plot <- eff_plot("gamma:eta:Method", pred_mdl)
## Multivariate model with estimation error
est_dta <- design_chr %>% mutate(Design = as.character(1:n())) %>%
  mutate_at(vars(p, gamma, eta, relpos), as.factor) %>%
  right_join(est_error, by = "Design") %>%
  mutate_if(is.character, as.factor) %>%
  mutate_at("p", as.factor) %>%
  mutate(Response = paste0("Y", Response))

est_dta_min <- est_dta %>%
  group_by(p, gamma, eta, relpos, Replication, Method, Response) %>%
  summarize(Est_Error = min(Est_Error)) %>%
  spread(Response, Est_Error)

est_mdl <- lm(cbind(Y1, Y2, Y3, Y4) ~ p * gamma * eta * relpos * Method,
              data = est_dta_min)

## est_aov <- map(paste0("Y", 1:3), function(y){
##     dta <- est_dta_min %>%
##         select_at(y) %>%
##         rename_at(y, str_remove, "[0-9]")
##     mdl <- lm(Y ~ p * gamma * eta * R2 * Method, data = dta)
##     return(mdl)
## })
## est_aov_anova <- lapply(est_aov, anova)

## anova(est_mdl)
## est_eff_plot <- eff_plot("gamma:eta:Method", est_mdl,
##                          title = "Estimation Error")
## gridExtra::grid.arrange(pred_eff_plot, est_eff_plot)
## p1 <- eff_plot("eta:gamma:R2:Method", est_mdl, title = "Estimation Error")
## p2 <- eff_plot("eta:gamma:R2:Method", pred_mdl, title = "Prediction Error")
## gridExtra::grid.arrange(p1, p2, ncol = 1)

## ---- PLS Model and some Plots ----

pls_mdl <- plsr(formula(pred_mdl), data = pred_dta_min, scale = TRUE,
                validation = "CV", segments = 10)

## validationplot(pls_mdl, val.type = "RMSEP", estimate = "all", ncomp = 1:25)

plot_score <- function(pls_mdl, x_var, y_var, col_var, facet_x, facet_y) {
    x_var <- enquo(x_var)
    y_var <- enquo(y_var)
    col_var <- enquo(col_var)
    facet_x <- enquo(facet_x)
    facet_y <- enquo(facet_y)
    expl_var <- explvar(pls_mdl) %>% round(3)
    x_lab <- paste(quo_name(x_var), paste0("(", expl_var[quo_name(x_var)], "%)"))
    y_lab <- paste(quo_name(y_var), paste0("(", expl_var[quo_name(y_var)], "%)"))
    mdl_mat <- model.matrix(pls_mdl)
    mdl_vars <- apply(mdl_mat, 1, function(x) names(which.max(x))) %>% unname()

    df <- scores(pls_mdl)[] %>%
        as_tibble() %>%
        select(!!x_var, !!y_var) %>%
        bind_cols(model_var = mdl_vars)

    plt <- df %>%
        group_by(!!col_var, !!facet_x, !!facet_y) %>%
        summarize_at(vars(!!x_var, !!y_var), mean) %>%
        ggplot(aes(!!x_var, !!y_var, color = !!col_var)) +
        ggrepel::geom_text_repel(aes(label = !!col_var)) +
        geom_hline(yintercept = 0, color = "grey70", linetype = "dashed") +
        geom_vline(xintercept = 0, color = "grey70", linetype = "dashed")
    plt <- plt + facet_grid(rows = vars(!!facet_y), cols = vars(!!facet_x))
    plt <- plt + labs(x = x_lab, y = y_lab)
    return(plt)
}

## plot_score(pls_mdl, `Comp 1`, `Comp 2`, Method, eta, gamma)

## ---- Saving the plots

plt <- eff_df("relpos:p:eta:gamma:Method", pred_mdl) %>% 
  eff_plot3(reorder = TRUE, labeller = label_both)

ggsave(plt, filename = "scripts/plots/Effect-Plot-color-relpos.pdf", device = "pdf",
       width = 15, height = 10, scale = 0.8)
