if (!all(file.exists('app/robj/min-pred-data.rds',
                    'app/robj/min-pred-comp-data.rds'))) {
  pred_error <- readRDS('scripts/robj/prediction-error.rds')
  pred_dta <- design_chr %>%
    select_if(function(x) n_distinct(x) > 1) %>%
    mutate(Design = as.character(1:n())) %>%
    mutate_at(vars(p, gamma, relpos, eta), as.factor) %>%
    right_join(pred_error, by = "Design") %>%
    mutate_if(is.character, as.factor) %>%
    mutate_at("p", as.factor) %>%
    mutate(Response = paste0("Y", Response))
  pred_spread_df <- pred_dta %>%
    as.data.frame() %>%
    select(-Design, -q) %>%
    spread(Response, Pred_Error)
  min_comp_stk <- pred_dta %>%
    group_by(p, relpos, eta, gamma, Method, Tuning_Param, Response) %>%
    summarize(Pred_Error = mean(Pred_Error)) %>%
    group_by(p, relpos, eta, gamma, Method, Response) %>%
    summarize(Tuning_Param = Tuning_Param[which.min(Pred_Error)])
  pred_min <- pred_dta %>% 
    select(-Design, -q) %>% 
    semi_join(min_comp_stk, by = c(
      "p", "relpos", "eta", "gamma", "Method", 
      "Tuning_Param", "Response"
    )) %>% select(-Tuning_Param) %>% 
    spread(Response, Pred_Error)
  comp_min <- pred_dta %>% 
    group_by(p, relpos, eta, gamma, Method, Replication, Response) %>% 
    summarize(Tuning_Param = Tuning_Param[which.min(Pred_Error)]) %>% 
    spread(Response, Tuning_Param)
  saveRDS(pred_min, file = 'app/robj/min-pred-data.rds')
  saveRDS(comp_min, file = 'app/robj/min-pred-comp-data.rds')  
} else {
  pred_min <- readRDS('app/robj/min-pred-data.rds')
  pred_comp_min <- readRDS('app/robj/min-pred-comp-data.rds')
}

if (!all(file.exists('app/robj/min-est-data.rds',
                    'app/robj/min-est-comp-data.rds'))) {
  est_error <- readRDS('scripts/robj/estimation-error.rds')
  est_dta <- design_chr %>%
    select_if(function(x) n_distinct(x) > 1) %>%
    mutate(Design = as.character(1:n())) %>%
    mutate_at(vars(p, gamma, relpos, eta), as.factor) %>%
    right_join(est_error, by = "Design") %>%
    mutate_if(is.character, as.factor) %>%
    mutate_at("p", as.factor) %>%
    mutate(Response = paste0("Y", Response))
  est_spread_df <- est_dta %>%
    as.data.frame() %>%
    select(-Design, -q) %>%
    spread(Response, Est_Error)
  est_min_comp_stk <- est_dta %>%
    group_by(p, relpos, eta, gamma, Method, Tuning_Param, Response) %>%
    summarize(Est_Error = mean(Est_Error)) %>%
    group_by(p, relpos, eta, gamma, Method, Response) %>%
    summarize(Tuning_Param = Tuning_Param[which.min(Est_Error)])
  est_min <- est_dta %>% 
    select(-Design, -q) %>% 
    semi_join(min_comp_stk, by = c(
      "p", "relpos", "eta", "gamma", "Method", 
      "Tuning_Param", "Response"
    )) %>% select(-Tuning_Param) %>% 
    spread(Response, Est_Error)
  est_comp_min <- est_dta %>% 
    group_by(p, relpos, eta, gamma, Method, Replication, Response) %>% 
    summarize(Tuning_Param = Tuning_Param[which.min(Est_Error)]) %>% 
    spread(Response, Tuning_Param)
  saveRDS(est_min, file = 'app/robj/min-est-data.rds')
  saveRDS(est_comp_min, file = 'app/robj/min-est-comp-data.rds')
} else {
  est_min <- readRDS('app/robj/min-est-data.rds')
  est_comp_min <- readRDS('app/robj/min-est-comp-data.rds')
}
