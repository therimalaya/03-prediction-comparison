args <- commandArgs()
DATA <- args[1]
NCOMP <- as.numeric(args[2])
CLASSIFY <- DATA == "majones"
CLSF_FUN <- function(x) ifelse(x <= -0.5, -1, ifelse(x <= 0.5, 0, 1))
ERR_TYPE <- ifelse(CLASSIFY, "loss", "rmsep")
YCOMP <- if (!is.na(args[3])) as.numeric(args[[3]]) else 2

source("scripts/00-example-function.r")

local({
    Dname <- load(paste0("data/", DATA, ".RData"))
    assign("Dataset", get(Dname), envir = .GlobalEnv)
})
Dataset <- as.list(Dataset)
attr(Dataset, "OldName") <- names(Dataset)
names(Dataset) <- c("X", "Y", "train")

relcomp <- list(
    train = list(
        X0 = with(Dataset, scale(X[train,], scale = FALSE)),
        Y0 = with(Dataset, scale(Y[train,], scale = FALSE))
    ),
    test = list(
        X0 = with(Dataset, scale(X[!train,], scale = FALSE)),
        Y0 = with(Dataset, scale(Y[!train,], scale = FALSE))
    )
)
relcomp_df <- map_df(relcomp, ~ex_get_abs_rel_comp(.x$X0, .x$Y0), .id = "Type")

## Plot Start ----------------------------------------------
## ex_plot_relcomp(relcomp_df, ncomp = NCOMP)
## Plot End ----------------------------------------------



eigen_lst <- list(
    Train = list(
        Response = with(Dataset, eigen(cov(Y[train,]))),
        Predictor = with(Dataset, eigen(cov(X[train,])))
    ),
    Test = list(
        Response = with(Dataset, eigen(cov(Y[!train,]))),
        Predictor = with(Dataset, eigen(cov(X[!train,])))
    )
)
eigen_df <- eigen_lst %>%
    map_df(~map_df(..1, ex_make_eigen_df, .id = "xy"),
           .id = "test_train") %>%
    filter(ncomp <= NCOMP)

## Plot Start ----------------------------------------------
## ggplot(eigen_df, aes(ncomp, var_explained)) +
##     geom_line() +
##     geom_point(shape = 21, fill = "lightgrey") +
##     facet_grid(test_train ~ xy, scales = 'free_x') +
##     labs(y = "Cumulative Eigenvalues",
##          x = "Number of Components")
## Plot End ----------------------------------------------


train <- with(Dataset, {
  ex_prepare_data(X[train,], Y[train,], prop = 0.975, ncomp = NCOMP)
})

train_ <- with(Dataset, {
  data.frame(x = I(X[train,]), y = I(Y[train,]))
})

test <- with(Dataset, {
  data.frame(x = I(X[!train,]), y = I(Y[!train,]))
})

methods <- c("PCR", "PLS1", "PLS2", "Xenv", "Senv")
names(methods) <- methods

pred_error <- map_df(methods, function(mthd) {
  ## Compute Coefficient
  fit <- ex_fit_model(train$data, method = mthd, ncomp = NCOMP, scale = FALSE, 
                      u = if (mthd == "Senv") YCOMP else NULL)
  if (mthd %in% c('Xenv', 'Senv')) {
    coef <- fit %>%
      ex_get_beta_fn(tolower(mthd))(intercept = FALSE) %>%
      ex_rotate_coef(train$rotation) %>%
      ex_compute_intercept(train$centers$x, train$centers$y)
  } else {
    coef <- fit %>%
    ex_get_beta_fn(tolower(mthd))(intercept = FALSE, ncomp = NCOMP) %>%
    ex_compute_intercept(train$centers$x, train$centers$y)
  }
  
  ## Make Prediction
  train_pred <- ex_get_prediction(train_$x, coef, classify = CLASSIFY, condition = CLSF_FUN)
  test_pred <- ex_get_prediction(test$x, coef, classify = CLASSIFY, condition = CLSF_FUN)

  ## Calculate Prediction Error
  train_error <- ex_get_error(train_pred, train_$y, type = ERR_TYPE) %>%
      as_tibble(rownames = "Response") %>%
      gather(Component, Error, -Response) %>%
      mutate(Component = str_remove(Component, "Comp") %>% as.integer())

  test_error <- ex_get_error(test_pred, test$y, type = ERR_TYPE) %>%
    as_tibble(rownames = "Response") %>%
    gather(Component, Error, -Response) %>%
    mutate(Component = str_remove(Component, "Comp") %>% as.integer())

  ## Bind both training and test prediction error
  bind_rows(train = train_error, test = test_error, .id = "Type")
}, .id = "Method")


## Plot Start ----------------------------------------------
## ggplot(pred_error, aes(Component, Error, color = Type)) +
##     geom_line(aes(group = Type)) +
##     geom_point(size = rel(0.5)) +
##     facet_grid(Response ~ Method, scales = 'free') +
##     scale_x_continuous(breaks = seq(0, NCOMP, 5)) +
##     theme(legend.position = "bottom") +
##     labs(x = "Number of Components",
##          y = "Prediction Error (RMSEP)",
##          color = NULL,
##          title = "Prediction Error",
##          subtitle = "Subdivided by each prediction methods per Response")
## Plot End ----------------------------------------------


pred_error_tbl <- pred_error %>%
    filter(Type == "test") %>%
    group_by(Method, Response) %>%
    summarize(Comp = Component[which.min(Error)],
              Error = min(Error)) %>%
    group_by(Response) %>%
    mutate(Error = ifelse(
               Error == min(Error),
               paste0("**", round(Error, 2), "**"),
               round(Error, 2)))
## pred_error_tbl %>%
##     mutate(label = paste0(Error," (", Comp, ")")) %>%
##     select(-Comp, -Error) %>%
##     spread(Response, label) %>%
##     knitr::kable(booktabs = TRUE, format = 'markdown')

