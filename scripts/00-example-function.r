## Prepare the data ---------
ex_prepare_data <- function(x, y, prop = 0.975, ncomp = 10) {
  ret <- new.env()
  x0 <- scale(x, scale = FALSE)
  y0 <- scale(y, scale = FALSE)
  centers <- list(
    x = attr(x0, 'scaled:center'),
    y = attr(y0, 'scaled:center'))
  ret$data <- data.frame(y = I(y0), x = I(x0))
  pc.a <- prcomp(x0)
  nc <- max(which(cumsum(pc.a$sdev/sum(pc.a$sdev)) < prop))
  if (!is.null(ncomp)) {
    if (nc < ncomp) {
      nc <- max(ncomp, nc)
    } else {
      nc <- min(ncomp, nc)
    }
  }
  ret$data$pc <- pc.a$x[, 1:nc]
  ret$rotation <- pc.a$rotation[, 1:nc]
  ret$centers <- centers
  return(ret)
}

## Fit a model with data and method supplied ----
ex_fit_model <- function(data, method = c("PCR", "PLS1", "PLS2", "Xenv", "Senv"), ncomp, scale = FALSE, ...) {
  method <- match.arg(method, method)
  list2env(list(...), environment())
  if (all(nrow(data$x) < ncol(data$x), method %in% c("Xenv", "Senv"))) {
    data$x <- data$pc
  }
  data$pc <- NULL
  if (method == "Senv" & !exists('u')) u <- 2
  fit <- switch(
    method,
    PCR   = pcr(y ~ x, data = data, ncomp = ncomp, scale = scale),
    PLS2  = plsr(y ~ x, data = data, ncomp = ncomp, scale = scale),
    Xenv  = map(1:min(ncomp, ncol(data$x)), function(nc) with(data, try(xenv(x, y, u = nc)))),
    Senv = map(1:ncomp, function(nc) with(data, try(stenv(x, y, q = nc, u = u)))),
    PLS1  = map(1:ncol(data$y), function(i){
      dta <- data.frame(y = data$y[, i], x = data$x)
      plsr(y ~ x, data = dta, scale = scale)
    })
  )
  return(fit)
}

## Compute Intercept ----
ex_compute_intercept <- function(beta_coef, x_mean, y_mean) {
  dim_coef <- dim(beta_coef)
  dim_coef[1] <- dim_coef[1] + 1
  dimnames_coef <- dimnames(beta_coef)
  dimnames_coef[[1]] <- c("(Intercept)", dimnames_coef[[1]])
  coef_int <- array(dim = dim_coef, dimnames = dimnames_coef)
  coef_int[-1, , ] <- beta_coef
  for (i in 1:dim(beta_coef)[3]) {
    coef_int[1, , i] <- y_mean - x_mean %*% beta_coef[, , i]
  }
  beta_coef <- coef_int
  return(beta_coef)
}

## get Estimated coefficients ----
ex_get_beta_fn <- function(method = c('pcr', 'pls', 'mvr', 'xenv', "senv", 'try-error', "pls1", 'pls2')){
  mthd <- match.arg(method)
  if (mthd %in% c('pcr', 'pls', 'pls2')) method <- 'mvr'
  if (mthd %in% c("senv", "stenv")) method <- "senv"
  if (mthd %in% c("try-error")) return(method)
  switch(
    method,
    mvr    = {
      coefs <- function(mdl, ncomp = 10, intercept = TRUE)
      {
        coef <- drop(coef(mdl, intercept = intercept, ncomp = 1:ncomp))
        dimnames(coef)[[3]] <- paste0("Comp", 1:dim(coef)[3])
        return(coef)
      }
    },
    pls1   = {
      coefs <- function(mdl, ncomp = 10, intercept = TRUE)
      {
        out <- sapply(mdl, coef, intercept = intercept,
                      ncomp = 1:ncomp, simplify = 'array')
        out <- unname(aperm(drop(out), c(1, 3, 2)))
        dimnames(out) <- list(
          paste0("X", 1:dim(out)[1]),
          paste0("Y", 1:dim(out)[2]),
          paste0("Comp", 1:dim(out)[3])
        )
        return(out)
      }
    },
    xenv   = {
      coefs <- function(mdl, intercept = TRUE)
      {
        out <- sapply(mdl, function(obj) {
          if (intercept) rbind(c(unname(obj$mu)), unname(obj$beta))
          else unname(obj$beta)
        }, simplify = 'array')
        dx <- dim(out)[1]
        dy <- dim(out)[2]
        dz <- dim(out)[3]
        dnx <- if (intercept) c("Intercept", paste0("X", 1:(dx - 1))) else paste0("X", 1:dx)
        dny <- paste0("Y", 1:dy)
        dnz <- paste0("Comp", 1:dz)
        dimnames(out) <- list(dnx, dny, dnz)
        return(out)
      }
    },
    senv   = {
      coefs <- function(mdl, intercept = TRUE)
      {
        out <- sapply(mdl, function(obj) {
          unname(obj$beta)
        }, simplify = 'array')
        if (intercept) {
          out <- abind::abind(sapply(mdl, "[[", "alpha"), out, along = 1)
        }
        dimnames(out) <- list(
          paste0("X", 1:dim(out)[1]),
          paste0("Y", 1:dim(out)[2]),
          paste0("Comp", 1:dim(out)[3])
        )
        return(out)
      }
    })
  return(coefs)
}

## Estimate Regression Coefficient ----
ex_rotate_coef <- function(coef_mat, rotation_mat) {
  out <- apply(coef_mat, 2:3, function(x) rotation_mat %*% x)
  dimnames(out)[[1]] <- paste0("X", 1:dim(out)[1])
  return(out)
}

## Compute Prediction Error --------
ex_get_prediction <- function(x, coef, classify = FALSE, condition = NULL) {
  predict_ <- function(x, coef) cbind(1, x) %*% coef
  if (length(dim(coef)) > 2) {
    out <- apply(coef, -c(1:2), function(cf) predict_(x, cf))
    dim(out) <- c(nrow(x), dim(coef)[2], dim(coef)[3])
  } else {
    out <- predict_(x, coef)
  }
  if (classify) {
    out <- apply(out, 1:3, condition)
  }
  return(out)
}

## Compute Prediction Error --------
ex_get_error <- function(predicted, true, type = c("msep", "rmsep", "loss")){
  type <- match.arg(type, type)
  msep <- function(x, y) colMeans((x - y)^2)
  rmsep <- function(x, y) sqrt(msep(x, y))
  loss <- function(x, y) {
    res <- sapply(1:ncol(x), function(i) {
      tbl <- table(x[,i], y[,i])
      sum(tbl[row(tbl) != col(tbl)]) / sum(tbl)
    })
    `names<-`(res, paste0("y", seq_along(res)))
  }
  fn <- switch(type, msep = msep, rmsep = rmsep, loss = loss)
  if (length(dim(predicted)) > 2) {
    out <- apply(predicted, -c(1:2), function(pred) fn(pred, true))
  } else {
    out <- fn(predicted, true)
  }
  colnames(out) <- paste0("Comp", 1:ncol(out))
  return(out)
}

## Absolute Covariance for relevant component plots ----
ex_get_abs_rel_comp <- function(x, y) {
  svd_x <- svd(x)

  egn_val <- svd_x$d^2/(nrow(x) - 1)
  egn_val_std <- egn_val/max(egn_val)

  z <- x %*% svd_x$v
  cov_yz <- cov(y, z)
  abs_cov <- t(abs(cov_yz))
  rel_comp <- abs_cov/max(abs_cov)
  rownames(rel_comp) <- 1:nrow(rel_comp)

  abs_rel_comp <- rel_comp %>%
    as_tibble(rownames = "Components") %>%
    mutate(Eigenvalues = egn_val_std) %>%
    mutate(Components = as.numeric(Components)) %>%
    gather(Response, covariance, -Components, -Eigenvalues)
}

## Capitalize function for ggplot2 facet -----
ex_capitalize <- function(string) {
  substr(string, 1, 1) <- toupper(substr(string, 1, 1))
  string
}

## Plot the relevant Components -----
ex_plot_relcomp <- function(relcomp_df, ncomp = 20, title_extra = NULL) {
  relcomp_df %>% filter(Components <= ncomp) %>%
    ggplot(aes(Components, covariance)) +
    stat_summary(aes(y = Eigenvalues), geom = "bar",
                 fun.y = mean, fill = "lightgrey") +
    geom_path(aes(color = Response, group = Response)) +
    geom_point(shape = 21, fill = "whitesmoke",
               aes(color = Response)) +
    facet_grid(rows = vars(Type), cols = vars("Estimated Relevant Components"),
               labeller = labeller(.default = ex_capitalize)) +
    labs(x = "Number of Components",
         y = "Normalized Absolute Covariance") +
    ggtitle(paste("Relevant Components and Eigenvalues", title_extra),
            bquote(
              "Line:" ~ frac(abs(widehat(Sigma)[YZ]), max(abs(widehat(Sigma)[YZ]))) ~
                "," ~ Z == X%*%V ~ "," ~ V == "Eigenvectors" ~
                "Bar:" ~ frac(lambda, max(lambda))
            )
    ) +
    theme(legend.position = c(0.75, 0.8))
}

## Create DataFrame for Eigenvalues ----
ex_make_eigen_df <- function(eigen) {
  tibble(
    ncomp = seq_along(eigen$values),
    var_explained = cumsum(eigen$values/sum(eigen$values))
  )
}
