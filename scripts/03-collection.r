## Source the basic setup script ----
source("scripts/01-setup.r")

## Prepare prediction error object ----
## Here the computation is executated only if the prediction-error
## object is missing. The prediction erorr is extracted from the robj
## created by 02-simulation.r script. Here all prediction error
## for 50 replication are bind together
if (file.exists('scripts/robj/prediction-error.rds')) {
  pred_error <- readRDS('scripts/robj/prediction-error.rds')
} else {
  pred_error <- map_df(1:nrow(design), function(dgn) {
    map_df(mthds, function(mthd) {
      fname <- paste0("scripts/robj/coef-error/dgn-", dgn, "-", tolower(mthd), ".Rdata")
      load(fname)
      map_df(out, "prediction_error", .id = "Replication")
    }, .id = "Method")
  }, .id = "Design")
  saveRDS(pred_error, file = "scripts/robj/prediction-error.rds")
}

## Prepare estimation error object ----
## Here the computation is executated only if the estimation-error
## object is missing. The estimation erorr is extracted from the robj
## created by 02-simulation.r script. Here all estimation error
## for 50 replication are bind together
if (file.exists('scripts/robj/estimation-error.rds')) {
  est_error <- readRDS('scripts/robj/estimation-error.rds')
} else {
  est_error <- map_df(1:nrow(design), function(dgn) {
    map_df(mthds, function(mthd) {
      fname <- paste0("scripts/robj/coef-error/dgn-", dgn, "-", tolower(mthd), ".Rdata")
      load(fname)
      map_df(out, "estimation_error", .id = "Replication")
    }, .id = "Method")
  }, .id = "Design")
  saveRDS(est_error, file = "scripts/robj/estimation-error.rds")
}



