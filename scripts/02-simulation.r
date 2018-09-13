## Command line input for task to run ----
tsk <- commandArgs(trailingOnly = TRUE)[1]

## Source the 01-setup script ----
source("scripts/01-setup.r")

## Setup task grid for all possible simulation ----
## Total simulation: n_method x design
tsk_grid <- expand.grid(dgn = 1:nrow(design), mthd = mthds)
mthd  <- as.character(tsk_grid[tsk, 'mthd'])
dgn   <- tsk_grid[tsk, 'dgn']
mthd_idx <- which(mthd == mthds)
wide_idx <- design %>%
    rownames_to_column("Design") %>%
    filter(p > n) %>%
    select(Design) %>%
    unlist() %>%
    unname() %>%
    as.integer()

## Each simulation is replicated 50 times ----
## This gives 50 different dataset with same properties
## The randomness is controlled by the set.seed function
## The seed is constructed as below
## For Method: "01: PCR"   "02: PLS1"  "03: PLS2"  "04: Xenv"  "05: Senv"  "06: Ridge" "07: Lasso"
## For Design: 01-32
## For Replication: 01-50
## The final seed is obtained by joining design, method and replication seeds.
## For Example: For PLS1, design 5 and replication 25 the seed will be 050225
## Here 05 for design, 02 fo rPLS1 and 25 for replication
## The object out is a list of 50. Each of these 50 elements is a list of
## three tables: prediction_error, estimation_error and regression coefficients
out <- map(1:50, function(r) {
  dgn_chr  <- formatC(dgn, digits = 0, width = 2, format = "f", flag = "0")
  mthd_chr <- formatC(mthd_idx, digits = 0, width = 2, format = "f", flag = "0")
  rep_chr  <- formatC(r, digits = 0, width = 2, format = "f", flag = "0")
  seed     <- as.numeric(paste0(dgn_chr, mthd_chr, rep_chr))
  use_pc <- all(mthd %in% c("Xenv", "Yenv", "Senv"), dgn %in% wide_idx)
  set.seed(seed)
  res <- design %>%
    get_design(dgn) %>%
    simulate() %>%
    coef_errors(mthd, need_pc = use_pc, prop = 0.975, ncomp = 10, scale = TRUE)
  class(res) <- append(class(res), "coefficient_error_list")
  return(res)
})

## Save Simulated Objects ----
## A file is created with 50 replicated design for each design and method combination
## The filename is prefixed as dgn-. For example:
## File dgn-2-pcr.Rdata contains 50 replicated simulation objects for design1 and PCR method.
fname <- paste0("scripts/robj/coef-error/", "dgn-", dgn, "-", tolower(mthd), ".Rdata")
save(out, file = fname)

