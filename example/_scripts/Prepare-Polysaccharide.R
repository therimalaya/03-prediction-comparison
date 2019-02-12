library(R.matlab)
sakacin <- readMat("_data/FTIR_FTIR_FTIR_AFLP_SakacinP.mat")

FTIR <- with(sakacin, {
  FTIR <- `dimnames<-`(
    FTIR.Polysaccharide,
    list(
      names.samples,
      wavelengths.FTIR.Polysaccharide
    )
  )
  Response <- `dimnames<-`(
    Responses,
    list(
      names.samples,
      names.responses
    )
  )
  train <- c(rep(TRUE, 50), rep(FALSE, 38))
  list(FTIR, Response, train)
})
save(FTIR, file = "_data/FTIR.RData")

## DID NOT WORK ON Xenv and Senv