library(R.matlab)

sakacin <- readMat("_data/FTIR_FTIR_FTIR_AFLP_SakacinP.mat")

Protein <- with(sakacin, {
  Protein <- `dimnames<-`(
    FTIR.Protein,
    list(
      names.samples,
      wavelengths.FTIR.Protein
    )
  )
  Response <- `dimnames<-`(
    Responses,
    list(
      names.samples,
      names.responses
    )
  )
  train <- c(rep(TRUE, 55), rep(FALSE, 33))
  list(Protein = Protein, Response = Response, train = train)
})
save(Protein, file = "_data/Protein.RData")