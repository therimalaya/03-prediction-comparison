library(R.matlab)

pufa <- readMat("_data/NIR_Raman_PUFA.mat")

NIR <- with(pufa, {
  NIR_name <- unname(unlist(NIRtextdata))
  NIR_dname <- split(NIR_name, rep(c("RowName", "ColName"), dim(NIRdata)))
  Pufa_name <- unname(unlist(PUFAtextdata))
  Pufa_dname <- split(Pufa_name, rep(c("RowName", "ColName"), dim(PUFAdata)))
  list(
    NIR = `dimnames<-`(NIRdata, rev(NIR_dname)),
    Pufa = `dimnames<-`(PUFAdata, rev(Pufa_dname)),
    train = rep(c(TRUE, FALSE), c(44, 25))
  )
})
save(NIR, file = "_data/NIR-PUFA.RData")

Raman <- with(pufa, {
  Raman_name <- unname(unlist(Ramantextdata))
  Raman_dname <- split(Raman_name, rep(c("RowName", "ColName"), dim(Ramandata)))
  Pufa_name <- unname(unlist(PUFAtextdata))
  Pufa_dname <- split(Pufa_name, rep(c("RowName", "ColName"), dim(PUFAdata)))
  list(
    Raman = `dimnames<-`(Ramandata, rev(Raman_dname)),
    Pufa = `dimnames<-`(PUFAdata, rev(Pufa_dname)),
    train = rep(c(TRUE, FALSE), c(44, 25))
  )
})
save(Raman, file = "_data/Raman-PUFA.RData")