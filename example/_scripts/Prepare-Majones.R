library(R.matlab)
majones <- readMat("_data/NIR_majones.mat")

majones <- with(majones, {
  out <- list(
    X = `colnames<-`(rbind(Xtrain, Xtest), paste0("x", 1:ncol(Xtrain))),
    Y = `colnames<-`(rbind(Ytrain[,-1], Ytest[,-1]), paste0("y", 2:ncol(Ytrain))),
    train = c(rep(TRUE, nrow(Xtrain)),
              rep(FALSE, nrow(Xtest)))
  )
  `attr<-`(out, "sel1", sel1)
  `attr<-`(out, "group", c(Ytrain[,1], Ytest[,1]))
})
save(majones, file = "_data/majones.RData")