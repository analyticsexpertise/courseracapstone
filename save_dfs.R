

#### Save data frames to RDS files

saveopdfs <- function(){
  
  save(biz_sample, file = "biz_sample.RData")
  
  save(fitdf, file = "fitdf.RData")
  
  save(revassocdf, file = "revassocdf.RData")
  
  save(bigcasinodf, file = "bigcasinodf.RData")
  
}

savedfs <- function(){
  
  save(ambdf, file = "ambdf.RData")
  
  save(ambvals, file = "ambvals.RData")
  
  save(assocdf, file = "assocdf.RData")
  
  save(attdf, file = "attdf.RData")
  
  save(attdistdf, file = "attdistdf.RData")
  
  save(bigcasinodf, file = "bigcasinodf.RData")
  
  save(biz_sample, file = "biz_sample.RData")
  
  save(bizatt, file = "bizatt.RData")
  
  save(bizcat, file = "bizcat.RData")
  
  save(cchkindf, file = "cchkindf.RData")
  
  save(crevdf, file = "crevdf.RData")
  
  save(ctipdf, file = "ctipdf.RData")
  
  save(cuserdf, file = "cuserdf.RData")
  
  save(fitdf, file = "fitdf.RData")
  
  save(revassocdf, file = "revassocdf.RData")
  
}