teacher_MeanGP<-function(stud, #this is meant to be applied to a list where data is split by teacher.
                         N=15,
                         bootstrap.N=NULL,
                         wt=NULL,
                         sgp.col.name="GrowthPercentile"
                      ) {
  if (is.null(bootstrap.N)) NA->tr else rep(NA,5)->tr
  if (is.null(wt)) rep(1,nrow(stud))->wt
  stud[[sgp.col.name]]->stud$GrowthPercentile
  #mgp
  stud[!is.na(stud$GrowthPercentile),]->stud
  if (nrow(stud)>=N) {
    weighted.mean(stud$GrowthPercentile,wt)->mgp
    if (!is.null(bootstrap.N)) {
      length(stud$GrowthPercentile)->N
      boot<-numeric()
      data.frame(GrowthPercentile=stud$GrowthPercentile,wt=wt)->tmp
      for (i in 1:bootstrap.N) {
        sample(1:N,N,replace=TRUE)->index
        tmp[index,]->tmp.boot
        if (all(wt==1)) mean(tmp.boot$GrowthPercentile)->boot[i] else weighted.mean(tmp.boot$GrowthPercentile,wt=wt)->boot[i]
      }
      quantile(boot,c(.025,.975,.16,.84))->boot.ci
      c(mgp,boot.ci)->tr
    } else mgp->tr
  }
  tr
}

