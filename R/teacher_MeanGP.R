

teacher_MeanGP<-function(stud, #this is meant to be applied to a list where data is split by teacher.
                         N=15,
                         bootstrap.N=NULL,
                         wt=NULL
                      ) {
  if (is.null(bootstrap.N)) NA->tr else rep(NA,3)->tr
  if (is.null(wt)) rep(1,nrow(stud))->wt
  #mgp
  if (nrow(stud)>15) {
    weighted.mean(stud$GrowthPercentile,wt)->mgp
    if (!is.null(bootstrap.N)) {
      length(x$GrowthPercentile)->N
      boot<-numeric()
      data.frame(GrowthPercentile=x$GrowthPercentile,wt=wt)->tmp
      for (i in 1:bootstrap.n) {
        sample(1:N,N,replace=TRUE)->index
        tmp[index,]->tmp.boot
        mean(tmp.boot$GrowthPercentile,wt=wt)->boot[i]
      }
      quantile(boot,c(.025,.975))->boot.ci
      c(mgp,boot.ci)->tr
    }
  }
  tr
}
  
