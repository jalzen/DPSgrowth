
teacher_covars<-function(stud, #student level data
                         outcome.grade,
                         outcome.year,
                         churn #churn data
                         ) {
  #get rid of bad data
  stud[!is.na(stud$TeacherID),]->stud
  #
  covars<-list()
  stud[stud$OutcomeYear==outcome.year & stud$GRADE==outcome.grade,]->x
  split(x,x$TeacherID)->classes
  names(classes)->tch.ids
  ## 1) average of students' prior grade achievement in same subject
  prior.score.mean<-function(x,prior) {
    x$ScaleScore.prior->prior
    mean(prior,na.rm=TRUE)
  }
  sapply(classes,prior.score.mean)->prior.scores
  data.frame(TeacherID=tch.ids,prior.mean=prior.scores)->covars$prior.mean
  ## 2) FRL% (which I would really like to replace with a much better SES indicator as noted in my writeup)
  frl.mean<-function(x) {
    x$FRL->frl
    ifelse(frl==3,0,1)->frl
    mean(frl,na.rm=TRUE)
  }
  sapply(classes,frl.mean)->frl
  data.frame(TeacherID=tch.ids,frl=frl)->covars$frl
  ## 3) SPED% and/or SCD% [not the same thing]
  iep.mean<-function(x) {
    x$ON_IEP==1 & x$GT==0->iep
    mean(iep,na.rm=TRUE)
  }
  sapply(classes,iep.mean)->iep
  data.frame(TeacherID=tch.ids,iep=iep)->covars$iep
  ## 4) ELL%
  ell.mean<-function(x) {
    x$ELL->ell
    mean(ell,na.rm=TRUE)
  }
  sapply(classes,ell.mean)->ell
  data.frame(TeacherID=tch.ids,ell=ell)->covars$ell
  ## 5) Gifted and Talented %
  gt.mean<-function(x) {
    x$GT->gt
    ifelse(gt %in% 1:3,1,0)->gt
    mean(gt,na.rm=TRUE)
  }
  sapply(classes,gt.mean)->gt
  data.frame(TeacherID=tch.ids,gt=gt)->covars$gt
  ## 6) class size
  class.size<-function(x) nrow(x)
  sapply(classes,class.size)->class.size
  data.frame(TeacherID=tch.ids,class.size=class.size)->covars$class.size
  ## 7) "churn rate" of students in class [defined as 1 - proportion of students associated with a teacher in a year that were in the class the entire year]
  #dat[dat$OutcomeYear==outcome.year & dat$GRADE==outcome.grade,]->tmp
  subset(x,select=c("TeacherID","StudentID"))->tmp
  churn[churn$OutcomeYear==outcome.year,]->churn
  merge(tmp,churn,by.x="StudentID",by.y="studentNumber")->churn
  churn[!duplicated(churn),]->churn
  aggregate(churn$stable.student,list(churn$TeacherID),function(x) sum(x,na.rm=TRUE)/sum(!is.na(x)))->churn
  names(churn)<-c("TeacherID","churn")
  1-churn$churn->churn$churn
  churn->covars$churn
  ## 8) Novice teacher indicator (<4 years experience)
  novice<-function(x) {
    x$TOTAL_TEACHING_EXPERIENCE->tmp
    if (all(is.na(tmp))) NA else {
      max(tmp,na.rm=TRUE)->tmp
      ifelse(tmp>4,0,1)
    }
  }
  sapply(classes,novice)->novice
  novice[!is.finite(novice)]<-NA
  data.frame(TeacherID=tch.ids,novice=novice)->covars$novice
  #
  list.merge<-function(L,by="Group.1") {
    L[[1]]->out
    for (i in 2:length(L)) merge(out,L[[i]],by=by,all=TRUE)->out
    out
  }
  list.merge(covars,by="TeacherID")->covars
  covars
}
