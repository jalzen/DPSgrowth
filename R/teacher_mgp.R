

teacher_mgp<-function(stud, #student data
                      outcome.grade,
                      outcome.year
                      ) {
  stud[stud$GRADE==outcome.grade & stud$OutcomeYear==outcome.year,]->tmp
  #mgp
  aggregate(tmp$SGP,list(tmp$TeacherID),mean,na.rm=TRUE)->mgp
  names(mgp)<-c("TeacherID","mgp")
  mgp
}
