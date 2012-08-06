dps_init<-function() {
  #this function basically just sets the working directory to the DPS_VAM data directory and other things.
  Sys.getenv("USER")->user
  switch(user,
         bd={
           setwd("~/Dropbox/DPS_VAM/project/data")
         },
         domingue={
           setwd("~/dps/data")
         },
         derekbriggs={
           setwd("Dropbox/DPS VAM/project/data")
         },
         briggsd={
           setwd("Dropbox/DPS VAM/project/data")
         })
}
           
