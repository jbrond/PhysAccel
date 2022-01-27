
#This package is a wrapper of the matlab code
pametrics <- function(filename,destinationdir,epoch,doAG,doAGI,doMAD,doENMO,doSkotte,skotteAgeGroup) {
  switch(Sys.info()[['sysname']],
         Windows= {cmd = "";},
         Linux  = {cmd = "";},
         Darwin = {
           #This is the application and the location of the libraries
           cmd = "/Applications/University_of_Southern_Denmark/pametrics/application/run_pametrics.sh /Applications/MATLAB/MATLAB_Runtime/v99/";
           })

  execmd = sprintf("%s %s %s %s %s %s %s %s %s",cmd,filename,destinationdir,epoch,doAG,doAGI,doMAD,doENMO,doSkotte,skotteAgeGroup);
  #Calling the matlab application
  system(execmd,wait = TRUE);
}
