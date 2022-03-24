
#This package is a wrapper of the matlab code
pametrics <- function(filename,destinationdir,epoch,doAG,doAGI,doMAD,doENMO,doSkotte,skotteAgeGroup) {
  switch(Sys.info()[['sysname']],
         Windows= {
           cmd = Sys.which("C:\\Program Files\\University of Southern Denmark\\pametrics\\application\\pametrics.exe")
           #cmd = Sys.which("C:/Users/LAB-ADMIN/Documents/MATLAB/pametrics/for_testing/pametrics.exe")

           execmd = sprintf("%s %s %s %s %s %s %s %s %s %s",cmd,filename,destinationdir,epoch,doAG,doAGI,doMAD,doENMO,doSkotte,skotteAgeGroup);
         },
         Linux  = {cmd = "";},
         Darwin = {
           #This is the application and the location of the libraries
           cmd = "/Applications/University_of_Southern_Denmark/pametrics/application/run_pametrics.sh /Applications/MATLAB/MATLAB_Runtime/v911/";
           execmd = sprintf("%s %s %s %s %s %s %s %s %s %s",cmd,filename,destinationdir,epoch,doAG,doAGI,doMAD,doENMO,doSkotte,skotteAgeGroup);

           })

    #Calling the matlab application
  system(execmd,wait = TRUE);
}

#Bulk processing of folder of cwa files
#Uses the pametrics function
generatePAmetricsFolder <- function(folder,destinationdir,epoch = 10,doAG = 1,doAGI = 1,doMAD = 0,doENMO = 0,doSkotte = 1,skotteAgeGroup = "adult") {

  #Lest check the destinationdir
  #Do we have a folder?
  if (file.exists(destinationdir)==FALSE){
    #The folder does not exist
    print("The destination folder is not found. Make sure to specify an existing folder.");
    return(0);
  }

  cwaFiles <- list.files(folder,pattern = ".cwa")


  if (length(cwaFiles)==0)
  {
    print("No files found in the source folder!");
    return(0)
  }

  myFileSep = .Platform$file.sep;
  #Fixing the windows backslah problem
  #The Matlab code requires backslash!!
  if (.Platform$OS.type=="windows") {
    #Need to fix the backslash part
    destinationdir = gsub("/", "\\\\", destinationdir)
    folder = gsub("/", "\\\\", folder)

    myFileSep = "\\"
  }

  #invisible(lapply(cwaFiles, print))
  for (file in cwaFiles) {
    filename = sprintf("%s%s%s",folder,myFileSep,file)
    print(filename)
    pametrics(filename,destinationdir,epoch,doAG,doAGI,doMAD,doENMO,doSkotte,skotteAgeGroup)
  }
}

countsSummary <- function(filename, id = "NA", cutPoints = c(-1,100,2000,5000,500000))
{

  header = AGread::AG_meta(filename,header_timestamp_format = "%d-%m-%Y %H:%M:%S")
  counts = AGread::read_AG_counts(filename, header = TRUE,header_timestamp_format = "%d-%m-%Y %H:%M:%S")

  #Use time stamp which is seconds since 1970 1 1

  sinceepoch = round(as.numeric(counts$Timestamp[1])/86400)*86400
  counts$mday = floor((as.numeric(counts$Timestamp)-sinceepoch)/86400)
  counts$timeofday = (as.numeric(counts$Timestamp)-sinceepoch)-counts$mday*86400

  #Setting nonwear as not a number
  counts[which(counts$Axis1<0),c(2,3,4)] = NA;

  #Getting the unique days
  udays = unique(counts$mday);

  daysummary = matrix(0,length(udays),9);

  for (i in udays){
    daydata = counts$Axis1[which(counts$mday==i & !is.na(counts$Axis1))] * 6;
    daysummary[i+1,1] = mean(daydata, na.rm = TRUE);
    daysummary[i+1,2] = sum(daydata, na.rm = TRUE);

    histc = hist(daydata, breaks = cutPoints,plot=FALSE);
    daysummary[i+1,c(6,7,8,9)] = (histc$counts * header$epoch)/60;

    daysummary[i+1,3] = length(daydata)
    daysummary[i+1,4] = mean(daydata[which(daydata>100)], na.rm = TRUE);

    daysummary[i+1,5] = sum(daydata[which(daydata>100)], na.rm = TRUE);

    #daysummary[i+1,10] = id;
  }

  colnames(daysummary) <- c("cpm","Total","NEpochs","cpm_pa","total_pa","Sedentary","Light","Moderate","Vigorous")

  daysummary = data.frame(daysummary);

  daysummary$ID = rep(id, nrow(daysummary));

  return (daysummary);
}


skotteSummary <- function(filename, id = "NA") {

  header = AGread::AG_meta(filename,header_timestamp_format = "%d-%m-%Y %H:%M:%S")
  skotte = AGread::read_AG_counts(filename, header = TRUE,header_timestamp_format = "%d-%m-%Y %H:%M:%S")

  sinceepoch = round(as.numeric(skotte$Timestamp[1])/86400)*86400
  skotte$mday = floor((as.numeric(skotte$Timestamp)-sinceepoch)/86400)
  skotte$timeofday = (as.numeric(skotte$Timestamp)-sinceepoch)-skotte$mday*86400

  #Setting nonwear as not a number
  skotte[which(skotte$X10<0),2:9] = NA;

  #Getting the unique days
  udays = unique(skotte$mday);

  skottesummary = matrix(0,length(udays),8);

  for (i in udays){
    daydata = skotte[which(skotte$mday==i & !is.na(skotte$X10)),];
    skottesummary[i+1,] = apply(daydata[,2:9],2,sum,na.rm=TRUE);
  }

  colnames(skottesummary) <- c("Sitting","Move","Stand","Bike","Stairs","Run","Walk","Lying")

  skottesummary = data.frame(skottesummary);
  skottesummary$ID = rep(id, nrow(skottesummary));

  return (skottesummary);
}


summaryIntensityFolder <- function(folder, intensityType = "AG") {

  #Lest check the destinationdir
  #Do we have a folder?
  if (file.exists(folder)==FALSE){
    #The folder does not exist
    print("The folder is not found!");
    return(0);
  }

  iType = switch(intensityType,"AG" = "_ag.csv","AGI" = "_agi.csv","MAD" = "_mad.csv", "ENMO" = "_enmo.csv")

  intensityFiles <- list.files(folder,pattern = iType)

  if (length(intensityFiles)==0)
  {
    print("No intensity files found in the source folder!");
    return(0)
  }

  daySummary = data.frame();
  #invisible(lapply(cwaFiles, print))
  for (file in intensityFiles) {
    filename = sprintf("%s%s%s",folder,.Platform$file.sep,file)
    if (file.exists(filename)) {
      #print(filename)

      #Lets focus on the file name as ID
      subjectID = gsub(iType,"", file)

      print(subjectID)
      if (nrow(daySummary)==0) {
        daySummary = countsSummary(filename, id = subjectID)
      } else {
        daySummary = rbind(daySummary,countsSummary(filename, id = subjectID))
      }
    }

  }

  return(daySummary)
}


summarySkotteFolder <- function(folder) {

  #Lest check the destinationdir
  #Do we have a folder?
  if (file.exists(folder)==FALSE){
    #The folder does not exist
    print("The folder is not found!");
    return(0);
  }

  iType = "_skotte.csv"

  skotteFiles <- list.files(folder,pattern = iType)

  if (length(skotteFiles)==0)
  {
    print("No intensity files found in the source folder!");
    return(0)
  }

  daySummary = data.frame();
  #invisible(lapply(cwaFiles, print))
  for (file in skotteFiles) {
    filename = sprintf("%s%s%s",folder,.Platform$file.sep,file)
    if (file.exists(filename)) {
      #print(filename)

      #Lets focus on the file name as ID
      subjectID = gsub(iType,"", file)

      print(subjectID)
      if (nrow(daySummary)==0) {
        daySummary = skotteSummary(filename, id = subjectID)
      } else {
        daySummary = rbind(daySummary,skotteSummary(filename, id = subjectID))
      }
    }

  }

  return(daySummary)
}

