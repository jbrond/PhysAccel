
#'This function is a wrapper of the matlab code
#'
#' \code{pametrics} generates Physical activity metrics from raw acceleration cwa file. The
#' function calls an external Matlab software to generate the files.
#'
#'
#' @param filename Filename of the cwa file
#' @param destinationdir The destination folder for which alle the PA metrics files are stored
#' @param epoch The epoch legnth used to generate the metrics. Skotte is always 1 second
#' @param doAG Should ActiGraph counts metrics be generated (0:no, 1:yes)
#' @param doAGI Should AGi counts metrics be generated (0:no, 1:yes)
#' @param doMAD Should MAD counts metrics be generated (0:no, 1:yes)
#' @param doENMO Should ENMO counts metrics be generated (0:no, 1:yes)
#' @param doSkotte Should Skotte activity type be generated (0:no, 1:yes). Requires thigh acceleration
#' @param skotteAgeGroup Age group for the classification of skotte (adult is default)
#' @export
#' @seealso \code{\link{generatePAmetricsFolder}}
#'
pametrics <- function(filename,destinationdir,epoch,doAG,doAGI,doMAD,doENMO,doSkotte,skotteAgeGroup) {
  #UseMethod("pametrics")
  switch(Sys.info()[['sysname']],
         Windows= {
           winpgf = Sys.getenv("PROGRAMFILES")
           if (nchar(winpgf)==0)
            cmd = Sys.which("\"C:\\Program Files\\University of Southern Denmark\\pametrics\\application\\pametrics.exe\"")
           else
            cmd = paste("\"",winpgf,"\\University of Southern Denmark\\pametrics\\application\\pametrics.exe\"",sep="")

           print(cmd)
           #cmd = Sys.which("C:/Users/LAB-ADMIN/Documents/MATLAB/pametrics/for_testing/pametrics.exe")

           execmd = sprintf("%s %s %s %s %s %s %s %s %s %s",cmd,filename,destinationdir,epoch,doAG,doAGI,doMAD,doENMO,doSkotte,skotteAgeGroup);
           print(execmd)
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


#'Bulk processing a folder of cwa files
#'
#' \code{generatePAmetricsFolder} Bulk processing of folder of cwa files
#'
#'
#' @param folder Folder which contains the cwa file
#' @param destinationdir The destination folder for which alle the PA metrics files are stored
#' @param epoch The epoch legnth used to generate the metrics. Skotte is always 1 second
#' @param doAG Should ActiGraph counts metrics be generated (0:no, 1:yes)
#' @param doAGI Should AGi counts metrics be generated (0:no, 1:yes)
#' @param doMAD Should MAD counts metrics be generated (0:no, 1:yes)
#' @param doENMO Should ENMO counts metrics be generated (0:no, 1:yes)
#' @param doSkotte Should Skotte activity type be generated (0:no, 1:yes). Requires thigh acceleration
#' @param skotteAgeGroup Age group for the classification of skotte (adult is default)
#' @export
#' @seealso \code{\link{pametrics}}
generatePAmetricsFolder <- function(folder,destinationdir,epoch = 10,doAG = 1,doAGI = 1,doMAD = 0,doENMO = 0,doSkotte = 1,skotteAgeGroup = "adult") {

  #UseMethod("generatePAmetricsFolder")
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

#'Summarizing the physical activity intensity for one file
#'
#' \code{intensitySummary} Estimating the sintensity ummary measure from physical activity csv files.
#'
#'
#' @param filename Filename of the csv file
#' @param id The id of the subject
#' @param curPoints The cut points used for estimating time spent in defined intensity domains
#' @return summary dataframe
#' @export
#' @seealso \code{\link{skotteSummary}}
intensitySummary <- function(filename, id = "NA", cutPoints = c(-1,100,2000,5000,500000))
{

  #header = AGread::AG_meta(filename,header_timestamp_format = "%d-%m-%Y %H:%M:%S")
  #counts = AGread::read_AG_counts(filename, header = TRUE,header_timestamp_format = "%d-%m-%Y %H:%M:%S")
  header = AG_meta(filename,header_timestamp_format = "%d-%m-%Y %H:%M:%S")
  counts = read_AG_counts(filename, header = TRUE,header_timestamp_format = "%d-%m-%Y %H:%M:%S")

  #Use time stamp which is seconds since 1970 1 1

  sinceepoch = round(as.numeric(counts$Timestamp[1])/86400)*86400
  counts$mday = floor((as.numeric(counts$Timestamp)-sinceepoch)/86400)
  counts$timeofday = (as.numeric(counts$Timestamp)-sinceepoch)-counts$mday*86400

  print(length(which(counts$Axis1<0)))
  #Setting nonwear as not a number
  counts[which(counts$Axis1<0),c(2,3,4)] = NA;

  toCpm = 60 / header$epoch;
  #Getting the unique days
  udays = unique(counts$mday);

  daysummary = matrix(0,length(udays),15);

  for (i in udays){
    #daycounts = counts[which(counts$mday==i & !is.na(counts$Axis1)),];

    daycounts = counts[which(counts$mday==i),];

    daydata = daycounts$Axis1 * toCpm
    daysummary[i+1,1] = i;
    daysummary[i+1,2] = sinceepoch;
    daysummary[i+1,3] = as.POSIXlt(daycounts$Timestamp[1],origin="1970-01-01",tz = "GMT")$wday;
    daysummary[i+1,4] = as.numeric(daysummary[i+1,3]>=6 | daysummary[i+1,3]<1)
    daysummary[i+1,5] = mean(daydata, na.rm = TRUE);
    daysummary[i+1,6] = sum(daydata, na.rm = TRUE);

    daysummary[i+1,7] = length(daydata)*header$epoch

    daysummary[i+1,8] = mean(daydata[which(daydata>100)], na.rm = TRUE);
    daysummary[i+1,9] = sum(daydata[which(daydata>100)], na.rm = TRUE);

    daysummary[i+1,10] = length(which(daydata>100))*header$epoch;

    daysummary[i+1,11] = length(which(is.na(daydata)==TRUE))*header$epoch

    #Adding the cutpoints
    lastEntry = 12;
    nhist = lastEntry+length(cutPoints)-2;

    histc = hist(daydata, breaks = cutPoints,plot=FALSE);
    daysummary[i+1,lastEntry:nhist] = (histc$counts * header$epoch)/60;
  }

  colnames(daysummary) <- c("MDay","StartDate","Weekday","DayType","cpm","Total","Duration","cpm_pa","total_pa","Duration_pa","Nonwear","Sedentary","Light","Moderate","Vigorous")

  daysummary = data.frame(daysummary);

  #Attaching the ID
  daysummary$ID = rep(id, nrow(daysummary));

  daysummary = daysummary[!(daysummary$StartDate==0),]
  daysummary$StartDate = as.POSIXct(daysummary$StartDate, origin = "1970-01-01", tz = "GMT")

  return (daysummary);
}

#'Summarizing the physical activity type for one file day by day
#'
#' \code{skotteSummary} Estimating the activity type summary measure day by day from physical activity csv files.
#'
#'
#' @param filename Filename of the csv file
#' @param id The id of the subject
#' @return summary dataframe with time spent Sitting,Move,Stand,Bike,Stairs,Run,Walk and Lying
#' @export
#' @seealso \code{\link{intensitySummary}}
skotteSummary <- function(filename, id = "NA") {

  header = AG_meta(filename,header_timestamp_format = "%d-%m-%Y %H:%M:%S")
  skotte = read_AG_counts(filename, header = TRUE,header_timestamp_format = "%d-%m-%Y %H:%M:%S")

  names(skotte) <- c("Timestamp","Data1","Data2","Data3","Data4","Data5","Data6","Data7","Data8")

  sinceepoch = round(as.numeric(skotte$Timestamp[1])/86400)*86400
  skotte$mday = floor((as.numeric(skotte$Timestamp)-sinceepoch)/86400)
  skotte$timeofday = (as.numeric(skotte$Timestamp)-sinceepoch)-skotte$mday*86400

  #Setting nonwear as not a number
  skotte[which(skotte$Data1<0),2:9] = NA;

  #Getting the unique days
  udays = unique(skotte$mday);

  skottesummary = matrix(0,length(udays),12);

  for (i in udays){
    daydata = skotte[which(skotte$mday==i & !is.na(skotte$Data1)),];
    #daydata = skotte[which(skotte$mday==i),];

    skottesummary[i+1,1] = i;
    skottesummary[i+1,2] = sinceepoch;
    skottesummary[i+1,3] = as.POSIXlt(daydata$Timestamp[1],origin="1970-01-01",tz = "GMT")$wday;
    skottesummary[i+1,4] = as.numeric(skottesummary[i+1,3]>=6 | skottesummary[i+1,3]<1)

    skottesummary[i+1,5:12] = apply(daydata[,2:9],2,sum,na.rm=TRUE);
  }

  colnames(skottesummary) <- c("MDay","StartDate","Weekday","DayType","Sitting","Move","Stand","Bike","Stairs","Run","Walk","Lying")

  skottesummary = data.frame(skottesummary);
  skottesummary$ID = rep(id, nrow(skottesummary));

  skottesummary = skottesummary[!(skottesummary$StartDate==0),]
  skottesummary$StartDate = as.POSIXct(skottesummary$StartDate, origin = "1970-01-01", tz = "GMT")

  return (skottesummary);
}

#'Summarizing the physical activity intensity for an entire project/folder
#'
#' \code{summaryIntensityFolder} Estimating the sintensity ummary measure from physical activity csv files.
#'
#'
#' @param folder Folder which contains all the csv files
#' @param intensityType The type of intensity which is summarized
#' @param curPoints The cut points used for estimating time spent in defined intensity domains
#' @return summary dataframe
#' @export
#'
#' @importFrom graphics hist
#' @importFrom stats aggregate
#' @importFrom magrittr %>% %T>% %<>% %$%
#' @seealso \code{\link{intensitySummary}}
summaryIntensityFolder <- function(folder, intensityType = "AG", cutPoints = c(-1,100,2000,5000,500000)) {

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
        daySummary = intensitySummary(filename, id = subjectID, cutPoints = cutPoints)
      } else {
        daySummary = rbind(daySummary,intensitySummary(filename, id = subjectID, cutPoints = cutPoints))
      }
    }

  }

  return(daySummary)
}

#'Summarizing the physical activity type for an entire project/folder
#'
#' \code{summarySkotteFolder} Estimating the time spent in different activity types from multiple csv files.
#'
#'
#' @param folder Folder which contains all the csv files
#' @return summary dataframe
#' @export
#' @seealso \code{\link{skotteSummary}}
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

#'Summarizing the physical activity intensity in average day
#'
#' \code{summaryAverageDayIntensity} Estimating average day statistics
#'
#'
#' @param summaryStatsIntensity Day by day intensity summary statistics
#' @param adjust5_7Rule Use the 5/7 week and 2/7 weekend adjustement
#' @param minTimeSecForValidDay Minimum time in seconds for valid day
#' @param minWeekDays Minimum number of week days required
#' @param minWeekendDays Minimum number of weekend days required
#' @return summaryAverageDay
#' @export
#' @seealso \code{\link{intensitySummary,summaryIntensityFolder}}
summaryAverageDayIntensity <- function(summaryStatsIntensity, adjust5_7Rule = TRUE, maxNonwearSec = 7200, minTimeSecForValidDay = 79200, minWeekDays = 3, minWeekendDays = 1) {

  validDays = summaryStats[which(summaryStats$Duration > minTimeSecForValidDay & summaryStats$Nonwear < maxNonwearSec),]

  Ndays = aggregate(MDay ~ ID+DayType, data=validDays, FUN=length)

  Ndays$DayType = factor(Ndays$DayType, labels = c("WeekDays","WeekendDays"))

  Ndays <- dcast(Ndays, ID ~ DayType, value.var="MDay")

  Ndays$WeekendDays[is.na(Ndays$WeekendDays)] = 0
  Ndays$WeekDays[is.na(Ndays$WeekDays)] = 0

  #if (is.na(Ndays$x[1])==TRUE) { Ndays$x[1] = 0}
  #if (is.na(Ndays$x[2])==TRUE) { Ndays$x[2] = 0}

  #if (Ndays$x[1] >= minWeekDays & Ndays$x[2]>=minWeekendDays) {

    summaryAday = aggregate(cbind(cpm,Total,Duration,cpm_pa,total_pa,Duration_pa,Sedentary,Light,Moderate,Vigorous) ~ ID+DayType, data = validDays, FUN = mean, na.rm = TRUE)

    if (adjust5_7Rule==TRUE) {
      summaryAday[which(summaryAday$DayType==0),3:ncol(summaryAday)] = summaryAday[which(summaryAday$DayType==0),3:ncol(summaryAday)] * 5/7
      summaryAday[which(summaryAday$DayType==1),3:ncol(summaryAday)] = summaryAday[which(summaryAday$DayType==1),3:ncol(summaryAday)] * 2/7
      summaryAday = aggregate(cbind(cpm,Total,Duration,cpm_pa,total_pa,Duration_pa,Sedentary,Light,Moderate,Vigorous) ~ ID, data = summaryAday, FUN = sum, na.rm = TRUE)
    } else {
      summaryAday = aggregate(cbind(cpm,Total,Duration,cpm_pa,total_pa,Duration_pa,Sedentary,Light,Moderate,Vigorous) ~ ID, data = summaryAday, FUN = mean, na.rm = TRUE)
    }

    summaryAday = merge(summaryAday,Ndays, by=c("ID"))

    summaryAday = na.omit(summaryAday)

    summaryAday = summaryAday[(summaryAday$WeekDays >= minWeekDays & summaryAday$WeekendDays >= minWeekendDays),]

    return(summaryAday)
  #}

}
#'Combining Intensity and Skotte
#'
#' \code{combinePatypesIntensity} Estimating average day statistics
#'
#'
#' @param patypes Day by day Skotte summary statistics
#' @param intensity Day by day Intensity summary statistics
#' @return all
#' @export
#' @seealso \code{\link{intensitySummary,summaryIntensityFolder}}
#'
#'
combinePatypesIntensity <- function(patypes, intensity ) {

  all <- merge(intensity,patypes,by=c("ID","MDay","Weekday","DayType","StartDate"))

  return(all)
}

#'Summarizing the physical activity in average day
#'
#' \code{summaryAverageDay} Estimating average day statistics
#'
#'
#' @param summaryStats Day by day Skotte and intensity summary statistics
#' @param adjust5_7Rule Use the 5/7 week and 2/7 weekend adjustement
#' @param minTimeSecForValidDay Minimum time in seconds for valid day
#' @param minWeekDays Minimum number of week days required
#' @param minWeekendDays Minimum number of weekend days required
#' @return summaryADay
#' @export
#' @importFrom reshape2 dcast
#' @seealso \code{\link{intensitySummary,summaryIntensityFolder}}
summaryAverageDay <- function(summaryStats, adjust5_7Rule = TRUE, minTimeSecForValidDay = 79200, maxNonwearSec = 7200, minWeekDays = 3, minWeekendDays = 1) {

  validDays = summaryStats[which(summaryStats$Duration > minTimeSecForValidDay & summaryStats$Nonwear < maxNonwearSec),]

  Ndays = aggregate(MDay ~ ID+DayType, data=validDays, FUN=length)

  Ndays$DayType = factor(Ndays$DayType, labels = c("WeekDays","WeekendDays"))

  Ndays <- dcast(Ndays, ID ~ DayType, value.var="MDay")

  Ndays$WeekendDays[is.na(Ndays$WeekendDays)] = 0
  Ndays$WeekDays[is.na(Ndays$WeekDays)] = 0

  #if (is.na(Ndays$x[1])==TRUE) { Ndays$x[1] = 0}
  #if (is.na(Ndays$x[2])==TRUE) { Ndays$x[2] = 0}

  #if (Ndays$x[1] >= minWeekDays & Ndays$x[2]>=minWeekendDays) {

    summaryAday = aggregate(cbind(cpm,Total,Duration,cpm_pa,total_pa,Duration_pa,Sedentary,Light,Moderate,Vigorous,Sitting,Move,Stand,Bike,Stairs,Run,Walk,Lying) ~ ID+DayType, data = validDays, FUN = mean, na.rm = TRUE)

    if (adjust5_7Rule==TRUE) {
      summaryAday[which(summaryAday$DayType==0),3:ncol(summaryAday)] = summaryAday[which(summaryAday$DayType==0),3:ncol(summaryAday)] * 5/7
      summaryAday[which(summaryAday$DayType==1),3:ncol(summaryAday)] = summaryAday[which(summaryAday$DayType==1),3:ncol(summaryAday)] * 2/7
      summaryAday = aggregate(cbind(cpm,Total,Duration,cpm_pa,total_pa,Duration_pa,Sedentary,Light,Moderate,Vigorous,Sitting,Move,Stand,Bike,Stairs,Run,Walk,Lying) ~ ID, data = summaryAday, FUN = sum, na.rm = TRUE)
    } else {
      summaryAday = aggregate(cbind(cpm,Total,Duration,cpm_pa,total_pa,Duration_pa,Sedentary,Light,Moderate,Vigorous,Sitting,Move,Stand,Bike,Stairs,Run,Walk,Lying) ~ ID, data = summaryAday, FUN = mean, na.rm = TRUE)
    }

    summaryAday = merge(summaryAday,Ndays, by=c("ID"))

    summaryAday = na.omit(summaryAday)

    summaryAday = summaryAday[(summaryAday$WeekDays >= minWeekDays & summaryAday$WeekendDays >= minWeekendDays),]

    return(summaryAday)
  #}

}

