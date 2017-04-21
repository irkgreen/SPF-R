options(echo=FALSE)
#RCode for SPFs

#remove working envr
#rm(list = ls())
cat(rep("\n",64))

library(MASS)
library(knitr) #download using Packages>Install Packages or Tools>Install Packages
library(ggplot2) #download using Packages>Install Packages or Tools>Install Packages
library(openxlsx) #download using Packages>Install Packages or Tools>Install Packages
library(installr) #download using Packages>Install Packages or Tools>Install Packages
#ALSO Install this: https://cran.r-project.org/bin/windows/Rtools/
#IMPORTANT: You should make sure that the box is checked to have the installer edit your PATH

#11/24/2016
VerNum <- c("RCode for SPFs ver 2.2.002")

#Directory
#Edit this in the event that your data is stored somewhere not relative to your home USER folder
if (Sys.info()["nodename"] == "MERAK") {
  MyUserFolder = "D:/SyncFolders"
} else if(Sys.info()["nodename"] == "COE4249") {
  MyUserFolder = "C:/Users/clblac2.AD"
} else {
  MyUserFolder = dirname(Sys.getenv("HOME"))
} 

#define data
TotalColumn = "Total" #The title of the column containing All Crashes. replace spaces with "." e.g. "Total.Crashes"
KABCColumn = "" #The title of the column containing KABC Crashes. leave blank to ignore
KABColumn = "KAB" #The title of the column containing KAB Crashes. leave blank to ignore
KAColumn = "" #The title of the column containing KA Crashes. leave blank to ignore
KColumn = "" #The title of the column containing K Crashes. leave blank to ignore
AADTColumn = "LASTCNT" #The title of the column containing AADT. 
LengthColumn = "Length" #The title of the column containing Length. 
ClassColumn = "Class" #The title of the column containing a class category. leave blank to ignore
ClassStart = 1 #Enter a positive integer. This will split the data into groups based on the Class defined above
ClassEnd = 7 #Enter a positive integer. This will split the data into groups based on the Class defined above
#Path to the input file as a CSV. Must contain headers as defined above.
CSVpath = paste0(MyUserFolder,"/Dropbox (Trans. Center)/~Annual Projects/HSIP/2016/Cable/Crash Data/OverlayResultsWithClassesClean.csv")
OutputProject_Base = "Cable class Test" #Project name - created in Documents/R_SPFs
myFilter_Base = "data[[AADTColumn]] > 0  & data[[LengthColumn]] > 0" # A base filter. Usually best to avoid Length or AADT of 0 or below. 
InputData_Base = "Median crossover 2011-2015" # be sure to uniquely describe the data so it can be traced back to the source
initTheta = 1.0 #starting point for theta. adjust as needed
######################################################################
######################################################################
##### Be sure to edit the model form in the glm.nb  if different #####
######################################################################
######################################################################

#Examples
#CSVPath = "D:/SyncFolders/Dropbox (Trans. Center)/PhD/Phd - Segmentation/Attribute/Case 1/SPFs/ParkwayTest/S4_1.csv"
#myFilter = "data[[AADTColumn]] > 0 & data[[LengthColumn]]"
#myFilter = "data[[AADTColumn]] < 500 & data$SHLDWID == 2 & data$LANEWID == 9 & (data$CURVECLS == 'A' | data$CURVECLS == 'B') & data$MEDTYPE == 8 & (data$GRADECLS == 'A' | data$GRADECLS == '')"

#flag to test if data is bound
databind = FALSE

#read data
result <- tryCatch({
  data=read.csv(CSVpath,header=T)
  databind = TRUE
}, warning = function(war) {
  # warning handler picks up where error was generated
  print(paste("MY_WARNING:  ",war))
  databind = FALSE
}, error = function(err) {
  # error handler picks up where error was generated
  print(paste("MY_ERROR:  ",err))
  databind = FALSE
}, finally = {
  # cleanup
})

#bind data
if (databind) {
result <- tryCatch({
  exists("data")
  databind = TRUE
}, warning = function(war) {
  # warning handler picks up where error was generated
  print(paste("MY_WARNING:  ",war))
  databind = FALSE
}, error = function(err) {
  # error handler picks up where error was generated
  print(paste("MY_ERROR:  ",err))
  databind = FALSE
}, finally = {
  # cleanup
})
}

#Main SPF function
RunSPF <- function() {
  #filter based on users' base filter
  data_temp <- data[ which(eval(parse(text = myFilter))),]
  
  #sort by AADT
  data2 <- data_temp[ order(data_temp[[AADTColumn]]),]
   
  #Point to variables
  crash=data2[[CrashColumn]]
  lnADT=log(data2[[AADTColumn]])
  lnL=log(data2[[LengthColumn]])
  #Calculate length if it doesn't exists - this will make zero length filter difficult
  #lnL=log(EMP-BMP)
  
  init.theta = initTheta
  
#################################################################
  SPF=glm.nb(crash~lnADT+offset(lnL))
#################################################################
  
  #add results from GLM
  dataout <- cbind(data2,Predicted=SPF$fitted.values,Residuals=resid(SPF,type="resp"),CumulRes=cumsum(resid(SPF,type="resp")))
  
  #calculate data for CURE plot
  datalimits <- data.frame(dataout$Residuals)
  datalimits["AADT"] <- NA
  datalimits$AADT <- data2[[AADTColumn]]
  datalimits["CumulRes"] <- NA
  datalimits$CumulRes <- dataout$CumulRes
  datalimits["Squared_Res"] <- NA
  datalimits$Squared_Res <- datalimits$dataout.Residuals^2
  datalimits["CumulSqRes"] <- NA
  datalimits$CumulSqRes <- cumsum(datalimits$Squared_Res)
  datalimits["SigmaSum"] <- NA
  datalimits$SigmaSum <- sqrt(datalimits$CumulSqRes)
  datalimits["StdDev"] <- NA
  datalimits$StdDev <- datalimits$SigmaSum*sqrt(1-datalimits$CumulSqRes/sum(datalimits$Squared_Res))
  datalimits["UpperLimit"] <- NA
  datalimits$UpperLimit <- datalimits$StdDev * 1.96
  datalimits["LowerLimit"] <- NA
  datalimits$LowerLimit <- datalimits$StdDev * (-1.96)
  datalimits["Per_CURE"] <- NA
  datalimits$Per_CURE <- ifelse(datalimits$CumulRes>datalimits$UpperLimit,1,ifelse(datalimits$CumulRes<datalimits$LowerLimit,1,0))
  
  #create CURE plot
  CUREPlot <- ggplot(datalimits, aes(datalimits$AADT, y = value, color = variable)) + 
    geom_point(aes(y = UpperLimit, col = "Upper")) + 
    geom_point(aes(y = LowerLimit, col = "Lower")) + 
    geom_point(aes(y = CumulRes, col = "CumulRes")) + 
    ggtitle("CURE Plot") +
    labs(x="AADT",y="Cumulative Residuals")
  ggsave(file=paste0(OutPath,OutputProject,"_CURE.png"))
  
  #Scatter Plot with SPF
  ScatterPlot <- ggplot(dataout, aes(dataout[[AADTColumn]], y = value, color = variable)) + 
    geom_point(aes(y = dataout[[CrashColumn]] / dataout[[LengthColumn]], col = "Obs Crashes")) + 
    geom_point(aes(y = dataout$Predicted / dataout[[LengthColumn]], col = "SPF")) + 
    ggtitle("SPF Scatter Plot") +
    labs(x="AADT",y="Crashes per mile")
  ggsave(file=paste0(OutPath,OutputProject,"_Scatter.png"))
   
  #Metrics/Stats
  Sample = nrow(dataout)
  Mileage = sum(dataout[[LengthColumn]])
  Crashes = sum(dataout[[CrashColumn]])
  ObsAvg = mean(dataout[[CrashColumn]])
  tmpTerm = sum((dataout[[CrashColumn]]-ObsAvg)^2)
  tmpTerm2 = sum((dataout[[CrashColumn]]-dataout$Predicted)^2)
  RSquared = (tmpTerm-tmpTerm2)/(tmpTerm-sum(dataout$Predicted))
  CDP = sum(datalimits$Per_CURE)/length(datalimits$Per_CURE)*100
  MACD = max(abs(datalimits$CumulRes))  
  MAD = mean(abs(dataout$Residuals))
  datametrics <- data.frame(Values = c(Sample,Mileage,Crashes,RSquared,CDP,MACD,MAD,SPF$theta,coef(summary(SPF))["(Intercept)","Estimate"],coef(summary(SPF))["lnADT","Estimate"], SPF$SE.theta, SPF$aic, "", "", ""))
  datametrics$Notes <- c("100-200 intersections*","100-200 miles*","300 crashes per year*","Higher values preferred","Less than 5%","Smaller values preferred","Smaller values preferred","Higher values preferred","(Intercept)","(lnAADT)", "", "", myFilter, InputData,"*As recommended by FHWA-SA-14-004")
  attr(datametrics, "row.names") <- c("Sample","Length","Crashes","R2","CDP","MACD","MAD","Theta","Alpha","Beta","StdErr","AIC", "Filter","Input Data","")
  datametrics$Values = as.numeric(as.character(datametrics$Values))
  
  #PCR (potential for crash reduction)
  # NOTE: the weight equation is based on a 5-year period. That is, the number of crashes in the input file is for
  #a 5-year period therefore year is not in the equation!
  dataout["Weight"] <- NA
  dataout$Weight <- 1/(1+dataout$Predicted/dataout[[LengthColumn]]/SPF$theta)
  dataout["EB_Estimate"] <- NA
  dataout$EB_Estimate <- dataout[[CrashColumn]]*(1-dataout$Weight) + dataout$Predicted*(dataout$Weight)
  dataout["PCR"] <- NA
  dataout$PCR <- dataout$EB_Estimate - dataout$Predicted
  
  #save results to Excel
  wb <- createWorkbook()
  options("openxlsx.borderStyle" = "thin")
  options("openxlsx.borderColour" = "#4F81BD")
  addWorksheet(wb, "Metrics")
  addWorksheet(wb, "Data")
  writeData(wb, "Metrics", datametrics, startCol = 2, startRow = 3, rowNames = TRUE)
  writeData(wb, "Metrics", VerNum, startCol = 1, startRow = 1)
  writeData(wb, "Metrics", CSVpath, startCol = 1, startRow = 2)
  writeData(wb, "Data", dataout)
  saveWorkbook(wb, paste0(OutPath,OutputProject,".xlsx"), overwrite = TRUE)
}

#Check if input data is valid
if (databind) {
  
  if (ClassColumn == "") {
    # this will disable the loop for classes
    ClassStart=0
    ClassEnd=0
    }
  
  for(i in ClassStart:ClassEnd) {
      
      # add a filter and change output path for classes if needed
      if (ClassColumn == "") {
        myFilter = myFilter_Base
        ClassOut = ""
      } else {
        myFilter = paste0(myFilter_Base," & data[[ClassColumn]] == ",i)
        ClassOut = paste0(" - Class ",i)
      }    
    
      # All crashes
      CrashColumn = TotalColumn
      InputData = paste0(ClassOut,InputData_Base)
      OutputProject = paste0(OutputProject_Base,ClassOut)
      #create folders
      dir.create(file.path(Sys.getenv("HOME"), "R_SPFs"))
      dir.create(file.path(paste0(Sys.getenv("HOME"),"/R_SPFs"),OutputProject))
      OutPath = paste0(Sys.getenv("HOME"),"/R_SPFs/",OutputProject,"/")
      
      RunSPF()
      print(paste0("All crashes finished",ClassOut))
    
      if (KABCColumn != "") {
        #KABC
        CrashColumn = KABCColumn
        InputData = paste0(InputData_Base," - KABC",ClassOut)
        OutputProject = paste0(OutputProject_Base," - KABC",ClassOut)
        #create folders
        dir.create(file.path(paste0(Sys.getenv("HOME"),"/R_SPFs"), OutputProject))
        OutPath = paste0(Sys.getenv("HOME"),"/R_SPFs/",OutputProject,"/")
        RunSPF()
        print(paste0("KABC crashes finished",ClassOut))
      }
      
      if (KABColumn != "") {
        #KAB
        CrashColumn = KABColumn
        InputData = paste0(InputData_Base," - KAB",ClassOut)
        OutputProject = paste0(OutputProject_Base," - KAB",ClassOut)
        #create folders
        dir.create(file.path(paste0(Sys.getenv("HOME"),"/R_SPFs"), OutputProject))
        OutPath = paste0(Sys.getenv("HOME"),"/R_SPFs/",OutputProject,"/")
        RunSPF()
        print(paste0("KAB crashes finished",ClassOut))
      }
    
      if (KAColumn != "") {
        #KA
        CrashColumn = KAColumn
        InputData = paste0(InputData_Base," - KA",ClassOut)
        OutputProject = paste0(OutputProject_Base," - KA",ClassOut)
        #create folders
        dir.create(file.path(paste0(Sys.getenv("HOME"),"/R_SPFs"), OutputProject))
        OutPath = paste0(Sys.getenv("HOME"),"/R_SPFs/",OutputProject,"/")
        RunSPF()
        print(paste0("KA crashes finished",ClassOut))
      }
      if (KColumn != "") {
        #K
        CrashColumn = KColumn
        InputData = paste0(InputData_Base," - K",ClassOut)
        OutputProject = paste0(OutputProject_Base," - K",ClassOut)
        #create folders
        dir.create(file.path(paste0(Sys.getenv("HOME"),"/R_SPFs"), OutputProject))
        OutPath = paste0(Sys.getenv("HOME"),"/R_SPFs/",OutputProject,"/")
        RunSPF()
        print(paste0("K crashes finished",ClassOut))
      }
  
  }
  print("finished")

} else {
  
  print("Check for error.")
  
}
