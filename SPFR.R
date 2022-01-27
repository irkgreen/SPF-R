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

#3/30/2021
VerNum <- c("RCode for SPFs ver 2.3.000")

#################
# F O L D E R S #
#################
#Set folders and files

DataFolder = "C:/SPF/"
InputCSV = "r2u.csv" #name of CSV in DataFolder (include extension)
CSVpath = paste0(DataFolder,InputCSV)
OutputProject_Base = "Rural 2 Lane" #Output folder name (will be created in datafolder if it doesn't exist)
InputData_Base = "Rural 2 Lane 2007-2011" # be sure to uniquely describe the data so it can be traced back to the source

###########
# D A T A #
###########
#define column names that are used below (case sensitive). Many of these are used more than once so it is helpful to define them once here
TotalColumn = "Total" #The title of the column containing All Crashes. replace spaces with "." e.g. "Total.Crashes"
KABCColumn = "KABC" #The title of the column containing KABC Crashes. leave blank to ignore
KABColumn = "KA" #The title of the column containing KAB Crashes. leave blank to ignore
KAColumn = "" #The title of the column containing KA Crashes. leave blank to ignore
KColumn = "" #The title of the column containing K Crashes. leave blank to ignore
AADTColumn = "aadt" #The title of the column containing AADT. 
LengthColumn = "Lyears" #The title of the column containing Length. 
ClassColumn = "" #The title of the column containing a class category (integers). leave blank to ignore
ClassStart = 1 #Enter a positive integer. This will split the data into groups based on the Class defined above
ClassEnd = 2 #Enter a positive integer. This will split the data into groups based on the Class defined above
#Path to the input file as a CSV. Must contain headers as defined above.

#################
# F I L T E R S #
#################
# each filter line requires a filter and a description. Be sure to update both as the description will be included in the output folder name
#Examples (you can use the variable references data[[LengthColumn]] or directly like data$LANEWID)
myFilter_Base = "data[[LengthColumn]] > 0"# A base filter. Usually best to avoid Length or AADT of 0 or below. 
#myFilter_Base = paste0(myFilter_Base," & data$LANEWID == 9"); OutputProject_Base = paste0(OutputProject_Base," LW9")
#myFilter_Base = paste0(myFilter_Base," & data$SHLDWID == 3"); OutputProject_Base = paste0(OutputProject_Base," SW3")
#myFilter_Base = paste0(myFilter_Base," & data$IntsctID_1 == 0"); OutputProject_Base = paste0(OutputProject_Base," noInt")
#myFilter_Base = paste0(myFilter_Base," & (data$GRADECLS == 'A' | data$GRADECLS == '')"); OutputProject_Base = paste0(OutputProject_Base," noVC")
#myFilter_Base = paste0(myFilter_Base," & (data$CURVECLS == 'A' | data$CURVECLS == 'B')"); OutputProject_Base = paste0(OutputProject_Base," noHC")
#myFilter_Base = paste0(myFilter_Base," & data$MEDTYPE == 8"); OutputProject_Base = paste0(OutputProject_Base," noMed")
#myFilter_Base = paste0(myFilter_Base," & data[[AADTColumn]] < 500"); OutputProject_Base = paste0(OutputProject_Base," ADT500")
#myFilter_Base = paste0(myFilter_Base," & data$RT_UNIQUE != '030-US-0060  -000'"); OutputProject_Base = paste0(OutputProject_Base," noUS60")
#myFilter_Base = paste0(myFilter_Base," & data[[AADTColumn]] > 0"); OutputProject_Base = paste0(OutputProject_Base," ADTgtzero")
#myFilter_Base = paste0(myFilter_Base," & data[[AADTColumn]] < 2000"); OutputProject_Base = paste0(OutputProject_Base," ADT2000")
#myFilter_Base = paste0(myFilter_Base," & data[[AADTColumn]] < 2500"); OutputProject_Base = paste0(OutputProject_Base," ADT2500")
#myFilter_Base = paste0(myFilter_Base," & data$LANEWID >= 9 & data$LANEWID <= 10"); OutputProject_Base = paste0(OutputProject_Base," LW9-10")
#myFilter_Base = paste0(myFilter_Base," & data$LANEWID >= 8 & data$LANEWID <= 10"); OutputProject_Base = paste0(OutputProject_Base," LW8-10")
#myFilter_Base = paste0(myFilter_Base," & data$SHLDWID >= 2 & data$SHLDWID <= 3"); OutputProject_Base = paste0(OutputProject_Base," SW2-3")
#myFilter_Base = paste0(myFilter_Base," & data$SHLDWID >= 2 & data$SHLDWID <= 4"); OutputProject_Base = paste0(OutputProject_Base," SW2-4")
#myFilter_Base = paste0(myFilter_Base," & data$SPEEDLIM >= 50"); OutputProject_Base = paste0(OutputProject_Base," SL50p")

###########
# M I S C #
###########
initTheta = 0.1 #starting point for theta. adjust as needed

######################################################################
######################################################################
##### Be sure to edit the model form in the glm.nb if different ######
######################################################################
######################################################################
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
  #ru=data2$ru #refer to a column directly
  
  #Calculate length if it doesn't exists - this will make zero length filter difficult
  #lnL=log(EMP-BMP)
  
  init.theta = initTheta

  ###########################################################################################
  ###########################################################################################  
  # Be sure to edit the datametrics list below to reflect any added parameters to the model #
  ###########################################################################################
  ###########################################################################################
  SPF=glm.nb(crash~lnADT+offset(lnL))
  #SPF=glm.nb(crash~lnADT+ru+offset(lnL)) # added a variable, see the users guide for more
  ###########################################################################################
  ###########################################################################################
  ###########################################################################################    

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
  
  #box plots
  jpeg(file=paste0(OutPath,OutputProject,"_CrashBox.png"))
    boxplot(dataout[[CrashColumn]], main="Crash Box Plot", ylab="Crashes")
  dev.off()
  jpeg(file=paste0(OutPath,OutputProject,"_CrashPerMileBox.png"))
    boxplot(dataout[[CrashColumn]]/dataout[[LengthColumn]], main="Crashes Per Mile Box Plot", ylab="Crashes Per Mile")
  dev.off()
  jpeg(file=paste0(OutPath,OutputProject,"_LengthBox.png"))
    boxplot(dataout[[LengthColumn]], main="Length Box Plot", ylab="Length")
  dev.off()
  jpeg(file=paste0(OutPath,OutputProject,"_AADTBox.png"))
    boxplot(dataout[[AADTColumn]], main="AADT Box Plot", ylab="AADT")
  dev.off()

  #Metrics
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

  ############################################################################################################
  ############################################################################################################
  # add or remove items to all three lists to correspond to any model parameters (see user guide for examples)
  ############################################################################################################
  ############################################################################################################
  datametrics <- data.frame(Values = c(Sample,Mileage,Crashes,RSquared,CDP,MACD,MAD,SPF$theta,coef(summary(SPF))["(Intercept)","Estimate"],coef(summary(SPF))["lnADT","Estimate"],SPF$SE.theta, SPF$aic, "", "", ""))
  datametrics$Notes <- c("100-200 intersections*","100-200 miles*","300 crashes per year*","Higher values preferred","Less than 5%","Smaller values preferred","Smaller values preferred","Higher values preferred",paste0("(Intercept) zvalue= ",coef(summary(SPF))["(Intercept)","Pr(>|z|)"]) ,paste0("lnADT zvalue= ",coef(summary(SPF))["lnADT","Pr(>|z|)"]),"", "", myFilter, InputData,"*As recommended by FHWA-SA-14-004") 
  attr(datametrics, "row.names") <- c("Sample","Length","Crashes","R2","CDP","MACD","MAD","Theta","Alpha","Beta","StdErr","AIC", "Filter","Input Data","")
  datametrics$Values = as.numeric(as.character(datametrics$Values))
  ############################################################################################################
  ############################################################################################################
  ### Example with variable added#############################################################################
  #datametrics <- data.frame(Values = c(Sample,Mileage,Crashes,RSquared,CDP,MACD,MAD,SPF$theta,coef(summary(SPF))["(Intercept)","Estimate"],coef(summary(SPF))["lnADT","Estimate"],coef(summary(SPF))["ru","Estimate"],SPF$SE.theta, SPF$aic, "", "", ""))
  #datametrics$Notes <- c("100-200 intersections*","100-200 miles*","300 crashes per year*","Higher values preferred","Less than 5%","Smaller values preferred","Smaller values preferred","Higher values preferred",paste0("(Intercept) zvalue= ",coef(summary(SPF))["(Intercept)","Pr(>|z|)"]) ,paste0("lnADT zvalue= ",coef(summary(SPF))["lnADT","Pr(>|z|)"]),paste0("ru zvalue= ",coef(summary(SPF))["ru","Pr(>|z|)"]),"", "", myFilter, InputData,"*As recommended by FHWA-SA-14-004") 
  #attr(datametrics, "row.names") <- c("Sample","Length","Crashes","R2","CDP","MACD","MAD","Theta","Alpha","BetaAADT","BetaRU","StdErr","AIC", "Filter","Input Data","")
  #datametrics$Values = as.numeric(as.character(datametrics$Values))
  ############################################################################################################
  
  #Stats
  statTitles <- data.frame(Stat=c("Mean","StdDev","Min","Q1","Median","Q3","Max"))
  CrashQuartiles = quantile(dataout[[CrashColumn]])
  statCrash <- data.frame(Crash=c(mean(dataout[[CrashColumn]]),sd(dataout[[CrashColumn]]),min(dataout[[CrashColumn]]),CrashQuartiles[2],CrashQuartiles[3],CrashQuartiles[4],max(dataout[[CrashColumn]])))
  CrashPerMileQuartiles = quantile(dataout[[CrashColumn]]/dataout[[LengthColumn]])
  statCrashPerMile <- data.frame(CrashesPerMile=c(mean(dataout[[CrashColumn]]/dataout[[LengthColumn]]),sd(dataout[[CrashColumn]]/dataout[[LengthColumn]]),min(dataout[[CrashColumn]]/dataout[[LengthColumn]]),CrashPerMileQuartiles[2],CrashPerMileQuartiles[3],CrashPerMileQuartiles[4],max(dataout[[CrashColumn]]/dataout[[LengthColumn]])))
  AADTQuartiles = quantile(dataout[[AADTColumn]])
  statAADT <- data.frame(AADT=c(mean(dataout[[AADTColumn]]),sd(dataout[[AADTColumn]]),min(dataout[[AADTColumn]]),AADTQuartiles[2],AADTQuartiles[3],AADTQuartiles[4],max(dataout[[AADTColumn]])))
  LengthQuartiles = quantile(dataout[[LengthColumn]])
  statLength <- data.frame(Length=c(mean(dataout[[LengthColumn]]),sd(dataout[[LengthColumn]]),min(dataout[[LengthColumn]]),LengthQuartiles[2],LengthQuartiles[3],LengthQuartiles[4],max(dataout[[LengthColumn]])))
  
  myStats = data.frame(statTitles,statCrash,statCrashPerMile,statAADT,statLength)
  
  #EEC (potential for crash reduction)
  # NOTE: the weight equation is based on a 5-year period. That is, the number of crashes in the input file is for
  #a 5-year period therefore year is not in the equation!
  dataout["Weight"] <- NA
  dataout$Weight <- 1/(1+dataout$Predicted/dataout[[LengthColumn]]/SPF$theta)
  dataout["EB_Estimate"] <- NA
  dataout$EB_Estimate <- dataout[[CrashColumn]]*(1-dataout$Weight) + dataout$Predicted*(dataout$Weight)
  dataout["EEC"] <- NA
  dataout$EEC <- dataout$EB_Estimate - dataout$Predicted
  dataout["SDev"] <- NA
  dataout$SDev <- sqrt((1-dataout$Weight)*dataout$EB_Estimate) #fixed error
  
    #calculate data for LOSS Plot
  dataout<-cbind(dataout,Expected=dataout$Predicted/dataout[[LengthColumn]],LOSS_Crashes=dataout$EB_Estimate/dataout[[LengthColumn]])
  dataout=cbind(dataout,beta=dataout$Expected/SPF$theta) # Fixed Error
  dataout=cbind(dataout,lowloss=qgamma(0.05,SPF$theta,scale=dataout$beta),uploss=qgamma(0.95,SPF$theta,scale=dataout$beta))
  dataout=cbind(dataout,LOSS_score=ifelse(dataout$LOSS_Crashes>dataout$uploss,4,ifelse(dataout$LOSS_Crashes>dataout$Expected,3,ifelse(dataout$LOSS_Crashes>dataout$lowloss,2,1)))) # fixed error
  
  #create LOSS plot
  LOSSPlot <- ggplot(dataout, aes(dataout[[AADTColumn]], y = value, color = variable)) + 
    geom_point(aes(y = uploss, col = "Upper LOSS")) + 
    geom_point(aes(y = lowloss, col = "Lower LOSS")) + 
    geom_point(aes(y = Expected, col = "Expected")) + 
    geom_point(aes(y = LOSS_Crashes, col = "Crashes (EB Corrected)")) + 
    ggtitle("LOSS Plot") +
    labs(x="AADT",y="Crashes per Mile")
  ggsave(file=paste0(OutPath,OutputProject,"_LOSS.png"))

  
  #save results to Excel
  wb <- createWorkbook()
  options("openxlsx.borderStyle" = "thin")
  options("openxlsx.borderColour" = "#4F81BD")
  addWorksheet(wb, "Metrics")
  addWorksheet(wb, "Data")
  writeData(wb, "Metrics", datametrics, startCol = 2, startRow = 3, rowNames = TRUE)
  writeData(wb, "Metrics", VerNum, startCol = 1, startRow = 1)
  writeData(wb, "Metrics", CSVpath, startCol = 1, startRow = 2)
  writeData(wb, "Metrics", myStats, startCol = 2, startRow = 20)
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
      dir.create(file.path(DataFolder, "R_SPFs"))
      dir.create(file.path(paste0(DataFolder,"/R_SPFs"),OutputProject))
      OutPath = paste0(DataFolder,"/R_SPFs/",OutputProject,"/")
      
      RunSPF()
      print(paste0("All crashes finished",ClassOut))
    
      if (KABCColumn != "") {
        #KABC
        CrashColumn = KABCColumn
        InputData = paste0(InputData_Base," - KABC",ClassOut)
        OutputProject = paste0(OutputProject_Base," - KABC",ClassOut)
        #create folders
        dir.create(file.path(paste0(DataFolder,"/R_SPFs"), OutputProject))
        OutPath = paste0(DataFolder,"/R_SPFs/",OutputProject,"/")
        RunSPF()
        print(paste0("KABC crashes finished",ClassOut))
      }
      
      if (KABColumn != "") {
        #KAB
        CrashColumn = KABColumn
        InputData = paste0(InputData_Base," - KAB",ClassOut)
        OutputProject = paste0(OutputProject_Base," - KAB",ClassOut)
        #create folders
        dir.create(file.path(paste0(DataFolder,"/R_SPFs"), OutputProject))
        OutPath = paste0(DataFolder,"/R_SPFs/",OutputProject,"/")
        RunSPF()
        print(paste0("KAB crashes finished",ClassOut))
      }
    
      if (KAColumn != "") {
        #KA
        CrashColumn = KAColumn
        InputData = paste0(InputData_Base," - KA",ClassOut)
        OutputProject = paste0(OutputProject_Base," - KA",ClassOut)
        #create folders
        dir.create(file.path(paste0(DataFolder,"/R_SPFs"), OutputProject))
        OutPath = paste0(DataFolder,"/R_SPFs/",OutputProject,"/")
        RunSPF()
        print(paste0("KA crashes finished",ClassOut))
      }
      if (KColumn != "") {
        #K
        CrashColumn = KColumn
        InputData = paste0(InputData_Base," - K",ClassOut)
        OutputProject = paste0(OutputProject_Base," - K",ClassOut)
        #create folders
        dir.create(file.path(paste0(DataFolder,"/R_SPFs"), OutputProject))
        OutPath = paste0(DataFolder,"/R_SPFs/",OutputProject,"/")
        RunSPF()
        print(paste0("K crashes finished",ClassOut))
      }
  
  }
  print("finished")

} else {
  
  print("Check for error.")
  
}
