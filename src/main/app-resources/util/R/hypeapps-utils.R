#!/opt/anaconda/bin/Rscript --vanilla --slave --quiet
#
# /hypeapps-[appName]/src/main/app-resources/util/R/hypeapps-utils.R
#
# Copyright 2019-2020 AGRHYMET
#
# This file is part of H-TEP Hydrological Modelling Application, which is open source 
# and distributed under the terms of the Lesser GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or (at your option) 
# any later version. The Hydrology TEP Hydrological Modelling Application is distributed 
# in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
# warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the Lesser GNU 
# General Public License for more details. You should have received a copy of the Lesser 
# GNU General Public License along with the Hydrology TEP Hydrological Modelling Application. 
# If not, see <http://www.gnu.org/licenses/>.
#
# hypeapps-utils.R: R tools for the HTEP hydrological modelling application 
# Author:           Bernard Minoungou, AGRHYMET
# Version:          2020-05-19 
#
# dependencies
library(data.table)

## --------------------------------------------------------------------------------
## initial settings
## --------------------------------------------------------------------------------
# set system flag if not set
if(!exists("app.sys")){
  app.sys ="tep"
}

# load rciop library
if(app.sys=="tep"){
  library("rciop")
}

# source hypeapps environment file, if needed
if(!exists("app.envset")){
  if(app.sys=="tep"){
    source(paste(Sys.getenv("_CIOP_APPLICATION_PATH"),"util/R/hypeapps-environment.R",sep="/"))
  }else if(app.sys=="win"){
    source("application/util/R/hypeapps-environment.R")  
  }
  if(app.sys=="tep"){rciop.log ("DEBUG", paste("hypeapps-environment.R sourced"), "/util/R/hypeapps-utils.R")}
}

# source hype-utils.R
if(app.sys=="tep"){
  
  #source(paste(Sys.getenv("_CIOP_APPLICATION_PATH"),"util/R/hypeapps-hype-utils.R",sep="/"))
  #rciop.log ("DEBUG", paste("hypeapps-hype-utils.R sourced"), "/util/R/hypeapps-utils.R")

}else if(app.sys=="win"){
  #source(paste("application","util/R/hypeapps-hype-utils.R",sep="/"))
}else{
}

## --------------------------------------------------------------------------------
## functions
## -------------------------------------------------------------------------------
## getHypeAppInput - function to load user parameter inputs, depending on application
getHypeAppInput<-function(appName){
  ## POSTPROCESSING ##  
  if(appName=="postprocessing"){
    if(app.sys=="tep"){
      # get parameters with rciop function when running on the TEP system
      idate                   <- rciop.getparam("idate")                  # Forecast issue date
      area                    <- rciop.getparam("area")                   # Area of Interest
      model                   <- rciop.getparam("model")                  # Selected Model
      variable                <- rciop.getparam("timeOutputData")         # time output input files
      filterzone              <- rciop.getparam("filterZone")             # Flood prone or all subbasins
      alertmethod             <- rciop.getparam("alertMethod")            # Selected alert method
      datasource              <- rciop.getparam("datasource")             # Data source for historical year (hype or qobs)
      yearr                   <- rciop.getparam("year")                   # Historical year
      returnPeriodIN          <- rciop.getparam("returnPeriod")           # return period inputs
      impact_index            <- rciop.getparam("impactIndex")            # True or False 
      userthreshold           <- rciop.getparam("userThreshold")          # Selected Treshold
      notification_settings   <- rciop.getparam("notificationSettings")   # Notification settings
      popmethod               <- rciop.getparam("popMethod")              # Population Method
      #returnPeriodIN          <-rciop.getparam("returnPeriod")
      rciop.log ("DEBUG", "Que se passe til", "/util/R/hypeapps-utils.R")
      returnPeriodIN<-c(2,5,30)
      #returnPeriodIN<-as.integer(trimws(strsplit(as.character(returnPeriodIN),split = ",")[[1]]))
    }else if(app.sys=="win"){
      idate                   <- ""                                       # Forecast issue date
      area                    <- "Guinea"                                 # Area of Interest
      model                   <- "mosaic-hype"                             # Selected Model
      variable                <- "timeCOUT"                               # time output input files
      filterzone              <- "Allbasins"                              # Flood prone or all subbasins
      alertmethod             <- "hypethreshold"                                  # Selected alert method
      datasource              <- "modeledhistorical"                      # Data source for historical year (hype or qobs)
      yearr                   <- 2012                                     # Historical year
      returnPeriodIN          <- c(2,5,30)                                # return period inputs
      impact_index            <- TRUE                                     # True or False 
      userthreshold           <- c(100, 200, 400)                         # Selected Treshold
      notification_settings   <- c(10, 0)                                 # Notification settings
      popmethod               <- 1                                        # Population Method
    }else{
      idate                   <- NULL                                     # Forecast issue date
      area                    <- NULL                                     # Area of Interest
      model                   <- NULL                                     # Selected Model
      variable                <- NULL                                     # time output input files
      filterzone              <- NULL                                     # Flood prone or all subbasins
      alertmethod             <- NULL                                     # Selected alert method
      yearr                   <- NULL                                     # Historical year
      returnPeriodIN          <- NULL                                     # return period inputs
      impact_index            <- NULL                                     # True or False 
      userthreshold           <- NULL                                     # Selected Treshold
      notification_settings   <- NULL                                     # Notification settings
      popmethod               <- NULL                                     # Population Method
      print("WARNING: hypeapps.sys not set, allowed values are 'tep' or 'win' ")
    }
    if(app.sys=="win"|app.sys=="tep") {
      appInput=list("idate"                   = idate,     
                    "area"                    = area ,   
                    "model"                   = model,
                    "variable"                = variable,
                    "filterzone"              = filterzone,
                    "alertmethod"             = alertmethod,
                    "datasource"              =datasource,
                    "yearr"                   = yearr,
                    "returnPeriodIN"          = returnPeriodIN,
                    "impact_index"            = impact_index,
                    "userthreshold"           = userthreshold,
                    "notification_settings"    = notification_settings,
                    "popmethod"               = popmethod)
    } else {
    appInput=list("appName"=NULL)
  }
  return(appInput)
  }
}

## prepare work directories and copy basic model files
getHypeAppSetup<-function(modelName,
                          modelBin,
                          tmpDir,
                          appDir,
                          appName,
                          appInput,
                          modelFilesURL,
                          forcingArchiveURL=NULL,
                          stateFilesURL=NULL,
                          stateFilesIN=NULL,
                          historical.files.url=NULL, 
                          historicalwaffi.files.url=NULL){
  
  ## model files run directory (for all applications, except returnperiod)
  if(appName=="historical"|appName=="forecast"|appName=="eodata"|appName=="returnperiod"|appName=="postprocessing"){
    modelFilesRunDir=paste(tmpDir,'model',modelName,sep="/")
    dir.create(modelFilesRunDir,recursive = T,showWarnings = F)
  }else{
    modelFilesRunDir=NULL
  }
  
  # model files results directory (not necessary for "eodata" and "returnperiod")
  if(appName=="historical"|appName=="postprocessing"){
    modelResDir=paste(modelFilesRunDir,'results',sep="/")
    dir.create(modelResDir,recursive = T,showWarnings = F)
  }else if(appName=="forecast"){
    modelResDir=paste(modelFilesRunDir,'hindcast',sep="/")
    modelResDir=c(modelResDir,paste(modelFilesRunDir,'forecast',sep="/"))
    for(i in 1:2){
      dir.create(modelResDir[i],recursive = T,showWarnings = F)
    }
  }else{
    modelResDir=NULL
  }
  
  # eodata results directory
  eodataResDir=NULL
  
  # return period run directory
  returnperiodResDir=NULL
  
  # postprocessing run directory
  
  
  ## model binary file (stays in application folder)
  modelBinaryFile=NULL
  sysCommand=NULL
  
  
  ## State file filenames and dates, set bdate limits
  stateFiles = NULL
  stateDates = NULL
  bdateMax   = NULL
  bdateMin   = NULL
  
  ## check existance of forcing archive, and if existing, it's first and last date.
  forcingArchiveExist=F
  
  ## Sub-basin shapefiles (potentially for all applications for map plots)
  if(!is.null(shapefile.url)){
    # libraries needed to read the shapefile
    library(sp)
    library(rgdal)
    library(rgeos)
    
    # subfolder for shapefiles
    shapefileDir = paste(modelFilesRunDir,"shapefile",sep="/")
    dir.create(shapefileDir,recursive = T,showWarnings = F)
    
    #download shapefile from storage
    if(app.sys=="tep") {
      for(i in 1:length(shapefile.basin.ext)){
        rciop.copy(paste(paste(shapefile.basin.url,shapefile.basin.layer,sep="/"),shapefile.basin.ext[i],sep=""), shapefileDir)
      }
      
      for(i in 1:length(shapefile.countries.ext)){
        rciop.copy(paste(paste(shapefile.countries.url,shapefile.countries.layer,sep="/"),shapefile.countries.ext[i],sep=""), shapefileDir)
      }
    } else if(app.sys=="win") {
      for(i in 1:length(shapefile.basin.ext)){
        file.copy(paste(paste(shapefile.basin.url, model.name, sep="/"),shapefile.basin.ext[i],sep=""), shapefileDir)
      }
      
      for(i in 1:length(shapefile.countries.ext)){
        file.copy(paste(paste(shapefile.countries.url,shapefile.countries.layer,sep="/"),shapefile.countries.ext[i],sep=""), shapefileDir)
      }
    }
    
    # open and save shapefile as Rdata
    shapefilebasinData = readOGR(dsn = shapefileDir, layer = model.name)
    shapefilebasinRdata = paste(shapefileDir,"/",model.name,".Rdata",sep="")
    save(list = "shapefilebasinData",file = shapefilebasinRdata)
    
    shapefilecountriesData = readOGR(dsn = shapefileDir, layer = shapefile.countries.layer)
    shapefilecountriesRdata = paste(shapefileDir,"/",shapefile.countries.layer,".Rdata",sep="")
    save(list = "shapefilecountriesData",file = shapefilecountriesRdata)
    
  }else{
    shapefilebasinRdata=NULL
    shapefilebasinData=NULL
    shapefilebasinDir=NULL
    shapefilecountriesRdata=NULL
    shapefilecountriesData=NULL
    shapefilecountriesDir=NULL
  }
  
  ##download historical time file from storage
  if(app.sys=="tep") {
    if(model.name=="niger-hype"|model.name=="ww-hype") {
      if(appInput$alertmethod=="hypethreshold"|(appInput$alertmethod=="historicyear"&appInput$datasource=="modeledhistorical")|appInput$alertmethod=="waffi") {
        rciop.copy(paste(paste(historical.files.url,paste0("historical_",appInput$variable),sep="/"),"_", model.name,".txt", sep=""), modelResDir)
      } else if ((appInput$alertmethod=="historicyear"&appInput$datasource=="observedvalues")|appInput$alertmethod=="observationthreshold") {
        rciop.copy(paste(qobs.files.url,paste0("Qobs_", model.name,".txt"), sep="/"), modelResDir)
      } 
      if(appInput$alertmethod=="waffi") {
        rciop.copy(paste(paste(historicalwaffi.files.url,paste0("historical_","timeWAFFI"),sep="/"),"_", model.name,".txt", sep=""), modelResDir)
      }
      rciop.copy(paste(paste(historical.files.url,paste0("forecast_",appInput$variable),sep="/"),"_", model.name,".txt", sep=""), modelResDir)
    } else if(model.name=="mosaic-hype") {
      if(appInput$alertmethod=="hypethreshold"|(appInput$alertmethod=="historicyear"&appInput$datasource=="modeledhistorical")|appInput$alertmethod=="waffi") {
        rciop.copy(paste(paste(historical.files.url,paste0("historical_",appInput$variable),sep="/"),"_", "niger-hype",".txt", sep=""), modelResDir)
        rciop.copy(paste(paste(historical.files.url,paste0("historical_",appInput$variable),sep="/"),"_", "ww-hype",".txt", sep=""), modelResDir)
      } else if ((appInput$alertmethod=="historicyear"&appInput$datasource=="observedvalues")|appInput$alertmethod=="observationthreshold") {
        rciop.copy(paste(qobs.files.url,paste0("Qobs_", "niger-hype",".txt"), sep="/"), modelResDir)
        rciop.copy(paste(qobs.files.url,paste0("Qobs_", "ww-hype",".txt"), sep="/"), modelResDir)
      }
      if (appInput$alertmethod=="waffi") {
        rciop.copy(paste(paste(historicalwaffi.files.url,paste0("historical_","timeWAFFI"),sep="/"),"_", "niger-hype",".txt", sep=""), modelResDir)
        rciop.copy(paste(paste(historicalwaffi.files.url,paste0("historical_","timeWAFFI"),sep="/"),"_", "ww-hype",".txt", sep=""), modelResDir)
      }
      rciop.copy(paste(paste(historical.files.url,paste0("forecast_",appInput$variable),sep="/"),"_", "niger-hype",".txt", sep=""), modelResDir)
      rciop.copy(paste(paste(historical.files.url,paste0("forecast_",appInput$variable),sep="/"),"_", "ww-hype",".txt", sep=""), modelResDir)
    }
    rciop.copy(paste(population.density.url,population.density.file, sep="/"), modelResDir) 
    } else if(app.sys=="win") {
    if(model.name=="niger-hype"|model.name=="ww-hype") {
      if(appInput$alertmethod=="hypethreshold"|(appInput$alertmethod=="historicyear"&appInput$datasource=="modeledhistorical")|appInput$alertmethod=="waffi") {
        file.copy(paste(paste(historical.files.url,paste0("historical_",appInput$variable),sep="/"),"_", model.name,".txt", sep=""), modelResDir)
      } else if ((appInput$alertmethod=="historicyear"&appInput$datasource=="observedvalues")|appInput$alertmethod=="observationthreshold") {
        file.copy(paste(qobs.files.url,paste0("Qobs_", model.name,".txt"), sep="/"), modelResDir)
      } else if(appInput$alertmethod=="waffi") {
        file.copy(paste(paste(historicalwaffi.files.url,paste0("historical_","timeWAFFI"),sep="/"),"_", model.name,".txt", sep=""), modelResDir)
      }
      file.copy(paste(paste(historical.files.url,paste0("forecast_",appInput$variable),sep="/"),"_", model.name,".txt", sep=""), modelResDir)
    } else if(model.name=="mosaic-hype") {
      if(appInput$alertmethod=="hypethreshold"|(appInput$alertmethod=="historicyear"&appInput$datasource=="modeledhistorical")|appInput$alertmethod=="waffi") {
        file.copy(paste(paste(historical.files.url,paste0("historical_",appInput$variable),sep="/"),"_", "niger-hype",".txt", sep=""), modelResDir)
        file.copy(paste(paste(historical.files.url,paste0("historical_",appInput$variable),sep="/"),"_", "ww-hype",".txt", sep=""), modelResDir)
      } else if ((appInput$alertmethod=="historicyear"&appInput$datasource=="observedvalues")|appInput$alertmethod=="observationthreshold") {
        file.copy(paste(qobs.files.url,paste0("Qobs_", "niger-hype",".txt"), sep="/"), modelResDir)
        file.copy(paste(qobs.files.url,paste0("Qobs_", "ww-hype",".txt"), sep="/"), modelResDir)
      } 
      file.copy(paste(paste(historical.files.url,paste0("forecast_",appInput$variable),sep="/"),"_", "niger-hype",".txt", sep=""), modelResDir)
      file.copy(paste(paste(historical.files.url,paste0("forecast_",appInput$variable),sep="/"),"_", "ww-hype",".txt", sep=""), modelResDir)
      if(appInput$alertmethod=="waffi") {
        file.copy(paste(paste(historicalwaffi.files.url,paste0("historical_","timeWAFFI"),sep="/"),"_", "niger-hype",".txt", sep=""), modelResDir)
        file.copy(paste(paste(historicalwaffi.files.url,paste0("historical_","timeWAFFI"),sep="/"),"_", "ww-hype",".txt", sep=""), modelResDir)
      }
    }
      file.copy(paste(population.density.url,population.density.file, sep="/"), modelResDir)
      
  }
  
  ## return period magnitudes default files OR file from input
  if(appName=="forecast"){
    rpFileCOUT=NULL
    
    rciop.log ("DEBUG", paste(" appInput$rpfile= ", appInput$rpfile, sep=""), "getHypeSetup")
    
    if( is.null(appInput$rpfile) || length(appInput$rpfile) == 0 || appInput$rpfile=="default"){
      # download default file from data storage
      rpFileURL = paste(modelFilesURL,"returnlevels",paste(modelName,"-rp-cout.txt",sep=""),sep="/")
      # download rpfile to forecast output folder - using rciop.copy since we already have the URL to the file
      rciop.copy(rpFileURL, modelResDir[2])
      # path to downloaded rpfile
      rpFileCOUT = paste(modelResDir[2],paste(paste(modelName,"-rp-cout.txt",sep="")),sep="/")
    }else{
      # download the file specified by user input
      #
      # in this case we have to use the opensearch-client command, since the user 
      # input is a opensearch URL and not the URL to the file
      
      # make subfolder for download:
      targetFolder = paste(tmpDir,"/rpFile_1",sep="")
      dir.create(targetFolder,recursive = T,showWarnings = F)
      
      rciop.log ("DEBUG", paste(" targetFolder = ", targetFolder, sep=""), "getHypeSetup")
      
      # get file using opensearch-client
      sysCmd=paste("opensearch-client '",appInput$rpfile,"' enclosure | ciop-copy -s -U -O ",targetFolder,"/ -", sep="")
      
      rciop.log ("DEBUG", paste(" sysCmd = ", sysCmd, sep=""), "getHypeSetup")
      
      rpFileCOUT=system(command = sysCmd,intern = T)
      
      rciop.log ("DEBUG", paste(" rpFileCOUT = ", rpFileCOUT, sep=""), "getHypeSetup")
      
    }
    # check existance of rpFileCOUT and check available return periods in the file
    if(!file.exists(rpFileCOUT)){
      rpFileCOUT=NULL
    }
    
  }else{
    rpFileCOUT=NULL
  }
  if(app.input$alertmethod=="hypethreshold"|app.input$alertmethod=="none") {
    ## return list with application setup
    appSetup = list("runDir"=modelFilesRunDir,
                    "resDir"=modelResDir,
                    "runCommand"=sysCommand,
                    "eodataResDir"=eodataResDir,
                    "returnperiodResDir"=returnperiodResDir,
                    "tmpDir"=tmpDir,
                    "appDir"=appDir,
                    "appName"=appName,
                    "modelName"=modelName,
                    "modelBin"=modelBin,
                    "stateFilesURL"=stateFilesURL,
                    "stateFiles"=stateFiles,
                    "stateDates"=stateDates,
                    "forcingArchiveURL"=forcingArchiveURL,
                    "forcingArchiveExist"=forcingArchiveExist,
                    "shapefileDir"=shapefileDir,
                    "shapefilebasinLayer"=shapefile.layer,
                    "shapefilebasinExt"=shapefile.ext,
                    "shapefilebasinRdata"=shapefilebasinRdata,
                    "shapefilebasinData"=shapefilebasinData,
                    "shapefilecountriesLayer"=shapefile.layer,
                    "shapefilecountriesExt"=shapefile.ext,
                    "shapefilecountriesRdata"=shapefilecountriesRdata,
                    "shapefilecountriesData"=shapefilebasinData,
                    "rpFileCOUT"=rpFileCOUT,
                    "historical.files.url"=historical.files.url,
                    "forecast.files.url"=forecast.files.url,
                    "population.density.file"=population.density.file)
  } else if (app.input$alertmethod=="historicyear"|app.input$alertmethod=="observationthreshold") {
    ## return list with application setup
    appSetup = list("runDir"=modelFilesRunDir,
                    "resDir"=modelResDir,
                    "runCommand"=sysCommand,
                    "eodataResDir"=eodataResDir,
                    "returnperiodResDir"=returnperiodResDir,
                    "tmpDir"=tmpDir,
                    "appDir"=appDir,
                    "appName"=appName,
                    "modelName"=modelName,
                    "modelBin"=modelBin,
                    "stateFilesURL"=stateFilesURL,
                    "stateFiles"=stateFiles,
                    "stateDates"=stateDates,
                    "forcingArchiveURL"=forcingArchiveURL,
                    "forcingArchiveExist"=forcingArchiveExist,
                    "shapefileDir"=shapefileDir,
                    "shapefilebasinLayer"=shapefile.layer,
                    "shapefilebasinExt"=shapefile.ext,
                    "shapefilebasinRdata"=shapefilebasinRdata,
                    "shapefilebasinData"=shapefilebasinData,
                    "shapefilecountriesLayer"=shapefile.layer,
                    "shapefilecountriesExt"=shapefile.ext,
                    "shapefilecountriesRdata"=shapefilecountriesRdata,
                    "shapefilecountriesData"=shapefilebasinData,
                    "rpFileCOUT"=rpFileCOUT,
                    "qobs.files.url"=qobs.files.url,
                    "forecast.files.url"=forecast.files.url,
                    "population.density.file"=population.density.file)
  } else if(app.input$alertmethod=="waffi") {
    appSetup = list("runDir"=modelFilesRunDir,
                    "resDir"=modelResDir,
                    "runCommand"=sysCommand,
                    "eodataResDir"=eodataResDir,
                    "returnperiodResDir"=returnperiodResDir,
                    "tmpDir"=tmpDir,
                    "appDir"=appDir,
                    "appName"=appName,
                    "modelName"=modelName,
                    "modelBin"=modelBin,
                    "stateFilesURL"=stateFilesURL,
                    "stateFiles"=stateFiles,
                    "stateDates"=stateDates,
                    "forcingArchiveURL"=forcingArchiveURL,
                    "forcingArchiveExist"=forcingArchiveExist,
                    "shapefileDir"=shapefileDir,
                    "shapefilebasinLayer"=shapefile.layer,
                    "shapefilebasinExt"=shapefile.ext,
                    "shapefilebasinRdata"=shapefilebasinRdata,
                    "shapefilebasinData"=shapefilebasinData,
                    "shapefilecountriesLayer"=shapefile.layer,
                    "shapefilecountriesExt"=shapefile.ext,
                    "shapefilecountriesRdata"=shapefilecountriesRdata,
                    "shapefilecountriesData"=shapefilebasinData,
                    "rpFileCOUT"=rpFileCOUT,
                    "historicalwaffi.files.url"=qobs.files.url,
                    "forecast.files.url"=forecast.files.url,
                    "population.density.file"=population.density.file)
  }
  
  return(appSetup)
}
## 
## ReadTimeOutput
##
ReadTimeOutput <- function(filename, dt.format = "%Y-%m-%d", hype.var = NULL, type = "df", select = NULL, nrows = -1L) {
  
  # argument checks
  if (!is.null(select) && !(1 %in% select)) {
    stop("Argument 'select' must include column 1.")
  }
  
  # handling output type user choice
  if (type == "df") {
    d.t <- F
  } else if (type %in% c("dt", "hsv")) {
    d.t <- T
  } else {
    stop(paste("Unknown type", type, "."))
  }
  
  # import subids, prepare subid attribute vector
  xattr <- readLines(filename, n = 2)
  sbd <- as.numeric(strsplit(xattr[2], split = "\t")[[1]][-1])
  if (!is.null(select)) {
    sbd <- sbd[select[-1] - 1]
  }
  
  # create select vector for fread, workaround for suspected bug in data.table (reported at https://github.com/Rdatatable/data.table/issues/2007)
  if (is.null(select)) {
    select <- 1:(length(sbd) + 1)
  }
  
  #read.table(filename, header = T, na.strings = "-9999", skip = 1)      
  x <- fread(filename,  na.strings = c("-9999", "****************"), skip = 2, sep = "\t", header = F, data.table = d.t, 
             select = select, nrows = nrows)
  #x <- fread(filename,  na.strings = c("-9999", "****************"), skip = 2, sep = "auto", header = F, data.table = d.t, 
  #           select = select, nrows = nrows)
  
  
  # read hype.var from filename, if not provided by user
  if (is.null(hype.var)) {
    hype.var <- substr(strsplit(filename, "time")[[1]][2], start = 1, stop = 4)
  }
  
  # create column names
  names(x) <- c("DATE", paste0("X", sbd))
  
  ## Date string handling, conditional on import format (HYPE allows for matlab or posix type, without or with hyphens),
  ## handles errors which might occur if the date string differs from the specified format. On error, strings are returned.
  
  # if user-requested, hop over date-time conversion
  if (!is.null(dt.format)) {
    
    # convert to posix string if possible, catch failed attempts with error condition and return string unchanged
    # conditional on class of imported data (different syntax for data.table)
    if (is.data.table(x)) {
      
      if (dt.format == "%Y-%m") {
        xd <- as.POSIXct(strptime(paste(x[, DATE], "-01", sep = ""), format = "%Y-%m-%d"), tz = "GMT")
        x[, DATE := tryCatch(na.fail(xd), error = function(e) {
          print("Date/time conversion attempt led to introduction of NAs, date/times returned as strings"); return(x[, DATE])})]
      } else if (dt.format == "%Y%m") {
        xd <- as.POSIXct(strptime(paste(x[, DATE], "-01", sep = ""), format = "%Y%m-%d"), tz = "GMT")
        x[, DATE := tryCatch(na.fail(xd), error = function(e) {
          print("Date/time conversion attempt led to introduction of NAs, date/times returned as strings"); return(x[, DATE])})]
      } else if (dt.format == "%Y") {
        xd <- as.POSIXct(strptime(paste(x[, DATE], "-01-01", sep = ""), format = "%Y-%m-%d"), tz = "GMT")
        x[, DATE := tryCatch(na.fail(xd), error = function(e) {
          print("Date/time conversion attempt led to introduction of NAs, date/times returned as strings"); return(x[, DATE])})]
      } else {
        xd <- as.POSIXct(strptime(x[, DATE], format = dt.format), tz = "GMT")
        x[, DATE := tryCatch(na.fail(xd), error = function(e) {
          print("Date/time conversion attempt led to introduction of NAs, date/times returned as strings"); return(x[, DATE])})]
      }
      
    } else {
      
      if (dt.format == "%Y-%m") {
        xd <- as.POSIXct(strptime(paste(x[, 1], "-01", sep = ""), format = "%Y-%m-%d"), tz = "GMT")
        x[, 1] <- tryCatch(na.fail(xd), error = function(e) {
          print("Date/time conversion attempt led to introduction of NAs, date/times returned as strings"); return(x[, 1])})
      } else if (dt.format == "%Y%m") {
        xd <- as.POSIXct(strptime(paste(x[, 1], "-01", sep = ""), format = "%Y%m-%d"), tz = "GMT")
        x[, 1] <- tryCatch(na.fail(xd), error = function(e) {
          print("Date/time conversion attempt led to introduction of NAs, date/times returned as strings"); return(x[, 1])})
      } else if (dt.format == "%Y") {
        xd <- as.POSIXct(strptime(paste(x[, 1], "-01-01", sep = ""), format = "%Y-%m-%d"), tz = "GMT")
        x[, 1] <- tryCatch(na.fail(xd), error = function(e) {
          print("Date/time conversion attempt led to introduction of NAs, date/times returned as strings"); return(x[, 1])})
      } else {
        xd <- as.POSIXct(strptime(x[, 1], format = dt.format), tz = "GMT")
        x[, 1] <- tryCatch(na.fail(xd), error = function(e) {
          print("Date/time conversion attempt led to introduction of NAs, date/times returned as strings"); return(x[, 1])})
      }
    }
  } else {
    # dummy date vector as there is always one needed in timestep attribute derivation below
    xd <- NA
  }
  
  # conditional on user choice: output formatting
  if (type %in% c("dt", "df")) {
    
    attr(x, which = "subid") <- sbd
    attr(x, "variable") <- toupper(hype.var)
    
    # conditional: timestep attribute identified by difference between first two entries
    tdff <- as.numeric(difftime(xd[2], xd[1], units = "hours"))
    if (!is.na(tdff)) {
      if (tdff == 24) {
        attr(x, which = "timestep") <- "day"
      } else if (tdff == 168) {
        attr(x, which = "timestep") <- "week"
      } else if (tdff %in% c(744, 720, 696, 672)) {
        attr(x, which = "timestep") <- "month"
      } else if (tdff %in% c(8760, 8784)) {
        attr(x, which = "timestep") <- "year"
      } else {
        attr(x, which = "timestep") <- paste(tdff, "hour", sep = "")
      }
    } else {
      # add timestep attribute with placeholder value
      attr(x, which = "timestep") <- "none"
    }
    
  } else {
    ## HypeSingleVar formatting
    # remove dates
    x <- x[, !"DATE", with = F]
    # convert to array (straigtht conversion to array gives error, therefore intermediate matrix)
    x <- as.array(as.matrix(x))
    # adding 'iteration' dimension
    dim(x) <- c(dim(x), 1)
    x <- HypeSingleVar(x = x, date = xd, subid = sbd, hype.var = toupper(hype.var))
  }
  
  return(x)
}

##
## ReadPTQobs
##
ReadPTQobs <- function (filename, dt.format = "%Y-%m-%d", nrows = -1) {
  
  ## import ptqobs file header, extract attribute
  # import
  xattr <- readLines(filename,n = 1)
  # extract obsids
  sbd <- as.integer(strsplit(xattr, split = "\t")[[1]][-1])
  
  # read the data
  x <- fread(filename,  na.strings = "-9999", sep = "\t", header = T, data.table = F, nrows = nrows)
  #colClasses = c("NA", rep("numeric", length(sbd))))
  
  attr(x, which = "obsid") <- sbd
  
  # date conversion 
  xd <- as.POSIXct(strptime(x[, 1], format = dt.format), tz = "GMT")
  x[, 1] <- xd
  #x[, 1] <- tryCatch(na.fail(xd), error = function(e) {
  #  print("Date/time conversion attempt led to introduction of NAs, date/times returned as strings"); return(x[, 1])
  # })
  
  return(x)
}

##Prepare mapWarningLevel files or index files
magnitude=function (appInput  = app.input, appSetput=app.setup) {
  if(app.sys=="win") {
    countries_niger.basin<-c("Benin", "Burkina Faso","Chad", "Guinea", "Ivory Coast", "Mali", "Niger", "Nigeria", "Niger Basin")
    if(appInput$alertmethod=="none") {
      if(appInput$model=="niger-hype") {
        hypeout<-ReadTimeOutput(filename = paste(appSetput$resDir, paste0("forecast_",appInput$variable,"_", model.name,".txt"), sep="/"),
                                dt.format = "%Y-%m-%d")
        if(appInput$area %in% countries_niger.basin) {
          #Read subbasin shapefile
          load(paste(appSetput$runDir, "shapefile", paste0(model.name, ".Rdata"), sep="/"))
          subbasin.shp<-shapefilebasinData
          load(paste(appSetput$runDir, "shapefile", "countries_fanfar.Rdata", sep="/"))
          countries.shp<-shapefilecountriesData
          #Extract corresponding feature according to chosen area
          if(appInput$area=="Niger Basin") {
            countries.shp.area<-countries.shp
            subbasin.shp.area<-subbasin.shp
            hypeout.area<-apply(hypeout[1:5,-1], 2, max, na.rm=T)
            hypeout.area<-cbind(sub("X", "", names(hypeout.area)), hypeout.area)
            colnames(hypeout.area)<-c("SUBID", "MaxPrecicipitation")
          } else {
            countries.shp.area<-countries.shp[countries.shp@data$CNTRY_NAME==appInput$area,]
            subbasin.shp.area<-crop(subbasin.shp,countries.shp.area)
            #Extract corresponding discharge
            mymatch3<-match(subbasin.shp.area@data$SUBID, sub("X", "", colnames(hypeout)))
            hypeout<-hypeout[,mymatch3]
            hypeout.area<-apply(hypeout[1:5, ], 2, max, na.rm=T)
            hypeout.area<-cbind(sub("X", "", names(hypeout.area)), hypeout.area)
            colnames(hypeout.area)<-c("SUBID", "MaxPrecicipitation")
          }
          write.table(hypeout.area, 
                      paste0(appSetput$runDir, "/results/001_forecast_", appInput$variable, "_", model.name, "_", appInput$area, ".txt"), 
                      sep="\t", 
                      row.names = F, 
                      quote=F)
        } else {
          print("The country is not part of the Niger Basin")
        }
      } else if(appInput$model=="ww-hype") {
        hypeout<-ReadTimeOutput(filename = paste(appSetput$resDir, paste0("forecast_",appInput$variable,"_", model.name,".txt"), sep="/"),
                                dt.format = "%Y-%m-%d")
          #Read subbasin shapefile
        load(paste(appSetput$runDir, "shapefile", paste0(model.name, ".Rdata"), sep="/"))
        subbasin.shp<-shapefilebasinData
        load(paste(appSetput$runDir, "shapefile", "countries_fanfar.Rdata", sep="/"))
        countries.shp<-shapefilecountriesData
          #Extract corresponding feature according to chosen area
          if(appInput$area=="wa") {
            countries.shp.area<-countries.shp
            subbasin.shp.area<-subbasin.shp
            hypeout.area<-apply(hypeout[1:5,-1], 2, max, na.rm=T)
            hypeout.area<-cbind(sub("X", "", names(hypeout.area)), hypeout.area)
            colnames(hypeout.area)<-c("SUBID", "MaxDischarge")
          } else {
            countries.shp.area<-countries.shp[countries.shp@data$CNTRY_NAME==appInput$area,]
            #Extract subbasin id in area
            subbasin.shp.area<-subbasin.shp[countries.shp.area,]
            subbasin.shp.area<-crop(subbasin.shp,countries.shp.area)
            #Extract corresponding warning levels classes
            mymatch3<-match(subbasin.shp.area@data$SUBID, sub("X", "", colnames(hypeout)))
            hypeout<-hypeout[,mymatch3]
            hypeout.area<-apply(hypeout[1:5, ], 2, max, na.rm=T)
            hypeout.area<-cbind(sub("X", "", names(hypeout.area)), hypeout.area)
            colnames(hypeout.area)<-c("SUBID", "MaxDischarge")
          }
        write.table(hypeout.area, 
                    paste0(appSetput$runDir, "/results/001_forecast_", appInput$variable, "_", model.name, "_", appInput$area, ".txt"), 
                    sep="\t", 
                    row.names = F, 
                    quote=F)
        } else 
          if( appInput$model=="mosaic-hype") {
            hypeout.ne<-ReadTimeOutput(filename = paste(appSetput$resDir, paste0("forecast_",appInput$variable,"_", "niger-hype",".txt"), sep="/"),
                                    dt.format = "%Y-%m-%d")
            hypeout.wa<-ReadTimeOutput(filename = paste(appSetput$resDir, paste0("forecast_",appInput$variable,"_", "ww-hype",".txt"), sep="/"),
                                    dt.format = "%Y-%m-%d")
            hypeout<-cbind(hypeout.ne,hypeout.wa[,-1])
            #Read subbasin shapefile
            load(paste(appSetput$runDir, "shapefile", paste0(model.name, ".Rdata"), sep="/"))
            subbasin.shp<-shapefilebasinData
            load(paste(appSetput$runDir, "shapefile", "countries_fanfar.Rdata", sep="/"))
            countries.shp<-shapefilecountriesData
            #Extract corresponding feature according to chosen area
            if(appInput$area=="wa") {
              countries.shp.area<-countries.shp
              subbasin.shp.area<-subbasin.shp
            } else {
              countries.shp.area<-countries.shp[countries.shp@data$CNTRY_NAME==appInput$area,]
              #Extract subbasin id in area
              #subbasin.shp.area<-subbasin.shp[countries.shp.area,]
              subbasin.shp.area<-crop(subbasin.shp,countries.shp.area)
              #Extract corresponding warning levels classes
              mymatch3<-match(subbasin.shp.area@data$SUBID, sub("X", "", colnames(hypeout)))
              hypeout<-hypeout[,mymatch3]
              hypeout.area<-apply(hypeout[1:5, ], 2, max, na.rm=T)
              hypeout.area<-cbind(sub("X", "", names(hypeout.area)), hypeout.area)
              colnames(hypeout.area)<-c("SUBID", "MaxDischarge")
            }
            write.table(hypeout.area, 
                        paste0(appSetput$runDir, "/results/001_forecast_", appInput$variable, "_", model.name, "_", appInput$area, ".txt"), 
                        sep="\t", 
                        row.names = F, 
                        quote=F)
          }
        
      }
    else if(appInput$alertmethod=="hypethreshold") {
      if(appInput$model=="niger-hype") {
        if(appInput$area %in% countries_niger.basin) {
          #Define return period magnitude
          returnPeriodMagnitudes(name.in = paste(appSetput$resDir, paste0("historical_",appInput$variable,"_", model.name,".txt"), sep="/"),
                                 name.out = paste(appSetput$resDir, paste0("001_MapWarningLevel_",appInput$variable,"_", model.name,".txt"), sep="/"),
                                 wl.rp =appInput$returnPeriodIN)
          # Define Warning level Classes for Niger HYPE
          #
          DeriveWarningClasses(timeCoutDir =appSetput$resDir,
                               timeCoutFile = paste0("forecast_",appInput$variable,"_", model.name,".txt"),
                               rp.fileDir = appSetput$resDir,
                               rp.magnitude.file =  paste0("001_MapWarningLevel_",appInput$variable,"_", model.name,".txt"),
                               OutDir = appSetput$resDir,
                               Outfile = paste0("002_MapWarningClasse_",appInput$variable,"_", model.name,".txt"),
                               lead.time=5,
                               method =appInput$alertmethod)
          Warning.classes<-read.table(paste(appSetput$resDir ,
                                                  paste0("002_MapWarningClasse_",appInput$variable,"_", model.name,".txt"), sep="/"), sep=",", h=T)
          #Read subbasin shapefile
          if(appInput$area!="Niger Basin") {
            load(paste(appSetput$runDir, "shapefile", paste0(model.name, ".Rdata"), sep="/"))
            subbasin.shp<-shapefilebasinData
            load(paste(appSetput$runDir, "shapefile", "countries_fanfar.Rdata", sep="/"))
            countries.shp<-shapefilecountriesData
            countries.shp.area<-countries.shp[countries.shp@data$CNTRY_NAME==appInput$area,]
            subbasin.shp.area<-crop(subbasin.shp,countries.shp.area)
            #Extract corresponding discharge
            mymatch3<-match(subbasin.shp.area@data$SUBID, Warning.classes$SUBID)
            Warning.classes<- Warning.classes[mymatch3,]
            write.table(Warning.classes, paste(appSetput$resDir ,
                              paste0("002_MapWarningClasse_",appInput$variable,"_", model.name, "_", appInput$area, ".txt"), sep="/"), 
                        sep=",", quote=F)
          }
          
        } else {
          print("The country is not part of the Niger Basin")
        }
        
      } 
      else if(appInput$model=="ww-hype") {
        #Define return period magnitude
        returnPeriodMagnitudes(name.in = paste(appSetput$resDir, paste0("historical_",appInput$variable,"_", model.name,".txt"), sep="/"),
                               name.out = paste(appSetput$resDir, paste0("001_MapWarningLevel_",appInput$variable,"_", model.name,".txt"), sep="/"),
                               wl.rp =appInput$returnPeriodIN)
        # Define Warning level Classes for Niger HYPE
        #
        DeriveWarningClasses(timeCoutDir =appSetput$resDir,
                             timeCoutFile = paste0("forecast_",appInput$variable,"_", model.name,".txt"),
                             rp.fileDir = appSetput$resDir,
                             rp.magnitude.file =  paste0("001_MapWarningLevel_",appInput$variable,"_", model.name,".txt"),
                             OutDir = appSetput$resDir,
                             Outfile = paste0("002_MapWarningClasse_",appInput$variable,"_", model.name,".txt"),
                             lead.time=5,
                             method =appInput$alertmethod)
        Warning.classes<-read.table(paste(appSetput$resDir ,
                                          paste0("002_MapWarningClasse_",appInput$variable,"_", model.name,".txt"), sep="/"), sep=",", h=T)
        #Read subbasin shapefile
        if(appInput$area!="wa") {
          load(paste(appSetput$runDir, "shapefile", paste0(model.name, ".Rdata"), sep="/"))
          subbasin.shp<-shapefilebasinData
          load(paste(appSetput$runDir, "shapefile", "countries_fanfar.Rdata", sep="/"))
          countries.shp<-shapefilecountriesData
          countries.shp.area<-countries.shp[countries.shp@data$CNTRY_NAME==appInput$area,]
          subbasin.shp.area<-crop(subbasin.shp,countries.shp.area)
          #Extract corresponding discharge
          mymatch3<-match(subbasin.shp.area@data$SUBID, Warning.classes$SUBID)
          Warning.classes<- Warning.classes[mymatch3,]
          write.table(Warning.classes, paste(appSetput$resDir ,
                                             paste0("002_MapWarningClasse_",appInput$variable,"_", model.name, "_", appInput$area, ".txt"), sep="/"), 
                      sep=",", quote=F)
        } 
      } 
      else if( appInput$model=="mosaic-hype") {
        #Define return period magnitude
        timecout.wa<-ReadTimeOutput(paste(appSetput$resDir,
                                          paste0("forecast_",appInput$variable,"_", "ww-hype",".txt"), sep="/"))
        timecout_niger<-ReadTimeOutput(paste(appSetput$resDir,
                                             paste0("forecast_",appInput$variable,"_", "niger-hype",".txt"), sep="/"))
        timecout_mosaic<-cbind(timecout.wa, timecout_niger[,-1])
        load(paste(appSetput$runDir, "shapefile", paste0(model.name, ".Rdata"), sep="/"))
        subbasin.shp<-shapefilebasinData
        mymatch<-match(subbasin.shp@data$SUBID,sub("X", "", colnames(timecout_mosaic)))
        timecout_mosaic<-timecout_mosaic[,c(1,mymatch)]
        colnames(timecout_mosaic)<-sub("X", "", colnames(timecout_mosaic))
        #Inclure une fonction pour sauver le timeCOUT au format de HYPE
        write.table(timecout_mosaic, paste(appSetput$resDir, "forecast_timeCOUT_mosai-hype.txt", sep="/"),
                    sep="\t", col.names=T, row.names=F, quote=F)
        returnPeriodMagnitudes(name.in = paste(appSetput$resDir, paste0("historical_",appInput$variable,"_", "niger-hype",".txt"), sep="/"),
                               name.out = paste(appSetput$resDir, paste0("001_MapWarningLevel_",appInput$variable,"_", "niger-hype",".txt"), sep="/"),
                               wl.rp =appInput$returnPeriodIN)
        returnPeriodMagnitudes(name.in = paste(appSetput$resDir, paste0("historical_",appInput$variable,"_", "ww-hype",".txt"), sep="/"),
                               name.out = paste(appSetput$resDir, paste0("001_MapWarningLevel_",appInput$variable,"_", "ww-hype",".txt"), sep="/"),
                               wl.rp =appInput$returnPeriodIN)
        # Define Warning level Classes for Niger HYPE
        DeriveWarningClasses(timeCoutDir =appSetput$resDir,
                             timeCoutFile = paste0("forecast_",appInput$variable,"_", "niger-hype",".txt"),
                             rp.fileDir = appSetput$resDir,
                             rp.magnitude.file =  paste0("001_MapWarningLevel_",appInput$variable,"_", "niger-hype",".txt"),
                             OutDir = appSetput$resDir,
                             Outfile = paste0("002_MapWarningClasse_",appInput$variable,"_", "niger-hype",".txt"),
                             lead.time=5,
                             method =appInput$alertmethod)
        DeriveWarningClasses(timeCoutDir =appSetput$resDir,
                             timeCoutFile = paste0("forecast_",appInput$variable,"_", "ww-hype",".txt"),
                             rp.fileDir = appSetput$resDir,
                             rp.magnitude.file =  paste0("001_MapWarningLevel_",appInput$variable,"_", "ww-hype",".txt"),
                             OutDir = appSetput$resDir,
                             Outfile = paste0("002_MapWarningClasse_",appInput$variable,"_", "ww-hype",".txt"),
                             lead.time=5,
                             method =appInput$alertmethod)
        ## Merge the two  Warning level Classes
        Warning.classes.Niger<-read.table(paste(appSetput$resDir ,
                                                paste0("002_MapWarningClasse_",appInput$variable,"_", "niger-hype",".txt"), sep="/"), sep=",", h=T)
        Warning.classes.WA<-read.table(paste(appSetput$resDir ,
                                             paste0("002_MapWarningClasse_",appInput$variable,"_", "ww-hype",".txt"), sep="/"), sep=",", h=T)
        Warning.classes<-rbind(Warning.classes.WA,Warning.classes.Niger)
        
          load(paste(appSetput$runDir, "shapefile", paste0(model.name, ".Rdata"), sep="/"))
          subbasin.shp<-shapefilebasinData
          load(paste(appSetput$runDir, "shapefile", "countries_fanfar.Rdata", sep="/"))
          countries.shp<-shapefilecountriesData
          if(appInput$area!="wa") {
          countries.shp.area<-countries.shp[countries.shp@data$CNTRY_NAME==appInput$area,]
          } else {
            countries.shp.area=countries.shp
          }
          subbasin.shp.area<-crop(subbasin.shp,countries.shp.area)
          #Extract corresponding discharge
          mymatch3<-match(subbasin.shp.area@data$SUBID, Warning.classes$SUBID)
          Warning.classes<- Warning.classes[mymatch3,]
          write.table(Warning.classes, paste(appSetput$resDir ,
                                             paste0("002_MapWarningClasse_",appInput$variable,"_", model.name, "_", appInput$area, ".txt"), sep="/"), 
                      sep=",", quote=F)
      } 
    }
    else if(appInput$alertmethod=="historicyear") {
        if(appInput$model=="niger-hype") {
          countries_niger.basin<-c("Benin", "Burkina Faso","Chad", "Guinea", "Ivory Coast", 
                                   "Mali", "Niger", "Nigeria", "Niger Basin")
          if(appInput$area %in% countries_niger.basin) {
            if( appInput$datasource=="observedvalues") {
              #Define return period magnitude
              historicalDischargeMagnitudes(name.in = paste(appSetput$resDir, paste0("Qobs_",model.name,".txt"), sep="/"),
                                            forecast.in = paste(appSetput$resDir, paste0("forecast_",appInput$variable,"_", model.name,".txt"), sep="/"),
                                            name.out = paste(appSetput$resDir, paste0("001_MapWarningLevel_",appInput$variable,"_", model.name,".txt"), sep="/"),
                                            wl.rp = appInput$returnPeriodIN,
                                            hist.year = appInput$yearr,
                                            datasource=appInput$datasource)
              # Define Warning level Classes for Niger HYPE
              DeriveWarningClasses(timeCoutDir =appSetput$resDir,
                                   timeCoutFile = paste0("forecast_",appInput$variable,"_", model.name,".txt"),
                                   rp.fileDir = appSetput$resDir,
                                   rp.magnitude.file =  paste0("001_MapWarningLevel_",appInput$variable,"_", model.name,".txt"),
                                   OutDir = appSetput$resDir,
                                   Outfile = paste0("002_MapWarningClasse_",appInput$variable,"_", model.name,".txt"),
                                   lead.time=5,
                                   method =appInput$alertmethod)
              
            } else if(appInput$datasource=="modeledhistorical") {
              #Define return period magnitude
              historicalDischargeMagnitudes(name.in = paste(appSetput$resDir, paste0("historical_",appInput$variable,"_", model.name,".txt"), sep="/"),
                                            forecast.in = paste(appSetput$resDir, paste0("forecast_",appInput$variable,"_", model.name,".txt"), sep="/"),
                                            name.out = paste(appSetput$resDir, paste0("001_MapWarningLevel_",appInput$variable,"_", model.name,".txt"), sep="/"),
                                            wl.rp = appInput$returnPeriodIN,
                                            hist.year = appInput$yearr,
                                            datasource=appInput$datasource)
              # Define Warning level Classes for Niger HYPE
              DeriveWarningClasses(timeCoutDir =appSetput$resDir,
                                   timeCoutFile = paste0("forecast_",appInput$variable,"_", model.name,".txt"),
                                   rp.fileDir = appSetput$resDir,
                                   rp.magnitude.file =  paste0("001_MapWarningLevel_",appInput$variable,"_", model.name,".txt"),
                                   OutDir = appSetput$resDir,
                                   Outfile = paste0("002_MapWarningClasse_",appInput$variable,"_", model.name,".txt"),
                                   lead.time=5,
                                   method =appInput$alertmethod)
            }
            
            #Read subbasin shapefile
            if(appInput$area!="Niger Basin") {
              Warning.classes<-read.table(paste(appSetput$resDir ,
                                                paste0("002_MapWarningClasse_",appInput$variable,"_", model.name,".txt"), sep="/"), sep=",", h=T)
              load(paste(appSetput$runDir, "shapefile", paste0(model.name, ".Rdata"), sep="/"))
              subbasin.shp<-shapefilebasinData
              load(paste(appSetput$runDir, "shapefile", "countries_fanfar.Rdata", sep="/"))
              countries.shp<-shapefilecountriesData
              countries.shp.area<-countries.shp[countries.shp@data$CNTRY_NAME==appInput$area,]
              subbasin.shp.area<-crop(subbasin.shp,countries.shp.area)
              #Extract corresponding discharge
              mymatch3<-match(subbasin.shp.area@data$SUBID, Warning.classes$SUBID)
              Warning.classes<- Warning.classes[mymatch3,]
              write.table(Warning.classes, paste(appSetput$resDir ,
                                                 paste0("002_MapWarningClasse_",appInput$variable,"_", model.name, "_", appInput$area, ".txt"), sep="/"), 
                          sep=",", quote=F)
            }
          } else {
              print("The country is not part of the Niger Basin")
          }
        } else if(appInput$model=="ww-hype") {
          if( appInput$datasource=="observedvalues") {
            #Define return period magnitude
            historicalDischargeMagnitudes(name.in = paste(appSetput$resDir, paste0("Qobs_",model.name,".txt"), sep="/"),
                                          forecast.in = paste(appSetput$resDir, paste0("forecast_",appInput$variable,"_", model.name,".txt"), sep="/"),
                                          name.out = paste(appSetput$resDir, paste0("001_MapWarningLevel_",appInput$variable,"_", model.name,".txt"), sep="/"),
                                          wl.rp = appInput$returnPeriodIN,
                                          hist.year = appInput$yearr,
                                          datasource=appInput$datasource)
            # Define Warning level Classes for Niger HYPE
            DeriveWarningClasses(timeCoutDir =appSetput$resDir,
                                 timeCoutFile = paste0("forecast_",appInput$variable,"_", model.name,".txt"),
                                 rp.fileDir = appSetput$resDir,
                                 rp.magnitude.file =  paste0("001_MapWarningLevel_",appInput$variable,"_", model.name,".txt"),
                                 OutDir = appSetput$resDir,
                                 Outfile = paste0("002_MapWarningClasse_",appInput$variable,"_", model.name,".txt"),
                                 lead.time=5,
                                 method =appInput$alertmethod)
            
          }
          else if(appInput$datasource=="modeledhistorical") {
            #Define return period magnitude
            historicalDischargeMagnitudes(name.in = paste(appSetput$resDir, paste0("historical_",appInput$variable,"_", model.name,".txt"), sep="/"),
                                          forecast.in = paste(appSetput$resDir, paste0("forecast_",appInput$variable,"_", model.name,".txt"), sep="/"),
                                          name.out = paste(appSetput$resDir, paste0("001_MapWarningLevel_",appInput$variable,"_", model.name,".txt"), sep="/"),
                                          wl.rp = appInput$returnPeriodIN,
                                          hist.year = appInput$yearr,
                                          datasource=appInput$datasource)
            # Define Warning level Classes for Niger HYPE
            DeriveWarningClasses(timeCoutDir =appSetput$resDir,
                                 timeCoutFile = paste0("forecast_",appInput$variable,"_", model.name,".txt"),
                                 rp.fileDir = appSetput$resDir,
                                 rp.magnitude.file =  paste0("001_MapWarningLevel_",appInput$variable,"_", model.name,".txt"),
                                 OutDir = appSetput$resDir,
                                 Outfile = paste0("002_MapWarningClasse_",appInput$variable,"_", model.name,".txt"),
                                 lead.time=5,
                                 method =appInput$alertmethod)
          }
          
          #Read subbasin shapefile
          if(appInput$area!="wa") {
            Warning.classes<-read.table(paste(appSetput$resDir ,
                                              paste0("002_MapWarningClasse_",appInput$variable,"_", model.name,".txt"), sep="/"), sep=",", h=T)
            load(paste(appSetput$runDir, "shapefile", paste0(model.name, ".Rdata"), sep="/"))
            subbasin.shp<-shapefilebasinData
            load(paste(appSetput$runDir, "shapefile", "countries_fanfar.Rdata", sep="/"))
            countries.shp<-shapefilecountriesData
            countries.shp.area<-countries.shp[countries.shp@data$CNTRY_NAME==appInput$area,]
            subbasin.shp.area<-crop(subbasin.shp,countries.shp.area)
            #Extract corresponding discharge
            mymatch3<-match(subbasin.shp.area@data$SUBID, Warning.classes$SUBID)
            Warning.classes<- Warning.classes[mymatch3,]
            write.table(Warning.classes, paste(appSetput$resDir ,
                                               paste0("002_MapWarningClasse_",appInput$variable,"_", model.name, "_", appInput$area, ".txt"), sep="/"), 
                        sep=",", quote=F)
          } 
        }
        else if(appInput$model=="mosaic-hype") {
            if( appInput$datasource=="observedvalues") {
              #Define return period magnitude
              historicalDischargeMagnitudes(name.in = paste(appSetput$resDir, paste0("Qobs_",model.name,".txt"), sep="/"),
                                            forecast.in = paste(appSetput$resDir, paste0("forecast_",appInput$variable,"_", model.name,".txt"), sep="/"),
                                            name.out = paste(appSetput$resDir, paste0("001_MapWarningLevel_",appInput$variable,"_", model.name,".txt"), sep="/"),
                                            wl.rp = appInput$returnPeriodIN,
                                            hist.year = appInput$yearr,
                                            datasource=appInput$datasource)
              # Define Warning level Classes for Niger HYPE
              DeriveWarningClasses(timeCoutDir =appSetput$resDir,
                                   timeCoutFile = paste0("forecast_",appInput$variable,"_", model.name,".txt"),
                                   rp.fileDir = appSetput$resDir,
                                   rp.magnitude.file =  paste0("001_MapWarningLevel_",appInput$variable,"_", model.name,".txt"),
                                   OutDir = appSetput$resDir,
                                   Outfile = paste0("002_MapWarningClasse_",appInput$variable,"_", model.name,".txt"),
                                   lead.time=5,
                                   method =appInput$alertmethod)
              
            } else if(appInput$datasource=="modeledhistorical") {
              #Define return period magnitude
              historicalDischargeMagnitudes(name.in = paste(appSetput$resDir, paste0("historical_",appInput$variable,"_", "niger-hype",".txt"), sep="/"),
                                            forecast.in = paste(appSetput$resDir, paste0("forecast_",appInput$variable,"_", "niger-hype",".txt"), sep="/"),
                                            name.out = paste(appSetput$resDir, paste0("001_MapWarningLevel_",appInput$variable,"_", "niger-hype",".txt"), sep="/"),
                                            wl.rp = appInput$returnPeriodIN,
                                            hist.year = appInput$yearr,
                                            datasource=appInput$datasource)
              historicalDischargeMagnitudes(name.in = paste(appSetput$resDir, paste0("historical_",appInput$variable,"_", "ww-hype",".txt"), sep="/"),
                                            forecast.in = paste(appSetput$resDir, paste0("forecast_",appInput$variable,"_", "ww-hype",".txt"), sep="/"),
                                            name.out = paste(appSetput$resDir, paste0("001_MapWarningLevel_",appInput$variable,"_", "ww-hype",".txt"), sep="/"),
                                            wl.rp = appInput$returnPeriodIN,
                                            hist.year = appInput$yearr,
                                            datasource=appInput$datasource)
              # Define Warning level Classes for Niger HYPE
              DeriveWarningClasses(timeCoutDir =appSetput$resDir,
                                   timeCoutFile = paste0("forecast_",appInput$variable,"_", "niger-hype",".txt"),
                                   rp.fileDir = appSetput$resDir,
                                   rp.magnitude.file =  paste0("001_MapWarningLevel_",appInput$variable,"_", "niger-hype",".txt"),
                                   OutDir = appSetput$resDir,
                                   Outfile = paste0("002_MapWarningClasse_",appInput$variable,"_", "niger-hype",".txt"),
                                   lead.time=5,
                                   method =appInput$alertmethod)
              DeriveWarningClasses(timeCoutDir =appSetput$resDir,
                                   timeCoutFile = paste0("forecast_",appInput$variable,"_", "ww-hype",".txt"),
                                   rp.fileDir = appSetput$resDir,
                                   rp.magnitude.file =  paste0("001_MapWarningLevel_",appInput$variable,"_", "ww-hype",".txt"),
                                   OutDir = appSetput$resDir,
                                   Outfile = paste0("002_MapWarningClasse_",appInput$variable,"_", "ww-hype",".txt"),
                                   lead.time=5,
                                   method =appInput$alertmethod)
              ## Merge the two  Warning level Classes
              Warning.classes.Niger<-read.table(paste(appSetput$resDir ,
                                                      paste0("002_MapWarningClasse_",appInput$variable,"_", "niger-hype",".txt"), sep="/"), sep=",", h=T)
              Warning.classes.WA<-read.table(paste(appSetput$resDir ,
                                                   paste0("002_MapWarningClasse_",appInput$variable,"_", "ww-hype",".txt"), sep="/"), sep=",", h=T)
              Warning.classes<-rbind(Warning.classes.WA,Warning.classes.Niger)
              
              load(paste(appSetput$runDir, "shapefile", paste0(model.name, ".Rdata"), sep="/"))
              subbasin.shp<-shapefilebasinData
              load(paste(appSetput$runDir, "shapefile", "countries_fanfar.Rdata", sep="/"))
              countries.shp<-shapefilecountriesData
              if(appInput$area!="wa") {
                countries.shp.area<-countries.shp[countries.shp@data$CNTRY_NAME==appInput$area,]
              } else {
                countries.shp.area=countries.shp
              }
              subbasin.shp.area<-crop(subbasin.shp,countries.shp.area)
              #Extract corresponding discharge
              mymatch3<-match(subbasin.shp.area@data$SUBID, Warning.classes$SUBID)
              Warning.classes<- Warning.classes[mymatch3,]
              write.table(Warning.classes, paste(appSetput$resDir ,
                                                 paste0("002_MapWarningClasse_",appInput$variable,"_", model.name, "_", appInput$area, ".txt"), sep="/"), 
                          sep=",", quote=F)
            }
          }
      } 
    else if(appInput$alertmethod=="waffi") {
      if(appInput$variable!="timeCPRC") {
        print("This method only applies to precipitation")
      } else {
        if(appInput$model=="niger-hype") {
          if(appInput$area %in% countries_niger.basin) {
            #Define Warning level Classes for Niger HYPE
            ComputeEpicIndex(historical.dir = appSetput$resDir,
                             historical.file =paste0("historical_",appInput$variable,"_", model.name,".txt"),
                             forecast.dir = appSetput$resDir,
                             forecast.file = paste0("forecast_",appInput$variable,"_", model.name,".txt"),
                             outdir = appSetput$resDir,
                             outfile = paste0("000_forecast_","timeWAFFI", "_", appInput$model,".txt"))
            returnPeriodMagnitudes_waffi(name.in = paste(appSetput$resDir, paste0("historical_","timeWAFFI", "_", appInput$model,".txt"), sep="/"),
                                         name.out = paste(appSetput$resDir, paste0("001_MapWarningLevel_","timeWAFFI","_", model.name,".txt"), sep="/"),
                                         wl.rp =appInput$returnPeriodIN)
            # Define Warning level Classes for Niger HYPE
            DeriveWarningClasses(timeCoutDir =appSetput$resDir,
                                 timeCoutFile = paste0("000_forecast_","timeWAFFI", "_", appInput$model,".txt"),
                                 rp.fileDir = appSetput$resDir,
                                 rp.magnitude.file =  paste0("001_MapWarningLevel_","timeWAFFI","_", model.name,".txt"),
                                 OutDir = appSetput$resDir,
                                 Outfile = paste0("002_MapWarningClasse_","timeWAFFI","_", model.name,".txt"),
                                 lead.time=5,
                                 method =appInput$alertmethod)
            Warning.classes<-read.table(paste(appSetput$resDir ,
                                              paste0("002_MapWarningClasse_","timeWAFFI","_", model.name,".txt"), sep="/"), sep=",", h=T)
            #Read subbasin shapefile
            if(appInput$area!="Niger Basin") {
              load(paste(appSetput$runDir, "shapefile", paste0(model.name, ".Rdata"), sep="/"))
              subbasin.shp<-shapefilebasinData
              load(paste(appSetput$runDir, "shapefile", "countries_fanfar.Rdata", sep="/"))
              countries.shp<-shapefilecountriesData
              countries.shp.area<-countries.shp[countries.shp@data$CNTRY_NAME==appInput$area,]
              subbasin.shp.area<-crop(subbasin.shp,countries.shp.area)
              #Extract corresponding discharge
              mymatch3<-match(subbasin.shp.area@data$SUBID, Warning.classes$SUBID)
              Warning.classes<- Warning.classes[mymatch3,]
              write.table(Warning.classes, paste(appSetput$resDir ,
                                                 paste0("002_MapWarningClasse_",appInput$variable,"_", model.name, "_", appInput$area, ".txt"), sep="/"), 
                          sep=",", quote=F)
          } 
        }else {
            print("The country is not part of the Niger Basin")
          }
            
          } 
        else if(appInput$model=="ww-hype") {
          #Define Warning level Classes for Niger HYPE
          ComputeEpicIndex(historical.dir = appSetput$resDir,
                           historical.file =paste0("historical_",appInput$variable,"_", model.name,".txt"),
                           forecast.dir = appSetput$resDir,
                           forecast.file = paste0("forecast_",appInput$variable,"_", model.name,".txt"),
                           outdir = appSetput$resDir,
                           outfile = paste0("000_forecast_","timeWAFFI", "_", appInput$model,".txt"))
          returnPeriodMagnitudes_waffi(name.in = paste(appSetput$resDir, paste0("historical_","timeWAFFI", "_", appInput$model,".txt"), sep="/"),
                                       name.out = paste(appSetput$resDir, paste0("001_MapWarningLevel_","timeWAFFI","_", model.name,".txt"), sep="/"),
                                       wl.rp =appInput$returnPeriodIN)
          # Define Warning level Classes for Niger HYPE
          DeriveWarningClasses(timeCoutDir =appSetput$resDir,
                               timeCoutFile = paste0("000_forecast_","timeWAFFI", "_", appInput$model,".txt"),
                               rp.fileDir = appSetput$resDir,
                               rp.magnitude.file =  paste0("001_MapWarningLevel_","timeWAFFI","_", model.name,".txt"),
                               OutDir = appSetput$resDir,
                               Outfile = paste0("002_MapWarningClasse_","timeWAFFI","_", model.name,".txt"),
                               lead.time=5,
                               method =appInput$alertmethod)
          Warning.classes<-read.table(paste(appSetput$resDir ,
                                            paste0("002_MapWarningClasse_","timeWAFFI","_", model.name,".txt"), sep="/"), sep=",", h=T)
          #Read subbasin shapefile
          if(appInput$area!="wa") {
            load(paste(appSetput$runDir, "shapefile", paste0(model.name, ".Rdata"), sep="/"))
            subbasin.shp<-shapefilebasinData
            load(paste(appSetput$runDir, "shapefile", "countries_fanfar.Rdata", sep="/"))
            countries.shp<-shapefilecountriesData
            countries.shp.area<-countries.shp[countries.shp@data$CNTRY_NAME==appInput$area,]
            subbasin.shp.area<-crop(subbasin.shp,countries.shp.area)
            #Extract corresponding discharge
            mymatch3<-match(subbasin.shp.area@data$SUBID, Warning.classes$SUBID)
            Warning.classes<- Warning.classes[mymatch3,]
            write.table(Warning.classes, paste(appSetput$resDir ,
                                               paste0("002_MapWarningClasse_",appInput$variable,"_", model.name, "_", appInput$area, ".txt"), sep="/"), 
                        sep=",", quote=F)
        } 
          } else if(appInput$model=="mosaic-hype") {
            #Define Warning level Classes for Niger HYPE
            ComputeEpicIndex(historical.dir = appSetput$resDir,
                             historical.file =paste0("historical_",appInput$variable,"_", "niger-hype",".txt"),
                             forecast.dir = appSetput$resDir,
                             forecast.file = paste0("forecast_",appInput$variable,"_", "niger-hype",".txt"),
                             outdir = appSetput$resDir,
                             outfile = paste0("000_forecast_","timeWAFFI", "_", "niger-hype",".txt"))
            ComputeEpicIndex(historical.dir = appSetput$resDir,
                             historical.file =paste0("historical_",appInput$variable,"_", "ww-hype",".txt"),
                             forecast.dir = appSetput$resDir,
                             forecast.file = paste0("forecast_",appInput$variable,"_", "ww-hype",".txt"),
                             outdir = appSetput$resDir,
                             outfile = paste0("000_forecast_","timeWAFFI", "_", "ww-hype",".txt"))
            returnPeriodMagnitudes_waffi(name.in = paste(appSetput$resDir, paste0("historical_","timeWAFFI", "_", "niger-hype",".txt"), sep="/"),
                                         name.out = paste(appSetput$resDir, paste0("001_MapWarningLevel_","timeWAFFI","_", "niger-hype",".txt"), sep="/"),
                                         wl.rp =appInput$returnPeriodIN)
            returnPeriodMagnitudes_waffi(name.in = paste(appSetput$resDir, paste0("historical_","timeWAFFI", "_", "ww-hype",".txt"), sep="/"),
                                         name.out = paste(appSetput$resDir, paste0("001_MapWarningLevel_","timeWAFFI","_", "ww-hype",".txt"), sep="/"),
                                         wl.rp =appInput$returnPeriodIN)
            # Define Warning level Classes for Niger HYPE
            DeriveWarningClasses(timeCoutDir =appSetput$resDir,
                                 timeCoutFile = paste0("000_forecast_","timeWAFFI", "_", "niger-hype",".txt"),
                                 rp.fileDir = appSetput$resDir,
                                 rp.magnitude.file =  paste0("001_MapWarningLevel_","timeWAFFI","_", "niger-hype",".txt"),
                                 OutDir = appSetput$resDir,
                                 Outfile = paste0("002_MapWarningClasse_","timeWAFFI","_", "niger-hype",".txt"),
                                 lead.time=5,
                                 method =appInput$alertmethod)
            DeriveWarningClasses(timeCoutDir =appSetput$resDir,
                                 timeCoutFile = paste0("000_forecast_","timeWAFFI", "_", "ww-hype",".txt"),
                                 rp.fileDir = appSetput$resDir,
                                 rp.magnitude.file =  paste0("001_MapWarningLevel_","timeWAFFI","_", "ww-hype",".txt"),
                                 OutDir = appSetput$resDir,
                                 Outfile = paste0("002_MapWarningClasse_","timeWAFFI","_", "ww-hype",".txt"),
                                 lead.time=5,
                                 method =appInput$alertmethod)
            ## Merge the two  Warning level Classes
            Warning.classes.Niger<-read.table(paste(appSetput$resDir ,
                                                    paste0("002_MapWarningClasse_","timeWAFFI","_", "niger-hype",".txt"), sep="/"), sep=",", h=T)
            Warning.classes.WA<-read.table(paste(appSetput$resDir ,
                                                 paste0("002_MapWarningClasse_","timeWAFFI","_", "ww-hype",".txt"), sep="/"), sep=",", h=T)
            Warning.classes<-rbind(Warning.classes.WA,Warning.classes.Niger)
            
            load(paste(appSetput$runDir, "shapefile", paste0(model.name, ".Rdata"), sep="/"))
            subbasin.shp<-shapefilebasinData
            load(paste(appSetput$runDir, "shapefile", "countries_fanfar.Rdata", sep="/"))
            countries.shp<-shapefilecountriesData
            if(appInput$area!="wa") {
              countries.shp.area<-countries.shp[countries.shp@data$CNTRY_NAME==appInput$area,]
            } else {
              countries.shp.area=countries.shp
            }
            subbasin.shp.area<-crop(subbasin.shp,countries.shp.area)
            #Extract corresponding discharge
            mymatch3<-match(subbasin.shp.area@data$SUBID, Warning.classes$SUBID)
            Warning.classes<- Warning.classes[mymatch3,]
            write.table(Warning.classes, paste(appSetput$resDir ,
                                               paste0("002_MapWarningClasse_","timeWAFFI","_", model.name, "_", appInput$area, ".txt"), sep="/"), 
                        sep=",", quote=F)
        }
      }
    }
    else if(appInput$alertmethod=="observationthreshold") {
      if( appInput$model=="niger-hype") {
        countries_niger.basin<-c("Benin", "Burkina Faso","Chad", "Guinea", "Ivory Coast", 
                                 "Mali", "Niger", "Nigeria", "Niger Basin")
        if(appInput$area %in% countries_niger.basin) {
          #Define return period magnitude
          returnPeriodMagnitudes.hist(name.in =paste(appSetput$resDir, paste0("Qobs_",model.name,".txt"), sep="/"),
                                      forecast.in = paste(appSetput$resDir, paste0("forecast_",appInput$variable,"_", model.name,".txt"), sep="/"),
                                      name.out =paste(appSetput$resDir, paste0("001_MapWarningLevel_",appInput$variable,"_", model.name,".txt"), sep="/"),
                                      wl.rp = appInput$returnPeriodIN)
          # Define Warning level Classes for Niger HYPE
          DeriveWarningClasses(timeCoutDir =appSetput$resDir,
                               timeCoutFile = paste0("forecast_",appInput$variable,"_", model.name,".txt"),
                               rp.fileDir = appSetput$resDir,
                               rp.magnitude.file =  paste0("001_MapWarningLevel_",appInput$variable,"_", model.name,".txt"),
                               OutDir = appSetput$resDir,
                               Outfile = paste0("002_MapWarningClasse_",appInput$variable,"_", model.name,".txt"),
                               lead.time=5,
                               method =appInput$alertmethod)
          Warning.classes<-read.table(paste(appSetput$resDir ,
                                            paste0("002_MapWarningClasse_",appInput$variable,"_", model.name,".txt"), sep="/"), sep=",", h=T)
          #Read subbasin shapefile
          if(appInput$area!="Niger Basin") {
            load(paste(appSetput$runDir, "shapefile", paste0(model.name, ".Rdata"), sep="/"))
            subbasin.shp<-shapefilebasinData
            load(paste(appSetput$runDir, "shapefile", "countries_fanfar.Rdata", sep="/"))
            countries.shp<-shapefilecountriesData
            countries.shp.area<-countries.shp[countries.shp@data$CNTRY_NAME==appInput$area,]
            subbasin.shp.area<-crop(subbasin.shp,countries.shp.area)
            #Extract corresponding discharge
            mymatch3<-match(subbasin.shp.area@data$SUBID, Warning.classes$SUBID)
            Warning.classes<- Warning.classes[mymatch3,]
            write.table(Warning.classes, paste(appSetput$resDir ,
                                               paste0("002_MapWarningClasse_",appInput$variable,"_", model.name, "_", appInput$area, ".txt"), sep="/"), 
                        sep=",", quote=F)
          
        } 
        } else {
          print("The country is not part of the Niger Basin")
        }
      
      } else if(appInput$model=="ww-hype") {
        #Define return period magnitude
        returnPeriodMagnitudes.hist(name.in =paste(appSetput$resDir, paste0("Qobs_",model.name,".txt"), sep="/"),
                                    forecast.in = paste(appSetput$resDir, paste0("forecast_",appInput$variable,"_", model.name,".txt"), sep="/"),
                                    name.out =paste(appSetput$resDir, paste0("001_MapWarningLevel_",appInput$variable,"_", model.name,".txt"), sep="/"),
                                    wl.rp = appInput$returnPeriodIN)
        # Define Warning level Classes for Niger HYPE
        DeriveWarningClasses(timeCoutDir =appSetput$resDir,
                             timeCoutFile = paste0("forecast_",appInput$variable,"_", model.name,".txt"),
                             rp.fileDir = appSetput$resDir,
                             rp.magnitude.file =  paste0("001_MapWarningLevel_",appInput$variable,"_", model.name,".txt"),
                             OutDir = appSetput$resDir,
                             Outfile = paste0("002_MapWarningClasse_",appInput$variable,"_", model.name,".txt"),
                             lead.time=5,
                             method =appInput$alertmethod)
        Warning.classes<-read.table(paste(appSetput$resDir ,
                                          paste0("002_MapWarningClasse_",appInput$variable,"_", model.name,".txt"), sep="/"), sep=",", h=T)
        #Read subbasin shapefile
        if(appInput$area!="wa") {
          load(paste(appSetput$runDir, "shapefile", paste0(model.name, ".Rdata"), sep="/"))
          subbasin.shp<-shapefilebasinData
          load(paste(appSetput$runDir, "shapefile", "countries_fanfar.Rdata", sep="/"))
          countries.shp<-shapefilecountriesData
          countries.shp.area<-countries.shp[countries.shp@data$CNTRY_NAME==appInput$area,]
          subbasin.shp.area<-crop(subbasin.shp,countries.shp.area)
          #Extract corresponding discharge
          mymatch3<-match(subbasin.shp.area@data$SUBID, Warning.classes$SUBID)
          Warning.classes<- Warning.classes[mymatch3,]
          write.table(Warning.classes, paste(appSetput$resDir ,
                                             paste0("002_MapWarningClasse_",appInput$variable,"_", model.name, "_", appInput$area, ".txt"), sep="/"), 
                      sep=",", quote=F)
          
        } 
      } else if(appInput$model=="mosaic-hype") {
        #Define return period magnitude
        returnPeriodMagnitudes.hist(name.in =paste(appSetput$resDir, paste0("Qobs_","niger-hype",".txt"), sep="/"),
                                    forecast.in = paste(appSetput$resDir, paste0("forecast_",appInput$variable,"_", "niger-hype",".txt"), sep="/"),
                                    name.out =paste(appSetput$resDir, paste0("001_MapWarningLevel_",appInput$variable,"_", "niger-hype",".txt"), sep="/"),
                                    wl.rp = appInput$returnPeriodIN)
        #Define return period magnitude
        returnPeriodMagnitudes.hist(name.in =paste(appSetput$resDir, paste0("Qobs_","ww-hype",".txt"), sep="/"),
                                    forecast.in = paste(appSetput$resDir, paste0("forecast_",appInput$variable,"_", "ww-hype",".txt"), sep="/"),
                                    name.out =paste(appSetput$resDir, paste0("001_MapWarningLevel_",appInput$variable,"_", "ww-hype",".txt"), sep="/"),
                                    wl.rp = appInput$returnPeriodIN)
        # Define Warning level Classes for Niger HYPE
        DeriveWarningClasses(timeCoutDir =appSetput$resDir,
                             timeCoutFile = paste0("forecast_",appInput$variable,"_", "niger-hype",".txt"),
                             rp.fileDir = appSetput$resDir,
                             rp.magnitude.file =  paste0("001_MapWarningLevel_",appInput$variable,"_", "niger-hype",".txt"),
                             OutDir = appSetput$resDir,
                             Outfile = paste0("002_MapWarningClasse_",appInput$variable,"_", "niger-hype",".txt"),
                             lead.time=5,
                             method =appInput$alertmethod)
        DeriveWarningClasses(timeCoutDir =appSetput$resDir,
                             timeCoutFile = paste0("forecast_",appInput$variable,"_", "ww-hype",".txt"),
                             rp.fileDir = appSetput$resDir,
                             rp.magnitude.file =  paste0("001_MapWarningLevel_",appInput$variable,"_", "ww-hype",".txt"),
                             OutDir = appSetput$resDir,
                             Outfile = paste0("002_MapWarningClasse_",appInput$variable,"_", "ww-hype",".txt"),
                             lead.time=5,
                             method =appInput$alertmethod)
        ## Merge the two  Warning level Classes
        Warning.classes.Niger<-read.table(paste(appSetput$resDir ,
                                                paste0("002_MapWarningClasse_",appInput$variable,"_", "niger-hype",".txt"), sep="/"), sep=",", h=T)
        Warning.classes.WA<-read.table(paste(appSetput$resDir ,
                                             paste0("002_MapWarningClasse_",appInput$variable,"_", "ww-hype",".txt"), sep="/"), sep=",", h=T)
        Warning.classes<-rbind(Warning.classes.WA,Warning.classes.Niger)
        
        load(paste(appSetput$runDir, "shapefile", paste0(model.name, ".Rdata"), sep="/"))
        subbasin.shp<-shapefilebasinData
        load(paste(appSetput$runDir, "shapefile", "countries_fanfar.Rdata", sep="/"))
        countries.shp<-shapefilecountriesData
        if(appInput$area!="wa") {
          countries.shp.area<-countries.shp[countries.shp@data$CNTRY_NAME==appInput$area,]
        } else {
          countries.shp.area=countries.shp
        }
        subbasin.shp.area<-crop(subbasin.shp,countries.shp.area)
        #Extract corresponding discharge
        mymatch3<-match(subbasin.shp.area@data$SUBID, Warning.classes$SUBID)
        Warning.classes<- Warning.classes[mymatch3,]
        write.table(Warning.classes, paste(appSetput$resDir ,
                                           paste0("002_MapWarningClasse_",appInput$variable,"_", model.name, "_", appInput$area, ".txt"), sep="/"), 
                    sep=",", quote=F)
     }
    } 
    else if (appInput$alertmethod=="userthreshold") {
      
    }
  }
}


## -------------------------------------------------------------------------------
## prepare application outputs
prepareHypeAppsOutput<-function(appSetup  = app.setup, appInput = app.input){
  
  # Create folder for data to be published
  outDir = paste(appSetup$tmpDir,'output',sep="/")
  dir.create(outDir,recursive = T,showWarnings = F)
  
  ## Post-process requested outputs (copy some files...)
  if(appSetup$appName=="historical"){
    ## list all files in output folder:
    outFiles=dir(outDir,all.files = F,full.names = T,recursive = F)
  }
   ## list all files in output folders:
    outFiles=dir(outDir[1],all.files = F,full.names = T,recursive = F)
    outFiles=c(outFiles,dir(outDir[2],all.files = F,full.names = T,recursive = F))
  }


# functions for application logfile that will be published as part of application results
appLogOpen<-function(appName,tmpDir,appDate,prefix=NULL){
  fileName=paste(appDate,"_","hypeapps-",appName,".log",sep="")
  if(!is.null(prefix)){
    fileName = paste(prefix,"_",fileName,sep="")
  }
  fileName = paste(tmpDir,"/",fileName,sep="")
  fileConn<-file(fileName,open="wt")
  writeLines(paste("hypeapps-",appName," starting, ",as.character(date()),sep=""),fileConn)
  return(list("fileName"=fileName,"fileConn"=fileConn))
}
appLogWrite<-function(logText,fileConn){
  writeLines(paste(logText,", ",as.character(date()),sep=""),fileConn)
  return(0)
}
appLogClose<-function(appName,fileConn){
  writeLines(paste("hypeapps-",appName," ending, ",as.character(date()),sep=""),fileConn)
  close(fileConn)
  return(0)
}


# internal log succesful sourcing of file
if(app.sys=="tep"){rciop.log ("DEBUG", paste("all functions sourced"), "/util/R/hypeapps-utils.R")}


