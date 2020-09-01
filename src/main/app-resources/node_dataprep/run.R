#!/opt/anaconda/bin/Rscript --vanilla --slave --quiet
#
# /hypeapps-eodata/src/main/app-resources/node_dataprep/run.R


# begin Bernard Minoungou


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

# Application 1: "FANFAR Post-processing" (hypeapps-postprocessing)
# Author:         Bernard Minoungou, AGRHYMET
# Version:        2020-05-19

#################################################################################
## 1 - Initialization
## ------------------------------------------------------------------------------
## create a date tag to include in output filenames
app.date = format(Sys.time(), "%Y%m%d_%H%M")

## set application name
app.name = "postprocessing"
## ------------------------------------------------------------------------------
## flag which environment is used, if not set
#app.sys="win"
if(!exists("app.sys")){
  app.sys ="tep"
}

## ------------------------------------------------------------------------------
## load rciop package and set working directory to TMPDIR when running on TEP 
if(app.sys=="tep"){
  library("rciop")
  
  rciop.log ("DEBUG", " *** hypeapps-postprocessing *** TEP hydrological modelling applications ***", "/node_dataprep/run.R")
  rciop.log ("DEBUG", " rciop library loaded", "/node_dataprep/run.R")
  
  setwd(TMPDIR)
  rciop.log("DEBUG", paste(" R session working directory set to ",TMPDIR,sep=""), "/node_dataprep/run.R")
}



# wrap all code in a foor loop over the stdin

# read the inputs coming from stdin
#f <- file("stdin")
#open(f)

#while(length(input <- readLines(f, n=1)) > 0) {
  
 # rciop.log("INFO", paste("processing input:", input, sep=" "))
  
  # copy the input to the process temporary folder TMPDIR
  #res <- rciop.copy(input, TMPDIR, uncompress=TRUE)
  
  #if (res$exit.code==0) local.url <- res$output
  
  #mycsv <- read.csv(local.url)
  
  # do something with the downloaded csv here in TMPDIR/output
  
  # publish the any results done 
  #rciop.publish(paste(TMPDIR,"output", sep="/"), recursive=TRUE, metalink=FALSE)
 
#}
    #print (row)

    # <property id="ciop.job.max.tasks">1</property> to the metadata


    # metalink false if we should publish the file as stdin to the next node
    #print ("begining cmd arguments")
    #rciop.publish(path=input, recursive=FALSE, metalink=FALSE)
    
    #rciop.publish(path="/application/test2.txt", recursive=FALSE, metalink=FALSE)
    
    #rciop.publish(path="/application/test3.txt", recursive=FALSE, metalink=FALSE)
    #q()
    # end victor naslund

#}

#q()



## ------------------------------------------------------------------------------
## load the R hypeapps-environment and hypeapps-utils
if(app.sys=="tep"){
  source(paste(Sys.getenv("_CIOP_APPLICATION_PATH"), "util/R/hypeapps-environment.R",sep="/"))
  source(paste(Sys.getenv("_CIOP_APPLICATION_PATH"), "util/R/hypeapps-utils.R", sep="/"))
  rciop.log ("DEBUG", paste(" libraries loaded and utilities sourced"), "/node_dataprep/run.R")
}else if(app.sys=="win"){
  setwd("E:/AGRHYMET/FANFAR/git/post_processing_fanfar/fanfar-post-processing/src/main")
  source(paste(getwd(), "app-resources/util/R/hypeapps-environment.R", sep="/"))  
  source(paste(getwd(), "app-resources/util/R/hypeapps-utils.R", sep="/"))
}
## open application logfile
logFile=appLogOpen(appName = app.name,tmpDir = getwd(),appDate = app.date, prefix = "000")
#################################################################################
## 2 - Application user inputs
## ------------------------------------------------------------------------------
## application input parameters
app.input <- getHypeAppInput(appName = app.name)

log.res=appLogWrite(logText = "Inputs and parameters read",fileConn = logFile$fileConn)

## close and publish the logfile
log.file=appLogClose(appName = app.name,fileConn = logFile$fileConn)
if(app.sys=="tep"){
  rciop.publish(path=logFile$fileName, recursive=FALSE, metalink=FALSE)
}
## ------------------------------------------------------------------------------
## exit with appropriate status code
q(save="no", status = 0)
