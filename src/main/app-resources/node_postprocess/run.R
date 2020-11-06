#!/opt/anaconda/envs/postprocessing-env/bin/Rscript --vanilla --slave --quiet

##### #!/opt/anaconda/bin/Rscript --vanilla --slave --quiet
##### #!/usr/bin/Rscript --vanilla --slave --quiet

# run 'which Rscript' in the terminal. If the path differs you may have to change the first line above.
# When using certain R packages, call Rscript in the appropriate conda environment with the appropriate source code file.

#
# /hypeapps-forecast/src/main/app-resources/node_postprocess/run.R
#

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

# Workflow overview:
# ------------------
# 1 Initialization          (load environmental variables, libraries, utility functions, etc)
# 2 Application inputs      (read all user inputs)
# 3 Produce        
# 4 Prepare mapWarningLevel files or index files
# 5 Produce impact map
# 6 Generate pdf and html files for emails
# 7 Output                  (prepare and publish output data)
# 8 End of workflow

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

## use R installed in cairo-env
use.r.cairoenv = FALSE # TRUE
use.r.cairoenv = use.r.cairoenv && app.sys == "tep"

## ------------------------------------------------------------------------------
## load rciop package and set working directory to TMPDIR when running on TEP 
if(app.sys=="tep"){
  library("rciop")
  
  rciop.log ("DEBUG", " *** hypeapps-forecast *** TEP hydrological modelling applications ***", "/node_postprocess/run.R")
  rciop.log ("DEBUG", " rciop library loaded", "/node_postprocess/run.R")
  
  setwd(TMPDIR)
  rciop.log("DEBUG", paste(" R session working directory set to ",TMPDIR,sep=""), "/node_postprocess/run.R")
}

# victor naslund
f<-file('stdin', 'r')
#open(f)
#row <- readLines(f, n=1)

#print (row)

while(length(input <- readLines(f, n=1)) > 0) {
    
    # print
    rciop.log("INFO", paste("processing input:", input, sep=" "))
    
    # Download the file
    res <- rciop.copy(input, TMPDIR, uncompress=TRUE)
    
    if (res$exit.code==0) {
        local.url <- res$output
    }
    
    #local.url)
    my_data <- read.delim(local.url)
    print (my_data)
}


#q()

# end victor naslund


## ------------------------------------------------------------------------------
## load hypeapps environment and additional R utility functions
if(app.sys=="tep"){
  source(paste(Sys.getenv("_CIOP_APPLICATION_PATH"), "util/R/hypeapps-environment.R",sep="/"))
  source(paste(Sys.getenv("_CIOP_APPLICATION_PATH"), "util/R/hypeapps-processing-settings.R", sep="/")) 
  source(paste(Sys.getenv("_CIOP_APPLICATION_PATH"), "util/R/hypeapps-utils.R", sep="/"))
  source(paste(Sys.getenv("_CIOP_APPLICATION_PATH"), "util/R/hypeapps-returnperiod-utils.R", sep="/"))
  source(paste(Sys.getenv("_CIOP_APPLICATION_PATH"), "util/R/hypeapps-historicalyear-utils.R", sep="/"))
  source(paste(Sys.getenv("_CIOP_APPLICATION_PATH"), "util/R/hypeapps-waffi-index-utils.R", sep="/"))
  #source(paste(Sys.getenv("_CIOP_APPLICATION_PATH"), "util/R/hypeapps-plot-warninglevel-map_windows.R", sep="/"))
  #source(paste(Sys.getenv("_CIOP_APPLICATION_PATH"), "util/R/hypeapps-plot-impact-map.R", sep="/"))
  source(paste(Sys.getenv("_CIOP_APPLICATION_PATH"), "util/R/hyppeapps-trigger-distribution.R", sep="/"))

  rciop.log ("DEBUG", paste(" libraries loaded and utilities sourced"), "/node_postprocessing/run.R")
}else if(app.sys=="win"){
  setwd("E:/AGRHYMET/FANFAR/git/post_processing_fanfar/fanfar-post-processing/src/main")
  source(paste(getwd(), "app-resources/util/R/hypeapps-environment.R", sep="/")) 
  source(paste(getwd(), "app-resources/util/R/hypeapps-processing-settings.R", sep="/")) 
  source(paste(getwd(), "app-resources/util/R/hypeapps-utils.R", sep="/"))
  source(paste(getwd(), "app-resources/util/R/hypeapps-returnperiod-utils.R", sep="/"))
  source(paste(getwd(), "app-resources/util/R/hypeapps-historicalyear-utils.R", sep="/"))
  source(paste(getwd(), "app-resources/util/R/hypeapps-waffi-index-utils.R", sep="/"))
  source(paste(getwd(), "app-resources/util/R/hypeapps-plot-warninglevel-map_windows.R", sep="/"))
  source(paste(getwd(), "app-resources/util/R/hypeapps-plot-impact-map.R", sep="/"))
  source(paste(getwd(), "app-resources/util/R/hyppeapps-trigger-distribution.R", sep="/"))
  
}
## open application logfile
logFile=appLogOpen(appName = app.name,tmpDir = getwd(),appDate = app.date,prefix="000")

#################################################################################
## 2 - Application user inputs
## ------------------------------------------------------------------------------
## application input parameters
app.input <- getHypeAppInput(appName = app.name)

if(app.sys=="tep"){rciop.log ("DEBUG", paste(" hypeapps inputs and parameters read"), "/node_postprocess/run.R")}
log.res=appLogWrite(logText = "Inputs and parameters read",fileConn = logFile$fileConn)

#################################################################################
## 3 - Application setup
## ------------------------------------------------------------------------------
## Prepare basic model setup (static input files and hype model executable copied to working folder)
app.setup <- getHypeAppSetup(modelName = model.name,
                             modelBin  = model.bin,
                             tmpDir    = app.tmp_path,
                             appDir    = app.app_path,
                             appName   = app.name,
                             appInput  = app.input,
                             modelFilesURL = model.files.url,
                             forcingArchiveURL = forcing.archive.url,
                             stateFilesURL = state.files.url,
                             stateFilesIN = state.files,
                             historical.files.url = historical.files.url,
                             historicalwaffi.files.url = historicalwaffi.files.url)

if(app.sys=="tep"){rciop.log ("DEBUG", paste("HypeApp setup read"), "/node_postprocess/run.R")}
log.res=appLogWrite(logText = "HypeApp setup read",fileConn = logFile$fileConn)

#################################################################################
## 4 - Prepare mapWarningLevel files or index files
## ------------------------------------------------------------------------------
if(use.r.cairoenv){
  # source(paste(Sys.getenv("_CIOP_APPLICATION_PATH"), "util/R/hypeapps-plot-warninglevel-map_windows.R", sep="/"))
  # source(paste(Sys.getenv("_CIOP_APPLICATION_PATH"), "util/R/hypeapps-plot-impact-map.R", sep="/"))
  # source(paste(Sys.getenv("_CIOP_APPLICATION_PATH"), "util/R/hypeapps-plot-mapoutput.R", sep="/"))
  
  # Call postprocess2.R, section 1,4-6, with R installed in the environment cairo-env
  syscmd = paste0("/opt/anaconda/envs/cairo-env/bin/Rscript"," --vanilla --slave --quiet ", Sys.getenv("_CIOP_APPLICATION_PATH"), "/node_postprocess/postprocess2.R")
  system(syscmd)
  
  # or (only an example):
  #args = paste0('--path_shapefiles',' ',app.setup$XYZ,' ','--alertmethod',' ',app.input$alertmethod)
  #system2(syscmd,args=args)
  
}else if(app.sys=="tep"){
  source(paste(Sys.getenv("_CIOP_APPLICATION_PATH"), "util/R/hypeapps-plot-warninglevel-map_windows.R", sep="/"))
  source(paste(Sys.getenv("_CIOP_APPLICATION_PATH"), "util/R/hypeapps-plot-impact-map.R", sep="/"))
  source(paste(Sys.getenv("_CIOP_APPLICATION_PATH"), "util/R/hypeapps-plot-mapoutput.R", sep="/"))
  
}else{
  source(paste(getwd(), "app-resources/util/R/hypeapps-plot-mapoutput.R", sep="/"))
}

if(! use.r.cairoenv){

  ## Alert threshold
  magnitude(appInput  = app.input, appSetput=app.setup)
  if(app.sys=="tep"){rciop.log ("DEBUG", paste("Produce return period magnitude and warning classes"), "/node_postprocessing/run.R")}
  log.res=appLogWrite(logText = "Produce return period magnitude and warning classes",fileConn = logFile$fileConn)


  #################################################################################
  ## 4 - Produce map showing the maximum risk level
  ## ------------------------------------------------------------------------------
  if( app.input$alertmethod!="none") {
    PlotRiskMap(appInput  = app.input, appSetput=app.setup)
  } else {
    if(app.input$variable=="timeCOUT") {
      varName="COUT"
      mapFile = "mapCOUT.txt" 
      crfun=colorRampPalette(c("#ede7ff", "#2300ff"))
    } else if (app.input$variable=="timeCPRC") {
      crfun <- colorRampPalette(c("#e0e7e8", "#00508c"))
      varName="CPRC"
      mapFile = "mapCOUT.txt" 
    }
    cdate=format(Sys.time(), "%Y-%m-%d_%H%M")
    edate=format(Sys.time(), "%Y-%m-%d_%H%M")
    modelName<-app.input$model
    #source(paste(getwd(), "app-resources/util/R/hypeapps-plot-mapoutput.R", sep="/"))
    PlotMapOutput(appInput  = app.input, appSetput=app.setup,  map.subid.column = 2, var.name = "",
                  col.ramp.fun = crfun,
                  plot.scale = F,
                  plot.arrow = F,
                  legend.title = varPlot,
                  legend.pos = "topleft",
                  par.mar = c(0,0,0,0),par.cex = 3,par.mai=c(0,0,0,0))
  }

  if(app.sys=="tep"){rciop.log ("DEBUG", "Produce map showing the maximum risk level", "/node_postprocessing/run.R")}
  log.res=appLogWrite(logText = "Produce map showing the maximum risk level",fileConn = logFile$fileConn)


  #################################################################################
  ## 5 - Produce impact map
  ## ------------------------------------------------------------------------------
  if( app.input$alertmethod!="none") {
    ImpactOutput_Map(appInput  = app.input, appSetput=app.setup, popmethod=1)
  }

  if(app.sys=="tep"){rciop.log ("DEBUG", "Produce impact map", "/node_postprocessing/run.R")}
  log.res=appLogWrite(logText = "Produce impact map",fileConn = logFile$fileConn)


  #################################################################################
  ## 6 - Triggers
  ## ------------------------------------------------------------------------------
  if(app.sys=="win") {
    trigger_distribution_outfiles <- TriggerDistribution(appInput  = app.input, appSetput=app.setup, dist.list.url=dist.list.url)
  } else {
    trigger_distribution_outfiles <- TriggerDistribution(appInput  = app.input, appSetput=app.setup, dist.list.url=dist.list.url)
  }

  if(app.sys=="tep"){rciop.log ("DEBUG", "Trigger distribution", "/node_postprocessing/run.R")}
  log.res=appLogWrite(logText = "Trigger distribution",fileConn = logFile$fileConn)


  #################################################################################
  ## 6 - Generate pdf and html files for emails
  system(paste0("Rscript -e rmarkdown::render('", app.setup$resDir, "/rmarkdown_fanfar.rmd','all')"))
  system("Rscript -e rmarkdown::render('E:/AGRHYMET/FANFAR/fanfar-postprocessing/RunDir/rmarkdown_fanfar.rmd','html_document')")

} # if(! use.r.cairoenv)

#################################################################################
## 7 - Output
## ------------------------------------------------------------------------------
app.outfiles <- dir(app.setup$resDir)
log.res=appLogWrite(logText = "HypeApp outputs prepared",fileConn = logFile$fileConn)

## ------------------------------------------------------------------------------
## publish postprocessed results
if(app.sys=="tep"){
#  for(k in 1:length(app.outdir)){
#    rciop.publish(path=paste(app.outdir[k],"/*",sep=""), recursive=FALSE, metalink=TRUE)
#  }
    for(k in 1:length(app.outfiles)){
      rciop.publish(path=app.outfiles[k], recursive=FALSE, metalink=TRUE)
    }
  
  log.res=appLogWrite(logText = "HypeApp outputs published",fileConn = logFile$fileConn)
}

## close and publish the logfile
log.file=appLogClose(appName = app.name,fileConn = logFile$fileConn)
if(app.sys=="tep"){
  rciop.publish(path=logFile$fileName, recursive=FALSE, metalink=TRUE)
}

#}
#################################################################################
## 8 - End of workflow
## ------------------------------------------------------------------------------
## exit with appropriate status code
q(save="no", status = 0)
