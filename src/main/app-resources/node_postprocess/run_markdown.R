#!/opt/anaconda/bin/Rscript --vanilla --slave --quiet
#
# /hypeapps-forecast/src/main/app-resources/node_postprocess/run.R

# begin Bernard Minoungou


# Copyright 2019-2020 SMHI
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
# 5 Prepare plot file            (run the hindcast model)
# 6 Generate pdf and html files for emails          (forecast forcing, initial state, info)
# 7 Run forecast            (run the forecast model)
# 8 Output                  (prepare and publish output data)
# 9 End of workflow

#################################################################################
## 1 - Initialization
## ------------------------------------------------------------------------------
## create a date tag to include in output filenames
app.date = format(Sys.time(), "%Y%m%d_%H%M")

## set application name
app.name = "postprocessing"
## ------------------------------------------------------------------------------
## flag which environment is used, if not set
app.sys="win"
if(!exists("app.sys")){
  app.sys ="tep"
}
## ------------------------------------------------------------------------------
## load rciop package and set working directory to TMPDIR when running on TEP 
if(app.sys=="tep"){
  library("rciop")
  
  rciop.log ("DEBUG", " *** hypeapps-forecast *** TEP hydrological modelling applications ***", "/node_postprocess/run.R")
  rciop.log ("DEBUG", " rciop library loaded", "/node_postprocess/run.R")
  
  setwd(TMPDIR)
  rciop.log("DEBUG", paste(" R session working directory set to ",TMPDIR,sep=""), "/node_postprocess/run.R")
}
if(app.sys=="tep"){
  source(paste(Sys.getenv("_CIOP_APPLICATION_PATH"), "util/R/hypeapps-environment.R",sep="/"))
  source(paste(Sys.getenv("_CIOP_APPLICATION_PATH"), "util/R/hypeapps-utils.R", sep="/"))
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
#################################################################################
## 2 - Application user inputs
## ------------------------------------------------------------------------------
## application input parameters
app.input <- getHypeAppInput(appName = app.name)

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

#################################################################################