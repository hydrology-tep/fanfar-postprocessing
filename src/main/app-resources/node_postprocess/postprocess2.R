
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


parse_args <- function(verbose=F)
{

    split_arg<-function(string)
    {
        print(string)
        if (grepl('=',string)){
            parts = strsplit(string,'=')
            key   = parts[[1]][1]
            value = parts[[1]][2]
        }else{
            key   = string
            value = NULL
        }

        return (list('key'=key,'value'=value))
    }

    # Outputs
    # arg.hindcast = F
    # arg.forecast = F
    # arg.issuedate = NULL
    arg.path_shapefiles = NULL

    args = commandArgs(trailingOnly = TRUE)

    if (length(args) > 0){
        for (i in 1:length(args)){
            x = split_arg(args[i])

            # if (x$key == '--hindcast'){
            #     arg.hindcast = T
            # }

            # if (x$key == '--forecast'){
            #     arg.forecast = T
            # }

            # if (x$key == '--issuedate'){
            #     if (! is.null(x$value)){
            #         arg.issuedate = x$value
            #     }
            # }

            if (x$key == '--path_shapefiles'){
                if (! is.null(x$value)){
                    arg.path_shapefiles = x$value
                }
            }
        } # for
    } # if

    if (verbose){
        # print(arg.hindcast)
        # print(arg.forecast)
        # print(arg.issuedate)
        print(arg.path_shapefiles)
    }

    return (list(#'hindcast'=arg.hindcast,
                 #'forecast'=arg.forecast,
                 #'issuedate'=arg.issuedate,
                 'path_shapefiles'=arg.path_shapefiles
                 ))
}


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
  
  rciop.log ("DEBUG", " *** hypeapps-forecast *** TEP hydrological modelling applications ***", "/node_postprocess/postprocess2.R")
  rciop.log ("DEBUG", " rciop library loaded", "/node_postprocess/postprocess2.R")
  
  setwd(TMPDIR)
  rciop.log("DEBUG", paste(" R session working directory set to ",TMPDIR,sep=""), "/node_postprocess/postprocess2.R")
}

# # victor naslund
# f<-file('stdin', 'r')
# #open(f)
# #row <- readLines(f, n=1)

# #print (row)

# while(length(input <- readLines(f, n=1)) > 0) {
    
#     # print
#     rciop.log("INFO", paste("processing input:", input, sep=" "))
    
#     # Download the file
#     res <- rciop.copy(input, TMPDIR, uncompress=TRUE)
    
#     if (res$exit.code==0) {
#         local.url <- res$output
#     }
    
#     #local.url)
#     my_data <- read.delim(local.url)
#     print (my_data)
# }


# #q()

# # end victor naslund


## ------------------------------------------------------------------------------
## load hypeapps environment and additional R utility functions
if(app.sys=="tep"){
  # source(paste(Sys.getenv("_CIOP_APPLICATION_PATH"), "util/R/hypeapps-environment.R",sep="/"))
  # source(paste(Sys.getenv("_CIOP_APPLICATION_PATH"), "util/R/hypeapps-processing-settings.R", sep="/")) 
  # source(paste(Sys.getenv("_CIOP_APPLICATION_PATH"), "util/R/hypeapps-utils.R", sep="/"))
  # source(paste(Sys.getenv("_CIOP_APPLICATION_PATH"), "util/R/hypeapps-returnperiod-utils.R", sep="/"))
  # source(paste(Sys.getenv("_CIOP_APPLICATION_PATH"), "util/R/hypeapps-historicalyear-utils.R", sep="/"))
  # source(paste(Sys.getenv("_CIOP_APPLICATION_PATH"), "util/R/hypeapps-waffi-index-utils.R", sep="/"))
  source(paste(Sys.getenv("_CIOP_APPLICATION_PATH"), "util/R/hypeapps-plot-warninglevel-map_windows.R", sep="/"))
  source(paste(Sys.getenv("_CIOP_APPLICATION_PATH"), "util/R/hypeapps-plot-impact-map.R", sep="/"))
  # source(paste(Sys.getenv("_CIOP_APPLICATION_PATH"), "util/R/hyppeapps-trigger-distribution.R", sep="/"))
  source(paste(Sys.getenv("_CIOP_APPLICATION_PATH"), "util/R/hypeapps-plot-mapoutput.R", sep="/"))

  rciop.log ("DEBUG", paste(" libraries loaded and utilities sourced"), "/node_postprocessing/postprocess2.R")
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
logFile=appLogOpen(appName = app.name,tmpDir = getwd(),appDate = app.date,prefix="111")

# #################################################################################
# ## 2 - Application user inputs
# ## ------------------------------------------------------------------------------
# ## application input parameters
# app.input <- getHypeAppInput(appName = app.name)

# if(app.sys=="tep"){rciop.log ("DEBUG", paste(" hypeapps inputs and parameters read"), "/node_postprocess/run.R")}
# log.res=appLogWrite(logText = "Inputs and parameters read",fileConn = logFile$fileConn)


#
# If problem running getHypeAppInput or getHypeAppSetup due to versions of R packages, either create a minimal getHypeAppSetup2 not using readOGR()
# or try to pass arguments/parameters to replace use of certain app.setup values.
#
# Arguments/Parameters from calling script
#args = parse_args()

#path_shapefiles=""
#if(! is.null(args$path_shapefiles)){
#  path_shapefiles = args$path_shapefiles
#}
# app.input = list(alertmethod=args$alertmethod,
#                  variable=args$variable)
# app.setup = list(resDir=args$resdir)


# #################################################################################
# ## 3 - Application setup
# ## ------------------------------------------------------------------------------
# ## Prepare basic model setup (static input files and hype model executable copied to working folder)
# app.setup <- getHypeAppSetup(modelName = model.name,
#                              modelBin  = model.bin,
#                              tmpDir    = app.tmp_path,
#                              appDir    = app.app_path,
#                              appName   = app.name,
#                              appInput  = app.input,
#                              modelFilesURL = model.files.url,
#                              forcingArchiveURL = forcing.archive.url,
#                              stateFilesURL = state.files.url,
#                              stateFilesIN = state.files,
#                              historical.files.url = historical.files.url,
#                              historicalwaffi.files.url = historicalwaffi.files.url)

# if(app.sys=="tep"){rciop.log ("DEBUG", paste("HypeApp setup read"), "/node_postprocess/run.R")}
# log.res=appLogWrite(logText = "HypeApp setup read",fileConn = logFile$fileConn)

#################################################################################
## 4 - Prepare mapWarningLevel files or index files
## ------------------------------------------------------------------------------
# if(app.sys!="tep"){
#   source(paste(getwd(), "app-resources/util/R/hypeapps-plot-mapoutput.R", sep="/"))
# }

## Alert threshold
magnitude(appInput  = app.input, appSetput=app.setup)
if(app.sys=="tep"){rciop.log ("DEBUG", paste("Produce return period magnitude and warning classes"), "/node_postprocessing/postprocess2.R")}
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

if(app.sys=="tep"){rciop.log ("DEBUG", "Produce map showing the maximum risk level", "/node_postprocessing/postprocess2.R")}
log.res=appLogWrite(logText = "Produce map showing the maximum risk level",fileConn = logFile$fileConn)


#################################################################################
## 5 - Produce impact map
## ------------------------------------------------------------------------------
if( app.input$alertmethod!="none") {
  ImpactOutput_Map(appInput  = app.input, appSetput=app.setup, popmethod=1)
}

if(app.sys=="tep"){rciop.log ("DEBUG", "Produce impact map", "/node_postprocessing/postprocess2.R")}
log.res=appLogWrite(logText = "Produce impact map",fileConn = logFile$fileConn)


#################################################################################
## 6 - Triggers
## ------------------------------------------------------------------------------
if(app.sys=="win") {
  trigger_distribution_outfiles <- TriggerDistribution(appInput  = app.input, appSetput=app.setup, dist.list.url=dist.list.url)
} else {
  trigger_distribution_outfiles <- TriggerDistribution(appInput  = app.input, appSetput=app.setup, dist.list.url=dist.list.url)
}

if(app.sys=="tep"){rciop.log ("DEBUG", "Trigger distribution", "/node_postprocessing/postprocess2.R")}
log.res=appLogWrite(logText = "Trigger distribution",fileConn = logFile$fileConn)


#################################################################################
## 6 - Generate pdf and html files for emails
system(paste0("Rscript -e rmarkdown::render('", app.setup$resDir, "/rmarkdown_fanfar.rmd','all')"))
system("Rscript -e rmarkdown::render('E:/AGRHYMET/FANFAR/fanfar-postprocessing/RunDir/rmarkdown_fanfar.rmd','html_document')")


# #################################################################################
# ## 7 - Output
# ## ------------------------------------------------------------------------------
# app.outfiles <- dir(app.setup$resDir)
# log.res=appLogWrite(logText = "HypeApp outputs prepared",fileConn = logFile$fileConn)

# ## ------------------------------------------------------------------------------
# ## publish postprocessed results
# if(app.sys=="tep"){
# #  for(k in 1:length(app.outdir)){
# #    rciop.publish(path=paste(app.outdir[k],"/*",sep=""), recursive=FALSE, metalink=TRUE)
# #  }
#     for(k in 1:length(app.outfiles)){
#       rciop.publish(path=app.outfiles[k], recursive=FALSE, metalink=TRUE)
#     }
  
#   log.res=appLogWrite(logText = "HypeApp outputs published",fileConn = logFile$fileConn)
# }

# ## close and publish the logfile
# log.file=appLogClose(appName = app.name,fileConn = logFile$fileConn)
# if(app.sys=="tep"){
#   rciop.publish(path=logFile$fileName, recursive=FALSE, metalink=TRUE)
# }

# #}
# #################################################################################
# ## 8 - End of workflow
# ## ------------------------------------------------------------------------------
# ## exit with appropriate status code
# q(save="no", status = 0)
