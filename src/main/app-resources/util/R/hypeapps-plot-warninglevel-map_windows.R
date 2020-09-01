#!/opt/anaconda/envs/cairo-env/bin/Rscript --vanilla --slave --quiet
#
# /hypeapps-[appName]/src/main/app-resources/util/R/hypeapps-plot-forecast-map.R
#
# Copyright 2019
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
# hypeapps-plot-warninglevel-map.R: Script to plot map with forecast warning levels, used for TEP Hydrology.
# Author:                           David Gustafsson, Jafet Andersson, SMHI & Bernard Minoungou (AGRHYMET)
# Version:                          2019-09-16
# -------------------------------------------------------------------
# -------------------------------------------------------------------
# 2 - Dependancies
# -------------------------------------------------------------------
if(app.sys=="tep") {
  library(rciop)
  library(data.table) # to read the basinoutput file
  library(Cairo)      # graphics device for output files
  library(sp)         # sp for reading Rdata file
} else {
  library(data.table) # to read the basinoutput file
  library(Cairo)      # graphics device for output files
  library(sp)         # sp for reading Rdata file
  library(prettymapr) # To add scalebar and north arrow
}
# --------------------------------------------------------------------------
# 2.3 function to write a world file to geotag an image file
# --------------------------------------------------------------------------
writeWorldFile<-function(fileName, pxWidth, pxHeight, degWidth, degHeight, lonBasin, latBasin, plotPos="below"){
  # write world file according to definition 
  # http://www.gdal.org/frmt_various.html#WLD
  
  # pixel X size
  pixelXSize = degWidth/pxWidth
  writeLinesData=as.character(round(pixelXSize,digits = 5)) 
  # rotation about the Y axis (usually 0.0)
  writeLinesData=c(writeLinesData,"0.0")
  # rotation about the X axis (usually 0.0)
  writeLinesData=c(writeLinesData,"0.0")
  # negative pixel Y size
  pixelYSize = degHeight/pxHeight
  writeLinesData=c(writeLinesData,as.character(-round(pixelYSize,digits = 5)))
  # X coordinate of upper left pixel center
  # Y coordinate of upper left pixel center
  if(plotPos=="below"){
    writeLinesData=c(writeLinesData,as.character(round(lonBasin,digits = 5)))
    writeLinesData=c(writeLinesData,as.character(round(latBasin,digits = 5)))
  }else if(plotPos=="upper"){
    writeLinesData=c(writeLinesData,as.character(round(lonBasin,digits = 5)))
    writeLinesData=c(writeLinesData,as.character(round(latBasin+degHeight,digits = 5)))
  }else if(plotPos=="center"){
    writeLinesData=c(writeLinesData,as.character(round(lonBasin-degWidth*0.5,digits = 5)))
    writeLinesData=c(writeLinesData,as.character(round(latBasin+degHeight*0.5,digits = 5)))
  }else{
    # default, plotPos=="below"
    writeLinesData=c(writeLinesData,as.character(round(lonBasin,digits = 5)))
    writeLinesData=c(writeLinesData,as.character(round(latBasin,digits = 5)))
  }
  # open file
  fileConn<-file(fileName)
  # write the lines
  writeLines(writeLinesData,con=fileConn,sep="\n")
  # close file
  close(fileConn)
  return(0)  
}

# -----------------------------------------------------
# 4 - Map warning levels
# -----------------------------------------------------
# load subbasin spatial points data frame (shapefileData)
if(app.sys=="tep") {
  rciop.log("DEBUG","Reading input shapefile...", "/util/R/hypeapps-plot-warninglevel-map.R")
}

PlotRiskMap<-function (appInput  = app.input, appSetput=app.setup) {
  # define colours / graphis
  graphScale<-1.6  
  wl.alpha=200  # out of 255
  wl.col <- rev(heat.colors(length(appInput$returnPeriodIN)))
  # wl.col=c("yellow","orange","red")  
  # plot(1:length(wl.rp),col=wl.col,cex=10,pch=20)
  
  .makeTransparent <- function(someColor, alpha=60) {
    newColor <- col2rgb(someColor)
    apply(newColor, 2, function(curcoldata){rgb(red = curcoldata[1], green = curcoldata[2], blue = curcoldata[3], alpha = alpha, maxColorValue = 255)})
  }
  thiswl<-read.table(paste(appSetput$resDir ,
                           paste0("002_MapWarningClasse_",appInput$variable,"_", model.name, "_", appInput$area, ".txt"), sep="/"), 
                     sep=",", h=T)
  thiswl[,"col"]<-NA
  thiswl[which(thiswl[,"WarningLevel"]==0),"col"]<-NA
  for(k in 1:length(wl.col)) { #k<-3
    thiswl[which(thiswl[,"WarningLevel"]==k),"col"]<- .makeTransparent(wl.col[k],wl.alpha)
  }
  load(paste(appSetput$runDir, "shapefile", paste0(model.name, ".Rdata"), sep="/"))
  # Plot dimensions and World file
  shapefileData<-shapefilebasinData
  ydiff = bbox(shapefileData)[2,2]-bbox(shapefileData)[2,1]
  xdiff = bbox(shapefileData)[1,2]-bbox(shapefileData)[1,1]
  
  width = round(xdiff/(1/60),digits=0)
  height = round(width * ydiff/xdiff*1.03,digits=0)
  
  
  cx = bbox(shapefileData)[1,2] - 0.5 * (bbox(shapefileData)[1,2]-bbox(shapefileData)[1,1])
  cy = bbox(shapefileData)[2,2] - 0.5 * (bbox(shapefileData)[2,2]-bbox(shapefileData)[2,1])
  name.wl.png<-paste0("003_mapWarningLevel_", appInput$area, ".png")
  plotFileName = paste(appSetput$resDir,paste0(name.wl.png,"_", appInput$area, ".png"), sep="/")
  wfres = writeWorldFile(paste(plotFileName,"w",sep=""), pxWidth=round(width,digits=0), pxHeight=height,
                         degWidth=xdiff,degHeight=ydiff, lonBasin=cx, latBasin=cy, plotPos="center")
  subbasin.shp<-shapefilebasinData
  load(paste(appSetput$runDir, "shapefile", "countries_fanfar.Rdata", sep="/"))
  countries.shp<-shapefilecountriesData
  if(appInput$model=="niger-hype") {
    countries_niger.basin<-c("Benin", "Burkina Faso","Chad", "Guinea", "Ivory Coast", "Mali", "Niger", "Nigeria", "Niger Basin")
    if(appInput$area %in% countries_niger.basin) {
      if(appInput$area!="Niger Basin") {
        countries.shp.area<-countries.shp[countries.shp@data$CNTRY_NAME==appInput$area,]
        subbasin.shp.area<-crop(subbasin.shp,countries.shp.area)
        shapefileData<-subbasin.shp.area
      }
    } else {
      print("The country is not part of the Niger Basin")
    }
  } else if((appInput$model=="ww-hype"|appInput$model=="mosaic-hype")&appInput$area!="wa") {
    countries.shp.area<-countries.shp[countries.shp@data$CNTRY_NAME==appInput$area,]
    subbasin.shp.area<-crop(subbasin.shp,countries.shp.area)
    shapefileData<-subbasin.shp.area
  }
  sm<-match(slot(shapefileData,"data")[,"SUBID"],thiswl[,"SUBID"])  #all(thiswl[sm,"SUBID"]==slot(subs,"data")$SUBID)  # match subids

  
  
  if(appInput$model=="niger-hype") {
    modelName<-"Niger-HYPE 1.6"
  } else if( appInput$model=="ww-hype") {
    modelName<-"World Wide-HYPE 1.3.6"
  } else if(appInput$model=="mosaic-hype") {
    modelName<-"Mosaic-HYPE"
  }
  if(appInput$filterzone=="FloodProne") {
    flood_prone_shp<-readOGR(dsn=shapeDir, layer=Flood_Prone_Shp)
    flood_prone_shp.area<-crop(flood_prone_shp, countries.shp.area)
    mymatch4<-match(flood_prone_shp.area$SUBID, Warning.classes.area$SUBID)
    #Warning.classes.flood.prone.area<-Warning.classes.area[mymatch4,]
    shapefileData<-flood_prone_shp.area
    thiswl[-na.omit(match(flood_prone_shp.area$SUBID, thiswl[,"SUBID"])),"col"]<-NA
    
  }
  
  
  
  # initiate jpeg or png file for plotting using Cairo graphics device
  CairoPNG(filename = plotFileName, width = width, height = height, units = "px",bg = "white")
  
  par(xaxs = "i", yaxs = "i", lend = 1,mar=c(0,0,0,0),cex=graphScale)
  if(appInput$model=="niger-hype"|appInput$filterzone=="FloodProne"|appInput$filterzone=="Allbasins") {
    plot(countries.shp.area,col=NA,border="grey")  # country boundaries
    plot(shapefileData,col=NA,border="grey", add=T)  # subbasin boundaries
  } else {
    plot(shapefileData,col=NA,border="grey")  # subbasin boundaries
  }
  
  if(!is.null(shapefileData)) {
    plot(shapefileData,col=thiswl[sm,"col"],border="NA",add=T)  # warninglevels
    addscalebar(pos="bottomright", lwd=4, htin = 0.4,label.cex = 1.5)
    addnortharrow(pos="topright",scale = 4)
  }
  ##cdate="2019-09-24"
  if(appInput$alertmethod=="historicyear") {
    legend("bottomleft",inset=c(0,0.07),title="Severity",
           pt.cex=graphScale*4,cex=2,pch=15,bty="n",
           col=.makeTransparent(wl.col,wl.alpha),
           legend=c(paste(" Severity 1   (",as.character(appInput$returnPeriodIN[1])," % )",sep=""),
                    paste(" Severity 2   (",as.character(appInput$returnPeriodIN[2])," % )",sep=""),
                    paste(" Severity 3   (",as.character(appInput$returnPeriodIN[3])," % )",sep="")))
  } else {
    legend("bottomleft",inset=c(0,0.07),title="Severity",
           pt.cex=graphScale*4,cex=2,pch=15,bty="n",
           col=.makeTransparent(wl.col,wl.alpha),
           legend=c(paste(" Severity 1   (",as.character(appInput$returnPeriodIN[1])," yr RP)",sep=""),
                    paste(" Severity 2   (",as.character(appInput$returnPeriodIN[2])," yr RP)",sep=""),
                    paste(" Severity 3   (",as.character(appInput$returnPeriodIN[3])," yr RP)",sep="")))
  }
  if (appInput$model=="mosaic-hype") {
    models<-"Mosaic-HYPE v1 + HGFDv1 + ECOPER"
  } else if(appInput$model=="niger-hype") {
    models<-"Niger-HYPE v1.6 + HGFDv1 + ECOPER"
  } else if(appInput$model=="ww-hype") {
    models<-"World Wide-HYPE v1.3.6 + HGFDv1 + ECOPER"
  }
  
  if (appInput$alertmethod=="hypethreshold") {
    methodss<-"HYPE Threshold"
  } else if(appInput$alertmethod=="historicyear") {
    methodss<-"Historical Year"
  } else if(appInput$alertmethod=="observationthreshold") {
    methodss<-"Local Threshold"
  } else if(appInput$alertmethod=="waffi") {
    methodss<-"WAFFI Index"
  }
  
  if(appInput$alertmethod=="historilyear") {
    expression1<-"Choosen percentage:"
  } else {
    expression1<-"Choosen Return Period:"
  }
  legend("topleft",inset=c(0,0.07),title="",
         col="white",
         pt.cex=graphScale*4,cex=2,pch=15,bty="n",
         legend=c(paste("Issue Date: ",substr(app.date, 1,10)),
                  paste("Lead time:", 5, "Days"), 
                  paste("Data source:", models ),
                  paste("Variable:", appInput$variable),
                  paste("Hazard Threshold Method:", methodss),
                  paste(expression1, appInput$returnPeriodIN[1], appInput$returnPeriodIN[2],appInput$returnPeriodIN[3])))
  
  dev.off()
}

#internal log succesful sourcing of file
if(app.sys=="tep"){rciop.log ("DEBUG", paste("all functions sourced"), "/util/R/hypeapps-plot-warningevel-map_windows.R")}