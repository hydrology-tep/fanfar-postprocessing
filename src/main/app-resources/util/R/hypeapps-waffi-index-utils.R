#!/opt/anaconda/bin/Rscript --vanilla --slave --quiet
#
# /hypeapps-[appName]/src/main/app-resources/util/R/hypeapps-utils.R
#
# Copyright 2019 FANFAR
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
# Version:          2019-09-26

# 2 - Dependancies
# -------------------------------------------------------------------
if(app.sys=="tep") {
  #library(ncdf4)
  #library(abind)
  #library(rasterVis)
  #library(hydroTSM)
} else {
  #library(ncdf4)
  library(abind)
  library(rasterVis)
  library(hydroTSM)
}
#app.date = format(Sys.time(), "%Y-%m-%d_%H%M")
#app.date<-"2018-08-31"
ComputeEpicIndex<-function (historical.dir, historical.file, forecast.dir, forecast.file, outdir, outfile) {
  hypehistorical<-ReadTimeOutput(filename = paste(historical.dir, historical.file, sep="/"),dt.format = "%Y-%m-%d")
  hypeforecast<-ReadTimeOutput(filename = paste(forecast.dir, forecast.file, sep="/"),dt.format = "%Y-%m-%d")
  mymatch<-match(colnames(hypehistorical), colnames(hypeforecast))
  hypeforecast<-hypeforecast[,mymatch]
  epic.index.10days<-NULL
  for( i in 1:nrow(hypeforecast)) {
      datees<-hypeforecast[i,1]
      month<-substr(datees,6,7)
      hypehistorical.subset<-hypehistorical[which(substr(hypehistorical[,1],6,7)==month),]
      hypehistorical.subset.montlhy.max<-daily2monthly(zoo(hypehistorical.subset[,-1],hypehistorical.subset[,1]), 
                                                       FUN=max, na.rm=T)
      epic.denominator<-apply(hypehistorical.subset.montlhy.max, 2, mean, na.rm=T)
      epic.index<-hypeforecast[i,-1]/epic.denominator
      epic.index.10days<-rbind(epic.index.10days,epic.index)
      print(datees)
  }
  epic.index.10days=cbind(hypeforecast[,1], epic.index.10days)
  colnames(epic.index.10days)<-sub("X", "", colnames(epic.index.10days))
  colnames(epic.index.10days)[1]<-"DATE"
  write.table(epic.index.10days, paste(outdir, outfile, sep="/"), row.names = F, quote=F, sep="\t")
}


returnPeriodMagnitudes_waffi<-function(name.in=NULL,name.out="retlev.txt",wl.rp=c(2,5,30),dist="gev"){
  
  if(file.exists(name.in)){
    # read timefile
    hypeout <- read.table(name.in,sep="\t", h=T)
    
    # log some file properties
    #    if(app.sys=="tep"){rciop.log ("DEBUG", paste("   name.in=",name.in,sep=""), "/util/R/hypeapps-returnperiod-utils.R")}
    #    if(app.sys=="tep"){rciop.log ("DEBUG", paste("   ncol(hypeoutput)=",as.character(ncol(hypeout)),sep=""), "/util/R/hypeapps-returnperiod-utils.R")}
    #    if(app.sys=="tep"){rciop.log ("DEBUG", paste("   nrow(hypeoutput)=",as.character(nrow(hypeout)),sep=""), "/util/R/hypeapps-returnperiod-utils.R")}
    #    if(app.sys=="tep"){rciop.log ("DEBUG", paste("   colnames(hypeoutput)=",colnames(hypeout),sep=""), "/util/R/hypeapps-returnperiod-utils.R")}
    
    # change column names
    colnames(hypeout) <- sub("X","",colnames(hypeout))
    
    # Define object to store return levels
    retlev<-as.data.frame(matrix(NA,nrow=ncol(hypeout)-1,ncol=1+length(wl.rp)))
    colnames(retlev)<-c("SUBID",paste("RP",wl.rp,sep=""))
    retlev[,"SUBID"]<-colnames(hypeout)[-1]
    
    # Calculat annual maximum for each year and subid
    annmax<-aggregate(hypeout[,-1],by=list(year=as.numeric(format(as.Date(hypeout[,"DATE"]),format="%Y"))),FUN="max")
    
    # if(app.sys=="tep"){rciop.log ("DEBUG", paste("   annmax=",as.character(annmax),sep=""), "/util/R/hypeapps-returnperiod-utils.R")}
    
    
    # Fit the distribution and derive return levels for each subbasin
    for(j in 1:nrow(retlev)){ # j<-2
      # fit frequency distribution
      suppressWarnings(  # gives error and warns if the FrequencyAnalysis fails (e.g. in very dry areas)
        fa.sim <- tryCatch(FrequencyAnalysis(series=annmax[,retlev[j,"SUBID"]], distribution=dist), error = function(e) {NULL})
      )
      if(!is.null(fa.sim)) {
        # extract return levels, note the loop is there in case some RP is chosen that is not included in the fa.sim$output$rp, then it interpolates between the nearest (davids method)
        wl.sim <- rep(0,length(wl.rp)) 
        rp.sim <- as.numeric(fa.sim$output$rp)
        for(i in 1:length(wl.rp)){ #i<-1
          ilow=which(rp.sim<=wl.rp[i])
          ihig=which(rp.sim>=wl.rp[i])
          if(ilow[length(ilow)]==ihig[1]){ # if the RP is in the estimate list
            wl.sim[i]=fa.sim$output$estimate[ihig[1]]
          }else{
            wl.sim[i]=fa.sim$output$estimate[ilow[length(ilow)]] + (wl.rp[i]-rp.sim[ilow[length(ilow)]]) * (fa.sim$output$estimate[ihig[1]]-fa.sim$output$estimate[ilow[length(ilow)]]) / (rp.sim[ihig[1]]-rp.sim[ilow[length(ilow)]])
          }
        }
        
        retlev[j,1+1:length(wl.rp)] <- wl.sim  # insert the derived return levels
        rm(wl.sim,rp.sim)  # Clean
      }
      
      rm(fa.sim)
    }
    
    # Write return period magnitude output file
    write.table(retlev,name.out,quote = F,row.names=F)  # note it writes with 11|12 decimals so reading in the object will have slighlty different values...
    # return ok signal
    return(0)
  }else{
    return(-1)
  }
}

#internal log succesful sourcing of file
if(app.sys=="tep"){rciop.log ("DEBUG", paste("all functions sourced"), "/util/R/hypeapps-waffi-index-utils.R")}

