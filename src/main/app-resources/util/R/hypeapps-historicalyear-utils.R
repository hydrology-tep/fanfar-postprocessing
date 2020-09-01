

historicalDischargeMagnitudes<-function(name.in=NULL,
                                        forecast.in=NULL,
                                        name.out="retlev.txt",
                                        wl.rp=c(50,70,100),
                                        hist.year=2012,
                                        datasource=NULL) {
  if(file.exists(name.in)&file.exists(forecast.in)) {
    if(datasource=="modeledhistorical") {
      hypeout <- ReadTimeOutput(filename=name.in, dt.format = "%Y-%m-%d")
      #hypeout <- read.table(name.in, sep="\t", h=T,na="-9999")
    } else if(datasource=="observedvalues") {
      hypeout <- read.table(name.in, sep="\t", h=T, na="-9999")
    }
    # change column names
    colnames(hypeout) <- sub("X","",colnames(hypeout))
    #Read forecast COUT file
    hype.forecast<-ReadTimeOutput(filename=forecast.in, dt.format = "%Y-%m-%d")
    colnames(hype.forecast) <- sub("X","",colnames(hype.forecast))
    # Define object to store return levels
    retlev<-as.data.frame(matrix(NA,nrow=ncol( hype.forecast)-1,ncol=1+length(wl.rp)))
    colnames(retlev)<-c("SUBID",paste("RP",wl.rp,sep=""))
    retlev[,"SUBID"]<-colnames(hype.forecast)[-1]
    
    # Calculate maximum value for each subbasin according to the year
    start.date<-as.Date(paste0(hist.year, substr(hype.forecast[,"DATE"], 5,10)))-15
    start.date<-start.date[1]
    end.date<-as.Date(paste0(hist.year, substr(hype.forecast[,"DATE"], 5,10)))+15
    end.date<-end.date[10]
    dataset.plage<-seq(start.date, end.date,1)
    hypeout.subset<-hypeout[match(dataset.plage, as.Date(hypeout[,"DATE"])),]
    max.value<-apply(hypeout.subset[,-1], 2, FUN=max, na.rm=T)
    max.value[which(max.value<=-9999999999)]<-NA
    retlev.bassin<-round(cbind(max.value*wl.rp[1]/100, max.value*wl.rp[2]/100, max.value*wl.rp[3]/100), 3)
    mymatch<-match(rownames(retlev.bassin),retlev[,"SUBID"])
    retlev[mymatch,paste("RP",wl.rp,sep="")]<-retlev.bassin
    write.table(retlev,name.out,quote = F,row.names=F)
    return(0)
  } else {
    return(-1)
  }
}


returnPeriodMagnitudes.hist<-function(name.in=NULL,forecast.in=NULL, name.out="retlev.txt",wl.rp=c(2,5,30),dist="gev"){
  
  if(file.exists(name.in)){
    # read timefile
    hypeout <- ReadPTQobs(filename = name.in,dt.format = "%Y-%m-%d")
    
    # log some file properties
    #    if(app.sys=="tep"){rciop.log ("DEBUG", paste("   name.in=",name.in,sep=""), "/util/R/hypeapps-returnperiod-utils.R")}
    #    if(app.sys=="tep"){rciop.log ("DEBUG", paste("   ncol(hypeoutput)=",as.character(ncol(hypeout)),sep=""), "/util/R/hypeapps-returnperiod-utils.R")}
    #    if(app.sys=="tep"){rciop.log ("DEBUG", paste("   nrow(hypeoutput)=",as.character(nrow(hypeout)),sep=""), "/util/R/hypeapps-returnperiod-utils.R")}
    #    if(app.sys=="tep"){rciop.log ("DEBUG", paste("   colnames(hypeoutput)=",colnames(hypeout),sep=""), "/util/R/hypeapps-returnperiod-utils.R")}
    
    # change column names
    colnames(hypeout) <- sub("X","",colnames(hypeout))
    
    #Read forecast COUT file
    hype.forecast<-ReadTimeOutput(filename=forecast.in, dt.format = "%Y-%m-%d")
    colnames(hype.forecast) <- sub("X","",colnames(hype.forecast))
    # Define object to store return levels
    retlev<-as.data.frame(matrix(NA,nrow=ncol(hypeout)-1,ncol=1+length(wl.rp)))
    colnames(retlev)<-c("SUBID",paste("RP",wl.rp,sep=""))
    retlev[,"SUBID"]<-colnames(hypeout)[-1]
    retlev2<-as.data.frame(matrix(NA,nrow=ncol(hype.forecast)-1,ncol=1+length(wl.rp)))
    colnames(retlev2)<-c("SUBID",paste("RP",wl.rp,sep=""))
    retlev2[,"SUBID"]<-colnames(hype.forecast)[-1]
    
    # Calculate annual maximum for each year and subid
    annmax<-aggregate(hypeout[,-1],by=list(year=as.numeric(format(hypeout[,"DATE"],format="%Y"))),FUN="max")
    
    # if(app.sys=="tep"){rciop.log ("DEBUG", paste("   annmax=",as.character(annmax),sep=""), "/util/R/hypeapps-returnperiod-utils.R")}
    
    
    # Fit the distribution and derive return levels for each subbasin
    for(j in 1:nrow(retlev)){ # j<-2
      # fit frequency distribution
      suppressWarnings(  # gives error and warns if the FrequencyAnalysis fails (e.g. in very dry areas)
        fa.sim <- tryCatch(FrequencyAnalysis(series=annmax[,retlev[j,"SUBID"]], distribution=dist), error = function(e) {NULL}))
      suppressWarnings(fa.sim <- tryCatch(FrequencyAnalysis(series=na.omit(annmax[,retlev[j,"SUBID"]]), distribution=dist), error = function(e) {NULL})
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
    mymatch<-match(retlev[,"SUBID"], retlev2[,"SUBID"])
    retlev2[mymatch,]<-retlev
    # Write return period magnitude output file
    write.table(retlev2,name.out,quote = F,row.names=F)  # note it writes with 11|12 decimals so reading in the object will have slighlty different values...
    # return ok signal
    return(0)
  }else{
    return(-1)
  }
}

#internal log succesful sourcing of file
if(app.sys=="tep"){rciop.log ("DEBUG", paste("all functions sourced"), "/util/R/hypeapps-historicalyear-utils.R")}

