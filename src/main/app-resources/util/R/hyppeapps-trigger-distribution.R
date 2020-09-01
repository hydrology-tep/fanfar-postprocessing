## Script to analyze the risk/warning levels produced by the forecasting, and judge if a trigger should be created to distribute the information.
## By Jafet Andersson, Feb, 2019
7
# The logic of the script
# 1. Analyze the "...forecast_mapWarningLevel.txt" file
# 2. Apply a few criteria for wether a trigger should be made or not
# 3. Create the trigger & and the messages to be sent by SMS and EMAIL.


# To do
# - add dynamic info for the model (now hardcoded to NH_2.23, but in future take it from some log information. BRIEF)
# - Add link to the job (Emmanuel?), read from some input par?
# - make nicers templates (images for email, insert all text from the criterion dynamically to not have so many templates?)
# - different settings (e.g. different criteria, different regions, different send lists)

download_file <- function(local_file_name, url, OutDir) {

  if(app.sys=="tep") {
    # delete local file if it exist
    if(file.exists(paste0("download/",local_file_name))){
      file.remove(paste0("download/",local_file_name))
    }
    dir.create("download")
    rciop.copy(url, "download", uncompress=TRUE)
    return (local_file_name)
  } else if(app.sys=="win") {
    # delete local file if it exist
    if(file.exists(paste0(OutDir, "/download/",local_file_name))){
      file.remove(paste0(OutDir, "/download/",local_file_name))
    }
    if(!file.exists(paste0(OutDir, "/download"))) {
      dir.create(paste0(OutDir, "/download"))
    }
    file.copy(url, paste0(OutDir, "/download"), overwrite = TRUE)
    return (local_file_name)
  }
}

TriggerDistribution <- function(appInput  = app.input, appSetput=app.setup, dist.list.url=dist.list.url) {
    
    # Define output list
    output_files <- list() 
    path<-appSetput$resDir
    # --------------------------------
    # 1 - General
    path.wl<-paste(path, "/", sep="")  # define the path to the location of  "...forecast_mapWarningLevel.txt"
    name.wl<-dir(appSetput$resDir,pattern=paste0("002_MapWarningClasse_",appInput$variable,"_", model.name, "_", appInput$area, ".txt"))
    #name.wl_out<-name.wl
    if(app.sys=="tep") {
    } else if( app.sys=="win") {
      adresse<-read.table(paste(dist.list.url, "adresse_hype.txt", sep="/"), sep="\t",h=T)
      adresse<-adresse[which(adresse$Countries==appInput$area),]
    }
    
    #name.wl_out <- gsub(substr(name.wl, 1, 8), gsub("-", "", as.character(idate)), name.wl, fixed=T)
    #name.wl_out <- paste0(substr(name.wl, 1, 4), name.wl_out)
    #name.wl_out <- gsub("_forecast_mapWarningLevel.txt", "", name.wl_out)
    #name.wl_out <- gsub(substr(name.wl_out, 13, 23), "", name.wl_out, fixed=T)
    # Read this from the hydro-smhi fileshare store
    #path.templates <- paste(Sys.getenv("_CIOP_APPLICATION_PATH"), "util/R/triggercode/templates/", sep="/")
    if(app.sys=="tep") {
      path.templates <- "./"
      download_file("./sms_hype.txt", "'https://store.terradue.com/hydro-smhi/fanfar/distribution-templates/sms_hype.txt'")
      download_file("./email_hype.txt", "'https://store.terradue.com/hydro-smhi/fanfar/distribution-templates/email_hype.txt'")
      sysCmd=paste("mv", "./download/sms_hype.txt", path.templates, sep=" ")
      system(sysCmd, intern=T)
      sysCmd=paste("mv", "./download/email_nigerhype.txt", path.templates, sep=" ")
      system(sysCmd, intern=T)
    } else if( app.sys=="win") {
      download_file( url= paste0(dist.list.url, "/sms_hype_", adresse$Langage, ".txt"),
                     local_file_name =paste0("sms_hype_", adresse$Langage, ".txt"), OutDir =  appSetput$resDir)
      download_file( url= paste0(dist.list.url, "/email_hype_", adresse$Langage, ".txt"),
                     local_file_name =paste0("email_hype_", adresse$Langage, ".txt"), OutDir =  appSetput$resDir)
    }
    

    #path.templates<-"../util/R/triggercode/templates/"  # path to where the message templates are stored. On our Store
    name.temp.sms<-paste0("sms_hype_", adresse$Langage, ".txt")  # name of the SMS template, make dynamic, reading an input parameter instead for which model to use
    name.temp.email<-paste0("email_hype_", adresse$Langage, ".txt")  # dito for email, possibly change to some format that can read images?
    path.out <- path.wl  # path to where the outputs shall be written. Normally the same as path.wl

    
    # ---------------------------------
    # 2 - Process
    # get data
    #wl<-read.table(paste0(path.wl,name.wl),header=T,sep=",",skip=1)
    wl<-read.table(paste0(path.wl,name.wl),header=T,sep=",")
    if( app.sys=="tep") {
      temp.sms<-readLines(paste0(path.templates,name.temp.sms))
      temp.email<-readLines(paste0(path.templates,name.temp.email))
    } else if (app.sys=="win") {
      path.templates<-paste0(appSetput$resDir, "/download")
      temp.sms<-readLines(paste0(path.templates,"/", name.temp.sms))
      temp.email<-readLines(paste0(path.templates,"/",name.temp.email))
    }
    
    
    # determine how many subbasins are at or above a certain warning level
    wl1<-length(which(wl$WarningLevel>=1))
    wl2<-length(which(wl$WarningLevel>=2))
    wl3<-length(which(wl$WarningLevel>=3))
    

    # trigger functions
    # If >10% of subbasins are at or above warning level 2
    t2fun<-function(mywl2) { 
        myval<-mywl2/nrow(wl)
        if(myval>0.1) TRUE else FALSE
        }
    # If any subbasins are are at or above Warning level 3
    t3fun<-function(mywl3) { 
        if(mywl3>0) TRUE else FALSE
        }
    
    
    # Run the trigger functions & prepare template
    if (t2fun(wl2) | t3fun(wl3)) {  # any trigger yes/no
        # initiate messages
        #sms<-paste0(temp.sms[1], model)
        sms<-temp.sms[1]
        email<-temp.email[1:2]
        #email[2]<-sub("YYYYMMDD",paste(unlist(strsplit(name.wl,split="_"))[2:3],collapse=" "),email[2])  # insert idate instead
        email[2]<-sub("YYYYMMDD",substr(app.date, 1,10),email[2])  #
        email[2]<-sub("ZZZ", appInput$model,email[2])
        
        if (t2fun(wl2)) { # trigger t2fun
            sms[length(sms)+1]<-sub("XX",wl2,temp.sms[2])  # todo: don't hardcode the positions of the different triggers
            email[length(email)+1]<-sub("XX",wl2,temp.email[3]) # dito
            }
        if (t3fun(wl3)) { # trigger t3fun, todo: don't hardcode the positions of the different triggers
            sms[length(sms)+1]<-sub("YY",wl3,temp.sms[3])
            email[length(email)+1]<-sub("YY",wl3,temp.email[4])
            }
        sms[1]<-sub("ZZZ",appInput$model,sms[1])
        # Add link here # fixme
        sms<-c(sms,temp.sms[4])
        email<-c(email,temp.email[5:6])
        adresses<-NULL
        for( i in 4:ncol(adresse)) {
          adresses<-c(adresses, as.character(adresse[,i]))
        }
        
        # write trigger file and message files, change to idate in the file names
        write("Triggers activated",file=paste0(path.out,"_send_messages.txt"))  # fixme: make dynamic to depend on the model/job instead
        writeLines(text=sms,con=paste0(path.out,"_sms_message.txt"))
        writeLines(text=email,con=paste0(path.out,"_email_message.txt"))
        writeLines(text=adresses,con=paste0(path.out,"_adresse_email&message.txt"))
        
        output_files <- c(output_files, paste0(path.out,"_send_messages.txt"))
        output_files <- c(output_files, paste0(path.out,"_sms_message.txt"))
        output_files <- c(output_files, paste0(path.out, "_email_message.txt"))
        #output_files <- c(output_files, paste0(path.out,paste0(paste(unlist(strsplit(name.wl,split="_"))[2:3],collapse="_"),"_email_message.txt")))

    } else {
        # write non-trigger file (do it since then one can always check if the process worked or not)  
        write("No triggers activated",file=paste0(path.out,"_donotsend_messages.txt"))  # fixme: make dynamic to depend on the model/job instead
        output_files <- c(output_files, paste0(path.out,"_donotsend_messages.txt"))
    }
        return (output_files)
}
  
  
#internal log succesful sourcing of file
if(app.sys=="tep"){rciop.log ("DEBUG", paste("all functions sourced"), "/util/R/hypeapps-trigger-distribution.R")}