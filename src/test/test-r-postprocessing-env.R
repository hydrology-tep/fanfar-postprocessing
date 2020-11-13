#!/opt/anaconda/envs/postprocessing-env/bin/Rscript --vanilla --slave --quiet

print("Start: test-r-postprocessing-env.R")

#
# Configuration settings
#
if(!exists("app.sys")){
  app.sys ="tep"
}

#
# Enable use of the other R script via an external system() call to let
# the other R script use R installed in (from) cairo-env.
#
call_external_r_code_via_system = FALSE # TRUE
call_external_r_code_via_system = call_external_r_code_via_system && app.sys == "tep"

#
# Enable use of the other R script via source (instead of via system() call)
#
call_external_r_code_via_source = TRUE # FALSE # TRUE


#
# Start of test program (main test program)
#
print("---------------------------------------------------------")
print("Running R from environment postprocessing-env???")


#
# Define a global variable
#
POSTPROCESSING_VARIABLE = 123


#
# Define an environment variable in the system
#
Sys.setenv(PATH_SHAPEFILE="/tmp/subid_shapefile.shp")


#
# View information to see that intended R version and packages are used
#
print("---------------------------------------------------------")
print("Information from .libPaths():")
print(.libPaths())

print("---------------------------------------------------------")
print("Information from sessionInfo():")
print(sessionInfo())


#
# Test to load a R package
#
print("---------------------------------------------------------")
print("Try to load R package lmomco:")
library(lmomco)
print("View lmomco to see that R has loaded the package:")
lmomcoNews


#
# Test to print variable defined in this script
#
print("---------------------------------------------------------")
print("Value of variable POSTPROCESSING_VARIABLE:")
print(POSTPROCESSING_VARIABLE)


#
# Test to print environment variable defined by this script
#
print("---------------------------------------------------------")
print("Value of environment variable PATH_SHAPEFILE:")
print( Sys.getenv("PATH_SHAPEFILE") )


#
# Test to see that we can run a separate R script in its own environment (an environment possibly using different R packages or different versions of R packages)
# This script could instead be a python script calling the other R script.
#
if (call_external_r_code_via_system == TRUE){
  print("")
  print("")
  print("")
  print("---------------------------------------------------------")
  print("Call external R script test-r-cairo-env:")

  # Call script test-r-cairo-env.R, with R installed in the environment cairo-env
  syscmd = paste0("/opt/anaconda/envs/cairo-env/bin/Rscript"," --vanilla --slave --quiet ", "test-r-cairo-env.R")
  system(syscmd)

  #syscmd = paste0("/opt/anaconda/envs/cairo-env/bin/Rscript"," --vanilla --slave --quiet ", Sys.getenv("_CIOP_APPLICATION_PATH"), "/node_postprocess/postprocess2.R")
  #system(syscmd)

  # or (only an example), see function in test-r-cairo-env.R that parses input parameters:
  #args = paste0('--path_shapefiles',' ',app.setup$XYZ,' ','--alertmethod',' ',app.input$alertmethod)
  #system2(syscmd,args=args)


  #
  # Test to print variable defined in the other script
  # Works as expected when source of file, but will not work for
  # variables that gets assigned by functions.
  #print("---------------------------------------------------------")
  #print("Value of variable CAIRO_VARIABLE:")
  #print(CAIRO_VARIABLE)


  print("---------------------------------------------------------")
  print("Done: Call external R script test-r-cairo-env:")
  print("")
  print("")
  print("")

} else if (call_external_r_code_via_source == TRUE){
  print("")
  print("")
  print("")
  print("---------------------------------------------------------")
  print("Source external R script test-r-cairo-env:")

  # Source of file to inherit variables and access functions
  source("test-r-cairo-env.R")


  #
  # View information to see that intended R version and packages are used
  # Shall not be cairo-env, it should still running R from postprocessing-env
  print("---------------------------------------------------------")
  print("Is script running R from postprocessing-env??? or cairo-env???")
  print("Information from .libPaths():")
  print(.libPaths())

  print("---------------------------------------------------------")
  print("Information from sessionInfo():")
  print(sessionInfo())


  #
  # Test to print variable defined in the other script
  #
  print("---------------------------------------------------------")
  print("Value of variable CAIRO_VARIABLE:")
  print(CAIRO_VARIABLE)


  #
  # Test to call function defined in the other script
  #
  print("---------------------------------------------------------")
  print("Value from function test_r_cairo_env_add_constant():")
  print( test_r_cairo_env_add_constant(value=-15) )


  print("---------------------------------------------------------")
  print("Done: Source external R script test-r-cairo-env:")
  print("")
  print("")
  print("")
}


#
# Unset environment variable in the system
#
Sys.unsetenv("PATH_SHAPEFILE")


print("---------------------------------------------------------")
print("Done: Running R from environment postprocessing-env")
print("End: test-r-postprocessing-env.R")
