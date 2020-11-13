#!/opt/anaconda/envs/cairo-env/bin/Rscript --vanilla --slave --quiet

print("Start: test-r-cairo-env.R")

#
# A function to get input arguments/parameters to this script if called from another script
#
parse_args<-function(verbose=F)
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
    # arg.forecast = F
    # arg.issuedate = NULL
    arg.path_shapefiles = NULL

    args = commandArgs(trailingOnly = TRUE)

    if (length(args) > 0){
        for (i in 1:length(args)){
            x = split_arg(args[i])

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


# Get arguments/parameters from a calling script
#args = parse_args()

#path_shapefiles=""
#if(! is.null(args$path_shapefiles)){
#  path_shapefiles = args$path_shapefiles
#}
# app.input = list(alertmethod=args$alertmethod,
#                  variable=args$variable)
# app.setup = list(resDir=args$resdir)


print("")
print("")    
print("")
print("+++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
print("Running R from environment cairo-env???")


#
# Define a global variable
#
CAIRO_VARIABLE = 456


#
# Define a function to be called from test-r-postprocessing.R
#
test_r_cairo_env_add_constant<-function(value)
{
  return (CAIRO_VARIABLE + value)
}


print("+++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
print("Information from .libPaths():")
print(.libPaths())

print("+++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
print("Information from sessionInfo():")
print(sessionInfo())


print("+++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
print("Try to load R package sp:")
library(sp)
print("+++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
print("View sp to see that R has loaded the package:")
sp.lines


#
# Test to print variable defined in this script
#
print("+++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
print("Value of variable CAIRO_VARIABLE:")
print(CAIRO_VARIABLE)


#
# Test to call function defined in this script
#
print("+++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
print("Value from function test_r_cairo_env_add_constant():")
print( test_r_cairo_env_add_constant(value=7) )


#
# Test to print variable defined in the other script
#
#print("+++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
#print("Value of variable POSTPROCESSING_VARIABLE:")
#print(POSTPROCESSING_VARIABLE)


#
# Test to print environment variable defined by other script
#
print("+++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
print("Value of environment variable PATH_SHAPEFILE:")
path_shapefile = Sys.getenv("PATH_SHAPEFILE")
if (nchar(path_shapefile) == 0){
   print("Value not set")
}else{
  print(path_shapefile)
}


print("+++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
#print("Done: Running R from environment cairo-env")
print("")
print("")
print("")
print("End: test-r-cairo-env.R")
