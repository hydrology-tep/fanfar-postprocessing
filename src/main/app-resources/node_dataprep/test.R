#!/opt/anaconda/envs/postprocessing-env/bin/Rscript --vanilla --slave


####!/opt/anaconda/envs/postprocessing-env/bin/Rscript --vanilla --slave --quiet

print ("aaaaaa")

#/opt/anaconda/envs/postprocessing-env/lib/R/library
#.libPaths( c( .libPaths(), "/opt/anaconda/envs/postprocessing-env/lib/R/library") )

#.libPaths()
print(.libPaths())

library("lmomco")

print(sessionInfo())
print ("sdf")