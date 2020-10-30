#!/opt/anaconda/envs/postprocessing-env/bin/Rscript --vanilla --slave --quiet

##### #!/opt/anaconda/bin/Rscript --vanilla --slave --quiet
##### #!/usr/bin/Rscript --vanilla --slave --quiet

# run 'which Rscript' in the terminal. If the path differs you may have to change the first line above.
# When using certain R packages, call Rscript in the appropriate conda environment with the appropriate source code file.

print('start postprocess.....')

syscmd = paste0("/opt/anaconda/envs/postprocessing-env/bin/Rscript"," --vanilla --slave --quiet ", Sys.getenv("_CIOP_APPLICATION_PATH"), "/node_postprocess/postprocess.R")
system(syscmd)

print('end postprocess....')
