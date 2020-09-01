#!/opt/anaconda/bin/Rscript --vanilla --slave --quiet
#
# /hypeapps-[appName]/src/main/app-resources/util/R/hypeapps-environment.R
#
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
#
# hypeapps-processing-settings.R: Model name, URL to store.terradue.com etc.
# Author:                    David Gustafsson, SMHI; Bernard Minoungou, AGRHYMET
# Version:                   2020-05-19

## [DO NOT EDIT] set all required variables to NULL
model.name      = "mosaic-hype"
model.bin       = NULL
model.files.url = NULL
forcing.files   = NULL
forcing.data.source = NULL
forcing.archive.url = NULL
forcing.archive.start = NULL
forcing.archive.end = NULL
state.files.url = NULL
state.files = NULL
hype2csv.url = NULL
hype2csv.file = NULL
shapefile.url   = TRUE
shapefile.layer = NULL
shapefile.ext   = c(".shp",".prj",".dbf",".shx")

historical.files.url=NULL
historical.files=NULL
forecast.files.url=NULL
forecast.files=NULL
shapefile.basin.url= NULL
shapefile.basin.layer=NULL
shapefile.basin.ext=NULL
shapefile.countries.url=NULL
shapefile.countries.layer=NULL
shapefile.countries.ext=NULL
population.density.url=NULL
population.density.file=NULL
population.density.ext=NULL
dist.list.url=NULL
dist.list.files=NULL


if(app.sys=="tep") {
  ## [EDIT HERE] Data store settings (model historical files)
  historical.files.url = "https://store.terradue.com/bernardmin/hype-files/historical-files/models" # model historical files root index
  
  ## [EDIT HERE] Forcing data settings
  historical.files = c("timeCOUT.txt","timeCPRC.txt","timeCROS.txt") # list of necessary output data files

  ## [EDIT HERE] Forcing data settings
  qobs.files.url="https://store.terradue.com/bernardmin/hype-files/historical-files/obs"
  
  ## [EDIT HERE] Forcing data settings
  historicalwaffi.files.url="https://store.terradue.com/bernardmin/hype-files/historical-files/waffi"
  
  ## [EDIT HERE] Data store settings (model forecast files)
  forecast.files.url = "https://store.terradue.com/bernardmin/hype-files/forecast-files/models" # model historical files root index
  
  ## [EDIT HERE] Forcing data settings
  forecast.files = c("timeCOUT.txt","timeCPRC.txt","timeCROS.txt") # list of necessary output data files
  
  
  # [EDIT HERE] sub-basin shapefile URL (shapefile.url should point to shapefile [model.name].shp, and in the same folder should be .dbf, .prj and .shx)
  shapefile.basin.url   = "https://store.terradue.com/bernardmin/hype-files/shapefiles"
  shapefile.basin.layer = c("niger-hype", "ww-hype", "mosaic-hype")
  shapefile.basin.ext   = c(".shp",".prj",".dbf",".shx") 
  
  
  # [EDIT HERE] countries shapefile URL (shapefile.url should point to shapefile [model.name].shp, and in the same folder should be .dbf, .prj and .shx)
  shapefile.countries.url   = "https://store.terradue.com/bernardmin/hype-files/shapefiles"
  shapefile.countries.layer = "countries_fanfar"
  shapefile.countries.ext   = c(".shp",".prj",".dbf",".shx")
  
  # [EDIT HERE] countries shapefile URL (shapefile.url should point to shapefile [model.name].shp, and in the same folder should be .dbf, .prj and .shx)
  shapefile.floodprone.url   = "https://store.terradue.com/bernardmin/hype-files/shapefiles"
  shapefile.floodprone.layer = "Flood_Prone_Area"
  shapefile.floodprone.ext   = c(".shp",".prj",".dbf",".shx")
  
  # [EDIT HERE] population density raster URL and file names
  population.density.url="https://store.terradue.com/bernardmin/hype-files/distribution-lists"
  population.density.file="population_density.tif"
  
  
  dist.list.url=NULL
  dist.list.files=NULL
  
  # log message
  if(app.sys=="tep"){rciop.log ("DEBUG", paste("model and data access settings set"), "/util/R/hypeapps-processing-settings.R")}
} else if(app.sys=="win") {
  historical.files.url="E:/AGRHYMET/FANFAR/git/post_processing_fanfar/input_catalogue/time"
  historical.files = c("timeCOUT.txt","timeCPRC.txt","timeCROS.txt")
  forecast.files.url="E:/AGRHYMET/FANFAR/git/post_processing_fanfar/input_catalogue/time"
  qobs.files.url="E:/AGRHYMET/FANFAR/git/post_processing_fanfar/input_catalogue/qobs"
  historicalwaffi.files.url="E:/AGRHYMET/FANFAR/git/post_processing_fanfar/input_catalogue/waffi"
  #forecast.files=c("timeCOUT.txt","timeCPRC.txt","timeCROS.txt")
  shapefile.basin.url= "E:/AGRHYMET/FANFAR/git/post_processing_fanfar/input_catalogue/shp"
  shapefile.basin.layer = c("niger-hype", "ww-hype", "mosaic-hype")
  shapefile.basin.ext   = c(".shp",".prj",".dbf",".shx") 
  shapefile.countries.url="E:/AGRHYMET/FANFAR/git/post_processing_fanfar/input_catalogue/shp"
  shapefile.countries.layer = "countries_fanfar"
  shapefile.countries.ext   = c(".shp",".prj",".dbf",".shx")
  population.density.url="E:/AGRHYMET/FANFAR/git/post_processing_fanfar/input_catalogue/shp"
  population.density.file="population_density.tif"
  dist.list.url="E:/AGRHYMET/FANFAR/git/post_processing_fanfar/input_catalogue/distribution-lists"
  dist.list.files=NULL
}

