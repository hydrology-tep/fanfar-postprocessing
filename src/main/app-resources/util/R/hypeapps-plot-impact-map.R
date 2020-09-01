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
  library(rciop)
  library(Cairo)      # graphics device for output files
  library(lattice)
  library(raster)
} else {
  library(Cairo)      # graphics device for output files
  library(lattice)
  library(raster)
}


.makeTransparent <- function(someColor, alpha=60) {
  newColor <- col2rgb(someColor)
  apply(newColor, 2, function(curcoldata){rgb(red = curcoldata[1], green = curcoldata[2], blue = curcoldata[3], alpha = alpha, maxColorValue = 255)})
}
### ----------------------------
PlotMapPopulation1 <- function(x, map, map1, map.subid.column = 2, var.name = "", map.adj = 0, plot.legend = T, 
                              legend.pos = "right", legend.title = NULL, legend.outer = F, legend.inset = c(0, 0), 
                              plot.scale = T, plot.arrow = T, 
                              par.cex = 1, par.mar = rep(0, 4) + .1, add = FALSE, restore.par = FALSE,main.title=NULL,par.mai=NULL, model, method, variable, wl.rp) {
  graphScale<-1.6  
  wl.alpha=200  # out of 255
  plot(map1, col=NA, border="grey")
  #plot(countries.shp.area,col=NA,border="grey")  # country boundaries
  par(xaxs = "i", yaxs = "i", lend = 1,mar=c(0,0,0,0),cex=1.6)
  ColDiffPop2<-colorRampPalette(c("#FFFFCC", "#CCCC00"))
  ColDiffPop3<-colorRampPalette(c("#FFCCCC", "#CC0000"))
  x2<-subset(x, x$WarningLevel==2)
  x3<-subset(x, x$WarningLevel==3)
  #crfun <- col.ramp.fun
  
  cbrks2 <- quantile(x2[, 3], probs = seq(0, 1, .1), na.rm = T)
  cbrks3 <- quantile(x3[, 3], probs = seq(0, 1, .1), na.rm = T)
  
  cbrks2 <- unique(cbrks2)
  cbrks3 <- unique(cbrks3)
  
  if (length(cbrks2) == 1) {
    cbrks2 <- range(cbrks2) + c(-1, 1)
  }
  if (length(cbrks3) == 1) {
    cbrks3 <- range(cbrks3) + c(-1, 1)
  }
  
  # discretise the modeled values in x into classed groups, add to x as new column (of type factor)
  x2[, 4] <- cut(x2[, 3], breaks = cbrks2, include.lowest = T)
  x3[, 4] <- cut(x3[, 3], breaks = cbrks3, include.lowest = T)
  # replace the factor levels with color codes using the color ramp function assigned above
  levels(x2[, 4]) <- ColDiffPop2(length(cbrks2) - 1)
  levels(x3[, 4]) <- ColDiffPop3(length(cbrks3) - 1)
  
  x2[, 4] <- as.character(x2[, 4])
  x3[, 4] <- as.character(x3[, 4])
  xx<-rbind(x2,x3)
  # convert to character to make it conform to plotting requirements below
  mymatch1<-match(x$SUBID, xx$SUBID)
  xx<-xx[mymatch1,]
  mymatch2<-match(map@data$SUBID, xx$SUBID)
  xx<-xx[mymatch2,]
  # give it a name
  names(xx)[4] <- "color"
  
  # add x to subid map table (in data slot, indicated by @), merge by SUBID
  if(model=="ww-hype"|model=="mosaic-hype") {
    map@data <- data.frame(map@data, xx[match(map@data[, "SUBID"], xx[,1]),])
  } else if(model=="niger-hype") {
    map@data <- data.frame(map@data, xx[match(map@data[, "SUBID"], xx[,1]),])
  }
  # plot width (inches)
  p.in.wd <- par("pin")[1]
  # legend position (fraction if 'add' is FALSE, otherwise already in map coordinates) 
  leg.fr.pos <- legend("topleft", legend = c(rep(NA, length(cbrks2) - 1),rep(NA, length(cbrks3) - 1)),
                       col = c(ColDiffPop2(length(cbrks2) - 1),ColDiffPop3(length(cbrks3) - 1)),
                       lty = 1, lwd = 14,  bty = "n", title = "Population", plot = F)
  # legend width (fraction if 'add' is FALSE, otherwise already in map coordinates) 
  leg.fr.wd <- leg.fr.pos$rect$w
  # legend box element height (fraction), with workaround for single-class maps
  if (length(leg.fr.pos$text$y) == 1) {
    te <- legend(legend.pos, legend = c(rep(NA, length(cbrks2)), rep(NA, length(cbrks3))),
                 col = c(ColDiffPop2(length(cbrks2)),ColDiffPop3(length(cbrks3))), 
                 lty = 1, lwd = 14,  bty = "n", title = legend.title, plot = F)
    legbx.fr.ht <- diff(c(te$text$y[length(c(cbrks2,cbrks3))], te$text$y[length(c(cbrks2,cbrks3)) - 2]))
  } else {
    legbx.fr.ht <- diff(c(leg.fr.pos$text$y[length(c(cbrks2,cbrks3)) - 3], leg.fr.pos$text$y[length(c(cbrks2,cbrks3)) - 4]))
  }
  
  ## prepare legend annotation
  
  # formatted annotation text (to be placed between legend boxes which is not possible with legend() directly)
  ann.txt <- signif(c(cbrks2,cbrks3), digits = 2)
  # conditional: remove outer break points
  if (!legend.outer) {
    ann.txt[c(1, length(ann.txt))] <- ""
  }
  # annotation width (inches)
  ann.in.wd <- max(strwidth(ann.txt, "inches"))
  # legend inset required to accomodate text annotation, and scalebar (always below legend)
  leg.inset <- c(ann.in.wd/p.in.wd, if(legend.pos %in% c("bottomright", "bottomleft")) {0.1} else {0})
  
  # conditional on legend placement side (legend annotation always right of color boxes)
  if (legend.pos %in% c("bottomright", "right", "topright")) {
    
    # update legend inset
    legend.inset <- legend.inset + leg.inset
    ## annotation positions (fraction if 'add' is FALSE, otherwise already in map coordinates)
    # inset scaling factor, used if 'add' is TRUE, otherwise 1 (explicitly because usr does not get updated directly when set)
    if (add) {
      f.inset.x <- par("usr")[2] - par("usr")[1]
      f.inset.y <- par("usr")[4] - par("usr")[3]
    } else {
      f.inset.x <- 1
      f.inset.y <- 1
    }
    ann.fr.x <- rep(leg.fr.pos$text$x[1], length(ann.txt)) - legend.inset[1] * f.inset.x - 0.01
    if (legend.pos == "bottomright") {
      ann.fr.y <- rev(seq(from = leg.fr.pos$text$y[length(c(cbrks2, cbrks3)) - 1] - legbx.fr.ht/2, by = legbx.fr.ht, length.out = length(c(cbrks2, cbrks3)))) + legend.inset[2] * f.inset.y
    } else if (legend.pos == "right") {
      ann.fr.y <- rev(seq(from = leg.fr.pos$text$y[length(c(cbrks2, cbrks3)) - 1] - legbx.fr.ht/2, by = legbx.fr.ht, length.out = length(c(cbrks2, cbrks3))))
    } else {
      ann.fr.y <- rev(seq(from = leg.fr.pos$text$y[length(c(cbrks2, cbrks3)) - 1] - legbx.fr.ht/2, by = legbx.fr.ht, length.out = length(c(cbrks2, cbrks3)))) - legend.inset[2] * f.inset.y
    }
    
  } else {
    # left side legend
    # update legend inset
    legend.inset[2] <- legend.inset[2] + leg.inset[2]
    ## annotation positions (fraction if 'add' is FALSE, otherwise already in map coordinates)
    # inset scaling factor, used if 'add' is TRUE, otherwise 1 (explicitly because usr does not get updated directly when set)
    if (add) {
      f.inset.x <- par("usr")[2] - par("usr")[1]
      f.inset.y <- par("usr")[4] - par("usr")[3]
    } else {
      f.inset.x <- 1
      f.inset.y <- 1
    }
    ann.fr.x <- rep(leg.fr.pos$text$x[1], length(ann.txt)) + legend.inset[1] * f.inset.x - 0.01
    if (legend.pos == "bottomleft") {
      ann.fr.y <- rev(seq(from = leg.fr.pos$text$y[length(c(cbrks2, cbrks3)) - 3] - legbx.fr.ht/2, by = legbx.fr.ht, length.out = length(c(cbrks2, cbrks3)))) + legend.inset[2] * f.inset.y
    } else if (legend.pos == "left") {
      ann.fr.y <- rev(seq(from = leg.fr.pos$text$y[length(c(cbrks2, cbrks3)) - 3] - legbx.fr.ht/2, by = legbx.fr.ht, length.out = length(c(cbrks2, cbrks3))))
    } else {
      ann.fr.y <- rev(seq(from = leg.fr.pos$text$y[length(c(cbrks2, cbrks3)) - 3] - legbx.fr.ht/2, by = legbx.fr.ht, length.out = length(c(cbrks2, cbrks3)))) - legend.inset[2] * f.inset.y
    }
  }
  
  ## calculate coordinates for map positioning
  
  # map coordinates,unprojected maps need a workaround with dummy map to calculate map side ratio
  if (is.projected(map)) {
    bbx <- bbox(map)
    # map side ratio (h/w)
    msr <- apply(bbx, 1, diff)[2] / apply(bbx, 1, diff)[1]
    # plot area side ratio (h/w)
    psr <- par("pin")[2] / par("pin")[1]
  } else {
    bbx <- bbox(map)
    # set user coordinates using a dummy plot (no fast way with Spatial polygons plot, therefore construct with SpatialPoints map)
    if(!is.null(main.title)){
      par(xaxs = "i", yaxs = "i", lend = 1,mar=c(0,0,0,0),cex=graphScale)
      if(model=="niger-hype") {
        plot(SpatialPoints(coordinates(map), proj4string = CRS(proj4string(map))), col = NULL,main=main.title)
      } else {
        plot(SpatialPoints(coordinates(map), proj4string = CRS(proj4string(map))), col = NULL,main=main.title)
        #plot(SpatialPoints(coordinates(map), proj4string = CRS(proj4string(map))), col = NULL, xlim = bbx[1, ], ylim = bbx[2, ],main=main.title)
      }
    }else{
      if(model=="niger-hype") {
        plot(SpatialPoints(coordinates(map), proj4string = CRS(proj4string(map))), col = NULL, add=T)
      } else {
        plot(SpatialPoints(coordinates(map), proj4string = CRS(proj4string(map))), col = NULL, add=T)
      }
    }
    # create a map side ratio based on the device region in user coordinates and the map bounding box
    p.range.x <- diff(par("usr")[1:2])
    p.range.y <- diff(par("usr")[3:4])
    m.range.x <- diff(bbox(map)[1, ])
    m.range.y <- diff(bbox(map)[2, ])
    # map side ratio (h/w)
    msr <- m.range.y / m.range.x
    # plot area side ratio (h/w)
    psr <- p.range.y / p.range.x
  }
  
  
  # define plot limits, depending on (a) map and plot ratios (plot will be centered if left to automatic) and (b) user choice
  if (msr > psr) {
    # map is smaller than plot window in x direction, map can be moved left or right
    if (map.adj == 0) {
      pylim <- as.numeric(bbx[2, ])
      pxlim <- c(bbx[1, 1], bbx[1, 1] + diff(pylim)/psr)
      pxlim <- as.numeric(bbx[1, ])
    } else if (map.adj == .5) {
      pylim <- as.numeric(bbx[2, ])
      pxlim <- c(mean(as.numeric(bbx[1, ])) - diff(pylim)/psr/2, mean(as.numeric(bbx[1, ])) + diff(pylim)/psr/2)
    } else {
      pylim <- as.numeric(bbx[2, ])
      pxlim <- c(bbx[1, 2] - diff(pylim)/psr, bbx[1, 2])
    }
  } else {
    # map is smaller than plot window in y direction, map can be moved up or down
    if (map.adj == 0) {
      pxlim <- as.numeric(bbx[1, ])
      pylim <- c(bbx[2, 1], bbx[2, 1] + diff(pxlim)*psr)
    } else if (map.adj == .5) {
      pxlim <- as.numeric(bbx[1, ])
      pylim <- c(mean(as.numeric(bbx[2, ])) - diff(pxlim)*psr/2, mean(as.numeric(bbx[2, ])) + diff(pxlim)*psr/2)
    } else {
      pxlim <- as.numeric(bbx[1, ])
      pylim <- c(bbx[2, 2] - diff(pxlim)*psr, bbx[2, 2])
    }
  }
  
  
  ## plot the map and add legend using the positioning information derived above
  
  # map, plot in current frame if not added because a new frame was already created above for calculating all the coordinates
  if (!add) {
    par(new = TRUE)
  }
  
  #plot(subbasin.shp.area, col=NA, border="grey")
  #plot(map, col = xx$color, border = NA, add=T)
  #map2=map[which(map@data$WarningLevel==2),]
  #plot(map2, col = map2$color, border = NA, add=T)
  #map3=map[which(map@data$WarningLevel==3),]
  #plot(map3, col = map3$color, border = NA, add=T)
  
  if(!is.null(main.title)){
    if(model=="niger-hype") {
      plot(map, col = xx$color, border = NA, main=main.title,add=T)
    } else {
      if(model=="mosaic-hype") {
        plot(map, col = xx$color, border = NA, main=main.title, add=T)
      } else {
        plot(map, col = xx$color, border = NA, main=main.title, add=T)
      }
      
    }
    #plot(map, col = map$color, border = NA, main=main.title,add=T)
    #plot(map, col = map$color, border = NA, ylim = pylim, xlim = pxlim, add = add, main=main.title)
  }else{
    plot(map, col = xx$color, border = NA, add=T)
    #plot(map, col = map$color, border = NA, ylim = pylim, xlim = pxlim, add = add)
  }
  # legend
  if (plot.legend) {
    legend(legend.pos, legend = c(rep(NA, length(cbrks2) - 1),rep(NA, length(cbrks3) - 1)), inset = legend.inset, 
           col = c(ColDiffPop2(length(cbrks2) - 1),ColDiffPop3(length(cbrks3) - 1)), lty = 1, lwd = 14,  bty = "n",
           title = legend.title)
    # convert annotation positioning to map coordinates, only if 'add' is FALSE
    # then plot annotation text
    if (!add) {
      ann.mc.x <- ann.fr.x * diff(pxlim) + pxlim[1]
      ann.mc.y <- ann.fr.y * diff(pylim) + pylim[1]
      text(x = ann.mc.x, y = ann.mc.y, labels = ann.txt, adj = c(0, .5), cex = 0.8)
    } else {
      text(x = ann.fr.x, y = ann.fr.y, labels = ann.txt, adj = c(0, .5), cex = 0.8)
    }
  }
  
  
  ## scale position (reference point: lower left corner), also used as reference point for north arrow
  ## conditional on 'add'
  
  if (add) {
    
    # x position conditional on legend placement side
    if (legend.pos %in% c("bottomright", "right", "topright")) {
      lx <- par("usr")[2] - signif(diff(par("usr")[1:2])/4, 0) - legend.inset[1] * diff(par("usr")[1:2])
    } else {
      lx <- par("usr")[1] + (legend.inset[1] + 0.02) * diff(par("usr")[1:2])
    }
    
    # y position conditional legend placement position (leg.fr.pos here is already in map coordinates)
    if (legend.pos %in% c("bottomright", "bottomleft")) {
      ly <- (leg.fr.pos$rect$top - leg.fr.pos$rect$h + legend.inset[2]*f.inset.y/2)
    } else if (legend.pos %in% c("right", "left")) {
      ly <- (leg.fr.pos$rect$top - leg.fr.pos$rect$h + (legend.inset[2]/2 - .1) * f.inset.y)
    } else {
      ly <- (leg.fr.pos$rect$top - leg.fr.pos$rect$h - (legend.inset[2]/2 - .1) * f.inset.y)
    }
  } else {
    
    # x position conditional on legend placement side
    if (legend.pos %in% c("bottomright", "right", "topright")) {
      lx <- pxlim[2] - signif(diff(bbx[1,])/4, 0) - legend.inset[1] * diff(pxlim)
    } else {
      lx <- pxlim[1] + (legend.inset[1] + 0.02) * diff(pxlim)
    }
    
    # y position conditional legend placement position
    if (legend.pos %in% c("bottomright", "bottomleft")) {
      ly <- (leg.fr.pos$rect$top - leg.fr.pos$rect$h + legend.inset[2]/2) * diff(pylim) + pylim[1]
    } else if (legend.pos %in% c("right", "left")) {
      ly <- (leg.fr.pos$rect$top - leg.fr.pos$rect$h + legend.inset[2]/2 - .1) * diff(pylim) + pylim[1]
    } else {
      ly <- (leg.fr.pos$rect$top - leg.fr.pos$rect$h - legend.inset[2]/2 - .1) * diff(pylim) + pylim[1]
    }
  }
  
  # invisible unless assigned: return map with added data and color codes
  invisible(map)
  if (model=="mosaic-hype") {
    models<-"Mosaic-HYPE v1 + HGFDv1 + ECOPER"
  } else if(model=="niger-hype") {
    models<-"Niger-HYPE v1.6 + HGFDv1 + ECOPER"
  } else if(model=="ww-hype") {
    models<-"WWorld Wide-HYPE v1.3.6 + HGFDv1 + ECOPER"
  }
  
  if (method=="hypethreshold") {
    methodss<-"HYPE Threshold"
  } else if(method=="historicalyear") {
    methodss<-"Historical Year"
  } else if(method=="observationthreshold") {
    methodss<-"Local Threshold"
  } else if(method=="epicthreshold") {
    methodss<-"WAFFI Index"
  }
  
  if(method=="historicalyear") {
    expression1<-"Choosen percentage:"
  } else {
    expression1<-"Choosen Return Period:"
  }
  

  legend("bottomleft",inset=c(0,0.07),title="",
         col="white",
         pt.cex=graphScale*4,cex=2,pch=15,bty="n",
         legend=c(paste("Issue Date: ",substr(app.date, 1,10)),
                  paste("Lead time:", 5, "Days"), 
                  paste("Data source:", models ),
                  paste("Variable:", variable),
                  paste("Hazard Threshold Method:", methodss),
                  paste(expression1, wl.rp[1], wl.rp[2],wl.rp[3])))
  
  
}


ColDiffPop<-colorRampPalette(c("#da62ed", "#300275"))
### ----------------------------

PlotMapPopulation2 <- function(x, map, map1, map.subid.column = 2, var.name = "", map.adj = 0, plot.legend = T, 
                               legend.pos = "right", legend.title = NULL, legend.outer = F, legend.inset = c(0, 0), 
                               col.ramp.fun = "auto", col.breaks = NULL, plot.scale = T, plot.arrow = T, 
                               par.cex = 1, par.mar = rep(0, 4) + .1, add = FALSE, restore.par = FALSE,main.title=NULL,par.mai=NULL, model, method, variable,wl.rp) {
  graphScale<-1.6  
  wl.alpha=200  # out of 255
  plot(map1, col=NA, border="grey")
  #plot(countries.shp.area,col=NA,border="grey")  # country boundaries
  par(xaxs = "i", yaxs = "i", lend = 1,mar=c(0,0,0,0),cex=1.6)
  if (is.function(col.ramp.fun)) {
    # Case 1: a color ramp palette function is supplied
    crfun <- col.ramp.fun
    if (!is.null(col.breaks)) {
      cbrks <- col.breaks
    } else {
      cbrks <- quantile(x[, 2], probs = seq(0, 1, .1), na.rm = T)
    }
  } 
  
  cbrks <- unique(cbrks)
  if (length(cbrks) == 1) {
    cbrks <- range(cbrks) + c(-1, 1)
  }
  # discretise the modeled values in x into classed groups, add to x as new column (of type factor)
  x[, 3] <- cut(x[, 2], breaks = cbrks, include.lowest = T)
  # replace the factor levels with color codes using the color ramp function assigned above
  levels(x[, 3]) <- crfun(length(cbrks) - 1)
  # convert to character to make it conform to plotting requirements below
  x[, 3] <- as.character(x[, 3])
  # give it a name
  names(x)[3] <- "color"
  
  # add x to subid map table (in data slot, indicated by @), merge by SUBID
  if(model=="ww-hype"|model=="mosaic-hype") {
    map@data <- data.frame(map@data, x[match(map@data[, map.subid.column], x[,1]),])
  } else if(model=="niger-hype") {
    map@data <- data.frame(map@data, x[match(map@data[, "SUBID"], x[,1]),])
  }
  # plot width (inches)
  p.in.wd <- par("pin")[1]
  # legend position (fraction if 'add' is FALSE, otherwise already in map coordinates) 
  leg.fr.pos <- legend("topleft", legend = rep(NA, length(cbrks) - 1),
                       col = crfun(length(cbrks) - 1), lty = 1, lwd = 14,  bty = "n", title = "Population", plot = F)
  # legend width (fraction if 'add' is FALSE, otherwise already in map coordinates) 
  leg.fr.wd <- leg.fr.pos$rect$w
  # legend box element height (fraction), with workaround for single-class maps
  if (length(leg.fr.pos$text$y) == 1) {
    te <- legend(legend.pos, legend = rep(NA, length(cbrks)),
                 col = crfun(length(cbrks)), lty = 1, lwd = 14,  bty = "n", title = legend.title, plot = F)
    legbx.fr.ht <- diff(c(te$text$y[length(cbrks)], te$text$y[length(cbrks) - 1]))
  } else {
    legbx.fr.ht <- diff(c(leg.fr.pos$text$y[length(cbrks) - 1], leg.fr.pos$text$y[length(cbrks) - 2]))
  }
  
  ## prepare legend annotation
  
  # formatted annotation text (to be placed between legend boxes which is not possible with legend() directly)
  ann.txt <- signif(cbrks, digits = 2)
  # conditional: remove outer break points
  if (!legend.outer) {
    ann.txt[c(1, length(ann.txt))] <- ""
  }
  # annotation width (inches)
  ann.in.wd <- max(strwidth(ann.txt, "inches"))
  # legend inset required to accomodate text annotation, and scalebar (always below legend)
  leg.inset <- c(ann.in.wd/p.in.wd, if(legend.pos %in% c("bottomright", "bottomleft")) {0.1} else {0})
  
  # conditional on legend placement side (legend annotation always right of color boxes)
  if (legend.pos %in% c("bottomright", "right", "topright")) {
    
    # update legend inset
    legend.inset <- legend.inset + leg.inset
    ## annotation positions (fraction if 'add' is FALSE, otherwise already in map coordinates)
    # inset scaling factor, used if 'add' is TRUE, otherwise 1 (explicitly because usr does not get updated directly when set)
    if (add) {
      f.inset.x <- par("usr")[2] - par("usr")[1]
      f.inset.y <- par("usr")[4] - par("usr")[3]
    } else {
      f.inset.x <- 1
      f.inset.y <- 1
    }
    ann.fr.x <- rep(leg.fr.pos$text$x[1], length(ann.txt)) - legend.inset[1] * f.inset.x - 0.01
    if (legend.pos == "bottomright") {
      ann.fr.y <- rev(seq(from = leg.fr.pos$text$y[length(cbrks) - 1] - legbx.fr.ht/2, by = legbx.fr.ht, length.out = length(cbrks))) + legend.inset[2] * f.inset.y
    } else if (legend.pos == "right") {
      ann.fr.y <- rev(seq(from = leg.fr.pos$text$y[length(cbrks) - 1] - legbx.fr.ht/2, by = legbx.fr.ht, length.out = length(cbrks)))
    } else {
      ann.fr.y <- rev(seq(from = leg.fr.pos$text$y[length(cbrks) - 1] - legbx.fr.ht/2, by = legbx.fr.ht, length.out = length(cbrks))) - legend.inset[2] * f.inset.y
    }
    
  } else {
    # left side legend
    # update legend inset
    legend.inset[2] <- legend.inset[2] + leg.inset[2]
    ## annotation positions (fraction if 'add' is FALSE, otherwise already in map coordinates)
    # inset scaling factor, used if 'add' is TRUE, otherwise 1 (explicitly because usr does not get updated directly when set)
    if (add) {
      f.inset.x <- par("usr")[2] - par("usr")[1]
      f.inset.y <- par("usr")[4] - par("usr")[3]
    } else {
      f.inset.x <- 1
      f.inset.y <- 1
    }
    ann.fr.x <- rep(leg.fr.pos$text$x[1], length(ann.txt)) + legend.inset[1] * f.inset.x - 0.01
    if (legend.pos == "bottomleft") {
      ann.fr.y <- rev(seq(from = leg.fr.pos$text$y[length(cbrks) - 1] - legbx.fr.ht/2, by = legbx.fr.ht, length.out = length(cbrks))) + legend.inset[2] * f.inset.y
    } else if (legend.pos == "left") {
      ann.fr.y <- rev(seq(from = leg.fr.pos$text$y[length(cbrks) - 1] - legbx.fr.ht/2, by = legbx.fr.ht, length.out = length(cbrks)))
    } else {
      ann.fr.y <- rev(seq(from = leg.fr.pos$text$y[length(cbrks) - 1] - legbx.fr.ht/2, by = legbx.fr.ht, length.out = length(cbrks))) - legend.inset[2] * f.inset.y
    }
  }
  
  ## calculate coordinates for map positioning
  
  # map coordinates,unprojected maps need a workaround with dummy map to calculate map side ratio
  if (is.projected(map)) {
    bbx <- bbox(map)
    # map side ratio (h/w)
    msr <- apply(bbx, 1, diff)[2] / apply(bbx, 1, diff)[1]
    # plot area side ratio (h/w)
    psr <- par("pin")[2] / par("pin")[1]
  } else {
    bbx <- bbox(map)
    # set user coordinates using a dummy plot (no fast way with Spatial polygons plot, therefore construct with SpatialPoints map)
    if(!is.null(main.title)){
      par(xaxs = "i", yaxs = "i", lend = 1,mar=c(0,0,0,0),cex=graphScale)
      if(model=="niger-hype") {
        plot(SpatialPoints(coordinates(map), proj4string = CRS(proj4string(map))), col = NULL,main=main.title)
      } else {
        plot(SpatialPoints(coordinates(map), proj4string = CRS(proj4string(map))), col = NULL,main=main.title)
        #plot(SpatialPoints(coordinates(map), proj4string = CRS(proj4string(map))), col = NULL, xlim = bbx[1, ], ylim = bbx[2, ],main=main.title)
      }
    }else{
      if(model=="niger-hype") {
        plot(SpatialPoints(coordinates(map), proj4string = CRS(proj4string(map))), col = NULL, add=T)
      } else {
        plot(SpatialPoints(coordinates(map), proj4string = CRS(proj4string(map))), col = NULL, add=T)
      }
    }
    # create a map side ratio based on the device region in user coordinates and the map bounding box
    p.range.x <- diff(par("usr")[1:2])
    p.range.y <- diff(par("usr")[3:4])
    m.range.x <- diff(bbox(map)[1, ])
    m.range.y <- diff(bbox(map)[2, ])
    # map side ratio (h/w)
    msr <- m.range.y / m.range.x
    # plot area side ratio (h/w)
    psr <- p.range.y / p.range.x
  }
  
  
  # define plot limits, depending on (a) map and plot ratios (plot will be centered if left to automatic) and (b) user choice
  if (msr > psr) {
    # map is smaller than plot window in x direction, map can be moved left or right
    if (map.adj == 0) {
      pylim <- as.numeric(bbx[2, ])
      pxlim <- c(bbx[1, 1], bbx[1, 1] + diff(pylim)/psr)
      pxlim <- as.numeric(bbx[1, ])
    } else if (map.adj == .5) {
      pylim <- as.numeric(bbx[2, ])
      pxlim <- c(mean(as.numeric(bbx[1, ])) - diff(pylim)/psr/2, mean(as.numeric(bbx[1, ])) + diff(pylim)/psr/2)
    } else {
      pylim <- as.numeric(bbx[2, ])
      pxlim <- c(bbx[1, 2] - diff(pylim)/psr, bbx[1, 2])
    }
  } else {
    # map is smaller than plot window in y direction, map can be moved up or down
    if (map.adj == 0) {
      pxlim <- as.numeric(bbx[1, ])
      pylim <- c(bbx[2, 1], bbx[2, 1] + diff(pxlim)*psr)
    } else if (map.adj == .5) {
      pxlim <- as.numeric(bbx[1, ])
      pylim <- c(mean(as.numeric(bbx[2, ])) - diff(pxlim)*psr/2, mean(as.numeric(bbx[2, ])) + diff(pxlim)*psr/2)
    } else {
      pxlim <- as.numeric(bbx[1, ])
      pylim <- c(bbx[2, 2] - diff(pxlim)*psr, bbx[2, 2])
    }
  }
  
  
  ## plot the map and add legend using the positioning information derived above
  
  # map, plot in current frame if not added because a new frame was already created above for calculating all the coordinates
  if (!add) {
    par(new = TRUE)
  }
  if(!is.null(main.title)){
    if(model=="niger-hype") {
      plot(map, col = map$color, border = NA, main=main.title,add=T)
    } else {
      if(model=="niger-hype") {
        plot(map, col = map$color, border = NA, main=main.title, add=T)
      } else {
        plot(map, col = map$color, border = NA, main=main.title, add=T)
      }
      
    }
    #plot(map, col = map$color, border = NA, main=main.title,add=T)
    #plot(map, col = map$color, border = NA, ylim = pylim, xlim = pxlim, add = add, main=main.title)
  }else{
    plot(map, col = map$color, border = NA, add=T)
    #plot(map, col = map$color, border = NA, ylim = pylim, xlim = pxlim, add = add)
  }
  # legend
  if (plot.legend) {
    legend(legend.pos, legend = rep(NA, length(cbrks) - 1), inset = legend.inset, 
           col = crfun(length(cbrks) - 1), lty = 1, lwd = 14,  bty = "n",
           title = legend.title)
    # convert annotation positioning to map coordinates, only if 'add' is FALSE
    # then plot annotation text
    if (!add) {
      ann.mc.x <- ann.fr.x * diff(pxlim) + pxlim[1]
      ann.mc.y <- ann.fr.y * diff(pylim) + pylim[1]
      text(x = ann.mc.x, y = ann.mc.y, labels = ann.txt, adj = c(0, .5), cex = 0.8)
    } else {
      text(x = ann.fr.x, y = ann.fr.y, labels = ann.txt, adj = c(0, .5), cex = 0.8)
    }
  }
  
  
  ## scale position (reference point: lower left corner), also used as reference point for north arrow
  ## conditional on 'add'
  
  if (add) {
    
    # x position conditional on legend placement side
    if (legend.pos %in% c("bottomright", "right", "topright")) {
      lx <- par("usr")[2] - signif(diff(par("usr")[1:2])/4, 0) - legend.inset[1] * diff(par("usr")[1:2])
    } else {
      lx <- par("usr")[1] + (legend.inset[1] + 0.02) * diff(par("usr")[1:2])
    }
    
    # y position conditional legend placement position (leg.fr.pos here is already in map coordinates)
    if (legend.pos %in% c("bottomright", "bottomleft")) {
      ly <- (leg.fr.pos$rect$top - leg.fr.pos$rect$h + legend.inset[2]*f.inset.y/2)
    } else if (legend.pos %in% c("right", "left")) {
      ly <- (leg.fr.pos$rect$top - leg.fr.pos$rect$h + (legend.inset[2]/2 - .1) * f.inset.y)
    } else {
      ly <- (leg.fr.pos$rect$top - leg.fr.pos$rect$h - (legend.inset[2]/2 - .1) * f.inset.y)
    }
  } else {
    
    # x position conditional on legend placement side
    if (legend.pos %in% c("bottomright", "right", "topright")) {
      lx <- pxlim[2] - signif(diff(bbx[1,])/4, 0) - legend.inset[1] * diff(pxlim)
    } else {
      lx <- pxlim[1] + (legend.inset[1] + 0.02) * diff(pxlim)
    }
    
    # y position conditional legend placement position
    if (legend.pos %in% c("bottomright", "bottomleft")) {
      ly <- (leg.fr.pos$rect$top - leg.fr.pos$rect$h + legend.inset[2]/2) * diff(pylim) + pylim[1]
    } else if (legend.pos %in% c("right", "left")) {
      ly <- (leg.fr.pos$rect$top - leg.fr.pos$rect$h + legend.inset[2]/2 - .1) * diff(pylim) + pylim[1]
    } else {
      ly <- (leg.fr.pos$rect$top - leg.fr.pos$rect$h - legend.inset[2]/2 - .1) * diff(pylim) + pylim[1]
    }
  }
  
  # invisible unless assigned: return map with added data and color codes
  invisible(map)
  if (model=="mosaic-hype") {
    models<-"Mosaic-HYPE v1 + HGFDv1 + ECOPER"
  } else if(model=="niger-hype") {
    models<-"Niger-HYPE v1.6 + HGFDv1 + ECOPER"
  } else if(model=="ww-hype") {
    models<-"WWorld Wide-HYPE v1.3.6 + HGFDv1 + ECOPER"
  }
  
  if (method=="hypethreshold") {
    methodss<-"HYPE Threshold"
  } else if(method=="historicalyear") {
    methodss<-"Historical Year"
  } else if(method=="observationthreshold") {
    methodss<-"Local Threshold"
  } else if(method=="epicthreshold") {
    methodss<-"WAFFI Index"
  }
  
  if(method=="historicalyear") {
    expression1<-"Choosen percentage:"
  } else {
    expression1<-"Choosen Return Period:"
  }
  
  
  legend("bottomleft",inset=c(0,0.07),title="",
         col="white",
         pt.cex=graphScale*4,cex=2,pch=15,bty="n",
         legend=c(paste("Issue Date: ",substr(app.date, 1,10)),
                  paste("Lead time:", 5, "Days"), 
                  paste("Data source:", models ),
                  paste("Variable:", variable),
                  paste("Hazard Threshold Method:", methodss),
                  paste(expression1, wl.rp[1], wl.rp[2],wl.rp[3])))
  
  
}


ImpactOutput_Map<-function (appInput  = app.input, appSetput=app.setup, popmethod=1) {
  exposure<-raster(paste(appSetput$resDir, appSetput$population.density.file, sep="/"))
  load(paste(appSetput$runDir, "shapefile", paste0(model.name, ".Rdata"), sep="/"))
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
  exposure.bassin<-raster::extract(exposure, shapefileData)
  
  exposure.bassin.aggregate<-unlist(lapply(exposure.bassin, FUN = mean, na.rm=T))
  population.basin<-exposure.bassin.aggregate*shapefileData@data$AREA/1e6
  warningclasses<-read.table(paste(appSetput$resDir ,
                                   paste0("002_MapWarningClasse_",appInput$variable,"_", model.name, "_", appInput$area, ".txt"), sep="/"), sep=",", h=T)
  data.output<-cbind(warningclasses, round(population.basin,0))
  colnames(data.output)[ncol(data.output)]<-"population.basin"
  data.output1<-melt(data.output, id.vars = c("SUBID", "population.basin"))
  data.output1<-data.output1[,c(1,3,4,2)]
  data.output1<-subset(data.output1,as.numeric(data.output1$value)>=0)
  colnames(data.output1)<-c("SUBID", "DATE", "Hazard", "Exposure")
  write.table(data.output1, paste(appSetput$resDir, paste0("004_Impact_",appInput$variable,"_", model.name, "_", appInput$area, ".txt"), sep="/"), sep="\t", 
                                  row.names=F, quote=F)
  #exposure.bassin.scaled<-round(exposure.bassin.aggredate/max(exposure.bassin.aggredate), 3)
  if(popmethod==1) {
    x<-data.frame(SUBID=shapefileData@data$SUBID, WarningLevel=warningclasses$WarningLevel,Population=population.basin)
    #x[,2]<-c(rep(c(1,2,3),139), 1,2)
    x=subset(x, as.numeric(x$WarningLevel)>1)
    area.shapefile1<-shapefileData[match(x$SUBID,shapefileData@data$SUBID),]
  } else {
    x<-as.data.frame(cbind(shapefileData@data$SUBID, population.basin))
    colnames(x)<-c("SUBID", "Population")
    x=subset(x, as.numeric(warningclasses$WarningLevel)>1)
    area.shapefile1<-shapefileData[match(x$SUBID,shapefileData@data$SUBID),]
  }
  if(popmethod==1) {
    plotFileName<-paste(appSetput$resDir, paste0("004_ExposureMap_V1_",appInput$variable,"_", model.name, "_", appInput$area, ".png"), sep="/")
    CairoPNG(filename = plotFileName, width = 3111, height = 1489, units = "px",bg = "white")
    PlotMapPopulation1(x = x, 
                       map = area.shapefile1,
                       map1=shapefileData,
                       var.name="Population",
                       plot.scale = F,
                       plot.arrow = F,
                       legend.title = "",
                       legend.pos = "topleft",
                       par.mar = c(0,0,0,0),par.cex = 3,par.mai=c(0,0,0,0), add=T,
                       model=appInput$model, 
                       method=appInput$alertmethod, 
                       variable=appInput$variable,
                       wl.rp=appInput$returnPeriodIN)
    addscalebar(pos="bottomright", lwd=4, htin = 0.4,label.cex = 1.5)
    addnortharrow(pos="topright",scale = 4)
    # Close plot
    dev.off()
  } else
  if(popmethod==2) {
    plotFileName<-paste(appSetput$resDir, paste0("004_ExposureMap_V2_",appInput$variable,"_", model.name, "_", appInput$area, ".png"), sep="/")
    CairoPNG(filename = plotFileName, width = 3111, height = 1489, units = "px",bg = "white")
    PlotMapPopulation2(x = x, 
                      map = area.shapefile1,
                      map1=shapefileData,
                      col.ramp.fun = ColDiffPop ,
                      var.name="Population",
                      plot.scale = F,
                      plot.arrow = F,
                      legend.title = "",
                      legend.pos = "topleft",
                      par.mar = c(0,0,0,0),par.cex = 3,par.mai=c(0,0,0,0), 
                      add=T,
                      model=appInput$model, 
                      method=appInput$alertmethod,
                      variable=appInput$variable,
                      wl.rp=appInput$returnPeriodIN)
    addscalebar(pos="bottomright", lwd=4, htin = 0.4,label.cex = 1.5)
    addnortharrow(pos="topright",scale = 4)
    # Close plot
    dev.off()
  }
  
}

#internal log succesful sourcing of file
if(app.sys=="tep"){rciop.log ("DEBUG", paste("all functions sourced"), "/util/R/hypeapps-plot-impact-map.R")}


