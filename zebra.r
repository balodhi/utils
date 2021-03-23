library(dplyr)
library(knitr)
library(ggplot2)
library(reshape2)

readFile <- function(file,sep=',') {
  #data <- read.delim(file=file, header=T, na.strings=c("n/a"), as.is=T, stringsAsFactors=F)
  data <- read.delim(file=file, header=T, na.strings=c("n/a"), sep=sep)
  return (data)
}

plateread <- function(file, reflx=FALSE, extmin=0, extmax=10000, tofmin=0, tofmax=10000, sep=',',droplast=0) {
  plate <- readFile(file)
  plate <- plate[1:(length(plate)-droplast)]
  
  
  return (plate)
}

extractTime <- function(plate){
  plate$time <- plate$time - min(plate$time)
  return(plate)
}



fillWells <- function(plate, rename=TRUE){
  complete = data.frame(row=rep(LETTERS[1:4], each=6), col=rep(1:6, 4))
  
  
  plate = merge(plate, complete, by=c("row", "col"), all=TRUE)
  return(plate)
}


plotTrait = function(plate, trait, trait2 = NULL, type="heat"){
  plate = data.frame(plate)
  if (colnames(plate)[4] == "Row") {
    colnames(plate)[4] <- "row"
    colnames(plate)[5] <- "col"
  }
  if(type == "heat"){
    plate$label <- ifelse(is.na(plate[,which(colnames(plate)==trait)]), "", plate[,which(colnames(plate)==trait)])
    plot = ggplot(plate) + geom_rect(aes_string(xmin=0,xmax=5,ymin=0,ymax=5,fill=trait)) +
      geom_text(data=plate,aes(x=2.5,y=2.5, label = label, colour="white"), colour="white")+
      theme(axis.ticks.x=element_line(size = 0), axis.ticks.y=element_line(size = 0), axis.text.x=element_text(size = 0), axis.text.y=element_text(size = 0)) +
      xlab("Columns") + ylab("Rows")
  } else if(type == "hist"){
    if(sum(is.na(plate[,which(colnames(plate)==trait)])) > 0){
      badWells = plate[is.na(plate[,which(colnames(plate)==trait)]),c("row", "col")]
      badWells = paste0(badWells$row, badWells$col)
      plate = removeWells(plate, badWells, drop=TRUE)
    }
    
    plot = ggplot(plate) + geom_histogram(aes_string(x = trait), binwidth = diff(range(plate[[trait]]))/15) + xlab(trait) + ylab("Count") +
      theme(axis.text.x=element_text(size="10", angle=45, hjust=1), axis.text.y=element_text(size="10"))
  }
  else if(type == "scatter"){
    plot = ggplot(plate) + geom_point(aes_string(x = trait, y = trait2), size=1.5) + xlab(trait) + ylab(trait2) +
      theme(axis.text.x=element_text(size="10", angle=45, hjust=1), axis.text.y=element_text(size="10"))
  }
  else if (type == 'line') {
    plot = ggplot(plate) + geom_line(aes_string(x = trait, y=trait2)) + xlab(trait) + ylab(trait2) +
      theme(axis.text.x=element_text(size="10", angle=45, hjust=1), axis.text.y=element_text(size="10"))
  }
  else {
    stop("Unrecognized plot type")
  }
  plot = plot + facet_grid(row~col, drop=FALSE)
  return(plot)
}

plotCorMatrix = function(plate_summary1, plate_summary2=plate_summary1){
  plate_summary1 = fillWells(plate_summary1, rename=FALSE)
  plate_summary2 = fillWells(plate_summary2, rename=FALSE)
 
  if(ncol(plate_summary1) != ncol(plate_summary2)){
    stop("Both plates have to have the same number of traits")
  }
  corDF = melt(cor(plate_summary1[,-(1:2)], plate_summary2[,-(1:2)], use = "complete.obs"))
  colnames(corDF) = c("Plate1", "Plate2", "Correlation")
  ggplot(corDF, aes(Plate1, Plate2, fill = Correlation)) + 
    geom_tile() + scale_fill_gradient2("Correlation",low = "blue", high = "red", mid = "green", limits = c(-1,1)) +
    theme(axis.text.x=element_text(angle = 90, hjust = 1)) + xlab("Plate 1") + ylab("Plate 2")
}

summarizePlate <- function(plate, ends=FALSE, quantiles=FALSE) {
  plate <- fillWells(plate)
  plate[2:5] <- lapply(plate[2:5], as.numeric)
  processed <- suppressWarnings(plate %>% group_by(row, col,const) %>%
                                  summarise(n=ifelse(length(vals[!is.na(vals)])==0, NA, length(vals[!is.na(vals)])),
                                            
                                            sn = sum(vals),
                                            
                                            mean.vals = mean(vals, na.rm=TRUE),
                                            min.vals=as.numeric(quantile(vals, na.rm=TRUE)[1]),
                                            q10.vals=as.numeric(quantile(vals, probs=0.1, na.rm=TRUE)[1]),
                                            q25.vals=as.numeric(quantile(vals, probs=0.25, na.rm=TRUE)[1]),
                                            median.vals=median(vals, na.rm=TRUE),
                                            q75.vals=as.numeric(quantile(vals, probs=0.75, na.rm=TRUE)[1]),
                                            q90.vals=as.numeric(quantile(vals, probs=0.90, na.rm=TRUE)[1]),
                                            max.vals=as.numeric(quantile(vals, na.rm=TRUE)[5])
                                  ))
                                            
                                            
  if(!quantiles){
    processed <- processed[,-(grep("q", colnames(processed)))]
  }
  
    return(processed)
}

file3 <- '~/Documents/PI staining setup/data/1.csv'
data2 <-read_data(file=file3, sep = ',', droplast = 1, reflx=FALSE)

file4 <- '~/Documents/PI staining setup/data/2.csv'
data3 <-read_data(file=file4, sep = ',', droplast = 1, reflx=FALSE)
plate <- summarizePlate(plate=data3)

plotTrait(plate=data3, "TOF", "EXT", type="line")
plotTrait(plate= plate, trait = "n")
plotTrait(plate = plate, trait = "TOF", type="heat")

plotTrait(plate = data3, trait = "TOF", type="hist")
plotTrait(plate=data3, "TOF", "EXT", type="scatter")