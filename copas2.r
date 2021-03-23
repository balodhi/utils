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
  if(!all(is.na(plate$Source.well)) & reflx) {
    stop("Data is from reflx please turn off the the reflx option")
  }
  plate <- plate[!is.na(plate$TOF),] #remove rows in TOF if NA
  plate <- plate[,!is.na(plate[1,])] # remove NA columns
  if (!reflx) {
    plate <- plate[(plate$Extinction>=extmin & plate$Extinction<=extmax) | plate$Extinction == -1,] #get data in range
    
    plate$Column <- as.factor(plate$Column) #convert columns's column to catagories
    plate$Row <- as.factor(plate$Row) #convert Row's column to catagories
  }
  
  #modplate <- with(plate, data.frame(row=Row, col=as.factor(Column), sort=Sorted.status, TOF=TOF, EXT=Extinction, time=Time, green=Green, yellow=Yellow, red=Red))
  #modplate <- modplate %>% group_by(modplate$row, modplate$col) %>% do(extractTime(.)) %>% data.frame()
  #modplate[,10:13] <- apply(modplate[,c(5, 7:9)], 2, function(x){x/modplate$TOF})
  #colnames(modplate)[10:13] <- c("norm.EXT", "norm.green", "norm.yellow", "norm.red")
  return (plate)
}

extractTime <- function(plate){
  plate$time <- plate$time - min(plate$time)
  return(plate)
}

read_data <- function(file, tofmin = 60, tofmax = 2000, extmin = 0,
                      extmax = 10000, SVM = TRUE, levels = 2, ...){
  
  # Read the raw sorter files and make the row names
  
  plate <- plateread(file, tofmin, tofmax, extmin, extmax, ...)
  
  if (all(plate$row == 0)) {
      modplate <- with(plate,
                       data.frame(row=stringr::str_split_fixed(Source.well, "[0-9]", 2)[,1],
                                  col=as.factor(stringr::str_split_fixed(Source.well, "[A-Z]", 2)[,2]),
                                  sort=Sorted.status,
                                  TOF=TOF,
                                  EXT=Extinction,
                                  time=as.numeric(Time),
                                  green=Green,
                                  yellow=Yellow,
                                  red=Red))
  }
  # Extract the time so that it is realtive to the first worm sorted
  
  modplate <- modplate %>%
    dplyr::group_by(row, col) %>%
    dplyr::do(extractTime(.))
  modplate <- data.frame(modplate)
  
  # Normalize the optical values by time of flight
  
  modplate[, 10:13] <- apply(modplate[, c(5, 7:9)], 2,
                             function(x) x / modplate$TOF)
  colnames(modplate)[10:13] <- c("norm.EXT", "norm.green", "norm.yellow",
                                 "norm.red")
  
  # Handle the SVM predictions if requested
  
  plateprediction <- kernlab::predict(
      bubbleSVMmodel_noProfiler,
      modplate[,3:length(modplate)],
      type="probabilities")
  modplate$object <- plateprediction[, "1"]
  modplate$call50 <- factor(as.numeric(modplate$object > 0.5),
                              levels=c(1, 0), labels=c("object", "bubble"))
  
  
  modplate$stage <- ifelse(modplate$TOF >= 60 & modplate$TOF < 90, "L1",
                           ifelse(modplate$TOF >= 90 & modplate$TOF < 200,
                                  "L2/L3",
                                  ifelse(modplate$TOF >= 200
                                         & modplate$TOF < 300, "L4",
                                         ifelse(modplate$TOF >= 300,
                                                "adult", NA))))
  return(modplate)
}

summarizePlate <- function(plate, ends=FALSE, quantiles=FALSE) {
  plate <- fillWells(plate)
  plate[2:13] <- lapply(plate[2:13], as.numeric)
  processed <- suppressWarnings(plate %>% group_by(row, col) %>%
                                  summarise(n=ifelse(length(TOF[!is.na(TOF)])==0, NA, length(TOF[!is.na(TOF)])),

                                            sTOF = sum(TOF),

                                            mean.TOF = mean(TOF, na.rm=TRUE),
                                            min.TOF=as.numeric(quantile(TOF, na.rm=TRUE)[1]),
                                            q10.TOF=as.numeric(quantile(TOF, probs=0.1, na.rm=TRUE)[1]),
                                            q25.TOF=as.numeric(quantile(TOF, probs=0.25, na.rm=TRUE)[1]),
                                            median.TOF=median(TOF, na.rm=TRUE),
                                            q75.TOF=as.numeric(quantile(TOF, probs=0.75, na.rm=TRUE)[1]),
                                            q90.TOF=as.numeric(quantile(TOF, probs=0.90, na.rm=TRUE)[1]),
                                            max.TOF=as.numeric(quantile(TOF, na.rm=TRUE)[5]),

                                            mean.Extinction = mean(EXT, na.rm=TRUE),
                                            min.Extinction=as.numeric(quantile(EXT, na.rm=TRUE)[1]),
                                            q10.Extinction=as.numeric(quantile(EXT, probs=0.1, na.rm=TRUE)[1]),
                                            q25.Extinction=as.numeric(quantile(EXT, probs=0.25, na.rm=TRUE)[1]),
                                            median.Extinction=median(EXT, na.rm=TRUE),
                                            q75.Extinction=as.numeric(quantile(EXT, probs=0.75, na.rm=TRUE)[1]),
                                            q90.Extinction=as.numeric(quantile(EXT, probs=0.90, na.rm=TRUE)[1]),
                                            max.Extinction=as.numeric(quantile(EXT, na.rm=TRUE)[5]),

                                            mean.red=mean(red, na.rm=TRUE),
                                            min.red=as.numeric(quantile(red, na.rm=TRUE)[1]),
                                            q10.red=as.numeric(quantile(red, probs=0.1, na.rm=TRUE)[1]),
                                            q25.red=as.numeric(quantile(red, probs=0.25, na.rm=TRUE)[1]),
                                            median.red=median(red, na.rm=TRUE),
                                            q75.red=as.numeric(quantile(red, probs=0.75, na.rm=TRUE)[1]),
                                            q90.red=as.numeric(quantile(red, probs=0.9, na.rm=TRUE)[1]),
                                            max.red=as.numeric(quantile(red, na.rm=TRUE)[5]),

                                            mean.green=mean(green, na.rm=TRUE),
                                            min.green=as.numeric(quantile(green, na.rm=TRUE)[1]),
                                            q10.green=as.numeric(quantile(green, probs=0.1, na.rm=TRUE)[1]),
                                            q25.green=as.numeric(quantile(green, probs=0.25, na.rm=TRUE)[1]),
                                            median.green=median(green, na.rm=TRUE),
                                            q75.green=as.numeric(quantile(green, probs=0.75, na.rm=TRUE)[1]),
                                            q90.green=as.numeric(quantile(green, probs=0.9, na.rm=TRUE)[1]),
                                            max.green=as.numeric(quantile(green, na.rm=TRUE)[5]),

                                            mean.yellow=mean(yellow, na.rm=TRUE),
                                            min.yellow=as.numeric(quantile(yellow, na.rm=TRUE)[1]),
                                            q10.yellow=as.numeric(quantile(yellow, probs=0.1, na.rm=TRUE)[1]),
                                            q25.yellow=as.numeric(quantile(yellow, probs=0.25, na.rm=TRUE)[1]),
                                            median.yellow=median(yellow, na.rm=TRUE),
                                            q75.yellow=as.numeric(quantile(yellow, probs=0.75, na.rm=TRUE)[1]),
                                            q90.yellow=as.numeric(quantile(yellow, probs=0.9, na.rm=TRUE)[1]),
                                            max.yellow=as.numeric(quantile(yellow, na.rm=TRUE)[5])
                                  ))
  if(!ends){
    processed <- processed[,-(grep("min", colnames(processed)))]
    processed <- processed[,-(grep("max", colnames(processed)))]
  }
  if(!quantiles){
    processed <- processed[,-(grep("q", colnames(processed)))]
  }
  analysis <- processed
  analysis[analysis$mean.TOF==-1 | is.na(analysis$mean.TOF),which(colnames(analysis)=="n"):ncol(analysis)] <- NA
  if (colnames(processed)[4] == "sTOF") {
    colnames(processed)[4] <- "TOF"
  }

  return(processed)
}

fillWells <- function(plate, rename=TRUE){
  complete = data.frame(row=rep(LETTERS[1:8], each=12), col=rep(1:3, 8))
  
  
  plate = merge(plate, complete, by=c("row", "col"), all=TRUE)
  return(plate)
}

#file3 <- '~/Documents/PI staining setup/data/1.csv'
file3 <- 'D:\\datafromuos\\copas.csv'
data2 <-read_data(file=file3, sep = ',', droplast = 1, reflx=FALSE)

#file4 <- '~/Documents/PI staining setup/data/2.csv'
file4 <- 'D:\\datafromuos\\2.csv'
data3 <-read_data(file=file4, sep = ',', droplast = 1, reflx=FALSE)
plate <- summarizePlate(plate=data2)

plotTrait(plate=data2, "TOF", "EXT", type="line")
plotTrait(plate= plate, trait = "n")
plotTrait(plate = plate, trait = "TOF", type="heat")

plotTrait(plate = data3, trait = "TOF", type="hist")
plotTrait(plate=data3, "TOF", "EXT", type="scatter")