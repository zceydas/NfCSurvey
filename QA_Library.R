# BPRU Questionnaire Analysis Library
#
# nsepeda1@jhmi.edu 2018.10.18
# Scales are sorted alphabetically

max.nonan <- function(x) ifelse( !all(is.na(x)), max(x, na.rm=T), NA)
#****************************************************************************
# 5D-ASC (5 Dimension Altered States of Consciousness)
# Scores the 94 item version or 43 item version depending on what data is provided
fdasc.scoring <- function(data,keep.vars,fdasc){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }

  fdasc.scores.comb <- c()
  raw.dat <- c()
  raw.colnames <- colnames(data)

  if(length(grep("5DASC_", raw.colnames)) == 94){
    if (missing(fdasc)) {
      fdasc <- c(paste('5DASC_',1:94,sep=""))
    }
    
    raw.dat <- data[,fdasc]
    
    ### 5 dimensions
    vars.5dascOceanBound <- paste0('5DASC_',c(1,3,9,12,16,18,26,34,35,36,40,41,42,
                                              45,50,52,57,62,63,69,71,73,81,86,87,91,94))
    vars.5dascDreadEgoDiss <- paste0('5DASC_', c(6,8,21,27,32,38,43,44,46,47,53,56,
                                                 60,64,67,78,79,80,85,88,89))
    vars.5dascVisionRestruct <- paste0('5DASC_', c(7,14,20,22,23,28,31,33,39,54,58,
                                                   70,72,75,77,82,83,90))
    vars.5dascAuditoryAlt <- paste0('5DASC_', c(4,5,11,13,19,25,30,48,49,55,65,66,
                                                74,76,92,93))
    vars.5dascVigilanceRed <- paste0('5DASC_', c(2,10,15,17,24,29,37,51,59,61,68,84))
    
    #calculate mean scores for each subscale
    fdasc.oceanicBoundlessness      <- rowSums(data[,vars.5dascOceanBound])/(length(vars.5dascOceanBound))
    fdasc.dreadEgoDissolution      <- rowSums(data[,vars.5dascDreadEgoDiss])/(length(vars.5dascDreadEgoDiss))
    fdasc.visionaryRestructuralization      <- rowSums(data[,vars.5dascVisionRestruct])/(length(vars.5dascVisionRestruct))
    fdasc.auditoryAlterations      <- rowSums(data[,vars.5dascAuditoryAlt])/(length(vars.5dascAuditoryAlt))
    fdasc.vigilanceReduction      <- rowSums(data[,vars.5dascVigilanceRed])/(length(vars.5dascVigilanceRed))
    
   
     ### 11 lower-order subscales
    vars.5dascUnity <- c(paste('5DASC_',c(18,34,41,42,52),sep=""))
    vars.5dascSpiritual <- c(paste('5DASC_',c(9,81,94),sep=""))
    vars.5dascBliss <- c(paste('5DASC_',c(12,86,91),sep=""))
    vars.5dascInsight <- c(paste('5DASC_',c(50,69,77),sep=""))
    vars.5dascDisembodiment <- c(paste('5DASC_',c(26,62,63),sep=""))
    vars.5dascImpairedCognition <- c(paste('5DASC_',c(8,27,38,47,64,67,78),sep=""))
    vars.5dascAnxiety <- c(paste('5DASC_',c(32,43,44,46,56,89),sep=""))
    vars.5dascComplexImagery <- c(paste('5DASC_',c(39,72,82),sep=""))
    vars.5dascElementImagery <- c(paste('5DASC_',c(14,22,33),sep=""))
    vars.5dascSynesthesiae <- c(paste('5DASC_',c(20,23,75),sep=""))
    vars.5dascChangedPercept <- c(paste('5DASC_',c(28,31,54),sep=""))

    #calculate mean scores for each subscale
    fdasc.unity      <- rowSums(data[,vars.5dascUnity])/(length(vars.5dascUnity))
    fdasc.spiritual      <- rowSums(data[,vars.5dascSpiritual])/(length(vars.5dascSpiritual))
    fdasc.bliss      <- rowSums(data[,vars.5dascBliss])/(length(vars.5dascBliss))
    fdasc.insight      <- rowSums(data[,vars.5dascInsight])/(length(vars.5dascInsight))
    fdasc.disembodiment      <- rowSums(data[,vars.5dascDisembodiment])/(length(vars.5dascDisembodiment))
    fdasc.impairedCognition      <- rowSums(data[,vars.5dascImpairedCognition])/(length(vars.5dascImpairedCognition))
    fdasc.anxiety     <- rowSums(data[,vars.5dascAnxiety])/(length(vars.5dascAnxiety))
    fdasc.complexImagery      <- rowSums(data[,vars.5dascComplexImagery])/(length(vars.5dascComplexImagery))
    fdasc.elementaryImagery      <- rowSums(data[,vars.5dascElementImagery])/(length(vars.5dascElementImagery))
    fdasc.synesthesiae     <- rowSums(data[,vars.5dascSynesthesiae])/(length(vars.5dascSynesthesiae))
    fdasc.changedPercept      <- rowSums(data[,vars.5dascChangedPercept])/(length(vars.5dascChangedPercept))

    fdasc.scores.comb <- cbind(fdasc.oceanicBoundlessness, fdasc.dreadEgoDissolution,
                               fdasc.visionaryRestructuralization, fdasc.auditoryAlterations, fdasc.vigilanceReduction,
                               fdasc.unity,fdasc.spiritual,fdasc.bliss,fdasc.insight,fdasc.disembodiment,fdasc.impairedCognition,
                               fdasc.anxiety,fdasc.complexImagery,fdasc.elementaryImagery,fdasc.synesthesiae,fdasc.changedPercept)
  } else if(length(grep("5DASC_", raw.colnames)) == 43){
    
    if (missing(fdasc)) {
      fdasc <- c(paste('5DASC_',1:43,sep=""))
    }
    
    raw.dat <- data[,fdasc]
    
    # 11 lower order subscales
    vars.5dascUnity <- c(paste('5DASC_',c(5,15,18,19,25),sep=""))
    vars.5dascSpiritual <- c(paste('5DASC_',c(2,37,42),sep=""))
    vars.5dascBliss <- c(paste('5DASC_',c(3,39,41),sep=""))
    vars.5dascInsight <- c(paste('5DASC_',c(24,32,35),sep=""))
    vars.5dascDisembodiment <- c(paste('5DASC_',c(9,28,29),sep=""))
    vars.5dascImpairedCognition <- c(paste('5DASC_',c(1,10,16,23,30,31,36),sep=""))
    vars.5dascAnxiety <- c(paste('5DASC_',c(13,20,21,22,27,40),sep=""))
    vars.5dascComplexImagery <- c(paste('5DASC_',c(17,33,38),sep=""))
    vars.5dascElementImagery <- c(paste('5DASC_',c(4,7,14),sep=""))
    vars.5dascSynesthesiae <- c(paste('5DASC_',c(6,8,34),sep=""))
    vars.5dascChangedPercept <- c(paste('5DASC_',c(11,12,26),sep=""))
    vars.5dascIntensity <- c('5DASC_43')

    fdasc.unity      <- rowSums(data[,vars.5dascUnity])/(length(vars.5dascUnity))
    fdasc.spiritual      <- rowSums(data[,vars.5dascSpiritual])/(length(vars.5dascSpiritual))
    fdasc.bliss      <- rowSums(data[,vars.5dascBliss])/(length(vars.5dascBliss))
    fdasc.insight      <- rowSums(data[,vars.5dascInsight])/(length(vars.5dascInsight))
    fdasc.disembodiment      <- rowSums(data[,vars.5dascDisembodiment])/(length(vars.5dascDisembodiment))
    fdasc.impairedcognition      <- rowSums(data[,vars.5dascImpairedCognition])/(length(vars.5dascImpairedCognition))
    fdasc.anxiety     <- rowSums(data[,vars.5dascAnxiety])/(length(vars.5dascAnxiety))
    fdasc.compleximagery      <- rowSums(data[,vars.5dascComplexImagery])/(length(vars.5dascComplexImagery))
    fdasc.elementaryimagery      <- rowSums(data[,vars.5dascElementImagery])/(length(vars.5dascElementImagery))
    fdasc.synesthesiae     <- rowSums(data[,vars.5dascSynesthesiae])/(length(vars.5dascSynesthesiae))
    fdasc.changedpercept      <- rowSums(data[,vars.5dascChangedPercept])/(length(vars.5dascChangedPercept))
    fdasc.intensity <- data[,vars.5dascIntensity]

    fdasc.scores.comb <- cbind(fdasc.unity,fdasc.spiritual,fdasc.bliss,fdasc.insight,fdasc.disembodiment,fdasc.impairedcognition,
                               fdasc.anxiety,fdasc.compleximagery,fdasc.elementaryimagery,fdasc.synesthesiae,fdasc.changedpercept, fdasc.intensity)
    if('5DASC_43' %in% colnames(fdasc.scores.comb)){
    fdasc.scores.comb <- data.table::setnames(fdasc.scores.comb,old=c('5DASC_43'),new=c('fdasc.intensity'))
    }

  }

  # get variables we want to pass through to the output
  # bring participant sub,id,date back into the data frame
  keeps <- data[,keep.vars]

  # combine scores and kept variables, make dataframe
  output <- cbind(keeps,raw.dat,fdasc.scores.comb)
  output <- as.data.frame(output)
  return(output)

}


#****************************************************************************
# AAQII (Acceptance & Action QA)
aaq.scoring <- function(data,keep.vars,aaq) {
  # if no 'varnames' is defined, we assume that items are named
  keep.vars <- intersect(c('ID','date','time.point','sub'), colnames(data))
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
  raw.colnames <- colnames(data)
  aaq.include <- c() # will hold the actual items to include in total score (matters when reading in 10-item version)
  aaq.prefix <- c() # differentiates between 'AAQ-II' and 'AAQ'
  if("AAQ_pre_1" %in% raw.colnames) {
    aaq.prefix <- "AAQ_pre_"
  } else if ("AAQ_post_1" %in% raw.colnames){
    aaq.prefix <- "AAQ_post_"
  } else {
    stop("Column names for AAQ not recognized. Use 'AAQ_pre_' or 'AAQ_post_'")
  }
  
  if(length(grep(aaq.prefix, raw.colnames)) == 7) {
    if (missing(aaq)) {
      aaq <- c(paste(aaq.prefix,1:7,sep=""))
    }
    if (aaq.prefix == "AAQ-II_") {
      data <- data.table::setnames(data, old=c(paste(aaq.prefix, 1:7, sep="")), new=aaq)
    }
    aaq.include <- aaq
  } else if(length(grep(aaq.prefix, raw.colnames)) == 10) {
      if (missing(aaq)) {
        aaq <- c(paste(aaq.prefix,1:10,sep=""))
      }
      if (aaq.prefix == "AAQ-II_") {
        data <- data.table::setnames(data, old=c(paste(aaq.prefix, 1:10, sep="")), new=aaq)
      }
      aaq.include <- c(paste('AAQ_',c(2,3,4,5,7,8,9),sep=""))
  }
  
  raw.dat <- data[,aaq]
  aaq.total <- rowSums(data[,aaq.include])

  keeps <- data[,keep.vars]
  

  output <- cbind(keeps,raw.dat,aaq.total)
  output <- as.data.frame(output)

  return(output)
}


#****************************************************************************
# Absorption in Music Scale (AIMS)
aims.scoring <- function(data,keep.vars,aims){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }

  if (missing(aims)) {
    aims <- c(paste('AIMS_',1:34,sep=""))
  }
  raw.dat <- data[,aims]

  aims.total <- rowSums(data[,aims])

  keeps <- data[,keep.vars]

  output <- cbind(keeps,raw.dat,aims.total)
  output <- as.data.frame(output)
  return(output)
}


#*********************************************************************
# Adverse Childhood Experience Scale (ACE)
#*********************************************************************
ace.scoring <- function(data, keep.vars, ace) {
  keep.vars <- c('ID', 'date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <- c(keep.vars, 'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub', keep.vars)
  }
  
  if ( (length(grep('ACE_', colnames(data))) == 10) ) {
    if (missing(ace)) {
      ace <- c(paste('ACE_', 1:10, sep=""))
    }
    raw.dat <- data[,ace]
    
    ace.originalTotal <- rowSums(data[,ace])
    
    keeps <- data[,keep.vars]
    
    output <- cbind(keeps, raw.dat, ace.originalTotal)
    output <- as.data.frame(output)
    return(output)
  } else if(length(grep('ACE_', colnames(data)) == 14)) {
    if (missing(ace)) {
      ace <- c(paste('ACE_', 1:14, sep=""))
    }
    raw.dat <- data[,ace]
    
    vars.aceOriginalTotal <- paste('ACE_',1:10,sep="")
    vars.aceAdditionalAdversity <- paste('ACE_',11:14,sep="")
    vars.aceExpandedTotal <- paste('ACE_',1:14,sep="")
    
    ace.originalTotal <- rowSums(data[,vars.aceOriginalTotal])
    ace.additionalAdversity <- rowSums(data[,vars.aceAdditionalAdversity])
    ace.expandedTotal <- rowSums(data[,vars.aceExpandedTotal])
    
    keeps <- data[,keep.vars]
    
    output <- cbind(keeps, raw.dat, ace.originalTotal, ace.additionalAdversity, ace.expandedTotal)
    output <- as.data.frame(output)
    return(output)
  }
  
  
  
}

################################################################################
# Anorexia Stages of Change Questionnaire (ANSOCQ) #############################
ansocq.scoring <- function(data,keep.vars,ansocq) {
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
  if (missing(ansocq)) { # note that the way we name these columns excludes the TEXT columns for item 17
    ansocq <- c(rep(paste('ANSOCQ_',1:20,sep=""),each=5))
    ansocq.sub <- c(rep(1:5,times=20))
    ansocq <- c(paste(ansocq, ansocq.sub,sep="_"))
  }
  ansocq.withtext <- c(rep(paste('ANSOCQ_',1:17,sep=""),each=5), rep(paste('ANSOCQ_',17:20,sep=""),each=5))
  ansocq.subtext <- c(rep(1:5,times=16), paste(rep(1:5, each=2), rep(c("", "_TEXT"), times=5), sep=""), rep(1:5, times= 3))
  ansocq.withtext <- c(paste(ansocq.withtext, ansocq.subtext, sep = "_"))
  if('MinWeight' %in% colnames(data)) {
    ansocq.withtext <- c("MinWeight", ansocq.withtext)
  }
  raw.dat <- data[,ansocq.withtext]
  
  # want to separate all the sub items to apply function within for loop
  sub2.names <- names(select(data[,ansocq],ends_with("_2"))) #get column names of all that end in "_2"
  sub3.names <- names(select(data[,ansocq],ends_with("_3"))) #get column names of all that end in "_3"
  sub4.names <- names(select(data[,ansocq],ends_with("_4"))) #get column names of all that end in "_4"
  sub5.names <- names(select(data[,ansocq],ends_with("_5"))) #get column names of all that end in "_5"
  
  # Change the value of 1 to correct number to indicate stage based on value endorsement
  data[,ansocq] <- sapply(data[,ansocq],as.numeric)
  data[,sub2.names][data[,sub2.names]==1]<-2
  data[,sub3.names][data[,sub3.names]==1]<-3
  data[,sub4.names][data[,sub4.names]==1]<-4
  data[,sub5.names][data[,sub5.names]==1]<-5
  
  ansocq.item.avg <- c()
  # create df where each column corresponds to the average score for responses endorsed per item
  # ex. average of non-na responses to "ANSOCQ_1_1", "ANSOCQ_1_2", "ANSOCQ_1_3", "ANSOCQ_1_4", "ANSOCQ_1_5"
  for (i in 1:20) {
    curr.val <- rowMeans(select(data[,ansocq],starts_with(paste("ANSOCQ_",i,"_",sep=""))),na.rm=T)
    ansocq.item.avg <- cbind(ansocq.item.avg,round(curr.val,2))
  }
  colnames(ansocq.item.avg) <- paste("ansocq.",c(1:20),sep="")
  
  ansocq.total <- rowMeans(ansocq.item.avg,na.rm=T)
  
  keeps <- data[,keep.vars]
  output <- cbind(keeps, raw.dat, ansocq.total) %>%
    as.data.frame()
  return(output)
  
}

#****************************************************************************
# Appreciation Scale
appreciation.scoring <- function(data,keep.vars,appreciation){
  # if no 'varnames' is defined, we assume that items are named
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }

  if (missing(appreciation)) {
    appreciation <- c(paste('AppreciationScale_',1:18,sep=""))
  }
  raw.dat <- data[,appreciation]
  # define subscales
  vars.appreciationTotal <- appreciation
  vars.appreciationFocus <- c(paste('AppreciationScale_',1:5,sep=""))
  vars.appreciationAwe <- c(paste('AppreciationScale_',6:8,sep=""))
  vars.appreciationRitual <- c(paste('AppreciationScale_',9:11,sep=""))
  vars.appreciationPresmoment <- c(paste('AppreciationScale_',c(12,16,17,18),sep=""))
  vars.appreciationLoss <- c(paste('AppreciationScale_',13:14,sep=""))
  vars.appreciationInterpersonal <- c('AppreciationScale_15')

  # calculate scores
  appreciationScale.total <- rowSums(data[,vars.appreciationTotal])
  appreciationScale.focus <- rowSums(data[,vars.appreciationFocus])
  appreciationScale.awe <- rowSums(data[,vars.appreciationAwe])
  appreciationScale.ritual <- rowSums(data[,vars.appreciationRitual])
  appreciationScale.presentMoment <- rowSums(data[,vars.appreciationPresmoment])
  appreciationScale.loss <- rowSums(data[,vars.appreciationLoss])
  appreciationScale.interpersonal <- data[,vars.appreciationInterpersonal]

  # participant info
  keeps <- data[,keep.vars]
  

  # combine scores and kept variables, make dataframe
  output <- cbind(keeps,raw.dat,appreciationScale.total, appreciationScale.focus, 
                  appreciationScale.awe, appreciationScale.ritual, appreciationScale.presentMoment,
                  appreciationScale.loss, appreciationScale.interpersonal)
  output <- as.data.frame(output)
  return(output)

}

#************************************************************************
# Altered States of Consciousness (APZ) Questionnaire
#************************************************************************
apz.scoring <- function(data, keep.vars, apz) {
  keep.vars <- c('ID', 'date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <- c(keep.vars, 'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub', keep.vars)
  }
  if (missing(apz)) {
    apz <- c(paste('APZ_', 1:72, sep = ""))
  }
  
  raw.dat <- data[,apz]
  
  vars.OSE <- c(paste('APZ_', c(1,5,8,10,17,20,36,41,44,45,54,56,67), sep = ""))
  vars.AIA <- c(paste('APZ_', c(6,18,22,26,28,29,33,35,38,40,43,47,48,49,57,59,61,65,68,70,71,72), sep = ""))
  vars.VUS <- c(paste('APZ_', c(9,16,19,24,25,27,37,39,46,51,52,55,60,63), sep = ""))
  
  apz.ose <- rowSums(data[,vars.OSE],na.rm = TRUE)
  apz.aia <- rowSums(data[,vars.AIA],na.rm = TRUE)
  apz.vus <- rowSums(data[,vars.VUS],na.rm = TRUE)
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps, raw.dat, apz.ose, apz.aia, apz.vus)
  output <- as.data.frame(output)
  return(output)
}

#************************************************************************
# Addiction Research Center Inventory (ARCI) Short Form
#************************************************************************
arci.scoring <- function(data, keep.vars, arci) {
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <- c(keep.vars, 'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub', keep.vars)
  }
  if (missing(arci)) {
    arci <- c(paste('ARCIShort_', 1:49, sep = ""))
  }
  
  raw.dat <- data[,arci]
  
  vars.pcag <- c(paste("ARCIShort_", c(1,2,3,4,5,6,7,8,9,10,11,12,23,29,48), sep = ""))
  vars.mbg <- c(paste("ARCIShort_", c(13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28), sep = ""))
  vars.lsd <- c(paste("ARCIShort_", c(11,19,33,34,35,41,42,43,44,45,46,47,48,49), sep = ""))
  vars.bg <- c(paste("ARCIShort_", c(10,23,26,28,30,31,32,34,36,37,38,39,40), sep = ""))
  vars.a <- c(paste("ARCIShort_", c(24,25,26,27,29,30,31,32,33,34,35), sep = ""))
  
  rev.pcag <- c(paste("ARCIShort_", c(12,23,29,48), sep = ""))
  rev.lsd <- c(paste("ARCIShort_", c(11,19,33,49), sep = "")) # replaced 22 w 19, almost same wording, both 0502 and 0014 use 19 instead
  rev.bg <- c(paste("ARCIShort_", c(10,38,39,40), sep = ""))
  
  pcag.copy <- data[,vars.pcag]
  pcag.copy[,rev.pcag] <- 1-data[,rev.pcag]
  
  lsd.copy <- data[, vars.lsd]
  lsd.copy[,rev.lsd] <- 1-data[,rev.lsd]
  
  bg.copy <- data[,vars.bg]
  bg.copy[,rev.bg] <- 1-data[,rev.bg]
  
  arcishort.pcag <- rowSums(pcag.copy)
  arcishort.mbg <- rowSums(data[,vars.mbg])
  arcishort.lsd <- rowSums(lsd.copy)
  arcishort.bg <- rowSums(bg.copy)
  arcishort.a <- rowSums(data[,vars.a])
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps,raw.dat,arcishort.pcag,arcishort.mbg,arcishort.lsd,arcishort.bg,arcishort.a)
  output <- as.data.frame(output)
  return(output)
}

#************************************************************************
# OLD VERSION in use for studies 0014 and 0502 as this version matches the published scores (studies 0014 and 0502 scored MBG as items 12-25,27-28 which is used here, we now score items 13-28 for this subscale as seen in arci.scoring)
# Addiction Research Center Inventory (ARCI) Short Form
#************************************************************************
oldarci.scoring <- function(data, keep.vars, arci) {
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <- c(keep.vars, 'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub', keep.vars)
  }
  if (missing(arci)) {
    arci <- c(paste('ARCIShort_', 1:49, sep = ""))
  }
  
  raw.dat <- data[,arci]
  
  vars.pcag <- c(paste("ARCIShort_", c(1,2,3,4,5,6,7,8,9,10,11,12,23,29,48), sep = ""))
  vars.mbg <- c(paste("ARCIShort_", c(12,13,14,15,16,17,18,19,20,21,22,23,24,25,27,28), sep = ""))
  vars.lsd <- c(paste("ARCIShort_", c(11,19,33,34,35,41,42,43,44,45,46,47,48,49), sep = ""))
  vars.bg <- c(paste("ARCIShort_", c(10,23,26,28,30,31,32,34,36,37,38,39,40), sep = ""))
  vars.a <- c(paste("ARCIShort_", c(24,25,26,27,29,30,31,32,33,34,35), sep = ""))
  
  rev.pcag <- c(paste("ARCIShort_", c(12,23,29,48), sep = ""))
  rev.lsd <- c(paste("ARCIShort_", c(11,19,33,49), sep = "")) # replaced 22 w 19, almost same wording, both 0502 and 0014 use 19 instead
  rev.bg <- c(paste("ARCIShort_", c(10,38,39,40), sep = ""))
  
  pcag.copy <- data[,vars.pcag]
  pcag.copy[,rev.pcag] <- 1-data[,rev.pcag]
  
  lsd.copy <- data[, vars.lsd]
  lsd.copy[,rev.lsd] <- 1-data[,rev.lsd]
  
  bg.copy <- data[,vars.bg]
  bg.copy[,rev.bg] <- 1-data[,rev.bg]
  
  arcishort.pcag <- rowSums(pcag.copy)
  arcishort.mbg <- rowSums(data[,vars.mbg])
  arcishort.lsd <- rowSums(lsd.copy)
  arcishort.bg <- rowSums(bg.copy)
  arcishort.a <- rowSums(data[,vars.a])
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps,raw.dat,arcishort.pcag,arcishort.mbg,arcishort.lsd,arcishort.bg,arcishort.a)
  output <- as.data.frame(output)
  return(output)
}

####################################################################
# Ayahuasca Experience Inventory
####################################################################
aei.scoring <- function(data, keep.vars, aei) {
        keep.vars <- c('ID','date')
        if('time.point' %in% colnames(data))
        {
                keep.vars <-c(keep.vars,'time.point')
        }
        if('sub' %in% colnames(data))
        {
                keep.vars <- c('sub',keep.vars)
        }
        
        if (missing(aei)) {
                aei <- c(paste('AEI_',1:23,sep=""))
        }
        raw.dat <- data[,aei]
        
        # define subscales
        vars.aeiClarity <- c(paste('AEI_', 1:28 , sep=""))
        vars.aeiReappraisal <- c(paste('AEI_', 29:50 , sep=""))
        vars.aeiDiscomfort <- c(paste('AEI_', 51:65 , sep=""))
        
        #calculate average score
        aei.clarity <- rowSums(data[,vars.aeiClarity])/ length(vars.aeiClarity)
        aei.reappraisal <- rowSums(data[,vars.aeiReappraisal])/length(vars.aeiReappraisal)
        aei.discomfort <- rowSums(data[,vars.aeiDiscomfort])/length(vars.aeiDiscomfort)
        
        keeps <- data[,keep.vars]
        
        output <- cbind(keeps, raw.dat, aei.clarity, aei.reappraisal, aei.discomfort) %>%
                as.data.frame()
        output
}


####################################################################
# ASPIRES
####################################################################
aspires.scoring <- function(data, keep.vars, aspires) {
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
  if (missing(aspires)) {
    aspires <- c(paste('ASPIRES_',1:23,sep=""))
  }
  raw.dat <- data[,aspires]
  
  # reverse score
  vars_rev <- c(paste('ASPIRES_', c(1, 2, 11, 18, 3, 13, 15, 7, 14, 23), sep=""))
  data[,vars_rev] <- 6- data[,vars_rev]

  # define subscales
  vars.aspiresConnectedness <- c(paste('ASPIRES_', c(7, 9, 10, 14, 22, 23), sep=""))
  vars.aspiresUniversality <- c(paste('ASPIRES_', c(3, 5, 6, 13, 15, 16, 20), sep=""))
  vars.aspiresPrayerFulfillment <- c(paste('ASPIRES_', c(1, 2, 4, 8, 11, 12, 17, 18, 19, 21), sep=""))

  #calculate average score
  aspires.connectedness <- rowSums(data[,vars.aspiresConnectedness])
  aspires.universality <- rowSums(data[,vars.aspiresUniversality])
  aspires.prayerFulfillment <- rowSums(data[,vars.aspiresPrayerFulfillment])

  # aspires.connectedness.average <- rowSums(data[,vars.aspiresConnectedness])/length(vars.aspiresConnectedness)
  # aspires.universality.average <- rowSums(data[,vars.aspiresUniversality])/length(vars.aspiresUniversality)
  # aspires.prayerFulfillment.average <- rowSums(data[,vars.aspiresPrayerFulfillment])/length(prayer_fulfillment)

  aspires.total <- rowSums(data[,aspires])
  #aspires.average <- rowSums(data[,aspires])/length(aspires)

  keeps <- data[,keep.vars]

  output <- cbind(keeps, raw.dat,aspires.connectedness, aspires.universality, aspires.prayerFulfillment,
                   aspires.total) %>%
    as.data.frame()
  output
}


#*********************************************************************
# Alcohol Use Disorders Identification Test (AUDIT)
#*********************************************************************
audit.scoring <- function(data, keep.vars, audit) {
  keep.vars <- c('ID', 'date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <- c(keep.vars, 'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub', keep.vars)
  }
  
  if (missing(audit)) {
    audit <- c(paste('AUDIT_', 1:10, sep=""))
  }
  raw.dat <- data[,audit]
  
  # calculate score
  #audit.total <- rowSums(data[,audit], na.rm=T)
  
  audit.total <- ifelse(data$AUDIT_1 == 0,
                         0,
                         rowSums(data[, audit], na.rm = TRUE))
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps, raw.dat, audit.total)
  output <- as.data.frame(output)
  return(output)
}

#*********************************************************************
# Awe Experience Scale
#*********************************************************************
awe.scoring <- function(data, keep.vars, awe){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
  if (missing(awe)){
    awe <- paste('AWES_',1:30,sep="")
  }
  raw.dat <- data[,awe]
  
  vars.aweTimeDilation <- paste('AWES_',1:5,sep="")
  vars.aweSelfDiminish <- paste('AWES_',6:10,sep="")
  vars.aweConnectedness <- paste('AWES_',11:15,sep="")
  vars.aweVastness <- paste('AWES_',16:20,sep="")
  vars.awePhysSensation <- paste('AWES_',21:25,sep="")
  vars.aweNeedAccommodation <- paste('AWES_',26:30,sep="")
  
  awes.timeDilation <- rowSums(data[,vars.aweTimeDilation])/length(vars.aweTimeDilation)
  awes.selfDiminish <- rowSums(data[,vars.aweSelfDiminish])/length(vars.aweSelfDiminish)
  awes.connectedness <- rowSums(data[,vars.aweConnectedness])/length(vars.aweConnectedness)
  awes.vastness <- rowSums(data[,vars.aweVastness])/length(vars.aweVastness)
  awes.physSensation <- rowSums(data[,vars.awePhysSensation])/length(vars.awePhysSensation)
  awes.needAccommodation <- rowSums(data[,vars.aweNeedAccommodation])/length(vars.aweNeedAccommodation)
  awes.total <- rowSums(data[,awe])/length(awe)
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps, raw.dat, awes.timeDilation, awes.selfDiminish, awes.connectedness,
                  awes.vastness, awes.physSensation, awes.needAccommodation, awes.total)
  output <- as.data.frame(output)
  return(output)
  
}

#*********************************************************************
# Awe Experience Short-Form (12-item)
#*********************************************************************
aweshort.scoring <- function(data, keep.vars, aweshort){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
  if (missing(aweshort)){
    aweshort <- paste('AWEShort_',1:12,sep="")
  }
  raw.dat <- data[,aweshort]
  
  vars.aweshortTimeDilation <- paste('AWEShort_',1:2,sep="")
  vars.aweshortSelfDiminish <- paste('AWEShort_',3:4,sep="")
  vars.aweshortConnectedness <- paste('AWEShort_',5:6,sep="")
  vars.aweshortVastness <- paste('AWEShort_',7:8,sep="")
  vars.aweshortPhysSensation <- paste('AWEShort_',9:10,sep="")
  vars.aweshortNeedAccommodation <- paste('AWEShort_',11:12,sep="")
  
  aweshort.timeDilation <- rowSums(data[,vars.aweshortTimeDilation])/length(vars.aweshortTimeDilation)
  aweshort.selfDiminish <- rowSums(data[,vars.aweshortSelfDiminish])/length(vars.aweshortSelfDiminish)
  aweshort.connectedness <- rowSums(data[,vars.aweshortConnectedness])/length(vars.aweshortConnectedness)
  aweshort.vastness <- rowSums(data[,vars.aweshortVastness])/length(vars.aweshortVastness)
  aweshort.physSensation <- rowSums(data[,vars.aweshortPhysSensation])/length(vars.aweshortPhysSensation)
  aweshort.needAccommodation <- rowSums(data[,vars.aweshortNeedAccommodation])/length(vars.aweshortNeedAccommodation)
  aweshort.total <- rowSums(data[,aweshort])/length(aweshort)
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps, raw.dat, aweshort.timeDilation, aweshort.selfDiminish, aweshort.connectedness,
                  aweshort.vastness, aweshort.physSensation, aweshort.needAccommodation, aweshort.total)
  output <- as.data.frame(output)
  return(output)
}


################################################################################
# Beliefs About Consciousness Questionnaire (BACQ)
################################################################################
bacq.scoring <- function(data, keep.vars, bacq) {
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  keeps <- data[,keep.vars]
  output <- data.frame() #initializing output variable
  if(missing(bacq)) {
    bacq <- paste0('BACQ_', 1:9)
  }
  
  raw.dat <- data[,bacq]
  
  vars.mammalConsciousness <- paste0('BACQ_', 1:4)
  vars.nonMammalConsciousness <- paste0('BACQ_', 5:9)
  
  bacq.mammalConsciousness <- rowSums(data[,vars.mammalConsciousness])/length(vars.mammalConsciousness)
  bacq.nonMammalConsciousness <- rowSums(data[,vars.nonMammalConsciousness])/length(vars.nonMammalConsciousness)
  
  output <- cbind(keeps, raw.dat, bacq.mammalConsciousness, bacq.nonMammalConsciousness)
  
  
  output
}

#*#************************************************************************
# Behavioral Activation and Behavioral Inhibition Scales (BAI)
#************************************************************************
bai.scoring <- function(data, keep.vars, bai){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
  if (missing(bai)){
    bai <- c(paste('BAI_', 1:20, sep = ""))
  }
  raw.dat <- data[,bai]
  
  rev.bai <- c(paste('BAI_', c(1, 18), sep = ""))
  data[,rev.bai] = 5-data[,rev.bai]
  
  vars.punishmentSens <- c(paste('BAI_', c(1, 6, 10, 13, 15, 18, 20), sep = ""))
  vars.rewardResponse <- c(paste('BAI_', c(3, 5, 11, 14, 19), sep = ""))
  vars.drive <- c(paste('BAI_', c(2, 7, 9, 17), sep = ""))
  vars.funSeek <- c(paste('BAI_', c(4, 8, 12, 16), sep = ""))
  
  bai.punishmentSens <- rowSums(data[,vars.punishmentSens])
  bai.rewardResponse <- rowSums(data[,vars.rewardResponse])
  bai.drive <- rowSums(data[,vars.drive])
  bai.funSeek <- rowSums(data[,vars.funSeek])
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps, raw.dat, bai.punishmentSens, bai.rewardResponse, bai.drive, bai.funSeek)
  
  output <- as.data.frame(output)
  return(output)
}

#*#************************************************************************
# Brief Aggression Questionnaire (BAQ)
#************************************************************************
baq.scoring <- function(data, keep.vars, baq){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
  if (missing(baq)){
    baq <- c(paste('BAQ_', 1:12, sep = ""))
  }
  raw.dat <- data[,baq]
  
  rev.baq <- c(paste('BAQ_', c(4), sep = ""))
  data[,rev.baq] = 8-data[,rev.baq]
  
  vars.Physicalaggression <- c(paste('BAQ_', c(1:3), sep = ""))
  vars.Anger <- c(paste('BAQ_', c(4:6), sep = ""))
  vars.Verbalaggression <- c(paste('BAQ_', c(7:9), sep = ""))
  vars.Hostility <- c(paste('BAQ_', c(10:12), sep = ""))
  
  baq.physicalaggression <- rowSums(data[,vars.Physicalaggression])/length(vars.Physicalaggression)
  baq.anger <- rowSums(data[,vars.Anger])/length(vars.Anger)
  baq.verbalaggression <- rowSums(data[,vars.Verbalaggression])/length(vars.Verbalaggression)
  baq.hostility <- rowSums(data[,vars.Hostility])/length(vars.Hostility)
  
  baq.total <- rowSums(data[,baq])/length(baq)
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps, raw.dat, baq.physicalaggression, baq.anger, baq.verbalaggression, baq.hostility, baq.total)
  
  output <- as.data.frame(output)
  return(output)
}

#****************************************************************************
# BDI (Beck Depression Inventory)
bdi.scoring <- function(data,keep.vars,bdi) {
  # if no 'varnames' is defined, we assume that items are named
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
  raw.colnames <- colnames(data)
  bdi.prefix <- c()
  if("BDI-II_1" %in% raw.colnames) {
    bdi.prefix <- "BDI-II_"
  } else if("BDI_1" %in% raw.colnames) {
    bdi.prefix <- "BDI_"
  } else {
    stop("Column names for BDI not recognized. Use 'BDI_' or 'BDI-II_'")
  }
  
  if ( (length(grep(bdi.prefix, raw.colnames)) == 20) ) {
    if (missing(bdi)){
      bdi <- c(paste('BDI_',1:8,sep=""), paste('BDI_',10:21,sep=""))
    }
    
    if (bdi.prefix == "BDI-II_") {
      data <- data.table::setnames(data, old=c(paste('BDI-II_',1:8,sep=""), paste('BDI-II_',10:21,sep="")), new=bdi)
    }
  } else if ( (length(grep(bdi.prefix, raw.colnames)) >= 21) ){
    if (missing(bdi)) {
      bdi <- c(paste('BDI_',1:21,sep=""))
    }
    
    if (bdi.prefix == "BDI-II_") {
      data <- data.table::setnames(data, old=c(paste('BDI-II_',1:21,sep="")), new=bdi)
    }
    
  }

  raw.dat <- data[,bdi]

  bdi.total <- rowSums(data[,bdi])
  keeps <- data[,keep.vars]

  output <- cbind(keeps,raw.dat,bdi.total)
  output <- as.data.frame(output)
  output
}

################################################################################
# Beck Hopelesness Scale
################################################################################
bhs.scoring <- function(data, keep.vars, bhs) {
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
  if (missing(bhs)) {
    bhs <- c(paste('BeckHopelessness_',1:20,sep=""))
  }
  raw.dat <- data[,bhs]
  
  beckHopelessness.total <- rowSums(data[,bhs])
  
  keeps <- data[,keep.vars]
  output <- cbind(keeps,raw.dat, beckHopelessness.total) %>%
    as.data.frame()
  output
  
}

#****************************************************************************
### Big Five Inventory (BFI) ###
bfi.scoring <- function(data,keeps.vars,bfi){
  # if no 'varnames' is defined, we assume that items are named
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
  if (missing(bfi)) {
    bfi <- c(paste('BFI_',1:44,sep=""))
  }
  raw.dat <- data[,bfi]
  # reverse scoring
  bfi_items <- c(paste('BFI_',1:44,sep=""))
  bfirev <- c(paste('BFI_',c(2,6,8,9,12,18,21,23,24,27,31,34,35,37,41,43),sep=""))
  
  data[,bfirev] = 6-data[,bfirev]
  
  # define subscales
  vars.bfiExtrav   <- c(paste('BFI_',c(1,6,11,16,21,26,31,36),sep=""))
  vars.bfiAgree    <- c(paste('BFI_',c(2,7,12,17,22,27,32,37,42),sep=""))
  vars.bfiConsc    <- c(paste('BFI_',c(3,8,13,18,23,28,33,38,43),sep=""))
  vars.bfiNeurotic <- c(paste('BFI_',c(4,9,14,19,24,29,34,39),sep=""))
  vars.bfiOpen     <- c(paste('BFI_',c(5,10,15,20,25,30,35,40,41,44),sep=""))
  #vars.bfiTotal    <- c(vars.bfiExtrav, vars.bfiAgree, vars.bfiConsc, vars.bfiNeurotic, vars.bfiOpen)
  
  # calculate scores
  bfi.extrav.mean <- rowSums(data[,vars.bfiExtrav])/(length(vars.bfiExtrav))
  bfi.agree.mean <- rowSums(data[,vars.bfiAgree])/(length(vars.bfiAgree))
  bfi.consc.mean <- rowSums(data[,vars.bfiConsc])/(length(vars.bfiConsc))
  bfi.neurotic.mean <- rowSums(data[,vars.bfiNeurotic])/(length(vars.bfiNeurotic))
  bfi.open.mean <- rowSums(data[,vars.bfiOpen])/(length(vars.bfiOpen))
  #bfi.total <- rowSums(data[,vars.bfiTotal])/(length(vars.bfiTotal))
  
  bfi.extrav.sum <- rowSums(data[,vars.bfiExtrav])
  bfi.agree.sum <- rowSums(data[,vars.bfiAgree])
  bfi.consc.sum <- rowSums(data[,vars.bfiConsc])
  bfi.neurotic.sum <- rowSums(data[,vars.bfiNeurotic])
  bfi.open.sum <- rowSums(data[,vars.bfiOpen])
  
  # get variables we want to pass through to the output
  keeps <- data[,keep.vars]
  
  
  # combine scores and kept variables, make dataframe
  output <- cbind(keeps,raw.dat,bfi.extrav.mean,bfi.agree.mean,bfi.consc.mean,
                  bfi.neurotic.mean,bfi.open.mean, bfi.extrav.sum,bfi.agree.sum,
                  bfi.consc.sum,bfi.neurotic.sum,bfi.open.sum)
  output <- as.data.frame(output)
  
  # return
  return(output)
  
}

#************************************************************************
# Behavior Identification Form (BIF)
#************************************************************************
bif.scoring <- function(data, keep.vars, bif) {
  keep.vars <- c('ID', 'date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <- c(keep.vars, 'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub', keep.vars)
  }
  if (missing(bif)) {
    bif <- c(paste('BIF_', 1:25, sep = ""))
  }
  
  raw.dat <- data[,bif]
  
  # High-level alternatives are coded as 1 and low-level as 0. Sum of all the items
  #produces count of high-level items chosen
  bif.total <- rowSums(data[,bif])
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps, raw.dat, bif.total)
  output <- as.data.frame(output)
  return(output)
}

#****************************************************************************
### Brief Affective Neuroscience Personality Scales (BANPS) ###
banps.scoring <- function(data,keep.vars,banps){
  # if no 'varnames' is defined, we assume that items are named
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }

  if (missing(banps)) {
    banps <- c(paste('BANPS_',1:33,sep=""))
  }
  
  if (min(data[,banps], na.rm = T) < 1){ # if score range is 0 to 4, then convert scale to 1-5, na.rm = T because min will set NA as lowest value if present
    data[,banps] <- 1 + data[,banps]
  }
  raw.dat <- data[,banps]

  # reverse scoring
  banpsall <- banps

  banpsrev <- c(paste('BANPS_',c(22,29,5,20,24,3,21,25,16,27,6,17,30,7,8,12),sep=""))
  data[,banpsrev] = 6-data[,banpsrev]

  # define subscales
  vars.banpsPlay <- c(paste('BANPS_',c(1, 9, 13, 19, 22, 29), sep=""))
  vars.banpsAnger <- c(paste('BANPS_',c(2, 5, 11, 20, 24, 26), sep=""))
  vars.banpsSeek <- c(paste('BANPS_', c(3, 21, 25, 28, 31, 33), sep=""))
  vars.banpsCare <- c(paste('BANPS_',c(4, 14, 16, 27), sep=""))
  vars.banpsFear <- c(paste('BANPS_',c(6, 15, 17, 23, 30), sep=""))
  vars.banpsSad <- c(paste('BANPS_', c(7, 8, 10, 12, 18, 32), sep=""))

  # calculate scores
  banps.play <- rowSums(data[,vars.banpsPlay])/(length(vars.banpsPlay))
  banps.anger <- rowSums(data[,vars.banpsAnger])/(length(vars.banpsAnger))
  banps.seek <- rowSums(data[,vars.banpsSeek])/(length(vars.banpsSeek))
  banps.care <- rowSums(data[,vars.banpsCare])/(length(vars.banpsCare))
  banps.fear <- rowSums(data[,vars.banpsFear])/(length(vars.banpsFear))
  banps.sad <- rowSums(data[,vars.banpsSad])/(length(vars.banpsSad))

  # get variables we want to pass through to the output
  keeps <- data[,keep.vars]

  # combine scores and kept variables, make dataframe
  output <- cbind(keeps,raw.dat,banps.play,banps.anger,banps.seek,banps.care,banps.fear,banps.sad)
  output <- as.data.frame(output)

  # return
  return(output)

}


#****************************************************************************
# Barratt Impulsiveness Scale (BIS-11)
#****************************************************************************
bis11.scoring <- function(data, keep.vars, bis11) {
  keep.vars <- c('ID', 'date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <- c(keep.vars, 'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub', keep.vars)
  }
  if (missing(bis11)) {
    bis11 <- c(paste('BIS11_', 1:30, sep = ""))
  }
  
  raw.dat <- data[,bis11]
  
  rev.items <- c(paste('BIS11_', c(1,7,8,9,10,12,13,15,20,29,30), sep = ""))
  data[,rev.items] <- 5-data[,rev.items]
  
  vars.attSecond <- c(paste('BIS11_', c(5,6,9,11,20,24,26,28), sep = ""))
  vars.attFirst <- c(paste('BIS11_', c(5,9,11,20,28), sep = ""))
  vars.ciFirst <- c(paste('BIS11_', c(6,24,26), sep = ""))
  
  vars.motSecond <- c(paste('BIS11_', c(2,3,4,16,17,19,21,22,23,25,30), sep = ""))
  vars.motFirst <- c(paste('BIS11_', c(2,3,4,17,19,22,25), sep = ""))
  vars.perFirst <- c(paste('BIS11_', c(16,21,23,30), sep = ""))
  
  vars.npSecond <- c(paste('BIS11_', c(1,7,8,10,12,13,14,15,18,27,29), sep = ""))
  vars.scFirst <- c(paste('BIS11_', c(1,7,8,12,13,14), sep = ""))
  vars.ccFirst <- c(paste('BIS11_', c(10,15,18,27,29), sep = ""))
  
  bis11.attSecond <- rowSums(data[,vars.attSecond])
  bis11.attFirst <- rowSums(data[,vars.attFirst])
  bis11.ciFirst <- rowSums(data[,vars.ciFirst])
  bis11.motSecond <- rowSums(data[,vars.motSecond])
  bis11.motFirst <- rowSums(data[,vars.motFirst])
  bis11.perFirst <- rowSums(data[,vars.perFirst])
  bis11.npSecond <- rowSums(data[,vars.npSecond])
  bis11.scFirst <- rowSums(data[,vars.scFirst])
  bis11.ccFirst <- rowSums(data[,vars.ccFirst])
  bis11.total <- rowSums(data[,bis11])
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps, raw.dat, bis11.attSecond, bis11.attFirst, bis11.ciFirst,
                  bis11.motSecond, bis11.motFirst, bis11.perFirst,
                  bis11.npSecond, bis11.scFirst, bis11.ccFirst,bis11.total)
  output <- as.data.frame(output)
  return(output)
}


#************************************************************************
# Brief Multidimensional Measurement of Religiousness/Spirituality (BMMRS), Forgiveness
#************************************************************************
bmmrs.forgive.scoring <- function(data, keep.vars, bmmrs.forgive) {
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <- c(keep.vars, 'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub', keep.vars)
  }
  if (missing(bmmrs.forgive)) {
    bmmrs.forgive <- c(paste('BMMRS_Forgive_', 1:3, sep = ""))
  }
  
  raw.dat <- data[,bmmrs.forgive]
  
  bmmrs.forgive <- rowSums(data[,bmmrs.forgive])
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps,raw.dat,bmmrs.forgive)
  output <- as.data.frame(output)
  return(output)
}

#****************************************************************************
# BPI (Brief Pain Inventory)
bpi.scoring <- function(data, keep.vars, bpi){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
  if (missing(bpi)){ 
    bpi_areas <- grep("^BPI_2", names(data), value = TRUE)
    bpi <- c('BPI_1', bpi_areas, paste0('BPI_',3:8), paste0('BPI_9',letters[1:7]))
  }
  
  raw.dat <- data[,bpi]
  
  vars.bpiSeverity <- paste('BPI_',3:6,sep="")
  vars.bpiInterference <- paste('BPI_9',letters[1:7],sep="")
  
  bpi.severity <- rowSums(data[,vars.bpiSeverity])/length(vars.bpiSeverity)
  bpi.interference <- rowSums(data[,vars.bpiInterference])/length(vars.bpiInterference)
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps, raw.dat, bpi.severity, bpi.interference)
  output <- as.data.frame(output)
  return(output)
}

#************************************************************************
# Religious Coping (RCOPE)
#************************************************************************
rcope.scoring <- function(data, keep.vars, rcope) {
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <- c(keep.vars, 'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub', keep.vars)
  }
  if (missing(rcope)) {
    rcope <- c(paste('RCOPE_', 1:105, sep = ""))
  }
  
  raw.dat <- data[,rcope]
  
  # SUBSCALE DEFINE
  # religious methods of coping to find meaning
  vars.benevReligReapp <- paste0('RCOPE_', c(11, 50, 53, 49, 38))
  vars.punishGodReapp <- paste0('RCOPE_', c(54, 12, 20, 30, 4))
  vars.demonReapp <- paste0('RCOPE_', c(19, 27, 39, 48, 55))
  vars.reappGodPower <- paste0('RCOPE_', c(13, 51, 42, 31, 5))
  
  # religious methods of coping to gain control
  vars.collabRelCoping <- paste0('RCOPE_', c(18, 10, 21, 6, 3))
  vars.activeRelSurr <- paste0('RCOPE_', c(17, 26, 52, 36, 22))
  vars.passRelDef <- paste0('RCOPE_', c(37, 7, 40, 8, 25))
  vars.pleadDirectInter <- paste0('RCOPE_', c(32, 9, 43, 29, 1))
  vars.selfDirRelCoping <- paste0('RCOPE_', c(2, 44, 33, 35, 16)) # 16 slightly diff wording than found online "Tried to deal with my feelings without God's help" in data vs "Tried to deal with the situation on my own without God's help"
  
  # religious methods of coping to gain comfort and closeness to god
  vars.seekSpiritSupp <- paste0('RCOPE_', c(47, 24, 45, 34, 23))
  vars.relFocus <- paste0('RCOPE_', c(14, 41, 46, 28, 15))
  vars.relPurif <- paste0('RCOPE_', c(60, 73, 67, 80, 96))
  vars.spiritConnect <- paste0('RCOPE_', c(74, 81, 72, 95, 88))
  vars.spiritDiscont <- paste0('RCOPE_', c(57, 97, 79, 94, 63))
  vars.markRelBound <- paste0('RCOPE_', c(87, 68, 93, 58, 66))
  
  # religious methods of coping to gain intimacy with others and closeness to God
  vars.seekSuppClergy <- paste0('RCOPE_', c(99, 70, 92, 105, 69))
  vars.relHelp <- paste0('RCOPE_', c(89, 62, 77, 102, 83))
  vars.interpersRelDisc <- paste0('RCOPE_', c(101, 103, 78, 61, 76))
  
  # religious methods of coping to achieve a life transformation
  vars.seekRelDir <- paste0('RCOPE_', c(84, 90, 65, 104, 100))
  vars.relConv <- paste0('RCOPE_', c(91, 85, 64, 75, 59))
  vars.relForgiv <- paste0('RCOPE_', c(71, 98, 86, 56, 82))
  
  # AVERAGES
  # religious methods of coping to find meaning
  rcope.benevolentReligousReappraisal <- rowSums(data[,vars.benevReligReapp])/length(vars.benevReligReapp)
  rcope.punishingGodReappraisal <- rowSums(data[,vars.punishGodReapp])/length(vars.punishGodReapp)
  rcope.demonicReappraisal <- rowSums(data[,vars.demonReapp])/length(vars.demonReapp)
  rcope.reappraisalGodPower <- rowSums(data[,vars.reappGodPower])/length(vars.reappGodPower)
  
  # religious methods of coping to gain control
  rcope.collaborativeReligiousCoping <- rowSums(data[,vars.collabRelCoping])/length(vars.collabRelCoping)
  rcope.activeReligiousSurrender <- rowSums(data[,vars.activeRelSurr])/length(vars.activeRelSurr)
  rcope.passiveReligiousDeferral <- rowSums(data[,vars.passRelDef])/length(vars.passRelDef)
  rcope.pleadingDirectIntercession <- rowSums(data[,vars.pleadDirectInter])/length(vars.pleadDirectInter)
  rcope.selfDirectingReligiousCoping <- rowSums(data[,vars.selfDirRelCoping])/length(vars.selfDirRelCoping)
  
  # religious methods of coping to gain comfort and closeness to God
  rcope.seekingSpiritualSupport <- rowSums(data[,vars.seekSpiritSupp])/length(vars.seekSpiritSupp)
  rcope.religiousFocus <- rowSums(data[,vars.relFocus])/length(vars.relFocus)
  rcope.religiousPurification <- rowSums(data[,vars.relPurif])/length(vars.relPurif)
  rcope.spiritualConnection <- rowSums(data[,vars.spiritConnect])/length(vars.spiritConnect)
  rcope.spiritDiscontent <- rowSums(data[,vars.spiritDiscont])/length(vars.spiritDiscont)
  rcope.markingRelBoundaries <- rowSums(data[,vars.markRelBound])/length(vars.markRelBound)
  
  # religious methods of coping to gain intimacy with others and closeness to God
  rcope.SeekingSupportClergyOrMembers <- rowSums(data[,vars.seekSuppClergy])/length(vars.seekSuppClergy)
  rcope.religiousHelping <- rowSums(data[,vars.relHelp])/length(vars.relHelp)
  rcope.interpersonalReligiousDiscontent <- rowSums(data[,vars.interpersRelDisc])/length(vars.interpersRelDisc)
  
  # religious methods of coping to achieve a life transformation
  rcope.seekingReligiousDirection <- rowSums(data[,vars.seekRelDir])/length(vars.seekRelDir)
  rcope.religiousConversion <- rowSums(data[,vars.relConv])/length(vars.relConv)
  rcope.religiousForgiving <- rowSums(data[,vars.relForgiv])/length(vars.relForgiv)
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps, raw.dat, rcope.benevolentReligousReappraisal, rcope.punishingGodReappraisal,
                  rcope.demonicReappraisal, rcope.reappraisalGodPower, rcope.collaborativeReligiousCoping,
                  rcope.activeReligiousSurrender, rcope.passiveReligiousDeferral, rcope.pleadingDirectIntercession,
                  rcope.selfDirectingReligiousCoping, rcope.seekingSpiritualSupport, rcope.religiousFocus,
                  rcope.religiousPurification, rcope.spiritualConnection, rcope.spiritDiscontent,
                  rcope.markingRelBoundaries, rcope.SeekingSupportClergyOrMembers, rcope.religiousHelping,
                  rcope.interpersonalReligiousDiscontent, rcope.seekingReligiousDirection, rcope.religiousConversion,
                  rcope.religiousForgiving)
  output <- as.data.frame(output)
  return(output)
}


#************************************************************************
# Brief COPE
#************************************************************************
briefcope.scoring <- function(data, keep.vars, briefcope) {
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <- c(keep.vars, 'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub', keep.vars)
  }
  if (missing(briefcope)) {
    briefcope <- c(paste('BriefCOPE_', 1:28, sep = ""))
  }
  
  raw.dat <- data[,briefcope]
  
  vars.problem.focused <- c(paste("BriefCOPE_", c(2,7,10,12,14,17,23,25), sep = ""))
  vars.emotion.focused <- c(paste("BriefCOPE_", c(5,9,13,15,18,20,21,22,24,26,27,28), sep = ""))
  vars.avoidant <- c(paste("BriefCOPE_", c(1,3,4,6,8,11,16,19), sep = ""))
  
  #facet scores
  vars.problemfocused.activecoping <- c(paste("BriefCOPE_", c(2,7), sep = ""))
  vars.problemfocused.infosupport <- c(paste("BriefCOPE_", c(10,23), sep = ""))
  vars.problemfocused.posreframe <- c(paste("BriefCOPE_", c(12,17), sep = ""))
  vars.problemfocused.planning <- c(paste("BriefCOPE_", c(14,25), sep = ""))
  
  vars.emotionfocused.emotsupport <- c(paste("BriefCOPE_", c(5,15), sep = ""))
  vars.emotionfocused.venting <- c(paste("BriefCOPE_", c(9,21), sep = ""))
  vars.emotionfocused.humor <- c(paste("BriefCOPE_", c(18,28), sep = ""))
  vars.emotionfocused.acceptance <- c(paste("BriefCOPE_", c(20,24), sep = ""))
  vars.emotionfocused.religion <- c(paste("BriefCOPE_", c(22,27), sep = ""))
  vars.emotionfocused.selfblame <- c(paste("BriefCOPE_", c(13,26), sep = ""))
  
  vars.avoidant.selfdistract <- c(paste("BriefCOPE_", c(1,19), sep = ""))
  vars.avoidant.denial <- c(paste("BriefCOPE_", c(3,8), sep = ""))
  vars.avoidant.substanceuse <- c(paste("BriefCOPE_", c(4,11), sep = ""))
  vars.avoidant.behavdisengage <- c(paste("BriefCOPE_", c(6,16), sep = ""))
  
  #scores
  briefcope.problemfocused <- rowSums(data[,vars.problem.focused])/length(vars.problem.focused)
  briefcope.emotionfocused <- rowSums(data[,vars.emotion.focused])/length(vars.emotion.focused)
  briefcope.avoidant <- rowSums(data[,vars.avoidant])/length(vars.avoidant)
  
  briefcope.problemfocusedActiveCoping <- rowSums(data[,vars.problemfocused.activecoping])/length(vars.problemfocused.activecoping)
  briefcope.problemfocusedInformationalSupport <- rowSums(data[,vars.problemfocused.infosupport])/length(vars.problemfocused.infosupport)
  briefcope.problemfocusedPositiveReframing <- rowSums(data[,vars.problemfocused.posreframe])/length(vars.problemfocused.posreframe)
  briefcope.problemfocusedPlanning <- rowSums(data[,vars.problemfocused.planning])/length(vars.problemfocused.planning)
  
  briefcope.emotionfocusedEmotionalSupport <- rowSums(data[,vars.emotionfocused.emotsupport])/length(vars.emotionfocused.emotsupport)
  briefcope.emotionfocusedVenting <- rowSums(data[,vars.emotionfocused.venting])/length(vars.emotionfocused.venting)
  briefcope.emotionfocusedHumor <- rowSums(data[,vars.emotionfocused.humor])/length(vars.emotionfocused.humor)
  briefcope.emotionfocusedAcceptance <- rowSums(data[,vars.emotionfocused.acceptance])/length(vars.emotionfocused.acceptance)
  briefcope.emotionfocusedReligion <- rowSums(data[,vars.emotionfocused.religion])/length(vars.emotionfocused.religion)
  briefcope.emotionfocusedSelfBlame <- rowSums(data[,vars.emotionfocused.selfblame])/length(vars.emotionfocused.selfblame)
  
  briefcope.avoidantSelfDistraction <- rowSums(data[,vars.avoidant.selfdistract])/length(vars.avoidant.selfdistract)
  briefcope.avoidantDenial <- rowSums(data[,vars.avoidant.denial])/length(vars.avoidant.denial)
  briefcope.avoidantSubstanceUse <- rowSums(data[,vars.avoidant.substanceuse])/length(vars.avoidant.substanceuse)
  briefcope.avoidantBehavioralDisengagement <- rowSums(data[,vars.avoidant.behavdisengage])/length(vars.avoidant.behavdisengage)
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps,raw.dat,briefcope.problemfocused,briefcope.emotionfocused,
                  briefcope.avoidant, briefcope.problemfocusedActiveCoping,
                  briefcope.problemfocusedInformationalSupport, briefcope.problemfocusedPositiveReframing,
                  briefcope.problemfocusedPlanning, briefcope.emotionfocusedEmotionalSupport,
                  briefcope.emotionfocusedVenting, briefcope.emotionfocusedHumor,
                  briefcope.emotionfocusedAcceptance, briefcope.emotionfocusedReligion,
                  briefcope.emotionfocusedSelfBlame, briefcope.avoidantSelfDistraction,
                  briefcope.avoidantDenial, briefcope.avoidantSubstanceUse, 
                  briefcope.avoidantBehavioralDisengagement)
  output <- as.data.frame(output)
  return(output)
}

#************************************************************************
# Brief RCOPE
#************************************************************************
brief.rcope.scoring <- function(data, keep.vars, brief.rcope) {
  keep.vars <- c('ID', 'date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <- c(keep.vars, 'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub', keep.vars)
  }
  if (missing(brief.rcope)) {
    brief.rcope <- c(paste('BriefRCOPE_', 1:14, sep = ""))
  }
  
  raw.dat <- data[,brief.rcope]
  
  vars.positive <- c(paste("BriefRCOPE_", 1:7, sep = ""))
  vars.negative <- c(paste("BriefRCOPE_", 8:14, sep = ""))
  
  briefrcope.positive <- rowSums(data[,vars.positive], na.rm = T)
  briefrcope.negative <- rowSums(data[,vars.negative], na.rm = T)
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps, raw.dat, briefrcope.positive, briefrcope.negative )
  output <- as.data.frame(output)
  return(output)
}

################################################################################
# Brief Symptom Inventory (BSI)
################################################################################
bsi.scoring <- function(data,keeps.vars,bsi){
  # if no 'varnames' is defined, we assume that items are named
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
  if (missing(bsi)) {
    bsi <- c(paste('BSI_',1:53,sep=""))
  }
  raw.dat <- data[,bsi]
  
  # define subscales
  vars.bsiSomatization <- c(paste('BSI_',c(2,7,23,29,30,33,37),sep=""))
  vars.bsiObsessionCompulsion <- c(paste('BSI_',c(5,15,26,27,32,36),sep=""))
  vars.bsiInterpersonalSensitivity <- c(paste('BSI_',c(20,21,22,42),sep=""))
  vars.bsiDepression <- c(paste('BSI_',c(9,16,17,18,35,50),sep=""))
  vars.bsiAnxiety <- c(paste('BSI_',c(1,12,19,38,45,49),sep=""))
  vars.bsiHostility <- c(paste('BSI_',c(6,13,40,41,46),sep=""))
  vars.bsiPhobic <- c(paste('BSI_',c(8,28,31,43,47),sep=""))
  vars.bsiParanoid <- c(paste('BSI_',c(4,10,24,48,51),sep=""))
  vars.bsiPsychoticism <- c(paste('BSI_',c(3,14,34,44,53),sep=""))
  
  # calculate scores, raw scores need to be converted to T scores using BSI Manual
  bsi.somatization <- rowSums(data[,vars.bsiSomatization])/length(vars.bsiSomatization)
  bsi.obsessionCompulsion <- rowSums(data[,vars.bsiObsessionCompulsion])/length(vars.bsiObsessionCompulsion)
  bsi.interpersonalSensitivity <- rowSums(data[,vars.bsiInterpersonalSensitivity])/length(vars.bsiInterpersonalSensitivity)
  bsi.depression <- rowSums(data[,vars.bsiDepression])/length(vars.bsiDepression)
  bsi.anxiety <- rowSums(data[,vars.bsiAnxiety])/length(vars.bsiAnxiety)
  bsi.hostility <- rowSums(data[,vars.bsiHostility])/length(vars.bsiHostility)
  bsi.phobicAnxiety <- rowSums(data[,vars.bsiPhobic])/length(vars.bsiPhobic)
  bsi.paranoidIdeation <- rowSums(data[,vars.bsiParanoid])/length(vars.bsiParanoid)
  bsi.psychoticism <- rowSums(data[,vars.bsiPsychoticism])/length(vars.bsiPsychoticism)
  
  bsi.globalSeverityIndex <- rowMeans(data[,bsi], na.rm=T)
  bsi.positiveSymptomTotal <- rowSums(data[,bsi]!=0)
  bsi.positiveSymptomDistressIndex <- rowSums(data[,bsi])/bsi.positiveSymptomTotal
  
  # get variables we want to pass through to the output
  keeps <- data[,keep.vars]
  
  
  # combine scores and kept variables, make dataframe
  output <- cbind(keeps,raw.dat,bsi.somatization, bsi.obsessionCompulsion, 
                  bsi.interpersonalSensitivity, bsi.depression, bsi.anxiety, bsi.hostility,
                  bsi.phobicAnxiety, bsi.paranoidIdeation, bsi.psychoticism, 
                  bsi.globalSeverityIndex, bsi.positiveSymptomTotal, bsi.positiveSymptomDistressIndex)
  output <- as.data.frame(output)
  
  # return
  return(output)
  
}

#****************************************************************************
# Cannabis Coping Motives (MM, Coping Subscale)
#****************************************************************************
mmcoping.scoring <- function(data, keep.vars, mmcoping) {
  keep.vars <- c('ID', 'date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <- c(keep.vars, 'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub', keep.vars)
  }
  if (missing(mmcoping)) {
    mmcoping <- c(paste('MM_Coping_', 1:4, sep = ""))
  }
  
  raw.dat <- data[,mmcoping]
  
  mmcoping.total <- rowSums(data[,mmcoping])
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps, raw.dat, mmcoping.total)
  output <- as.data.frame(output)
  return(output)
}


#****************************************************************************
# Cannabis Withdrawal Scale (CWS)
#****************************************************************************
cws.scoring <- function(data, keep.vars, cws) {
  keep.vars <- c('ID', 'date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <- c(keep.vars, 'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub', keep.vars)
  }
  if (missing(cws)) {
    cws <- c(paste('CWS_Experience_', 1:19, sep = ""), paste('CWS_Impact_', 1:19, sep = ""))
  }
  
  vars.experience <- c(paste('CWS_Experience_', 1:19, sep = ""))
  vars.impact <- c(paste('CWS_Impact_', 1:19, sep = ""))
  
  raw.dat <- data[,cws]
  
  cws.experience <- rowSums(data[,vars.experience])
  cws.impact <- rowSums(data[,vars.impact])
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps, raw.dat, cws.experience, cws.impact)
  output <- as.data.frame(output)
  return(output)
}
#****************************************************************************
### CEQ (Challenging Experience Questionnaire) Old Scoring Method
### scores derived from SOCQ, HRS, and 5DASC items

oldceq.scoring <- function(data,keep.vars,socq,hrs,fdasc){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
  if (missing(socq)){
    socq <- c(paste('SOCQ_',1:100,sep=""))}
  if (missing(hrs)){
    hrs = c('HRS_1','HRS_2','HRS_2a','HRS_3','HRS_3a','HRS_4','HRS_4a','HRS_5','HRS_5a','HRS_6','HRS_7',
            'HRS_7a','HRS_8','HRS_9', 'HRS_9a', paste('HRS_', 10:22, sep = ''),'HRS_22a',paste('HRS_', 23:27, sep = ''),'HRS_27a','HRS_27b',
            'HRS_28','HRS_29','HRS_30','HRS_31','HRS_31a','HRS_32','HRS_33','HRS_34','HRS_34a','HRS_35','HRS_36',
            'HRS_36a',paste('HRS_', 37:40, sep = ''),'HRS_40a','HRS_41','HRS_41a',paste('HRS_', 42:50, sep = ''),
            'HRS_50a','HRS_51','HRS_51a','HRS_52','HRS_52a','HRS_53','HRS_54','HRS_55','HRS_55a','HRS_56',
            'HRS_57','HRS_58','HRS_59','HRS_59a','HRS_60','HRS_60a',paste('HRS_', 61:66, sep = ''),'HRS_66a',
            'HRS_66b','HRS_67','HRS_68','HRS_68a',paste('HRS_', 69:76, sep = ''),'HRS_76a','HRS_77','HRS_78',
            'HRS_79','HRS_80','HRS_80a','HRS_81','HRS_81a','HRS_82','HRS_82a','HRS_83','HRS_84','HRS_85','HRS_86',
            'HRS_86a','HRS_87','HRS_88','HRS_88a','HRS_89','HRS_90','HRS_90a',paste('HRS_', 91:99, sep = ''),'HRS_99a')
  }
  if (missing(fdasc)){
    fdasc <- c(paste('5DASC_',1:94,sep=""))
  }
  
  #identify which items will be used to score CEQ 
  hrs_items <- c(paste('HRS_',c(9,10,11,12,13,25,26,27,36,38,39,44,70,88),sep=""))
  socq_items <- c(paste('SOCQ_',c(52,91,16,13,85,45,70,40,72),sep=""))
  fdasc_items <- c(paste('5DASC_',c(89,32,64),sep=""))
  
  # scores across different scales use different ranges
  # adjust scores so that all data are within the same range
  hrs_adj = data[,hrs_items]/4
  socq_adj = data[,socq_items]/5
  fdasc_adj = data[,fdasc_items]/100
  
  new.ceq <- cbind(hrs_adj,socq_adj,fdasc_adj)
  new.ceq <- new.ceq*5
  
  vars.ceqFear <- c('HRS_26','HRS_27','SOCQ_52','HRS_25','5DASC_89')
  vars.ceqGrief <- c('HRS_36','SOCQ_91','HRS_38','HRS_39','SOCQ_16','SOCQ_13')
  vars.ceqPhysDist <- c(paste('HRS_',c(9:13),sep=""))
  vars.ceqInsanity <- c('SOCQ_85','HRS_88','5DASC_32')
  vars.ceqIsolation <- c('5DASC_64','HRS_44','SOCQ_45')
  vars.ceqDeath <- c('SOCQ_70','HRS_70')
  vars.ceqParanoia <- c ('SOCQ_40','SOCQ_72')
  vars.ceqTotal <- c(vars.ceqFear,vars.ceqGrief,vars.ceqPhysDist,vars.ceqInsanity,vars.ceqIsolation,vars.ceqDeath,vars.ceqParanoia)

  #make sure all responses are on a 0-origin scale. If min value of response data is "1", subtract 1 from each response
  ceq.fear      <- rowSums(new.ceq[,vars.ceqFear])/(length(vars.ceqFear))
  ceq.grief     <- rowSums(new.ceq[,vars.ceqGrief])/(length(vars.ceqGrief))
  ceq.physdistress  <- rowSums(new.ceq[,vars.ceqPhysDist])/(length(vars.ceqPhysDist))
  ceq.insanity  <- rowSums(new.ceq[,vars.ceqInsanity])/(length(vars.ceqInsanity))
  ceq.isolation <- rowSums(new.ceq[,vars.ceqIsolation])/(length(vars.ceqIsolation))
  ceq.death     <- rowSums(new.ceq[,vars.ceqDeath])/(length(vars.ceqDeath))
  ceq.paranoia  <- rowSums(new.ceq[,vars.ceqParanoia])/(length(vars.ceqParanoia))
  ceq.total     <- rowSums(new.ceq[,vars.ceqTotal])/(length(vars.ceqTotal))
  
  #calculate percentage of maximum possible score
  ceq.fear.pctmax  <-  ceq.fear/5
  ceq.grief.pctmax  <-  ceq.grief/5
  ceq.physdistress.pctmax  <-  ceq.physdistress/5
  ceq.insanity.pctmax  <-  ceq.insanity/5
  ceq.isolation.pctmax  <-  ceq.isolation/5
  ceq.death.pctmax  <-  ceq.death/5
  ceq.paranoia.pctmax  <-  ceq.paranoia/5
  ceq.total.pctmax  <-  ceq.total/5
  
  keeps <- data[,keep.vars]
  # uncomment if want to include raw data
  #raw.dat <- data[,ceq]
  
  # combine scores and kept variables, make dataframe
  ceqdf.pct <- cbind(ceq.fear.pctmax, ceq.grief.pctmax, ceq.physdistress.pctmax, ceq.insanity.pctmax,
                     ceq.isolation.pctmax, ceq.death.pctmax, ceq.paranoia.pctmax, ceq.total.pctmax)
  
  # uncomment if want to include raw data
  #output <- cbind(keeps,raw.dat,ceq.fear,ceq.grief,ceq.physdistress,ceq.insanity,ceq.isolation,
                 # ceq.death,ceq.paranoia,ceq.total, ceqdf.pct)
  
  output <- cbind(keeps,ceq.fear,ceq.grief,ceq.physdistress,ceq.insanity,ceq.isolation,
                  ceq.death,ceq.paranoia,ceq.total, ceqdf.pct) #excludes raw data
  output <- as.data.frame(output)
  return(output) 
}

#****************************************************************************
### CEQ (Challenging Experience Questionnaire) Old Scoring Method Partial 
### to be used when only HRS and SOCQ are present (no 5DASC)

oldceqPartial.scoring <- function(data,keep.vars,socq,hrs) { # oldceq.scoring but no 5DASC in raw columns (includes only some of the subscales)
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
  if (missing(socq)){
    socq <- c(paste('SOCQ_',1:100,sep=""))}
  if (missing(hrs)){
    hrs = c('HRS_1','HRS_2','HRS_2a','HRS_3','HRS_3a','HRS_4','HRS_4a','HRS_5','HRS_5a','HRS_6','HRS_7',
            'HRS_7a','HRS_8','HRS_9', 'HRS_9a', paste('HRS_', 10:22, sep = ''),'HRS_22a',paste('HRS_', 23:27, sep = ''),'HRS_27a','HRS_27b',
            'HRS_28','HRS_29','HRS_30','HRS_31','HRS_31a','HRS_32','HRS_33','HRS_34','HRS_34a','HRS_35','HRS_36',
            'HRS_36a',paste('HRS_', 37:40, sep = ''),'HRS_40a','HRS_41','HRS_41a',paste('HRS_', 42:50, sep = ''),
            'HRS_50a','HRS_51','HRS_51a','HRS_52','HRS_52a','HRS_53','HRS_54','HRS_55','HRS_55a','HRS_56',
            'HRS_57','HRS_58','HRS_59','HRS_59a','HRS_60','HRS_60a',paste('HRS_', 61:66, sep = ''),'HRS_66a',
            'HRS_66b','HRS_67','HRS_68','HRS_68a',paste('HRS_', 69:76, sep = ''),'HRS_76a','HRS_77','HRS_78',
            'HRS_79','HRS_80','HRS_80a','HRS_81','HRS_81a','HRS_82','HRS_82a','HRS_83','HRS_84','HRS_85','HRS_86',
            'HRS_86a','HRS_87','HRS_88','HRS_88a','HRS_89','HRS_90','HRS_90a',paste('HRS_', 91:99, sep = ''),'HRS_99a')
  }
  # if (missing(fdasc)){
  #   fdasc <- c(paste('5DASC_',1:94,sep=""))
  #}
  
  #identify which items will be used to score CEQ 
  hrs_items <- c(paste('HRS_',c(9,10,11,12,13,25,26,27,36,38,39,44,70,88),sep=""))
  socq_items <- c(paste('SOCQ_',c(52,91,16,13,85,45,70,40,72),sep=""))
  # fdasc_items <- c(paste('5DASC_',c(89,32,64),sep=""))
  
  # scores across different scales use different ranges
  # adjust scores so that all data are within the same range
  hrs_adj = data[,hrs_items]/4
  socq_adj = data[,socq_items]/5
  # fdasc_adj = data[,fdasc_items]/100
  
  new.ceq <- cbind(hrs_adj,socq_adj)
  new.ceq <- new.ceq*5
  
  #vars.ceqFear <- c('HRS_26','HRS_27','SOCQ_52','HRS_25','5DASC_89')
  vars.ceqGrief <- c('HRS_36','SOCQ_91','HRS_38','HRS_39','SOCQ_16','SOCQ_13')
  vars.ceqPhysDist <- c(paste('HRS_',c(9:13),sep=""))
  #vars.ceqInsanity <- c('SOCQ_85','HRS_88','5DASC_32')
  #vars.ceqIsolation <- c('5DASC_64','HRS_44','SOCQ_45')
  vars.ceqDeath <- c('SOCQ_70','HRS_70')
  vars.ceqParanoia <- c ('SOCQ_40','SOCQ_72')
  #vars.ceqTotal <- c(vars.ceqGrief,vars.ceqPhysDist,vars.ceqDeath,vars.ceqParanoia)
  
  #make sure all responses are on a 0-origin scale. If min value of response data is "1", subtract 1 from each response
  #ceq.fear      <- rowSums(new.ceq[,vars.ceqFear])/(length(vars.ceqFear))
  ceq.grief     <- rowSums(new.ceq[,vars.ceqGrief])/(length(vars.ceqGrief))
  ceq.physDistress  <- rowSums(new.ceq[,vars.ceqPhysDist])/(length(vars.ceqPhysDist))
  #ceq.insanity  <- rowSums(new.ceq[,vars.ceqInsanity])/(length(vars.ceqInsanity))
  #ceq.isolation <- rowSums(new.ceq[,vars.ceqIsolation])/(length(vars.ceqIsolation))
  ceq.death     <- rowSums(new.ceq[,vars.ceqDeath])/(length(vars.ceqDeath))
  ceq.paranoia  <- rowSums(new.ceq[,vars.ceqParanoia])/(length(vars.ceqParanoia))
  #ceq.total     <- rowSums(new.ceq[,vars.ceqTotal])/(length(vars.ceqTotal))
  
  #calculate percentage of maximum possible score
  #ceq.fear.pctmax  <-  ceq.fear/5
  ceq.grief.pctmax  <-  ceq.grief/5
  ceq.physDistress.pctmax  <-  ceq.physDistress/5
  #ceq.insanity.pctmax  <-  ceq.insanity/5
  #ceq.isolation.pctmax  <-  ceq.isolation/5
  ceq.death.pctmax  <-  ceq.death/5
  ceq.paranoia.pctmax  <-  ceq.paranoia/5
  #ceq.pctmax  <-  ceq.total/5
  
  keeps <- data[,keep.vars]
  # uncomment if want to include raw data
  #raw.dat <- data[,ceq]
  
  # combine scores and kept variables, make dataframe
  ceqdf.pct <- cbind(ceq.grief.pctmax, ceq.physDistress.pctmax, ceq.death.pctmax, 
                     ceq.paranoia.pctmax)
  
  # uncomment if want to include raw data
  #output <- cbind(keeps,raw.dat,ceq.fear,ceq.grief,ceq.physDistress,ceq.insanity,ceq.isolation,
  # ceq.death,ceq.paranoia,ceq.total, ceqdf.pct)
  
  output <- cbind(keeps,ceq.grief,ceq.physDistress,
                  ceq.death,ceq.paranoia,ceqdf.pct) #excludes raw data
  output <- as.data.frame(output)
  return(output) 
}

#****************************************************************************
# Brief CEQ (Brief Challenging Experience Questionnaire, 7-item)
#****************************************************************************
briefceq.scoring <- function(data,keep.vars,briefceq){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
  if (missing(briefceq)){
    briefceq <- c(paste('BriefCEQ_',1:7,sep=""))
  }
  
  raw.dat <- data[,briefceq]
  
  briefceq.total <- rowSums(data[,briefceq])/(length(briefceq))
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps, raw.dat, briefceq.total)
  return(output)
}

#****************************************************************************
# CEQ (Challenging Experience Questionnaire)
ceq.scoring <- function(data,keep.vars,ceq){
  keep.vars <- intersect(c('ID','date','time.point','sub'), colnames(data))

  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
  if (missing(ceq)){
    ceq <- c(paste('CEQ_',1:26,sep=""))
  }

  raw.dat <- data[,ceq]
  
  vars.ceqFear <- c(paste('CEQ_',c(4,7,14,21,26),sep=""))
  vars.ceqGrief <- c(paste('CEQ_',c(2,6,9,11,23,25),sep=""))
  vars.ceqPhysDist <- c(paste('CEQ_',c(3,5,15,17,18),sep=""))
  vars.ceqInsanity <- c(paste('CEQ_',c(8,13,19),sep=""))
  vars.ceqIsolation <- c(paste('CEQ_',c(1,10,24),sep=""))
  vars.ceqDeath <- c(paste('CEQ_',c(16,20),sep=""))
  vars.ceqParanoia <- c(paste('CEQ_',c(12,22),sep=""))
  vars.ceqTotal <- c(paste('CEQ_',1:26,sep=""))

  #make sure all responses are on a 0-origin scale. If min value of response data is "1", subtract 1 from each response
  ceq.fear      <- rowSums(data[,vars.ceqFear])/(length(vars.ceqFear))
  ceq.grief     <- rowSums(data[,vars.ceqGrief])/(length(vars.ceqGrief))
  ceq.physDistress  <- rowSums(data[,vars.ceqPhysDist])/(length(vars.ceqPhysDist))
  ceq.insanity  <- rowSums(data[,vars.ceqInsanity])/(length(vars.ceqInsanity))
  ceq.isolation <- rowSums(data[,vars.ceqIsolation])/(length(vars.ceqIsolation))
  ceq.death     <- rowSums(data[,vars.ceqDeath])/(length(vars.ceqDeath))
  ceq.paranoia  <- rowSums(data[,vars.ceqParanoia])/(length(vars.ceqParanoia))
  ceq.total     <- rowSums(data[,vars.ceqTotal])/(length(vars.ceqTotal))

  #calculate percentage of maximum possible score
  ceq.fear.pctmax  <-  ceq.fear/5
  ceq.grief.pctmax  <-  ceq.grief/5
  ceq.physDistress.pctmax  <-  ceq.physDistress/5
  ceq.insanity.pctmax  <-  ceq.insanity/5
  ceq.isolation.pctmax  <-  ceq.isolation/5
  ceq.death.pctmax  <-  ceq.death/5
  ceq.paranoia.pctmax  <-  ceq.paranoia/5
  ceq.total.pctmax  <-  ceq.total/5

  keeps <- data[,keep.vars]
  
  # combine scores and kept variables, make dataframe
  ceqdf.pct <- cbind(ceq.fear.pctmax, ceq.grief.pctmax, ceq.physDistress.pctmax, ceq.insanity.pctmax,
                     ceq.isolation.pctmax, ceq.death.pctmax, ceq.paranoia.pctmax, ceq.total.pctmax)
  output <- cbind(keeps,raw.dat,ceq.fear,ceq.grief,ceq.physDistress,ceq.insanity,ceq.isolation,
                  ceq.death,ceq.paranoia,ceq.total, ceqdf.pct)
  output <- as.data.frame(output)
  return(output)
}

#####
# Chronic Pain Values Inventory
#####
cpvi.scoring <- function(data, keeps.vars, cpvi) {
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
  if (missing(cpvi)){
    cpvi <- paste('CPVI_', 1:12, sep="")
  }
  raw.dat <- data[, cpvi]
  
  vars.success <- paste('CPVI_', 7:12, sep="")
  cpvi.success <- rowSums(data[,vars.success])/length(vars.success)
  
  
  cpvi.family <- data$CPVI_1 - data$CPVI_7
  cpvi.intRelations <- data$CPVI_2 - data$CPVI_8
  cpvi.friends <- data$CPVI_3 - data$CPVI_9
  cpvi.work <- data$CPVI_4 - data$CPVI_10
  cpvi.health <- data$CPVI_5 - data$CPVI_11
  cpvi.growth <- data$CPVI_6 - data$CPVI_12
  
  cpvi.discrepancy <- (cpvi.family + cpvi.intRelations + cpvi.friends + cpvi.work + cpvi.health + cpvi.growth)/6
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps, raw.dat, cpvi.success, cpvi.discrepancy)
  output <- as.data.frame(output)
  return(output)
}

#************************************************************************
# Core Dispositional Flow Scale
#************************************************************************
cdfs.scoring <- function(data, keep.vars, cdfs) {
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <- c(keep.vars, 'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub', keep.vars)
  }
  if (missing(cdfs)) {
    cdfs <- c(paste('CDFS_', 1:10, sep = ""))
  }
  
  raw.dat <- data[,cdfs]

  cdfs.total <- rowSums(data[,cdfs])/length(cdfs)
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps,raw.dat,cdfs.total)
  output <- as.data.frame(output)
  return(output)
}

#************************************************************************
# Core Flow State Scale
#************************************************************************
cfss.scoring <- function(data, keep.vars, cfss) {
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <- c(keep.vars, 'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub', keep.vars)
  }
  if (missing(cfss)) {
    cfss <- c(paste('CFSS_', 1:10, sep = ""))
  }
  
  raw.dat <- data[,cfss]
  
  cfss.total <- rowSums(data[,cfss])/length(cfss)
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps,raw.dat,cfss.total)
  output <- as.data.frame(output)
  return(output)
}


#*******************************************************************************
# Caregiver Reaction Assessment (CRA)
cra.scoring <- function(data,keep.vars,cra){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }

  if (missing(cra)){
    cra <- c(paste('CRA_',1:24,sep=""))
  }
  raw.dat <- data[,cra]
  
  crarev <- c(paste('CRA_',c(7,13,15,19,3),sep=""))
  data[,crarev] = 6-data[,crarev]
  
  vars.esteem <- c(c('CRA_1','CRA_7','CRA_9','CRA_12','CRA_17','CRA_20','CRA_23'))
  vars.familySupport <- c(c('CRA_2','CRA_6','CRA_13','CRA_16','CRA_22'))
  vars.finances <- c(c('CRA_3','CRA_21','CRA_24'))
  vars.schedule <- c(c('CRA_4','CRA_8','CRA_11','CRA_14','CRA_18'))
  vars.health <- c(c('CRA_5','CRA_10','CRA_15','CRA_19'))
  
  cra.esteem <- rowSums(sapply(data[,vars.esteem],as.numeric))/length(vars.esteem)
  cra.familySupport <- rowSums(sapply(data[,vars.familySupport],as.numeric))/length(vars.familySupport)
  cra.finances <- rowSums(sapply(data[,vars.finances],as.numeric))/length(vars.finances)
  cra.schedule <- rowSums(sapply(data[,vars.schedule],as.numeric))/length(vars.schedule)
  cra.health <- rowSums(sapply(data[,vars.health],as.numeric))/length(vars.health)
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps,raw.dat, cra.esteem, cra.familySupport,
                  cra.finances, cra.schedule, cra.health)
  output <- as.data.frame(output)
  return(output)
  
}

#************************************************************************
# Cognitive Flexibility Scale 
#************************************************************************
cogflex.scoring <- function(data, keep.vars, cogflex) {
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <- c(keep.vars, 'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub', keep.vars)
  }
  if (missing(cogflex)) {
    cogflex <- c(paste('CogFlex_',c(1:12), sep = ""))
  }
  raw.dat <- data[,cogflex]
  
  reverse.items <- c(paste('CogFlex_', c(2,3,5,10), sep = ""))
  data[,reverse.items] <- 7-data[,reverse.items]
  
  cogflex.total <- rowSums(data[,cogflex])
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps,raw.dat,cogflex.total)
  output <- as.data.frame(output)
  return(output)
}

#****************************************************************************
# Cognitive Fusion (BEFORE & AFTER)
cogfus.prepost.scoring <- function(data,keep.vars,cogfus){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
  if (missing(cogfus)){
    cogfus <- c(c(paste('CogFus_',1:7,'_Before',sep="")), c(paste('CogFus_',1:7,'_After',sep="")))
  }
  raw.dat <- data[,cogfus]
  
  vars.cogfus.before <- c(paste('CogFus_',1:7,'_Before',sep=""))
  vars.cogfus.after <- c(paste('CogFus_',1:7,'_After',sep=""))
  
  # calculate scores
  cogfus.before <- rowMeans(sapply(data[,vars.cogfus.before],as.numeric))
  cogfus.after <- rowMeans(sapply(data[,vars.cogfus.after],as.numeric))
  
  # get variables we want to pass through to the output
  keeps <- data[,keep.vars]
  
  # combine scores and kept variables, make dataframe
  output <- cbind(keeps,raw.dat,cogfus.before,cogfus.after)
  output <- as.data.frame(output)
  
  # return
  return(output)
  
}
#************************************************************************
# Connectedness to Nature Scale (CNS)
#************************************************************************
cns.scoring <- function(data, keep.vars, cns) {
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <- c(keep.vars, 'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub', keep.vars)
  }
  if (missing(cns)) {
    cns <- c(paste('CNS_', 1:14, sep = ""))
  }
  
  raw.dat <- data[,cns]
  
  rev.items <- c(paste("CNS_", c(4,12,14), sep = ""))
  data[,rev.items] <- 6-data[,rev.items]
  
  cns.score <- rowSums(data[,cns])/length(cns)
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps,raw.dat,cns.score)
  output <- as.data.frame(output)
  return(output)
}
#************************************************************************
# Current Opioid Misuse Measure (COMM)
#************************************************************************
comm.scoring <- function(data, keep.vars, comm) {
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <- c(keep.vars, 'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub', keep.vars)
  }
  if (missing(comm)) {
    comm <- c(paste('COMM_', 1:17, sep = ""))
  }
  
  raw.dat <- data[,comm]
  
  comm.total <- rowSums(data[,comm])
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps,raw.dat,comm.total)
  output <- as.data.frame(output)
  return(output)
}

#****************************************************************************
# Coppenhagen Burnout Inventory
copenhagenburnout.scoring <- function(data, keep.vars, burnout){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
  if (missing(burnout)){
    burnout <- c(paste('CopenhagenBurnoutPersonal_',1:6,sep=""), paste('CopenhagenBurnoutWork_',1:7,sep=""))
  }
  
  raw.dat <- data[,burnout]
  
  # reverse scoring for final item of burnout work subscale
  rev.burnout <- 'CopenhagenBurnoutWork_7'
  
  for (j in 1:nrow(data)){
    if (is.na(data[,rev.burnout][j])){
      data[,rev.burnout][j] = NA
    } else if (data[,rev.burnout][j] == 0){
      data[,rev.burnout][j] = 100
    } else if (data[,rev.burnout][j] == 25){
      data[,rev.burnout][j] = 75
    } else if (data[,rev.burnout][j] == 75){
      data[,rev.burnout][j] = 25
    } else if (data[,rev.burnout][j] == 100){
      data[,rev.burnout][j] = 0
    }
  }
  
  vars.burnoutPersonal <- paste('CopenhagenBurnoutPersonal_',1:6,sep="")
  vars.burnoutWork <- paste('CopenhagenBurnoutWork_',1:7,sep="")
  
  burnout.responses <- data.frame()
  for (i in 1:nrow(data)){
    if (sum(is.na(data[,vars.burnoutPersonal][i,])) > 3){
      burnout.personal <- NA
    } else {
        burnout.personal <- sum(data[,vars.burnoutPersonal][i,])/length(vars.burnoutPersonal)
    }
    
    if (sum(is.na(data[,vars.burnoutWork][i,])) > 3){
      burnout.work <- NA
    } else {
      burnout.work <- sum(data[,vars.burnoutWork][i,])/length(vars.burnoutWork)
    }
    
    burnout.total <- cbind(burnout.personal, burnout.work)
    burnout.responses <- rbind(burnout.responses, burnout.total)
  }

  keeps <- data[,keep.vars]
  
  # combine scores and kept variables, make dataframe
  output <- cbind(keeps,raw.dat, burnout.responses)
  output <- as.data.frame(output)
  return(output)

}

#************************************************************************
# Clinical Opiate Withdrawal Scale (COWS)
#************************************************************************

cows.scoring <- function(data, keep.vars, cows){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
  if (missing(cows)){
    cows <- c(paste('COWS_',1:11,sep=""))
  }
  
  raw.dat <- data[,cows]
  
  cows.total <- rowSums(data[,cows])
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps, raw.dat, cows.total)
  output <- as.data.frame(output)
  return(output)
}
  

#****************************************************************************
# DASS (Depression, Anxiety, Stress Scale)
dass.scoring <- function(data,keep.vars,dass) {
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
  if (missing(dass)){
    dass <- c(paste('DASS_',1:21,sep=""))
  }
  raw.dat <- data[,dass]
  
  # Define total scale and all subscales
  vars.dassDep <- c(paste('DASS_',c(3,5,10,13,16,17,21),sep=""))
  vars.dassAnx <- c(paste('DASS_',c(2,4,7,9,15,19,20),sep=""))
  vars.dassStress <- c(paste('DASS_',c(1,6,8,11,12,14,18),sep=""))
  
  # Calculate score. Note subscale scores are doubled
  if(length(data[,'DASS_1']) > 1) { # if we have more than one participant. If we only have one, we cannot apply rowSums or sapply to data.
    dass.depression <- rowSums(sapply(data[,vars.dassDep],as.numeric))*2
    dass.anxiety <- rowSums(sapply(data[,vars.dassAnx],as.numeric))*2
    dass.stress <- rowSums(sapply(data[,vars.dassStress],as.numeric))*2
  } else {
    dass.depression <- sum(as.numeric(data[,vars.dassDep]))*2
    dass.anxiety <- sum(as.numeric(data[,vars.dassAnx]))*2
    dass.stress <- sum(as.numeric(data[,vars.dassStress]))*2 
  }
  
  # get variables we want to pass through to the output
  # bring participant sub,id,date back into the data frame
  keeps <- data[,keep.vars]
  
  # combine scores and kept variables, make dataframe
  output <- cbind(keeps,raw.dat,dass.depression,dass.anxiety,dass.stress)
  output <- as.data.frame(output)
  #names(output) <- c(keep.vars,'dass.total','dass.depression','dass.anxietyiety','dass.stress')
  
  # return
  return(output)
}

#************************************************************************
# The Death Transcendence Scale (DTS) ### ITEM NUMBERS SHOULD BE ALTERED TO MATCH DOCUMENTATION FOR THIS FUNCTION TO WORK
#************************************************************************
dts.scoring <- function(data, keep.vars, dts) {
  keep.vars <- c('ID', 'date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <- c(keep.vars, 'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub', keep.vars)
  }
  if (missing(dts)) {
    dts <- c(paste('DTS_', 1:26, sep = ""))
  }
  
  raw.dat <- data[,dts]
  
  reverse.items <- c(paste("DTS_", c(4,8,17), sep = ""))
  data[,reverse.items] <- 8-data[, reverse.items]
  
  vars.Mystical <- c(paste('DTS_', c(3, 4, 8, 11, 17), sep = ""))
  vars.Religious <- c(paste('DTS_', c(5, 7, 12, 15, 24), sep = ""))
  vars.Nature <- c(paste('DTS_', c(6, 9, 14, 19, 25), sep = ""))
  vars.Biosocial <- c(paste('DTS_', c(2, 13, 20:23), sep = ""))
  vars.Creative <- c(paste("DTS_", c(1, 10, 16, 18, 26), sep = ""))
  
  # Calculate sum scores for each subscale with na.rm = TRUE
  dts.mystical <- rowSums(data[, vars.Mystical])
  dts.religious <- rowSums(data[, vars.Religious])
  dts.nature <- rowSums(data[, vars.Nature])
  dts.biosocial <- rowSums(data[, vars.Biosocial])
  dts.creative <- rowSums(data[, vars.Creative])
  
  dts.total <- rowSums(data[,dts])
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps, raw.dat, dts.mystical, dts.religious, dts.nature, dts.biosocial, dts.creative, dts.total)
  output <- as.data.frame(output)
  return(output)
}
###################################################################################################
# Delay Discounting Scoring - Money (9 ITEM SMALL Magnitude)
###################################################################################################
ddmoney.scoring <- function(data, keep.vars, ddmoney) {
  # 0.00015813 = 5
  # 0.00039904 = 7
  # 0.00100267 = 9
  # 0.0025 = 8
  # 0.00595829 = 1
  # 0.0158046 = 6
  # 0.04135338 = 2
  # 0.1025641 = 3
  # 0.24675325 = 4
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  if (missing(ddmoney)){
    ddmoney <- c(paste('DDMoney_', 1:9,sep=""))
  }
  raw.dat <- data[,ddmoney]
  k.at.indif <- c('5'=0.00015813, '7'=0.00039904, '9'=0.00100267, '8'=0.0025,
                  '1'=0.00595829, '6'=0.0158046, '2'=0.04135338, '3'=0.1025641, '4'=0.24675325)
  
  if(max(data[,ddmoney]) == 1) {
    delay <- 1
    imm <- 0
  } else if(max(data[,ddmoney]) == 2) {
    delay <- 2
    imm <- 1
  } #else {
  # return 
  #}
  
  # Count 
  c5 <- rowSums(data[,ddmoney]==delay) # # of delay's overall
  
  c7 <- rowSums(data['DD_Money_Kirby_5']==imm) + # don't use , when subsetting with 1 col to keep dimension the same
    rowSums(data[,c(paste('DD_Money_Kirby_',c(1:4,6:9),sep=""))]==delay) # # of imm's in #5 + number of delay's in rest
  
  c9 <- rowSums(data[,c(paste('DD_Money_Kirby_',c(5,7),sep=""))]==imm) + 
    rowSums(data[,c(paste('DD_Money_Kirby_',c(1:4,6,8,9),sep=""))]==delay)
  
  c8 <- rowSums(data[,c(paste('DD_Money_Kirby_',c(5,7,9),sep=""))]==imm) + 
    rowSums(data[,c(paste('DD_Money_Kirby_',c(1:4,6,8),sep=""))]==delay)
  
  c1 <- rowSums(data[,c(paste('DD_Money_Kirby_',c(5,7,9, 8),sep=""))]==imm) + 
    rowSums(data[,c(paste('DD_Money_Kirby_',c(1:4,6),sep=""))]==delay)
  
  c6 <- rowSums(data[,c(paste('DD_Money_Kirby_',c(5,7,9,8,1),sep=""))]==imm) + 
    rowSums(data[,c(paste('DD_Money_Kirby_',c(2:4,6),sep=""))]==delay)
  
  c2 <- rowSums(data[,c(paste('DD_Money_Kirby_',c(5,7,9,8,1,6),sep=""))]==imm) + 
    rowSums(data[,c(paste('DD_Money_Kirby_',c(2:4),sep=""))]==delay)
  
  c3 <- rowSums(data[,c(paste('DD_Money_Kirby_',c(5,7,9,8,1,6,2),sep=""))]==imm) + 
    rowSums(data[,c(paste('DD_Money_Kirby_',c(3,4),sep=""))]==delay)
  
  c4_1 <- rowSums(data[,c(paste('DD_Money_Kirby_',c(5,7,9,8,1,6,2,3),sep=""))]==imm) + 
    rowSums(data['DD_Money_Kirby_4']==delay)
  
  c4_2 <- rowSums(data[,ddmoney]==imm)
  calculations.df <- cbind(c5, c7, c9, c8, c1, c6, c2, c3, c4_1, c4_2)
  
  # calculate the max number of consistent responses per participant
  num.consistent <- apply(X=calculations.df, MARGIN=1, FUN=max)
  calculations.df <- cbind(calculations.df, num.consistent)
  
  # if the previously calculated c<current number> == the num.consistent for that 
  # row, supply the geometric mean of the k at indifference associated with that
  # current number and the previous number (order based off of instructions for measure)
  gm <- function(x) {
    return (exp(mean(log(na.omit(x)))))
  }
  
  k5 <- apply(calculations.df, 1,function(x) if(x['c5']==x['num.consistent']) {k.at.indif['5']} else {NA})
  k7 <- apply(calculations.df, 1,function(x) if(x['c7']==x['num.consistent']) {gm(c(k.at.indif['5'], k.at.indif['7']))} else {NA})
  k9 <- apply(calculations.df, 1,function(x) if(x['c9']==x['num.consistent']) {gm(c(k.at.indif['9'], k.at.indif['7']))} else {NA})
  k8 <- apply(calculations.df, 1,function(x) if(x['c8']==x['num.consistent']) {gm(c(k.at.indif['8'], k.at.indif['9']))} else {NA})
  k1 <- apply(calculations.df, 1,function(x) if(x['c1']==x['num.consistent']) {gm(c(k.at.indif['1'], k.at.indif['8']))} else {NA})
  k6 <- apply(calculations.df, 1,function(x) if(x['c6']==x['num.consistent']) {gm(c(k.at.indif['6'], k.at.indif['1']))} else {NA})
  k2 <- apply(calculations.df, 1,function(x) if(x['c2']==x['num.consistent']) {gm(c(k.at.indif['2'], k.at.indif['6']))} else {NA})
  k3 <- apply(calculations.df, 1,function(x) if(x['c3']==x['num.consistent']) {gm(c(k.at.indif['3'], k.at.indif['2']))} else {NA})
  k4_1 <- apply(calculations.df, 1,function(x) if(x['c4_1']==x['num.consistent']) {gm(c(k.at.indif['4'], k.at.indif['3']))} else {NA})
  k4_2 <- apply(calculations.df, 1,function(x) if(x['c4_2']==x['num.consistent']) {k.at.indif['4']} else {NA})
  k.df <- cbind(k5,k7,k9,k8,k1,k6,k2,k3,k4_1,k4_2)
  
  ddmoney.kValue <- round(apply(k.df, 1, gm),5)
  #ln.k.value <- round(log(k.value),5)
  #log10.k.value <- round(log10(k.value),5)
  ddmoney.pctConsistent <- round((num.consistent/9)*100,5)
  ddmoney.pctLDR <- round(rowSums(data[,ddmoney]==delay)/length(ddmoney)*100,5)
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps,raw.dat,ddmoney.pctConsistent,ddmoney.kValue, ddmoney.pctLDR)
  output <- as.data.frame(output)
  return(output)
}

###################################################################################################
# Difficulties in Emotion Regulation Scale (DERS)
###################################################################################################
ders.scoring <- function(data, keep.vars, ders) {
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
  raw.colnames <- colnames(data)
  output <- c()
  
  if(length(grep("DERS_", raw.colnames)) == 36 || (length(grep("DifficultEmoReg_", raw.colnames)) == 36)){

    if (missing(ders)) {
      if("DifficultEmoReg_1" %in% colnames(data)) {
        ders <- c(paste('DifficultEmoReg_',1:36,sep=""))
        prefix <- 'DifficultEmoReg_'
      } else {
        ders <- c(paste('DERS_',1:36,sep=""))
        prefix <- 'DERS_'
      }
    }
    raw.dat <- data[,ders]

    # reverse Scoring
    vars.rev <- c(paste(prefix, c(2, 6, 8, 10, 17, 34, 1, 7, 24, 20, 22), sep=""))
    data[,vars.rev] <- 6-data[,vars.rev]

    # subscales
    vars.dersAwareness <- c(paste(prefix, c(2, 6, 8, 10, 17, 34), sep=""))
    vars.dersClarity <- c(paste(prefix, c(1, 4, 5, 7, 9), sep=""))
    vars.dersImpulse <- c(paste(prefix, c(3, 14, 19, 24, 27, 32), sep=""))
    vars.dersGoals <- c(paste(prefix, c(13, 18, 20, 26, 33), sep=""))
    vars.dersNonaccept <- c(paste(prefix, c(11, 12, 21, 23, 25, 29), sep=""))
    vars.dersStrategies <- c(paste(prefix, c(15, 16, 22, 28, 30, 31, 35, 36), sep=""))

    # scoring
    ders.awareness <- rowSums(data[,vars.dersAwareness])
    ders.clarity <- rowSums(data[,vars.dersClarity])
    ders.impulse <- rowSums(data[,vars.dersImpulse])
    ders.goals <- rowSums(data[vars.dersGoals])
    ders.nonacceptance <- rowSums(data[,vars.dersNonaccept])
    ders.strategies <- rowSums(data[,vars.dersStrategies])
    ders.total <- rowSums(data[,ders])

    keeps <- data[,keep.vars]
    output <- cbind(keeps, raw.dat,ders.awareness, ders.clarity, ders.impulse, ders.goals,
                    ders.nonacceptance, ders.strategies, ders.total)
    return(output)
  } else if( (length(grep("DERS_", raw.colnames)) == 18) || (length(grep("DER_SF_", raw.colnames)) == 18)) {
    if (missing(ders)) {
      if("DER_SF_1" %in% colnames(data)) {
        ders <- c(paste('DER_SF_',1:18,sep=""))
        prefix <- 'DER_SF_'
      } else {
        ders <- c(paste('DERS_',1:18,sep=""))
        prefix <- 'DERS_'
      }
    }
    raw.dat <- data[,ders]
    
    # reverse scoring
    vars.rev <- c(paste(prefix, c(1, 4, 6), sep=""))
    data[,vars.rev] <- 6-data[,vars.rev]
    
    # subscales
    vars.dersAwareness <- c(paste(prefix, c(1, 4, 6), sep=""))
    vars.dersClarity <- c(paste(prefix, c(2, 3, 5), sep=""))
    vars.dersImpulse <- c(paste(prefix, c(9, 14, 17), sep=""))
    vars.dersGoals <- c(paste(prefix, c(8, 11, 13), sep=""))
    vars.dersNonaccept <- c(paste(prefix, c(7, 12, 16), sep=""))
    vars.dersStrategies <- c(paste(prefix, c(10, 15, 18), sep=""))
    
    # scoring
    ders.awareness <- rowSums(data[,vars.dersAwareness])
    ders.clarity <- rowSums(data[,vars.dersClarity])
    ders.impulse <- rowSums(data[,vars.dersImpulse])
    ders.goals <- rowSums(data[vars.dersGoals])
    ders.nonacceptance <- rowSums(data[,vars.dersNonaccept])
    ders.strategies <- rowSums(data[,vars.dersStrategies])
    ders.total <- rowSums(data[,ders])
    
    keeps <- data[,keep.vars]
    output <- cbind(keeps, raw.dat,ders.awareness, ders.clarity, ders.impulse, ders.goals,
                    ders.nonacceptance, ders.strategies, ders.total)
    return(output)
    
  }
}

#************************************************************************
# Dispositional Flow Scale (dfs2)
#************************************************************************
dfs2.scoring <- function(data, keep.vars, dfs2) {
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <- c(keep.vars, 'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub', keep.vars)
  }
  if (missing(dfs2)) {
    dfs2 <- c(paste('DFS2_', 1:36, sep = ""))
  }
  
  raw.dat <- data[,dfs2]
  
  vars.challengeSkillBalance <- paste0('DFS2_', c(1,10,19,28))
  vars.actionAwareness <- paste0('DFS2_', c(2,11,20,29))
  vars.clearGoals <- paste0('DFS2_', c(3,12,21,30))
  vars.unambiguousFeedback <- paste0('DFS2_', c(4,13,22,31))
  vars.concentration <- paste0('DFS2_', c(5,14,23,32))
  vars.senseControl <- paste0('DFS2_', c(6,15,24,33))
  vars.lossSelfConsciousness <- paste0('DFS2_', c(7,16,25,34))
  vars.transformationTime <- paste0('DFS2_', c(8,17,26,35))
  vars.autotelic <- paste0('DFS2_', c(9,18,27,36))
  
  dfs2.challengeSkillBalance <- rowSums(data[,vars.challengeSkillBalance], na.rm=T)
  dfs2.actionAwareness <- rowSums(data[,vars.actionAwareness], na.rm=T)
  dfs2.clearGoals <- rowSums(data[,vars.clearGoals], na.rm=T)
  dfs2.unambiguousFeedback <- rowSums(data[,vars.unambiguousFeedback], na.rm=T)
  dfs2.concentration <- rowSums(data[,vars.concentration], na.rm=T)
  dfs2.senseControl <- rowSums(data[,vars.senseControl], na.rm=T)
  dfs2.lossSelfConsciousness <- rowSums(data[,vars.lossSelfConsciousness], na.rm=T)
  dfs2.transformationTime <- rowSums(data[,vars.transformationTime], na.rm=T)
  dfs2.autotelic <- rowSums(data[,vars.autotelic], na.rm=T)
  dfs2.total <- rowSums(data[,dfs2])
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps,raw.dat,dfs2.challengeSkillBalance,dfs2.actionAwareness,
                  dfs2.clearGoals,dfs2.unambiguousFeedback,dfs2.concentration,
                  dfs2.senseControl,dfs2.lossSelfConsciousness,dfs2.transformationTime,
                  dfs2.autotelic,dfs2.total)
  output <- as.data.frame(output)
  return(output)
}

#************************************************************************
# Brief Dissociative Experiences Scale (DES-B)
# 8 items scored from 0-4, with a max possible score of 32.
#************************************************************************
desb.scoring <- function(data, keep.vars, desb) {
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
  if (missing(desb)) {
    desb <- c(paste('BDES_',1:8,sep=""))
  }
  raw.dat <- data[,desb]
  
  #average score
  desb.average <- rowSums(data[,desb])/length(desb)
  desb.total <- rowSums(data[,desb])
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps, raw.dat, desb.total, desb.average) %>%
    as.data.frame()
  output
}

#****************************************************************************
# mDES (modified Differential Emotion Scale)
mdes.scoring <- function(data,keep.vars,mdes){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
  if (missing(mdes)){
    mdes <- c(paste('mDES_',1:20,sep=""))
  }
  raw.dat <- data[,mdes]
  # Define total scale and all subscales, then calculate scores
  vars.mdesPositive <- c(paste('mDES_',c(1,4,8,11,12,13,14,15,16,19),sep=""))
  vars.mdesNegative <- c(paste('mDES_',c(2,3,5,6,7,9,10,17,18,20),sep=""))
  
  mdes.positive <- rowSums(data[,vars.mdesPositive])/length(vars.mdesPositive)
  mdes.negative <- rowSums(data[,vars.mdesNegative])/length(vars.mdesNegative)
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps,raw.dat,mdes.positive,mdes.negative)
  output <- as.data.frame(output)
  return(output)
}

#****************************************************************************
# DPES (Dispotional Positive Emotion Scale)
dpes.scoring <- function(data,keep.vars,dpes){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
  if (missing(dpes)){
    dpes <- c(paste('DPES_',1:38,sep=""))
  }
  raw.dat <- data[,dpes]
  
  # Define total scale and all subscales, then calculate scores
  vars.dpesTotal      <- c(paste('DPES_',1:38,sep=""))
  vars.dpesJoy        <- c(paste('DPES_',c(2,8,14,26,32,37),sep=""))
  vars.dpesContent    <- c(paste('DPES_',c(1,7,13,19,25),sep=""))
  vars.dpesPride      <- c(paste('DPES_',c(4,17,22,29,35),sep=""))
  vars.dpesLove       <- c(paste('DPES_',c(3,9,15,20,27,33),sep=""))
  vars.dpesCompassion <- c(paste('DPES_',c(10,16,21,28,34),sep=""))
  vars.dpesAmusement  <- c(paste('DPES_',c(5,11,18,23,30),sep=""))
  vars.dpesAwe        <- c(paste('DPES_',c(6,12,24,31,36,38),sep=""))
  
  dpes.total      <- rowSums(data[,vars.dpesTotal])/length(vars.dpesTotal)
  #calculate average of subscales
  dpes.joy        <- rowSums(data[,vars.dpesJoy])/(length(vars.dpesJoy))
  dpes.content    <- rowSums(data[,vars.dpesContent])/(length(vars.dpesContent))
  dpes.pride     <- rowSums(data[,vars.dpesPride])/(length(vars.dpesPride))
  dpes.love       <- rowSums(data[,vars.dpesLove])/(length(vars.dpesLove))
  dpes.compassion <- rowSums(data[,vars.dpesCompassion])/(length(vars.dpesCompassion))
  dpes.amuse      <- rowSums(data[,vars.dpesAmusement])/(length(vars.dpesAmusement))
  dpes.awe        <- rowSums(data[,vars.dpesAwe])/(length(vars.dpesAwe))
  
  # get variables we want to pass through to the output
  # bring participant sub,id,date back into the data frame
  keeps <- data[,keep.vars]
  
  # combine scores and kept variables, make dataframe
  output <- cbind(keeps,raw.dat,dpes.total,dpes.joy,dpes.content,dpes.pride,dpes.love,dpes.compassion,dpes.amuse,dpes.awe)
  output <- as.data.frame(output)
  return(output)
  
}

#****************************************************************************
# 28 Item Dispositional Positive Emotions Scale
dpes28.scoring <- function(data,keep.vars,dpes){
        keep.vars <- c('ID','date')
        if('time.point' %in% colnames(data))
        {
                keep.vars <-c(keep.vars,'time.point')
        }
        if('sub' %in% colnames(data))
        {
                keep.vars <- c('sub',keep.vars)
        }
        
        if (missing(dpes)){
                dpes <- c(paste('DPES_', 1:28, sep=""))
        }
        
        raw.dat <- data[,dpes]
        
        # Define total scale and all subscales, then calculate scores
        vars.dpesEnthusiasm <- c(paste('DPES_', cbind(1,8,15,22), sep=""))
        vars.dpesContent    <- c(paste('DPES_', cbind(2,9,16,23), sep=""))
        vars.dpesPride      <- c(paste('DPES_', cbind(3,10,17,24), sep=""))
        vars.dpesLove       <- c(paste('DPES_', cbind(4,11,18,25), sep=""))
        vars.dpesCompassion <- c(paste('DPES_', cbind(5,12,19,26), sep=""))
        vars.dpesAmusement  <- c(paste('DPES_', cbind(6,13,20,27), sep=""))
        vars.dpesAwe        <- c(paste('DPES_', cbind(7,14,21,28), sep=""))
        
        dpes.enthusiasm <- rowSums(data[,vars.dpesEnthusiasm])
        dpes.content    <- rowSums(data[,vars.dpesContent])
        dpes.pride      <- rowSums(data[,vars.dpesPride])
        dpes.love       <- rowSums(data[,vars.dpesLove])
        dpes.compassion <- rowSums(data[,vars.dpesCompassion])
        dpes.amuse      <- rowSums(data[,vars.dpesAmusement])
        dpes.awe        <- rowSums(data[,vars.dpesAwe])
        dpes.total      <- rowSums(data[,dpes])
        
        
        # get variables we want to pass through to the output
        # bring participant sub,id,date back into the data frame
        keeps <- data[,keep.vars]
        
        # combine scores and kept variables, make dataframe
        output <- cbind(keeps, raw.dat, dpes.enthusiasm, dpes.content, dpes.pride, dpes.love, dpes.compassion, dpes.amuse, dpes.awe, dpes.total)
        output <- as.data.frame(output)
        return(output)
        
}

####################################################################
#Daily Spiritual Experience Scale (DSES)
####################################################################
dses.scoring <- function(data, keep.vars, dses) {
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
  if (missing(dses)) {
    dses <- c(paste('DSES_',1:16,sep=""))
  }
  
  # reverse score all items EXCEPT item 16
  # measure's original scale: 1 = many times a day to 6 = never or almost never
  # however, we want higher scores to indicate higher frequency of spiritual experiences
  # thus, the new scales would be 0 = never or almost never to 5 = many times a day
  # the final item, #16, DOES NOT get reverse scored
  # but scale of #16 is shifted to 0 = not close at all to 3 = as close as possible.
  # item 16 oftentimes reported as a single item and scales are flipped due to preference
  # so, raw data should reflect this change.
  revdses <- paste('DSES_',1:15,sep="")
  data[,revdses] <- 6-data[revdses]
  
  data[,'DSES_16'] <- data[,'DSES_16']-1
  
  raw.dat <- data[,dses]
  
  #average score with item 16 spread across 6 points
  
  #total score
  dses.total <- rowSums(data[,dses])
  
  keeps <- data[,keep.vars]
  output <- cbind(keeps, raw.dat, dses.total) %>%
    as.data.frame()
  output
  
}

################################################################################
# Reflective Dualism Subscale from Mind-Body Relationship Scale 
## (part of belief change measures block in Qualtrics)
################################################################################
dualism.scoring <- function(data, keep.vars, dualism) {
  keep.vars <- c('ID', 'date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <- c(keep.vars, 'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub', keep.vars)
  }
  if (missing(dualism)) {
    dualism <- paste0('Dualism_', 1:12, sep = "")
  }
  raw.dat <- data[,dualism]
  
  vars.rev <- paste0('Dualism_', 10:11)
  data[,vars.rev] <- 0-data[,vars.rev]
  
  # calculate dualism Score
  dualism.total <- rowSums(data[,dualism])/length(dualism)
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps, raw.dat, dualism.total)
  output <- as.data.frame(output)
  return(output)
}

#*************************************************************************
# Drug Use Disorders Identification Test (DUDIT)
#*************************************************************************
dudit.scoring <- function(data, keep.vars, dudit) {
  keep.vars <- c('ID', 'date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <- c(keep.vars, 'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub', keep.vars)
  }
  if (missing(dudit)) {
    dudit <- c(paste('DUDIT_', 1:11, sep = ""))
  }
  raw.dat <- data[,dudit]
  
  # calculate DUDIT Score
  dudit.total <- rowSums(data[,dudit], na.rm = T)
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps, raw.dat, dudit.total)
  output <- as.data.frame(output)
  return(output)
}

#****************************************************************************
# External and Internal Shame Scale (EISS)
#****************************************************************************
eiss.scoring <- function(data,keep.vars,eiss){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
  if (missing(eiss)){
    eiss <- c(paste('EISS_',1:8,sep=""))
  }
  
  raw.dat <- data[,eiss]
  
  vars.eissExternalShame <- paste('EISS_', c(1,3,5,6), sep="")
  vars.eissInternalShame <- paste('EISS_', c(2,4,7,8), sep="")
  
  eiss.external <- rowSums(data[,vars.eissExternalShame])
  eiss.internal <- rowSums(data[,vars.eissInternalShame])
  eiss.global <- rowSums(data[,eiss])
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps, raw.dat, eiss.external, eiss.internal, eiss.global)
  output <- as.data.frame(output)
  return(output)
}

#****************************************************************************
# EDE-Q (Eating Disorders Examination Questionnaire)
edeq.scoring <- function(data,keep.vars,edeq){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }

  if (missing(edeq)){
    edeq <- c(paste('EDEQ_',1:33,sep=""))
  }
  
  raw.dat <- data[,edeq]

  # frequency data
  # subscale scores are mean of each subscale. missing values are fine only if more than half the items have been rated

  vars.edeqRestraint <- c(paste("EDEQ_",1:5,sep=""))
  vars.edeqEatConcern <- c(paste("EDEQ_",c(7,9,19,21,20),sep=""))
  vars.edeqShapeConcern <- c(paste("EDEQ_",c(6,8,23,10,26,27,28,11),sep=""))
  vars.edeqWeightConcern <- c(paste("EDEQ_",c(22,24,8,25,12),sep=""))
  
  edeq.restraint <- ifelse(rowSums(is.na(data[,vars.edeqRestraint])) >= (length(vars.edeqRestraint)/2),
                           NA, rowMeans(data[,vars.edeqRestraint], na.rm=TRUE))
  edeq.eatConcern <- ifelse(rowSums(is.na(data[,vars.edeqEatConcern])) >= (length(vars.edeqEatConcern)/2),
                           NA, rowMeans(data[,vars.edeqEatConcern], na.rm=TRUE))
  edeq.shapeConcern <- ifelse(rowSums(is.na(data[,vars.edeqShapeConcern])) >= (length(vars.edeqShapeConcern)/2),
                           NA, rowMeans(data[,vars.edeqShapeConcern], na.rm=TRUE))
  edeq.weightConcern <- ifelse(rowSums(is.na(data[,vars.edeqWeightConcern])) >= (length(vars.edeqWeightConcern)/2),
                           NA, rowMeans(data[,vars.edeqWeightConcern], na.rm=TRUE))
  
  edeq.global <- (edeq.restraint + edeq.eatConcern + edeq.shapeConcern + edeq.weightConcern)/4
  
  edeq.freqLargeAmount         <- data[,'EDEQ_13']
  edeq.freqLostControl         <- data[,'EDEQ_14']
  edeq.freqDaysOvereat         <- data[,'EDEQ_15']
  edeq.freqVomit               <- data[,'EDEQ_16']
  edeq.freqLaxative            <- data[,'EDEQ_17']
  edeq.freqCompulsiveExercise  <- data[,'EDEQ_18']

  keeps <- data[,keep.vars]
  

  output <- cbind(keeps,raw.dat,edeq.restraint,edeq.eatConcern,edeq.shapeConcern,edeq.weightConcern,
                  edeq.global,edeq.freqLargeAmount,edeq.freqLostControl,edeq.freqDaysOvereat,
                  edeq.freqVomit,edeq.freqLaxative,edeq.freqCompulsiveExercise)
  output <- as.data.frame(output)
  return(output)
}


#****************************************************************************
# EHQ (Edinburgh Handedness Questionnaire)
edinburgh.scoring <- function(data,keep.vars,edinburgh){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }

  if (missing(edinburgh)){
    edinburgh <- c(paste('EHQ_',1:12,sep=""))
  }
  raw.dat <- data[,edinburgh]
  if (max(data[,edinburgh], na.rm = T) > 2) { #check for 1-6 scale
    #1=-2, 2=-1, 3=0, 4=1, 5=2, 6=NA
    data[,edinburgh][data[,edinburgh] == 1] <- -2
    data[,edinburgh][data[,edinburgh] == 2] <- -1
    data[,edinburgh][data[,edinburgh] == 3] <- 0
    data[,edinburgh][data[,edinburgh] == 4] <- 1
    data[,edinburgh][data[,edinburgh] == 5] <- 2
    data[,edinburgh][data[,edinburgh] == 6] <- NA
  }
  ehq.total <- 100*((rowSums(data[,edinburgh],na.rm = T))/
                                      (rowSums(abs(data[,edinburgh]),na.rm=T)))
  #absval.vars <- rowSums(abs(data[,edinburgh]),na.rm = T)
  #handedness <- (hand.vars/absval.vars)*100
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps,raw.dat,ehq.total)
  output <- as.data.frame(output)
  return(output)
}

#****************************************************************************
# Ego Dissolution Inventory
egodis.scoring <- function(data,keep.vars,egodis){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
  if (missing(egodis)){
    egodis <- c(paste('EgoDis_',1:8,sep=""))
  }
  raw.dat <- data[,egodis]
  # calculate scores
  egodis.total <- rowMeans(sapply(data[,egodis],as.numeric))
  
  # get variables we want to pass through to the output
  keeps <- data[,keep.vars]
  
  # combine scores and kept variables, make dataframe
  output <- cbind(keeps,raw.dat,egodis.total)
  output <- as.data.frame(output)
  
  # return
  return(output)
}

#****************************************************************************
# Experiences in Close Relationships (ECR)
ecr.scoring <- function(data,keep.vars,ecr){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }

  if (missing(ecr)){
    ecr <- c(paste('ECR_',1:36,sep=""))
  }
  
  raw.dat <- data[,ecr]

  # reverse scored items
  revecr <- c(paste('ECR_',c(3,15,19,22,25,27,29,31,33,35),sep=""))
  data[,revecr] <-  8-data[,revecr]

  # Define subscales
  vars.ecrAvoidance <- c(paste('ECR_',c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35),sep=""))
  vars.ecrAnxiety <- c(paste('ECR_',c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36),sep=""))

  ecr.avoidance <- rowSums(data[,vars.ecrAvoidance])/length(vars.ecrAvoidance)
  ecr.anxiety <- rowSums(data[,vars.ecrAnxiety])/length(vars.ecrAnxiety)

  keeps <- data[,keep.vars]
  

  output <- cbind(keeps,raw.dat,ecr.avoidance,ecr.anxiety)
  output <- as.data.frame(output)
  return(output)
}

#****************************************************************************
# Experiences in Close Relationships (ECRShort)
ecrshort.scoring <- function(data,keep.vars,ecrshort){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }

  if (missing(ecrshort)){
    ecrshort <- c(paste('ECRShort_',1:12,sep=""))
  }
  
  raw.dat <- data[,ecrshort]

  # reverse scored items
  revecrshort <- c(paste('ECRShort_',c(1,5,8,9),sep=""))
  data[,revecrshort] <-  8-data[,revecrshort]

  # Define subscales
  vars.ecrshortAvoidance <- c(paste('ECRShort_',c(2,4,6,8,10,12),sep=""))
  vars.ecrshortAnxiety <- c(paste('ECRShort_',c(1,3,5,7,9,11),sep=""))

  ecrshort.avoidance <- rowSums(data[,vars.ecrshortAvoidance])/length(vars.ecrshortAvoidance)
  ecrshort.anxiety <- rowSums(data[,vars.ecrshortAnxiety])/length(vars.ecrshortAnxiety)

  keeps <- data[,keep.vars]
  

  output <- cbind(keeps,raw.dat,ecrshort.avoidance,ecrshort.anxiety)
  output <- as.data.frame(output)
  return(output)
}


###################################################################################################
# Eating Disorder Quality of Life Scale (EDQLS)
###################################################################################################

edqls.scoring <- function(data,keep.vars,edqls) {
  
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
  if (missing(edqls)) {
    edqls.all <- c(paste('EDQLS_',1:53,sep=""))
    edqls <- c(paste('EDQLS_',1:40,sep=""))#53 items but only automate scores of first 40
  }
  raw.dat <- data[,edqls.all]
  
  vars.edqlsCognitive <- c(paste("EDQLS_",c(4,20,29),sep=""))  ###
  vars.edqlsEducation <- c(paste("EDQLS_",c(11,17,39),sep=""))
  vars.edqlsFamily <- c(paste("EDQLS_",c(3,16,28),sep=""))
  vars.edqlsRelateOthers <- c(paste("EDQLS_",c(10,19,22),sep=""))
  vars.edqlsFuture <- c(paste("EDQLS_",c(2,21,27),sep=""))
  vars.edqlsAppear <- c(paste("EDQLS_",c(13,25,38),sep=""))  ###
  vars.edqlsLeisure <- c(paste("EDQLS_",c(1,14,24),sep=""))
  vars.edqlsPsychological <- c(paste("EDQLS_",c(7,26,31),sep=""))   ###
  vars.edqlsEmotional <- c(paste("EDQLS_",c(6,30,36),sep=""))
  vars.edqlsValues <- c(paste("EDQLS_",c(5,18,37),sep=""))
  vars.edqlsPhysical <- c(paste("EDQLS_",c(9,32,40),sep=""))
  vars.edqlsEatingDx <- c(paste("EDQLS_",c(8,12,15,23,33,34,35),sep=""))
  
  edqlsrev <- c(paste("EDQLS_",c(2,4,6,8,12,15,16,17,18,19,22,23,25,
                                     29,30,31,34,35,37,38,39,40),sep=""))
  
  #data[,edqls] <- sapply(data[,edqls],as.numeric)
  data[,edqlsrev] = 6-data[,edqlsrev] #reverse score
  
  ## SUBSCALES
  # If more than one NA for a participant for subscales other than eating 
  # (2 for this subscale), leave sum as NA
  
  edqls.cognitive <- ifelse(rowSums(is.na(data[,edqls])) > 4,
                            NA,
                            ifelse(rowSums(is.na(data[,vars.edqlsCognitive])) == 0, 
                                   rowSums(data[,vars.edqlsCognitive]),
                                   ifelse(rowSums(is.na(data[,vars.edqlsCognitive])) == 1,
                                          (rowSums(data[,vars.edqlsCognitive], na.rm = TRUE) + 
                                             rowMeans(data[,vars.edqlsCognitive],na.rm=TRUE)), NA)))
  data[,vars.edqlsCognitive][] <- t(zoo::na.aggregate(t(data[,vars.edqlsCognitive])))
  
  edqls.education <- ifelse(rowSums(is.na(data[,edqls])) > 4,
                            NA,
                            ifelse(rowSums(is.na(data[,vars.edqlsEducation])) == 0, 
                                   rowSums(data[,vars.edqlsEducation]),
                                   ifelse(rowSums(is.na(data[,vars.edqlsEducation])) == 1,
                                          (rowSums(data[,vars.edqlsEducation], na.rm = TRUE) + 
                                             rowMeans(data[,vars.edqlsEducation],na.rm=TRUE)), NA)))
  data[,vars.edqlsEducation][] <- t(zoo::na.aggregate(t(data[,vars.edqlsEducation])))
  
  
  edqls.family <- ifelse(rowSums(is.na(data[,edqls])) > 4,
                         NA,
                         ifelse(rowSums(is.na(data[,vars.edqlsFamily])) == 0, 
                                rowSums(data[,vars.edqlsFamily]),
                                ifelse(rowSums(is.na(data[,vars.edqlsFamily])) == 1,
                                       (rowSums(data[,vars.edqlsFamily], na.rm = TRUE) + 
                                          rowMeans(data[,vars.edqlsFamily],na.rm=TRUE)), NA)))
  data[,vars.edqlsFamily][] <- t(zoo::na.aggregate(t(data[,vars.edqlsFamily])))
  
  
  edqls.relateOthers <- ifelse(rowSums(is.na(data[,edqls])) > 4,
                               NA,
                               ifelse(rowSums(is.na(data[,vars.edqlsRelateOthers])) == 0, 
                                      rowSums(data[,vars.edqlsRelateOthers]),
                                      ifelse(rowSums(is.na(data[,vars.edqlsRelateOthers])) == 1,
                                             (rowSums(data[,vars.edqlsRelateOthers], na.rm = TRUE) + 
                                                rowMeans(data[,vars.edqlsRelateOthers],na.rm=TRUE)), NA)))
  data[,vars.edqlsRelateOthers][] <- t(zoo::na.aggregate(t(data[,vars.edqlsRelateOthers])))
  
  
  edqls.future <- ifelse(rowSums(is.na(data[,edqls])) > 4,
                         NA,
                         ifelse(rowSums(is.na(data[,vars.edqlsFuture])) == 0, 
                                rowSums(data[,vars.edqlsFuture]),
                                ifelse(rowSums(is.na(data[,vars.edqlsFuture])) == 1,
                                       (rowSums(data[,vars.edqlsFuture], na.rm = TRUE) + 
                                          rowMeans(data[,vars.edqlsFuture],na.rm=TRUE)), NA)))
  data[,vars.edqlsFuture][] <- t(zoo::na.aggregate(t(data[,vars.edqlsFuture])))
  
  
  edqls.appear <- ifelse(rowSums(is.na(data[,edqls])) > 4,
                         NA,
                         ifelse(rowSums(is.na(data[,vars.edqlsAppear])) == 0, 
                                rowSums(data[,vars.edqlsAppear]),
                                ifelse(rowSums(is.na(data[,vars.edqlsAppear])) == 1,
                                       (rowSums(data[,vars.edqlsAppear], na.rm = TRUE) + 
                                          rowMeans(data[,vars.edqlsAppear],na.rm=TRUE)), NA)))
  data[,vars.edqlsAppear][] <- t(zoo::na.aggregate(t(data[,vars.edqlsAppear])))
  
  
  edqls.leisure <- ifelse(rowSums(is.na(data[,edqls])) > 4,
                          NA,
                          ifelse(rowSums(is.na(data[,vars.edqlsLeisure])) == 0, 
                                 rowSums(data[,vars.edqlsLeisure]),
                                 ifelse(rowSums(is.na(data[,vars.edqlsLeisure])) == 1,
                                        (rowSums(data[,vars.edqlsLeisure], na.rm = TRUE) + 
                                           rowMeans(data[,vars.edqlsLeisure],na.rm=TRUE)), NA)))
  data[,vars.edqlsLeisure][] <- t(zoo::na.aggregate(t(data[,vars.edqlsLeisure])))
  
  
  edqls.psychological <- ifelse(rowSums(is.na(data[,edqls])) > 4,
                                NA,
                                ifelse(rowSums(is.na(data[,vars.edqlsPsychological])) == 0, 
                                       rowSums(data[,vars.edqlsPsychological]),
                                       ifelse(rowSums(is.na(data[,vars.edqlsPsychological])) == 1,
                                              (rowSums(data[,vars.edqlsPsychological], na.rm = TRUE) + 
                                                 rowMeans(data[,vars.edqlsPsychological],na.rm=TRUE)), NA)))
  data[,vars.edqlsPsychological][] <- t(zoo::na.aggregate(t(data[,vars.edqlsPsychological])))
  
  
  edqls.emotional <- ifelse(rowSums(is.na(data[,edqls])) > 4,
                            NA,
                            ifelse(rowSums(is.na(data[,vars.edqlsEmotional])) == 0, 
                                   rowSums(data[,vars.edqlsEmotional]),
                                   ifelse(rowSums(is.na(data[,vars.edqlsEmotional])) == 1,
                                          (rowSums(data[,vars.edqlsEmotional], na.rm = TRUE) + 
                                             rowMeans(data[,vars.edqlsEmotional],na.rm=TRUE)), NA)))
  data[,vars.edqlsEmotional][] <- t(zoo::na.aggregate(t(data[,vars.edqlsEmotional])))
  
  
  edqls.values <- ifelse(rowSums(is.na(data[,edqls])) > 4,
                         NA,
                         ifelse(rowSums(is.na(data[,vars.edqlsValues])) == 0, 
                                rowSums(data[,vars.edqlsValues]),
                                ifelse(rowSums(is.na(data[,vars.edqlsValues])) == 1,
                                       (rowSums(data[,vars.edqlsValues], na.rm = TRUE) + 
                                          rowMeans(data[,vars.edqlsValues],na.rm=TRUE)), NA)))
  data[,vars.edqlsValues][] <- t(zoo::na.aggregate(t(data[,vars.edqlsValues])))
  
  
  edqls.physical <- ifelse(rowSums(is.na(data[,edqls])) > 4,
                           NA,
                           ifelse(rowSums(is.na(data[,vars.edqlsPhysical])) == 0, 
                                  rowSums(data[,vars.edqlsPhysical]),
                                  ifelse(rowSums(is.na(data[,vars.edqlsPhysical])) == 1,
                                         (rowSums(data[,vars.edqlsPhysical], na.rm = TRUE) + 
                                            rowMeans(data[,vars.edqlsPhysical],na.rm=TRUE)), NA)))
  data[,vars.edqlsPhysical][] <- t(zoo::na.aggregate(t(data[,vars.edqlsPhysical])))
  
  
  edqls.eatingDx <- ifelse(rowSums(is.na(data[,edqls])) > 4,
                           NA,
                           ifelse(rowSums(is.na(data[,vars.edqlsEatingDx])) == 0, 
                                  rowSums(data[,vars.edqlsEatingDx]),
                                  ifelse(rowSums(is.na(data[,vars.edqlsEatingDx])) == 1,
                                         (rowSums(data[,vars.edqlsEatingDx], na.rm = TRUE) + 
                                            rowMeans(data[,vars.edqlsEatingDx],na.rm=TRUE)), 
                                         ifelse(rowSums(is.na(data[,vars.edqlsEatingDx])) == 2,
                                                (rowSums(data[,vars.edqlsEatingDx], na.rm = TRUE) + 
                                                   2*rowMeans(data[,vars.edqlsEatingDx],na.rm=TRUE)), NA))))
  data[,vars.edqlsEatingDx][] <- t(zoo::na.aggregate(t(data[,vars.edqlsEatingDx])))
  
  # TOTAL
  edqls.total <- ifelse(rowSums(is.na(data[,edqls])) <= 4,(rowSums(data[,edqls])), NA) 
  
  
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps,raw.dat,edqls.cognitive, edqls.education, edqls.family,
                  edqls.relateOthers, edqls.future, edqls.appear, edqls.leisure,
                  edqls.psychological, edqls.emotional, edqls.values, edqls.physical,
                  edqls.eatingDx, edqls.total)
  
  output <- as.data.frame(output)
}

###################################################################################################
# Emotion Regulation Questionnaire (ERQ)
###################################################################################################
erq.scoring <- function(data, keep.vars, erq) {
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }

  if (missing(erq)) {
    erq <- c(paste('ERQ_',1:10,sep=""))
  }
  raw.dat <- data[,erq]
  # subscales
  vars.erqCogReappraisal <- c(paste('ERQ_', c(1, 3, 5, 7, 8, 10), sep=""))
  vars.erqEspressiveSuppression <- c(paste('ERQ_', c(2, 4, 6, 9), sep=""))

  # scoring
  erq.cognitiveReappraisal <- rowSums(data[,vars.erqCogReappraisal])/length(vars.erqCogReappraisal)
  erq.expressiveSuppression <- rowSums(data[,vars.erqEspressiveSuppression])/length(vars.erqEspressiveSuppression)

  keeps <- data[,keep.vars]
  
  output <- cbind(keeps, raw.dat,erq.cognitiveReappraisal, erq.expressiveSuppression) %>%
    as.data.frame()
  output
}

###################################################################################################
# Emotional Breakthrough Inventory (EBI)
###################################################################################################
ebi.scoring <- function(data,keep.vars,ebi) {
        keep.vars <- c('ID', 'date')
        if('time.point' %in% colnames(data))
        {
                keep.vars <- c(keep.vars, 'time.point')
        }
        if('sub' %in% colnames(data))
        {
                keep.vars <- c('sub', keep.vars)
        }
        if (missing(ebi)) {
                ebi <- c(paste('EBI_', 1:6, sep = ""))
        }
        
        raw.dat <- data[,ebi]
        
        ebi.total <- rowSums(data[,ebi]) / length(ebi)

        keeps <- data[,keep.vars]
        
        output <- cbind(keeps,raw.dat,ebi.total)
        output <- as.data.frame(output)
        output
}


###################################################################################################
# Emotional State Assessment Tool (ESAT)
###################################################################################################
esat.scoring <- function(data,keep.vars,esat) {
  keep.vars <- c('ID', 'date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <- c(keep.vars, 'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub', keep.vars)
  }
  if (missing(esat)) {
    esat <- c(paste('ESAT_', 1:18, sep = ""))
  }
  
  raw.dat <- data[,esat]
  
  # 2-factor scoring
  vars.esatNegative <- c(paste('ESAT_', c(1:9), sep = ""))
  vars.esatPositive <- c(paste('ESAT_', c(10:18), sep = ""))
  
  esat.negative <- rowSums(data[,vars.esatNegative])/length(vars.esatNegative)
  esat.positive <- rowSums(data[,vars.esatPositive])/length(vars.esatPositive)
  
  # 6-factor scoring
  vars.esatStress <- c(paste('ESAT_', c(1:3), sep = ""))
  vars.esatLethargy <- c(paste('ESAT_', c(4:6), sep = ""))
  vars.esatSadness <- c(paste('ESAT_', c(7:9), sep = ""))
  vars.esatVitality <- c(paste('ESAT_', c(10:12), sep = ""))
  vars.esatCheer <- c(paste('ESAT_', c(13:15), sep = ""))
  vars.esatSerenity <- c(paste('ESAT_', c(16:18), sep = ""))
  
  esat.stress <- rowSums(data[,vars.esatStress])/length(vars.esatStress)
  esat.lethargy <- rowSums(data[,vars.esatLethargy])/length(vars.esatLethargy)
  esat.sadness <- rowSums(data[,vars.esatSadness])/length(vars.esatSadness)
  esat.vitality <- rowSums(data[,vars.esatVitality])/length(vars.esatVitality)
  esat.cheer <- rowSums(data[,vars.esatCheer])/length(vars.esatCheer)
  esat.serenity <- rowSums(data[,vars.esatSerenity])/length(vars.esatSerenity)
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps,raw.dat,esat.stress,esat.lethargy,esat.sadness,esat.vitality,esat.cheer,esat.serenity,esat.negative,esat.positive)
  output <- as.data.frame(output)
  output
}

###################################################################################################
# Fagerstrom Test for Nicotine Dependence (FTND)
###################################################################################################
ftnd.scoring <- function(data, keep.vars, ftnd) {
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
  if (missing(ftnd)) {
    if('FTND_Initial' %in% colnames(data)) {
      ftnd <- c('FTND_Initial', paste('FTND_',1:6,sep=""))
    } else {
      ftnd <- c(paste('FTND_',1:6,sep=""))
    }

  }
  raw.dat <- data[,ftnd]
  
  ftnd.total <- rowSums(data[,ftnd])
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps, raw.dat,ftnd.total)
  output <- as.data.frame(output)
  return(output)
}


###################################################################################################
# Flourishing Measure (FM)
###################################################################################################
flourish.scoring <- function(data,keep.vars,flourish) {
  keep.vars <- c('ID', 'date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <- c(keep.vars, 'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub', keep.vars)
  }
  if (missing(flourish)) {
    flourish <- c(paste('Flourish_', 1:12, sep = ""))
  }
  
  raw.dat <- data[,flourish]
  
  # Flourish and Secure Flourish
  vars.flourishTotal <- c(paste('Flourish_', c(1:10), sep = ""))
  vars.flourishTotalSecure <- c(paste('Flourish_', c(1:12), sep = ""))
  
  flourish.total <- rowSums(data[,vars.flourishTotal])/length(vars.flourishTotal)
  flourish.totalsecure <- rowSums(data[,vars.flourishTotalSecure])/length(vars.flourishTotalSecure)
  
  # 6-Domain Scoring
  vars.flourishHappy <- c(paste('Flourish_', c(1:2), sep = ""))
  vars.flourishHealth <- c(paste('Flourish_', c(3:4), sep = ""))
  vars.flourishPurpose <- c(paste('Flourish_', c(5:6), sep = ""))
  vars.flourishVirtue <- c(paste('Flourish_', c(7:8), sep = ""))
  vars.flourishSocial <- c(paste('Flourish_', c(9:10), sep = ""))
  vars.flourishStable <- c(paste('Flourish_', c(11:12), sep = ""))
  
  flourish.happy <- rowSums(data[,vars.flourishHappy])/length(vars.flourishHappy)
  flourish.health <- rowSums(data[,vars.flourishHealth])/length(vars.flourishHealth)
  flourish.purpose <- rowSums(data[,vars.flourishPurpose])/length(vars.flourishPurpose)
  flourish.virtue <- rowSums(data[,vars.flourishVirtue])/length(vars.flourishVirtue)
  flourish.social <- rowSums(data[,vars.flourishSocial])/length(vars.flourishSocial)
  flourish.stable <- rowSums(data[,vars.flourishStable])/length(vars.flourishStable)
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps,raw.dat,flourish.happy,flourish.health,flourish.purpose,flourish.virtue,flourish.social,flourish.stable,flourish.total,flourish.totalsecure)
  output <- as.data.frame(output)
  return(output)
}

#************************************************************************
# Flow State Scale (FSS2)
#************************************************************************
fss2.scoring <- function(data, keep.vars, fss2) {
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <- c(keep.vars, 'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub', keep.vars)
  }
  if (missing(fss2)) {
    fss2 <- c(paste('FSS2_', 1:36, sep = ""))
  }
  
  raw.dat <- data[,fss2]
  
  vars.challengeSkillBalance <- paste0('FSS2_', c(1,10,19,28))
  vars.actionAwareness <- paste0('FSS2_', c(2,11,20,29))
  vars.clearGoals <- paste0('FSS2_', c(3,12,21,30))
  vars.unambiguousFeedback <- paste0('FSS2_', c(4,13,22,31))
  vars.concentration <- paste0('FSS2_', c(5,14,23,32))
  vars.senseControl <- paste0('FSS2_', c(6,15,24,33))
  vars.lossSelfConsciousness <- paste0('FSS2_', c(7,16,25,34))
  vars.transformationTime <- paste0('FSS2_', c(8,17,26,35))
  vars.autotelic <- paste0('FSS2_', c(9,18,27,36))
  
  fss2.challengeSkillBalance <- rowSums(data[,vars.challengeSkillBalance], na.rm=T)
  fss2.actionAwareness <- rowSums(data[,vars.actionAwareness], na.rm=T)
  fss2.clearGoals <- rowSums(data[,vars.clearGoals], na.rm=T)
  fss2.unambiguousFeedback <- rowSums(data[,vars.unambiguousFeedback], na.rm=T)
  fss2.concentration <- rowSums(data[,vars.concentration], na.rm=T)
  fss2.senseControl <- rowSums(data[,vars.senseControl], na.rm=T)
  fss2.lossSelfConsciousness <- rowSums(data[,vars.lossSelfConsciousness], na.rm=T)
  fss2.transformationTime <- rowSums(data[,vars.transformationTime], na.rm=T)
  fss2.autotelic <- rowSums(data[,vars.autotelic], na.rm=T)
  fss2.total <- rowSums(data[,fss2])
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps,raw.dat,fss2.challengeSkillBalance,fss2.actionAwareness,
                  fss2.clearGoals,fss2.unambiguousFeedback,fss2.concentration,
                  fss2.senseControl,fss2.lossSelfConsciousness,fss2.transformationTime,
                  fss2.autotelic,fss2.total)
  output <- as.data.frame(output)
  return(output)
}



###################################################################################################
# Functional Assessment of Chronic Illness Therapy (FACIT)
###################################################################################################
facit.scoring <- function(data, keep.vars, facit) {
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }

  if(length(grep('FACIT_', colnames(data)) == 12)) {
    if (missing(facit)) {
      facit <- c(paste('FACIT_',1:12,sep=""))
    }
    raw.dat <- data[,facit]
    
    # reverse score
    vars.rev <- c(paste('FACIT_', c(4, 8), sep=""))
    data[,vars.rev] <- 4 - data[,vars.rev]
    
    # subscales
    vars.facitMeaning <- c(paste('FACIT_', c(2, 3, 5, 8), sep=""))
    vars.facitPeace <- c(paste('FACIT_', c(1, 4, 6, 7), sep=""))
    vars.facitFaith <- c(paste('FACIT_', c(9, 10, 11, 12), sep=""))
    
    # count number of items that are not NA's for each subscale per row
    vars.facitMeaning.na <- rowSums(!is.na(data[,vars.facitMeaning]))
    vars.facitPeace.na <- rowSums(!is.na(data[,vars.facitPeace]))
    vars.facitFaith.na <- rowSums(!is.na(data[,vars.facitFaith]))
    vars.facit.na <- rowSums(!is.na(data[,facit]))
    
    # scoring
    facit.meaning <- rowSums(data[,vars.facitMeaning], na.rm = T)/(4*vars.facitMeaning.na)
    facit.peace <- rowSums(data[,vars.facitPeace], na.rm = T)/(4*vars.facitPeace.na)
    facit.faith <- rowSums(data[,vars.facitFaith], na.rm = T)/(4*vars.facitFaith.na)
    facit.spiritualExperience12total <- rowSums(data[,facit], na.rm = T)/(4*vars.facit.na)
    
    keeps <- data[,keep.vars]
    
    output <- cbind(keeps, raw.dat,facit.meaning, facit.peace, facit.faith, facit.spiritualExperience12total) %>%
      as.data.frame()
  } else if (length(grep('FACIT_', colnames(data))) == 23) { # scoring as done from scoring document from facit.org and in line with out 0706 scored it
    if (missing(facit)) {
      facit <- c(paste('FACIT_',1:23,sep=""))
    }
    raw.dat <- data[,facit]
    
    # reverse score
    vars.rev <- c(paste('FACIT_', c(4, 8), sep=""))
    data[,vars.rev] <- 4 - data[,vars.rev]
    
    # subscales
    vars.facitMeaningPeace <- c(paste('FACIT_', 1:8, sep=""))
    vars.facitFaith <- c(paste('FACIT_', 9:12, sep=""))
    vars.facitScore <- paste0('FACIT_', 1:12)
    
    # count number of items that are not NA's for each subscale per row
    vars.facitMeaningPeace.na <- rowSums(!is.na(data[,vars.facitMeaningPeace]))
    vars.facitFaith.na <- rowSums(!is.na(data[,vars.facitFaith]))
    vars.facit.na <- rowSums(!is.na(data[,vars.facitScore]))
    
    # scoring
    facit.meaningPeace <- rowSums(data[,vars.facitMeaning], na.rm = T)/(4*vars.facitMeaning.na)
    facit.faith <- rowSums(data[,vars.facitFaith], na.rm = T)/(4*vars.facitFaith.na)
    facit.spiritualExperience12Total <- rowSums(data[,vars.facitScore], na.rm = T)/(4*vars.facit.na)
    
    keeps <- data[,keep.vars]
    
    output <- cbind(keeps, raw.dat,facit.meaningPeace, facit.faith, facit.spiritualExperience12Total) %>%
      as.data.frame()
  }
  
  return(output)
}



###################################################################################################
# Functional Activities Questionnaire (FAQ)
###################################################################################################
faq.scoring <- function(data,keep.vars,faq){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }

  if (missing(faq)){
    faq <- c(paste('FAQ_',1:10,sep=""))
  }
  raw.dat <- data[,faq]
  # calculate scores
  faq.total <- rowSums(sapply(data[,faq],as.numeric))

  # get variables we want to pass through to the output
  keeps <- data[,keep.vars]
  

  # combine scores and kept variables, make dataframe
  output <- cbind(keeps,raw.dat,faq.total)
  output <- as.data.frame(output)

  # return
  return(output)

}

#****************************************************************************
# FFMQ (Five Facet Mindfulness Questionnaire)
ffmq.scoring <- function(data,keep.vars,ffmq){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
  if (missing(ffmq)){
    ffmq <- c(paste('FFMQ_',1:39,sep=""))
  }
  raw.dat <- data[,ffmq]
  
  revffmq <- c(paste('FFMQ_',c(3,5,8,10,12,13,14,16,17,18,22,23,25,28,30,34,35,38,39),sep=""))
  data[,revffmq] <- 6-data[,revffmq]
  
  vars.ffmqObserve <- c(paste('FFMQ_',c(1,6,11,15,20,26,31,36),sep=""))
  vars.ffmqAwareness <- c(paste('FFMQ_',c(5,8,13,18,23,28,34,38),sep=""))
  vars.ffmqLabeling <- c(paste('FFMQ_',c(2,7,12,16,22,27,32,37),sep=""))
  vars.ffmqNonjudge <- c(paste('FFMQ_',c(3,10,14,17,25,30,35,39),sep=""))
  vars.ffmqNonreactive <- c(paste('FFMQ_',c(4,9,19,21,24,29,33),sep=""))
  
  
  ffmq.observe <- rowSums(data[,vars.ffmqObserve]) / length(vars.ffmqObserve)
  ffmq.awareness <- rowSums(data[,vars.ffmqAwareness]) / length(vars.ffmqAwareness)
  ffmq.labeling <- rowSums(data[,vars.ffmqLabeling]) / length(vars.ffmqLabeling)
  ffmq.nonjudge <- rowSums(data[,vars.ffmqNonjudge]) / length(vars.ffmqNonjudge)
  ffmq.nonreactive <- rowSums(data[,vars.ffmqNonreactive]) / length(vars.ffmqNonreactive)
  ffmq.total <- rowSums(data[,ffmq]) / 39
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps,raw.dat,ffmq.total,ffmq.observe,ffmq.awareness,ffmq.labeling,ffmq.nonjudge,ffmq.nonreactive)
  output <- as.data.frame(output)
  return(output)
  
}

#****************************************************************************
#### Food Preference Questionnaire (FPQ)
fpq.scoring <- function(data,keep.vars,fpq){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }

  if (missing(fpq)){
    fpq <- c(paste('FPQ_',1:72,sep=""))
  }
  raw.dat <- data[,fpq]

  vars.fpq.HF.HS <- c(paste('FPQ_',seq(1,67,by=6),sep=""))
  vars.fpq.HF.HCCHO <- c(paste('FPQ_',seq(2,68,by=6),sep=""))
  vars.fpq.HF.HPRO <- c(paste('FPQ_',seq(3,69,by=6),sep=""))
  vars.fpq.LF.HS <- c(paste('FPQ_',seq(4,70,by=6),sep=""))
  vars.fpq.LF.HCCHO <- c(paste('FPQ_',seq(5,71,by=6),sep=""))
  vars.fpq.LF.HPRO <- c(paste('FPQ_',seq(6,72,by=6),sep=""))

  fpq.highfatHS <- rowMeans(data[,vars.fpq.HF.HS], na.rm = T)
  fpq.highfatHCCHO <- rowMeans(data[,vars.fpq.HF.HCCHO], na.rm = T)
  fpq.highfatHPRO <- rowMeans(data[,vars.fpq.HF.HPRO], na.rm = T)
  fpq.lowfatHS <- rowMeans(data[,vars.fpq.LF.HS], na.rm = T)
  fpq.lowfatHCCHO <- rowMeans(data[,vars.fpq.LF.HCCHO], na.rm = T)
  fpq.lowfatHPRO <- rowMeans(data[,vars.fpq.LF.HPRO], na.rm = T)

  dat.HF <- cbind(data[,vars.fpq.HF.HS],data[,vars.fpq.HF.HCCHO],
                  data[,vars.fpq.HF.HPRO])
  dat.LF <- cbind(data[,vars.fpq.LF.HS],data[,vars.fpq.LF.HCCHO],
                  data[,vars.fpq.LF.HPRO])

  fpq.highfat <- rowMeans(dat.HF,na.rm = T)
  fpq.lowfat <- rowMeans(dat.LF,na.rm = T)
  fpq.fatpref <- (fpq.highfat/fpq.lowfat)*100

  keeps <- data[,keep.vars]
  

  output <- cbind(keeps,raw.dat,fpq.highfatHS,fpq.highfatHCCHO,fpq.highfatHPRO,
                  fpq.lowfatHS,fpq.lowfatHCCHO,
                  fpq.lowfatHPRO,fpq.highfat,fpq.lowfat,fpq.fatpref)
  output <- as.data.frame(output)
  return(output)
}

####################################################################
# Freiburg Mindfulness Inventory (FMI)
####################################################################
fmi.scoring <- function(data, keep.vars, fmi) {
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }

  if (missing(fmi)) {
    fmi <- c(paste('FMI_',1:14,sep=""))
  }
  raw.dat <- data[,fmi]

  # reverse score item 13
  data[,'FMI_13'] <- 5-data[,'FMI_13']

  # calculate score
  fmi.total <- rowSums(data[,fmi])

  keeps <- data[,keep.vars]

  output <- cbind(keeps, raw.dat, fmi.total) %>%
    as.data.frame()
  output
}

#************************************************************************
# Faith Maturity Scale (FMS)
#************************************************************************
fms.scoring <- function(data, keep.vars, fms) {
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <- c(keep.vars, 'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub', keep.vars)
  }
  if (missing(fms)) {
    fms <- c(paste('FMS_', 1:12, sep = ""))
  }
  
  raw.dat <- data[,fms]
  
  vars.fms.vertical <- c(paste("FMS_", c(2,5,6,9,10,11,12), sep = ""))
  vars.fms.horizontal <- c(paste("FMS_", c(1,3,4,7,8), sep = ""))
  vars.cpcr.vertical <- c(paste("FMS_", c(2,6,10,11), sep = ""))
  vars.cpcr.horizontal <- c(paste("FMS_", c(3,4,7,8), sep = ""))
  
  fms.vertical <- rowSums(data[, vars.fms.vertical])
  fms.horizontal <- rowSums(data[, vars.fms.horizontal])
  fms.cpcrVertical <- rowSums(data[, vars.cpcr.vertical])
  fms.cpcrHorizontal <- rowSums(data[, vars.cpcr.horizontal])
  fms.total <- rowSums(data[, fms])
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps,raw.dat,fms.vertical,fms.horizontal,fms.cpcrVertical,fms.cpcrHorizontal,fms.total)
  output <- as.data.frame(output)
  return(output)
}

#************************************************************************
# Fatigue Severity Scale (FSS)
#************************************************************************
fss.scoring <- function(data, keep.vars, fss) {
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <- c(keep.vars, 'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub', keep.vars)
  }
  if (missing(fss)) {
    fss <- c(paste('FSS_', 1:9, sep = ""))
    fss.scored <- c(paste('FSS_', 1:9, sep = "")) #so that all items are in output but only 1:9 are scored
    if('FSS_10' %in% colnames(data)) {
      fss <- c(paste('FSS_', 1:10, sep=""))
    }
  }
  
  raw.dat <- data[,fss]
  
  fss.total <- rowSums(data[,fss.scored])
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps,raw.dat,fss.total)
  output <- as.data.frame(output)
  return(output)
}

#************************************************************************
# Fagerstrom Test of Cigarette Dependence (FTCD)
#************************************************************************
ftcd.scoring <- function(data, keep.vars, ftcd) {
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <- c(keep.vars, 'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub', keep.vars)
  }
  if (missing(ftcd)) {
    ftcd <- c(paste('FTCD_', 1:6, sep = ""))
  }
  
  raw.dat <- data[,ftcd]
  
  ftcd.total <- rowSums(data[,ftcd])
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps,raw.dat,ftcd.total)
  output <- as.data.frame(output)
  return(output)
}

#******************************************************************
# Generalized Anxiety Disorder-7 (GAD-7)
# 7 items, Scored 0-3, Total Sum Range 0-21
#******************************************************************
gad7.scoring <- function(data, keep.vars, gad) {
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <- c(keep.vars, 'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub', keep.vars)
  }
  
  if (missing(gad)) {
    gad <- c(paste('GAD_', 1:7, sep=""))
  }
  
  raw.dat <- data[,gad]
  
  # calculate score
  gad.total <- rowSums(data[,gad])
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps, raw.dat, gad.total)
  output <- as.data.frame(output)
  return(output)
}
#************************************************************************
# General Self-Efficacy Scale (GSES)
#************************************************************************
gses.scoring <- function(data, keep.vars, gses) {
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <- c(keep.vars, 'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub', keep.vars)
  }
  if (missing(gses)) {
    gses <- c(paste('GSES_', 1:10, sep = ""))
  }
  
  raw.dat <- data[,gses]
  
  gses.total <- rowSums(data[,gses])
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps,raw.dat,gses.total)
  output <- as.data.frame(output)
  return(output)
}
#*#**********************************************************************
# Graded Chronic Pain scale - Revised (GCPS-R)
#************************************************************************
gcpsr.scoring <- function(data, keep.vars, gcpsr) {
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <- c(keep.vars, 'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub', keep.vars)
  }
  if (missing(gcpsr)) {
    gcpsr <- c(paste('GCPSR_', 1:6, sep = ""))
  }
  
  raw.dat <- data[,gcpsr]
  
  vars.peg <- c(paste("GCPSR_", 3:5, sep = ""))
  gcpsr.peg <- rowSums(data[,vars.peg])
  
  gcpsr.score <- ifelse(data[,c("GCPSR_1")] <= 1, "Grade 0", 
                        ifelse(data[,c("GCPSR_2")] >= 2, "Grade 3",
                               ifelse(gcpsr.peg >= 12, "Grade 2", "Grade 1")))
  
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps,raw.dat, gcpsr.peg,gcpsr.score)
  output <- as.data.frame(output)
  
  return(output)
}

#*#**********************************************************************
# Geriatric Depression Scale (Short Form (15-item))
#************************************************************************
gds.scoring <- function(data, keep.vars, gds) {
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <- c(keep.vars, 'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub', keep.vars)
  }
  if (missing(gds)) {
    gds <- c(paste('GDS_', 1:15, sep = ""))
  }
  
  raw.dat <- data[,gds]
  
  revgds <- paste0('GDS_',c(1,5,7,11,13))
  data[,revgds] <-  1-data[,revgds]
  
  
  gds.total <- rowSums(data[,gds])
  

  keeps <- data[,keep.vars]
  
  output <- cbind(keeps,raw.dat,gds.total)
  output <- as.data.frame(output)
  
  return(output)
}

#****************************************************************************
# GQ (Gratitude Questionnaire)
gratitude.scoring <- function(data,keep.vars,gratitude){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }

  if (missing(gratitude)){
    gratitude <- c(paste('GQ_',1:6,sep=""))
  }
  raw.dat <- data[,gratitude]

  # reverse scored items
  revgratitude <- c(paste('GQ_',c(3,6),sep=""))
  data[,revgratitude] <-  8-data[,revgratitude]

  vars.gratitude <- c(paste('GQ_',1:6,sep=""))

  gratitude.total <- rowSums(data[,vars.gratitude])

  keeps <- data[,keep.vars]

  output <- cbind(keeps,raw.dat,gratitude.total)
  output <- as.data.frame(output)
  return(output)
}

#************************************************************************
# General Symptom Questionnaire-30 (GSQ-30)
#************************************************************************
gsq.scoring <- function(data, keep.vars, gsq) {
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <- c(keep.vars, 'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub', keep.vars)
  }
  
  if (missing(gsq)) {
    gsq <- c(paste('GSQ_', 1:30, sep = ""), 'GSQ_31', paste('GSQ_32_', 1:7, sep=""))
    gsq.scored <- c(paste('GSQ_', 1:30, sep = ""))
  }
  
  raw.dat <- data[,gsq]
  
  vars.viral.like <- c(paste("GSQ_", c(2,3,4,6,23), sep = ""))
  vars.pain.fatigue <- c(paste("GSQ_", c(5,7:15), sep = ""))
  vars.neurological <- c(paste("GSQ_", c(1,16:22,24,25), sep = ""))
  vars.neuropsychiatric <- c(paste("GSQ_", 26:30, sep = ""))
  
  gsq.viralLike <- rowSums(data[,vars.viral.like])/length(vars.viral.like)
  gsq.painFatigue <- rowSums(data[,vars.pain.fatigue])/length(vars.pain.fatigue)
  gsq.neurological <- rowSums(data[,vars.neurological])/length(vars.neurological)
  gsq.neuroPsychiatric <- rowSums(data[,vars.neuropsychiatric])/length(vars.neuropsychiatric)
  gsq.total <- rowSums(data[,gsq.scored])/length(gsq.scored)
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps,raw.dat,gsq.viralLike,gsq.painFatigue,gsq.neurological,gsq.neuroPsychiatric,gsq.total)
  output <- as.data.frame(output)
  return(output)
}


#****************************************************************************
# HADS (Hospital Anxiety & Depression Scale)
hads.scoring <- function(data, keep.vars, hads){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }

  if (missing(hads)){
    hads <- c(paste('HADS_',1:14,sep=""))
  }
  raw.dat <- data[,hads]

  vars.hadsDepression <- c(paste('HADS_',c(2,4,6,8,10,12,14),sep=""))
  vars.hadsAnxiety <- c(paste('HADS_',c(1,3,5,7,9,11,13),sep=""))

  hads.depression <- rowSums(data[,vars.hadsDepression])
  hads.anxiety <- rowSums(data[,vars.hadsAnxiety])
  hads.total <- rowSums(data[,hads])

  keeps <- data[,keep.vars]

  output <- cbind(keeps,raw.dat,hads.depression,hads.anxiety, hads.total)
  output <- as.data.frame(output)
  return(output)
}

#************************************************************************
# Hamilton Anxiety Rating Scale (HAM-A)
# 14 questions on a scale of 0 - 4
#************************************************************************
hama.scoring <- function(data, keep.vars, hama) {
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
  if (missing(hama)) {
    hama <- c(paste('HAMA_',1:14,sep=""))
  }
  raw.dat <- data[,hama]
  
  hama.total <- rowSums(data[,hama])
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps, raw.dat, hama.total) %>%
    as.data.frame()
  output
}


#****************************************************************************
# HMS (Hood Mysticism Scale)
mscale.scoring <- function(data,keep.vars,mscale){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  if (missing(mscale)){
    mscale <- c(paste('MScale_',1:32,sep=""))
  }
  
  if (min(data[,mscale], na.rm = T) < 0){ # if score range is -4 to 4, then convert scale to 1-9, na.rm = T because min will set NA as lowest value if present
    data[,mscale] <- 5 + data[,mscale]
    # stop("Data for Hood Mysticism Scale are out of range. This measure should be scored from 1 to 9.", 
    #      call. = T, domain = NULL)
  }
  
  raw.dat <- data[,mscale]
  
  #data[,mscale] <- 5+data[,mscale] # if score range is -4 to 4, then convert scale to 1-9
  mscalerev <- c(paste('MScale_',c(2,6,15,21,24,27,8,10,28,30,7,9,14,16,25,26),sep=""))
  data[,mscalerev] <- 10-data[,mscalerev] #reverse score

  #vars.introvertiveMyst <- c(paste('HoodMScale_',c(1:4,6,11,15,21,23,24,27,32),sep=""))
  #vars.ineffibilityMyst <- c(paste('HoodMScale_',c(2,21,23,32),sep=""))
  #vars.egolossMyst <- c(paste('HoodMScale_',c(3,4,6,24),sep=""))
  #vars.timelessMyst <- c(paste('HoodMScale_',c(1,11,15,27),sep=""))
  #vars.extrovertiveMyst <- c(paste('HoodMScale_',c(8,10,12,19,28:31),sep=""))
  #vars.innersubjMyst <- c(paste('HoodMScale_',c(8,10,29,31),sep=""))
  #vars.unityMyst <- c(paste('HoodMScale_',c(12,19,28,30),sep=""))
  #vars.interpretationMyst <- c(paste('HoodMScale_',c(5,7,9,13,14,16:18,20,22,25,26),sep=""))
  #vars.posaffectMyst <- c(paste('HoodMScale_',c(5,7,18,25),sep=""))
  #vars.sacrednessMyst <- c(paste('HoodMScale_',c(9,14,20,22),sep=""))
  #vars.noeticMyst <- c(paste('HoodMScale_',c(13,16,17,26),sep=""))

  vars.extrovertiveMyst <- c(paste('MScale_',c(8,10,12,19,28,29,30,31),sep=""))
  vars.introvertiveMyst <- c(paste('MScale_',c(1,2,3,4,6,11,15,21,23,24,27,32),sep=""))
  vars.interpretationMyst <- c(paste('MScale_',c(5,7,9,13,14,16,17,18,20,22,25,26),sep=""))
  vars.mscaleTotal <- c(paste('MScale_',1:32,sep=""))

  mscale.introvertive <- rowSums(data[,vars.introvertiveMyst])
  mscale.extrovertive <- rowSums(data[,vars.extrovertiveMyst])
  mscale.interpretation <- rowSums(data[,vars.interpretationMyst])
  mscale.total <- rowSums(data[,vars.mscaleTotal])

  mscale.introvertive.pctmax <- rowSums(data[,vars.introvertiveMyst])/(length(vars.introvertiveMyst)*9)
  mscale.extrovertive.pctmax <- rowSums(data[,vars.extrovertiveMyst])/(length(vars.extrovertiveMyst)*9)
  mscale.interpretation.pctmax <- rowSums(data[,vars.interpretationMyst])/(length(vars.interpretationMyst)*9)
  mscale.total.pctmax <- rowSums(data[,vars.mscaleTotal])/(length(vars.mscaleTotal)*9)

  keeps <- data[,keep.vars]
  
  output <- cbind(keeps,raw.dat, mscale.introvertive, mscale.introvertive.pctmax,
                  mscale.extrovertive, mscale.extrovertive.pctmax,
                  mscale.interpretation, mscale.interpretation.pctmax,
                  mscale.total, mscale.total.pctmax)
  output <- as.data.frame(output)
  return(output)
}

#****************************************************************************
# HRS (Hallucinogen Rating Scale)
hrs.scoring <- function(data,keep.vars,hrs){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }

  if (missing(hrs)){
    hrs = c('HRS_1','HRS_2','HRS_2a','HRS_3','HRS_3a','HRS_4','HRS_4a','HRS_5','HRS_5a','HRS_6','HRS_7',
            'HRS_7a','HRS_8','HRS_9', 'HRS_9a', paste('HRS_', 10:22, sep = ''),'HRS_22a',paste('HRS_', 23:27, sep = ''),'HRS_27a','HRS_27b',
            'HRS_28','HRS_29','HRS_30','HRS_31','HRS_31a','HRS_32','HRS_33','HRS_34','HRS_34a','HRS_35','HRS_36',
            'HRS_36a',paste('HRS_', 37:40, sep = ''),'HRS_40a','HRS_41','HRS_41a',paste('HRS_', 42:50, sep = ''),
            'HRS_50a','HRS_51','HRS_51a','HRS_52','HRS_52a','HRS_53','HRS_54','HRS_55','HRS_55a','HRS_56',
            'HRS_57','HRS_58','HRS_59','HRS_59a','HRS_60','HRS_60a',paste('HRS_', 61:66, sep = ''),'HRS_66a',
            'HRS_66b','HRS_67','HRS_68','HRS_68a',paste('HRS_', 69:76, sep = ''),'HRS_76a','HRS_77','HRS_78',
            'HRS_79','HRS_80','HRS_80a','HRS_81','HRS_81a','HRS_82','HRS_82a','HRS_83','HRS_84','HRS_85','HRS_86',
            'HRS_86a','HRS_87','HRS_88','HRS_88a','HRS_89','HRS_90','HRS_90a',paste('HRS_', 91:99, sep = ''),'HRS_99a')
  }
  raw.dat <- data[,hrs]

  revhrs <- c(paste('HRS_',c(32,91:96),sep=""))
  data[,revhrs] <- abs(4-data[,revhrs])

  vars.hrsIntensity <- c(paste('HRS_',c(1,97:99),sep="")) #range 0-4.25
  vars.hrsSomaesthesia <- c(paste('HRS_',c(2:11,16,20,21),sep="")) #range 0-4 for rest
  vars.hrsAffect <- c(paste('HRS_',c(25,26,29,30:33,37,40:45,47:49),sep=""))
  vars.hrsPerception <- c(paste('HRS_',c(17,22,52,54,55,57:62,64:69),sep=""))
  vars.hrsCognition <- c(paste('HRS_',c(71,74:77,80:83,85,86,88),sep=""))
  vars.hrsVolition <- c(paste('HRS_',c(89:96),sep=""))

  # Calculate mean scores for each subscale
  hrs.intensity <- rowMeans(data[, vars.hrsIntensity])
  hrs.somaesthesia <- rowMeans(data[, vars.hrsSomaesthesia])
  hrs.affect <- rowMeans(data[, vars.hrsAffect])
  hrs.perception <- rowMeans(data[, vars.hrsPerception])
  hrs.cognition <- rowMeans(data[, vars.hrsCognition])
  hrs.volition <- rowMeans(data[, vars.hrsVolition])

  keeps <- data[,keep.vars]

  output <- cbind(keeps,raw.dat,hrs.intensity,hrs.somaesthesia,hrs.affect,
                  hrs.perception,hrs.cognition,hrs.volition)
  output <- as.data.frame(output)
  return(output)
}


#****************************************************************************
# IDS (Inventory for Depressive Symptoms)
# 30-item QA that includes all QIDS measures (16 items)
# calculate sum of all QIDS items and remaining 14 IDS items to score

ids.scoring <- function(data,keep.vars,qids,ids){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
  if (missing(ids)){
    ids.vars <- c(paste0('IDS_',1:4),'IDS_4a','IDS_4b',paste0('IDS_',5:14))
  }
  if (missing(qids)){
    qids.vars <- c(c(paste('QIDS_',1:5,sep="")),'QIDS_5a','QIDS_6','QIDS_7','QIDS_7a',c(paste('QIDS_',8:16,sep="")))
  }
  
  raw.dat <- data[,ids.vars]

  # subtract 1 from all ids items to make scale 0-3
  idspartial <- c(paste('IDS_',1:14,sep=""))
  data[,idspartial] <- sapply(data[,idspartial],as.numeric)
  data[,idspartial] <- data[,idspartial]-1

  vars.ids <- c(c(paste('QIDS_',1:16,sep="")),c(paste('IDS_',1:14,sep="")))
  data[,vars.ids] <- sapply(data[,vars.ids],as.numeric)

  ids.total <- rowSums(data[,vars.ids],na.rm=T)

  keeps <- data[,keep.vars]
  output <- cbind(keeps,raw.dat, ids.total)
  output <- as.data.frame(output)
  return(output)

}

#****************************************************************************
# Inclusion of Self (IOS)
ios.scoring <- function(data,keep.vars,ios){
  keep.vars <- c('ID','date')
  
  raw.colnames <- colnames(data)
  
  if (missing(ios)){
    ios <- paste0('IOS_', 1:5 )
  }
  
  raw.data <- data[,ios]
  
  ios.total <- rowSums(data[, ios])/(length(ios))
  keeps <- data[,keep.vars]
  
  
  output <- cbind(keeps, raw.data, ios.total) 
  output <- as.data.frame(output)
  return(output)
  
}

####################################################################
#20-Item Mini-IPIP
####################################################################
ipip.scoring <- function(data, keep.vars, ipip) {
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
  if (missing(ipip)) {
    ipip <- c(paste('IPIPMini_',1:20,sep=""))
  }
  raw.dat <- data[,ipip]
  
  # reverse scoring
  vars.rev <- c(paste('IPIPMini_', c(6, 7, 8, 9, 10, 15, 16, 17, 18, 19, 20), sep=""))
  data[,vars.rev] <- 6 - data[,vars.rev]
  
  # subscales
  vars.ipipExtraversion <- c(paste('IPIPMini_', c(1, 6, 11, 16), sep=""))
  vars.ipipAgreeableness <- c(paste('IPIPMini_', c(2, 7, 12, 17), sep=""))
  vars.ipipConscientiousness <- c(paste('IPIPMini_', c(3, 8, 13, 18), sep=""))
  vars.ipipNeuroticism <- c(paste('IPIPMini_', c(4, 9, 14, 19), sep=""))
  vars.ipipIntellect <- c(paste('IPIPMini_', c(5, 10, 15, 20), sep=""))
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps, raw.dat, vars.ipipExtraversion, vars.ipipAgreeableness, 
                  vars.ipipConscientiousness, vars.ipipNeuroticism, vars.ipipIntellect) %>%
    as.data.frame()
  output
}

#****************************************************************************
# Psychological Insight Questionnaire (PIQ)
psychinsight.scoring <- function(data,keep.vars,insight){
  keep.vars <- intersect(c('ID','date','time.point','sub'), colnames(data))

  raw.colnames <- colnames(data)
  raw.dat <- c()
  psychinsight.scores <- c()

  # for 28 item need to separate and only score
  # the items used in the validated 23 item
  if(length(grep("PsychInsight_", raw.colnames)) == 23){
    if (missing(insight)){
      insight <- c(paste('PsychInsight_',1:23,sep=""))
    }
    raw.dat <- data[,insight]

    #subscales
    #Avoidance and Maladaptive Patterns
    vars.psychinsightAmp <- c(paste('PsychInsight_', c(1,3,5,7,9,10,12,14,15,17,18,19,21,23),sep=""))
    #Goals and Adaptive Patterns (GAP)
    vars.psychinsightGap <- c(paste('PsychInsight_',c(2,4,6,8,11,13,16,20,22),sep=""))

    #scores
    psychinsight.amp <- rowSums(data[,vars.psychinsightAmp])/length(vars.psychinsightAmp)
    psychinsight.gap <- rowSums(data[,vars.psychinsightGap])/length(vars.psychinsightGap)
    psychinsight.total <- rowSums(data[,insight])/length(insight)
    psychinsight.scores <- cbind(psychinsight.amp, psychinsight.gap, psychinsight.total)

  } else if (length(grep("PsychInsight_", raw.colnames)) == 28) {
    if (missing(insight)) {
      insight <- c(paste('PsychInsight_',1:28,sep=""))
    }
    raw.dat <- data[,insight]

    #subscales
    vars.psychinsightAmp <- c(paste('PsychInsight_',c(1,2,3,4,5,8,10,11,14,16,17,19,20,24),sep=""))
    vars.psychinsightGap <- c(paste('PsychInsight_',c(6,7,12,13,15,18,23,26,28),sep=""))
    vars.total <- c(vars.psychinsightAmp, vars.psychinsightGap)
    #scores
    psychinsight.amp <- rowSums(data[,vars.psychinsightAmp])/length(vars.psychinsightAmp)
    psychinsight.gap <- rowSums(data[,vars.psychinsightGap])/length(vars.psychinsightGap)
    psychinsight.total <- rowSums(data[,vars.total])/length(vars.total)
    psychinsight.scores <- cbind(psychinsight.amp, psychinsight.gap, psychinsight.total)
  }

  keeps <- data[,keep.vars]
  
  output <- cbind(keeps, raw.dat, psychinsight.scores)

  output <- as.data.frame(output)
  return(output)
}

#*********************************************************************
# Insomnia Severity Index (ISI)
#*********************************************************************
isi.scoring <- function(data, keep.vars, isi) {
  keep.vars <- c('ID', 'date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <- c(keep.vars, 'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub', keep.vars)
  }
  
  if (missing(isi)) {
    isi <- c(paste('ISI_', 1:7, sep=""))
  }
  raw.dat <- data[,isi]
  
  isi.total <- rowSums(data[,isi])
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps, raw.dat, isi.total)
  output <- as.data.frame(output)
  return(output)
}

####################################################################
#Inspiration Scale
####################################################################
inspiration.scoring <- function(data, keep.vars, insp) {
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }

  if (missing(insp)) {
    insp <- c('Inspiration_1a', 'Inspiration_1b', 'Inspiration_2a', 'Inspiration_2b',
              'Inspiration_3a', 'Inspiration_3b', 'Inspiration_4a', 'Inspiration_4b')
  }

  raw.dat <- data[,insp]

  # define subscales
  vars.inspirationFrequency <- c('Inspiration_1a', 'Inspiration_2a', 'Inspiration_3a', 
                                 'Inspiration_4a')
  vars.inspirationIntensity <- c('Inspiration_1b', 'Inspiration_2b','Inspiration_3b', 
                                  'Inspiration_4b')

  # calculate scores
  inspiration.frequency<- rowSums(data[,vars.inspirationFrequency])
  inspiration.intensity <- rowSums(data[,vars.inspirationIntensity])
  inspiration.total <- rowSums(data[,insp])

  keeps <- data[,keep.vars]

  output <- cbind(keeps, raw.dat, inspiration.frequency, inspiration.intensity, inspiration.total) %>%
    as.data.frame()
  output
}

####################################################################
# Interpersonal Reactivity Index (IRI)
#calculate sum??
####################################################################
iri.scoring <- function(data, keep.vars, iri) {
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }

  if (missing(iri)) {
    iri <- c(paste('IRI_',1:28,sep=""))
  }

  raw.dat <- data[,iri]

  # reverse score items
  vars.rev <- c(paste('IRI_', c(3, 4, 7, 12, 13, 14, 15, 18, 19), sep=""))
  data[,vars.rev] <- 4-data[,vars.rev]
  
  # define subscales
  vars.iriPerspectiveTaking <- c(paste('IRI_', c(3, 8, 11, 15, 21, 25, 28), sep=""))
  vars.iriFantasy <- c(paste('IRI_', c(1, 5, 7, 12, 16, 23, 26), sep=""))
  vars.iriEmpatheticConcern <- c(paste('IRI_', c(2, 4, 9, 14, 18, 20, 22), sep=""))
  vars.iriPersonalDistress <- c(paste('IRI_', c(6, 10, 13, 17, 19, 24, 27), sep=""))

  # calculate average scores
  iri.perspectiveTaking <- rowSums(data[,vars.iriPerspectiveTaking])/length(vars.iriPerspectiveTaking)
  iri.fantasy <- rowSums(data[,vars.iriFantasy])/length(vars.iriFantasy)
  iri.empathicConcern <- rowSums(data[,vars.iriEmpatheticConcern])/length(vars.iriEmpatheticConcern)
  iri.personalDistress <- rowSums(data[,vars.iriPersonalDistress])/length(vars.iriPersonalDistress)
  iri.total <- rowSums(data[,iri])/length(iri)

  keeps <- data[,keep.vars]

  output <- cbind(keeps, raw.dat, iri.perspectiveTaking, iri.fantasy , iri.empathicConcern,
                  iri.personalDistress, iri.total) %>%
    as.data.frame()
  output
}

#****************************************************************************
# Inclusion of Self (IOS)
ios.scoring <- function(data,keep.vars,ios){
        keep.vars <- c('ID','date')
        
        raw.colnames <- colnames(data)
        
        if (missing(ios)){
                ios <- paste0('IOS_', 1:5 )
        }
        
        raw.data <- data[,ios]
        
        ios.scored <- rowSums(data[, ios])/(length(ios))
        keeps <- data[,keep.vars]
        
        
        output <- cbind(keeps, raw.data, ios.scored) 
        output <- as.data.frame(output)
        return(output)
        
}

####################################################################
# Life Attitude Profile Revised (LAP-R)
####################################################################
lapr.scoring <- function(data, keep.vars, lapr) {
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
  if(length(grep('LAPR_',colnames(data))) == 15) {
    if (missing(lapr)) {
      lapr <- c(paste('LAPR_',1:15,sep=""))
    }
    
    raw.dat <- data[,lapr]
    
    # define subscales
    vars.laprCoherence <- c(paste('LAPR_', c(1,3,5,8,10,12:14), sep=""))
    vars.laprDeathAcceptance <- c(paste('LAPR_', c(2,4,6,7,9,11,15), sep=""))
    
    # calculate scores
    lapr.coherence <- rowSums(data[,vars.laprCoherence])
    lapr.deathAcceptance <- rowSums(data[,vars.laprDeathAcceptance])
    
    keeps <- data[,keep.vars]
    
    output <- cbind(keeps, raw.dat, lapr.coherence, lapr.deathAcceptance) %>%
      as.data.frame()
    
  } else if(length(grep('LAPR',colnames(data))) == 48) {
    if (missing(lapr)) {
      lapr <- c(paste('LAPR_',1:48,sep=""))
    }
    
    raw.dat <- data[,lapr]
    
    # define subscales
    vars.laprCoherence <- c(paste('LAPR_', c(7, 12, 16, 27, 29, 35, 38, 46), sep=""))
    vars.laprDeathAcceptance <- c(paste('LAPR_', c(8, 15, 22, 25, 28, 32, 47), sep=""))
    
    # calculate scores
    lapr.coherence <- rowSums(data[,vars.laprCoherence])
    lapr.deathAcceptance <- rowSums(data[,vars.laprDeathAcceptance])
    
    keeps <- data[,keep.vars]
    
    output <- cbind(keeps, raw.dat, lapr.coherence, lapr.deathAcceptance) %>%
      as.data.frame()
  }
  return(output)
}
#************************************************************************
# Revised Life Orientation Test (LOT-R)
#************************************************************************
lotr.scoring <- function(data, keep.vars, lotr) {
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <- c(keep.vars, 'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub', keep.vars)
  }
  if (missing(lotr)) {
    lotr <- c(paste('LOTR_', 1:10, sep = ""))
  }
  
  raw.dat <- data[,lotr]
  
  rev.items <- c(paste("LOTR_", c(3,7,9), sep = ""))
  data[,rev.items] <- 4-data[,rev.items]
  
  vars.score <- c(paste("LOTR_", c(1,3,4,7,9,10), sep = ""))
  
  lotr.total <- rowSums(data[,vars.score])
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps,raw.dat,lotr.total)
  output <- as.data.frame(output)
  return(output)
}
#****************************************************************************
# Marijuana Craving Questionnaire (MCQ-SF)
#****************************************************************************
mcq.scoring <- function(data, keep.vars, mcq) {
  keep.vars <- c('ID', 'date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <- c(keep.vars, 'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub', keep.vars)
  }
  if (missing(mcq)) {
    mcq <- c(paste('MCQ_', 1:12, sep = ""))
  }
  
  vars.compulsivity <- c(paste('MCQ_', c(2,7,10), sep = ""))
  vars.emotionality <- c(paste('MCQ_', c(4,6,9), sep = ""))
  vars.expectancy <- c(paste('MCQ_', c(5,11,12), sep = ""))
  vars.purposefulness <- c(paste('MCQ_', c(1,3,8), sep = ""))
  
  raw.dat <- data[,mcq]
  
  mcq.compulsivity <- rowSums(data[,vars.compulsivity])
  mcq.emotionality <- rowSums(data[,vars.emotionality])
  mcq.expectancy <- rowSums(data[,vars.expectancy])
  mcq.purposefulness <- rowSums(data[,vars.purposefulness])
  mcq.total <- rowSums(data[,mcq])
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps, raw.dat, mcq.compulsivity, mcq.emotionality, mcq.expectancy, mcq.purposefulness, mcq.total)
  output <- as.data.frame(output)
  return(output)
}
#****************************************************************************
# MAIA (Multiple Assessment of Interoceptive Awaresness)
maia.scoring <- function(data,keep.vars,maia){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }

  if (missing(maia)){
    maia <- c(paste('MAIA_',1:32,sep=""))
  }
  raw.dat <- data[,maia]

  # reverse scored items
  revmaia <- c(paste('MAIA_',5:9,sep=""))
  data[,revmaia] <-  5-data[,revmaia]

  # Define total scale and all subscales, then calculate scores
  vars.maiaTotal            <- c(paste('MAIA_',1:32,sep=""))
  vars.maiaNotice           <- c(paste('MAIA_',1:4,sep=""))
  vars.maiaNotDistracting   <- c(paste('MAIA_',5:7,sep=""))
  vars.maiaNotWorrying      <- c(paste('MAIA_',8:10,sep=""))
  vars.maiaAttnRegulation   <- c(paste('MAIA_',11:17,sep=""))
  vars.maiaEmoAware         <- c(paste('MAIA_',18:22,sep=""))
  vars.maiaSelfRegulation   <- c(paste('MAIA_',23:26,sep=""))
  vars.maiaBodyListen       <- c(paste('MAIA_',27:29,sep=""))
  vars.maiaTrust            <- c(paste('MAIA_',30:32,sep=""))

  maia.notice <- rowSums(data[,vars.maiaNotice])/(length(vars.maiaNotice))
  maia.distract <- rowSums(data[,vars.maiaNotDistracting])/(length(vars.maiaNotDistracting))
  maia.worry <- rowSums(data[,vars.maiaNotWorrying])/(length(vars.maiaNotWorrying))
  maia.attention <- rowSums(data[,vars.maiaAttnRegulation])/(length(vars.maiaAttnRegulation))
  maia.emotionalAwareness <- rowSums(data[,vars.maiaEmoAware])/(length(vars.maiaEmoAware))
  maia.selfRegulation <- rowSums(data[,vars.maiaSelfRegulation])/(length(vars.maiaSelfRegulation))
  maia.listen <- rowSums(data[,vars.maiaBodyListen])/(length(vars.maiaBodyListen))
  maia.trust <- rowSums(data[,vars.maiaTrust])/(length(vars.maiaTrust))

  # get variables we want to pass through to the output
  # bring participant sub,id,date back into the data frame
  keeps <- data[,keep.vars]

  # combine scores and kept variables, make dataframe
  output <- cbind(keeps,raw.dat,maia.notice,maia.distract,maia.worry,maia.attention,maia.emotionalAwareness,maia.selfRegulation,maia.listen,maia.trust)
  output <- as.data.frame(output)
  return(output)

}

#****************************************************************************
# MLQ (Meaning of Life Questionnaire)
mlq.scoring <- function(data,keep.vars,mlq){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }

  if (missing(mlq)){
    mlq <- c(paste('MLQ_',1:10,sep=""))
  }
  
  raw.dat <- data[,mlq]

  revmlq <- c('MLQ_9')
  data[,revmlq] <- 8-data[,revmlq]

  vars.mlqPresence <- c(paste('MLQ_',c(1,4,5,6,9),sep=""))
  vars.mlqSearch <- c(paste('MLQ_',c(2,3,7,8,10),sep=""))

  mlq.presence <- rowSums(data[,vars.mlqPresence])
  mlq.search <- rowSums(data[,vars.mlqSearch])

  keeps <- data[,keep.vars]

  output <- cbind(keeps,raw.dat,mlq.presence,mlq.search)
  output <- as.data.frame(output)
  return(output)
}

#************************************************************************
# Measure of Actualization of Potential (MAP)
#************************************************************************
map.scoring <- function(data, keep.vars, map) {
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <- c(keep.vars, 'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub', keep.vars)
  }
  if (missing(map)) {
    map <- c(paste('MAP_', 1:27, sep = ""))
  }
  
  raw.dat <- data[,map]
  
  rev.map <- c(paste('MAP_',cbind(6,11,21),sep=""))
  data[, rev.map] <- 6-data[, rev.map]
  
  vars.adaptation <- c(paste('MAP_',cbind(5,10,15,20),sep=""))
  vars.autonomy <- c(paste('MAP_',cbind(1,6,11,16,21,25),sep=""))
  vars.openessToSelf <- c(paste('MAP_',cbind(2,7,12,17,22,26),sep=""))
  vars.openessToLife <- c(paste('MAP_',cbind(4,9,14,19,24),sep=""))
  vars.openessToOthers <- c(paste('MAP_',cbind(3, 8, 13, 18, 23, 27),sep=""))
  
  map.adaptation <- rowMeans(data[,vars.adaptation])
  map.autonomy <- rowMeans(data[,vars.autonomy])
  map.openessToSelf<- rowMeans(data[,vars.openessToSelf])
  map.openessToLife <- rowMeans(data[,vars.openessToLife])
  map.openessToOthers <- rowMeans(data[,vars.openessToOthers])
  map.total <- rowMeans(data[,map])
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps,raw.dat,map.adaptation,map.autonomy,map.openessToSelf,map.openessToLife,map.openessToOthers,map.total)

  output <- as.data.frame(output)
  return(output)
}


#****************************************************************************
# Old MEQ (Mystical Experience Questionnaire)
# Uses ratings from SOCQ to derive MEQ score

oldmeq.scoring <- function(data,keeps.vars,socq){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
  
  if(length(grep('SOCQ_', colnames(data))) == 100) {
    if (missing(socq)){
      socq <- c(paste('SOCQ_',1:100,sep=""))
    }
    socq <- c(paste('SOCQ_', c(2,  5,  6,  9, 12, 14, 15, 18, 22, 23, 29, 
                               30, 34, 35, 36, 41, 43, 47, 48, 54, 55, 65, 
                               69, 73, 74, 77, 80, 83, 86, 87), sep=""))
    
    vars.meqMystical <- c(paste('SOCQ_',c(35,41,54,77,83,12,14,47,74,9,22,69,36,55,73),sep=""))
    vars.meqPositive <- c(paste('SOCQ_',c(5,18,30,43,80,87),sep=""))
    vars.meqTranscendence <- c(paste('SOCQ_',c(2,15,29,34,48,65),sep=""))
    vars.meqIneffibility <- c(paste('SOCQ_',c(6,23,86),sep=""))
    vars.meqTotal <- c(vars.meqMystical, vars.meqPositive, vars.meqTranscendence, vars.meqIneffibility)
    
    meq.mystical.pctmax      <- rowSums(data[,vars.meqMystical])/(length(vars.meqMystical))/5
    meq.positive.pctmax      <- rowSums(data[,vars.meqPositive])/(length(vars.meqPositive))/5
    meq.transendence.pctmax  <- rowSums(data[,vars.meqTranscendence])/(length(vars.meqTranscendence))/5
    meq.ineffibility.pctmax  <- rowSums(data[,vars.meqIneffibility])/(length(vars.meqIneffibility))/5
    meq.total.pctmax        <- rowSums(data[,vars.meqTotal])/(length(vars.meqTotal))/5
    
    meq.mystical      <- rowSums(data[,vars.meqMystical])/(length(vars.meqMystical))
    meq.positive      <- rowSums(data[,vars.meqPositive])/(length(vars.meqPositive))
    meq.transendence  <- rowSums(data[,vars.meqTranscendence])/(length(vars.meqTranscendence))
    meq.ineffibility  <- rowSums(data[,vars.meqIneffibility])/(length(vars.meqIneffibility))
    meq.total        <- rowSums(data[,vars.meqTotal])/(length(vars.meqTotal))
    
    # get variables we want to pass through to the output
    # bring participant sub,id,date back into the data frame
    keeps <- data[,keep.vars]
    #raw.dat <- data[,socq]
    
    # combine scores and kept variables, make dataframe
    meqdf.pctmax <- cbind(meq.total.pctmax,meq.mystical.pctmax,meq.positive.pctmax,meq.transendence.pctmax,meq.ineffibility.pctmax)
    meq.complete <- meqdf.pctmax[,"meq.mystical.pctmax"] >= 0.6 & meqdf.pctmax[,"meq.positive.pctmax"] >= 0.6 &
      meqdf.pctmax[,"meq.transendence.pctmax"] >= 0.6 & meqdf.pctmax[,"meq.ineffibility.pctmax"] >= 0.6
    
    #meqdf <- cbind(keeps,raw.dat,meq.total,meq.mystical,meq.positive,meq.transendence,meq.ineffibility)
    meqdf <- cbind(keeps,meq.total,meq.mystical,meq.positive,meq.transendence,meq.ineffibility)
    
    output <- cbind(meqdf,meqdf.pctmax,meq.complete)
    return(output)
    
  } else if (length(grep('SOCQ_', colnames(data))) == 43) {
    if(missing(socq)){
      socq <- c(paste('SOCQ_',1:43,sep=""))
    }
    
    keeps <- data[,keep.vars]
    #raw.dat <- data[,socq]
    output <- cbind(keeps)
    return(output)
  }
   
}

#****************************************************************************
# Brief MEQ (Brief Mystical Experience Questionnaire, 4-items)
#****************************************************************************
briefmeq.scoring <- function(data,keep.vars,briefmeq){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
  if (missing(briefmeq)){
    briefmeq <- c(paste('BriefMEQ_',1:4,sep=""))
  }
  
  raw.dat <- data[,briefmeq]
  
  briefmeq.total <- rowSums(data[,briefmeq])/(length(briefmeq))
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps, raw.dat, briefmeq.total)
  return(output)
}

#****************************************************************************
# MEQ (Mystical Experience Questionnaire)
meq.scoring <- function(data,keep.vars,meq){
  keep.vars <- intersect(c('ID','date','time.point','sub'), colnames(data))
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
  if (missing(meq)){
    meq <- c(paste('MEQ_',1:30,sep=""))
  }
  raw.dat <- data[,meq]

  data[,meq] <- sapply(data[,meq]-1,as.numeric)

  vars.meqMystical        <- c(paste('MEQ_',c(4,5,6,9,14,15,16,18,20,21,23,24,25,26,28),sep=""))
  vars.meqPositive        <- c(paste('MEQ_',c(2,8,12,17,27,30),sep=""))
  vars.meqTranscendence    <- c(paste('MEQ_',c(1,7,11,13,19,22),sep=""))
  vars.meqIneffibility    <- c(paste('MEQ_',c(3,10,29),sep=""))
  vars.meqTotal           <- c(paste('MEQ_',1:30,sep=""))

  meq.mystical.pctmax      <- rowSums(data[,vars.meqMystical]) / (length(vars.meqMystical)) / 5
  meq.positive.pctmax      <- rowSums(data[,vars.meqPositive]) / (length(vars.meqPositive)) / 5
  meq.transendence.pctmax  <- rowSums(data[,vars.meqTranscendence]) / (length(vars.meqTranscendence)) / 5
  meq.ineffibility.pctmax  <- rowSums(data[,vars.meqIneffibility]) / (length(vars.meqIneffibility)) / 5
  meq.total.pctmax         <- rowSums(data[,vars.meqTotal]) / (length(vars.meqTotal)) / 5
  
  meq.mystical      <- rowSums(data[,vars.meqMystical]) / (length(vars.meqMystical))
  meq.positive      <- rowSums(data[,vars.meqPositive]) / (length(vars.meqPositive))
  meq.transendence  <- rowSums(data[,vars.meqTranscendence]) / (length(vars.meqTranscendence))
  meq.ineffibility  <- rowSums(data[,vars.meqIneffibility]) / (length(vars.meqIneffibility))
  meq.total         <- rowSums(data[,vars.meqTotal]) / (length(vars.meqTotal))

  # get variables we want to pass through to the output
  # bring participant sub,id,date back into the data frame
  keeps <- data[,keep.vars]
  

  # combine scores and kept variables, make dataframe
  meqdf.pctmax <- cbind(meq.total.pctmax,meq.mystical.pctmax,meq.positive.pctmax,meq.transendence.pctmax,meq.ineffibility.pctmax)
  meq.complete <- meqdf.pctmax[,"meq.mystical.pctmax"] >= 0.6 & meqdf.pctmax[,"meq.positive.pctmax"] >= 0.6 &
    meqdf.pctmax[,"meq.transendence.pctmax"] >= 0.6 & meqdf.pctmax[,"meq.ineffibility.pctmax"] >= 0.6

  meqdf <- cbind(keeps,raw.dat,meq.total,meq.mystical,meq.positive,meq.transendence,meq.ineffibility)

  output <- cbind(meqdf,meqdf.pctmax,meq.complete)
  return(output)
}

###################################################################################################
# 10-Item Measure of Purpose in Life
###################################################################################################
mpl.scoring <- function(data, keep.vars, mpl) {
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
  if (missing(mpl)) {
    mpl <- c(paste('MPL_',1:10,sep=""))
  }
  raw.dat <- data[,mpl]
  
  vars.rev <- c(paste('MPL_', c(2,3,5,6,10), sep=""))
  data[,vars.rev] <- 6 - data[,vars.rev]
  
  mpl.total <- rowSums(data[,mpl])/length(mpl)
  
  keeps <- data[,keep.vars]
  output <- cbind(keeps, raw.dat, mpl.total) %>%
    as.data.frame()
  output
}

#************************************************************************
# McGill Pain Questionnaire (MPQ)
#************************************************************************  
mcgillpain.scoring <- function(data, keep.vars, mpq) {
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <- c(keep.vars, 'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub', keep.vars)
  }
  if (missing(mpq)) {
    mpq <- c(paste('MPQ_', 1:17, sep = ""))
  }
  
  raw.dat <- data[,mpq]
  
  vars.sensory <- c(paste("MPQ_", 1:11, sep = ""))
  vars.affective <- c(paste("MPQ_", 12:15, sep = ""))
  vars.vas <- data[,"MPQ_16"]
  vars.ppi <- data[,"MPQ_17"]
  vars.total <- c(paste("MPQ_", 1:15, sep = ""))
  
  mpq.sensory <- rowSums(data[,vars.sensory])
  mpq.affective <- rowSums(data[,vars.affective])
  mpq.vas <- vars.vas
  mpq.ppi <- vars.ppi
  mpq.chosen <- rowSums(data[,vars.total] > 0)
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps,raw.dat,mpq.sensory,mpq.affective,mpq.vas,mpq.ppi,mpq.chosen)
  output <- as.data.frame(output)
  return(output)
}

#************************************************************************
# McGill Quality of Life Questionnaire (McGill QoL)
#************************************************************************
mcgillqol.scoring <- function(data, keep.vars, mcgill.qol) {
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <- c(keep.vars, 'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub', keep.vars)
  }
  if (missing(mcgill.qol)) {
    mcgill.qol <- c('MQOL_PartA', paste0(rep(paste0('MQOL_PartB_', 1:3), each=3), rep(c('a', 'a_TEXT', 'b'), times = 3)), 
                    'MQOL_PartB_4',paste0('MQOL_PartC_', 5:16), 'MQOL_PartD_Essay')
  }
  
  raw.dat <- data[,mcgill.qol]
  
  vars.rev <- c(paste0("MQOL_PartB_", c('1b', '2b', '3b')), paste0("MQOL_PartC_", 5:8))
  data[, vars.rev] <- 10-data[,vars.rev]
  
  vars.physicalSymptoms <- paste0("MQOL_PartB_", c('1b', '2b', '3b'))
  vars.physicalWellBeing <- data[,'MQOL_PartB_4']
  vars.psychological <- paste0("MQOL_PartC_", 5:8)
  vars.existential <- paste0("MQOL_PartC_", 9:14)
  vars.support <- paste0("MQOL_PartC_", 15:16)
  
  mqol.physicalSymptoms <- rowSums(data[,vars.physicalSymptoms])/length(vars.physicalSymptoms)
  mqol.physicalWellBeing <- vars.physicalWellBeing
  mqol.psychological <- rowSums(data[,vars.psychological])/length(vars.psychological)
  mqol.existential <- rowSums(data[,vars.existential])/length(vars.existential)
  mqol.support <- rowSums(data[,vars.support])/length(vars.support)
  
  mqol.total <- (mqol.physicalSymptoms + mqol.physicalWellBeing + mqol.psychological + mqol.existential + mqol.support)/5
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps, raw.dat, mqol.physicalSymptoms, mqol.physicalWellBeing, mqol.psychological, mqol.existential, mqol.support, mqol.total) %>%
    as.data.frame()
  return(output)
}
#************************************************************************
# Mindful Attention Awareness Scale (MAAS)
#************************************************************************

maas.scoring <- function(data, keep.vars, maas) {
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <- c(keep.vars, 'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub', keep.vars)
  }
  if (missing(maas)) {
    maas <- c(paste('MAAS_',c(1:15), sep = ""))
  }
  raw.dat <- data[,maas]
  
  maas.total <- rowSums(data[,maas])/length(maas)
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps,raw.dat,maas.total)
  output <- as.data.frame(output)
  return(output)
}

#************************************************************************
# Mentalization Scale (MentS)
#************************************************************************
ments.scoring <- function(data, keep.vars, ments) {
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <- c(keep.vars, 'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub', keep.vars)
  }
  if (missing(ments)) {
    ments <- paste0('MentS_', 1:28)
  }
  
  raw.dat <- data[,ments]
  
  vars.rev <- paste0('MentS_',c(8,9,11,14,18,19,21,22,26,27))
  data[,vars.rev] <- 6-data[,vars.rev]
  
  vars.mentsOthers <- paste0('MentS_', c(2,3,5,6,10,12,20,23,25,28))
  vars.mentsSelf <- paste0('MentS_', c(8,11,14,18,19,21,22,26))
  vars.mentsMotiv <- paste0('MentS_', c(1,4,7,9,13,15,16,17,24,27))
  
  ments.others <- rowSums(data[,vars.mentsOthers])/length(vars.mentsOthers)
  ments.self <- rowSums(data[,vars.mentsSelf])/length(vars.mentsSelf)
  ments.motivation <- rowSums(data[,vars.mentsMotiv])/length(vars.mentsMotiv)
  ments.total <- rowSums(data[,ments])/length(ments)
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps,raw.dat,ments.others, ments.self, ments.motivation, ments.total)
  output <- as.data.frame(output)
  return(output)
}


#************************************************************************
# Mindful Reinterpretation of Pain Scale (MRPS)
#************************************************************************
mrps.scoring <- function(data, keep.vars, mrps) {
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <- c(keep.vars, 'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub', keep.vars)
  }
  if (missing(mrps)) {
    mrps <- c(paste('MRPS_', 1:9, sep = ""))
  }
  
  raw.dat <- data[,mrps]
  
  mrps.total <- rowSums(data[,mrps])
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps,raw.dat,mrps.total)
  output <- as.data.frame(output)
  return(output)
}


####################################################################
# 20-Item Mini-IPIP
####################################################################
ipip.scoring <- function(data, keep.vars, ipip) {
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
  if (missing(ipip)) {
    ipip <- c(paste('IPIP_',1:20,sep=""))
  }
  raw.dat <- data[,ipip]
  
  # reverse scoring
  vars.rev <- c(paste('IPIP_', c(6, 7, 8, 9, 10, 15, 16, 17, 18, 19, 20), sep=""))
  data[,vars.rev] <- 6 - data[,vars.rev]
  
  # subscales
  vars.ipipExtraversion <- c(paste('IPIP_', c(1, 6, 11, 16), sep=""))
  vars.ipipAgreeableness <- c(paste('IPIP_', c(2, 7, 12, 17), sep=""))
  vars.ipipConscientiousness <- c(paste('IPIP_', c(3, 8, 13, 18), sep=""))
  vars.ipipNeuroticism <- c(paste('IPIP_', c(4, 9, 14, 19), sep=""))
  vars.ipipIntellect <- c(paste('IPIP_', c(5, 10, 15, 20), sep=""))
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps, raw.dat, vars.ipipExtraversion, vars.ipipAgreeableness, 
                  vars.ipipConscientiousness, vars.ipipNeuroticism, vars.ipipIntellect) %>%
    as.data.frame()
  output
}

####################################################################
# McClean Instrument for Borderline Personality Disorder (MSIBPD)
####################################################################
msibpd.scoring <- function(data, keep.vars, msibpd) {
        keep.vars <- c('ID','date')
        if('time.point' %in% colnames(data))
        {
                keep.vars <-c(keep.vars,'time.point')
        }
        if('sub' %in% colnames(data))
        {
                keep.vars <- c('sub',keep.vars)
        }
        
        if (missing(msibpd)) {
                msibpd <- c(paste('MSIBPD_',1:10,sep=""))
        }
        raw.dat <- data[,msibpd]
        
        msibpd.total <- rowSums(data[,msibpd])
        
        keeps <- data[,keep.vars]
        
        output <- cbind(keeps, raw.dat, msibpd.total) %>%
                as.data.frame()
        output
}
####################################################################
# Multidimensional Psychological Flexibility Inventory (MPFI) #TODO verify how the total average score is done
####################################################################
mpfi.scoring <- function(data, keep.vars, mpfi) {
        keep.vars <- c('ID','date')
        if('time.point' %in% colnames(data))
        {
                keep.vars <-c(keep.vars,'time.point')
        }
        if('sub' %in% colnames(data))
        {
                keep.vars <- c('sub',keep.vars)
        }
        
        if (missing(mpfi)) {
                mpfi <- c(paste('MPFI_',1:24,sep=""))
        }
        raw.dat <- data[,mpfi]
        
        # Flexibility subscales
        vars.mpfiAcceptance <- c(paste('MPFI_', c(9,10), sep=""))
        vars.mpfiPresentMomentAwareness <- c(paste('MPFI_', c(3,4), sep=""))
        vars.mpfiSelfAsContext <- c(paste('MPFI_', c(7,8), sep=""))
        vars.mpfiDefusion <- c(paste('MPFI_', c(5,6), sep=""))
        vars.mpfiValues <- c(paste('MPFI_', c(1,2), sep=""))
        vars.mpfiCommittedAction <- c(paste('MPFI_', c(11,12), sep=""))
        
        # Inflexibility subscales
        vars.mpfiExperientialAvoidance <- c(paste('MPFI_',c(21,22),sep=""))
        vars.mpfiLackOfContactWithPresentMoment <- c(paste('MPFI_',c(15,16),sep=""))
        vars.mpfiSelfAsContent <- c(paste('MPFI_',c(19,20),sep=""))
        vars.mpfiFusion <- c(paste('MPFI_',c(17,18),sep=""))
        vars.mpfiLackOfContactWithValues <- c(paste('MPFI_',c(13,14),sep=""))
        vars.mpfiInaction <- c(paste('MPFI_',c(23,24),sep=""))
        
        # Calculate averages for each flexibility subscale
        mpfi.acceptance <- rowSums(data[,vars.mpfiAcceptance])/length(vars.mpfiAcceptance)
        mpfi.presentMomentAwareness <- rowSums(data[,vars.mpfiPresentMomentAwareness])/length(vars.mpfiPresentMomentAwareness)
        mpfi.selfAsContext <- rowSums(data[,vars.mpfiSelfAsContext])/length(vars.mpfiSelfAsContext)
        mpfi.defusion <- rowSums(data[,vars.mpfiDefusion])/length(vars.mpfiDefusion)
        mpfi.values <- rowSums(data[,vars.mpfiValues])/length(vars.mpfiValues)
        mpfi.committedAction <- rowSums(data[,vars.mpfiCommittedAction])/length(vars.mpfiCommittedAction)
        
        # Calculate averages for each inflexibility subscale
        mpfi.experientialAvoidance <- rowSums(data[, vars.mpfiExperientialAvoidance]) / length(vars.mpfiExperientialAvoidance)
        mpfi.lackOfContactWithPresentMoment <- rowSums(data[, vars.mpfiLackOfContactWithPresentMoment]) / length(vars.mpfiLackOfContactWithPresentMoment)
        mpfi.selfAsContent <- rowSums(data[, vars.mpfiSelfAsContent]) / length(vars.mpfiSelfAsContent)
        mpfi.fusion <- rowSums(data[, vars.mpfiFusion]) / length(vars.mpfiFusion)
        mpfi.lackOfContactWithValues <- rowSums(data[, vars.mpfiLackOfContactWithValues]) / length(vars.mpfiLackOfContactWithValues)
        mpfi.inaction <- rowSums(data[, vars.mpfiInaction]) / length(vars.mpfiInaction)
        
        # Calculate total flexibility and inflexibility averages
        mpfi.flexibility <- rowMeans(cbind(mpfi.acceptance, mpfi.presentMomentAwareness, mpfi.selfAsContext, mpfi.defusion, mpfi.values, mpfi.committedAction))
        mpfi.inflexibility <- rowMeans(cbind(mpfi.experientialAvoidance, mpfi.lackOfContactWithPresentMoment, mpfi.selfAsContent, mpfi.fusion, mpfi.lackOfContactWithValues, mpfi.inaction))
        
        # Output the data
        keeps <- data[, keep.vars]
        output <- cbind(keeps, raw.dat, mpfi.acceptance, mpfi.presentMomentAwareness, mpfi.selfAsContext, mpfi.defusion, mpfi.values, mpfi.committedAction, mpfi.flexibility, 
                        mpfi.experientialAvoidance, mpfi.lackOfContactWithPresentMoment, mpfi.selfAsContent, mpfi.fusion, mpfi.lackOfContactWithValues, mpfi.inaction, mpfi.inflexibility) %>%
                as.data.frame()
        
        return(output)
}

###################################################################################################
# Nature Relatedness Scale
###################################################################################################
nrs.scoring <- function(data, keep.vars, nrs) {
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }

  if (missing(nrs)) {
    nrs <- c(paste('NatureRelate_',1:21,sep=""))
  }
  raw.dat <- data[,nrs]

  # reverse scoring
  vars.rev <- c(paste('NatureRelate_', c(2, 3, 10, 11, 13, 14, 15, 18), sep=""))
  data[,vars.rev] <- 6 - data[,vars.rev]

  # subscales
  vars.nrsSelf <- c(paste('NatureRelate_', c(5, 7, 8, 12, 14, 16, 17, 21), sep=""))
  vars.nrsPerspective <- c(paste('NatureRelate_', c(2, 3, 11, 15, 18, 19, 20), sep=""))
  vars.nrsExperience <- c(paste('NatureRelate_', c(1, 4, 6, 9, 10, 13), sep=""))

  # scoring
  naturerelate.self <- rowSums(data[,vars.nrsSelf])/length(vars.nrsSelf)
  naturerelate.perspective <- rowSums(data[,vars.nrsPerspective])/length(vars.nrsPerspective)
  naturerelate.experience <- rowSums(data[,vars.nrsExperience])/length(vars.nrsExperience)
  naturerelate.total <- rowSums(data[,nrs])/length(nrs)

  keeps <- data[,keep.vars]
  
  output <- cbind(keeps, raw.dat,naturerelate.self, naturerelate.perspective, naturerelate.experience, naturerelate.total) %>%
    as.data.frame()
  output
}

#****************************************************************************
# Nature Relatedness Scale - Short Form (Before & After)
nrs.short.prepost.scoring <- function(data,keep.vars,nrs.short){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
  if (missing(nrs.short)){
    nrs.short <- c(c(paste('NatureRelate_Short_',1:6,'_Before',sep="")), 
                   c(paste('NatureRelate_Short_',1:6,'_After',sep="")))
  }
  raw.dat <- data[,nrs.short]
  
  vars.nrs.short.before <- c(paste('NatureRelate_Short_',1:6,'_Before',sep=""))
  vars.nrs.short.after <- c(paste('NatureRelate_Short_',1:6,'_After',sep=""))
  
  # calculate scores
  naturerelateShort.before <- rowMeans(sapply(data[,vars.nrs.short.before],as.numeric))
  naturerelateShort.after <- rowMeans(sapply(data[,vars.nrs.short.after],as.numeric))
  
  # get variables we want to pass through to the output
  keeps <- data[,keep.vars]
  
  # combine scores and kept variables, make dataframe
  output <- cbind(keeps,raw.dat,naturerelateShort.before,naturerelateShort.after)
  output <- as.data.frame(output)
  
  # return
  return(output)
}

#****************************************************************************
# NEO Psychology Inventory, Revised (NEO-PI-R)

neo.scoring <- function(data, keep.vars, neo) {
        keep.vars <- c('ID','date')
        if('time.point' %in% colnames(data))
        {
                keep.vars <-c(keep.vars,'time.point')
        }
        if('sub' %in% colnames(data))
        {
                keep.vars <- c('sub', keep.vars)
        }
        
        if (missing(neo)) {
                neo <- c(paste('NEO_', 1:30, sep= ""))
        }
        
        # old.neo <- c(paste('NEO_', 1:30, sep = ""))
        # 
        # setnames(data,old = old.neo, new = neo)
        # print(neo)
        raw.dat <- data[, neo]
        
        
        # define subscales
        vars.neoNeuroticism <- c(paste('NEO_', 1:6, sep = ""))
        vars.neoExtraversion <- c(paste('NEO_', 7:12, sep = ""))
        vars.neoOpenness <- c(paste('NEO_', 13:18, sep = ""))
        vars.neoAgreeableness <- c(paste('NEO_', 19:24, sep = ""))
        vars.neoConscientiousness <- c(paste('NEO_', 25:30, sep = ""))
        
        # # Print class type of vars.neoNeuroticism
        # data_types <- sapply(data[, vars.neoNeuroticism], class)
        # print(data_types)
        
        #calculate average score
        neo.neuroticism <- rowSums(data[,vars.neoNeuroticism])
        neo.extraversion <- rowSums(data[,vars.neoExtraversion])
        neo.openness <- rowSums(data[,vars.neoOpenness])
        neo.agreeableness <- rowSums(data[,vars.neoAgreeableness])
        neo.conscientiousness <- rowSums(data[,vars.neoConscientiousness])
        
        # print("got here")
        # print(keep.vars)
        # print(data[,keep.vars])
        keeps <- data[, keep.vars]
        
        output <- cbind(keeps, raw.dat, neo.neuroticism, neo.extraversion, neo.openness, neo.agreeableness, neo.conscientiousness)
        output <- as.data.frame(output)
        return(output)
}

#*********************************************************************
# New Sexual Satisfaction Scale - Short
#*********************************************************************
nsss.scoring <- function(data, keep.vars, newsexsatisf){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
  if (missing(newsexsatisf)){
    newsexsatisf <- paste('NSSS_',1:12,sep="")
  }
  raw.dat <- data[,newsexsatisf]
  
  nsss.total <- rowSums(data[,newsexsatisf])
  
  keeps <- data[,keep.vars]
  output <- cbind(keeps, raw.dat, nsss.total)
  output <- as.data.frame(output)
  return(output)
  
}

#****************************************************************************
# Next Day Volunteer Ratings
nextday.scoring <- function(data,keep.vars,nextday){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
  if (missing(nextday)){
    nextday<- c(c('NextDayVol_1','NextDayVol_1a','NextDayVol_2','NextDayVol_2a','NextDayVol_2b','NextDayVol_3','NextDayVol_3a','NextDayVol_4',
                  'NextDayVol_4a'),c(paste('NextDayVol_',5:11,sep="")))
  }

  # persisting.effects.nextday <- c(paste('NextDayVol_',1:4,sep=""))
  # peq.nextday <- data[,persisting.effects.nextday]
  keeps <- data[,keep.vars]
  raw.dat <- data[,nextday]

  output <- cbind(keeps,raw.dat)
  #,peq.nextday)
  output <- as.data.frame(output)
  return(output)
}

#****************************************************************************
### NIH Toolbox - Positive Affect
#****************************************************************************
nihAffect.scoring <- function (data, keep.vars, nihAffect){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
  if (missing(nihAffect)){
    nihAffect <- c(paste('NIHAffect_',1:34,sep=""))
  }
  
  raw.dat <- data[,nihAffect]
  
  nihAffect.total <- rowSums(data[,nihAffect])/length(nihAffect)
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps, raw.dat, nihAffect.total)
  output <- as.data.frame(output)
  return(output)
}

#****************************************************************************
### NIH Toolbox - Apathy
#****************************************************************************
nihApathy.scoring <- function (data, keep.vars, nihApathy){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
  if (missing(nihApathy)){
    nihApathy <- c(paste('NIHApathy_',1:7,sep=""))
  }
  
  raw.dat <- data[,nihApathy]
  
  nihApathy.total <- rowSums(data[,nihApathy])/length(nihApathy)
  
  
  keeps <- data[,keep.vars]
  
  
  output <- cbind(keeps, raw.dat, nihApathy.total)
  output <- as.data.frame(output)
  return(output)
}

#****************************************************************************
### NIH Toolbox - General Life Satisfaction 
#****************************************************************************
nihLifeSat.scoring <- function (data, keep.vars, nihLifeSat){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
  if (missing(nihLifeSat)){
    nihLifeSat <- c(paste('NIHLifeSat_',1:10,sep=""))
  }
  
  raw.dat <- data[,nihLifeSat]
  
  vars_rev <- "NIHLifeSat_8"
  data[,vars_rev] <- 6- data[,vars_rev]
  
  nihLifeSat.total <- rowSums(data[,nihLifeSat])/length(nihLifeSat)
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps, raw.dat, nihLifeSat.total)
  output <- as.data.frame(output)
  return(output)
}


#****************************************************************************
### NIH Toolbox - Perceived Stress
#****************************************************************************
nihStress.scoring <- function (data, keep.vars, nihStress){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
  if (missing(nihStress)){
    nihStress <- c(paste('NIHStress_',1:10,sep=""))
  }
  
  raw.dat <- data[,nihStress]
  
  vars_rev <- c(paste('NIHStress_', cbind(4, 5, 7, 8), sep=""))
  data[,vars_rev] <- 6-data[,vars_rev]
  
  nihStress.total <- rowSums(data[,nihStress])/length(nihStress)
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps, raw.dat, nihStress.total)
  output <- as.data.frame(output)
  return(output)
}

#*******************************************************************************
# NPI-Q (Neuropsychiatric Inventory Questionnaire)
npiq.scoring <- function(data,keep.vars,npiq){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }

  if (missing(npiq)){
    npiq <- c(paste('NPIQ_',1:12,sep=""), paste('NPIQ_',1:12,'a',sep=""), paste('NPIQ_',1:12,'b',sep=""))
  }
  raw.dat <- data[,npiq]

  #Separating severity and distress scores
  vars.npiqA <- paste('NPIQ_',1:12,'a',sep="")
  vars.npiqB <- paste('NPIQ_',1:12,'b',sep="")
  
  npiq.severityPatient <- rowSums(data[,vars.npiqA], na.rm=T)
  npiq.distressCaregiver <- rowSums(data[,vars.npiqB], na.rm=T)

  keeps <- data[,keep.vars]

  output <- cbind(keeps,raw.dat,npiq.severityPatient,npiq.distressCaregiver)
  output <- as.data.frame(output)
  return(output)

}

####################################################################
# Non-Attachment Scale (NAS)
####################################################################
nas.scoring <- function(data, keep.vars, nas) {
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }

  if (missing(nas)) {
    nas <- c(paste('NAS_',1:30,sep=""))
  }

  raw.dat <- data[,nas]
  # reverse score items 4, 13, 24
  vars.rev <- c(paste('NAS_', c(4, 13, 24), sep=""))
  data[,vars.rev] <- 7 - data[,vars.rev]

  # calculate score
  nas.total <- rowSums(data[,nas])/length(nas)

  keeps <- data[,keep.vars]

  output <- cbind(keeps, raw.dat,nas.total) %>%
    as.data.frame()
  output
}

###################################################################################################
# Opioid Craving Scale (OCS)
###################################################################################################
ocs.scoring <- function(data, keep.vars, ocs) {
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
  if (missing(ocs)) {
    ocs <- c(paste('OCS_',1:3,sep=""))
  }
  raw.dat <- data[,ocs]
  
  ocs.total <- rowSums(data[,ocs])/length(ocs)
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps, raw.dat,ocs.total)
  output <- as.data.frame(output)
  output
}

###################################################################################################
# Oswestry Disability Index
###################################################################################################
odi.scoring <- function(data, keep.vars, odi){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
  if (missing(odi)){
    odi <- paste('ODI_', 1:10, sep="")
  }
  raw.dat <- data[, odi]
  
  #Scoring instructions say that for every question not answered, the denominator is reduced by 5. 
  #However, in Qualtrics, we have forced response, so denominator should always be 50. 
  odi.total <- (rowSums(data[,odi])/50)*100
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps, raw.dat, odi.total)
  output <- as.data.frame(output)
  return(output)
}

#************************************************************************
# Pain Catastrophizing Scale (PCS)
#************************************************************************
pcs.scoring <- function(data, keep.vars, pcs) {
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <- c(keep.vars, 'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub', keep.vars)
  }
  if (missing(pcs)) {
    pcs <- c(paste('PCS_', 1:13, sep = ""))
  }
  
  raw.dat <- data[,pcs]
  
  vars.rumination <- c(paste("PCS_", 8:11, sep = ""))
  vars.magnification <- c(paste("PCS_", c(6,7,13), sep = ""))
  vars.helplessness <- c(paste("PCS_", c(1:5, 12), sep = ""))
  
  pcs.rumination <- rowSums(data[,vars.rumination])
  pcs.magnification <- rowSums(data[,vars.magnification])
  pcs.helplessness <- rowSums(data[,vars.helplessness])
  pcs.total <- rowSums(data[,pcs])
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps,raw.dat,pcs.rumination,pcs.magnification,pcs.helplessness,pcs.total)
  output <- as.data.frame(output)
  return(output)
}

################################################################################
# painDETECT
################################################################################
painDETECT.scoring <- function(data, keep.vars, painDETECT){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
  if (missing(painDETECT)){
    painDETECT <- paste('painDETECT_', c(paste0('1', c(letters[1:18], '_888', '_888_TEXT')), 2:10), sep="")
  }
  raw.dat <- data[, painDETECT]
  
  vars.total <- paste('painDETECT_', 2:10, sep = "")
  painDETECT.total <- rowSums(data[,vars.total])
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps, raw.dat, painDETECT.total)
  output <- as.data.frame(output)
  return(output)
}
#****************************************************************************
#### PEQ Modified 0014 Functions ###
###### modified column names because 0014 does not include every item for each subscale ###

peqlife0014.scoring <- function(data,keep.vars,peqlife){
        keep.vars <- c('ID','date')
        if('time.point' %in% colnames(data))
        {
                keep.vars <-c(keep.vars,'time.point')
        }
        if('sub' %in% colnames(data))
        {
                keep.vars <- c('sub',keep.vars)
        }
        
        if (missing(peqlife)){
                peqlife <- paste0('PEQ_AttitudeLife_', 1:20)
        }
        
        raw.dat <- data[,peqlife]
        
        vars.peqlife.pos <- c(paste('PEQ_AttitudeLife_',c(1,3,6,8,10,11,13,15,18,19),sep=""))
        vars.peqlife.neg <- c(paste('PEQ_AttitudeLife_',c(2,4,5,7,9,12,14,16,17,20),sep=""))
        
        peq.attitudelife.pos <- rowSums(data[,vars.peqlife.pos])/(5*length(vars.peqlife.pos)) 
        peq.attitudelife.neg <- rowSums(data[,vars.peqlife.neg])/(5*length(vars.peqlife.neg))
        
        keeps <- data[,keep.vars]
        
        output <- cbind(keeps,raw.dat,peq.attitudelife.pos, peq.attitudelife.neg)
        output <- as.data.frame(output)
        return(output)
        
}

#****************************************************************************
# Persisting Effects: Attitudes about Self
peqself0014.scoring <- function(data,keep.vars,peqself){
        keep.vars <- c('ID','date')
        if('time.point' %in% colnames(data))
        {
                keep.vars <-c(keep.vars,'time.point')
        }
        if('sub' %in% colnames(data))
        {
                keep.vars <- c('sub',keep.vars)
        }
        
        if (missing(peqself)){
                peqself <- paste0('PEQ_AttitudeSelf_', c(1:16))
        }
        
        raw.dat <- data[,peqself]
        
        vars.peqself.pos <- c(paste('PEQ_AttitudeSelf_',c(1,4,6,7,9,12,14,15),sep=""))
        vars.peqself.neg <- c(paste('PEQ_AttitudeSelf_',c(2,3,5,8,10,11,13,16),sep=""))
        
        peq.attitudeself.pos <- rowSums(data[,vars.peqself.pos])/(5*length(vars.peqself.pos))
        peq.attitudeself.neg <- rowSums(data[,vars.peqself.neg])/(5*length(vars.peqself.neg))
        
        keeps <- data[,keep.vars]
        
        output <- cbind(keeps,raw.dat,peq.attitudeself.pos,peq.attitudeself.neg)
        output <- as.data.frame(output)
        return(output)
}

#****************************************************************************
# Persisting Effects: Mood Changes
peqmood0014.scoring <- function(data,keep.vars,peqmood){
        keep.vars <- c('ID','date')
        if('time.point' %in% colnames(data))
        {
                keep.vars <-c(keep.vars,'time.point')
        }
        if('sub' %in% colnames(data))
        {
                keep.vars <- c('sub',keep.vars)
        }
        
        if (missing(peqmood)){
                peqmood <- paste0('PEQ_MoodChanges_', c(1:4, 7:10))
        }
        
        raw.dat <- data[,peqmood]
        
        vars.peqmood.pos <- c(paste('PEQ_MoodChanges_',c(1,3,8,10),sep=""))
        vars.peqmood.neg <- c(paste('PEQ_MoodChanges_',c(2,4,7,9),sep=""))
        
        peq.moodchange.pos <- rowSums(data[,vars.peqmood.pos])/(5*length(vars.peqmood.pos))
        peq.moodchange.neg <- rowSums(data[,vars.peqmood.neg])/(5*length(vars.peqmood.neg))
        
        keeps <- data[,keep.vars]
        
        output <- cbind(keeps,raw.dat,peq.moodchange.pos,peq.moodchange.neg)
        output <- as.data.frame(output)
        return(output)
}

#****************************************************************************
# Persisting Effects: Relationships
peqrelate0014.scoring <- function(data,keep.vars,peqrelate){
        keep.vars <- c('ID','date')
        if('time.point' %in% colnames(data))
        {
                keep.vars <-c(keep.vars,'time.point')
        }
        if('sub' %in% colnames(data))
        {
                keep.vars <- c('sub',keep.vars)
        }
        
        if (missing(peqrelate)){
                peqrelate <- paste0('PEQ_Relationships_', 1:16)
        }
        
        raw.dat <- data[,peqrelate]
        
        vars.peqrelate.pos <- c(paste('PEQ_Relationships_',c(1,4,5,7,10,12,13,16),sep=""))
        vars.peqrelate.neg <- c(paste('PEQ_Relationships_',c(2,3,6,8,9,11,14,15),sep=""))
        
        peq.relationships.pos <- rowSums(data[,vars.peqrelate.pos])/(5*length(vars.peqrelate.pos))
        peq.relationships.neg <- rowSums(data[,vars.peqrelate.neg])/(5*length(vars.peqrelate.neg))
        
        keeps <- data[,keep.vars]
        
        output <- cbind(keeps,raw.dat,peq.relationships.pos,peq.relationships.neg)
        output <- as.data.frame(output)
        return(output)
}

#****************************************************************************
# Persisting Effects: Behavior Change
peqbehavior0014.scoring <- function(data,keep.vars,peqbehavior){
        keep.vars <- c('ID','date')
        if('time.point' %in% colnames(data))
        {
                keep.vars <-c(keep.vars,'time.point')
        }
        if('sub' %in% colnames(data))
        {
                keep.vars <- c('sub',keep.vars)
        }
        
        if (missing(peqbehavior)){
                peqbehavior <- c('PEQ_BehaviorChange_1','PEQ_BehaviorChange_2')
        }
        
        raw.dat <- data[,peqbehavior]
        
        vars.peqbehavior.neg <- c(paste('PEQ_BehaviorChange_',c(1),sep=""))
        vars.peqbehavior.pos <- c(paste('PEQ_BehaviorChange_',c(2),sep=""))
        
        peq.behaviorchange.pos <- data[,vars.peqbehavior.pos]/(5*length(vars.peqbehavior.pos))
        peq.behaviorchange.neg <- data[,vars.peqbehavior.neg]/(5*length(vars.peqbehavior.neg))
        
        keeps <- data[,keep.vars]
        output <- cbind(peq.behaviorchange.pos, peq.behaviorchange.neg)
        
        if ('PEQ_BehaviorChange_1' %in% colnames(output)) {
                output <- rename(output, peq.behaviorchange.pos='PEQ_BehaviorChange_2')
                output <- rename(output, peq.behaviorchange.neg='PEQ_BehaviorChange_1')
        }
        
        output <- cbind(keeps,raw.dat,output)
        output <- as.data.frame(output)
        return(output)
}

#****************************************************************************
# Persisting Effects: Spirituality
peqspirit0014.scoring <- function(data,keep.vars,peqspirit){
        keep.vars <- c('ID','date')
        if('time.point' %in% colnames(data))
        {
                keep.vars <-c(keep.vars,'time.point')
        }
        if('sub' %in% colnames(data))
        {
                keep.vars <- c('sub',keep.vars)
        }
        # if("dose" %in% colnames(data))
        # {
        #   keep.vars <-c('sub','ID','date', 'dose')
        # }
        
        if(missing(peqspirit)) {
                peqspirit <- paste0('PEQ_Spirituality_', 1:8)
        }
        
        raw.dat <- data[,peqspirit]
        
        vars.peqspirit.pos <- c(paste('PEQ_Spirituality_',c(1,4,6,7),sep=""))
        vars.peqspirit.neg <- c(paste('PEQ_Spirituality_',c(2,3,5,8),sep=""))
        
        peq.spirituality.pos <- rowSums(data[,vars.peqspirit.pos])/(5*length(vars.peqspirit.pos))
        peq.spirituality.neg <- rowSums(data[,vars.peqspirit.neg])/(5*length(vars.peqspirit.neg))
        peqspirit.scores.comb <- cbind(peq.spirituality.pos, peq.spirituality.neg)
        
        
        keeps <- data[,keep.vars]
        
        output <- cbind(keeps,raw.dat,peqspirit.scores.comb)
        output <- as.data.frame(output)
        return(output)
}

#****************************************************************************
# Persisting Effects: Miscellaneous 
peqmisc0014.scoring <- function(data,keep.vars,peqmisc){
        keep.vars <- c('ID','date')
        if('time.point' %in% colnames(data))
        {
                keep.vars <-c(keep.vars,'time.point')
        }
        if('sub' %in% colnames(data))
        {
                keep.vars <- c('sub',keep.vars)
        }
        
        if (missing(peqmisc)){
                peqmisc <- c(paste0('PEQ_Misc_', 3:13), "PEQ_PersMeaning", "PEQ_SpiritSignif", "PEQ_HowUnusual", 
                "PEQ_ChangeWellBeing", paste0('PEQ_Additional_', 1:5))
        }
        raw.dat <- data[,peqmisc]
        
        vars.peqmisc.pos <- c(paste('PEQ_Misc_',c(4,6,7,9:13 ),sep=""))
        vars.peqmisc.neg <- c(paste('PEQ_Misc_',c(3,5,8),sep=""))
        
        peqmisc.pos <- rowSums(data[,vars.peqmisc.pos])/(5*length(vars.peqmisc.pos))
        peqmisc.neg <- rowSums(data[,vars.peqmisc.neg])/(5*length(vars.peqmisc.neg))
        
        keeps <- data[,keep.vars]
        
        output <- cbind(keeps,raw.dat,peqmisc.pos, peqmisc.neg)
        output <- as.data.frame(output)
        return(output)
}

#****************************************************************************
# Persisting Effects Short Form
peqshort.scoring <- function(data,keep.vars,peqshort){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
  if (missing(peqshort)){
    peqshort <- c('PEQ_MoodChanges_5', 'PEQ_AttitudeLife_19', 'PEQ_MoodChanges_17',
                  'PEQ_Relationships_10', 'PEQ_AttitudeLife_3', 'PEQ_MoodChanges_11',
                  'PEQ_Relationships_5', 
                  
                  'PEQ_MoodChanges_6', 'PEQ_AttitudeLife_20', 'PEQ_AttitudeLife_16',
                  'PEQ_Relationships_9', 'PEQ_Spirituality_20', 'PEQ_MoodChanges_12',
                  'PEQ_Relationships_8')
  }
  
  raw.dat <- data[,peqshort]
  
  
  vars.peqshort.pos <- c('PEQ_MoodChanges_5', 'PEQ_AttitudeLife_19', 'PEQ_MoodChanges_17',
                         'PEQ_Relationships_10', 'PEQ_AttitudeLife_3', 'PEQ_MoodChanges_11',
                         'PEQ_Relationships_5')
  vars.peqshort.neg <- c('PEQ_MoodChanges_6', 'PEQ_AttitudeLife_20', 'PEQ_AttitudeLife_16',
                         'PEQ_Relationships_9', 'PEQ_Spirituality_20', 'PEQ_MoodChanges_12',
                         'PEQ_Relationships_8')
  
  peqshort.pos <- rowSums(data[,vars.peqshort.pos])/(length(vars.peqshort.pos))
  peqshort.neg <- rowSums(data[,vars.peqshort.neg])/(length(vars.peqshort.neg))
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps, raw.dat, peqshort.pos, peqshort.neg)
  output <- as.data.frame(output)
  return(output)
}

#****************************************************************************
# Persisting Effects: Attitudes about Life
peqlife.scoring <- function(data,keep.vars,peqlife){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
  # if("dose" %in% colnames(data))
  # {
  #   keep.vars <-c('sub','ID','date', 'dose')
  # }
  
  peqlife.scores.comb <- c()
  raw.colnames <- colnames(data)
  
  if(length(grep("PEQ_AttitudeLife_", raw.colnames)) == 39){ #26-item version
    if (missing(peqlife)){
      peqlife <- c('PEQ_AttitudeLife_1','PEQ_AttitudeLife_2','PEQ_AttitudeLife_2a','PEQ_AttitudeLife_3','PEQ_AttitudeLife_4','PEQ_AttitudeLife_4a',
                 'PEQ_AttitudeLife_5','PEQ_AttitudeLife_6','PEQ_AttitudeLife_6a','PEQ_AttitudeLife_7','PEQ_AttitudeLife_8','PEQ_AttitudeLife_8a',
                 'PEQ_AttitudeLife_9','PEQ_AttitudeLife_10','PEQ_AttitudeLife_10a','PEQ_AttitudeLife_11','PEQ_AttitudeLife_12','PEQ_AttitudeLife_12a',
                 'PEQ_AttitudeLife_13','PEQ_AttitudeLife_14','PEQ_AttitudeLife_14a','PEQ_AttitudeLife_15','PEQ_AttitudeLife_16','PEQ_AttitudeLife_16a',
                 'PEQ_AttitudeLife_17','PEQ_AttitudeLife_18','PEQ_AttitudeLife_18a','PEQ_AttitudeLife_19','PEQ_AttitudeLife_20','PEQ_AttitudeLife_20a',
                 'PEQ_AttitudeLife_21','PEQ_AttitudeLife_22','PEQ_AttitudeLife_22a','PEQ_AttitudeLife_23','PEQ_AttitudeLife_24','PEQ_AttitudeLife_24a',
                 'PEQ_AttitudeLife_25','PEQ_AttitudeLife_26','PEQ_AttitudeLife_26a');
    }

    vars.peqlife.pos <- c(paste('PEQ_AttitudeLife_',c(1,3,6,8,10,11,13,15,18,19,21,24,25),sep=""))
    vars.peqlife.neg <- c(paste('PEQ_AttitudeLife_',c(2,4,5,7,9,12,14,16,17,20,22,23,26),sep=""))
  
    peq.attitudelife.pos <- rowSums(data[,vars.peqlife.pos])/(5*length(vars.peqlife.pos))
    peq.attitudelife.neg <- rowSums(data[,vars.peqlife.neg])/(5*length(vars.peqlife.neg))
    peqlife.scores.comb <- cbind(peq.attitudelife.pos, peq.attitudelife.neg)
  } else if (length(grep("PEQ_AttitudeLife_", raw.colnames)) == 36){ #24-item version
    if (missing(peqlife)){
      peqlife <- c('PEQ_AttitudeLife_1','PEQ_AttitudeLife_2','PEQ_AttitudeLife_2a','PEQ_AttitudeLife_3','PEQ_AttitudeLife_4','PEQ_AttitudeLife_4a',
                   'PEQ_AttitudeLife_5','PEQ_AttitudeLife_6','PEQ_AttitudeLife_6a','PEQ_AttitudeLife_7','PEQ_AttitudeLife_8','PEQ_AttitudeLife_8a',
                   'PEQ_AttitudeLife_9','PEQ_AttitudeLife_10','PEQ_AttitudeLife_10a','PEQ_AttitudeLife_11','PEQ_AttitudeLife_12','PEQ_AttitudeLife_12a',
                   'PEQ_AttitudeLife_13','PEQ_AttitudeLife_14','PEQ_AttitudeLife_14a','PEQ_AttitudeLife_15','PEQ_AttitudeLife_16','PEQ_AttitudeLife_16a',
                   'PEQ_AttitudeLife_17','PEQ_AttitudeLife_18','PEQ_AttitudeLife_18a','PEQ_AttitudeLife_19','PEQ_AttitudeLife_20','PEQ_AttitudeLife_20a',
                   'PEQ_AttitudeLife_21','PEQ_AttitudeLife_22','PEQ_AttitudeLife_22a','PEQ_AttitudeLife_23','PEQ_AttitudeLife_24','PEQ_AttitudeLife_24a')
      
    }
    vars.peqlife.pos <- c(paste('PEQ_AttitudeLife_',c(1,3,6,8,10,11,13,15,17,19,22,23),sep=""))
    vars.peqlife.neg <- c(paste('PEQ_AttitudeLife_',c(2,4,5,7,9,12,14,16,18,20,21,24),sep=""))
    
    peq.attitudelife.pos <- rowSums(data[,vars.peqlife.pos])/(5*length(vars.peqlife.pos))
    peq.attitudelife.neg <- rowSums(data[,vars.peqlife.neg])/(5*length(vars.peqlife.neg))
    peqlife.scores.comb <- cbind(peq.attitudelife.pos, peq.attitudelife.neg)
  }
  

  keeps <- data[,keep.vars]
  raw.dat <- data[,peqlife]
  output <- cbind(keeps,raw.dat,peqlife.scores.comb)
  output <- as.data.frame(output)
  return(output)

}
#****************************************************************************
# Persisting Effects: Attitudes about Self
peqself.scoring <- function(data,keep.vars,peqself){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
    # if("dose" %in% colnames(data))
    # {
    #   keep.vars <-c('sub','ID','date', 'dose')
    # }
  
  peq.attitudeself.pos <- c()
  peq.attitudeself.neg <- c()
  raw.colnames <- colnames(data)
  if(length(grep("PEQ_AttitudeSelf_", raw.colnames)) == 33){
    if (missing(peqself)){
      peqself <- c('PEQ_AttitudeSelf_1','PEQ_AttitudeSelf_2','PEQ_AttitudeSelf_2a','PEQ_AttitudeSelf_3','PEQ_AttitudeSelf_4','PEQ_AttitudeSelf_4a',
                 'PEQ_AttitudeSelf_5','PEQ_AttitudeSelf_6','PEQ_AttitudeSelf_6a','PEQ_AttitudeSelf_7','PEQ_AttitudeSelf_8','PEQ_AttitudeSelf_8a',
                 'PEQ_AttitudeSelf_9','PEQ_AttitudeSelf_10','PEQ_AttitudeSelf_10a','PEQ_AttitudeSelf_11','PEQ_AttitudeSelf_12','PEQ_AttitudeSelf_12a',
                 'PEQ_AttitudeSelf_13','PEQ_AttitudeSelf_14','PEQ_AttitudeSelf_14a','PEQ_AttitudeSelf_15','PEQ_AttitudeSelf_16','PEQ_AttitudeSelf_16a',
                 'PEQ_AttitudeSelf_17','PEQ_AttitudeSelf_18','PEQ_AttitudeSelf_18a','PEQ_AttitudeSelf_19','PEQ_AttitudeSelf_20','PEQ_AttitudeSelf_20a',
                 'PEQ_AttitudeSelf_21','PEQ_AttitudeSelf_22','PEQ_AttitudeSelf_22a')
      }

      vars.peqself.pos <- c(paste('PEQ_AttitudeSelf_',c(1,4,6,7,9,12,14,15,17,20,21),sep=""))
      vars.peqself.neg <- c(paste('PEQ_AttitudeSelf_',c(2,3,5,8,10,11,13,16,18,19,22),sep=""))

      peq.attitudeself.pos <- rowSums(data[,vars.peqself.pos])/(5*length(vars.peqself.pos))
      peq.attitudeself.neg <- rowSums(data[,vars.peqself.neg])/(5*length(vars.peqself.neg))
  
    } else if(length(grep("PEQ_AttitudeSelf_", raw.colnames)) == 27) {
      peqself <- c('PEQ_AttitudeSelf_1','PEQ_AttitudeSelf_2','PEQ_AttitudeSelf_2a','PEQ_AttitudeSelf_3','PEQ_AttitudeSelf_4','PEQ_AttitudeSelf_4a',
        'PEQ_AttitudeSelf_5','PEQ_AttitudeSelf_6','PEQ_AttitudeSelf_6a','PEQ_AttitudeSelf_7','PEQ_AttitudeSelf_8','PEQ_AttitudeSelf_8a',
        'PEQ_AttitudeSelf_9','PEQ_AttitudeSelf_10','PEQ_AttitudeSelf_10a','PEQ_AttitudeSelf_11','PEQ_AttitudeSelf_12','PEQ_AttitudeSelf_12a',
        'PEQ_AttitudeSelf_13','PEQ_AttitudeSelf_14','PEQ_AttitudeSelf_14a','PEQ_AttitudeSelf_15','PEQ_AttitudeSelf_16','PEQ_AttitudeSelf_16a',
        'PEQ_AttitudeSelf_17','PEQ_AttitudeSelf_18','PEQ_AttitudeSelf_18a')
        vars.peqself.pos <- c(paste('PEQ_AttitudeSelf_',c(2,3,5,8,10,11,13,16,17),sep=""))
        vars.peqself.neg <- c(paste('PEQ_AttitudeSelf_',c(1,4,6,7,9,12,14,15,18),sep=""))
    
        peq.attitudeself.pos <- rowSums(data[,vars.peqself.pos])/(5*length(vars.peqself.pos))
        peq.attitudeself.neg <- rowSums(data[,vars.peqself.neg])/(5*length(vars.peqself.neg))
    }
  

  keeps <- data[,keep.vars]
  raw.dat <- data[,peqself]
  output <- cbind(keeps,raw.dat,peq.attitudeself.pos,peq.attitudeself.neg)
  output <- as.data.frame(output)
  return(output)

}
#****************************************************************************
# Persisting Effects: Mood Changes
peqmood.scoring <- function(data,keep.vars,peqmood){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
    # if("dose" %in% colnames(data))
    # {
    #   keep.vars <-c('sub','ID','date', 'dose')
    # }
  if (missing(peqmood)){
    peqmood <- c('PEQ_MoodChanges_1','PEQ_MoodChanges_2','PEQ_MoodChanges_2a','PEQ_MoodChanges_3','PEQ_MoodChanges_4','PEQ_MoodChanges_4a',
                 'PEQ_MoodChanges_5','PEQ_MoodChanges_6','PEQ_MoodChanges_6a','PEQ_MoodChanges_7','PEQ_MoodChanges_8','PEQ_MoodChanges_8a',
                 'PEQ_MoodChanges_9','PEQ_MoodChanges_10','PEQ_MoodChanges_10a','PEQ_MoodChanges_11','PEQ_MoodChanges_12','PEQ_MoodChanges_12a',
                 'PEQ_MoodChanges_13','PEQ_MoodChanges_14','PEQ_MoodChanges_14a','PEQ_MoodChanges_15','PEQ_MoodChanges_16','PEQ_MoodChanges_16a',
                 'PEQ_MoodChanges_17','PEQ_MoodChanges_18','PEQ_MoodChanges_18a')
  }

  vars.peqmood.pos <- c(paste('PEQ_MoodChanges_',c(1,3,5,8,10,11,14,16,17),sep=""))
  vars.peqmood.neg <- c(paste('PEQ_MoodChanges_',c(2,4,6,7,9,12,13,15,18),sep=""))

  peq.moodchange.pos <- rowSums(data[,vars.peqmood.pos])/(5*length(vars.peqmood.pos))
  peq.moodchange.neg <- rowSums(data[,vars.peqmood.neg])/(5*length(vars.peqmood.neg))

  keeps <- data[,keep.vars]
  raw.dat <- data[,peqmood]
  output <- cbind(keeps,raw.dat,peq.moodchange.pos,peq.moodchange.neg)
  output <- as.data.frame(output)
  return(output)

}
#****************************************************************************
# Persisting Effects: Relationships
peqrelate.scoring <- function(data,keep.vars,peqrelate){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
  # if("dose" %in% colnames(data))
  # {
  #   keep.vars <-c('sub','ID','date', 'dose')
  # }
  
  #peqlife.scores.comb <- c()
  raw.colnames <- colnames(data)
  
  if((length(grep("PEQ_Relationships_", raw.colnames)) == 27) || (length(grep("PEQ_Relationship_", raw.colnames)) == 27)){ #18-item version
    if (missing(peqrelate)){
      peqrelate <- c('PEQ_Relationships_1','PEQ_Relationships_2','PEQ_Relationships_2a','PEQ_Relationships_3','PEQ_Relationships_4','PEQ_Relationships_4a',
                     'PEQ_Relationships_5','PEQ_Relationships_6','PEQ_Relationships_6a','PEQ_Relationships_7','PEQ_Relationships_8','PEQ_Relationships_8a',
                     'PEQ_Relationships_9','PEQ_Relationships_10','PEQ_Relationships_10a','PEQ_Relationships_11','PEQ_Relationships_12','PEQ_Relationships_12a',
                     'PEQ_Relationships_13','PEQ_Relationships_14','PEQ_Relationships_14a','PEQ_Relationships_15','PEQ_Relationships_16','PEQ_Relationships_16a',
                     'PEQ_Relationships_17','PEQ_Relationships_18','PEQ_Relationships_18a')
    }
    if ('PEQ_Relationship_1' %in% colnames(data)) {
      oldpeqrelate <- c('PEQ_Relationship_1','PEQ_Relationship_2','PEQ_Relationship_2a','PEQ_Relationship_3','PEQ_Relationship_4','PEQ_Relationship_4a',
                        'PEQ_Relationship_5','PEQ_Relationship_6','PEQ_Relationship_6a','PEQ_Relationship_7','PEQ_Relationship_8','PEQ_Relationship_8a',
                        'PEQ_Relationship_9','PEQ_Relationship_10','PEQ_Relationship_10a','PEQ_Relationship_11','PEQ_Relationship_12','PEQ_Relationship_12a',
                        'PEQ_Relationship_13','PEQ_Relationship_14','PEQ_Relationship_14a','PEQ_Relationship_15','PEQ_Relationship_16','PEQ_Relationship_16a',
                        'PEQ_Relationship_17','PEQ_Relationship_18','PEQ_Relationship_18a')
      data <- data.table::setnames(data, old=oldpeqrelate, new=peqrelate)
    }
    
    
    vars.peqrelate.pos <- c(paste('PEQ_Relationships_',c(1,4,5,7,10,12,13,16,18),sep=""))
    vars.peqrelate.neg <- c(paste('PEQ_Relationships_',c(2,3,6,8,9,11,14,15,17),sep=""))
    
    peq.relationships.pos <- rowSums(data[,vars.peqrelate.pos])/(5*length(vars.peqrelate.pos))
    peq.relationships.neg <- rowSums(data[,vars.peqrelate.neg])/(5*length(vars.peqrelate.neg))
    
  } else if (length(grep("PEQ_Relationships_", raw.colnames)) == 30 || (length(grep("PEQ_Relationship_", raw.colnames)) == 30)){ #20-item version
    if (missing(peqrelate)){
      peqrelate <- c('PEQ_Relationships_1','PEQ_Relationships_2','PEQ_Relationships_2a','PEQ_Relationships_3','PEQ_Relationships_4','PEQ_Relationships_4a',
                     'PEQ_Relationships_5','PEQ_Relationships_6','PEQ_Relationships_6a','PEQ_Relationships_7','PEQ_Relationships_8','PEQ_Relationships_8a',
                     'PEQ_Relationships_9','PEQ_Relationships_10','PEQ_Relationships_10a','PEQ_Relationships_11','PEQ_Relationships_12','PEQ_Relationships_12a',
                     'PEQ_Relationships_13','PEQ_Relationships_14','PEQ_Relationships_14a','PEQ_Relationships_15','PEQ_Relationships_16','PEQ_Relationships_16a',
                     'PEQ_Relationships_17','PEQ_Relationships_18','PEQ_Relationships_18a','PEQ_Relationships_19','PEQ_Relationships_20','PEQ_Relationships_20a')
    }
    
    if ('PEQ_Relationship_1' %in% colnames(data)) {
      oldpeqrelate <- c('PEQ_Relationship_1','PEQ_Relationship_2','PEQ_Relationship_2a_4','PEQ_Relationship_3','PEQ_Relationship_4','PEQ_Relationship_4a_4',
                        'PEQ_Relationship_5','PEQ_Relationship_6','PEQ_Relationship_6a_4','PEQ_Relationship_7','PEQ_Relationship_8','PEQ_Relationship_8a_4',
                        'PEQ_Relationship_9','PEQ_Relationship_10','PEQ_Relationship_10a_4','PEQ_Relationship_11','PEQ_Relationship_12','PEQ_Relationship_12a_4',
                        'PEQ_Relationship_13','PEQ_Relationship_14','PEQ_Relationship_14a_4','PEQ_Relationship_15','PEQ_Relationship_16','PEQ_Relationship_16a_4',
                        'PEQ_Relationship_17','PEQ_Relationship_18','PEQ_Relationship_18a_4','PEQ_Relationship_19','PEQ_Relationship_20','PEQ_Relationship_20a_4')
      data <- data.table::setnames(data, old=oldpeqrelate, new=peqrelate)
      
    }
    
    
    vars.peqrelate.pos <- c(paste('PEQ_Relationships_',c(1,4,5,7,10,12,13,16,18,20),sep=""))
    vars.peqrelate.neg <- c(paste('PEQ_Relationships_',c(2,3,6,8,9,11,14,15,17,19),sep=""))
    
    peq.relationships.pos <- rowSums(data[,vars.peqrelate.pos])/(5*length(vars.peqrelate.pos))
    peq.relationships.neg <- rowSums(data[,vars.peqrelate.neg])/(5*length(vars.peqrelate.neg))
  }
  
  keeps <- data[,keep.vars]
  raw.dat <- data[,peqrelate]
  output <- cbind(keeps,raw.dat,peq.relationships.pos,peq.relationships.neg)
  output <- as.data.frame(output)
  return(output)
  
}
#****************************************************************************
# Persisting Effects: Behavior Change
peqbehavior.scoring <- function(data,keep.vars,peqbehavior){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
  # if("dose" %in% colnames(data))
  # {
  #   keep.vars <-c('sub','ID','date', 'dose')
  # }
  if (missing(peqbehavior)){
    peqbehavior <- c('PEQ_BehaviorChange_1','PEQ_BehaviorChange_2','PEQ_BehaviorChange_2a')
  }
  
  if ('PEQ_Behavior_1' %in% colnames(data)) {
    oldpeqbehavior <- c('PEQ_Behavior_1','PEQ_Behavior_2','PEQ_Behavior_2a_4')
    data <- data.table::setnames(data, old=oldpeqbehavior, new=peqbehavior)
    
  }
  
  
  vars.peqbehavior.neg <- c(paste('PEQ_BehaviorChange_',c(1),sep=""))
  vars.peqbehavior.pos <- c(paste('PEQ_BehaviorChange_',c(2),sep=""))
  
  peq.behaviorchange.neg <- data[,vars.peqbehavior.neg]/(5*length(vars.peqbehavior.neg))
  peq.behaviorchange.pos <- data[,vars.peqbehavior.pos]/(5*length(vars.peqbehavior.pos))
  
  keeps <- data[,keep.vars]
  raw.dat <- data[,peqbehavior]
  output <- cbind(peq.behaviorchange.pos, peq.behaviorchange.neg)
  
  if ('PEQ_BehaviorChange_1' %in% colnames(output)) {
    output <- rename(output, peq.behaviorchange.pos='PEQ_BehaviorChange_2')
    output <- rename(output, peq.behaviorchange.neg='PEQ_BehaviorChange_1')
  }
  output <- cbind(keeps,raw.dat,output)
  output <- as.data.frame(output)
  return(output)
  
}
#****************************************************************************
# Persisting Effects: Spirituality 
peqspirit.scoring <- function(data,keep.vars,peqspirit){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  # if("dose" %in% colnames(data))
  # {
  #   keep.vars <-c('sub','ID','date', 'dose')
  # }



  peqspirit.scores.comb <- c()
  raw.colnames <- colnames(data)
  
  if(length(grep("PEQ_Spirituality_", raw.colnames)) == 88 || (length(grep("PEQ_Spiritual_", raw.colnames)) == 88)){ #59-item version
    if (missing(peqspirit)){
      peqspirit <- c('PEQ_Spirituality_1','PEQ_Spirituality_2','PEQ_Spirituality_2a','PEQ_Spirituality_3','PEQ_Spirituality_4','PEQ_Spirituality_4a',
                     'PEQ_Spirituality_5','PEQ_Spirituality_6','PEQ_Spirituality_6a','PEQ_Spirituality_7','PEQ_Spirituality_8','PEQ_Spirituality_8a',
                     'PEQ_Spirituality_9','PEQ_Spirituality_10','PEQ_Spirituality_10a','PEQ_Spirituality_11','PEQ_Spirituality_12','PEQ_Spirituality_12a',
                     'PEQ_Spirituality_13','PEQ_Spirituality_14','PEQ_Spirituality_14a','PEQ_Spirituality_15','PEQ_Spirituality_16','PEQ_Spirituality_16a',
                     'PEQ_Spirituality_17','PEQ_Spirituality_18','PEQ_Spirituality_18a','PEQ_Spirituality_19','PEQ_Spirituality_20','PEQ_Spirituality_20a',
                     'PEQ_Spirituality_21','PEQ_Spirituality_22','PEQ_Spirituality_22a','PEQ_Spirituality_23','PEQ_Spirituality_24','PEQ_Spirituality_24a',
                     'PEQ_Spirituality_25','PEQ_Spirituality_26','PEQ_Spirituality_26a','PEQ_Spirituality_27','PEQ_Spirituality_28','PEQ_Spirituality_28a',
                     'PEQ_Spirituality_29','PEQ_Spirituality_30','PEQ_Spirituality_30a','PEQ_Spirituality_31','PEQ_Spirituality_32','PEQ_Spirituality_32a',
                     'PEQ_Spirituality_33','PEQ_Spirituality_34','PEQ_Spirituality_34a','PEQ_Spirituality_35','PEQ_Spirituality_36','PEQ_Spirituality_36a',
                     'PEQ_Spirituality_37','PEQ_Spirituality_38','PEQ_Spirituality_38a','PEQ_Spirituality_39','PEQ_Spirituality_40','PEQ_Spirituality_40a',
                     'PEQ_Spirituality_41','PEQ_Spirituality_42','PEQ_Spirituality_42a','PEQ_Spirituality_43','PEQ_Spirituality_44','PEQ_Spirituality_45',
                     'PEQ_Spirituality_45a','PEQ_Spirituality_46','PEQ_Spirituality_47','PEQ_Spirituality_47a','PEQ_Spirituality_48','PEQ_Spirituality_49',
                     'PEQ_Spirituality_49a','PEQ_Spirituality_50','PEQ_Spirituality_51','PEQ_Spirituality_51a','PEQ_Spirituality_52','PEQ_Spirituality_53',
                     'PEQ_Spirituality_53a','PEQ_Spirituality_54','PEQ_Spirituality_55','PEQ_Spirituality_55a','PEQ_Spirituality_56','PEQ_Spirituality_57',
                     'PEQ_Spirituality_57a','PEQ_Spirituality_58','PEQ_Spirituality_59','PEQ_Spirituality_59a')
    }
    
    if ('PEQ_Spiritual_1' %in% colnames(data)) {
      oldpeqspirit <- c('PEQ_Spiritual_1','PEQ_Spiritual_2','PEQ_Spiritual_2a','PEQ_Spiritual_3','PEQ_Spiritual_4','PEQ_Spiritual_4a',
                        'PEQ_Spiritual_5','PEQ_Spiritual_6','PEQ_Spiritual_6a','PEQ_Spiritual_7','PEQ_Spiritual_8','PEQ_Spiritual_8a',
                        'PEQ_Spiritual_9','PEQ_Spiritual_10','PEQ_Spiritual_10a','PEQ_Spiritual_11','PEQ_Spiritual_12','PEQ_Spiritual_12a',
                        'PEQ_Spiritual_13','PEQ_Spiritual_14','PEQ_Spiritual_14a','PEQ_Spiritual_15','PEQ_Spiritual_16','PEQ_Spiritual_16a',
                        'PEQ_Spiritual_17','PEQ_Spiritual_18','PEQ_Spiritual_18a','PEQ_Spiritual_19','PEQ_Spiritual_20','PEQ_Spiritual_20a',
                        'PEQ_Spiritual_21','PEQ_Spiritual_22','PEQ_Spiritual_22a','PEQ_Spiritual_23','PEQ_Spiritual_24','PEQ_Spiritual_24a',
                        'PEQ_Spiritual_25','PEQ_Spiritual_26','PEQ_Spiritual_26a','PEQ_Spiritual_27','PEQ_Spiritual_28','PEQ_Spiritual_28a',
                        'PEQ_Spiritual_29','PEQ_Spiritual_30','PEQ_Spiritual_30a','PEQ_Spiritual_31','PEQ_Spiritual_32','PEQ_Spiritual_32a',
                        'PEQ_Spiritual_33','PEQ_Spiritual_34','PEQ_Spiritual_34a','PEQ_Spiritual_35','PEQ_Spiritual_36','PEQ_Spiritual_36a',
                        'PEQ_Spiritual_37','PEQ_Spiritual_38','PEQ_Spiritual_38a','PEQ_Spiritual_39','PEQ_Spiritual_40','PEQ_Spiritual_40a',
                        'PEQ_Spiritual_41','PEQ_Spiritual_42','PEQ_Spiritual_42a','PEQ_Spiritual_43','PEQ_Spiritual_44','PEQ_Spiritual_45',
                        'PEQ_Spiritual_45a','PEQ_Spiritual_46','PEQ_Spiritual_47','PEQ_Spiritual_47a','PEQ_Spiritual_48','PEQ_Spiritual_49',
                        'PEQ_Spiritual_49a','PEQ_Spiritual_50','PEQ_Spiritual_51','PEQ_Spiritual_51a','PEQ_Spiritual_52','PEQ_Spiritual_53',
                        'PEQ_Spiritual_53a','PEQ_Spiritual_54','PEQ_Spiritual_55','PEQ_Spiritual_55a','PEQ_Spiritual_56','PEQ_Spiritual_57',
                        'PEQ_Spiritual_57a','PEQ_Spiritual_58','PEQ_Spiritual_59','PEQ_Spiritual_59a')
      data <- data.table::setnames(data, old=oldpeqspirit, new=peqspirit)
      
    }

    vars.peqspirit.pos <- c(paste('PEQ_Spirituality_',c(1,4,6,7,10,12,13,16,18,19,21,23,25,28,30,31,34,35,
                                                        37,40,41,43,44,46,49,51,52,54,57,59),sep=""))
    vars.peqspirit.neg <- c(paste('PEQ_Spirituality_',c(2,3,5,8,9,11,14,15,17,20,22,24,26,27,29,32,33,36,
                                                        38,39,42,45,47,48,50,53,55,56,58),sep=""))
    #z <- duplicated(c(vars.peqspirit.neg,vars.peqspirit.pos)); length(z[z==TRUE]) #checks for any duplicate variables

    peq.spirituality.pos <- rowSums(data[,vars.peqspirit.pos])/(5*length(vars.peqspirit.pos))
    peq.spirituality.neg <- rowSums(data[,vars.peqspirit.neg])/(5*length(vars.peqspirit.neg))
    peqspirit.scores.comb <- cbind(peq.spirituality.pos, peq.spirituality.neg)
    
 } else if(length(grep("PEQ_Spirituality_", raw.colnames)) == 90 || (length(grep("PEQ_Spiritual_", raw.colnames)) == 90)){ #60-item version
   if (missing(peqspirit)){
     peqspirit <- c('PEQ_Spirituality_1','PEQ_Spirituality_2','PEQ_Spirituality_2a','PEQ_Spirituality_3','PEQ_Spirituality_4','PEQ_Spirituality_4a',
                    'PEQ_Spirituality_5','PEQ_Spirituality_6','PEQ_Spirituality_6a','PEQ_Spirituality_7','PEQ_Spirituality_8','PEQ_Spirituality_8a',
                    'PEQ_Spirituality_9','PEQ_Spirituality_10','PEQ_Spirituality_10a','PEQ_Spirituality_11','PEQ_Spirituality_12','PEQ_Spirituality_12a',
                    'PEQ_Spirituality_13','PEQ_Spirituality_14','PEQ_Spirituality_14a','PEQ_Spirituality_15','PEQ_Spirituality_16','PEQ_Spirituality_16a',
                    'PEQ_Spirituality_17','PEQ_Spirituality_18','PEQ_Spirituality_18a','PEQ_Spirituality_19','PEQ_Spirituality_20','PEQ_Spirituality_20a',
                    'PEQ_Spirituality_21','PEQ_Spirituality_22','PEQ_Spirituality_22a','PEQ_Spirituality_23','PEQ_Spirituality_24','PEQ_Spirituality_24a',
                    'PEQ_Spirituality_25','PEQ_Spirituality_26','PEQ_Spirituality_26a','PEQ_Spirituality_27','PEQ_Spirituality_28','PEQ_Spirituality_28a',
                    'PEQ_Spirituality_29','PEQ_Spirituality_30','PEQ_Spirituality_30a','PEQ_Spirituality_31','PEQ_Spirituality_32','PEQ_Spirituality_32a',
                    'PEQ_Spirituality_33','PEQ_Spirituality_34','PEQ_Spirituality_34a','PEQ_Spirituality_35','PEQ_Spirituality_36','PEQ_Spirituality_36a',
                    'PEQ_Spirituality_37','PEQ_Spirituality_38','PEQ_Spirituality_38a','PEQ_Spirituality_39','PEQ_Spirituality_40','PEQ_Spirituality_40a',
                    'PEQ_Spirituality_41','PEQ_Spirituality_42','PEQ_Spirituality_42a','PEQ_Spirituality_43','PEQ_Spirituality_44','PEQ_Spirituality_44a',
                    'PEQ_Spirituality_45', 'PEQ_Spirituality_46', 'PEQ_Spirituality_46a'
                    ,'PEQ_Spirituality_47','PEQ_Spirituality_48', 'PEQ_Spirituality_48a',,'PEQ_Spirituality_49', 'PEQ_Spirituality_50','PEQ_Spirituality_50a',
                    'PEQ_Spirituality_51','PEQ_Spirituality_52','PEQ_Spirituality_52a','PEQ_Spirituality_53', 'PEQ_Spirituality_54','PEQ_Spirituality_54a',
                    'PEQ_Spirituality_55','PEQ_Spirituality_56','PEQ_Spirituality_56a','PEQ_Spirituality_57', 'PEQ_Spirituality_58','PEQ_Spirituality_58a',
                    'PEQ_Spirituality_59','PEQ_Spirituality_60', 'PEQ_Spirituality_60a')
   }
   
   if ('PEQ_Spiritual_1' %in% colnames(data)) {
     oldpeqspirit <- c('PEQ_Spiritual_1','PEQ_Spiritual_2','PEQ_Spiritual_2a','PEQ_Spiritual_3','PEQ_Spiritual_4','PEQ_Spiritual_4a',
                                    'PEQ_Spiritual_5','PEQ_Spiritual_6','PEQ_Spiritual_6a','PEQ_Spiritual_7','PEQ_Spiritual_8','PEQ_Spiritual_8a',
                                    'PEQ_Spiritual_9','PEQ_Spiritual_10','PEQ_Spiritual_10a','PEQ_Spiritual_11','PEQ_Spiritual_12','PEQ_Spiritual_12a',
                                    'PEQ_Spiritual_13','PEQ_Spiritual_14','PEQ_Spiritual_14a','PEQ_Spiritual_15','PEQ_Spiritual_16','PEQ_Spiritual_16a',
                                    'PEQ_Spiritual_17','PEQ_Spiritual_18','PEQ_Spiritual_18a','PEQ_Spiritual_19','PEQ_Spiritual_20','PEQ_Spiritual_20a',
                                    'PEQ_Spiritual_21','PEQ_Spiritual_22','PEQ_Spiritual_22a','PEQ_Spiritual_23','PEQ_Spiritual_24','PEQ_Spiritual_24a',
                                    'PEQ_Spiritual_25','PEQ_Spiritual_26','PEQ_Spiritual_26a','PEQ_Spiritual_27','PEQ_Spiritual_28','PEQ_Spiritual_28a',
                                    'PEQ_Spiritual_29','PEQ_Spiritual_30','PEQ_Spiritual_30a','PEQ_Spiritual_31','PEQ_Spiritual_32','PEQ_Spiritual_32a',
                                    'PEQ_Spiritual_33','PEQ_Spiritual_34','PEQ_Spiritual_34a','PEQ_Spiritual_35','PEQ_Spiritual_36','PEQ_Spiritual_36a',
                                    'PEQ_Spiritual_37','PEQ_Spiritual_38','PEQ_Spiritual_38a','PEQ_Spiritual_39','PEQ_Spiritual_40','PEQ_Spiritual_40a',
                                    'PEQ_Spiritual_41','PEQ_Spiritual_42','PEQ_Spiritual_42a','PEQ_Spiritual_43','PEQ_Spiritual_44','PEQ_Spiritual_44a',
                                    'PEQ_Spiritual_45', 'PEQ_Spiritual_46', 'PEQ_Spiritual_46a'
                                    ,'PEQ_Spiritual_47','PEQ_Spiritual_48', 'PEQ_Spiritual_48a',,'PEQ_Spiritual_49', 'PEQ_Spiritual_50','PEQ_Spiritual_50a',
                                    'PEQ_Spiritual_51','PEQ_Spiritual_52','PEQ_Spiritual_52a','PEQ_Spiritual_53', 'PEQ_Spiritual_54','PEQ_Spiritual_54a',
                                    'PEQ_Spiritual_55','PEQ_Spiritual_56','PEQ_Spiritual_56a','PEQ_Spiritual_57', 'PEQ_Spiritual_58','PEQ_Spiritual_58a',
                                    'PEQ_Spiritual_59','PEQ_Spiritual_60', 'PEQ_Spiritual_60a')
     data <- data.table::setnames(data, old=oldpeqspirit, new=peqspirit)
     
   }
   
   vars.peqspirit.pos <- c(paste('PEQ_Spirituality_',c(1,4,6,7,10,12,13,16,18,19,21,23,25,28,30,31,34,35,
                                                       37,40,41,43,45,47,50,52,53,55,58,60),sep=""))
   vars.peqspirit.neg <- c(paste('PEQ_Spirituality_',c(2,3,5,8,9,11,14,15,17,20,22,24,26,27,29,32,33,36,
                                                       38,39,42,44,46,48,49,51,54,56,57,59),sep=""))
 
   peq.spirituality.pos <- rowSums(data[,vars.peqspirit.pos])/(5*length(vars.peqspirit.pos))
   peq.spirituality.neg <- rowSums(data[,vars.peqspirit.neg])/(5*length(vars.peqspirit.neg))
   peqspirit.scores.comb <- cbind(peq.spirituality.pos, peq.spirituality.neg)
   
 } else if(length(grep("PEQ_Spirituality_", raw.colnames)) == 85 || (length(grep("PEQ_Spiritual_", raw.colnames)) == 85)) { #57-item version
   if (missing(peqspirit)){
     peqspirit <- c('PEQ_Spirituality_1','PEQ_Spirituality_2','PEQ_Spirituality_2a','PEQ_Spirituality_3','PEQ_Spirituality_4','PEQ_Spirituality_4a',
                    'PEQ_Spirituality_5','PEQ_Spirituality_6','PEQ_Spirituality_6a','PEQ_Spirituality_7','PEQ_Spirituality_8','PEQ_Spirituality_8a',
                    'PEQ_Spirituality_9','PEQ_Spirituality_10','PEQ_Spirituality_10a','PEQ_Spirituality_11','PEQ_Spirituality_12','PEQ_Spirituality_12a',
                    'PEQ_Spirituality_13','PEQ_Spirituality_14','PEQ_Spirituality_14a','PEQ_Spirituality_15','PEQ_Spirituality_16','PEQ_Spirituality_16a',
                    'PEQ_Spirituality_17','PEQ_Spirituality_18','PEQ_Spirituality_18a','PEQ_Spirituality_19','PEQ_Spirituality_20','PEQ_Spirituality_20a',
                    'PEQ_Spirituality_21','PEQ_Spirituality_22','PEQ_Spirituality_22a','PEQ_Spirituality_23','PEQ_Spirituality_24','PEQ_Spirituality_24a',
                    'PEQ_Spirituality_25','PEQ_Spirituality_26','PEQ_Spirituality_26a','PEQ_Spirituality_27','PEQ_Spirituality_28','PEQ_Spirituality_28a',
                    'PEQ_Spirituality_29','PEQ_Spirituality_30','PEQ_Spirituality_30a','PEQ_Spirituality_31','PEQ_Spirituality_32','PEQ_Spirituality_32a',
                    'PEQ_Spirituality_33','PEQ_Spirituality_34','PEQ_Spirituality_34a','PEQ_Spirituality_35','PEQ_Spirituality_36','PEQ_Spirituality_36a',
                    'PEQ_Spirituality_37','PEQ_Spirituality_38','PEQ_Spirituality_38a','PEQ_Spirituality_39','PEQ_Spirituality_40','PEQ_Spirituality_40a',
                    'PEQ_Spirituality_41','PEQ_Spirituality_42','PEQ_Spirituality_43','PEQ_Spirituality_43a','PEQ_Spirituality_44','PEQ_Spirituality_45',
                    'PEQ_Spirituality_45a','PEQ_Spirituality_46','PEQ_Spirituality_47','PEQ_Spirituality_47a','PEQ_Spirituality_48','PEQ_Spirituality_49',
                    'PEQ_Spirituality_49a','PEQ_Spirituality_50','PEQ_Spirituality_51','PEQ_Spirituality_51a','PEQ_Spirituality_52','PEQ_Spirituality_53',
                    'PEQ_Spirituality_53a','PEQ_Spirituality_54','PEQ_Spirituality_55','PEQ_Spirituality_55a','PEQ_Spirituality_56','PEQ_Spirituality_57',
                    'PEQ_Spirituality_57a')
   }
   
   if ('PEQ_Spiritual_1' %in% colnames(data)) {
     oldpeqspirit <- c('PEQ_Spiritual_1','PEQ_Spiritual_2','PEQ_Spiritual_2a','PEQ_Spiritual_3','PEQ_Spiritual_4','PEQ_Spiritual_4a',
                       'PEQ_Spiritual_5','PEQ_Spiritual_6','PEQ_Spiritual_6a','PEQ_Spiritual_7','PEQ_Spiritual_8','PEQ_Spiritual_8a',
                       'PEQ_Spiritual_9','PEQ_Spiritual_10','PEQ_Spiritual_10a','PEQ_Spiritual_11','PEQ_Spiritual_12','PEQ_Spiritual_12a',
                       'PEQ_Spiritual_13','PEQ_Spiritual_14','PEQ_Spiritual_14a','PEQ_Spiritual_15','PEQ_Spiritual_16','PEQ_Spiritual_16a',
                       'PEQ_Spiritual_17','PEQ_Spiritual_18','PEQ_Spiritual_18a','PEQ_Spiritual_19','PEQ_Spiritual_20','PEQ_Spiritual_20a',
                       'PEQ_Spiritual_21','PEQ_Spiritual_22','PEQ_Spiritual_22a','PEQ_Spiritual_23','PEQ_Spiritual_24','PEQ_Spiritual_24a',
                       'PEQ_Spiritual_25','PEQ_Spiritual_26','PEQ_Spiritual_26a','PEQ_Spiritual_27','PEQ_Spiritual_28','PEQ_Spiritual_28a',
                       'PEQ_Spiritual_29','PEQ_Spiritual_30','PEQ_Spiritual_30a','PEQ_Spiritual_31','PEQ_Spiritual_32','PEQ_Spiritual_32a',
                       'PEQ_Spiritual_33','PEQ_Spiritual_34','PEQ_Spiritual_34a','PEQ_Spiritual_35','PEQ_Spiritual_36','PEQ_Spiritual_36a',
                       'PEQ_Spiritual_37','PEQ_Spiritual_38','PEQ_Spiritual_38a','PEQ_Spiritual_39','PEQ_Spiritual_40','PEQ_Spiritual_40a',
                       'PEQ_Spiritual_41','PEQ_Spiritual_42','PEQ_Spiritual_43','PEQ_Spiritual_43a','PEQ_Spiritual_44','PEQ_Spiritual_45',
                       'PEQ_Spiritual_45a','PEQ_Spiritual_46','PEQ_Spiritual_47','PEQ_Spiritual_47a','PEQ_Spiritual_48','PEQ_Spiritual_49',
                       'PEQ_Spiritual_49a','PEQ_Spiritual_50','PEQ_Spiritual_51','PEQ_Spiritual_51a','PEQ_Spiritual_52','PEQ_Spiritual_53',
                       'PEQ_Spiritual_53a','PEQ_Spiritual_54','PEQ_Spiritual_55','PEQ_Spiritual_55a','PEQ_Spiritual_56','PEQ_Spiritual_57',
                       'PEQ_Spiritual_57a')
     data <- data.table::setnames(data, old=oldpeqspirit, new=peqspirit)
     
   }
   
   vars.peqspirit.pos <- c(paste('PEQ_Spirituality_',c(1,4,6,7,10,12,13,16,17,19,21,23,26,28,29,32,
                                                       33,35,38,39,41,42,44,47,49,50,52,55,57),sep=""))
   vars.peqspirit.neg <- c(paste('PEQ_Spirituality_',c(2,3,5,8,9,11,14,15,18,20,22,24,25,27,30,31,
                                                       34,36,37,40,43,45,46,48,51,53,54,56),sep=""))
   #z <- duplicated(c(vars.peqspirit.neg,vars.peqspirit.pos)); length(z[z==TRUE]) #checks for any duplicate variables
   
   peq.spirituality.pos <- rowSums(data[,vars.peqspirit.pos])/(5*length(vars.peqspirit.pos))
   peq.spirituality.neg <- rowSums(data[,vars.peqspirit.neg])/(5*length(vars.peqspirit.neg))
   peqspirit.scores.comb <- cbind(peq.spirituality.pos, peq.spirituality.neg)
  }

  keeps <- data[,keep.vars]
  raw.dat <- data[,peqspirit]
  output <- cbind(keeps,raw.dat,peqspirit.scores.comb)
  output <- as.data.frame(output)
  return(output)

}
#****************************************************************************
# Persisting Effects: Miscellaneous 
peqmisc.scoring <- function(data,keep.vars,peqmisc){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  # if("dose" %in% colnames(data))
  # {
  #   keep.vars <-c('sub','ID','date', 'dose')
  # }

  raw.colnames <- colnames(data)
  peqmisc.comb <- c()
  
  if(length(grep("PEQ_Misc_", raw.colnames)) == 17) { #13-item version
    if (missing(peqmisc)){
      peqmisc <- c('PEQ_Misc_1','PEQ_Misc_2','PEQ_Misc_2a','PEQ_Misc_3','PEQ_Misc_4','PEQ_Misc_4a','PEQ_Misc_5',
                   'PEQ_Misc_6','PEQ_Misc_6a','PEQ_Misc_7','PEQ_Misc_8','PEQ_Misc_8a','PEQ_Misc_9','PEQ_Misc_10',
                   'PEQ_Misc_11','PEQ_Misc_12','PEQ_Misc_13')
    }

    vars.peqmisc.pos <- c(paste('PEQ_Misc_',c(1,4,6,7,9:13),sep=""))
    vars.peqmisc.neg <- c(paste('PEQ_Misc_',c(2,3,5,8),sep=""))

    peq.misc.pos <- rowSums(data[,vars.peqmisc.pos])/(5*length(vars.peqmisc.pos))
    peq.misc.neg <- rowSums(data[,vars.peqmisc.neg])/(5*length(vars.peqmisc.neg))
    peqmisc.comb <- cbind(peq.misc.pos, peq.misc.neg)
    
  } else if(length(grep("PEQ_Misc_", raw.colnames)) == 11) { #9-item version
    if (missing(peqmisc)){
      peqmisc <- c('PEQ_Misc_1','PEQ_Misc_2','PEQ_Misc_2a','PEQ_Misc_3','PEQ_Misc_4','PEQ_Misc_4a',c(paste('PEQ_Misc_',5:9,sep="")))
    }
    vars.peqmisc.pos <- c(paste('PEQ_Misc_',c(1,4,5:9),sep=""))
    vars.peqmisc.neg <- c(paste('PEQ_Misc_',c(2,3),sep=""))

    peq.misc.pos <- rowSums(data[,vars.peqmisc.pos])/(5*length(vars.peqmisc.pos))
    peq.misc.neg <- rowSums(data[,vars.peqmisc.neg])/(5*length(vars.peqmisc.neg))
    peqmisc.comb <- cbind(peq.misc.pos, peq.misc.neg)
    
  } else if(length(grep("PEQ_Misc_", raw.colnames)) == 8) {
    if (missing(peqmisc)){
      peqmisc <- c('PEQ_Misc_1','PEQ_Misc_2','PEQ_Misc_2a','PEQ_Misc_3','PEQ_Misc_4','PEQ_Misc_4a','PEQ_Misc_5',
                   'PEQ_Misc_6')
    }
    
    vars.peqmisc.pos <- c(paste('PEQ_Misc_',c(1,4,5,6),sep=""))
    vars.peqmisc.neg <- c(paste('PEQ_Misc_',c(2,3),sep=""))
    
    peq.misc.pos <- rowSums(data[,vars.peqmisc.pos])/(5*length(vars.peqmisc.pos))
    peq.misc.neg <- rowSums(data[,vars.peqmisc.neg])/(5*length(vars.peqmisc.neg))
    peqmisc.comb <- cbind(peq.misc.pos, peq.misc.neg)
  } else if(length(grep("PEQ_Misc_", raw.colnames)) == 5) {
    if (missing(peqmisc)){
      peqmisc <- c('PEQ_Misc_1','PEQ_Misc_2','PEQ_Misc_3','PEQ_Misc_4','PEQ_Misc_5')
    }
    
    vars.peqmisc.pos <- c(paste('PEQ_Misc_',1:5,sep=""))
    
    peq.misc.pos <- rowSums(data[,vars.peqmisc.pos])/(5*length(vars.peqmisc.pos))
    peqmisc.comb <- cbind(peq.misc.pos)
  }

  keeps <- data[,keep.vars]
  raw.dat <- data[,peqmisc]
  output <- cbind(keeps,raw.dat,peqmisc.comb)
  output <- as.data.frame(output)
  return(output)

}
#****************************************************************************
# Persisting Effects: Religion
peqreligion.scoring <- function(data,keep.vars,peqrelig){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
  if (missing(peqrelig)){
    peqrelig <- c('PEQ_Religion_1', 'PEQ_Religion_2','PEQ_Religion_2a','PEQ_Religion_3','PEQ_Religion_4','PEQ_Religion_4a',
                  'PEQ_Religion_5','PEQ_Religion_6','PEQ_Religion_6a','PEQ_Religion_7','PEQ_Religion_8','PEQ_Religion_8a',
                  'PEQ_Religion_9','PEQ_Religion_10','PEQ_Religion_10a','PEQ_Religion_11','PEQ_Religion_12','PEQ_Religion_12a')
  }
  
  vars.peqrelig.pos <- c(paste('PEQ_Religion_',c(1,3,5,7,10,11),sep=""))
  vars.peqrelig.neg <- c(paste('PEQ_Religion_',c(2,4,6,8,9,12),sep=""))
  
  peq.religion.pos <- rowSums(data[,vars.peqrelig.pos])/(5*length(vars.peqrelig.pos))
  peq.religion.neg <- rowSums(data[,vars.peqrelig.neg])/(5*length(vars.peqrelig.neg))
  
  keeps <- data[,keep.vars]
  raw.dat <- data[,peqrelig]
  output <- cbind(keeps,raw.dat, peq.religion.pos,peq.religion.neg)
  output <- as.data.frame(output)
  return(output)
}

#****************************************************************************
# # PANAS-X (Positive and Negative Affect States Expanded)
panas.scoring <- function(data, keep.vars, panas){
  keep.vars <- c('ID','date')
  if ('time.point' %in% colnames(data))
  {
    keep.vars <- c(keep.vars,'time.point')
  }
  if ('sub' %in% colnames(data))
  {
    keep.vars <- c('sub', keep.vars)
  }
  if (missing(panas)){
    panas <- paste('PANAS_',1:60,sep="")
  }
  raw.dat <- data[,panas]
  
  #General Dimension Scales
  vars.panasNegaff <- paste('PANAS_', c(11,  18,  26,  32,  34,  38,  40,  42,  44,  50), sep="")
  vars.panasPosaff <- paste('PANAS_', c(3,  8,  13,  25,  31,  37,  39,  47,  52,  55), sep="")
  
  # Basic Negative Emotion Scales
  vars.panasFear <- paste('PANAS_', c(18,  21,  34,  40,  44,  53), sep="")
  vars.panasHostility <- paste('PANAS_', c(2,  9,  11,  27,  38,  56), sep="")
  vars.panasGuilt <- paste('PANAS_', c(15,  32,  42,  46,  51,  60), sep="")
  vars.panasSadness <- paste('PANAS_', c(16,  24,  29,  35,  48), sep="")
  
  # Basic Positive Emotion Scales
  vars.panasJovial <- paste('PANAS_', c(1,  12,  22,  33,  37,  41,  47,  58), sep="")
  vars.panasSelfAssurance <- paste('PANAS_', c(6,  8,  14,  28,  39,  57), sep="")
  vars.panasAttentiveness <- paste('PANAS_', c(3,  25,  52,  59), sep="")
  
  # Other Affective Scales
  vars.panasShyness <- paste('PANAS_', c(4,  23,  30,  49), sep="")
  vars.panasFatigue <- paste('PANAS_', c(5,  19,  36,  45), sep="")
  vars.panasSerenity <- paste('PANAS_', c(10,  17,  43), sep="")
  vars.panasSurprise <- paste('PANAS_', c(7,  20,  54), sep="")
  
  # Calculate sum scores for each subscale
  panas.negAffect <- rowSums(data[,vars.panasNegaff])
  panas.posAffect <- rowSums(data[,vars.panasPosaff])
  panas.fear <- rowSums(data[,vars.panasFear])
  panas.hostility <- rowSums(data[,vars.panasHostility])
  panas.guilt <- rowSums(data[,vars.panasGuilt])
  panas.sadness <- rowSums(data[,vars.panasSadness])
  panas.jovial <- rowSums(data[,vars.panasJovial])
  panas.selfAssurance <- rowSums(data[,vars.panasSelfAssurance])
  panas.attentiveness <- rowSums(data[,vars.panasAttentiveness])
  panas.shyness <- rowSums(data[,vars.panasShyness])
  panas.fatigue <- rowSums(data[,vars.panasFatigue])
  panas.serenity <- rowSums(data[,vars.panasSerenity])
  panas.surprise <- rowSums(data[,vars.panasSurprise])
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps,raw.dat,panas.negAffect,panas.posAffect,panas.fear,panas.hostility,panas.guilt,
                  panas.sadness,panas.jovial,panas.selfAssurance,panas.attentiveness,panas.shyness,
                  panas.fatigue,panas.serenity,panas.surprise)
  output <- as.data.frame(output)
  return(output)
}

#****************************************************************************
# PANAS-X (Positive and Negative Affect States Expanded - Score by Name)
panasByname.scoring <- function(data,keep.vars,panas){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }

  if (missing(panas)){
    panas.items <- c('cheerful','disgusted','attentive','bashful','sluggish','daring',
                     'surprised','strong','scornful','relaxed','irritable','delighted',
                     'inspired','fearless','disgusted with self','sad','calm','afraid',
                     'tired','amazed','shaky','happy','timid','alone','alert','upset',
                     'angry','bold','blue','shy','active','guilty','joyful','nervous',
                     'lonely','sleepy','excited','hostile','proud','jittery','lively',
                     'ashamed','at ease','scared','drowsy','angry at self','enthusiastic',
                     'downhearted','sheepish','distressed','blameworthy','determined',
                     'frightened','astonished','interested','loathing','confident',
                     'energetic','concentrating','dissatisfied with self')
    panas <- c(paste('PANAS_',panas.items,sep=""))
  }
  raw.dat <- data[,panas]

  #General Dimension Scales
  vars.panasNegaff <- c(paste('PANAS_',c('afraid','scared','nervous','jittery','irritable','hostile','guilty','ashamed','upset','distressed'),sep=""))
  vars.panasPosaff <- c(paste('PANAS_',c('active', 'alert', 'attentive', 'determined', 'enthusiastic', 'excited', 'inspired', 'interested', 'proud', 'strong'),sep=""))

  #Basic Negative Emotion Scales
  vars.panasFear <- c(paste('PANAS_',c('afraid', 'scared', 'frightened', 'nervous', 'jittery', 'shaky'),sep=""))
  vars.panasHostility <- c(paste('PANAS_',c('angry', 'hostile', 'irritable', 'scornful', 'disgusted', 'loathing'),sep=""))
  vars.panasGuilt <- c(paste('PANAS_',c('guilty', 'ashamed', 'blameworthy', 'angry at self', 'disgusted with self', 'dissatisfied with self'),sep=""))
  vars.panasSadness <- c(paste('PANAS_',c('sad', 'blue', 'downhearted', 'alone', 'lonely'),sep=""))

  #Basic Positive Emotional Scales
  vars.panasJovial <- c(paste('PANAS_',c('happy', 'joyful', 'delighted', 'cheerful', 'excited', 'enthusiastic', 'lively', 'energetic'),sep=""))
  vars.panasSelfAssurance <- c(paste('PANAS_',c('proud', 'strong', 'confident', 'bold', 'daring', 'fearless'),sep=""))
  vars.panasAttentiveness <- c(paste('PANAS_',c('alert', 'attentive', 'concentrating', 'determined'),sep=""))

  #Other Affective Scales
  vars.panasShyness <- c(paste('PANAS_',c('shy', 'bashful', 'sheepish', 'timid'),sep=""))
  vars.panasFatigue <- c(paste('PANAS_',c('sleepy', 'tired', 'sluggish', 'drowsy'),sep=""))
  vars.panasSerenity <- c(paste('PANAS_',c('calm', 'relaxed', 'at ease'),sep=""))
  vars.panasSurprise <- c(paste('PANAS_',c('amazed', 'surprised', 'astonished'),sep=""))

  panas.negaffect <- rowSums(data[,vars.panasNegaff])
  panas.posaffect <- rowSums(data[,vars.panasPosaff])
  panas.fear <- rowSums(data[,vars.panasFear])
  panas.hostility <- rowSums(data[,vars.panasHostility])
  panas.guilt <- rowSums(data[,vars.panasGuilt])
  panas.sadness <- rowSums(data[,vars.panasSadness])
  panas.jovial <- rowSums(data[,vars.panasJovial])
  panas.selfassurance <- rowSums(data[,vars.panasSelfAssurance])
  panas.attentiveness <- rowSums(data[,vars.panasAttentiveness])
  panas.shyness <- rowSums(data[,vars.panasShyness])
  panas.fatigue <- rowSums(data[,vars.panasFatigue])
  panas.serenity <- rowSums(data[,vars.panasSerenity])
  panas.surprise <- rowSums(data[,vars.panasSurprise])

  keeps <- data[,keep.vars]
  
  output <- cbind(keeps,raw.dat,panas.negaffect,panas.posaffect,panas.fear,panas.hostility,panas.guilt,
                  panas.sadness,panas.jovial,panas.selfassurance,panas.attentiveness,panas.shyness,
                  panas.fatigue,panas.serenity,panas.surprise)
  output <- as.data.frame(output)
  return(output)
}

################################################################################
# Paranormal Spiritual Scale 
## part of belief change measures block in Qualtrics
################################################################################
paranormalSpiritual.scoring <- function(data, keep.vars, paranormalSpiritual){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <- c(keep.vars, 'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub', keep.vars)
  }
  
  if (missing(paranormalSpiritual)){
    paranormalSpiritual <- paste0('ParanormalSpiritual_',1:5,sep="")
  }
  
  raw.dat <- data[,paranormalSpiritual]

  paranormalSpiritual.total <- rowSums(data[,paranormalSpiritual])/ length(paranormalSpiritual)
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps,raw.dat,paranormalSpiritual.total)
  output <- as.data.frame(output)
  return(output)
}


################################################################################
# Post-traumatic Maladaptive Beliefs Scale (PMBS)
################################################################################
pmbs.scoring <- function(data, keep.vars, pmbs){
  
        keep.vars <- c('ID','date')
        if('time.point' %in% colnames(data))
        {
                keep.vars <- c(keep.vars, 'time.point')
        }
        if('sub' %in% colnames(data))
        {
                keep.vars <- c('sub', keep.vars)
        }
        
        if (missing(pmbs)){
                pmbs <- paste('PMBS_',1:15,sep="")
        }
        
        raw.dat <- data[,pmbs]
        
        revpmbs <- c(paste('PMBS_',c(2,3,6,7,11,13,14,15), sep=""))
        data[,revpmbs] = 8-data[,revpmbs]
        
        # subscales
        vars.pmbsThreatOfHarm <- paste('PMBS_',c(1,4,5,8,12),sep="")
        vars.pmbsSelfWorthAndJudgment <- paste('PMBS_',c(3,7,9,10,15),sep="")
        vars.pmbsReliabilityAndTrustworthinessOfOthers <- paste('PMBS_',c(2,6,11,13,14),sep="")
        
        # calculate averages
        pmbs.threatOfHarm <- rowSums(data[,vars.pmbsThreatOfHarm])/length(vars.pmbsThreatOfHarm)
        pmbs.selfWorthAndJudgment <- rowSums(data[,vars.pmbsSelfWorthAndJudgment])/length(vars.pmbsSelfWorthAndJudgment)
        pmbs.reliabilityAndTrustworthinessOfOthers <- rowSums(data[,vars.pmbsReliabilityAndTrustworthinessOfOthers])/ length(vars.pmbsReliabilityAndTrustworthinessOfOthers)
        
        pmbs.total <- rowSums(data[,pmbs])/ length(pmbs)
        
        keeps <- data[,keep.vars]
        
        output <- cbind(keeps,raw.dat,pmbs.threatOfHarm,pmbs.selfWorthAndJudgment,pmbs.reliabilityAndTrustworthinessOfOthers,pmbs.total)
        output <- as.data.frame(output)
        return(output)
}

#****************************************************************************
# POMS (Profile of Mood States)
poms.scoring <- function(data, keep.vars, poms){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <- c(keep.vars, 'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub', keep.vars)
  }
  
  if (missing(poms)){
    poms <- paste('POMS_',1:65,sep="")
  }
  raw.dat <- data[,poms]
  
  revpoms <- c('POMS_22','POMS_54')
  data[,revpoms] = 4-data[,revpoms]
  
  vars.pomsTension <- paste('POMS_',c(2, 10, 16, 20, 22, 26, 27, 34, 41),sep="")
  vars.pomsDepression <- paste('POMS_',c(5, 9, 14, 18, 21, 23, 32, 35, 36, 44, 45, 48, 58, 61, 62),sep="")
  vars.pomsAnger <- paste('POMS_',c(3,  12,  17,  24,  31,  33,  39,  42,  47,  52,  53,  57),sep="")
  vars.pomsFatigue <- paste('POMS_',c(4,  11,  29,  40,  46,  49,  65),sep="")
  vars.pomsConfusion <- paste('POMS_',c(8,  28,  37,  50,  54,  59,  64),sep="")
  vars.pomsVigor <- paste('POMS_',c(7,  15,  19,  38,  51,  56,  60,  63),sep="")
  
  #total mood disturbance (TMD)
  vars.pomsTMDneg <- c(vars.pomsTension,vars.pomsDepression,vars.pomsAnger,vars.pomsFatigue,vars.pomsConfusion)
  vars.pomsTMDpos <- vars.pomsVigor
  
  # calculate sum scores
  poms.tension <- rowSums(data[,vars.pomsTension])
  poms.depression <- rowSums(data[,vars.pomsDepression])
  poms.anger <- rowSums(data[,vars.pomsAnger])
  poms.fatigue <- rowSums(data[,vars.pomsFatigue])
  poms.confusion <- rowSums(data[,vars.pomsConfusion])
  poms.vigor <- rowSums(data[,vars.pomsVigor])
  poms.tmd <- rowSums(data[,vars.pomsTMDneg])-rowSums(data[,vars.pomsTMDpos])
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps,raw.dat,poms.tension,poms.depression,poms.anger,poms.fatigue,
                  poms.confusion,poms.vigor,poms.tmd)
  output <- as.data.frame(output)
  return(output)
}

#****************************************************************************
# POMS (Profile of Mood States - Score by Name)
pomsByname.scoring <- function(data,keep.vars,poms){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }

  if (missing(poms)){
    poms.items <- c('Friendly','Tense','Angry','Worn Out','Unhappy',
                    'Clear-headed','Lively','Confused','Sorry for things done','Shaky',
                    'Listless','Peeved','Considerate','Sad','Active',
                    'On edge','Grouchy','Blue','Energetic','Panicky',
                    'Hopeless','Relaxed','Unworthy','Spiteful','Sympathetic',
                    'Uneasy','Restless','Unable to concentrate','Fatigued','Helpful',
                    'Annoyed','Discouraged','Resentful','Nervous','Lonely',
                    'Miserable','Muddled','Cheerful','Bitter','Exhausted',
                    'Anxious','Ready to fight','Good natured','Gloomy','Desperate',
                    'Sluggish','Rebellious','Helpless','Weary','Bewildered',
                    'Alert','Deceived','Furious','Efficient','Trusting',
                    'Full of pep','Bad-tempered','Worthless','Forgetful','Carefree',
                    'Terrified','Guilty','Vigorous','Uncertain about things','Bushed')
    poms <- c(paste('POMS_',poms.items,sep=""))
  }
  raw.dat <- data[,poms]

  revpoms <- c('POMS_Relaxed','POMS_Efficient')
  data[,revpoms] = 4-data[,revpoms]

  vars.pomsTension    <- c(paste('POMS_',c('Tense', 'Shaky', 'On edge', 'Panicky', 'Relaxed', 'Uneasy', 'Restless', 'Nervous', 'Anxious'),sep=""))
  vars.pomsDepression <- c(paste('POMS_',c('Unhappy', 'Sorry for things done', 'Sad', 'Blue', 'Hopeless','Unworthy', 'Discouraged', 'Lonely', 'Miserable', 'Gloomy', 'Desperate', 'Helpless', 'Worthless', 'Terrified','Guilty'),sep=""))
  vars.pomsAnger      <- c(paste('POMS_',c('Angry', 'Peeved', 'Grouchy', 'Spiteful', 'Annoyed', 'Resentful', 'Bitter', 'Ready to fight', 'Rebellious', 'Deceived', 'Furious','Bad-tempered'),sep=""))
  vars.pomsFatigue    <- c(paste('POMS_',c('Worn Out','Listless','Fatigued','Exhausted','Sluggish', 'Weary','Bushed'),sep=""))
  vars.pomsConfusion  <- c(paste('POMS_',c('Confused', 'Unable to concentrate', 'Muddled', 'Bewildered', 'Efficient','Forgetful', 'Uncertain about things'),sep=""))
  vars.pomsVigor      <- c(paste('POMS_',c('Lively', 'Active', 'Energetic', 'Cheerful', 'Alert', 'Full of pep','Carefree','Vigorous'),sep=""))

  #total mood disturbance (TMD)
  vars.pomsTMDneg     <- c(vars.pomsTension,vars.pomsDepression,vars.pomsAnger,vars.pomsFatigue,vars.pomsConfusion)
  vars.pomsTMDpos     <- vars.pomsVigor

  poms.tension <- rowSums(data[,vars.pomsTension])
  poms.depression <- rowSums(data[,vars.pomsDepression])
  poms.anger <- rowSums(data[,vars.pomsAnger])
  poms.fatigue <- rowSums(data[,vars.pomsFatigue])
  poms.confusion <- rowSums(data[,vars.pomsConfusion])
  poms.vigor <- rowSums(data[,vars.pomsVigor])
  poms.tmd <- rowSums(data[,vars.pomsTMDneg])-rowSums(data[,vars.pomsTMDpos])

  keeps <- data[,keep.vars]

  output <- cbind(keeps,raw.dat,poms.tension,poms.depression,poms.anger,poms.fatigue,poms.confusion,poms.vigor,poms.tmd)
  output <- as.data.frame(output)
  return(output)

}

#*#**********************************************************************
# Primals Inventory, 6-items (PI-6)
#*#**********************************************************************
primals6.scoring <- function(data, keep.vars, primalsShort){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
  if (missing(primalsShort)) {
    primalsShort <- c(paste('PrimalsShort_', c(1:6), sep= ""))
  }
  
  raw.dat <- data[, primalsShort]
  
  primalsShort.rev <- c(paste('PrimalsShort_', c(3,4,6), sep = ""))
  data[, primalsShort.rev] <- 5-data[, primalsShort.rev]
  
  # calculate score
  vars.total <- c(paste('PrimalsShort_', 1:6, sep = ""))
  primalsShort.total <- rowSums(data[,vars.total])/length(vars.total)
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps, raw.dat, primalsShort.total)
  output <- as.data.frame(output) 
  return(output)
}

#*#**********************************************************************
# Primals Inventory, 18-items (PI-18)
#*#**********************************************************************
primals18.scoring <- function(data, keep.vars, primals){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
  if(missing(primals)) {
    primals <- c(paste('Primals_', c(1:18), sep = ""))
  }
  
  raw.dat <- data[, primals]
  
  primals.rev <- c(paste('Primals_', c(5,6,9,10,11,12,13), sep = ""))
  data[, primals.rev] <- 5-data[, primals.rev]
  
  vars.good <- c(paste('Primals_', c(1:3, 5:11, 13:15, 17:18), sep = ""))
  vars.safe <- c(paste('Primals_', c(3, 10, 11, 13, 15, 17), sep = ""))
  vars.enticing <- c(paste('Primals_', c(1, 5:9, 18), sep = ""))
  vars.alive <- c(paste('Primals_', c(2, 4, 12, 14, 16), sep = ""))
  
  primals.good <- rowSums(data[, vars.good])/length(vars.good)
  primals.safe <- rowSums(data[, vars.safe])/length(vars.safe)
  primals.enticing <- rowSums(data[, vars.enticing])/length(vars.enticing)
  primals.alive <- rowSums(data[, vars.alive])/length(vars.alive)
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps, raw.dat, primals.good, primals.safe, 
                  primals.enticing, primals.alive)
  return(output)
} 

#************************************************************************
# Patient-Reported Outcomes Measurement Information System (PROMIS-29)
#************************************************************************
promis29.scoring <- function(data, keep.vars, promis29) {
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <- c(keep.vars, 'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub', keep.vars)
  }
  if (missing(promis29)) {
    promis29 <- c(paste('PROMIS29_', 1:29, sep = ""))
  }
  
  raw.dat <- data[,promis29]
  
  vars.physicalFunction <- c(paste("PROMIS29_", 1:4, sep = ""))
  vars.anxiety <- c(paste("PROMIS29_", 5:8, sep = ""))
  vars.depression <- c(paste("PROMIS29_", 9:12, sep = ""))
  vars.fatigue <- c(paste("PROMIS29_", 13:16, sep = ""))
  vars.sleepDisturbance <- c(paste("PROMIS29_", 17:20, sep = ""))
  vars.social <- c(paste("PROMIS29_", 21:24, sep = ""))
  vars.painInterference <- c(paste("PROMIS29_", 25:28, sep = ""))
  vars.painIntensity <- c("PROMIS29_29")
  
  promis29.physicalFunction <- rowSums(data[,vars.physicalFunction])
  promis29.anxiety <- rowSums(data[,vars.anxiety])
  promis29.depression <- rowSums(data[,vars.depression])
  promis29.fatigue <- rowSums(data[,vars.fatigue])
  promis29.sleepDisturbance <- rowSums(data[,vars.sleepDisturbance])
  promis29.socialRolesAndActivities <- rowSums(data[,vars.social])
  promis29.painInterference <- rowSums(data[,vars.painInterference])
  promis29.painIntensity <- data[,vars.painIntensity]
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps,raw.dat,promis29.physicalFunction,promis29.anxiety,promis29.depression,
                  promis29.fatigue,promis29.sleepDisturbance,promis29.socialRolesAndActivities,
                  promis29.painInterference,promis29.painIntensity)
  output <- as.data.frame(output)
  return(output)
}

#************************************************************************
# Pain Resilience Scale (PRS)
#************************************************************************
prs.scoring <- function(data, keep.vars, prs) {
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <- c(keep.vars, 'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub', keep.vars)
  }
  if (missing(prs)) {
    prs <- c(paste('PRS_', 1:14, sep = ""))
  }
  
  raw.dat <- data[,prs]
  
  vars.behavioralPerseverance <- c(paste("PRS_", 1:5, sep = ""))
  vars.cognitiveAffectivePositivity <- c(paste("PRS_", 6:14, sep = ""))
  
  prs.behavioralPerseverance <- rowSums(data[,vars.behavioralPerseverance])
  prs.cognitiveAffectivePositivity <- rowSums(data[,vars.cognitiveAffectivePositivity])
  prs.total <- rowSums(data[,prs])
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps,raw.dat,prs.behavioralPerseverance,prs.cognitiveAffectivePositivity,prs.total)
  output <- as.data.frame(output)
  return(output)
}

#******************************************************************
# Patient Health Questionnaire-9 (PHQ-9) - Depression Scale
# 9 items, Scored 0-3, Total Sum Range 0-27
#******************************************************************
phq9.scoring <- function(data, keep.vars, phq) {
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <- c(keep.vars, 'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub', keep.vars)
  }
  
  if (missing(phq)) {
    phq <- c(paste('PHQ_', 1:9, sep=""))
  }
  
  raw.dat <- data[,phq]
  
  # calculate score
  phq.total <- rowSums(data[,phq])
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps, raw.dat, phq.total)
  output <- as.data.frame(output)
  return(output)
}

#*#**********************************************************************
# Pittsburgh Sleep Quality Index (PSQI)
# NOTE: Requires Questions 1 and 3 to have the format of "HH:MM AM/PM" e.g, 10:00 PM
#************************************************************************
psqi.scoring <- function(data, keep.vars, psqi){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <- c(keep.vars, 'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub', keep.vars)
  }
  
  if (missing(psqi)){
    psqi <- c(c(paste('PSQI_',1:3,sep="")),'PSQI_4a','PSQI_4b',
              c(paste('PSQI_5',letters[1:10],sep="")),'PSQI_5j_TEXT',
              c(paste('PSQI_',6:9,sep="")))
  }
  
  raw.dat <- data[,psqi]
  
  #Component 1: Subjective Sleep Quality - item 9 responses already adjusted to a 0-3 scale, so Component 1 score should just be equal to item 9 response. 
  psqi.sleepQuality <- data[,c("PSQI_9")]

  
  #Component 2: Sleep Latency - Score is the sum of item 2 and item 5a responses. Must first convert item 2 scores to a 0-3 scale. 
  data[,c("PSQI_2")] <- ifelse(data[,c("PSQI_2")] <= 15, 0, 
                               ifelse(data[,c("PSQI_2")] <= 30, 1,
                                      ifelse(data[,c("PSQI_2")] <= 60, 2, 3)))
  
  vars.component2 <- c(paste("PSQI_", c(2, "5a"), sep = ""))
  
  psqi.sleepLatency <- ifelse((rowSums(data[,vars.component2])) == 0, 0,
                              ifelse((rowSums(data[,vars.component2])) <= 2, 1,
                                     ifelse((rowSums(data[,vars.component2])) <= 4, 2, 3)))
  
  #Component 3: Sleep Duration - 4a responses adjusted to a 0-3 scale. 
  psqi.sleepDuration <- ifelse(data[,c("PSQI_4a")] > 7, 0,
                               ifelse(data[,c("PSQI_4a")] >= 6, 1,
                                      ifelse(data[,c("PSQI_4a")] >= 5, 2, 3)))
  
  
  #Component 4: Sleep Efficiency = (# hours slept [4a] / # hours in bed [item 3 - item 1]) x 100%
  
  # format <- "%I:%M %p" #time format to 24 hours
  # 
  # time_cond <- as.POSIXct(data$PSQI_1, format = format) < as.POSIXct(data$PSQI_3, format = format) # checking if day 1 time is less than day 3
  # 
  # sleep_date <- rep(as.character(Sys.Date()), nrow(data))
  # wake_date <- ifelse(time_cond == TRUE, as.character(Sys.Date()), as.character(Sys.Date() + 1))
  # 
  # format <- paste("%Y-%m-%d",format)
  # PSQI_1_new <- as.POSIXct(paste(sleep_date, data$PSQI_1), format=format)
  # 
  # PSQI_3_new <- as.POSIXct(paste(wake_date, data$PSQI_3), format=format)
  # 
  # hrs_in_bed <- as.numeric(difftime(PSQI_3_new, PSQI_1_new))
  hrs_in_bed <- data$PSQI_4b

  
  psqi.sleepEfficiency <- data$PSQI_4a/hrs_in_bed
  psqi.sleepEfficiency <- ifelse(psqi.sleepEfficiency < .65, 3,
                                 ifelse(psqi.sleepEfficiency <= .74, 2,
                                        ifelse(psqi.sleepEfficiency < .845, 1, 0)))
  
  #Component 5: Sleep Disturbance - Sum of items 5b through 5j
  vars.component5 <- paste('PSQI_5', letters[2:10], sep = "")
  psqi.sleepDisturbance <- ifelse((rowSums(data[,vars.component5])) == 0, 0,
                                  ifelse((rowSums(data[,vars.component5])) <= 9, 1,
                                         ifelse((rowSums(data[,vars.component5])) <= 18, 2, 3)))
  
  #Component 6: Sleep Medication - item 6 responses already on 0-3 scale, so Component 6 score should be equal to item 6 response
  psqi.sleepMedication <- data[,c("PSQI_6")]

  
  #Component 7: Daytime Dysfunction - sum of items 7 and 8
  vars.component7 <- c(paste("PSQI_", 7:8, sep = ""))
  psqi.daytimeDysfunction <- ifelse((rowSums(data[,vars.component7])) == 0, 0,
                                    ifelse((rowSums(data[,vars.component7])) <= 2, 1,
                                           ifelse((rowSums(data[,vars.component7])) <= 4, 2, 3)))
  
  # Global Score
  psqi.subscales <- cbind(psqi.sleepQuality, psqi.sleepLatency, psqi.sleepDuration, psqi.sleepEfficiency, 
                     psqi.sleepDisturbance, psqi.sleepMedication, psqi.daytimeDysfunction)
  names(psqi.subscales) <- c('psqi.sleepQuality', 'psqi.sleepLatency', 'psqi.sleepDuration', 'psqi.sleepEfficiency', 
                             'psqi.sleepDisturbance', 'psqi.sleepMedication', 'psqi.daytimeDysfunction')
  
  psqi.global <- rowSums(psqi.subscales)
  
  
  # Combine data
  keeps <- data[,keep.vars]
  output <- cbind(keeps, raw.dat, psqi.subscales, psqi.global) 
  output <- as.data.frame(output)
  return(output)
}

#******************************************************************
# The Prodromal Questionnaire (PQ-16)
# 16 items, Scored 0-3, Total Sum Range 0-48
#******************************************************************
pq16.scoring <- function(data, keep.vars, pq16) {
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <- c(keep.vars, 'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub', keep.vars)
  }
  
  if (missing(pq16)) {
    pq16 <- c(paste('PQ_', 1:16, sep=""))
  }
  
  raw.dat <- data[,pq16]
  
  # calculate score
  pq16.total <- rowSums(data[,pq16])
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps, raw.dat, pq16.total)
  output <- as.data.frame(output)
  return(output)
}

#************************************************************************
# PTSD Checklist for DSM-5 (PCL-5)
#************************************************************************
pcl.scoring <- function(data, keep.vars, pcl) {
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <- c(keep.vars, 'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub', keep.vars)
  }
  
  if (missing(pcl)) {
    pcl <- c(paste('PCL_', 1:20, sep=""))
  }
  
  raw.dat <- data[,pcl]
  
  vars.clusterB <- c(paste("PCL_", c(1:5), sep = ""))
  vars.clusterC <- c(paste("PCL_", c(6:10), sep = ""))
  vars.clusterD <- c(paste("PCL_", c(11:15), sep = ""))
  vars.clusterE <- c(paste("PCL_", c(16:20), sep = ""))
  
  pcl.clusterB <- rowSums(data[,vars.clusterB])
  pcl.clusterC <- rowSums(data[,vars.clusterC])
  pcl.clusterD <- rowSums(data[,vars.clusterD])
  pcl.clusterE <- rowSums(data[,vars.clusterE])
  pcl.total <- rowSums(data[,pcl])
  
  keeps <- data[,keep.vars]
  
  if('PCL_Describe' %in% colnames(data)){
    pcl.description <- data[,'PCL_Describe']
    output <- cbind(keeps, pcl.description, raw.dat, pcl.clusterB, pcl.clusterC, 
                    pcl.clusterD, pcl.clusterE, pcl.total)
  } else {
    output <- cbind(keeps, raw.dat, pcl.clusterB, pcl.clusterC, 
                    pcl.clusterD, pcl.clusterE, pcl.total)
  }
  
  output <- as.data.frame(output)
  return(output)
}

#************************************************************************
# PTSD Checklist for DSM-5 (PCL-5)
#************************************************************************
pcl5.scoring <- function(data, keep.vars, pcl5) {
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <- c(keep.vars, 'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub', keep.vars)
  }
  
  if (missing(pcl5)) {
    pcl5 <- c(paste('PCL5_', 1:20, sep=""))
  }
  
  raw.dat <- data[,pcl5]
  
  vars.clusterB <- c(paste("PCL5_", c(1:5), sep = ""))
  vars.clusterC <- c(paste("PCL5_", c(6:10), sep = ""))
  vars.clusterD <- c(paste("PCL5_", c(11:15), sep = ""))
  vars.clusterE <- c(paste("PCL5_", c(16:20), sep = ""))
  
  pcl.clusterB <- rowSums(data[,vars.clusterB])
  pcl.clusterC <- rowSums(data[,vars.clusterC])
  pcl.clusterD <- rowSums(data[,vars.clusterD])
  pcl.clusterE <- rowSums(data[,vars.clusterE])
  pcl.total <- rowSums(data[,pcl5])
  
  keeps <- data[,keep.vars]
  
  if('PCL5_Describe' %in% colnames(data)){
    pcl.description <- data[,'PCL5_Describe']
    output <- cbind(keeps, pcl.description, raw.dat, pcl.clusterB, pcl.clusterC, 
                    pcl.clusterD, pcl.clusterE, pcl.total)
  } else {
    output <- cbind(keeps, raw.dat, pcl.clusterB, pcl.clusterC, 
                    pcl.clusterD, pcl.clusterE, pcl.total)
  }
  
  output <- as.data.frame(output)
  return(output)
}
#************************************************************************
# Purpose in Life (PIL) Test
#************************************************************************
pil.scoring <- function(data, keep.vars, pil) {
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <- c(keep.vars, 'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub', keep.vars)
  }
  if (missing(pil)) {
    pil <- c(paste('PIL_',c(1:20), sep = ""))
  }
  raw.dat <- data[,pil]
  
  pil.total <- rowSums(data[,pil])
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps,raw.dat,pil.total)
  output <- as.data.frame(output)
  return(output)
}

#******************************************************************
# The Prodromal Questionnaire (PQ-16)
# 16 items, Scored 0-3, Total Sum Range 0-48
#******************************************************************
pq16.scoring <- function(data, keep.vars, pq16) {
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <- c(keep.vars, 'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub', keep.vars)
  }
  
  if (missing(pq16)) {
    pq16 <- c(paste('PQ_', 1:16, sep=""))
  }
  
  raw.dat <- data[,pq16]
  
  # calculate score
  pq16.total <- rowSums(data[,pq16])
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps, raw.dat, pq16.total)
  output <- as.data.frame(output)
  return(output)
}

################################################################################
# Psychological Inflexibility in Pain Scale (PIPS)
################################################################################
pips.scoring <- function(data, keep.vars, pips){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
  if (missing(pips)){
    pips <- paste('PIPS_', 1:16, sep="")
  }
  raw.dat <- data[, pips]
  
  vars.avoidance <- paste('PIPS_', c(2, 3, 7, 8, 9, 11, 13, 14, 15, 16), sep="")
  vars.cognitiveFusion <- paste('PIPS_', c(1, 4, 5, 6, 10, 12), sep="")
  
  pips.avoidance <- rowSums(data[,vars.avoidance])
  pips.cognitiveFusion <- rowSums(data[,vars.cognitiveFusion])
  pips.total <- rowSums(data[,pips])
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps, raw.dat, pips.avoidance, pips.cognitiveFusion, pips.total)
  output <- as.data.frame(output)
  return(output)
}

################################################################################
# Personality Inventory for DSM-5 (PID5) # TODO verify scoring
################################################################################
pid5.scoring <- function(data, keep.vars, pid5){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
  if (missing(pid5)){
    pid5 <- paste('PID5_', c(5, 21, 24, 25, 33, 36, 37, 42, 44, 52, 55, 59, 70, 71, 77, 
                             83, 94, 99, 106, 139, 143, 150, 152, 154, 172, 185, 192, 193, 
                             194, 205, 209, 213, 217), sep="")
  }
  raw.dat <- data[, pid5]
  
  vars.unusualBeliefsExperiences <- paste('PID5_', c(94, 99, 106, 139, 143, 150, 194, 209), sep="")
  vars.eccentricity <- paste('PID5_', c(5,21,24,25,33,52,55,70,71,152,172,185,205), sep="")
  vars.perceptualDysregulation <- paste('PID5_', c(36,37,42,44,59,77,83,154,192,193,213,217), sep="")
  
  pid5.unusualBeliefsExperiences <- rowSums(data[,vars.unusualBeliefsExperiences])/length(vars.unusualBeliefsExperiences)
  pid5.eccentricity <- rowSums(data[,vars.eccentricity])/length(vars.eccentricity)
  pid5.perceptualDysregulation <- rowSums(data[,vars.perceptualDysregulation])/length(vars.perceptualDysregulation)
  pid5.psychoticism <- rowSums(data[,pid5])/length(pid5)
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps, raw.dat, pid5.unusualBeliefsExperiences, pid5.eccentricity,pid5.perceptualDysregulation, pid5.psychoticism)
  output <- as.data.frame(output)
  return(output)
}
#****************************************************************************
# QIDS (Quick Inventory of Depressive Symptoms)
qids.scoring <- function(data,keep.vars,qids) {
  keep.vars <- c('ID','date')
  if("time.point" %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
  if (missing(qids)){
    if('QIDS_5a' %in% colnames(data) & 'QIDS_7a' %in% colnames(data)) {
      qids <- c(c(paste('QIDS_',1:5,sep="")),'QIDS_5a',
                'QIDS_6','QIDS_7','QIDS_7a',c(paste('QIDS_',8:16,sep="")))
    } else {
      qids <- c(paste('QIDS_',1:16,sep=""))
      }
    
  }
  raw.dat <- data[,qids]
  
  # calculate scores
  # scoring guide https://sierraketamineclinics.com/downloads/qids-sr16.pdf
  vars.sleep <- c(paste('QIDS_',1:4,sep=""))
  #vars.qidsSad <- 'QIDS_5'
  vars.appetite <- c(paste('QIDS_',c(6,7,8,9),sep=""))
  #vars.qidsConcentration <- 'QIDS_10'
  #vars.qidsSelfCriticism <- 'QIDS_11'
  #vars.qidsSuicidalIdeation <- 'QIDS_12'
  #vars.qidsInterest <- 'QIDS_13'
  #vars.qidsEnergy <- 'QIDS_14'
  vars.psychomotor <- c(paste('QIDS_',c(15,16),sep=""))
  
  # find highest scores among sleep, appetite/weight, and psychomotor items
  # if NA values may exist, use the max.nonan function above for the respective subscale
  data[,'maxsleep']     <- base::apply(data[,vars.sleep],1,max)
  data[,'maxappetite']  <- base::apply(data[,vars.appetite],1,max.nonan)
  data[,'maxpsychomotor'] <- base::apply(data[,vars.psychomotor],1,max)
  
  # create dataframe for each scoring group
  qids.sleep <- data.frame(sleep=data[,'maxsleep'])
  qids.sadmood <- data.frame(I5=data[,'QIDS_5']) #sad mood item
  qids.appetite <- data.frame(appetite=data[,'maxappetite'])
  qids.other <- data.frame(data[,c(paste('QIDS_',c(10:14),sep=""))]) #concentration,self-criticism,suicidal ideation,interest,energy
  qids.psychomotor <- data.frame(data[,'maxpsychomotor'])
  
  qids.total <- (rowSums(qids.sleep)+rowSums(qids.sadmood)+rowSums(qids.appetite)
                 +rowSums(qids.other)+rowSums(qids.psychomotor))
  
  #scores <- qids.total
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps,raw.dat,qids.total)
  output <- as.data.frame(output)
  #names(output) <- c(keep.vars,'QIDStotal')
  
  # return
  return(output)
}

#****************************************************************************
# Quality of Life Inventory (QOLI)
#****************************************************************************
qoli.scoring <- function(data, keep.vars, qoli) {
  keep.vars <- c('ID', 'date')
  if ('time.point' %in% colnames(data)) {
    keep.vars <- c(keep.vars, 'time.point')
  }
  if ('sub' %in% colnames(data)) {
    keep.vars <- c('sub', keep.vars)
  }
  if (missing(qoli)) {
    qoli <- paste0("QOLI_", rep(1:16, each = 2), c("a", "b"))
  }
  
  raw.dat <- data[,qoli]
  
  vars.health <- c('QOLI_1a', 'QOLI_1b')
  qoli.health <- data[[vars.health[1]]] * data[[vars.health[2]]]
  
  vars.selfEsteem <- c('QOLI_2a', 'QOLI_2b')
  qoli.selfEsteem <- data[[vars.selfEsteem[1]]] * data[[vars.selfEsteem[2]]]
  
  vars.goalsValues <- c('QOLI_3a', 'QOLI_3b')
  qoli.goalsValues <- data[[vars.goalsValues[1]]] * data[[vars.goalsValues[2]]]
  
  vars.money <- c('QOLI_4a', 'QOLI_4b')
  qoli.money <- data[[vars.money[1]]] * data[[vars.money[2]]]
  
  vars.work <- c('QOLI_5a', 'QOLI_5b')
  qoli.work <- data[[vars.work[1]]] * data[[vars.work[2]]]
  
  vars.play <- c('QOLI_6a', 'QOLI_6b')
  qoli.play <- data[[vars.play[1]]] * data[[vars.play[2]]]
  
  vars.learning <- c('QOLI_7a', 'QOLI_7b')
  qoli.learning <- data[[vars.learning[1]]] * data[[vars.learning[2]]]
  
  vars.creativity <- c('QOLI_8a', 'QOLI_8b')
  qoli.creativity <- data[[vars.creativity[1]]] * data[[vars.creativity[2]]]
  
  vars.helping <- c('QOLI_9a', 'QOLI_9b')
  qoli.helping <- data[[vars.helping[1]]] * data[[vars.helping[2]]]
  
  vars.love <- c('QOLI_10a', 'QOLI_10b')
  qoli.love <- data[[vars.love[1]]] * data[[vars.love[2]]]
  
  vars.friends <- c('QOLI_11a', 'QOLI_11b')
  qoli.friends <- data[[vars.friends[1]]] * data[[vars.friends[2]]]
  
  vars.children <- c('QOLI_12a', 'QOLI_12b')
  qoli.children <- data[[vars.children[1]]] * data[[vars.children[2]]]
  
  vars.relatives <- c('QOLI_13a', 'QOLI_13b')
  qoli.relatives <- data[[vars.relatives[1]]] * data[[vars.relatives[2]]]
  
  vars.home <- c('QOLI_14a', 'QOLI_14b')
  qoli.home <- data[[vars.home[1]]] * data[[vars.home[2]]]
  
  vars.neighborhood <- c('QOLI_15a', 'QOLI_15b')
  qoli.neighborhood <- data[[vars.neighborhood[1]]] * data[[vars.neighborhood[2]]]
  
  vars.community <- c('QOLI_16a', 'QOLI_16b')
  qoli.community <- data[[vars.community[1]]] * data[[vars.community[2]]]
  
  # Self subscale
  qoli.selfNonzero <- (qoli.health != 0) + (qoli.selfEsteem != 0) + (qoli.goalsValues != 0 ) + (qoli.money != 0 )
  qoli.selfWeighted <- qoli.health + qoli.selfEsteem + qoli.goalsValues + qoli.money
  qoli.self <- qoli.selfWeighted / qoli.selfNonzero
  
  # Personal Fulfillment subscale
  qoli.persFulfillNonzero <- (qoli.work != 0 ) + (qoli.play != 0 ) + (qoli.learning != 0 ) + (qoli.creativity != 0 ) +
    (qoli.helping != 0 )
  qoli.persFulfillWeighted <- qoli.work + qoli.play + qoli.learning + qoli.creativity + qoli.helping
  qoli.persFulfill <- qoli.persFulfillWeighted / qoli.persFulfillNonzero
  
  
  # Relationships subscale   
  qoli.relationshipsNonzero <- (qoli.love != 0 ) + (qoli.friends != 0 ) + (qoli.children != 0 ) + (qoli.relatives != 0 )
  qoli.relationshipsWeighted <- qoli.love + qoli.friends + qoli.children  + qoli.relatives
  qoli.relationships <- qoli.relationshipsWeighted / qoli.relationshipsNonzero
  
  # Surroundings subscale
  qoli.surroundingsNonzero <- (qoli.home != 0 ) + (qoli.neighborhood != 0 ) + (qoli.community != 0 ) 
  qoli.surroundingsWeighted <- qoli.home + qoli.neighborhood + qoli.community
  qoli.surroundings <- qoli.surroundingsWeighted / qoli.surroundingsNonzero
  
  # Overall score 
  qoli.lifeAreas <- (qoli.health != 0) + (qoli.selfEsteem != 0) + (qoli.goalsValues != 0 ) + (qoli.money != 0 ) + 
    (qoli.work != 0 ) + (qoli.play != 0 ) + (qoli.learning != 0 ) + (qoli.creativity != 0 ) +
    (qoli.helping != 0 ) + (qoli.love != 0 ) + (qoli.friends != 0 ) + (qoli.children != 0 ) +
    (qoli.relatives != 0 ) + (qoli.home != 0 ) + (qoli.neighborhood != 0 ) + (qoli.community != 0 ) 
  
  qoli.weightedSatisfaction <- qoli.health + qoli.selfEsteem + qoli.goalsValues + qoli.money + 
    qoli.work + qoli.play + qoli.learning + qoli.creativity +
    qoli.helping + qoli.love + qoli.friends + qoli.children  +
    qoli.relatives + qoli.home + qoli.neighborhood + qoli.community
  
  qoli.total <- qoli.weightedSatisfaction / qoli.lifeAreas
  
  keeps <- data[, keep.vars]
  
  output <- cbind(keeps, raw.dat,  
                  qoli.self, qoli.persFulfill, qoli.relationships, 
                  qoli.surroundings, qoli.total)
  
  output <- as.data.frame(output)
  
  return(output)
}


################################################################################
# Quality of Life Enjoyment and Satisfaction Questionnaire (QLES-Q-SF)
# https://www.thehealthandwellnesssource.com/userfiles/1680076/file/THWS-Q-LES-Q-SF.pdf 
################################################################################
sfqlesq.scoring <- function(data, keep.vars, sfqlesq) {
  keep.vars <- c('ID','date')
  if("time.point" %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
  if (missing(sfqlesq)) {
    sfqlesq <- c(paste('QLESQ_',1:16,sep=""))
  }
  
  raw.dat <- data[,sfqlesq]
  
  qlesq.scored.items <- c(paste('QLESQ_',1:14,sep=""))
  qlesq.total <- rowSums(data[,qlesq.scored.items])
  qlesq.pctmax <- (qlesq.total - 14)/56
  
  keeps <- data[,keep.vars]
  output <- cbind(keeps, raw.dat, qlesq.total, qlesq.pctmax) %>%
    as.data.frame()
  output
}


#*********************************************************************
# Quality of Life in Alzheimer's Disease
#*********************************************************************
qolalz.scoring <- function(data, keep.vars, qolalz){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
  if (missing(qolalz)){
    qolalz <- paste('QOLAlz_',1:13,sep="")
  }
  raw.dat <- data[,qolalz]
  
  qolalz.total <- rowSums(data[,qolalz])
  
  keeps <- data[,keep.vars]
  output <- cbind(keeps, raw.dat, qolalz.total)
  output <- as.data.frame(output)
  return(output)
  
}



#*********************************************************************
# Quality of Life After Brain Injury
#*********************************************************************
qol.braininjury.scoring <- function(data, keep.vars, qolBrainInjury){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
  if (missing(qolBrainInjury)){
    qolBrainInjury <- paste('QOLBrainInjury_',1:6,sep="")
  }
  raw.dat <- data[,qolBrainInjury]
  
  qolBrainInjury.total <- rowSums(data[,qolBrainInjury])
  qolBrainInjury.pctmax <- qolBrainInjury.total/(length(qolBrainInjury)*5)
  
  keeps <- data[,keep.vars]
  output <- cbind(keeps, raw.dat, qolBrainInjury.total, qolBrainInjury.pctmax)
  output <- as.data.frame(output)
  return(output)
  
}
#************************************************************************
# Questionnaire on Smoking Urges (QSU)
#************************************************************************
qsu.scoring <- function(data, keep.vars, qsu) {
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <- c(keep.vars, 'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub', keep.vars)
  }
  if (missing(qsu)) {
    qsu <- c(paste('QSU_', 1:32, sep = ""))
  }
  
  raw.dat <- data[,qsu]
  
  rev.items <- c(paste("QSU_", c(4,6,8,10,11,16,17,21,22,26,27,28,32), sep = ""))
  data[,rev.items] <- 8-data[,rev.items]
  
  vars.pleasure <- c(paste("QSU_", c(4,5,6,9,11,16,17,20,21,22,23,25,27,28,32), sep = ""))
  vars.relief <- c(paste("QSU_", c(2,3,7,12,13,14,18,19,24,29,30), sep = ""))
  
  qsu.pleasure <- rowSums(data[,vars.pleasure])/length(vars.pleasure)
  qsu.relief <- rowSums(data[,vars.relief])/length(vars.relief)
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps,raw.dat,qsu.pleasure,qsu.relief)
  output <- as.data.frame(output)
  return(output)
}

#****************************************************************************
# Quality of Life Inventory (QOLI)
#****************************************************************************
qoli.scoring <- function(data, keep.vars, qoli) {
        keep.vars <- c('ID', 'date')
        if ('time.point' %in% colnames(data)) {
                keep.vars <- c(keep.vars, 'time.point')
        }
        if ('sub' %in% colnames(data)) {
                keep.vars <- c('sub', keep.vars)
        }
        if (missing(qoli)) {
                qoli <- paste0("QOLI_", rep(1:16, each = 2), c("a", "b"))
        }
        
        raw.dat <- data[,qoli]
        
        vars.health <- c('QOLI_1a', 'QOLI_1b')
        qoli.health <- data[[vars.health[1]]] * data[[vars.health[2]]]
        
        vars.selfEsteem <- c('QOLI_2a', 'QOLI_2b')
        qoli.selfEsteem <- data[[vars.selfEsteem[1]]] * data[[vars.selfEsteem[2]]]
        
        vars.goalsValues <- c('QOLI_3a', 'QOLI_3b')
        qoli.goalsValues <- data[[vars.goalsValues[1]]] * data[[vars.goalsValues[2]]]
        
        vars.money <- c('QOLI_4a', 'QOLI_4b')
        qoli.money <- data[[vars.money[1]]] * data[[vars.money[2]]]
        
        vars.work <- c('QOLI_5a', 'QOLI_5b')
        qoli.work <- data[[vars.work[1]]] * data[[vars.work[2]]]
        
        vars.play <- c('QOLI_6a', 'QOLI_6b')
        qoli.play <- data[[vars.play[1]]] * data[[vars.play[2]]]
        
        vars.learning <- c('QOLI_7a', 'QOLI_7b')
        qoli.learning <- data[[vars.learning[1]]] * data[[vars.learning[2]]]
        
        vars.creativity <- c('QOLI_8a', 'QOLI_8b')
        qoli.creativity <- data[[vars.creativity[1]]] * data[[vars.creativity[2]]]
        
        vars.helping <- c('QOLI_9a', 'QOLI_9b')
        qoli.helping <- data[[vars.helping[1]]] * data[[vars.helping[2]]]
        
        vars.love <- c('QOLI_10a', 'QOLI_10b')
        qoli.love <- data[[vars.love[1]]] * data[[vars.love[2]]]
        
        vars.friends <- c('QOLI_11a', 'QOLI_11b')
        qoli.friends <- data[[vars.friends[1]]] * data[[vars.friends[2]]]
        
        vars.children <- c('QOLI_12a', 'QOLI_12b')
        qoli.children <- data[[vars.children[1]]] * data[[vars.children[2]]]
        
        vars.relatives <- c('QOLI_13a', 'QOLI_13b')
        qoli.relatives <- data[[vars.relatives[1]]] * data[[vars.relatives[2]]]
        
        vars.home <- c('QOLI_14a', 'QOLI_14b')
        qoli.home <- data[[vars.home[1]]] * data[[vars.home[2]]]
        
        vars.neighborhood <- c('QOLI_15a', 'QOLI_15b')
        qoli.neighborhood <- data[[vars.neighborhood[1]]] * data[[vars.neighborhood[2]]]
        
        vars.community <- c('QOLI_16a', 'QOLI_16b')
        qoli.community <- data[[vars.community[1]]] * data[[vars.community[2]]]
        
        # Self subscale
        qoli.selfNonzero <- (qoli.health != 0) + (qoli.selfEsteem != 0) + (qoli.goalsValues != 0 ) + (qoli.money != 0 )
        qoli.selfWeighted <- qoli.health + qoli.selfEsteem + qoli.goalsValues + qoli.money
        qoli.self <- qoli.selfWeighted / qoli.selfNonzero
        
        # Personal Fulfillment subscale
        qoli.fulfillmentNonzero <- (qoli.work != 0 ) + (qoli.play != 0 ) + (qoli.learning != 0 ) + (qoli.creativity != 0 ) +
                (qoli.helping != 0 )
        qoli.fulfillmentWeighted <- qoli.work + qoli.play + qoli.learning + qoli.creativity + qoli.helping
        qoli.fulfillment <- qoli.fulfillmentWeighted / qoli.fulfillmentNonzero
        
        
        # Relationships subscale   
        qoli.relationshipsNonzero <- (qoli.love != 0 ) + (qoli.friends != 0 ) + (qoli.children != 0 ) + (qoli.relatives != 0 )
        qoli.relationshipsWeighted <- qoli.love + qoli.friends + qoli.children  + qoli.relatives
        qoli.relationships <- qoli.relationshipsWeighted / qoli.relationshipsNonzero
        
        # Surroundings subscale
        qoli.surroundingsNonzero <- (qoli.home != 0 ) + (qoli.neighborhood != 0 ) + (qoli.community != 0 ) 
        qoli.surroundingsWeighted <- qoli.home + qoli.neighborhood + qoli.community
        qoli.surroundings <- qoli.surroundingsWeighted / qoli.surroundingsNonzero
        
        # Overall score 
        qoli.lifeAreas <- (qoli.health != 0) + (qoli.selfEsteem != 0) + (qoli.goalsValues != 0 ) + (qoli.money != 0 ) + 
                (qoli.work != 0 ) + (qoli.play != 0 ) + (qoli.learning != 0 ) + (qoli.creativity != 0 ) +
                (qoli.helping != 0 ) + (qoli.love != 0 ) + (qoli.friends != 0 ) + (qoli.children != 0 ) +
                (qoli.relatives != 0 ) + (qoli.home != 0 ) + (qoli.neighborhood != 0 ) + (qoli.community != 0 ) 
        
        qoli.weightedSatisfaction <- qoli.health + qoli.selfEsteem + qoli.goalsValues + qoli.money + 
                qoli.work + qoli.play + qoli.learning + qoli.creativity +
                qoli.helping + qoli.love + qoli.friends + qoli.children  +
                qoli.relatives + qoli.home + qoli.neighborhood + qoli.community
        
        qoli.score <- qoli.weightedSatisfaction / qoli.lifeAreas
        
        keeps <- data[, keep.vars]
        
        output <- cbind(keeps, raw.dat, 
                        qoli.weightedSatisfaction , qoli.lifeAreas, qoli.score, 
                        qoli.selfWeighted, qoli.selfNonzero, qoli.self,
                        qoli.fulfillmentWeighted, qoli.fulfillmentNonzero, qoli.fulfillment,
                        qoli.relationshipsWeighted, qoli.relationshipsNonzero, qoli.relationships,
                        qoli.surroundingsWeighted, qoli.surroundingsNonzero, qoli.surroundings)
        output <- as.data.frame(output)
        
        return(output)
}



###################################################################################################
# Rivermead Post Concussion Symptoms Questionnaire (RPQ)
###################################################################################################
rpq.scoring <- function(data, keep.vars, rpq){
  keep.vars <- c('ID','date')
  if("time.point" %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
  if (missing(rpq)) {
    rpq <- c(paste('RPQ_',1:16,sep=""), 
             paste('RPQ_OtherDifficulties_', 
                   c('Yes.No', 'Rating1', 'Rating1_TEXT', 'Rating2', 'Rating2_TEXT'), sep=""))
  }
  raw.dat <- data[,rpq]
  
  vars.rpq3item <- paste('RPQ_',1:3,sep="")
  vars.rpq13item <- paste('RPQ_', 4:16,sep="")
  
  rpq.3item <- rowSums(data[,vars.rpq3item])
  rpq.13item <- rowSums(data[,vars.rpq13item])
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps, raw.dat, rpq.3item, rpq.13item)
  output <- as.data.frame(output)
  return(output)
}

###################################################################################################
# Ruminative Response Scale
###################################################################################################
rrs.scoring <- function(data, keep.vars, rrs) {
  keep.vars <- c('ID','date')
  if("time.point" %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
  if (missing(rrs)) {
    rrs <- c(paste('RRS_',1:10,sep=""))
  }
  raw.dat <- data[,rrs]
  
  rrs.sum <- rowSums(data[,rrs])
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps, raw.dat,rrs.sum) %>%
    as.data.frame()
  output
}


#****************************************************************************
# Ryff PWB (Ryff Psychological Well-Being Scale)
rwb.scoring <- function(data,keep.vars,rwb){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }

  if (missing(rwb)){
    rwb <- c(paste('RWB_',1:54,sep=""))
  }
  raw.dat <- data[,rwb]

  revrwb <- c(paste('RWB_',c(3,5,7,8,10,12,13,14,16,18,20,22,24,25,26,27,30,
                             31,32,39,40,41,43,44,46,49,50,54),sep=""))
  data[,revrwb] <- 7-data[,revrwb]

  vars.rwbAutonomy <- c(paste('RWB_',c(6,11,16,19,25,35,40,44,52),sep=""))
  vars.rwbEnvironMastery <- c(paste('RWB_',c(2,7,12,17,20,29,36,49,53),sep=""))
  vars.rwbPersGrowth <- c(paste('RWB_',c(3,18,21,26,37,41,45,50,54),sep=""))
  vars.rwbPurposeLife <- c(paste('RWB_',c(8,13,22,27,30,33,38,42,46),sep=""))
  vars.rwbPosRelations <- c(paste('RWB_',c(1,5,10,15,24,32,34,39,47),sep=""))
  vars.rwbSelfAccept <- c(paste('RWB_',c(4,9,14,23,28,31,43,48,51),sep=""))



  rwb.autonomy <- ifelse((rowSums(is.na(data[,vars.rwbAutonomy]))!=length(vars.rwbAutonomy)),
                         rowSums(data[,vars.rwbAutonomy],na.rm = T), NA)
  rwb.environmentalMastery <- ifelse((rowSums(is.na(data[,vars.rwbEnvironMastery]))!=length(vars.rwbEnvironMastery)),
                                      rowSums(data[,vars.rwbEnvironMastery],na.rm = T), NA)
  rwb.personalGrowth <- ifelse((rowSums(is.na(data[,vars.rwbPersGrowth]))!=length(vars.rwbPersGrowth)),
                                rowSums(data[,vars.rwbPersGrowth],na.rm = T), NA)
  rwb.purposeLife <- ifelse((rowSums(is.na(data[,vars.rwbPurposeLife]))!=length(vars.rwbPurposeLife)),
                             rowSums(data[,vars.rwbPurposeLife],na.rm = T), NA)
  rwb.positiveRelations <- ifelse((rowSums(is.na(data[,vars.rwbPosRelations]))!=length(vars.rwbPosRelations)),
                                   rowSums(data[,vars.rwbPosRelations],na.rm = T), NA)
  rwb.selfAcceptance <- ifelse((rowSums(is.na(data[,vars.rwbSelfAccept]))!=length(vars.rwbSelfAccept)),
                                rowSums(data[,vars.rwbSelfAccept],na.rm = T), NA)

  keeps <- data[,keep.vars]
  
  output <- cbind(keeps,raw.dat,rwb.autonomy,rwb.environmentalMastery,rwb.personalGrowth,
                  rwb.purposeLife,rwb.positiveRelations,rwb.selfAcceptance)

  output <- as.data.frame(output)
  return(output)

}
#************************************************************************
# Smoking Abstinence Self-Efficacy (SASE)
#************************************************************************
sase.scoring <- function(data, keep.vars, sase) {
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <- c(keep.vars, 'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub', keep.vars)
  }
  if (missing(sase)) {
    sase <- c(paste('SASE_', 1:40, sep = ""))
  }
  
  raw.dat <- data[,sase]
  
  vars.confidence <- c(paste("SASE_", 1:20, sep = ""))
  vars.temptation <- c(paste("SASE_", 21:40, sep = ""))
  
  sase.confidence <- rowSums(data[,vars.confidence])
  sase.temptation <- rowSums(data[,vars.temptation])
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps,raw.dat,sase.confidence, sase.temptation)
  output <- as.data.frame(output)
  return(output)
}
#****************************************************************************
# SDS (Sheehan Disability Scale)
# items scored individually on scale of 0-10, and combined for global functional impairment
# scores of 5 or greater indicate significant functional impairment
sds.scoring <- function(data,keep.vars,sds){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
  if (missing(sds)){
    sds <- c(paste('SDS_',1:5,sep=""),'SDS_1a')
  }
  
  raw.dat <- data[,sds]

  sds.work    <- data[,'SDS_1a']; names(sds.work) <- 'sds.work'
  sds.social  <- data[,'SDS_2']; names(sds.social) <- 'sds.social'
  sds.family  <- data[,'SDS_3']; names(sds.family) <- 'sds.family'
  
  if (max(sds.work, na.rm = T) <= 1){
    warning("SDS Work may be incorrectly assigned. Check that this is the item rated 0-10, 
            and not the TRUE/FALSE question of whether respondent did not work for reasons unrelated to disorder.")
  }

  sdsdisability.df <- cbind(sds.work,sds.social,sds.family)
  sds.impairedWork <- sdsdisability.df[,"sds.work"] >= 5
  sds.impairedSocial <- sdsdisability.df[,"sds.social"] >= 5
  sds.impairedFamily <- sdsdisability.df[,"sds.family"] >= 5

  vars.global <- c('SDS_1a','SDS_2','SDS_3')
  sds.globalFunctionalImpair <- rowSums(data[,vars.global],na.rm = T)

  sds.daysLost <- data[,'SDS_4']; names(sds.daysLost) <- 'sds.daysLost'
  sds.daysUnproductive <- data[,'SDS_5']; names(sds.daysUnproductive) <- 'sds.daysUnproductive'

  # get variables we want to pass through to the output
  # bring participant sub,id,date back into the data frame
  keeps <- data[,keep.vars]
  

  # combine scores and kept variables, make dataframe
  output <- cbind(keeps,raw.dat,sds.work,sds.impairedWork,sds.social,sds.impairedSocial,sds.family,
                 sds.impairedFamily,sds.globalFunctionalImpair,sds.daysLost,sds.daysUnproductive)

  output <- as.data.frame(output)
  return(output)
}

###################################################################################################
# SCS-SF (Self Compassion Scale - Short Form)
###################################################################################################
selfcompassion.scoring <- function(data,keep.vars,selfcompassion){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  

  if (missing(selfcompassion)){
    selfcompassion <- c(paste('SCS_',1:12,sep=""))
  }
  raw.dat <- data[,selfcompassion]

  revselfcompassion <- c(paste('SCS_',c(11,12,4,8,1,9),sep=""))
  data[,revselfcompassion] <- 6-data[,revselfcompassion]

  scs.total <- rowSums(data[,selfcompassion])/length(selfcompassion)

  keeps <- data[,keep.vars]
  
  output <- cbind(keeps,raw.dat,scs.total)
  output <- as.data.frame(output)
  return(output)
}

####################################################################
# Stanford Expectations for Treatment Scale (SETS) ## currently written for the ptsd modified version as well as original version
####################################################################
sets.scoring <- function(data, keep.vars, sets) {
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
  output <- data.frame() # initializing output variable
  if('SETS_1a' %in% colnames(data) & 'SETS_1b' %in% colnames(data)) { # scoring the ptsd version (only version in use atm)
    sets_a <- c()
    sets_b <- c()
    if(missing(sets)) {
      sets_a <- paste0('SETS_', c('1a', '2a', '3a', '4a', '5a', '6a', '8a', '9a'))
      sets_b <- paste0('SETS_', c('1b', '2b', '3b', '4b', '5b', '6b', '8b', '9b'))
      sets <- c(sets_a, sets_b)
    }
    
    raw.dat <- data[,sets]
    
    vars.treatment1Positive <- paste0('SETS_', c('1a', '3a', '5a'))
    vars.treatment1Negative <- paste0('SETS_', c('2a', '4a', '6a'))
    vars.treatment2Positive <- paste0('SETS_', c('1b', '3b', '5b'))
    vars.treatment2Negative <- paste0('SETS_', c('2b', '4b', '6b'))
    
    sets.treatment1Positive <- rowSums(data[,vars.treatment1Positive])/length(vars.treatment1Positive)
    sets.treatment1Negative <- rowSums(data[,vars.treatment1Negative])/length(vars.treatment1Negative)
    sets.treatment2Positive <- rowSums(data[,vars.treatment2Positive])/length(vars.treatment2Positive)
    sets.treatment2Negative <- rowSums(data[,vars.treatment2Negative])/length(vars.treatment2Negative)
    
    keeps <- data[,keep.vars]
    
    output <- cbind(keeps, raw.dat, sets.treatment1Positive, sets.treatment1Negative, 
                    sets.treatment2Positive, sets.treatment2Negative)
  } else { #default to scoring original scoring method
    if(missing(sets)) {
      sets <- paste0('SETS_', 1:10)
    }
    
    raw.dat <- data[,sets]
    
    vars.positive <- paste0('SETS_', c(1,3,5))
    vars.negative <- paste0('SETS_', c(2,4,6))
    
    sets.positive <- rowSums(data[,vars.positive])/length(vars.positive)
    sets.negative <- rowSums(data[,vars.negative])/length(vars.negative)
    
    keeps <- data[,keep.vars]
    
    output <- cbind(keeps, raw.dat, sets.positive, sets.negative)
  }
  
  output
}


####################################################################
#Scale of Emotional Exhaustion in Ministry (SEEM)
####################################################################
seem.scoring <- function(data, keep.vars, seem) {
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }

  if (missing(seem)) {
    seem <- c(paste('SEEM_',1:11,sep=""))
  }
  raw.dat <- data[,seem]
  
  #reverse scoring
  data[,'SEEM_5'] <- 6 - data[,'SEEM_5']

  #average score
  seem.total <- rowSums(data[,seem])/length(seem)

  keeps <- data[,keep.vars]

  output <- cbind(keeps, raw.dat, seem.total) %>%
    as.data.frame()
  output
}


###################################################################################################
# 12-Item Short Form Health Survey
###################################################################################################
sf12.scoring <- function(data, keep.vars, sf12) {
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }

  if (missing(sf12)) {
    sf12 <- c(paste('SF12_',1:12,sep=""))
  }
  raw.dat <- data[,sf12]

  # reverse score items 1, 8, 9, 10
  vars.rev.5p <- c(paste('SF12_',c(1,8),sep=""))
  data[,vars.rev.5p] <- 6-data[,vars.rev.5p]

  vars.rev.6p <- c(paste('SF12_',9:10,sep=""))
  data[,vars.rev.6p] <- 7-data[,vars.rev.6p]

  # create indicator variables
  # 1
  data$SF12_1[data$SF12_1 == 5] <- 0 #note 1 was rev scored, so "Excellent" is now 5

  # 2-3
  data$SF12_2[data$SF12_2 == 3] <- 0 #set indic var for "No not limited at all" to 0
  data$SF12_3[data$SF12_3 == 3] <- 0

  #4-7
  data$SF12_4[data$SF12_4 == 2] <- 0
  data$SF12_5[data$SF12_5 == 2] <- 0
  data$SF12_6[data$SF12_6 == 2] <- 0
  data$SF12_7[data$SF12_7 == 2] <- 0

  #8
  data$SF12_8[data$SF12_8 == 5] <- 0 # note that 8 was rev scored first, so now "Not at all" is 5

  #9-11
  data$SF12_9[data$SF12_9 == 6] <- 0 # 9&10 were reverse scored, so now "All of the time" is 6
  data$SF12_10[data$SF12_10 == 6] <- 0
  data$SF12_11[data$SF12_11 == 6] <- 0

  #12
  data$SF12_12[data$SF12_12 == 5] <- 0

  #weight indicator variables and sum across rows

  # weighting of physical component scores done by creating a vector for each item
  # that holds the weight for each response in that item's column
  pcs1 <- sapply(data$SF12_1, function(x) {ifelse(x==1,-8.37399,
                                                               ifelse(x==2, -5.56461,
                                                                      ifelse(x==3, -3.02396,
                                                                             ifelse(x==4,-1.31872,0))))})
  pcs2 <- sapply(data$SF12_2, function(x) {ifelse(x==1,-7.23216,
                                                               ifelse(x==2, -3.45555,0))})
  pcs3 <- sapply(data$SF12_3, function(x) {ifelse(x==1,-6.24397,
                                                               ifelse(x==2, -2.73557,0))})
  pcs4 <- sapply(data$SF12_4, function(x) {ifelse(x==1,-4.61617,0)})
  pcs5 <- sapply(data$SF12_5, function(x) {ifelse(x==1,-5.51747,0)})
  pcs6 <- sapply(data$SF12_6, function(x) {ifelse(x==1,3.04365,0)})
  pcs7 <- sapply(data$SF12_7, function(x) {ifelse(x==1,2.32091,0)})

  pcs8 <- sapply(data$SF12_8, function(x) {ifelse(x==1,-11.25544,
                                                               ifelse(x==2, -8.38063,
                                                                      ifelse(x==3, -6.50522,
                                                                             ifelse(x==4,-3.80130,0))))})
  pcs9 <- sapply(data$SF12_9, function(x) {ifelse(x==1,3.46638,
                                                               ifelse(x==2, 2.90426,
                                                                      ifelse(x==3, 2.37241,
                                                                             ifelse(x==4,1.36689,
                                                                                    ifelse(x==5,0.66514,0)))))})
  pcs10 <- sapply(data$SF12_10, function(x) {ifelse(x==1,-2.44706,
                                                                 ifelse(x==2, -2.02168,
                                                                        ifelse(x==3, -1.61850,
                                                                               ifelse(x==4, -1.14387,
                                                                                      ifelse(x==5, -.42251,0)))))})
  pcs11 <- sapply(data$SF12_11, function(x) {ifelse(x==1, 4.61446,
                                                                 ifelse(x==2, 3.41593,
                                                                        ifelse(x==3, 2.34247,
                                                                               ifelse(x==4,1.28044,
                                                                                      ifelse(x==5,0.41188,0)))))})
  pcs12 <- sapply(data$SF12_12, function(x) {ifelse(x==1,-.33682,
                                                                 ifelse(x==2, -0.94342,
                                                                        ifelse(x==3, -0.18043,
                                                                               ifelse(x==4,0.11038,0))))})

  pcs_weights <- cbind(pcs1, pcs2, pcs3, pcs4, pcs5, pcs6, pcs7, pcs8, pcs9, pcs10, pcs11, pcs12)
  pcs_12_summary_score <- rowSums(pcs_weights)
  sf12.physicalNorm <- pcs_12_summary_score + 56.57706

  # weighting of mental component scores done by creating a vector for each item
  # that holds the weight for each response in that item's column
  mcs1 <- sapply(data$SF12_1, function(x) {ifelse(x==1,-1.71175,
                                                               ifelse(x==2, -0.16891,
                                                                      ifelse(x==3, 0.03482,
                                                                             ifelse(x==4,-0.06064,0))))})
  mcs2 <- sapply(data$SF12_2, function(x) {ifelse(x==1,3.93115,
                                                               ifelse(x==2, 1.86840,0))})
  mcs3 <- sapply(data$SF12_3, function(x) {ifelse(x==1,2.68282,
                                                               ifelse(x==2, 1.43103,0))})
  mcs4 <- sapply(data$SF12_4, function(x) {ifelse(x==1,1.44060,0)})
  mcs5 <- sapply(data$SF12_5, function(x) {ifelse(x==1,1.66968,0)})
  mcs6 <- sapply(data$SF12_6, function(x) {ifelse(x==1,-6.82672,0)})
  mcs7 <- sapply(data$SF12_7, function(x) {ifelse(x==1, -5.69921,0)})

  mcs8 <- sapply(data$SF12_8, function(x) {ifelse(x==1,1.48619,
                                                               ifelse(x==2, 1.76691,
                                                                      ifelse(x==3, 1.49384,
                                                                             ifelse(x==4,0.90384,0))))})
  mcs9 <- sapply(data$SF12_9, function(x) {ifelse(x==1,-10.19085,
                                                               ifelse(x==2, -7.92717,
                                                                      ifelse(x==3, -6.31121,
                                                                             ifelse(x==4,-4.09842,
                                                                                    ifelse(x==5,-1.94949,0)))))})
  mcs10 <- sapply(data$SF12_10, function(x) {ifelse(x==1,-6.02409,
                                                                 ifelse(x==2, -4.88962,
                                                                        ifelse(x==3, -3.29805,
                                                                               ifelse(x==4, -1.65178,
                                                                                      ifelse(x==5, -0.92057,0)))))})
  mcs11 <- sapply(data$SF12_11, function(x) {ifelse(x==1, -16.15395,
                                                                 ifelse(x==2, -10.77911,
                                                                        ifelse(x==3, -8.09914,
                                                                               ifelse(x==4,-4.59055,
                                                                                      ifelse(x==5,-1.95934,0)))))})
  mcs12 <- sapply(data$SF12_12, function(x) {ifelse(x==1,-6.29724,
                                                                 ifelse(x==2, -8.26066,
                                                                        ifelse(x==3, -5.63286,
                                                                               ifelse(x==4, -3.13896,0))))})

  mcs_weights <- cbind(mcs1, mcs2, mcs3, mcs4, mcs5, mcs6, mcs7, mcs8, mcs9, mcs10, mcs11, mcs12)
  mcs_12_summary_score <- rowSums(mcs_weights)
  sf12.mentalNorm <- mcs_12_summary_score + 60.75781

  keeps <- data[,keep.vars]
  
  output <- cbind(keeps, raw.dat,sf12.physicalNorm,sf12.mentalNorm) %>%
    as.data.frame()
  output
}

#************************************************************************
# Short-Form Healthy Survey, Version 2 (SF36)
#************************************************************************
sf36.scoring <- function(data, keep.vars, sf36) {
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <- c(keep.vars, 'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub', keep.vars)
  }
  if (missing(sf36)) {
    sf36 <- c(paste('SF36_', c("1", "2", "3a", "3b", "3c", "3d", "3e", "3f",
                                "3g", "3h", "3i", "3j", "4a", "4b", "4c",
                                "4d", "5a", "5b", "5c", "6", "7", "8",
                                "9a", "9b", "9c", "9d", "9e", "9f", "9g",
                                "9h", "9i", "10", "11a", "11b", "11c", "11d"), sep = ""))
  }
  
  raw.dat <- data[,sf36]
  
  # recode into scale of 0->100
  items.5ptdec <- c(paste("SF36_", c("1", "2", "6", "8", "11b", "11d"), sep = ""))
  items.5ptinc <- c(paste("SF36_", c("10", "11a", "11c"), sep = ""))
  items.6ptdec <- c(paste("SF36_", c("7", "9a", "9d", "9e", "9h"), sep = ""))
  items.6ptinc <- c(paste("SF36_", c("9b", "9c", "9f", "9g", "9i"), sep = ""))
  items.3ptinc <- c(paste("SF36_", c("3a", "3b", "3c", "3d", "3e", "3f",
                                      "3g", "3h", "3i", "3j"), sep = ""))
  items.2ptinc <- c(paste("SF36_", c("4a", "4b", "4c","4d", "5a", "5b", "5c"), sep = ""))
  
  data[,items.5ptdec] <- sapply(data[,items.5ptdec], function(x) {
    ifelse(x==1,100,
           ifelse(x==2, 75,
                  ifelse(x==3, 50,
                         ifelse(x==4,25,
                                0))))}
  )
  data[,items.5ptinc] <- sapply(data[,items.5ptinc], function(x) {
    ifelse(x==1,0,
           ifelse(x==2, 25,
                  ifelse(x==3, 50,
                         ifelse(x==4,75,
                                100))))}
  )
  data[,items.6ptdec] <- sapply(data[,items.6ptdec], function(x) {
    ifelse(x==1,100,
           ifelse(x==2, 80,
                  ifelse(x==3, 60,
                         ifelse(x==4,40,
                                ifelse(x==5,20,
                                       0)))))}
  )
  data[,items.6ptinc] <- sapply(data[,items.6ptinc], function(x) {
    ifelse(x==1,0,
           ifelse(x==2, 20,
                  ifelse(x==3, 40,
                         ifelse(x==4,60,
                                ifelse(x==5,80,
                                       100)))))}
  )
  data[,items.3ptinc] <- sapply(data[,items.3ptinc], function(x) {
    ifelse(x==1,0,
           ifelse(x==2, 50, 100))}
  )
  data[,items.2ptinc] <- sapply(data[,items.2ptinc], function(x) {
    ifelse(x==1,0,100)}
  )
  
  
  # scoring
  vars.physical.functioning <- c(paste("SF36_", c("3a", "3b", "3c", "3d", "3e", "3f", "3g", "3h", "3i", "3j"), sep = ""))
  vars.physical.limitations <- c(paste("SF36_", c("4a", "4b", "4c", "4d"), sep = ""))
  vars.bodily.pain <- c(paste("SF36_", c("7", "8"), sep = ""))
  vars.social.functioning <- c(paste("SF36_", c("6", "10"), sep = ""))
  vars.mental.health <- c(paste("SF36_", c("9b", "9c", "9d", "9f", "9h"), sep = ""))
  vars.emotional.limitations <- c(paste("SF36_", c("5a", "5b", "5c"), sep = ""))
  vars.vitality <- c(paste("SF36_", c("9a", "9e", "9g", "9i"), sep = ""))
  vars.general.health.perceptions <- c(paste("SF36_", c("1","11a", "11b", "11c", "11d"), sep = ""))
  
  sf36.physicalFunctioning <- rowSums(data[,vars.physical.functioning])/length(vars.physical.functioning)
  sf36.physicalLimitations <- rowSums(data[,vars.physical.limitations])/length(vars.physical.limitations)
  sf36.bodilyPain <- rowSums(data[,vars.bodily.pain])/length(vars.bodily.pain)
  sf36.socialFunctioning <- rowSums(data[,vars.social.functioning])/length(vars.social.functioning)
  sf36.mentalHealth <- rowSums(data[,vars.mental.health])/length(vars.mental.health)
  sf36.emotionalLimitations <- rowSums(data[,vars.emotional.limitations])/length(vars.emotional.limitations)
  sf36.vitality <- rowSums(data[,vars.vitality])/length(vars.vitality)
  sf36.generalHealthPerceptions <- rowSums(data[,vars.general.health.perceptions])/length(vars.general.health.perceptions)
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps, raw.dat, sf36.physicalFunctioning, sf36.physicalLimitations, sf36.bodilyPain,
                  sf36.socialFunctioning, sf36.mentalHealth, sf36.emotionalLimitations, sf36.vitality, 
                  sf36.generalHealthPerceptions)
  output <- as.data.frame(output)
  return(output)
}



####################################################################
# Satisfaction in Ministry Scale (SIMS)
####################################################################
sims.scoring <- function(data, keep.vars, sims) {
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }

  if (missing(sims)) {
    sims <- c(paste('SIMS_',1:11,sep=""))
  }
  raw.dat <- data[,sims]

  sims.total <- rowSums(data[,sims])/length(sims)

  keeps <- data[,keep.vars]
  
  output <- cbind(keeps, raw.dat, sims.total) %>%
    as.data.frame()
  output

}

#****************************************************************************
# Sensed Presence Questionnaire
sensedpres.scoring <- function(data,keep.vars,sensedpres){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
  if (missing(sensedpres)){
    sensedpres <- c(paste('SensedPres_',1:5,sep=""))
  }
  raw.dat <- data[,sensedpres]
  
  vars.sensedpresMalignant <- c(paste('SensedPres_',2:3,sep=""))
  vars.sensedpresBenign <- c(paste('SensedPres_',c(1,4,5),sep=""))
  
  sensedpres.malignant <- rowMeans(data[,vars.sensedpresMalignant], na.rm = T)
  sensedpres.benign <- rowMeans(data[,vars.sensedpresBenign], na.rm = T)
  
  keeps <- data[,keep.vars]
  
  # combine scores and kept variables, make dataframe
  output <- cbind(keeps,raw.dat,sensedpres.malignant,sensedpres.benign)
  output <- as.data.frame(output)
  
  # return
  return(output)
  
}

#************************************************************************
# Shaith-Hamilton Pleasure Scale (SHAPS)
#************************************************************************
shaps.scoring <- function(data, keep.vars, shaps) {
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <- c(keep.vars, 'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub', keep.vars)
  }
  if (missing(shaps)) {
    shaps <- c(paste('SHAPS_', 1:14, sep = ""))
  }
  
  raw.dat <- data[,shaps]
  
  rev.items <- c(paste("SHAPS_", c(2,4,5,7,9), sep = ""))
  data[,rev.items] <- 3-data[,rev.items]
  
  shaps.total <- rowSums(data[,shaps])
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps,raw.dat,shaps.total)
  output <- as.data.frame(output)
  return(output)
}

#************************************************************************
# States of Consciousness Questionnaire (SOCQ)
#************************************************************************
socq.scoring <- function(data, keep.vars, socq) {
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <- c(keep.vars, 'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub', keep.vars)
  }
  
  socq_numitems <- length(grep('SOCQ_', colnames(data)))
  
  if(socq_numitems == 43) { # try scoring 43-item version
    if (missing(socq)) {
      socq <- c(paste('SOCQ_', 1:43, sep = ""))
    }
    
    raw.dat <- data[,socq]
    
    #Compute the sum of all item scores for the items in each subscale to get that subscale score.
    vars.internalUnity <- c(paste("SOCQ_", c(15,21,23,30,39,41), sep = ""))
    vars.externalUnity <- c(paste("SOCQ_", c(9,16,26,29,34,38), sep = ""))
    vars.transcendance <- c(paste("SOCQ_", c(1,8,10,17,20,24,27,35), sep = ""))
    vars.ineffability <- c(paste("SOCQ_", c(4,12,14,32,42), sep = ""))
    vars.sacredness <- c(paste("SOCQ_", c(3,5,19,22,31,37,40), sep = ""))
    vars.noeticQuality <- c(paste("SOCQ_", c(2,6,13,36), sep = ""))
    vars.positiveMood <- c(paste("SOCQ_", c(7,11,18,25,28,33,43), sep = ""))
  } else {
    if (missing(socq)) {
      socq <- c(paste('SOCQ_', 1:100, sep = ""))
    }
    
    raw.dat <- data[,socq]
    
    #Compute the sum of all item scores for the items in each subscale to get that subscale score.
    vars.internalUnity <- c(paste("SOCQ_", c(26,35,41,54,77,83), sep = ""))
    vars.externalUnity <- c(paste("SOCQ_", c(14,27,47,51,62,74), sep = ""))
    vars.transcendance <- c(paste("SOCQ_", c(2,12,15,29,34,42,48,65), sep = ""))
    vars.ineffability <- c(paste("SOCQ_", c(6,19,23,59,86), sep = ""))
    vars.sacredness <- c(paste("SOCQ_", c(5,8,31,36,55,73,80), sep = ""))
    vars.noeticQuality <- c(paste("SOCQ_", c(3,9,22,69), sep = ""))
    vars.positiveMood <- c(paste("SOCQ_", c(10,18,30,43,50,60,87), sep = ""))
  }
    
  socq.internalUnity <- rowSums(data[,vars.internalUnity])
  socq.externalUnity <- rowSums(data[,vars.externalUnity])
  socq.transcendenceOfTimeAndSpace <- rowSums(data[,vars.transcendance])
  socq.ineffabilityAndParadoxicality <- rowSums(data[,vars.ineffability])
  socq.senseOfSacredness <- rowSums(data[,vars.sacredness])
  socq.noeticQuality <- rowSums(data[,vars.noeticQuality])
  socq.deeplyFeltPositiveMood <- rowSums(data[,vars.positiveMood])
  
  #Transform all item-level data into "percentage of maximum possible" and compute the average
  #of all transformed item scores for the items in each subscale to get that subscale score.
  #Each subscale score from the step prior was divided by the maximum possible subscale score,
  #which is the number of items in that subscale multiplied by 5 (the max score per item).
  
  socq.internalUnity.pctmax <- socq.internalUnity/(length(vars.internalUnity)*5)
  socq.externalUnity.pctmax <- socq.externalUnity/(length(vars.externalUnity)*5)
  socq.transcendenceOfTimeAndSpace.pctmax <- socq.transcendenceOfTimeAndSpace/(length(vars.transcendance)*5)
  socq.ineffabilityAndParadoxicality.pctmax <- socq.ineffabilityAndParadoxicality/(length(vars.ineffability)*5)
  socq.senseOfSacredness.pctmax <- socq.senseOfSacredness/(length(vars.sacredness)*5)
  socq.noeticQuality.pctmax <- socq.noeticQuality/(length(vars.noeticQuality)*5)
  socq.deeplyFeltPositiveMood.pctmax <- socq.deeplyFeltPositiveMood/(length(vars.positiveMood)*5)
  
  
  #Compute the average of all transformed subscale scores to get the total SOCQ score. 43 items are scored,
  #meaning that 215 (43*5) is the max possible points. Therefore, the sum of subscale scores divided by 215
  #is the total score. 
  socq.total <- (socq.internalUnity + socq.externalUnity + socq.transcendenceOfTimeAndSpace
                 + socq.ineffabilityAndParadoxicality + socq.senseOfSacredness + socq.noeticQuality
                 + socq.deeplyFeltPositiveMood)/(5*(length(vars.internalUnity) + length(vars.externalUnity)
                                                       + length(vars.transcendance) + length(vars.ineffability)
                                                       + length(vars.sacredness) + length(vars.noeticQuality)
                                                       + length(vars.positiveMood)))
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps, raw.dat, socq.internalUnity, socq.externalUnity,
                  socq.transcendenceOfTimeAndSpace, socq.ineffabilityAndParadoxicality,
                  socq.senseOfSacredness, socq.noeticQuality, socq.deeplyFeltPositiveMood,
                  socq.internalUnity.pctmax, socq.externalUnity.pctmax,
                  socq.transcendenceOfTimeAndSpace.pctmax, socq.ineffabilityAndParadoxicality.pctmax,
                  socq.senseOfSacredness.pctmax, socq.noeticQuality.pctmax, socq.deeplyFeltPositiveMood.pctmax,
                  socq.total)
  output <- as.data.frame(output)
  return(output)
}



#****************************************************************************
# The Symptoms of Major Depressive Disorder Scale (SMDDS)
smdds.scoring <- function(data, keep.vars, smdds) {
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
  if (missing(smdds)) {
    smdds <- c(paste('SMDDS_',1:16,sep=""))
  }
  
  raw.dat <- data[,smdds]
  
  vars.smdds.negemotions <- c(paste('SMDDS_', c(1,2,3,9), sep=""))
  vars.smdds.anxiety <- c(paste('SMDDS_', 4:5, sep=""))
  vars.smdds.cognition <-c(paste('SMDDS_', 7:8, sep=""))
  vars.smdds.lowmotivation <- c(paste('SMDDS_', 13:14, sep=""))

  
  #categories with 1 item will be reported as is
  smdds.lowEnergy <- data[,c('SMDDS_6')]
  smdds.sleepDisturbance <- data[,c('SMDDS_10')]
  smdds.selfHarm <- data[,c('SMDDS_16')]
  smdds.senseOfSelf <- data[,c('SMDDS_15')]
  
  # only use most severe response for overall eating behavior score
  vars.smdds.eatingbehavior <- c(paste('SMDDS_', 11:12, sep=""))
  
  # means
  smdds.negEmotions <- rowSums(data[,vars.smdds.negemotions])
  smdds.anxiety <- rowSums(data[,vars.smdds.anxiety])
  smdds.cognition <- rowSums(data[,vars.smdds.cognition])
  smdds.lowMotivation <- rowSums(data[,vars.smdds.lowmotivation])
  smdds.eatingBehavior <- base::apply(data[,vars.smdds.eatingbehavior], 1, max)

  smdds.total <- rowSums(data[,smdds])
  
  keeps <- data[,keep.vars]
  output <- cbind(keeps, raw.dat, smdds.negEmotions, smdds.anxiety,
                  smdds.lowEnergy, smdds.cognition, smdds.sleepDisturbance, 
                  smdds.selfHarm, smdds.lowMotivation, smdds.senseOfSelf,
                  smdds.eatingBehavior, smdds.total) %>% 
    as.data.frame()
  output
}

#******************************************************************
# Social Anxiety Disorder Severity Measure
# 10 items, Scored 0-4, Total Average Range 0-4
#******************************************************************
sad.scoring <- function(data, keep.vars, sad) {
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
  if (missing(sad)) {
    sad <- c(paste('SAD_', 1:10, sep=""))
  }
  
  raw.dat <- data[,sad]
  
  # calculate score
  sad.total <- rowSums(data[,sad])
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps, raw.dat, sad.total)
  output <- as.data.frame(output)
  return(output)
}

#************************************************************************
# Subjective Opiate Withdrawal Scale (SOWS)
#************************************************************************
sows.scoring <- function(data, keep.vars, sows) {
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <- c(keep.vars, 'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub', keep.vars)
  }
  if (missing(sows)) {
    sows <- c(paste('SOWS_', 1:16, sep = ""))
  }
  
  raw.dat <- data[,sows]
  
  sows.total <- rowSums(data[,sows])
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps,raw.dat,sows.total)
  output <- as.data.frame(output)
  return(output)
}
#************************************************************************

#************************************************************************
# Short Opiate Withdrawal Scale (SOWS-Gossop)
#************************************************************************
sowsgossop.scoring <- function(data, keep.vars, sowsgossop) {
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <- c(keep.vars, 'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub', keep.vars)
  }
  if (missing(sowsgossop)) {
    sowsgossop <- c(paste('SOWSGossop_', 1:10, sep = ""))
  }
  
  raw.dat <- data[,sowsgossop]
  
  sowsgossop.total <- rowSums(data[,sowsgossop])
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps, raw.dat, sowsgossop.total)
  output <- as.data.frame(output)
  return(output)
}

#****************************************************************************
# Sanctification of Strivings - 0905

sanctstrive.scoring <- function(data, keep.vars, sanctstrive) {
        keep.vars <- c('ID','date')
        if('time.point' %in% colnames(data))
        {
                keep.vars <-c(keep.vars,'time.point')
        }
        if('sub' %in% colnames(data))
        {
                keep.vars <- c('sub', keep.vars)
        }
        
        if (missing(sanctstrive)) {
                sanctstrive <- c(paste('SS_', 1:26, sep= ""))
        }
        
        raw.dat <- data[, sanctstrive]
        
        # define subscales
        vars.sanctstriveHoly <- c(paste('SS_', c(2,5,8,13,14), sep= ""))
        vars.sanctstriveSpiritual <- c(paste('SS_', c(2,14), sep= ""))
        
        #calculate average score
        sanctstrive.holy <- rowSums(data[,vars.sanctstriveHoly])/length(vars.sanctstriveHoly)
        sanctstrive.spiritual <- rowSums(data[, vars.sanctstriveSpiritual])/length(vars.sanctstriveSpiritual)
        
        keeps <- data[, keep.vars]
        
        output <- cbind(keeps, raw.dat, sanctstrive.holy, sanctstrive.spiritual)
        output <- as.data.frame(output)
        return(output)
}

#****************************************************************************
# Spiritual-Related Growth Scale (SRG)
srg.scoring <- function(data, keep.vars, srg) {
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
  if (missing(srg)) {
    srg <- c(paste('SRG_', 1:15, sep= ""))
  }
  
  raw.dat <- data[,srg]
  
  srg.total <- rowSums(data[,srg])
  
  keeps <- data[,keep.vars]
  
  output<- as.data.frame(cbind(keeps, raw.dat, srg.total))
  return(output)
}

#****************************************************************************
# STAI (State Trait Anxiety Inventory)
#items are scored on a 4-point scale
#all anxiety-absent items are reverse scored
stai.scoring <- function(data,keep.vars,stai){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
  raw.colnames <- colnames(data)

  if(length(grep("STAI_", raw.colnames)) == 40){
    if (missing(stai)){
      if ('STAI_State_1' %in% colnames(data) & 'STAI_Trait_1' %in% colnames(data)) {
        stai <- c(c(paste('STAI_State_',1:20,sep="")), c(paste('STAI_Trait_',1:20,sep="")))
      } else {
        stai <- c(paste('STAI_',1:40,sep = ""))
      }
    }
    raw.dat <- data[,stai]
    vars.stai <- stai
    
    if ('STAI_State_1' %in% colnames(data) & 'STAI_Trait_1' %in% colnames(data)) {
      vars.staiState <- c(paste('STAI_State_',1:20,sep=""))
      vars.staiTrait <- c(paste('STAI_Trait_',1:20,sep=""))
      revstai <- c(c(paste('STAI_State_',c(1,2,5,8,10,11,15,16,19,20), sep="")),c(paste('STAI_Trait_',c(1,3,6,7,10,13,14,16,19),sep="")))
    } else {
      vars.staiState <- c(paste('STAI_',1:20,sep=""))
      vars.staiTrait <- c(paste('STAI_',21:40,sep=""))
      revstai <- c(paste('STAI_',c(1,2,5,8,10,11,15,16,19,20,21,23,26,27,30,33,34,36,39),sep=""))
    }
    
    data[,revstai] = 5-data[,revstai]
    
    stai.total <- rowSums(data[,vars.stai])
    stai.state <- rowSums(data[,vars.staiState])
    stai.trait <- rowSums(data[,vars.staiTrait])
    
    keeps <- data[,keep.vars]
    output <- cbind(keeps,raw.dat,stai.state,stai.trait,stai.total)
    output <- as.data.frame(output)
  } else if(length(grep("STAI_", raw.colnames)) == 20 & !('STAI_21' %in% raw.colnames)) { # just scoring stai.state
    
    if(missing(stai)) {
      stai <- paste0('STAI_', 1:20)
    }
    
    raw.dat <- data[,stai]
    
    revstai <- c(paste('STAI_',c(1,2,5,8,10,11,15,16,19,20),sep=""))
    data[,revstai] = 5-data[,revstai]
    
    vars.staiState <- c(paste('STAI_',1:20,sep=""))
    stai.state <- rowSums(data[,vars.staiState])
    
    keeps <- data[,keep.vars]
    
    output <- cbind(keeps,raw.dat,stai.state)
    output <- as.data.frame(output)
  }
  return(output)
}


#****************************************************************************
# State Shame Guilt Scale (SSGS)
#****************************************************************************
ssgs.scoring <- function(data, keep.vars, ssgs){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
  if (missing(ssgs)){
    ssgs <- c(paste('SSGS_',1:13,sep=""))
  }
  raw.dat <- data[,ssgs]
  
  vars.ssgsShame <- paste('SSGS_',c(1,3,5,7,9),sep="")
  vars.ssgsGuilt <- paste('SSGS_',c(2,4,6,8,10),sep="")
  
  ssgs.shame <- rowSums(data[,vars.ssgsShame])
  ssgs.guilt <- rowSums(data[,vars.ssgsGuilt])
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps,raw.dat,ssgs.shame,ssgs.guilt)
  output <- as.data.frame(output)
  return(output)
}

#****************************************************************************
# State of Surrender Questionnaire (SoS)
statesurrender.scoring <- function(data, keep.vars, statesurrender){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
  if (missing(statesurrender)){
    statesurrender <- c(paste('StSurrender_',1:12,sep=""))
  }
  raw.dat <- data[,statesurrender]
  
  vars.stsurrenderScored <- paste('StSurrender_',1:10,sep="")
  stsurrender.total <- rowSums(data[,vars.stsurrenderScored])/length(vars.stsurrenderScored)
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps,raw.dat,stsurrender.total)
  output <- as.data.frame(output)
  return(output)
}

#************************************************************************
# Spiritual Transcendence Scale (STS)
#************************************************************************
sts.scoring <- function(data, keep.vars, sts) {
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <- c(keep.vars, 'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub', keep.vars)
  }
  if (missing(sts)) {
    sts <- c(paste('STS_', 1:24, sep = ""))
  }
  
  raw.dat <- data[,sts]
  
  vars.connectedness <- c(paste("STS_", c(1,7,8,9,12,22), sep = ""))
  vars.prayerFulfillment <- c(paste("STS_", c(2,3,10,14,17,18,19,20,21), sep = ""))
  vars.universality <- c(paste("STS_", c(4,5,6,11,13,15,16,23,24), sep = ""))
  
  sts.connectedness <- rowSums(data[,vars.connectedness])
  sts.prayerFulfillment <- rowSums(data[,vars.prayerFulfillment])
  sts.universality <- rowSums(data[,vars.universality])
  sts.total <- rowSums(data[,sts])
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps,raw.dat,sts.connectedness,sts.universality, sts.prayerFulfillment,sts.total)
  output <- as.data.frame(output)
  return(output)
}

#************************************************************************
# MODIFIED Spiritual Transcendence Scale (STS)
# studies 0014 and 0502 have re-worded question text from the original measure (placing 'NOT' in multiple questions). 
# This scoring function accounts for the negatively worded items with reverse scoring.
#************************************************************************
stsMod.scoring <- function(data, keep.vars, sts) {
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <- c(keep.vars, 'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub', keep.vars)
  }
  if (missing(sts)) {
    sts <- c(paste('STSMod_', 1:24, sep = ""))
  }
  
  raw.dat <- data[,sts]
  
  rev.items <- paste0("STSMod_", c(7, 9, 13, 15, 16, 19)) # remove 17 from rev.items?
  data[,rev.items] <- 6-data[,rev.items]
  
  vars.connectedness <- c(paste("STSMod_", c(1,7,8,9,12,22), sep = ""))
  vars.prayerFulfillment <- c(paste("STSMod_", c(2,3,10,14,17,18,19,20,21), sep = ""))
  vars.universality <- c(paste("STSMod_", c(4,5,6,11,13,15,16,23,24), sep = ""))
  
  sts.connectedness <- rowSums(data[,vars.connectedness])
  sts.prayerFulfillment <- rowSums(data[,vars.prayerFulfillment])
  sts.universality <- rowSums(data[,vars.universality])
  sts.total <- rowSums(data[,sts])
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps,raw.dat,sts.connectedness,sts.universality, sts.prayerFulfillment,sts.total)
  output <- as.data.frame(output)
  return(output)
}

#****************************************************************************
# Schwartz Value Scale

svs.scoring <- function(data, keep.vars, svs) {
        keep.vars <- c('ID','date')
        if('time.point' %in% colnames(data))
        {
                keep.vars <-c(keep.vars,'time.point')
        }
        if('sub' %in% colnames(data))
        {
                keep.vars <- c('sub', keep.vars)
        }
        
        if (missing(svs)) {
                svs <- c(paste('SVS_', 1:57, sep= ""),paste('MRAT_SVS_', 1:57, sep= ""))
        }
        
        raw.dat <- data[,svs]
        
        # define subscales
        vars.svsConformity <- c(paste('SVS_', c(11, 20, 40, 47), sep= ""))
        vars.svsTradition <- c(paste('SVS_', c(18, 32, 36, 44, 51), sep= ""))
        vars.svsBenevolence <- c(paste('SVS_', c(33, 45, 49, 52, 54), sep= ""))
        vars.svsUniversalism <- c(paste('SVS_', c(1, 17, 24, 26, 29, 30, 35, 38), sep = ""))
        vars.svsSelfDirection <- c(paste('SVS_', c(5, 16, 31, 41, 53), sep = ""))
        vars.svsStimulation <- c(paste('SVS_', c(9, 25, 37), sep = ""))
        vars.svsHedonism <- c(paste('SVS_', c(4, 50, 57), sep = ""))
        vars.svsAchievement <- c(paste('SVS_', c(34, 39, 43, 55), sep = ""))
        vars.svsPower <- c(paste('SVS_', c(3, 12, 27, 46), sep = ""))
        vars.svsSecurity <- c(paste('SVS_', c(8, 13, 15, 22, 56), sep = ""))
        
        #calculate average score
        svs.conformity <- rowSums(data[,vars.svsConformity])/length(vars.svsConformity)
        svs.tradition <- rowSums(data[,vars.svsTradition])/length(vars.svsTradition)
        svs.benevolence <- rowSums(data[,vars.svsBenevolence])/length(vars.svsBenevolence)
        svs.universalism <- rowSums(data[,vars.svsUniversalism])/length(vars.svsUniversalism)
        svs.selfDirection <- rowSums(data[,vars.svsSelfDirection])/length(vars.svsSelfDirection)
        svs.stimulation <- rowSums(data[,vars.svsStimulation])/length(vars.svsStimulation)
        svs.hedonism <- rowSums(data[,vars.svsHedonism])/length(vars.svsHedonism)
        svs.achievement <- rowSums(data[,vars.svsAchievement])/length(vars.svsAchievement)
        svs.power <- rowSums(data[,vars.svsPower])/length(vars.svsPower)
        svs.security <- rowSums(data[,vars.svsSecurity])/length(vars.svsSecurity)
        
        # svs.mrat <- rowSums(data[,svs])/length(svs)
        # 
        # #calculating the MRAT - corrected item scores 
        # #MRAT = average of all item scores
        # #MRAT score = (original value) - MRAT
        # 
        # data <- raw.dat
        # 
        # mrat.svs <- c(paste('SVS_MRAT_', 1:57, sep = ""))
        # data <- setnames(data, old = svs, new = mrat.svs)
        # 
        # data <- t(scale(t(raw.dat), scale = FALSE))

        
        vars.svsConformityMRAT <- c(paste('MRAT_SVS_', c(11, 20, 40, 47), sep= ""))
        vars.svsTraditionMRAT <- c(paste('MRAT_SVS_', c(18, 32, 36, 44, 51), sep= ""))
        vars.svsBenevolenceMRAT <- c(paste('MRAT_SVS_', c(33, 45, 49, 52, 54), sep= ""))
        vars.svsUniversalismMRAT <- c(paste('MRAT_SVS_', c(1, 17, 24, 26, 29, 30, 35, 38), sep = ""))
        vars.svsSelfDirectionMRAT <- c(paste('MRAT_SVS_', c(5, 16, 31, 41, 53), sep = ""))
        vars.svsStimulationMRAT <- c(paste('MRAT_SVS_', c(9, 25, 37), sep = ""))
        vars.svsHedonismMRAT <- c(paste('MRAT_SVS_', c(4, 50, 57), sep = ""))
        vars.svsAchievementMRAT <- c(paste('MRAT_SVS_', c(34, 39, 43, 55), sep = ""))
        vars.svsPowerMRAT <- c(paste('MRAT_SVS_', c(3, 12, 27, 46), sep = ""))
        vars.svsSecurityMRAT <- c(paste('MRAT_SVS_', c(8, 13, 15, 22, 56), sep = ""))
        
        #calculate average score
        svs.conformityMRAT <- rowSums(data[,vars.svsConformityMRAT])/length(vars.svsConformityMRAT)
        svs.traditionMRAT <- rowSums(data[,vars.svsTraditionMRAT])/length(vars.svsTraditionMRAT)
        svs.benevolenceMRAT <- rowSums(data[,vars.svsBenevolenceMRAT])/length(vars.svsBenevolenceMRAT)
        svs.universalismMRAT <- rowSums(data[,vars.svsUniversalismMRAT])/length(vars.svsUniversalismMRAT)
        svs.selfDirectionMRAT <- rowSums(data[,vars.svsSelfDirectionMRAT])/length(vars.svsSelfDirectionMRAT)
        svs.stimulationMRAT <- rowSums(data[,vars.svsStimulationMRAT])/length(vars.svsStimulationMRAT)
        svs.hedonismMRAT <- rowSums(data[,vars.svsHedonismMRAT])/length(vars.svsHedonismMRAT)
        svs.achievementMRAT <- rowSums(data[,vars.svsAchievementMRAT])/length(vars.svsAchievementMRAT)
        svs.powerMRAT <- rowSums(data[,vars.svsPowerMRAT])/length(vars.svsPowerMRAT)
        svs.securityMRAT <- rowSums(data[,vars.svsSecurityMRAT])/length(vars.svsSecurityMRAT)
        
        keeps <- data[, keep.vars]
        
        output <- cbind(keeps, raw.dat, 
                        svs.conformity, svs.tradition, svs.benevolence, svs.universalism,
                        svs.selfDirection, svs.stimulation, svs.hedonism, svs.achievement, 
                        svs.power, svs.security,
                        
                        svs.conformityMRAT, svs.traditionMRAT, svs.benevolenceMRAT, 
                        svs.universalismMRAT, svs.selfDirectionMRAT, svs.stimulationMRAT,
                        svs.hedonismMRAT, svs.achievementMRAT, svs.powerMRAT, svs.securityMRAT)
        output <- as.data.frame(output)
        return(output)
}

#****************************************************************************
# SWLS (Satisfaction with Life Scale)
swls.scoring <- function(data,keep.vars,swls){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }

  if (missing(swls)){
    swls <- c(paste('SWLS_',1:5,sep=""))
  }
  raw.dat <- data[,swls]

  swls.total <- rowSums(data[,swls])

  keeps <- data[,keep.vars]
  
  output <- cbind(keeps,raw.dat,swls.total)
  output <- as.data.frame(output)
  return(output)
}

#****************************************************************************
# TAS (Tellegen Absorption Scale)
tas.scoring <- function(data,keep.vars,tas){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }

  if (missing(tas)){
    tas <- c(paste('TAS_',1:34,sep=""))
  }
  raw.dat <- data[,tas]

  tas.total <- rowSums(data[,tas])

  keeps <- data[,keep.vars]

  output <- cbind(keeps,raw.dat,tas.total)
  output <- as.data.frame(output)
  return(output)
}
#************************************************************************
# TCI-R Self-Transcendence Scale (TCI-R_STS)
#************************************************************************
tcir.sts.scoring <- function(data, keep.vars, tcir.sts.names) {
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <- c(keep.vars, 'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub', keep.vars)
  }
  if (missing(tcir.sts)) {
    tcir.sts.names <- c(paste('TCIR_STS_', 1:26, sep = ""))
  }
  
  raw.dat <- data[,tcir.sts.names]
  
  rev.items <- c(paste("TCIR_STS_", c(1,2,17), sep = ""))
  data[,rev.items] <- 6-data[,rev.items]
  
  tcir.sts <- rowSums(data[,tcir.sts.names])
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps,raw.dat,tcir.sts)
  output <- as.data.frame(output)
  return(output)
}
#************************************************************************
# Transgression Narrative Test of Forgiveness (TNTF)
#************************************************************************
tntf.scoring <- function(data, keep.vars, tntf) {
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <- c(keep.vars, 'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub', keep.vars)
  }
  if (missing(tntf)) {
    tntf <- c(paste('TNTF_', 1:5, sep = ""))
  }
  
  raw.dat <- data[,tntf]
  
  tntf.total <- rowSums(data[,tntf])
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps,raw.dat,tntf.total)
  output <- as.data.frame(output)
  return(output)
}

#*#************************************************************************
# Ten Item Personality Inventory (TIPI)
#************************************************************************
tipi.scoring <- function(data, keep.vars, tipi){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
  if (missing(tipi)){
    tipi <- c(paste('TIPI_', 1:10, sep = ""))
  }
  raw.dat <- data[,tipi]
  
  rev.tipi <- c(paste('TIPI_', c(2, 4, 6, 8, 10), sep = ""))
  data[,rev.tipi] = 8-data[,rev.tipi]
  
  vars.extraversion <- c(paste('TIPI_', c(1, 6), sep = ""))
  vars.agreeableness <- c(paste('TIPI_', c(2, 7), sep = ""))
  vars.conscientiousness <- c(paste('TIPI_', c(3, 8), sep = ""))
  vars.emotionalStability <- c(paste('TIPI_', c(4, 9), sep = ""))
  vars.openness <- c(paste('TIPI_', c(5, 10), sep = ""))
  
  tipi.extraversion <- rowSums(data[,vars.extraversion])/length(vars.extraversion)
  tipi.agreeableness <- rowSums(data[,vars.agreeableness])/length(vars.agreeableness)
  tipi.conscientiousness <- rowSums(data[,vars.conscientiousness])/length(vars.conscientiousness)
  tipi.emotionalStability <- rowSums(data[,vars.emotionalStability])/length(vars.emotionalStability)
  tipi.openness <- rowSums(data[,vars.openness])/length(vars.openness)
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps, raw.dat, tipi.extraversion, tipi.agreeableness, tipi.conscientiousness,
                  tipi.emotionalStability, tipi.openness)
  
  output <- as.data.frame(output)
  return(output)
}
#****************************************************************************
# TRIM (Transgression-Related Interpersonal Motivations)
trim.scoring <- function(data,keep.vars,trim){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }

  if (missing(trim)){
    trim <- c(paste('TRIM_',1:18,sep=""))
  }
  raw.dat <- data[,trim]
  
  if (min(data[,trim], na.rm = T) < 0){ # if score range is -2 to 2, then convert scale to 1-5, na.rm = T because min will set NA as lowest value if present
    data[,trim] <- 3 + data[,trim]
  }

  vars.trimAvoidance <- c(paste('TRIM_',c(2,5,7,10,11,15,18),sep=""))
  vars.trimRevenge <- c(paste('TRIM_',c(1,4,9,13,17),sep=""))
  vars.trimBenevolence <- c(paste('TRIM_',c(3,6,8,12,14,16),sep=""))

  trim.avoidance <- rowSums(data[,vars.trimAvoidance])
  trim.revenge <- rowSums(data[,vars.trimRevenge])
  trim.benevolence <- rowSums(data[,vars.trimBenevolence])

  keeps <- data[,keep.vars]
  
  output <- cbind(keeps,raw.dat,trim.avoidance,trim.revenge,trim.benevolence)
  output <- as.data.frame(output)
  return(output)
}

#****************************************************************************
# TFEQ (Three Factor Eating Questionnaire)
tfeq.scoring <- function(data, keep.vars, tfeq) {
  keep.vars <- c('sub','ID','date')
  if("timepoint" %in% colnames(data))
  {
    keep.vars <-c('sub','ID','date','time.point')
  }
  
  if (missing(tfeq)) {
    tfeq <- c(paste('TFEQ_', 1:51, sep=""))
  }
  raw.dat <- data[,tfeq]
  # reverse score
  vars.rev <- c(paste('TFEQ_', c(10,16,21,25,30,31), sep=""))
  data[,vars.rev] <- 1-data[,vars.rev]
  
  # assign 1 or 0 for part 2 items
  p2.plus <- c(paste('TFEQ_', c(37:46,48,49,51), sep=""))
  data[,p2.plus] <- ifelse((data[,p2.plus]==3 | data[,p2.plus]==4), 1, 0)
  data['TFEQ_50'] <- ifelse((data[,'TFEQ_50']==3 | data[,'TFEQ_50']==4 | data[,'TFEQ_50']==5), 1, 0)
  p2.minus <- c('TFEQ_47')
  data[p2.minus] <- data[,p2.minus] <- ifelse((data[,p2.minus]==1 | data[,p2.minus]==2), 1, 0)
  
  # split items into factors
  vars.tfeqCogRestraintEating <- c(paste('TFEQ_', c(4,6,10,14,18,21,23,28,
                                                30,32,33,35,37,38,40,
                                                42,43,44,46,48,50), sep=""))
  vars.tfeqDisinhibition <- c(paste('TFEQ_', c(1,2,7,9,11,13,15,16,20,25,
                                           27,31,36,45,49,51), sep=""))
  vars.tfeqHunger <- c(paste('TFEQ_', c(3,5,8,12,17,19,22,24,26,29,
                                    34,39,41,47), sep=""))
  # get total scores
  tfeq.cogRestraintEating <- rowSums(data[,vars.tfeqCogRestraintEating])
  tfeq.disinhibition <- rowSums(data[,vars.tfeqDisinhibition])
  tfeq.hunger <- rowSums(data[,vars.tfeqHunger])
  
  # get percentage maximum score
  tfeq.cogRestraintEating.pctmax <- tfeq.cogRestraintEating/length(vars.tfeqCogRestraintEating)
  tfeq.disinhibition.pctmax <- tfeq.disinhibition/length(vars.tfeqDisinhibition)
  tfeq.hunger.pctmax <- tfeq.hunger/length(vars.tfeqHunger)
  
  #output
  keeps <- data[,keep.vars]
  output <- cbind(keeps, raw.dat, tfeq.cogRestraintEating, tfeq.disinhibition, 
                  tfeq.hunger, tfeq.cogRestraintEating.pctmax, tfeq.disinhibition.pctmax,
                  tfeq.hunger.pctmax) %>%
    as.data.frame()
  output
}

#****************************************************************************
# TRGI (Trauma Related Guilt Inventory) # TODO verify subscales
#************************************************************************
trgi.scoring <- function(data,keep.vars,trgi){
        keep.vars <- c('ID','date')
        if('time.point' %in% colnames(data))
        {
                keep.vars <-c(keep.vars,'time.point')
        }
        if('sub' %in% colnames(data))
        {
                keep.vars <- c('sub',keep.vars)
        }
        
        if (missing(trgi)){
                trgi <- c(paste('TRGI_',1:32,sep=""))
        }
        
        raw.dat <- data[,trgi]
        
        # reverse score
        vars.rev <- c(paste('TRGI_', c(4,8,12,18,22,25,32), sep="")) # 17 reversed after guilt cognitions scored
        data[,vars.rev] <- 4-data[,vars.rev]
        
        vars.globalGuilt <- c(paste('TRGI_',c(13,18,22,25),sep=""))
        vars.distress <- c(paste('TRGI_',c(2,6,10,15,20,24),sep = ""))
        vars.guiltCognitions <- c(paste('TRGI_',c(1,4,5,7,8,9,11,12,14,16,17,19,21,23,26,27,28,29,30,31,32),sep=""))
        vars.hindsightBiasResponsibility <- c(paste('TRGI_',c(1,5,9,14,19,23,26),sep=""))
        vars.wrongDoing <- c(paste('TRGI_',c(3,7,11,16,21),sep=""))
        vars.lackOfJustification <- c(paste('TRGI_',c(4,8,12,17),sep=""))
        
        trgi.globalGuilt <- rowSums(data[,vars.globalGuilt])/length(vars.globalGuilt)
        trgi.distress <- rowSums(data[,vars.distress])/length(vars.distress)
        trgi.guiltCognitions <- rowSums(data[,vars.guiltCognitions])/length(vars.guiltCognitions)
        
        data[,'TRGI_17'] <- 4-data[,'TRGI_17'] # 17 is not reverse scored in guilt cognitions but is for lack of just.
        
        trgi.hindsightBiasResponsibility <- rowSums(data[,vars.hindsightBiasResponsibility])/length(vars.hindsightBiasResponsibility)
        trgi.wrongDoing <- rowSums(data[,vars.wrongDoing])/length(vars.wrongDoing)
        trgi.lackOfJustification <- rowSums(data[,vars.lackOfJustification])/length(vars.lackOfJustification)
        
        keeps <- data[,keep.vars]
        
        output <- cbind(keeps,raw.dat, trgi.globalGuilt, trgi.distress, trgi.guiltCognitions, trgi.hindsightBiasResponsibility, trgi.wrongDoing, trgi.lackOfJustification)
        output <- as.data.frame(output)
        return(output)
}

#****************************************************************************
# TFS (Trait Forgiveness Scale)
tfs.scoring <- function(data,keep.vars,tfs){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }

  if (missing(tfs)){
    tfs <- c(paste('TFS_',1:10,sep=""))
  }
  raw.dat <- data[,tfs]

  revtfs <- c(paste('TFS_',c(1,3,6,7,8),sep=""))
  data[,revtfs] = 6-data[,revtfs]

  vars.tfs <- c(paste('TFS_',1:10,sep=""))

  tfs.total <- rowSums(data[,vars.tfs])

  keeps <- data[,keep.vars]
  
  output <- cbind(keeps,raw.dat,tfs.total)
  output <- as.data.frame(output)
  return(output)
}

###################################################################################################
# Toronto Alexithymia Scale (20-items)
###################################################################################################
alexithymia.scoring <- function(data, keep.vars, alexithymia) {
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }

  if (missing(alexithymia)) {
    alexithymia <- c(paste('TorontoAlexithymia_',1:20,sep=""))
  }
  raw.dat <- data[,alexithymia]

  # reverse scoring
  vars.rev <- c(paste('TorontoAlexithymia_', c(4, 5, 10, 18, 19), sep=""))
  data[,vars.rev] <- 6 - data[,vars.rev]

  # subscales
  vars.AlexithymiaDif <- c(paste('TorontoAlexithymia_', c(1, 3, 6, 7, 9, 13, 14), sep=""))
  vars.AlexithymiaDdf <- c(paste('TorontoAlexithymia_', c(2, 4, 11, 12, 17), sep=""))
  vars.AlexithymiaEot <- c(paste('TorontoAlexithymia_', c(5, 8, 10, 15, 16, 18, 19, 20), sep=""))

  # scoring
  torontoAlexithymia.difficultyIdentifyFeelings <- rowSums(data[,vars.AlexithymiaDif])
  torontoAlexithymia.difficultyDescribeFeelings <- rowSums(data[,vars.AlexithymiaDdf])
  torontoAlexithymia.externalOrientedThinking <- rowSums(data[,vars.AlexithymiaEot])
  torontoAlexithymia.total <- rowSums(data[,alexithymia])

  keeps <- data[,keep.vars]
  
  output <- cbind(keeps, raw.dat, torontoAlexithymia.difficultyIdentifyFeelings, 
                  torontoAlexithymia.difficultyDescribeFeelings, 
                  torontoAlexithymia.externalOrientedThinking,
                  torontoAlexithymia.total) %>%
    as.data.frame()
  output

}

#****************************************************************************
# WAI-SR (Working Alliance Inventory Self-Report)
wai.scoring <- function(data,keep.vars,wai){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }

  if (missing(wai)){
    if("WAI-SR_1" %in% colnames(data)) {
      waiprefix <- "WAI-SR_"
    } else {
      waiprefix <- "WAI_"
    }
    if('WAI_Guide1' %in% colnames(data)) {
      wai <- c('WAI_Guide1','WAI_Guide2',c(paste(waiprefix,1:12,sep="")))
    } else {
      wai <- c(paste(waiprefix,1:12,sep=""))
    }
  }
  raw.dat <- data[,wai]

  vars.waiGoal <- c(paste(waiprefix,c(4,6,8,11),sep=""))
  vars.waiTask <- c(paste(waiprefix,c(1,2,10,12),sep=""))
  vars.waiBond <- c(paste(waiprefix,c(3,5,7,9),sep=""))

  wai.goal <- rowSums(data[,vars.waiGoal])
  wai.task <- rowSums(data[,vars.waiTask])
  wai.bond <- rowSums(data[,vars.waiBond])

  keeps <- data[,keep.vars]
 

  output <- cbind(keeps,raw.dat,wai.goal,wai.task,wai.bond)
  output <- as.data.frame(output)
  return(output)

}

#************************************************************************
# Watts Connectedness Scale (WCS)
#************************************************************************
wcs.scoring <- function(data,keep.vars,wcs){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
  if (missing(wcs)){
    wcs <- paste0('WCS_', 1:19)
  }
  
  raw.dat <- data[,wcs]
  
  vars.wcsConnectSelf <- paste0('WCS_', 1:6)
  vars.wcsConnectOthers <- paste0('WCS_', 7:12)
  vars.wcsConnectWorld <- paste0('WCS_', 13:19)
  
  wcs.connectednessSelf <- rowSums(data[,vars.wcsConnectSelf])/length(vars.wcsConnectSelf)
  wcs.connectednessOthers <- rowSums(data[,vars.wcsConnectOthers])/length(vars.wcsConnectOthers)
  wcs.connectednessWorld <- rowSums(data[,vars.wcsConnectWorld])/length(vars.wcsConnectWorld)
  
  wcs.total <- rowSums(data[,wcs])/length(wcs)
  
  keeps <- data[,keep.vars]
  
  
  output <- cbind(keeps,raw.dat,wcs.connectednessSelf, wcs.connectednessOthers, wcs.connectednessWorld)
  output <- as.data.frame(output)
  return(output)
  
}

#************************************************************************
# The World Health Organization Quality of Life (WHOQOLbref)
#************************************************************************
whoqolbref.scoring <- function(data,keep.vars,whoqolbref){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
  if (missing(whoqolbref)){
    whoqolbref <- paste0('WHOQOLbref_', 1:26)
  }
  
  raw.dat <- data[,whoqolbref]
  
  # reverse scoring
  vars.rev <- c(paste('WHOQOLbref_', c(3, 4, 26), sep=""))
  data[,vars.rev] <- 6 - data[,vars.rev]
  
  # Function to calculate domain score with missing value handling
  calculate_domain <- function(domain_vars, max_missing) {
          # check how many items are missing from a specific domain 
          missing_count <- rowSums(is.na(data[, domain_vars]))
          
          # If more than allowed missing, return NA for the domain score
          score <- ifelse(missing_count > max_missing, NA, 
                          # na.rm = T because we still calculate domain score and ignore missing values
                          rowSums(data[, domain_vars], na.rm = TRUE) / (length(domain_vars) - missing_count))
          
          return(score)
  }
  
  # Check if more than 20% of the assessment data is missing, discard assessment
  total_missing_percentage <- rowSums(is.na(data[, whoqolbref])) / length(whoqolbref)
  discard <- total_missing_percentage > 0.2
  
  vars.whoqolbrefPhysicalHealth <- c(paste('WHOQOLbref_',c(3,4,10,15,16,17,18),sep = ""))
  vars.whoqolbrefPsychological <- c(paste('WHOQOLbref_',c(5,6,7,11,19,26), sep = ""))
  vars.whoqolbrefSocialRelationships <- c(paste('WHOQOLbref_',c(20,21,22), sep = ""))
  vars.whoqolbrefEnvironment <- c(paste('WHOQOLbref_',c(8,9,12,13,14,23,24,25), sep = ""))
  
  # Scoring each domain with missing data rules
  whoqolbref.PhysicalHealth <- ifelse(discard, NA, calculate_domain(vars.whoqolbrefPhysicalHealth, 2))
  whoqolbref.Psychological <- ifelse(discard, NA, calculate_domain(vars.whoqolbrefPsychological, 2))
  
  # Special rule: Domain 3 should only be calculated if 1 or fewer items are missing
  whoqolbref.SocialRelationships <- ifelse(discard, NA, calculate_domain(vars.whoqolbrefSocialRelationships, 1))
  
  whoqolbref.Environment <- ifelse(discard, NA, calculate_domain(vars.whoqolbrefEnvironment, 2))
  
  # Calculating total score
  whoqolbref.total <- rowSums(data[, whoqolbref], na.rm = TRUE) / (length(whoqolbref) - rowSums(is.na(data[, whoqolbref])))
  whoqolbref.total <- ifelse(discard, NA, whoqolbref.total)
  
  keeps <- data[, keep.vars]
  
  # Final output
  output <- cbind(keeps,raw.dat,whoqolbref.PhysicalHealth, whoqolbref.Psychological, whoqolbref.SocialRelationships, whoqolbref.Environment)
  output <- as.data.frame(output)
  return(output)
}

#************************************************************************
# Wisconsin Smoking Withdrawal Scale (WSWS)
#************************************************************************
wsws.scoring <- function(data, keep.vars, wsws) {
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <- c(keep.vars, 'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub', keep.vars)
  }
  if (missing(wsws)) {
    wsws <- c(paste('WSWS_', 1:28, sep = ""))
  }
  
  raw.dat <- data[,wsws]
  
  rev.items <- c(paste("WSWS_", c(1,2,4,7,10,17,22,24), sep = ""))
  data[,rev.items] <- 4-data[,rev.items]
  
  vars.anger <- c(paste("WSWS_", c(13,15,18), sep = ""))
  vars.anxiety <- c(paste("WSWS_", c(3,6,8,10), sep = ""))
  vars.concentration <- c(paste("WSWS_", c(4,23,27), sep = ""))
  vars.craving <- c(paste("WSWS_", c(9,11,20,26), sep = ""))
  vars.hunger <- c(paste("WSWS_", c(1,14,16,21,28), sep = ""))
  vars.sadness <- c(paste("WSWS_", c(7,12,19,24), sep = ""))
  vars.sleep <- c(paste("WSWS_", c(2,5,17,22,25), sep = ""))
  
  wsws.anger <- rowSums(data[,vars.anger])
  wsws.anxiety <- rowSums(data[,vars.anxiety])
  wsws.concentration <- rowSums(data[,vars.concentration])
  wsws.craving <- rowSums(data[,vars.craving])
  wsws.hunger <- rowSums(data[,vars.hunger])
  wsws.sadness <- rowSums(data[,vars.sadness])
  wsws.sleep <- rowSums(data[,vars.sleep])
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps,raw.dat,wsws.anger,wsws.anxiety,wsws.concentration,
                  wsws.craving,wsws.hunger,wsws.sadness,wsws.sleep)
  output <- as.data.frame(output)
  return(output)
}

#************************************************************************
# Zimbardo Time Perspective Inventory (ZTPI)
#************************************************************************
ztpi.scoring <- function(data, keep.vars, ztpi) {
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <- c(keep.vars, 'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub', keep.vars)
  }
  if (missing(ztpi)) {
    ztpi <- c(paste('ZTPI_', 1:56, sep = ""))
  }
  
  raw.dat <- data[,ztpi]
  
  rev.items <- c(paste("ZTPI_", c(9,24,25,41,56), sep = ""))
  data[,rev.items] <- 6-data[,rev.items]
  
  vars.past.negative <- c(paste("ZTPI_", c(4,5,16,22,27,33,34,36,50,54), sep = ""))
  vars.present.hedonistic <- c(paste("ZTPI_", c(1,8,12,17,19,23,26,28,31,32,42,44,46,48,55), sep = ""))
  vars.future <- c(paste("ZTPI_", c(6,9,10,13,18,21,24,30,40,43,45,51,56), sep = ""))
  vars.past.positive <- c(paste("ZTPI_", c(2,7,11,15,20,25,29,41,49), sep = ""))
  vars.present.fatalistic <- c(paste("ZTPI_", c(3,14,35,37,38,39,47,52,53), sep = ""))
  
  ztpi.pastNegative <- rowSums(data[,vars.past.negative])/length(vars.past.negative)
  ztpi.presentHedonistic <- rowSums(data[,vars.present.hedonistic])/length(vars.present.hedonistic)
  ztpi.future <- rowSums(data[,vars.future])/length(vars.future)
  ztpi.pastPositive <- rowSums(data[,vars.past.positive])/length(vars.past.positive)
  ztpi.presentFatalistic <- rowSums(data[,vars.present.fatalistic])/length(vars.present.fatalistic)
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps,raw.dat,ztpi.pastNegative,ztpi.presentHedonistic,
                  ztpi.future,ztpi.pastPositive,ztpi.presentFatalistic)
  output <- as.data.frame(output)
  return(output)
}

################################################################################
## 0502 Specific Scoring Functions
################################################################################


#****************************************************************************
# Persisting Effects: Attitudes about Life
peqlife0502.scoring <- function(data,keep.vars,peqlife){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
  peqlife.scores.comb <- c()
  raw.colnames <- colnames(data)
  if (missing(peqlife)){
    peqlife <- paste0('PEQ_AttitudeLife_', 1:26)
  }
  
  vars.peqlife.pos <- c(paste('PEQ_AttitudeLife_',c(1,3,6,8,10,11,13,15,18,19,21,24,25),sep=""))
  vars.peqlife.neg <- c(paste('PEQ_AttitudeLife_',c(2,4,5,7,9,12,14,16,17,20,22,23,26),sep=""))
  
  peq.attitudelife.pos <- rowSums(data[,vars.peqlife.pos])/(5*length(vars.peqlife.pos))
  peq.attitudelife.neg <- rowSums(data[,vars.peqlife.neg])/(5*length(vars.peqlife.neg))
  peqlife.scores.comb <- cbind(peq.attitudelife.pos, peq.attitudelife.neg)
  
  
  keeps <- data[,keep.vars]
  raw.dat <- data[,peqlife]
  
  output <- cbind(keeps,raw.dat,peqlife.scores.comb)
  output <- as.data.frame(output)
  return(output)
  
}



#****************************************************************************
# Persisting Effects: Attitudes about Self
peqself0502.scoring <- function(data,keep.vars,peqself){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
  peq.attitudeself.pos <- c()
  peq.attitudeself.neg <- c()
  raw.colnames <- colnames(data)
  if (missing(peqself)){
    peqself <- paste0('PEQ_AttitudeSelf_', 1:22)
  }
  
  vars.peqself.pos <- c(paste('PEQ_AttitudeSelf_',c(1,4,6,7,9,12,14,15,17,20,21),sep=""))
  vars.peqself.neg <- c(paste('PEQ_AttitudeSelf_',c(2,3,5,8,10,11,13,16,18,19,22),sep=""))
  
  peq.attitudeself.pos <- rowSums(data[,vars.peqself.pos])/(5*length(vars.peqself.pos))
  peq.attitudeself.neg <- rowSums(data[,vars.peqself.neg])/(5*length(vars.peqself.neg))
  
  
  keeps <- data[,keep.vars]
  raw.dat <- data[,peqself]
  output <- cbind(keeps,raw.dat,peq.attitudeself.pos,peq.attitudeself.neg)
  output <- as.data.frame(output)
  return(output)
  
}
#****************************************************************************
# Persisting Effects: Mood Changes
peqmood0502.scoring <- function(data,keep.vars,peqmood){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
  if (missing(peqmood)){
    peqmood <- paste0('PEQ_MoodChanges_', 1:18)
  }
  
  vars.peqmood.pos <- c(paste('PEQ_MoodChanges_',c(1,3,5,8,10,11,14,16,17),sep=""))
  vars.peqmood.neg <- c(paste('PEQ_MoodChanges_',c(2,4,6,7,9,12,13,15,18),sep=""))
  
  peq.moodchange.pos <- rowSums(data[,vars.peqmood.pos])/(5*length(vars.peqmood.pos))
  peq.moodchange.neg <- rowSums(data[,vars.peqmood.neg])/(5*length(vars.peqmood.neg))
  
  keeps <- data[,keep.vars]
  raw.dat <- data[,peqmood]
  output <- cbind(keeps,raw.dat,peq.moodchange.pos,peq.moodchange.neg)
  output <- as.data.frame(output)
  return(output)
  
}
#****************************************************************************
# Persisting Effects: Relationships
peqrelate0502.scoring <- function(data,keep.vars,peqrelate){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
  
  if (missing(peqrelate)){
    peqrelate <- paste0('PEQ_Relationships_', 1:18)
  }
  
  vars.peqrelate.pos <- c(paste('PEQ_Relationships_',c(1,4,5,7,10,12,13,16,18),sep=""))
  vars.peqrelate.neg <- c(paste('PEQ_Relationships_',c(2,3,6,8,9,11,14,15,17),sep=""))
  
  peq.relationships.pos <- rowSums(data[,vars.peqrelate.pos])/(5*length(vars.peqrelate.pos))
  peq.relationships.neg <- rowSums(data[,vars.peqrelate.neg])/(5*length(vars.peqrelate.neg))
  
  
  keeps <- data[,keep.vars]
  raw.dat <- data[,peqrelate]
  output <- cbind(keeps,raw.dat,peq.relationships.pos,peq.relationships.neg)
  output <- as.data.frame(output)
  return(output)
  
}
#****************************************************************************
# Persisting Effects: Behavior Change
peqbehavior0502.scoring <- function(data,keep.vars,peqbehavior){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
  if (missing(peqbehavior)){
    peqbehavior <- c('PEQ_BehaviorChange_1','PEQ_BehaviorChange_2')
  }
  
  
  
  vars.peqbehavior.neg <- c(paste('PEQ_BehaviorChange_',c(1),sep=""))
  vars.peqbehavior.pos <- c(paste('PEQ_BehaviorChange_',c(2),sep=""))
  
  peq.behaviorchange.neg <- data[,vars.peqbehavior.neg]/(5*length(vars.peqbehavior.neg))
  peq.behaviorchange.pos <- data[,vars.peqbehavior.pos]/(5*length(vars.peqbehavior.pos))
  
  keeps <- data[,keep.vars]
  raw.dat <- data[,peqbehavior]
  output <- cbind(peq.behaviorchange.pos, peq.behaviorchange.neg)
  
  if ('PEQ_BehaviorChange_1' %in% colnames(output)) {
    output <- rename(output, peq.behaviorchange.pos='PEQ_BehaviorChange_2')
    output <- rename(output, peq.behaviorchange.neg='PEQ_BehaviorChange_1')
  }
  output <- cbind(keeps,raw.dat,output)
  output <- as.data.frame(output)
  return(output)
  
}
#****************************************************************************
# Persisting Effects: Spirituality 
peqspirit0502.scoring <- function(data,keep.vars,peqspirit){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
  
  peqspirit.scores.comb <- c()
  raw.colnames <- colnames(data)
  
  if (missing(peqspirit)){
    peqspirit <- paste0('PEQ_Spirituality_', 1:43)
  }
  
  
  vars.peqspirit.pos <- c(paste('PEQ_Spirituality_',c(1,4,6,7,10,12,13,16,18,19,21,23,25,28,30,31,34,35,
                                                      37,40,41,43),sep=""))
  vars.peqspirit.neg <- c(paste('PEQ_Spirituality_',c(2,3,5,8,9,11,14,15,17,20,22,24,26,27,29,32,33,36,
                                                      38,39,42),sep=""))
  #z <- duplicated(c(vars.peqspirit.neg,vars.peqspirit.pos)); length(z[z==TRUE]) #checks for any duplicate variables
  
  peq.spirituality.pos <- rowSums(data[,vars.peqspirit.pos])/(5*length(vars.peqspirit.pos))
  peq.spirituality.neg <- rowSums(data[,vars.peqspirit.neg])/(5*length(vars.peqspirit.neg))
  peqspirit.scores.comb <- cbind(peq.spirituality.pos, peq.spirituality.neg)
  
  
  keeps <- data[,keep.vars]
  raw.dat <- data[,peqspirit]
  output <- cbind(keeps,raw.dat,peqspirit.scores.comb)
  output <- as.data.frame(output)
  return(output)
  
}
#****************************************************************************
# Persisting Effects: Miscellaneous 
peqmisc0502.scoring <- function(data,keep.vars,peqmisc){

  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
  raw.colnames <- colnames(data)
  peqmisc.comb <- c()
  
  if (missing(peqmisc)){
    if("PEQ_PersMeaning" %in% colnames(data) & "PEQ_HowUnusual" %in% colnames(data)) {
      peqmisc <- c(paste0('PEQ_Misc_', 3:13), 
                        "PEQ_PersMeaning", "PEQ_SpiritSignif", "PEQ_HowUnusual", 
                        "PEQ_ChangeWellBeing") 
    } else {
      peqmisc <- paste0('PEQ_Misc_', 3:13)
    }
  }
  
  vars.peqmisc.pos <- c(paste('PEQ_Misc_',c(4,6,7,9:13),sep=""))
  vars.peqmisc.neg <- c(paste('PEQ_Misc_',c(3,5,8),sep=""))
  
  peq.misc.pos <- rowSums(data[,vars.peqmisc.pos])/(5*length(vars.peqmisc.pos))
  peq.misc.neg <- rowSums(data[,vars.peqmisc.neg])/(5*length(vars.peqmisc.neg))
  peqmisc.comb <- cbind(peq.misc.pos, peq.misc.neg)
  
  
  keeps <- data[,keep.vars]
  raw.dat <- data[,peqmisc]
  output <- cbind(keeps,raw.dat,peqmisc.comb)
  output <- as.data.frame(output)
  return(output)
  
}

################################################################################
## 0706 Specific Scoring Functions
################################################################################

#****************************************************************************
# BPI (Brief Pain Inventory) (0706 specific)
bpi0706.scoring <- function(data, keep.vars, bpi){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
  if (missing(bpi)){
    bpi <- c(paste0('BPI_', 1:8), paste0('BPI_9',letters[1:7]))
    
  }
  raw.dat <- data[,bpi]
  vars.bpiSeverity <- paste('BPI_',3:6,sep="")
  vars.bpiInterference <- paste('BPI_9',letters[1:7],sep="")
  
  bpi.severity <- rowSums(data[,vars.bpiSeverity])/length(vars.bpiSeverity)
  bpi.interference <- rowSums(data[,vars.bpiInterference])/length(vars.bpiInterference)
  
  keeps <- data[,keep.vars]
  
  
  output <- cbind(keeps,raw.dat, bpi.severity, bpi.interference)
  output <- as.data.frame(output)
  return(output)
}


#************************************************************************
# McGill Quality of Life Questionnaire (McGill QoL) # Modified for 0706 column names (Does not include 2b,3b,4b, or TEXT columns which are currently in Qualtrics)
#************************************************************************
mcgillqol0706.scoring <- function(data, keep.vars, mcgill.qol) {
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <- c(keep.vars, 'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub', keep.vars)
  }
  if (missing(mcgill.qol)) {
    mcgill.qol <- c('MQOL_PartA', paste0('MQOL_PartB_', 1:4) ,paste0('MQOL_PartC_', 5:16), 'MQOL_PartD_Essay')
  }
  
  raw.dat <- data[,mcgill.qol]
  
  vars.rev <- c(paste0('MQOL_PartB_', 1:3), paste0('MQOL_PartC_', 5:8))
  data[,vars.rev] <- 10-data[,vars.rev]
  
  vars.physicalSymptoms <- paste0("MQOL_PartB_", 1:3)
  vars.physicalWellBeing <- c('MQOL_PartB_4')
  vars.psychological <- c(paste("MQOL_PartC_", 5:8, sep = ""))
  vars.existential <- paste0("MQOL_PartC_", 9:14)
  vars.support <- paste0("MQOL_PartC_", 15:16)
  
  mqol.physicalSymptoms <- rowSums(data[,vars.physicalSymptoms])/length(vars.physicalSymptoms)
  mqol.physicalWellBeing <- data[,vars.physicalWellBeing]
  mqol.psychological <- rowSums(data[,vars.psychological])/length(vars.psychological)
  mqol.existential <- rowSums(data[,vars.existential])/length(vars.existential)
  mqol.support <- rowSums(data[,vars.support])/length(vars.support)
  
  mqol.total <- (mqol.physicalSymptoms + mqol.physicalWellBeing + mqol.psychological + mqol.existential + mqol.support)/5
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps, raw.dat, mqol.physicalSymptoms, mqol.physicalWellBeing, mqol.psychological, mqol.existential, mqol.support, mqol.total) %>%
    as.data.frame() 
  return(output)
}

#### PEQ Modified Functions ###
###### modified column names because 0706 does not include the _a columns ###

peqlife0706.scoring <- function(data,keep.vars,peqlife){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
  if (missing(peqlife)){
    peqlife <- paste0('PEQ_AttitudeLife_', 1:26)
  }
  
  raw.dat <- data[,peqlife]
  
  vars.peqlife.pos <- c(paste('PEQ_AttitudeLife_',c(1,3,6,8,10,11,13,15,18,19,21,24,25),sep=""))
  vars.peqlife.neg <- c(paste('PEQ_AttitudeLife_',c(2,4,5,7,9,12,14,16,17,20,22,23,26),sep=""))
  
  peq.attitudelife.pos <- rowSums(data[,vars.peqlife.pos])/(5*length(vars.peqlife.pos)) # 351 is missing 1 item at the 6 month post high dose timepoint, previously included na.rm = T
  peq.attitudelife.neg <- rowSums(data[,vars.peqlife.neg])/(5*length(vars.peqlife.neg))
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps,raw.dat,peq.attitudelife.pos, peq.attitudelife.neg)
  output <- as.data.frame(output)
  return(output)
  
}

#****************************************************************************
# Persisting Effects: Attitudes about Self
peqself0706.scoring <- function(data,keep.vars,peqself){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
  if (missing(peqself)){
    peqself <- paste0('PEQ_AttitudeSelf_', 1:22)
  }
  
  raw.dat <- data[,peqself]
  
  vars.peqself.pos <- c(paste('PEQ_AttitudeSelf_',c(1,4,6,7,9,12,14,15,17,20,21),sep=""))
  vars.peqself.neg <- c(paste('PEQ_AttitudeSelf_',c(2,3,5,8,10,11,13,16,18,19,22),sep=""))
  
  peq.attitudeself.pos <- rowSums(data[,vars.peqself.pos])/(5*length(vars.peqself.pos)) # 337 is missing item 15 item at the 1 month post high dose, previously using na.rm = T
  peq.attitudeself.neg <- rowSums(data[,vars.peqself.neg])/(5*length(vars.peqself.neg))
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps,raw.dat,peq.attitudeself.pos,peq.attitudeself.neg)
  output <- as.data.frame(output)
  return(output)
}

#****************************************************************************
# Persisting Effects: Mood Changes
peqmood0706.scoring <- function(data,keep.vars,peqmood){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
  if (missing(peqmood)){
    peqmood <- paste0('PEQ_MoodChanges_', 1:18)
  }
  
  raw.dat <- data[,peqmood]
  
  vars.peqmood.pos <- c(paste('PEQ_MoodChanges_',c(1,3,5,8,10,11,14,16,17),sep=""))
  vars.peqmood.neg <- c(paste('PEQ_MoodChanges_',c(2,4,6,7,9,12,13,15,18),sep=""))
  
  peq.moodchange.pos <- rowSums(data[,vars.peqmood.pos])/(5*length(vars.peqmood.pos))
  peq.moodchange.neg <- rowSums(data[,vars.peqmood.neg])/(5*length(vars.peqmood.neg))
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps,raw.dat,peq.moodchange.pos,peq.moodchange.neg)
  output <- as.data.frame(output)
  return(output)
}

#****************************************************************************
# Persisting Effects: Relationships
peqrelate0706.scoring <- function(data,keep.vars,peqrelate){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
  if (missing(peqrelate)){
    peqrelate <- paste0('PEQ_Relationships_', 1:18)
  }
  
  raw.dat <- data[,peqrelate]
  
  vars.peqrelate.pos <- c(paste('PEQ_Relationships_',c(1,4,5,7,10,12,13,16,18),sep=""))
  vars.peqrelate.neg <- c(paste('PEQ_Relationships_',c(2,3,6,8,9,11,14,15,17),sep=""))
  
  peq.relationships.pos <- rowSums(data[,vars.peqrelate.pos])/(5*length(vars.peqrelate.pos))
  peq.relationships.neg <- rowSums(data[,vars.peqrelate.neg])/(5*length(vars.peqrelate.neg))
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps,raw.dat,peq.relationships.pos,peq.relationships.neg)
  output <- as.data.frame(output)
  return(output)
}

#****************************************************************************
# Persisting Effects: Behavior Change
peqbehavior0706.scoring <- function(data,keep.vars,peqbehavior){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
  if (missing(peqbehavior)){
    peqbehavior <- c('PEQ_BehaviorChange_1','PEQ_BehaviorChange_2')
  }
  
  raw.dat <- data[,peqbehavior]
  
  vars.peqbehavior.neg <- c(paste('PEQ_BehaviorChange_',c(1),sep=""))
  vars.peqbehavior.pos <- c(paste('PEQ_BehaviorChange_',c(2),sep=""))
  
  peq.behaviorchange.pos <- data[,vars.peqbehavior.pos]/(5*length(vars.peqbehavior.pos))
  peq.behaviorchange.neg <- data[,vars.peqbehavior.neg]/(5*length(vars.peqbehavior.neg))
  
  keeps <- data[,keep.vars]
  output <- cbind(peq.behaviorchange.pos, peq.behaviorchange.neg)
  
  if ('PEQ_BehaviorChange_1' %in% colnames(output)) {
    output <- rename(output, peq.behaviorchange.pos='PEQ_BehaviorChange_2')
    output <- rename(output, peq.behaviorchange.neg='PEQ_BehaviorChange_1')
  }
  
  output <- cbind(keeps,raw.dat,output)
  output <- as.data.frame(output)
  return(output)
}

peqspirit0706.scoring <- function(data,keep.vars,peqspirit){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  # if("dose" %in% colnames(data))
  # {
  #   keep.vars <-c('sub','ID','date', 'dose')
  # }
  
  if(missing(peqspirit)) {
    peqspirit <- paste0('PEQ_Spirituality_', 1:43)
  }
  
  raw.dat <- data[,peqspirit]
  
  vars.peqspirit.pos <- c(paste('PEQ_Spirituality_',c(1,4,6,7,10,12,13,16,18,19,21,23,25,28,30,31,34,35,
                                                      37,40,41,43),sep=""))
  vars.peqspirit.neg <- c(paste('PEQ_Spirituality_',c(2,3,5,8,9,11,14,15,17,20,22,24,26,27,29,32,33,36,
                                                      38,39,42),sep=""))
  
  peq.spirituality.pos <- rowSums(data[,vars.peqspirit.pos])/(5*length(vars.peqspirit.pos))
  peq.spirituality.neg <- rowSums(data[,vars.peqspirit.neg])/(5*length(vars.peqspirit.neg))
  peqspirit.scores.comb <- cbind(peq.spirituality.pos, peq.spirituality.neg)
  
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps,raw.dat,peqspirit.scores.comb)
  output <- as.data.frame(output)
  return(output)
}

#****************************************************************************
# Persisting Effects: Miscellaneous 
peqmisc0706.scoring <- function(data,keep.vars,peqmisc){
  keep.vars <- c('ID','date')
  if('time.point' %in% colnames(data))
  {
    keep.vars <-c(keep.vars,'time.point')
  }
  if('sub' %in% colnames(data))
  {
    keep.vars <- c('sub',keep.vars)
  }
  
  if (missing(peqmisc)){
    peqmisc <- c(paste0('PEQ_Misc_', 1:11), "PEQ_PersMeaning_Top10", "PEQ_PersMeaning_Top5", "PEQ_SpiritSignif_Top5", "PEQ_ChangeWellBeing_AnyIncrease", "PEQ_PersMeaning_SingleMost",
                 "PEQ_SpiritSignif_SingleMost", "PEQ_ChangeWellbeing_IncModerateOrVeryMuch", "PEQ_ChangeWellBeing_IncVeryMuch", "PEQ_ChangeWellBeing_AnyDecrease")
  }
  raw.dat <- data[,peqmisc]
  
  vars.peqmisc.pos <- c(paste('PEQ_Misc_',c(2,4,5,7:11),sep=""))
  vars.peqmisc.neg <- c(paste('PEQ_Misc_',c(1,3,6),sep=""))
  
  peqmisc.pos <- rowSums(data[,vars.peqmisc.pos])/(5*length(vars.peqmisc.pos))
  peqmisc.neg <- rowSums(data[,vars.peqmisc.neg])/(5*length(vars.peqmisc.neg))
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps,raw.dat,peqmisc.pos, peqmisc.neg)
  output <- as.data.frame(output)
  return(output)
}

