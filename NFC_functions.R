# NFC (Need for Cognition)
nfc.scoring <- function(data,keep.vars,nfc) {
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

  nfc.prefix <- c() # differentiates between pre and post
  if("NfC_Pre_1" %in% raw.colnames) {
    nfc.prefix <- "NfC_Pre_"
  } else if ("NfC_Post_1" %in% raw.colnames){
    nfc.prefix <- "NfC_Post_"
  } else {
    stop("Column names for NFC not recognized. Use 'NfC_Pre_' or 'NfC_Post_'")
  }
  
  if (nfc.prefix =="NfC_Pre_"){
    nfc <- c(paste("NfC_Pre_",1:18,sep = ""))
  }else{
    nfc <- c(paste("NfC_Post_",1:18,sep = ""))
  }
  
  raw.dat <- data[,nfc]
  reverse_items <- c(3,4,5,7,8,9,12,16,17)
  raw.dat[, reverse_items] <- lapply(raw.dat[, reverse_items], function(x) 6 - as.numeric(x))
  nfc.total <- rowSums(raw.dat, na.rm = TRUE) 
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps,raw.dat,nfc.total)
  output <- as.data.frame(output)
  return(output)
}

####################################
# CFS (Cognitive Flexibility Scale)
cfs.scoring <- function(data,keep.vars,cfs) {
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
  
  cfs.prefix <- c() # differentiates between pre and post
  if("CFS_Pre_1" %in% raw.colnames) {
    cfs.prefix <- "CFS_Pre_"
  } else if ("CFS_Post_1" %in% raw.colnames){
    cfs.prefix <- "CFS_Post_"
  } else {
    stop("Column names for CFS not recognized. Use 'CFS_Pre_' or 'CFS_Post_'")
  }
  
  if (cfs.prefix =="CFS_Pre_"){
    cfs <- c(paste("CFS_Pre_",1:12,sep = ""))
  }else{
    cfs <- c(paste("CFS_Post_",1:12,sep = ""))
  }
  
  raw.dat <- data[,cfs]
  reverse_items <- c(2,3,5,10)
  raw.dat[, reverse_items] <- lapply(raw.dat[, reverse_items], function(x) 7 - as.numeric(x))
  cfs.total <- rowSums(raw.dat, na.rm = TRUE)
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps,raw.dat,cfs.total)
  output <- as.data.frame(output)
  return(output)
}


####################################
# WMQ (Working Memory Quesionaire)
wmq.scoring <- function(data,keep.vars,wmq) {
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
  
  wmq.prefix <- c() # differentiates between pre and post
  if("WMQ_Pre_1" %in% raw.colnames) {
    wmq.prefix <- "WMQ_Pre_"
  } else if ("WMQ_Post_1" %in% raw.colnames){
    wmq.prefix <- "WMQ_Post_"
  } else {
    stop("Column names for WMQ not recognized. Use 'WMQ_Pre_' or 'WMQ_Post_'")
  }
  
  if (wmq.prefix =="WMQ_Pre_"){
    wmq <- c(paste("WMQ_Pre_",1:30,sep = ""))
  }else{
    wmq <- c(paste("WMQ_Post_",1:30,sep = ""))
  }
  
  raw.dat <- data[,wmq]
  
  ### 3 dimensions
  vars.WMQStorage <- paste0(wmq.prefix,c(3,5,7,11,15,17,21,25,27,30))
  vars.WMQAttention <- paste0(wmq.prefix, c(1,4,8,10,13,16,19,22,24,28))
  vars.WMQExecutive <- paste0(wmq.prefix, c(2,6,9,12,14,18,20,23,26,29))

  wmq.storage <- rowSums(raw.dat[,vars.WMQStorage], na.rm=TRUE)
  wmq.attention <- rowSums(raw.dat[,vars.WMQAttention], na.rm=TRUE)
  wmq.executive <- rowSums(raw.dat[,vars.WMQExecutive], na.rm=TRUE)

  wmq.total <- rowSums(raw.dat, na.rm = TRUE)
  
  keeps <- data[,keep.vars]
  
  output <- cbind(keeps,raw.dat,wmq.total, wmq.storage, wmq.attention, wmq.executive)
  output <- as.data.frame(output)
  return(output)
}

################################
# DASS (Depression, Anxiety, Stress Scale)
dassprepost.scoring <- function(data,keep.vars,dass) {
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
  
  dass.prefix <- c() # differentiates between pre and post
  if("DASS_pre_1" %in% raw.colnames) {
    dass.prefix <- "DASS_pre_"
  } else if ("DASS_post_1" %in% raw.colnames){
    dass.prefix <- "DASS_post_"
  } else {
    stop("Column names for DASS not recognized. Use 'DASS_Pre_' or 'DASS_Post_'")
  }
  
  # Define total scale and all subscales
  vars.dassDep <- c(paste(dass.prefix,c(3,5,10,13,16,17,21),sep=""))
  vars.dassAnx <- c(paste(dass.prefix,c(2,4,7,9,15,19,20),sep=""))
  vars.dassStress <- c(paste(dass.prefix,c(1,6,8,11,12,14,18),sep=""))
  
  # Calculate score. Note subscale scores are doubled
    dass.depression <- rowSums(sapply(data[,vars.dassDep],as.numeric))*2
    dass.anxiety <- rowSums(sapply(data[,vars.dassAnx],as.numeric))*2
    dass.stress <- rowSums(sapply(data[,vars.dassStress],as.numeric))*2
 
  
  # get variables we want to pass through to the output
  # bring participant sub,id,date back into the data frame
  keeps <- data[,keep.vars]
  raw.dat <- data[, paste0(dass.prefix, 1:21)]
  # combine scores and kept variables, make dataframe
  output <- cbind(keeps,raw.dat,dass.depression,dass.anxiety,dass.stress)
  output <- as.data.frame(output)
  #names(output) <- c(keep.vars,'dass.total','dass.depression','dass.anxietyiety','dass.stress')
  
  # return
  return(output)
}

###################################
#Psychedelics_when barplot
plot_delta_when <- function(df, delta_var, title){
  df %>%
    group_by(Psychedelics_when) %>%
    summarise(
      mean_delta = mean(.data[[delta_var]], na.rm = TRUE),
      se = sd(.data[[delta_var]], na.rm = TRUE) / sqrt(n())
    ) %>%
    ggplot(aes(x = Psychedelics_when, y = mean_delta)) +
    geom_bar(stat = "identity") +
    geom_errorbar(aes(ymin = mean_delta - se, ymax = mean_delta + se), width = 0.2) +
    labs(title = title, x = "Time Since First Use", y = "Change (Post - Pre)") +
    theme_minimal()
}
