library (data.table)
#library(gdata)
#=======================================================================
# Function defines several Modflow Models characteristics
#=======================================================================
defineMFmodel <- function() {
  MFmodel = c('ECFTX', 'NPALM', 'LWCSIM', 'ECFM')
  res = c(1250, 704, 1000, 2400)
  xmin = c(24352.000, 680961.000, 218436.000, 565465.000)
  ymin = c(983097.000, 840454.000, 441788.000,-44448.000)
  nlays = c(11, 3, 9, 7)
  nrows = c(603, 292, 553, 552)
  ncols = c(740, 408, 512, 236)
  startYr = c(1999, 1965, 1999, 1989)
  freq = c('Month', 'Month', 'Month', 'Month')
  nsp = c(192, 14975, 192, 288)
  mpath = c(
    "\\\\whqhpc01p\\hpcc_shared\\dbandara\\CFWI\\ECFTX\\Model\\Transient\\*.*",
    "\\\\whqhpc01p\\hpcc_shared\\jgidding\\LECSR\\LOX18\\*.*",
    "\\\\ad.sfwmd.gov\\dfsroot\\data\\wsd\\MOD\\LWCSASIAS\\model\\*.*",
    "\\\\ad.sfwmd.gov\\dfsroot\\data\\wsd\\MOD\\ECFM\\MB\\*.*"
  )
  MFmodel.Params <-
    data.frame(MFmodel,
               res,
               xmin,
               ymin,
               nlays,
               nrows,
               ncols,
               nsp,
               startYr,
               freq,
               mpath)
  rownames(MFmodel.Params) <- MFmodel.Params$MFmodel
  return(MFmodel.Params)
}
#=================================================================
# Function Reads just the Header record from a Binary Heads file
#=================================================================
readHeadsHeader <- function(filPtr) {
  while (length(record <- readBin(filPtr, raw(), 44)) > 0)
  {
    ints <- readBin(record, integer(), 11)
    txt <- intToUtf8(record[16L + seq(16)])
    flts <- readBin(record,"double",n=11,size=4)
    
    KSTP <- ints[1]
    KPER <- ints[2]
    PERTIM <- flts[3]
    TOTTIM <- flts[4]
    NC <- ints[9]
    NR <- ints[10]
    K <- ints[11]
    header <- list(
      KSTP = KSTP,
      KPER = KPER,
      PERTIM = PERTIM,
      TOTTIM = TOTTIM,
      TEXT = txt,
      NC = NC,
      NR = NR,
      K = K
    )
    #    print (paste(header))
    return(header)
  }
  return(list())
}
#===============================================
# Function to search for a Heads by  Layer
# and returns a vector of values
# by Stress Periods identified in range of values
#===============================================
readHeadsbin <- function(filPtr, SP_rng) {
  bigVector <- vector('numeric')
  HeaderRead <- readHeadsHeader(filPtr)
  kntFloats <- HeaderRead$K * HeaderRead$NR * HeaderRead$NC
  Lay1floats <- HeaderRead$NR * HeaderRead$NC
  HeadBlock <- readBin(filPtr, double(), n = Lay1floats, size = 4)
  bigVector <- c(bigVector, HeadBlock[1:Lay1floats])
  i <- 1
  cat(paste("0%.."))
  
  repeat {
    HeaderRead <- readHeadsHeader(filPtr)
    # Don't read past EOF
    if (length(HeaderRead) > 0) {
      if (is.element(HeaderRead$TOTTIM, SP_rng) &&
          HeaderRead$TOTTIM <= max(SP_rng)) {
        i <- i + 1
        cat ('*')
        HeadBlock <-
          readBin(filPtr, double(), n = Lay1floats, size = 4)
        bigVector <- c(bigVector, HeadBlock[1:Lay1floats])
        if (HeaderRead$K == M$nlays){
          HeadsMatrix<- array(bigVector,c(M$ncols,M$nrows,M$nlays))
        }
      } else {
        seek(filPtr, (Lay1floats * 4), origin = 'current')
      }
    }
    # don't read everything unless necessary
    if (length(HeaderRead) == 0) {
      cat('\n')
      break
    }
    
    if (HeaderRead$TOTTIM > max(SP_rng)) {
      cat('\n')
      break
    }
    # Display % complete	  
    cat(paste('\r',format(as.numeric(HeaderRead$PERTIM) / max(SP_rng) * 100,digits = 2,nsmall = 2),"%"))
  }
  print (paste(HeaderRead$TOTTIM, SP_rng))
  return(HeadsMatrix)
}

#=================================================================
# Beginning of Script
#
# Created by Kevin A. Rodberg - February 2018
#
# Purpose: Create series of raster figures to review Modflow Binary data
#          and large ET or Recharge datasets/

#=================================================================
# Choose Modflow Binary Heads file
#=================================================================
MFmodel.Params <- defineMFmodel()

#=================================================================
#model <- chooseModel()
#!/usr/bin/env Rscript
#=================================================================

args = commandArgs(trailingOnly = TRUE)

# test if there is at least one argument: if not, return an error
if (length(args) == 0) {
  model <- "ECFM"
  headsFile <-
    "//ad.sfwmd.gov/dfsroot/data/wsd/MOD/ECFM/MB/2016B/ecfm_tr_vm.hds"
  startHeads <-  "//ad.sfwmd.gov/dfsroot/data/wsd/MOD/ECFM/MB/2016B/InitHeads"
} else if (length(args) < 2) {
  startHeads <- "//ad.sfwmd.gov/dfsroot/data/wsd/MOD/ECFM/MB/2016B/InitHeads"
}else {
  model = args[1]
  print(args[1])
  
  bpath = args[2]
  #Binary Heads file headsFile<-"fort.91"
  headsFile = args[3]
  print (args[3])
  
  initHeads = args[4]
  
  M <- as.data.frame(MFmodel.Params[model, ])
  mpath <- toString(MFmodel.Params[model, ]$mpath)
  
  #bpath <- gsub("[*].*$", "", mpath)
  headsFile <- sprintf(paste0(bpath, headsFile))
  startHeads <- sprintf(paste0(bpath, initHeads))
  
#  startHeads <-"//ad.sfwmd.gov/dfsroot/data/wsd/MOD/ECFM/MB/2016B/InitHeads"
}
print(model)
M <- as.data.frame(MFmodel.Params[model, ])
print(headsFile)
if (length(headsFile) == 0) {
  exit(paste("HeadsFile doesn't exist", headsFile))
}
to.read = file(headsFile, "rb")

#===============================================
# Estimate number of stress periods in Heads file
#===============================================
fileSz <- file.info(headsFile)$size

TtlStrPd = fileSz / ( M$nlays * ((M$ncols * M$nrows * 4) + 44))

TtlStrPd <- 365
print(TtlStrPd)

#===============================================
# Define range of Stress Periods to read
#===============================================
if (length(args)==3) {
  lastSP <- args[3]
}  else {lastSP <- TtlStrPd
}
print(lastSP)

if (lastSP > TtlStrPd || lastSP < 1) {
  print (args[3])
  print(TtlStrPd)
  stop('Out of Range')
}


#===============================================
# Retrieve Heads by Layer
#===============================================
to.read <- file(headsFile, "rb")
options(digits = 4) 
options(width=12)
options(format="e")
HeadsVector <- readHeadsbin(to.read, lastSP)
#HeadsMatrix<- array(HeadsVector,c(M$ncols,M$nrows,M$nlays))
for (lay in seq(1,M$nlays)){
  layx <- t(HeadsVector[,,lay])
  fname=sprintf(paste0(startHeads,lay))
  write.table(format(layx,digits=4, width=12), file=fname, 
              col.names=FALSE,row.names=FALSE,quote=FALSE)
}
close(to.read)

