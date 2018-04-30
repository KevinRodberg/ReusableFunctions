library(tcltk2)

#===============================================
#  define ok and Cancel functions for tcl buttons
#  and stadardize some tcl vars
#===============================================
done <- tclVar(0)
fnOK <- function() {  tclvalue(done) <- 1}
fnCncl <- function() {  tclvalue(done) <- 2}
fontHeading <- tkfont.create(family = "Arial",size = 24,weight = "bold",slant = "italic")

#===============================================
# Function to exit a little more nicely
#===============================================
exit <- function(msg)
{
  cat(paste0("*** ERROR ***: ", msg))
  closeAllConnections()
  .Internal(.invokeRestart(list(NULL, NULL), NULL))
  options(warn=0)
  
}
promptUser4Text <- function(msg)  {
  winc <- tktoplevel()
  lbl.msg <- tk2label(winc, text = msg, font = fontHeading)
  tkgrid(lbl.msg, padx = 30)
  tkraise(winc)
  entryInit=""
  btn.OK <- tk2button(winc,text = "OK",width = -6,command = fnOK)
  btn.Cncl <- tk2button(winc,text = "Cancel",width = -6,command = fnCncl)
  responseVarTcl <- tclVar(paste(entryInit))
  textEntryWidget <- tk2entry(winc, width = 35, textvariable = responseVarTcl)
  tkgrid(tklabel(winc, text = "Range of values",font = fontHeading), 
         textEntryWidget, btn.OK,btn.Cncl,padx = 10, pady = 5)
  tkraise(winc)
  tkwait.variable(done)
  tkdestroy(winc)
  if (tclvalue(done) != 1) {
    exit("User canceled Data Entry")
  }
  promptResponse <-tclvalue(responseVarTcl)
  
  return(promptResponse)
}

#===============================================
# Function to Prompt for user to enter an integer
#===============================================
readinteger <- function() {
  n <- readline(prompt = "Enter an integer: ")
  return(as.integer(n))
}

#===============================================
# Function opens window to Accept a string 
# defining range of integers vals which are
# reformed as a unique sequence
#===============================================
readRange <- function() {
  winB <- tktoplevel()
  msg = paste("Total Number Stress Periods Available=", TtlStrPd, 
              "\n\nChoose Range or Periods of interest \n i.e.: 1:3,5,7:100,200 \n")
  
  lbl.msg <- tk2label(winB, text = msg, font = fontHeading)
  tkgrid(lbl.msg, padx = 30)
  tkraise(winB)
  entryInit=""
  btn.OK <- tk2button(winB,text = "OK",width = -6,command = fnOK)
  btn.Cncl <- tk2button(winB,text = "Cancel",width = -6,command = fnCncl)
  rangeVarTcl <- tclVar(paste(entryInit))
  textEntryWidget <- tk2entry(winB, width = 35, textvariable = rangeVarTcl)
  tkgrid(tklabel(winB, text = "Range of values",font = fontHeading), 
         textEntryWidget, btn.OK,btn.Cncl,padx = 10, pady = 5)
  tkbind(winB, "<Return>", fnOK)
  tkraise(winB)
  tkwait.variable(done)
  tkdestroy(winB)
  if (tclvalue(done) != 1) {
    exit("User canceled Model Selection")
  }
  #  Convert string of numeric vals to a range   
  rngStr <-tclvalue(rangeVarTcl)
  rngStr <- gsub(" ", ",", rngStr)
  rngStr <- gsub(",,", ",", rngStr)
  
  df <- as.vector(rngStr)
  rng <-
    sapply(df, function(x)
      dget(textConnection(paste('c(', x, ')'))))
  rng <- unique(sort(rng))
  return(rng)
}
