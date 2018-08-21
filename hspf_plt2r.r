plt2r <- function(fileName) {
  ## gets data from HSPF pltgen and returns a data fram with ...
  
  ## read pltgen output file
  
  ## open connection to file
  conn <- file(fileName,open="r")
   
  ## determine number of curves in tablulated columns
  ## curve number information is on line 3
  linn <-readLines(conn, n=3)
  # print(linn)
  noCurves <- as.numeric(substr(linn[3],72,74))
  
  ## now scan for line with curve headings (looking for "TRANCOD")
  while (grepl('TRANCOD',linn[1]) == FALSE){
    linn <- readLines(conn, n=1)
    # print(linn)
  }
  
  ## read curve labels
  labels <- NULL
  for (i in 1:noCurves){
    linn <- readLines(conn, n=1)
    # print(linn)
    labels <- rbind(labels,trimws(substr(linn,6,25),which="right"))
  }
  
  ## scan for time series initial condition line
  while (grepl('-1.0000000E+30',linn[1],fixed=TRUE) == FALSE){
    linn <- readLines(conn, n=1)
    # print(linn)
  }
  
  ## read tabulated data
  linn <-readLines(conn)
  close(conn)
  
  dat <- read.table(text = linn[1:length(linn)])
  
  ## create date vector from pltgen file year, month, day, hour, and second columns
  dat$date <- as.POSIXlt(apply( dat[ , c(2,3,4,5,6) ] , 1 , paste , collapse = " " ),format="%Y %m %d %H %S")
  
  ## select final data columns
  datecol <- 7 + noCurves
  endcol <- 6 + noCurves
  dat <- dat[c(datecol,7:endcol)]
  
  ## rename columns using labels in plt file
  ncolumns <- noCurves + 1
  for (i in 2:ncolumns){
    names(dat)[i] <- labels[i-1]
  }
  
  return(dat)
  
}