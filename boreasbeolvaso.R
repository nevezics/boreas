library (xts)

##Beolvasó függvény
process.boreas <- function(file.name,path="textmeasfull"){
  split.block <- function(block){
    rawstring <- unlist(strsplit(block,"/"))
    rawnumbers <- as.numeric(rawstring)
    raw.matrix <- matrix(rawnumbers,ncol=length(block))
    raw.matrix <- raw.matrix[-1,]
    as.data.frame(t(raw.matrix))
  }
  full.path <- paste(path,file.name,sep="/")
  raw.file <- read.table(full.path,sep=" ", colClasses="character")
  date.time <- as.POSIXct(strptime(raw.file[,2],"%Y%m%d%H%M%S"))
  ## Process temperature
  raw.temp <- raw.file[,3]
  temp <- split.block(raw.temp)
  names(temp) <- c("t","tdp","RH")
  ## Process wind
  raw.wind <- raw.file[,4]
  wind <- split.block(raw.wind)
  names(wind) <- c("umean","umax","udirmean","udirmin","udirmax",
                   "tumax","tumaxdir")
  ## Process precipitation
  raw.precip <- raw.file[,5]
  precip <- split.block(raw.precip)
  precip <- precip[,1:2]
  names(precip) <- c("P","imax")
  ## Process radiation
  raw.radiation <- raw.file[,6]
  radiation <- split.block(raw.radiation)
  names(radiation) <- c("SWIn","SWOut","LWIn","LWOut")
  data <- cbind(temp,wind,precip,radiation)
  xts(data,date.time)
}

##Adatok feldolgozása ttfiles mappából
ttpath <- "textmeasfull"
ttfiles <- dir(ttpath,"2021")

nyers <- list()

for(tti in 1:length(ttfiles)){
nyers[[tti]] <- process.boreas(ttfiles[tti], path=ttpath)
cat(tti)
}

teljes <- nyers[[1]]
for(tti in 2:length(nyers))
teljes <- c(teljes,nyers[[tti]])

##Meteorológiai adatok grafikonjai és Excel exportjai 2021

##Hőmérséklet
plot(teljes[,1])
write.zoo(teljes[,1],"hom2021.csv",sep=";",dec=",")

## Órás hőmérséklet
ep.hours <- endpoints(teljes[,1], "hours")
hom.hours <- period.apply(teljes[,1], ep.hours, mean)
write.zoo(hom.hours,"hom2021oras.csv",sep=";",dec=",")

##Páratartalom
plot(teljes[,3])
write.zoo(teljes[,3],"para2021.csv",sep=";",dec=",")

##Csapadék
plot(teljes[,11])
write.zoo(teljes[,11],"csap2021.csv",sep=";",dec=",")

## Mentés napi összegként
csap.napi <- apply.daily(teljes[,11], sum)
plot(csap.napi, type = "h")
write.zoo(csap.napi,"napicsap2021.csv",sep=";",dec=",")
