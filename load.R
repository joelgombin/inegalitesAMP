# load data
library(sp)
library(maptools)
library(rgeos)
library(rgdal)
library(plyr)
library(tidyr)
library(reshape)
library(dplyr)

load("./data/RFDUiris20012011.Rdata")
load("./data/RFDUcomm20012011.Rdata")

load("./data/communes_ident.Rdata")
AMM <- c("13001","13002","13003","13005","13007","13008","13009","13012","13013","13014","13015","13016","13019","13020","13021","13022","13023","13024","13025","13026","13028","13029","13030","13031","13032","13033","13035","13037","13039","13040","13041","13042","13043","13044","13046","13047","13048","13049","13050","13051","13053","13054","13055","13056","13059","13060","13062","13063","13069","13070","13071","13072","13073","13074","13075","13077","13078","13079","13080","13081","13082","13084","13085","13086","13087","13088","13090","13091","13092","13093","13095","13098","13099","13101","13102","13103","13104","13105","13106","13107","13109","13110","13111","13112","13113","13114","13115","13117","13118","13119","83120","84089", as.character(13201:13216))


load("./data/IRISCONTOURS-IRIS84.Rdata")
load("./data/IRISCONTOURS-IRIS83.Rdata")
load("./data/IRISCONTOURS-IRIS13.Rdata")
`IRISCONTOURS-IRIS13` <- spChFIDs(`IRISCONTOURS-IRIS13`, `IRISCONTOURS-IRIS13`@data$DCOMIRIS)
`IRISCONTOURS-IRIS84` <- spChFIDs(`IRISCONTOURS-IRIS84`, `IRISCONTOURS-IRIS84`@data$DCOMIRIS)
`IRISCONTOURS-IRIS83` <- spChFIDs(`IRISCONTOURS-IRIS83`, `IRISCONTOURS-IRIS83`@data$DCOMIRIS)
iris <- rbind.SpatialPolygonsDataFrame(`IRISCONTOURS-IRIS13`, `IRISCONTOURS-IRIS84`, `IRISCONTOURS-IRIS83`)
communes <- unionSpatialPolygons(iris, iris@data$DEPCOM)
communes <- SpatialPolygonsDataFrame(communes, data = data.frame(ID = row.names(communes), row.names = row.names(communes), stringsAsFactors = FALSE))

iris <- iris[iris@data$DEPCOM %in% AMM, ]
communes <- communes[communes@data$ID %in% AMM, ]

iris <- spTransform(iris, CRSobj = CRS("+ellps=WGS84 +proj=longlat +datum=WGS84 +no_defs"))
communes <- spTransform(communes, CRSobj = CRS("+ellps=WGS84 +proj=longlat +datum=WGS84 +no_defs"))

characterise <- function(df) {
  df$CodeIris <- as.character(df$CodeIris)
  df
}

for (i in c(2001, 2002, 2004:2011)) {
  assign(paste0("RFDU", i, "iris"),
         value = characterise(get(paste0("RFDU", i, "iris"))))
}

RFDU <- join_all(mget(paste0("RFDU", c(2001, 2002, 2004:2011), "iris")), by = "CodeIris")

RFDU <- RFDU %>%
          filter(substr(CodeIris, 1, 5) %in% AMM)

RFDU <- RFDU %>%
  gather("key", "value", -CodeIris) %>%
  separate(key, c("variable", "année"), sep = 6) %>%
  mutate(année = 2000 + as.integer(année)) %>%
  mutate(CodeInsee = substr(CodeIris, 1, 5))

RFDUcomm <- RFDUcomm %>%
  mutate(CodeInsee = as.character(CodeInsee)) %>%
  gather("key", "value", -CodeInsee) %>%
  separate(key, c("variable", "année"), sep = 6) %>%
  mutate(année = 2000 + as.integer(année)) %>%
  filter(CodeInsee %in% AMM) %>% 
  mutate(iris = paste0(CodeInsee, "0000"))

getvalue <- function(i) {
  RFDUcomm[RFDUcomm$CodeInsee %in% ifelse(RFDU[i, "CodeInsee"] %in% 13201:13216, "13055", RFDU[i, "CodeInsee"]) &
             RFDUcomm$variable %in% RFDU[i, "variable"] &
             RFDUcomm$année %in% RFDU[i, "année"], "value"]
}

RFDU[is.na(RFDU$value), "value"] <- sapply(which(is.na(RFDU$value)), getvalue)

RFDUiris <- bind_rows(RFDUcomm %>% mutate(CodeIris = iris) %>% select(-CodeInsee, -iris),
                          RFDU )
RFDUiris$CodeInsee <- substr(RFDUiris$CodeIris, 1, 5)

RFDUiris[RFDUiris$variable %in% "RFUCRD", "value"] <- log(RFDUiris[RFDUiris$variable %in% "RFUCRD", "value"])


test <- RFDUiris %>% group_by(CodeInsee) %>% summarise(n = n()) %>% filter(n == 66)
test <- test[["CodeInsee"]]
test <- test[-which(test %in% "13055")]


interm <- do.call(rbind, lapply(test, function(i) {
  tmp <- communes[communes@data$ID %in% i,]
  tmp@data <- data.frame(DEPCOM = tmp@data$ID, NOM_COM = unique(iris@data[iris@data$DEPCOM %in% i, "NOM_COM"]), IRIS = "0000", DCOMIRIS = paste0(i, "0000"), NOM_IRIS = unique(iris@data[iris@data$DEPCOM %in% i, "NOM_COM"]), TYP_IRIS = NA, ORIGINE = NA, row.names = row.names(tmp@data))
  tmp
})
)

iris <- rbind(interm, iris[!iris@data$DEPCOM %in% test,])


