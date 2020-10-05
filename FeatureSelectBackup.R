setwd("~/Dropbox/Transfer/Projects/Personhood/WordLists")
chi.wordlists<-read.csv(file="Final20cWordlist.csv", header=T, stringsAsFactors=F)
setwd("~/Dropbox/Transfer/Projects/Personhood/RV_Method/ChicagoModel")
chi.features<-read.csv(file="50K_ModelFeatureTable.csv", header=T, stringsAsFactors = F)
hum.obj.index<-which(chi.wordlists[,2] %in% c("Human", "Object"))
length(hum.obj.index)
chi.wordlists<-chi.wordlists[hum.obj.index,]
chi.names<-chi.features[,1]
chi.features<-chi.features[,-1]
dup.index<-which(duplicated(chi.names))
chi.features<-chi.features[-dup.index,]
chi.names<-chi.names[-dup.index]
rownames(chi.features)<-chi.names
chi.features<-chi.features[which(rownames(chi.features) %in% chi.wordlists[,1]),]
chi.wordlists<-chi.wordlists[which(chi.wordlists[,1] %in% rownames(chi.features)),]
chi.wordlists<-chi.wordlists[order(chi.wordlists[,1]),]
chi.features<-chi.features[order(rownames(chi.features)),]
chi.features.scaled<-chi.features/rowSums(chi.features)
source('~/Dropbox/Transfer/Projects/Personhood/RV_Method/AmficModel/Code/logisticFunctions.R')
chi.groups<-rep(0, nrow(chi.wordlists))
chi.groups[which(chi.wordlists[,2]=="Human")]<-1
