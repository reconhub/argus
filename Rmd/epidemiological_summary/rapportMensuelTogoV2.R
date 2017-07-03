########
## JG: to review and put in R markdown
#######









#####################################
##### Donnees 
# Tables MySQL du pilote d'Argus au Togo
##### Etapes
# Graphiques de compl√©tude/promptitude des structures sanitaires par semaine pour Lom√© Communes et Savanes
# Graphiques de compl√©tude/promptitude de validation par les districts pour Lom√© Communes et Savanes
# Graphiques du nombre d'√©v√®nements sanitaires pour Lom√© Communes et Savanes
# Cartes du nombre d'√©v√®nements sanitaires pour Lom√© Communes et Savanes
# Tableau de cumul du nombre de cas depuis le pilote
# ## Resultats
# Donn√©es du rapport mensuel Togo
#######################################


#### Monitorer le pilote de Argus au Togo

library(RMySQL)
library(RSQLite)
library(Hmisc)
library(lubridate)
library(ggplot2)
library(grid)
library(gtools)

########################
##*************************** revoir le script avec les nouvelles variables de la base de donnÈes, prendre exemple sur le script monitoring
#*****************************************************************

baseSql <- dbConnect(MySQL(),user = 'root',password = '0000',host = 'localhost',dbname = 'frontlinesms2')

# p√©riode d'analyse

# depuis le d√©but du pilote
dateBaseline <- as.Date("23/05/16", format="%d/%m/%y")
# p√©riode d'interet pour le rapport
dateBaseline2 <- as.Date("30/01/17", format="%d/%m/%y")
dateEstimate <- as.Date("20/02/17", format="%d/%m/%y")

numSem <- seq(from=week(dateBaseline), to=week(dateEstimate), by=1)
numSem2 <- seq(from=week(dateBaseline2), to=week(dateEstimate), by=1)

# chemin pour les fichiers:
path <- "D:/travaux/National support/Togo/Argus pilot test/rapport_mensuel/results_february/"

# Cr√©ation des bases de donn√©es pour les analyses

userData <- dbGetQuery(baseSql, paste("SELECT path, reception, contactPhoneNumber, periodStart, disease, value1 FROM ses_data WHERE disease <> 'ALERT' AND periodStart >=", paste("'",dateBaseline," 00:00:00'", sep=""),"AND periodStart <=",paste("'",dateEstimate," 23:59:59'", sep=""),";", sep=" "))

fullreport <- dbGetQuery(baseSql, paste("SELECT id, FK_SiteId, startDate, status, aggregate, createdDate, firstValidationDate, firstRejectionDate, statusModifiedDate FROM sesdashboard_fullreport WHERE startDate >=", paste("'",dateBaseline," 00:00:00'", sep=""),"AND startDate <=",paste("'",dateEstimate," 23:59:59'", sep=""),";", sep=" "))

sites <- dbGetQuery(baseSql, "SELECT id, name, level, path, parentSiteReference FROM sesdashboard_sites")

partreport <- dbGetQuery(baseSql, paste("SELECT id, status, FK_FullReportId, aggregate, createdDate, statusModifiedDate, firstValidationDate, firstRejectionDate FROM sesdashboard_partreport WHERE FK_FullReportId IN (",paste(fullreport$id,collapse =","),");"))

report <- dbGetQuery(baseSql, paste("SELECT id, disease, status, isArchived, FK_PartReportId, receptionDate FROM sesdashboard_report WHERE FK_PartReportId IN (",paste(partreport$id,collapse =","),");"))

reportvalues <- dbGetQuery(baseSql, paste("SELECT * FROM sesdashboard_reportvalues WHERE FK_ReportId IN (",paste(report$id,collapse =","),");"))

dbDisconnect(baseSql)

### nettoyage des donn√©es

userData$week <- week(userData$periodStart)
fullreport$week <- week(fullreport$startDate)

sites$name2 <- tolower(sites$name)
sites$name2 <-  gsub(pattern = "_", replacement = "", x = sites$name2)

userData$name2 <- userData$path

listZone <- sites[which(sites$level %in% c(1,2,3)),c("name","level","name2")]
listZone[which(duplicated(listZone$name))]
listZone$name[which(listZone$name=="Lome_Commune")] <- "LomeCom"

for (i in 1:length(listZone$name)) {
  userData$name2 <- gsub(pattern = listZone$name[i], replacement = "", x = userData$name2)
}
userData$name2 <-  gsub(pattern = "SitesRoot", replacement = "", x = userData$name2)
userData$name2 <-  gsub(pattern = "/", replacement = "", x = userData$name2)
userData$name2 <-  gsub(pattern = "_", replacement = "", x = userData$name2)
userData$name2 <-  tolower(userData$name2)

userData$name2[which(userData$name2=="hop")] <- "hopcinkasse"
userData$name2[which(userData$name2 %nin% sites$name2)]

listZone <- sites[which(sites$level==3),c("name","parentSiteReference","level","name2")]
listZone[which(duplicated(listZone$name))]

reportvalues <- reportvalues[,-c(1,5,6)]

### imputation des donnÈes dans table fullreport ‡ partir de la table userData du fait du changement de structure de la base de donnÈes en cours de pilote

userData$reception <- as.POSIXct(userData$reception)

# createdDate

fullreportOld <- which(is.na(fullreport$createdDate))

for (i in fullreportOld) {
  temp <-NA
  temp <- which(userData$name2==sites$name2[which(sites$id==fullreport$FK_SiteId[i])] & userData$week==fullreport$week[i])
  fullreport$createdDate[i] <- ifelse(length(temp)==0,NA,as.character(min(userData$reception[temp])))
}
fullreport$createdDate <- as.POSIXct(fullreport$createdDate)

# firstValidationDate

fullreport$firstValidationDate <- ifelse(fullreport$status=="VALIDATED" & is.na(fullreport$firstValidationDate),fullreport$statusModifiedDate,fullreport$firstValidationDate)

# firstRejectionDate

fullreport$firstRejectionDate <- ifelse(fullreport$status=="REJECTED" & is.na(fullreport$firstRejectionDate),fullreport$statusModifiedDate,fullreport$firstRejectionDate)


# retirer sites avec t√©l√©phones non distribu√©s
sites2 <- sites[-which(sites$name %in% c( "Lowossou","Meilleure_Sante","Veritas","Le_Diagnostic","Lotus","Novissi","Gendarmerie","Jerusalem","FAT","ATBEF","GADH","MUSA_CSTT","Saint_martyr_de_louganda")),]

## Cr√©ation tables d'int√©r√™t

# table pour le calcul des compl√©tudes / promptitudes de r√©ception et validation + % de rapports valid√©s inclus dans les analyses (rapports valid√©s et derni√®res donn√©es valid√©es en cas de mise √† jour)

fullreport$startDate <- as.POSIXct(fullreport$startDate)
fullreport$firstValidationDate <- as.POSIXct(fullreport$firstValidationDate)
fullreport$firstRejectionDate <- as.POSIXct(fullreport$firstRejectionDate)

rapports <- matrix(c("semaine","region","district","nb_recus","nb_attendus","nb_recus_tim","nb_valid_refus","nb_valid_refus_tim","nb_valid"),nrow = 1 )

# nb_recus_tim : rapports re√ßus avant lundi 16h
# nb_valid_refus : rapports non en attente
# nb_valid_refus_tim : rapports non en attente avant lundi minuit
# nb_valid : rapports valid√©s ou ayant √©t√© valid√©s puis mis √† jour (pour connaitre le nb de rapports utilis√©s dans les analyses)

for (i in 1:length(numSem)) {
  startWeek <- as.POSIXct(userData$periodStart[which(userData$week==numSem[i])][1])

  for (j in 1:length(listZone$name)) {
    temp <- matrix(NA, nrow=1, ncol = 9)
    temp[1,1] <- numSem[i]
    temp[1,2] <- listZone$parentSiteReference[j]
    temp[1,3] <- listZone$name[j]
    
    temp2 <- NULL
    temp2 <- sites2[grep(x=sites2$path,pattern=listZone$name[j]),]
    temp2 <- temp2$id[which(temp2$level==4)]
    
    temp3 <- NULL
    temp3 <- fullreport$id[which(fullreport$FK_SiteId %in% temp2 & fullreport$week==numSem[i])]
    
    temp[1,4] <- length(temp3)
    
    temp[1,5] <- length(temp2)
    
    temp4 <- NULL
    temp4 <- fullreport$id[which(fullreport$FK_SiteId %in% temp2 & fullreport$week==numSem[i] & fullreport$createdDate < (fullreport$startDate+662400))]
    
    temp[1,6] <- length(temp4)
    
    temp5 <- NULL
    temp5 <- sites2[grep(x=sites2$path,pattern=listZone$name[j]),]
    temp5 <- temp5$id[which(temp5$level==(listZone$level[j]+1))]
    
    temp6 <- NA
    temp6 <- fullreport$id[which(fullreport$FK_SiteId %in% temp5 & fullreport$week==numSem[i] & fullreport$status!="PENDING")]
    
    temp[1,7] <- length(temp6)
    
    temp7 <- NULL
    temp7 <- fullreport$id[which(fullreport$FK_SiteId %in% temp5 & fullreport$week==numSem[i] & fullreport$status!="PENDING" & fullreport$createdDate < (fullreport$startDate+662400) & ((!is.na(fullreport$firstValidationDate) & fullreport$firstValidationDate < (fullreport$startDate+691200)) | (!is.na(fullreport$firstRejectionDate) & fullreport$firstRejectionDate < (fullreport$startDate+691200))))]
    
    temp[1,8] <- length(temp7)
    
    temp8 <- NULL
    temp8 <- fullreport$id[which(fullreport$FK_SiteId %in% temp5 & fullreport$week==numSem[i] & fullreport$status=="VALIDATED")]

    temp9 <- NULL
    temp9 <- fullreport$id[which(fullreport$FK_SiteId %in% temp5 & fullreport$week==numSem[i] & fullreport$status=="CONFLICTING")]
    
    temp10 <- NULL
    if (length(temp9>0)) {
      temp10 <- 0
      for (k in 1:length(temp9)) {
        temp11 <- NA
        temp11 <- partreport[which(partreport$FK_FullReportId==temp9[k]),]
        temp11 <- ifelse(temp11$status[length(temp11$id)-1]=="VALIDATED",temp11$id[length(temp11$id)-1],0)
        temp10 <- c(temp10,temp11)
      }
      temp10 <- temp10[-which(temp10==0)]
    } else {
    }
    
    temp[1,9] <- length(temp8) + length(temp10)
    
    rapports <- rbind(rapports,temp)
  }
}

rapports <- data.frame(rapports, stringsAsFactors = F)
colnames(rapports) <- as.character(rapports[1,])
rapports <- rapports[-1,]

rapports$region[which(rapports$region=="LomeCom")] <- "Lome Commune"

for (i in 4:9) {
  rapports[,i] <- as.integer(rapports[,i])
}

# table des donn√©es de cas et de d√©c√®s pour les rapports ayant √©t√© valid√©s au moins une fois

##### revoir en fonction des changements op√©r√©s sur la base de donn√©es MySql, actuellement les donn√©es √† utiliser sont celles des partreport "VALIDATED" dont les reports ont un isArchived==0, il s'agit des derni√®res donn√©es valid√©es, que le fullreport soit en statut valid√© ou mis √† jour.
## A vÈrifier avec Emmanuel, pour Ítre s˚r que cela soit correct


donnees <- matrix(c("semaine","region","district","maladie","cas","deces"),nrow = 1 )

diseases <- data.frame(name2=unique(report$disease), name=c("Rougeole","MÈningite","CholÈra","DiarrÈe rouge","DiarrhÈe grave","FiËvre jaune","TÈtanos nÈonatal","PFA","Charbon humain","Rage","Sd grippal","IRAS","SFH"), stringsAsFactors = F)

for (i in 1:length(numSem)) {
  temp2 <- NA
  temp2 <- sites2[which(sites2$level==4),]
  
  for (j in 1:length(listZone$name)) {
    tempSites <- NA
    tempSites <- temp2$id[grep(x=temp2$path,pattern=listZone$name[j])]
    tempFull <- NA
    tempFull <- fullreport$id[which(fullreport$week==numSem[i] & fullreport$FK_SiteId %in% tempSites & fullreport$status %in% c("VALIDATED","CONFLICTING"))]
    tempPart <- NA
    tempPart <- partreport$id[which(partreport$FK_FullReportId %in% tempFull & partreport$status=="VALIDATED")]
    tempReport <- NA
    tempReport <- report[which(report$FK_PartReportId %in% tempPart & report$isArchived==0), c("id","disease")]
    
    for (k in 1:length(diseases$name2)) { 
      temp <- matrix(NA, nrow=1, ncol = 6)
      temp[1,1] <- numSem[i]
      temp[1,2] <- listZone$parentSiteReference[j]
      temp[1,3] <- listZone$name[j]
      temp[1,4] <- as.character(diseases$name[k])
      temp[1,5] <- sum(reportvalues$Value[which(reportvalues$FK_ReportId %in% tempReport$id[which(tempReport$disease==diseases$name2[k])] & reportvalues$Key=="cas")])
      temp[1,6] <- sum(reportvalues$Value[which(reportvalues$FK_ReportId %in% tempReport$id[which(tempReport$disease==diseases$name2[k])] & reportvalues$Key=="deces")])

    donnees <- rbind(donnees,temp)
    }
  }
}

donnees <- data.frame(donnees, stringsAsFactors = F)
colnames(donnees) <- as.character(donnees[1,])
donnees <- donnees[-1,]

donnees$region[which(donnees$region=="LomeCom")] <- "Lome Commune"

for (i in 5:6) {
  donnees[,i] <- as.integer(donnees[,i])
}

# Cumul du nombre de cas depuis le d√©but du pilote

cumulDonnees <- aggregate(donnees[,c("cas","deces")], by = list(donnees$maladie,donnees$region),FUN = "sum")

cumulDonnees <- data.frame(cumulDonnees, stringsAsFactors = F)
colnames(cumulDonnees) <- c("maladie","region","cas", "deces")
cumulDonnees <- cumulDonnees[,c("region","maladie","cas", "deces")]

cumulDonnees2 <- reshape(cumulDonnees,idvar = "region",v.names = c("cas","deces"), timevar="maladie", direction="wide")
cumulDonnees2$valid <- NA

cumulRapports <- aggregate(rapports[,c("nb_valid","nb_attendus")], by = list(rapports$region),FUN = "sum")
cumulRapports <- data.frame(cumulRapports, stringsAsFactors = F)
colnames(cumulRapports) <- c("region","nb_valid", "nb_attendus")


for (i in 1:length(cumulDonnees2$region)) {
  temp1 <- NA
  temp1 <- cumulRapports$nb_valid[which(cumulRapports$region==cumulDonnees2$region[i])]
  temp2 <- NA
  temp2 <- cumulRapports$nb_attendus[which(cumulRapports$region==cumulDonnees2$region[i])]
  cumulDonnees2$valid[i] <- paste(temp1,"/",temp2," (",round((temp1/temp2)*100,1),"%)",sep="")
}

cumulDonnees2$periode <- paste("S",min(numSem)," - S",max(numSem), sep="")
cumulDonnees2 <- cumulDonnees2[,c(1,29,28,2:27)]


write.csv(cumulDonnees2,paste(path, "cumulDonnees2.csv", sep=""),na="",row.names=F)

## cumul du nombre de rapports attendus et du nombre de rapports utilisÈs pour les analyses prÈsentÈes sur les graphiques

rapportsPer <- rapports[which(rapports$semaine %in% numSem2),]
cumulRapportsPer <- aggregate(rapportsPer[,c("nb_valid","nb_attendus")], by = list(rapportsPer$region),FUN = "sum")
cumulRapportsPer <- data.frame(cumulRapportsPer, stringsAsFactors = F)
colnames(cumulRapportsPer) <- c("region","nb_valid", "nb_attendus")

#### Graphiques

##  Completude/promptitude

rapports$envoi.completude <- round((rapports$nb_recus/rapports$nb_attendus)*100,1)
rapports$valid.completude <- round((rapports$nb_valid_refus/rapports$nb_recus)*100,1)
rapports$envoi.promptitude <- round((rapports$nb_recus_tim/rapports$nb_attendus)*100,1)
rapports$valid.promptitude <- round((rapports$nb_valid_refus_tim/rapports$nb_recus_tim)*100,1)

rapports$semaine <- as.integer(rapports$semaine)

rapports2 <- rapports[,c(1:3,10:13)]

rapports2 <- reshape(rapports2,varying = c(4:7),v.names= c("envoi","valid"), direction="long", times=c("completude","promptitude"))

rapports2$district <- factor(rapports2$district,levels = c("District1","Cinkasse","District2","Kpendjal","District3","Oti","District4","Tandjaore","District5","Tone"))

label_districts <- vector(mode = "character",length = 10)
nomNumDistrict <- unique(rapports[,c("district","nb_attendus")])
for (i in 1:length(nomNumDistrict$district)) {
  label_districts[[i]] <- paste(nomNumDistrict$district[i],", n=",as.character(nomNumDistrict$nb_attendus[i]),"*", sep="")
}
names(label_districts) <- nomNumDistrict$district
for (i in 1:5) {
  label_districts[i] <- paste(substr(label_districts[i],start = 1,stop = 8),substr(label_districts[i], start=9, stop = nchar(label_districts[i])))
}

png(paste(path,"graphEnvoi.png", sep=""), res = 300,width = 15, units = "cm", height = 20)

graphEnvoi <- ggplot(data = rapports2[which(rapports2$semaine %in% numSem2),],aes(x=semaine, y=envoi))
graphEnvoi + geom_point(aes(shape=time), size=I(2.5)) + 
  scale_y_continuous(breaks=c(0,20,40,60,80,100), limits = c(0,100)) +
  scale_shape(name="", breaks=c("completude","promptitude"), labels=c("ComplÈtude","Promptitude")) +
  geom_line(aes(linetype=time)) + 
  scale_linetype(name="", breaks=c("completude","promptitude"), labels=c("ComplÈtude","Promptitude")) +
  geom_abline(intercept=80, slope=0, color=I("red")) +
  facet_wrap(~district,nrow=5,labeller = labeller(district=label_districts)) +
  labs(title="Envoi des rapports hebdomadaires",x="Semaine ÈpidÈmiologique", y="%") +
  theme(legend.position="bottom", panel.background = element_rect(fill = "white", colour = "grey"), panel.grid.major = element_line(colour = "grey", linetype = "dotted"), strip.text=element_text(size = 11), axis.title=element_text(size = 10), legend.text=element_text(size=11), legend.key.size = unit(0.7, "cm"), legend.key=element_rect(fill="white", colour="white"), axis.text.x=element_text(size=8,colour = "black", hjust = 0),axis.text.y=element_text(size=9,colour = "black"))

dev.off()

png(paste(path,"graphValid.png", sep=""), res = 300,width = 15, units = "cm", height = 20)

graphValid <- ggplot(data = rapports2[which(rapports2$semaine %in% numSem2),],aes(x=semaine, y=valid))
graphValid + geom_point(aes(shape=time), size=I(2.5)) + 
  scale_y_continuous(breaks=c(0,20,40,60,80,100)) +
  scale_shape(name="", breaks=c("completude","promptitude"), labels=c("ComplÈtude","Promptitude")) +
  geom_line(aes(linetype=time)) + 
  scale_linetype(name="", breaks=c("completude","promptitude"), labels=c("ComplÈtude","Promptitude")) +
  geom_abline(intercept=80, slope=0, color=I("red")) +
  facet_wrap(~district,nrow=5) +
  labs(title="Validation des rapports hebdomadaires",x="Semaine ÈpidÈmiologique", y="%") +
  theme(legend.position="bottom", panel.background = element_rect(fill = "white", colour = "grey"), panel.grid.major = element_line(colour = "grey", linetype = "dotted"), strip.text=element_text(size = 11), axis.title=element_text(size = 10), legend.text=element_text(size=11), legend.key.size = unit(0.7, "cm"), legend.key=element_rect(fill="white", colour="white"), axis.text.x=element_text(size=8,colour = "black", hjust = 0),axis.text.y=element_text(size=9,colour = "black"))

dev.off()

## donn√©es

donnees$semaine <- as.numeric(donnees$semaine)

donneesZero <- aggregate(cbind(cas,deces) ~ region + maladie, data=donnees[which(donnees$semaine %in% numSem2),],sum)
donneesZeroLom <- donneesZero$maladie[which(donneesZero$cas==0 & donneesZero$deces==0 & donneesZero$region=="Lome Commune")]
donneesZeroSav <- donneesZero$maladie[which(donneesZero$cas==0 & donneesZero$deces==0 & donneesZero$region=="Savanes")]


donnees2 <- aggregate(cbind(cas,deces) ~ region + maladie + semaine, data=donnees,sum)

donnees2 <- reshape(donnees2,varying = c(4:5),v.names="data", direction="long", times=c("cas","deces"))

maladieLab <- unique(donnees2$maladie)
maladieLab <- maladieLab[c(10,7,2,4,3,5,13,8,1,9,11,6,12)]

maladieLabLom <- maladieLab[-which(maladieLab %in% donneesZeroLom)]
maladieLabSav <- maladieLab[-which(maladieLab %in% donneesZeroSav)]

cumulRapports2 <- aggregate(rapports[which(rapports$semaine %in% numSem2),c("nb_valid","nb_attendus")], by = list(rapports$region[which(rapports$semaine %in% numSem2)]),FUN = "sum")
cumulRapports2 <- data.frame(cumulRapports, stringsAsFactors = F)
colnames(cumulRapports2) <- c("region","nb_valid", "nb_attendus")

labelTitreLomCom <- paste("Evolution du nombre de cas et de dÈcËs - LomÈ Commune \n  ",round(100*(cumulRapportsPer$nb_valid[cumulRapportsPer$region=="Lome Commune"]/cumulRapportsPer$nb_attendus[cumulRapportsPer$region=="Lome Commune"]),0),"% (n=",cumulRapportsPer$nb_valid[cumulRapportsPer$region=="Lome Commune"],"/",cumulRapportsPer$nb_attendus[cumulRapportsPer$region=="Lome Commune"], ") des rapports reÁus et validÈs",sep="")

labelTitreSav <- paste("Evolution du nombre de cas et de dÈcËs - Savanes \n  ",round(100*(cumulRapportsPer$nb_valid[cumulRapportsPer$region=="Savanes"]/cumulRapportsPer$nb_attendus[cumulRapportsPer$region=="Savanes"]),0),"% (n=",cumulRapportsPer$nb_valid[cumulRapportsPer$region=="Savanes"],"/",cumulRapportsPer$nb_attendus[cumulRapportsPer$region=="Savanes"], ") des rapports reÁus et validÈs",sep="")

# fonction pour cr√©e les valeurs de l'axe des ordonn√©es selon le nombre de cas de maladie
seqRange  <-   function(x) {
  x <- round(ceiling(x*1.2),0)
  x <- ifelse(even(x),x,x+1)
  x <- unique(round(seq(from=0, to=x, length.out = 4),0))
  x[odd(x)] <- x[odd(x)] + 1
  x <- unique(x)
  x
}

#Lome commune

png(paste(path,"graphDonneesLM.png", sep=""), res = 300,width = 15, units = "cm", height = 20)

grid.newpage()
pushViewport(viewport(layout = grid.layout(nrow=ceiling(length(maladieLabLom)/3)+1, ncol=3)))
j <- rep(x = c(1,2,3), times=ceiling(length(maladieLabLom)/3))

for (i in 1:length(maladieLabLom)) {
  
  temp <- NA
  temp <- seqRange(max(donnees2$data[which(donnees2$region=="Lome Commune" & donnees2$maladie==maladieLabLom[i] & donnees2$semaine %in% numSem2)]))
  
  graphDonneesLM <- ggplot(data = donnees2[which(donnees2$region=="Lome Commune" & donnees2$maladie==maladieLabLom[i] & donnees2$semaine %in% numSem2),],aes(x=semaine, y=data)) +
    geom_point(aes(shape=time), size=I(2.5)) + 
    scale_y_continuous(breaks = temp, limits=c(min(temp),max(temp))) +
    geom_line(aes(linetype=time)) +
    labs(title=maladieLabLom[i],x="NumÈro de semaine", y="") +
    theme(legend.position="" , panel.background = element_rect(fill = "white", colour = "grey"), panel.grid.major = element_line(colour = "grey", linetype = "dotted"), plot.title=element_text(size = 12, face = "bold"), axis.title=element_text(size = 11), legend.text=element_text(size=11), legend.key.size = unit(1.2, "cm"), legend.key=element_rect(fill="white", colour="white"))
  
  print(graphDonneesLM, vp=viewport(layout.pos.row=ceiling(i/3)+1, layout.pos.col=j[i]))
  
} 

a <- data.frame(x=0,y=0,time=c("Cas","DÈcËs")) 
graphTitreLegende <-  ggplot(data = a[,],aes(x=x, y=y)) +
  geom_point(aes(shape=time), size=I(2.5)) + 
  geom_line(aes(linetype=time)) +
  annotate("text",x=0, y=0,label=labelTitreLomCom, size=4) +
  annotate("point",x=0, y=0, size=4, colour="white") +
  labs(x="",y="") +
  theme(legend.position="right" , legend.title=element_blank(), panel.background = element_blank(), axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),legend.text=element_text(size=11), legend.key.size = unit(1.2, "cm"), legend.key=element_rect(fill="white", colour="black"))
  
print(graphTitreLegende, vp=viewport(layout.pos.row=1, layout.pos.col=c(1,3)))

dev.off()

#savanes

png(paste(path,"graphDonneesSav.png", sep=""), res = 300,width = 15, units = "cm", height = 20)

grid.newpage()
pushViewport(viewport(layout = grid.layout(nrow=ceiling(length(maladieLabSav)/3)+1, ncol=3)))

j <- rep(x = c(1,2,3), times=ceiling(length(maladieLabSav)/3))

for (i in 1:length(maladieLabSav)) {
  
  temp <- NA
  temp <- seqRange(max(donnees2$data[which(donnees2$region=="Savanes" & donnees2$maladie==maladieLabSav[i] & donnees2$semaine %in% numSem2)]))
  
  graphDonneesSav <- ggplot(data = donnees2[which(donnees2$region=="Savanes" & donnees2$maladie==maladieLabSav[i] & donnees2$semaine %in% numSem2),],aes(x=semaine, y=data)) +
    geom_point(aes(shape=time), size=I(2.5)) + 
    scale_y_continuous(breaks = temp, limits=c(min(temp),max(temp))) +
    geom_line(aes(linetype=time)) +
    labs(title=maladieLabSav[i],x="NumÈro de semaine", y="") +
    theme(legend.position="" , panel.background = element_rect(fill = "white", colour = "grey"), panel.grid.major = element_line(colour = "grey", linetype = "dotted"), plot.title=element_text(size = 12, face = "bold"), axis.title=element_text(size = 11), legend.text=element_text(size=11), legend.key.size = unit(1.2, "cm"), legend.key=element_rect(fill="white", colour="white"))
  
  print(graphDonneesSav, vp=viewport(layout.pos.row=ceiling(i/3)+1, layout.pos.col=j[i]))
  
} 

a <- data.frame(x=0,y=0,time=c("Cas","DÈcËs")) 
graphTitreLegende <-  ggplot(data = a[,],aes(x=x, y=y)) +
  geom_point(aes(shape=time), size=I(2.5)) + 
  geom_line(aes(linetype=time)) +
  annotate("text",x=0, y=0,label=labelTitreSav, size=4) +
  annotate("point",x=0, y=0, size=4, colour="white") +
  labs(x="",y="") +
  theme(legend.position="right" , legend.title=element_blank(), panel.background = element_blank(), axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),legend.text=element_text(size=11), legend.key.size = unit(1.2, "cm"), legend.key=element_rect(fill="white", colour="black"))

print(graphTitreLegende, vp=viewport(layout.pos.row=1, layout.pos.col=c(1,3)))

dev.off()

## Cartes

library(png)
library(qdap)

donnees$semaine <- as.numeric(donnees$semaine)

donnees3 <- aggregate(cas ~ district + maladie, data=donnees[which(donnees$semaine %in% numSem2),],sum)

savanesGrob <- rasterGrob(readPNG("D:/travaux/National support/Togo/Argus pilot test/rapport_mensuel/images/Savanes3.png"), 0,0,1,1, just=c("left","bottom"))

lomeComGrob <- rasterGrob(readPNG("D:/travaux/National support/Togo/Argus pilot test/rapport_mensuel/images/mapLomeCommune4.png"), 0,0,1,1, just=c("left","bottom"))

donnees3$long <- NA
donnees3$lat <- NA

donnees3$long[which(donnees3$district=="Cinkasse")] <- 0.25
donnees3$lat[which(donnees3$district=="Cinkasse")] <- 0.88

donnees3$long[which(donnees3$district=="Kpendjal")] <- 0.7
donnees3$lat[which(donnees3$district=="Kpendjal")] <- 0.725

donnees3$long[which(donnees3$district=="Oti")] <- 0.7
donnees3$lat[which(donnees3$district=="Oti")] <- 0.35

donnees3$long[which(donnees3$district=="Tandjaore")] <- 0.25
donnees3$lat[which(donnees3$district=="Tandjaore")] <- 0.6

donnees3$long[which(donnees3$district=="Tone")] <- 0.3
donnees3$lat[which(donnees3$district=="Tone")] <- 0.775

donnees3$long[which(donnees3$district=="District1")] <- 0.35
donnees3$lat[which(donnees3$district=="District1")] <- 0.175

donnees3$long[which(donnees3$district=="District2")] <- 0.6
donnees3$lat[which(donnees3$district=="District2")] <- 0.7

donnees3$long[which(donnees3$district=="District3")] <- 0.65
donnees3$lat[which(donnees3$district=="District3")] <- 0.375

donnees3$long[which(donnees3$district=="District4")] <- 0.22
donnees3$lat[which(donnees3$district=="District4")] <- 0.2

donnees3$long[which(donnees3$district=="District5")] <- 0.15
donnees3$lat[which(donnees3$district=="District5")] <- 0.75

donnees3 <- donnees3[-which(donnees3$cas==0),]

labelMapLomCom <- paste("Distribution du nombre de cas - LomÈ Commune"," S",min(numSem2)," - S",max(numSem2),"\n ",round(100*(cumulRapportsPer$nb_valid[cumulRapportsPer$region=="Lome Commune"]/cumulRapportsPer$nb_attendus[cumulRapportsPer$region=="Lome Commune"]),0),"% (n=",cumulRapportsPer$nb_valid[cumulRapportsPer$region=="Lome Commune"],"/",cumulRapportsPer$nb_attendus[cumulRapportsPer$region=="Lome Commune"], ") des rapports reÁus et validÈs",sep="")

labelMapSav <- paste("Distribution du nombre de cas - Savanes"," S",min(numSem2)," - S",max(numSem2),"\n ",round(100*(cumulRapportsPer$nb_valid[cumulRapportsPer$region=="Savanes"]/cumulRapportsPer$nb_attendus[cumulRapportsPer$region=="Savanes"]),0),"% (n=",cumulRapportsPer$nb_valid[cumulRapportsPer$region=="Savanes"],"/",cumulRapportsPer$nb_attendus[cumulRapportsPer$region=="Savanes"], ") des rapports reÁus et validÈs",sep="")

seqRange2  <-   function(x) {
  x <- round(ceiling(x*1.2),0)
  x <- unique(round(seq(from=1, to=x, length.out = 3),0))
  x <- unique(x)
  x
}

for (i in 1:3) {
  print(seqRange2(i))
}

#Lome commune

png(paste(path,"mapLom.png", sep=""), res = 300,width = 18, units = "cm", height = 20)

grid.newpage()
pushViewport(viewport(layout = grid.layout(nrow=ceiling(length(maladieLabLom)/3)+1, ncol=3)))

j <- rep(x = c(1,2,3), times=ceiling(length(maladieLabLom)/3))

for (i in 1:length(maladieLabLom)) {
  
  temp <- NA
  temp <- unique(donnees3$cas[which(donnees3$district %in% c("District1","District2","District3","District4","District5") & donnees3$maladie==maladieLabLom[i])])
  temp <- temp[order(temp)]

  mapLom<- ggplot(data=donnees3[which(donnees3$district %in% c("District1","District2","District3","District4","District5") & donnees3$maladie==maladieLabLom[i]),], aes(x=long, y=lat, size=cas,color=I("red"))) +
    geom_point() +
    scale_size(breaks = temp) +
    annotation_custom(lomeComGrob, 0, 1, 0, 1) + 
    scale_x_continuous(breaks=seq(0, 1), limits = c(0,1))+ 
    scale_y_continuous(breaks=seq(0, 1), limits = c(0,1))+
    labs(title=maladieLabLom[i],x="", y="") +
    theme(panel.background = element_rect(colour="grey", fill="white"), axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(), plot.title=element_text(size = 12, face = "bold"), axis.title=element_text(size = 11), legend.text=element_text(size=8), legend.title=element_blank(), legend.key=element_rect(fill="white", colour="white"), legend.margin = unit(0.1, "cm"))

  print(mapLom, vp=viewport(layout.pos.row=ceiling(i/3)+1, layout.pos.col=j[i]))
  
} 

a <- data.frame(x=0,y=0) 
graphTitreLegende <-  ggplot(data = a[,],aes(x=x, y=y)) +
  annotate("text",x=0, y=0,label=labelMapLomCom, size=6) +
  labs(x="",y="") +
  theme(legend.position="" , panel.background = element_blank(), axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank())

print(graphTitreLegende, vp=viewport(layout.pos.row=1, layout.pos.col=c(1,3)))

dev.off()

# Savanes

png(paste(path,"mapSavanes.png", sep=""), res = 300,width = 18, units = "cm", height = 20)

grid.newpage()
pushViewport(viewport(layout = grid.layout(nrow=ceiling(length(maladieLabSav)/3)+1, ncol=3)))

j <- rep(x = c(1,2,3), times=ceiling(length(maladieLabSav)/3))

for (i in 1:length(maladieLabSav)) {
  
  temp <- NA
  temp <- unique(donnees3$cas[which(donnees3$district %in% c("Cinkasse","Kpendjal","Oti","Tandjaore","Tone") & donnees3$maladie==maladieLabSav[i])])
  temp <- temp[order(temp)]
  
  mapSavanes<- ggplot(data=donnees3[which(donnees3$district %in% c("Cinkasse","Kpendjal","Oti","Tandjaore","Tone") & donnees3$maladie==maladieLabSav[i]),]) +
    geom_point(aes(x=long, y=lat, size=cas,color=I("red"))) +
    scale_size(breaks = temp) +
    annotation_custom(savanesGrob, 0, 1, 0, 1) + 
    scale_x_continuous(breaks=seq(0, 1), limits = c(0,1))+ 
    scale_y_continuous(breaks=seq(0, 1), limits = c(0,1))+
    labs(title=maladieLabSav[i],x="", y="") +
    theme(panel.background = element_rect(colour="grey", fill="white"), axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(), plot.title=element_text(size = 12, face = "bold"), axis.title=element_text(size = 11), legend.text=element_text(size=8), legend.title=element_blank(), legend.key=element_rect(fill="white", colour="white"), legend.margin = unit(0.1, "cm"))
  
  print(mapSavanes, vp=viewport(layout.pos.row=ceiling(i/3)+1, layout.pos.col=j[i]))
  
} 

a <- data.frame(x=0,y=0) 
graphTitreLegende <-  ggplot(data = a[,],aes(x=x, y=y)) +
  annotate("text",x=0, y=0,label=labelMapSav, size=6) +
  labs(x="",y="") +
  theme(legend.position="" , panel.background = element_blank(), axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank())

print(graphTitreLegende, vp=viewport(layout.pos.row=1, layout.pos.col=c(1,3)))

dev.off()

