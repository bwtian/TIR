###############################################################################
# This code is used to clean the TEAM trees dataset for processing. This 
# combines the QA/QC checks from both Lydia Beaudrot and Alex Zvoleff.
#
# Use this script on a recent download from the team vegetation database.
#
# Date: July 2014
###############################################################################

library(dplyr)
library(stringr)
library(lubridate)

# Uncomment below to 
#veg_data <- f.teamdb.query('vegetation')

dir(".", pattern="veg_data")
load('H:/Data/TEAM_Database_Downloads/veg_data2014-09-11.gzip')

trees <- result$tree
sitecode_key <- read.csv("sitecode_key.csv")
trees$sitecode <- sitecode_key$sitecode[match(trees$SiteName, 
                                              sitecode_key$sitename_database)]
trees$ObservationDate <- as.Date(trees$ObservationDate)

trees <- tbl_df(trees)

###############################################################################
# Some sites (mainly BCI) incorrectly use zeros instead of NAs when NewDiameter 
# does not apply.  
trees$Diameter[trees$Diameter == 0] <- NA
trees$POMHeight[trees$POMHeight == 0] <- NA
trees$NewDiameter[trees$NewDiameter == 0] <- NA
trees$NewPOMHeight[trees$NewPOMHeight == 0] <- NA

###############################################################################
# Add an identifier to stems to code the sampling period number on a per site 
# basis, with 1 assigned to the first sampling period in each site
SamplingPeriods <- summarize(group_by(trees, sitecode, SamplingPeriod))
SamplingPeriods <- SamplingPeriods[order(SamplingPeriods$sitecode, SamplingPeriods$SamplingPeriod), ]
SamplingPeriods <- mutate(SamplingPeriods, SamplingPeriodNumber=seq(1, length(SamplingPeriod)))
trees$SamplingPeriodNumber <- SamplingPeriods$SamplingPeriodNumber[match(paste(trees$sitecode, trees$SamplingPeriod),
                                                                         paste(SamplingPeriods$sitecode, SamplingPeriods$SamplingPeriod))]

###############################################################################
# Clean the ConditionCodes field
trees$ConditionCodes[trees$ConditionCodes == " "] <- ""
trees$ConditionCodes[is.na(trees$ConditionCodes)] <- ""
trees$ConditionCodes[grepl(' ', trees$ConditionCodes)]
trees$ConditionCodes <- gsub('^,', '', trees$ConditionCodes)
trees$ConditionCodes <- gsub('[,.]$', '', trees$ConditionCodes)
trees$ConditionCodes <- gsub(' ', '', trees$ConditionCodes)
trees$ConditionCodes <- gsub('[.]', ',', trees$ConditionCodes)

table(grepl('[.]', trees$ConditionCodes))
table(grepl('[ ]', trees$ConditionCodes))
table(grepl('[.,]$', trees$ConditionCodes))
table(grepl('^[.,]', trees$ConditionCodes))

# Add condition code columns, one column per code
ConditionCodes <- c('B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 
                    'N', 'O', 'P', 'R', 'S', 'T', 'U', 'V', 'W')
for (ConditionCode in ConditionCodes) {
    this_code <- unlist(lapply(trees$ConditionCodes, function(x) ConditionCode %in% x))
    trees <- cbind(trees, this_code)
    names(trees)[ncol(trees)] <- paste0('ConditionCode_', ConditionCode)
}

###############################################################################
# There are multiple observations per sampling period for the same tree. This 
# should not occur except in case of remeasurement or multiple stems. Multiple 
# stems however should have unique SamplingUnitName values.
trees <- mutate(group_by(trees, sitecode, SamplingUnitName, SamplingPeriod),
                         n_obs=rep(length(Diameter), length(Diameter)),
                         obs_num=seq(1:length(Diameter)))
# Make a dataframe of trees with multiple measurements
mult_obs_trees <- select(filter(trees, n_obs > 1), ObservationDate, Diameter, POMHeight, SamplingUnitName, ConditionCodes, obs_num)
mult_obs_trees <- mult_obs_trees[order(mult_obs_trees$SamplingUnitName, mult_obs_trees$ObservationDate), ]
write.csv(mult_obs_trees, file="trees_with_multiple_obs_per_period.csv", row.names=FALSE)

mean(trees$n_obs)
table(trees$n_obs)
table(trees[trees$n_obs > 1,]$sitecode, trees[trees$n_obs > 1,]$SamplingPeriod)

###############################################################################
# Correct 999 used to code missing or dead trees in CSN per Jul 10 email from 
# Jimmy. This value codes missing or dead trees. Code as NA so these are 
# dropped from growth calculations. TODO: Need to recode these in the condition 
# code column.
trees[which((trees$sitecode == 'CSN') & (trees$Diameter == 999)), ]$Diameter <- NA

summ_stats <- summarize(group_by(trees, sitecode, SamplingPeriod),
                        dbh_mean=mean(Diameter, na.rm=TRUE),
                        dbh_min=min(Diameter, na.rm=TRUE),
                        dbh_max=max(Diameter, na.rm=TRUE),
                        dbh_sd=sd(Diameter, na.rm=TRUE),
                        POM_mean=mean(POMHeight, na.rm=TRUE),
                        POM_min=min(POMHeight, na.rm=TRUE),
                        POM_max=max(POMHeight, na.rm=TRUE),
                        POM_sd=sd(POMHeight, na.rm=TRUE),
                        newdbh_mean=mean(NewDiameter, na.rm=TRUE),
                        newdbh_min=min(NewDiameter, na.rm=TRUE),
                        newdbh_max=max(NewDiameter, na.rm=TRUE),
                        newdbh_sd=sd(NewDiameter, na.rm=TRUE),
                        newPOM_mean=mean(NewPOMHeight, na.rm=TRUE),
                        newPOM_min=min(NewPOMHeight, na.rm=TRUE),
                        newPOM_max=max(NewPOMHeight, na.rm=TRUE),
                        newPOM_sd=sd(NewPOMHeight, na.rm=TRUE))

###############################################################################
# Correct for NewDiameter issue. For now always take NewDiameter and/or 
# NewPOMHeight when available
trees$Diameter <- ifelse(is.na(trees$NewDiameter), trees$Diameter, trees$NewDiameter)
trees$POMHeight <- ifelse(is.na(trees$NewPOMHeight), trees$POMHeight, trees$NewPOMHeight)

###############################################################################
# Fix variable name starting with numeric
names(trees) <- gsub('1haPlot', 'OnehaPlot', names(trees))

# Add column with site codes
trees$Site.CodeT <- gsub("-", "", str_extract(trees$OnehaPlotNumber,"-[a-zA-Z]*-"))

# Manually clean data (prior to database cleaning) using corrected spellings from Tropicos
# Duplicate tree genera
trees$Genus[trees$Genus=="Albizia Durazz."] <- "Albizia"
trees$Genus[trees$Genus=="Allophyllus"] <- "Allophylus"
trees$Genus[trees$Genus=="Cassearea"] <- "Casearia"
trees$Genus[trees$Genus=="Cassipourea Aubl."] <- "Cassipourea"
trees$Genus[trees$Genus=="Casspourea"] <- "Cassipourea"
trees$Genus[trees$Genus=="Chrysoclamys"] <- "Chrysochlamys"
trees$Genus[trees$Genus=="Clerodendron"] <- "Clerodendrum"
trees$Genus[trees$Genus=="Corida"] <- "Cordia"
trees$Genus[trees$Genus=="Denrocnide"] <- "Dendrocnide"
trees$Genus[trees$Genus=="Drypetes Vahl"] <- "Drypetes"
trees$Genus[trees$Genus=="Dysoxyllum"] <- "Dysoxylum"
trees$Genus[trees$Genus=="Elaegia"] <- "Elaeagia"
trees$Genus[trees$Genus=="Erythroxylon"] <- "Erythroxylum"
trees$Genus[trees$Genus=="Illex"] <- "Ilex"
trees$Genus[trees$Genus=="Luehopsis"] <- "Lueheopsis"
trees$Genus[trees$Genus=="Melanochylla"] <- "Melanochyla"
trees$Genus[trees$Genus=="Neea/Guapira"] <- "Neea"
trees$Genus[trees$Genus=="Payena Lucida"] <- "Payena"
trees$Genus[trees$Genus=="Pleurothiryum"] <- "Pleurothyrium"
trees$Genus[trees$Genus=="Pleurotyrium"] <- "Pleurothyrium"
trees$Genus[trees$Genus=="Potamea"] <- "Potameia"
trees$Genus[trees$Genus=="Psychotria Mahonii"] <- "Psychotria"
trees$Genus[trees$Genus=="Querqus"] <- "Quercus"
trees$Genus[trees$Genus=="Rhodmnia"] <- "Rhodamnia"
trees$Genus[trees$Genus=="Rinoerocarpus"] <- "Rinoreocarpus"
trees$Genus[trees$Genus=="Ritchea"] <- "Ritchiea"
trees$Genus[trees$Genus=="Rytiginia"] <- "Rytigynia"
trees$Genus[trees$Genus=="Sapotaceae"] <- "Unknown"
trees$Genus[trees$Genus=="Sygygium"] <- "Syzygium"
trees$Genus[trees$Genus=="Sygyzium"] <- "Syzygium"
trees$Genus[trees$Genus=="Symplocoa"] <- "Symplocos"
trees$Genus[trees$Genus=="Tratinickia"] <- "Trattinnickia"
trees$Genus[trees$Genus=="Tremma"] <- "Trema"
trees$Genus[trees$Genus=="Turaea"] <- "Turraea"
trees$Genus[trees$Genus=="Xantophyllum"] <- "Xanthophyllum"
trees$Genus[trees$Genus=="Xymolas"] <- "Xymalos"
trees$Genus[trees$Genus=="Zanthoxylem"] <- "Zanthoxylum"
trees$Genus[trees$Genus=="Zizyphus"] <- "Ziziphus"
trees$Genus[trees$Genus=="Gymnosporia (Ex. Maytenus)"] <- "Gymnacranthera"
trees$Genus[trees$Genus=="Hensenia"] <- "Heinsenia"
trees$Genus[trees$Genus=="Mycronychia"] <- "Micronychia"
trees$Genus[trees$Genus=="Podo"] <- "Podocarpus"
trees$Genus[trees$Genus=="Polycious"] <- "Polyscias"
trees$Genus[trees$Genus=="Scheflera"] <- "Schefflera"
# Tree misspellings
trees$Genus[trees$Genus=="Caloxylon"] <- "Merrillia"
trees$Genus[trees$Genus=="Cavanalesia"] <- "Cavanillesia"
trees$Genus[trees$Genus=="Chysoceton"] <- "Unknown"
trees$Genus[trees$Genus=="Clochidion"] <- "Unknown"
trees$Genus[trees$Genus=="Cytogononia"] <- "Unknown"
trees$Genus[trees$Genus=="Eonimus"] <- "Euonymus"
trees$Genus[trees$Genus=="Escalonia"] <- "Escallonia"
trees$Genus[trees$Genus=="Euricoma"] <- "Eurycoma"
trees$Genus[trees$Genus=="Ferminiana"] <- "Unknown"
trees$Genus[trees$Genus=="Fragraea"] <- "Fagraea"
trees$Genus[trees$Genus=="Graffenriedia"] <- "Unknown"
trees$Genus[trees$Genus=="Hyeronima"] <- "Hieronyma"
trees$Genus[trees$Genus=="Hynocarpus"] <- "Hydnocarpus"
trees$Genus[trees$Genus=="Julbelniadia"] <- "Julbernardia"
trees$Genus[trees$Genus=="Lagerstromia"] <- "Lagerstroemia"
trees$Genus[trees$Genus=="Lauraceae"] <- "Unknown" #Family not genus
trees$Genus[trees$Genus=="Malanochyla"] <- "Unknown"
trees$Genus[trees$Genus=="Mdumbadumba"] <- "Unknown"
trees$Genus[trees$Genus=="Metrododorea"] <- "Unknown"
trees$Genus[trees$Genus=="Meyogine"] <- "Meiogyne"
trees$Genus[trees$Genus=="Microsdesmis"] <- "Microdesmis"
trees$Genus[trees$Genus=="Mnunganunga"] <- "Unknown"
trees$Genus[trees$Genus=="Mollotus"] <- "Mallotus"
trees$Genus[trees$Genus=="Msengela"] <- "Unknown"
trees$Genus[trees$Genus=="Muntinga"] <- "Unknown"
trees$Genus[trees$Genus=="Neouvaria"] <- "Neo-uvaria"
trees$Genus[trees$Genus=="Octodes"] <- "Unknown"
trees$Genus[trees$Genus=="Onychapetalum"] <- "Unknown"
trees$Genus[trees$Genus=="Papilionoidea"] <- "Unknown"
trees$Genus[trees$Genus=="Physocalymna"] <- "Physocalymma"
trees$Genus[trees$Genus=="Rhinorea"] <- "Unknown"
trees$Genus[trees$Genus=="Rhytigynia"] <- "Unknown"
trees$Genus[trees$Genus=="Rnodostemomodaphne"] <- "Unknown"
trees$Genus[trees$Genus=="Saccopethalum"] <- "Saccopetalum"
trees$Genus[trees$Genus=="Schweilera"] <- "Eschweilera"
trees$Genus[trees$Genus=="Staphyllea"] <- "Staphylea"
trees$Genus[trees$Genus=="Ticodendrom"] <- "Ticodendron"
trees$Genus[trees$Genus=="Vuguierenthus"] <- "Unknown"
trees$Genus[trees$Genus=="Yrianthera"] <- "Iryanthera"
# Tree families
trees$Family[trees$Family=="Acanthaceae Juss."] <- "Acanthaceae"
trees$Family[trees$Family=="Annoniaceae"] <- "Annonaceae"
trees$Family[trees$Family=="Apocynaceae Juss."] <- "Apocynaceae"
trees$Family[trees$Family=="Aquifoliaceae Bercht. & J. Presl"] <- "Aquifoliaceae"
trees$Family[trees$Family=="Asteraceae Bercht. & J. Presl"] <- "Asteraceae"
trees$Family[trees$Family=="Berseraceae"] <- "Burseraceae"
trees$Family[trees$Family=="Capparaceae Juss."] <- "Capparaceae"
trees$Family[trees$Family=="Capparidaceae"] <- "Capparaceae"
trees$Family[trees$Family=="Caryocaceae"] <- "Caryocaraceae"
trees$Family[trees$Family=="Celastomataceae"] <- "Melastomataceae"
trees$Family[trees$Family=="Chlorantaceae"] <- "Chloranthaceae"
trees$Family[trees$Family=="Chrysobalanaceae R. Br."] <- "Chrysobalanaceae"
trees$Family[trees$Family=="Cluciaceae"] <- "Clusiaceae"
trees$Family[trees$Family=="Cornaceae Bercht. & J. Presl"] <- "Cornaceae"
trees$Family[trees$Family=="Eleaocarpaceae"] <- "Elaeocarpaceae"
trees$Family[trees$Family=="Euphorbiacae"] <- "Euphorbiaceae"
trees$Family[trees$Family=="Euphorbiacaea"] <- "Euphorbiaceae"
trees$Family[trees$Family=="Euphorbiaceae Juss."] <- "Euphorbiaceae"
trees$Family[trees$Family=="Fabaceae-Caesalpinioideae"] <- "Fabaceae"
trees$Family[trees$Family=="Fabaceae Lindl."] <- "Fabaceae"
trees$Family[trees$Family=="Fabaceae-Mimosoideae"] <- "Fabaceae"
trees$Family[trees$Family=="Fabaceae-Papilionoideae"] <- "Fabaceae"
trees$Family[trees$Family=="Familia Incogn."] <- "Unknown"
trees$Family[trees$Family=="Hippocastenacea"] <- "Hippocastanaceae"
trees$Family[trees$Family=="Labiateae"] <- "Lamiaceae"
trees$Family[trees$Family=="Lacistemaceae"] <- "Lacistemataceae"
trees$Family[trees$Family=="Malvaceae Juss."] <- "Malvaceae"
trees$Family[trees$Family=="Meliaceae Juss."] <- "Meliaceae"
trees$Family[trees$Family=="Melianthaceae Horan."] <- "Melianthaceae"
trees$Family[trees$Family=="Mirsinaceae"] <- "Myrsinaceae"
trees$Family[trees$Family=="Monimiaceae Juss."] <- "Monimiaceae"
trees$Family[trees$Family=="Moraceae Gaudich."] <- "Moraceae"
trees$Family[trees$Family=="Myrtaceae Juss."] <- "Myrtaceae"
trees$Family[trees$Family=="Olaceae"] <- "Olacaceae"
trees$Family[trees$Family=="Olacaceae R. Br."] <- "Olacaceae"
trees$Family[trees$Family=="Oleaceae Hoffmanns. & Link"] <- "Oleaceae"
trees$Family[trees$Family=="Phyllantaceae"] <- "Phyllanthaceae"
trees$Family[trees$Family=="Phytollacaceae"] <- "Phytolaccaceae"
trees$Family[trees$Family=="Podocarpaceae Endl."] <- "Podocarpaceae"
trees$Family[trees$Family=="Primulaceae Batsch Ex Borkh."] <- "Primulaceae"
trees$Family[trees$Family=="Putranjivaceae Meisn."] <- "Putranjivaceae"
trees$Family[trees$Family=="Rhamnaceae Juss."] <- "Rhamnaceae"
trees$Family[trees$Family=="Rhizophoraceae Pers."] <- "Rhizophoraceae"
trees$Family[trees$Family=="Rosaceae Juss"] <- "Rosaceae"
trees$Family[trees$Family=="Rosaceae Juss."] <- "Rosaceae"
trees$Family[trees$Family=="Rubiaceae Juss."] <- "Rubiaceae"
trees$Family[trees$Family=="Rutaceae Juss."] <- "Rutaceae"
trees$Family[trees$Family=="Sapindaceae Juss."] <- "Sapindaceae"
trees$Family[trees$Family=="Sapotaceae Juss."] <- "Sapotaceae"
trees$Family[trees$Family=="Staphylaceae"] <- "Staphyleaceae"
trees$Family[trees$Family=="Torrecilliaceae"] <- "Unknown"
trees$Family[trees$Family=="Urticaceae Juss."] <- "Urticaceae"

# Add decimal KRP plot with inflated diameters until issue is resolved in the database
trees$Diameter[trees$OnehaPlotNumber=="VG-KRP-1" & trees$SamplingPeriod=="2011.01"] <- trees$Diameter[trees$OnehaPlotNumber=="VG-KRP-1"& trees$SamplingPeriod=="2011.01"]/10

# Add decimal to 2012 BCI stems with inflated diameters (all >100) until issue is resolved in the database

# Add decimal to 6 stems in plot VG-COU-5 that have inflated diameters until issue is resolved in the database
trees$Diameter[trees$OnehaPlotNumber=="VG-COU-5" & trees$Diameter==392] <- 39.2
trees$Diameter[trees$OnehaPlotNumber=="VG-COU-5" & trees$Diameter==525] <- 52.5
trees$Diameter[trees$OnehaPlotNumber=="VG-COU-5" & trees$Diameter==564] <- 56.4
trees$Diameter[trees$OnehaPlotNumber=="VG-COU-5" & trees$Diameter==603] <- 60.3
trees$Diameter[trees$OnehaPlotNumber=="VG-COU-5" & trees$Diameter==723] <- 72.3
trees$Diameter[trees$OnehaPlotNumber=="VG-COU-5" & trees$Diameter==1120] <- 112.0
# Add decimal to outlier diameter in plot VG-YAS-1
trees$Diameter[trees$OnehaPlotNumber=="VG-YAS-1" & trees$Diameter==420] <- 42.0

# Check to be sure that no other years had outlier values for these stems
trees$Diameter[trees$OnehaPlotNumber=="VG-COU-5" & trees$Diameter>300]

###############################################################################
# Exclude stems under 10 cm dbh
table(trees$Diameter <= 10)
trees <- filter(trees, Diameter >= 10)

save(trees, file='trees_clean.RData')
