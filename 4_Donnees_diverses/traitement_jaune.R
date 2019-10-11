library(XLConnect)
library(plyr)

### Nomenclatures
nom2015 = read.csv2('Nomenclatures/PLF2015-Nomenclature_MPA.csv')
nom2015 = unique(nom2015[, c('Type.de.Mission', 'Mission', 'Code.Programme', 'Programme')])
colnames(nom2015) = c('Type_mission', 'Mission', 'Code_programme', 'Programme')
nom2015$Millesime = "2015"

nom2016 = read.csv2('Nomenclatures/PLF2016-Nomenclature_MPA.csv')
nom2016 = unique(nom2016[, c('Type.de.mission', 'Mission', 'Code.Programme', 'Programme')])
colnames(nom2016) = c('Type_mission', 'Mission', 'Code_programme', 'Programme')
nom2016$Millesime = "2016"

nom2017 = read.csv2('Nomenclatures/PLF2017-Nomenclature_MPA.csv')
nom2017 = unique(nom2017[, c('Type.de.mission', 'Mission', 'Code.Programme', 'Programme')])
colnames(nom2017) = c('Type_mission', 'Mission', 'Code_programme', 'Programme')
nom2017$Millesime = "2017"

classeur = loadWorkbook('Nomenclatures/NOM_2018_Mission_Programmes_Ministeres.xls', create = F)
nom2018 = readWorksheet(classeur, sheet = 2)
nom2018$ANNEE = NULL
colnames(nom2018) = c('Type_mission', "Code_mission", 
                      'Mission', 'Code_programme', 'Programme', "Code_ministere", "Ministere")
nom2018$Millesime = "2018"

classeur = loadWorkbook('Nomenclatures/NOM_2019_Mission_Programmes_Ministeres.xls', create = F)
nom2019 = readWorksheet(classeur, sheet = 2)
nom2019$ANNEE = NULL
colnames(nom2019) = c('Type_mission', "Code_mission", 
                      'Mission', 'Code_programme', 'Programme', "Code_ministere", "Ministere")
nom2019$Millesime = "2019"

nomenclatures = list(nom2015, nom2016, nom2017, nom2018, nom2019)
nomenclatures = rbind.fill(nomenclatures)

rm(list = c('nom2015', 'nom2016', 'nom2017', 'nom2018', 'nom2019', 'classeur') )

### Nomenclatures

plf2015 = read.csv2('Jaunes/plf-2015-jaune-effort-financier-de-letat-en-faveur-des-associations-.csv',
                   encoding = "UTF-8", stringsAsFactors = FALSE)
colnames(plf2015) = c("Millesime", "Type_budget", "Ministere", "Code_programme",
            "Association", "SIREN", "Departement", "Ville", "Montant",
            "Objet", "Convention", "Elements_convention", "Mission")
plf2015$Millesime = "2015"
plf2015$Code_programme = gsub('(.*) - .*', '\\1', plf2015$Code_programme)
plf2015 = plf2015[, c('Millesime', "Ministere", "Code_programme", 
                      "Association", "SIREN", "Departement", "Ville", "Montant",
                      "Objet", "Convention", "Elements_convention")]


plf2016 = read.csv2('Jaunes/plf-2016-jaune-effort-financier-de-letat-en-faveur-des-associations-.csv',
                   encoding = "UTF-8", stringsAsFactors = FALSE)
colnames(plf2016) = c("Millesime", "Type_budget", "Ministere", "Code_programme",
            "Association", "SIREN", "Departement", "Ville", "Montant",
            "Objet", "Convention", "Elements_convention", "Mission")
plf2016$Millesime = "2016"
plf2016$Code_programme = gsub('(.*) - .*', '\\1', plf2016$Code_programme)
plf2016 = plf2016[, c('Millesime', "Ministere", "Code_programme", 
                      "Association", "SIREN", "Departement", "Ville", "Montant",
                      "Objet", "Convention", "Elements_convention")]

plf2017 = read.csv2('Jaunes/projet-de-loi-de-finances-pour-2017-plf-2017-jaune-effort-financier-de-letat-en-.csv',
                   encoding = "UTF-8", stringsAsFactors = FALSE)
colnames(plf2017) = c("Millesime", "Type_budget", "Ministere", "Code_programme",
             "Association", "SIREN", "Departement", "Ville", "Montant",
             "Mission")
plf2017$Millesime = "2017"
plf2017$Mission = NULL

# Plus de ministère, mais présent dans le référentiel.
plf2018 = read.csv2('Jaunes/projet-de-loi-de-finances-pour-2018-plf-2018-donnees-de-lannexe-jaune-effort-fin.csv',
                   encoding = "UTF-8", stringsAsFactors = FALSE)
colnames(plf2018) = c("Code_programme", "SIREN", "NIC", "Association", "Montant", 
             "Objet", "Parlementaire", "Reserve", "Convention",
             "Departement", "Commune", "Ville", "Nomenclature_juridique",
             "Code_NAF", "Situation_SIRENE", "RNA")
plf2018$Situation_SIRENE = NULL
plf2018$Millesime = "2018"

############ 2019
plf2019 = read.csv2('Jaunes/projet-de-loi-de-finances-pour-2019-plf-2019-donnees-de-lannexe-jaune-effort-fin.csv',
                   encoding = "Latin-1", stringsAsFactors = FALSE)
colnames(plf2019) = c("Code_programme", "SIREN", "NIC", "Association", "Montant", 
             "Objet", "Parlementaire", "Reserve", "Convention", "SIRENE_01",
             "existe_au_01", "SIRENE_12", "existe_au_12", 
             "Departement", "Commune", "Ville", "Code_NAF", "Nomenclature_juridique")
plf2019$Millesime = "2019"
plf2019$Montant = as.numeric(gsub(',', '.', gsub(' ', '', plf2019$Montant)))

col_retirer = c('SIRENE_01', 'existe_au_01', "SIRENE_12", 'existe_au_12', "Situation_SIRENE")
for (col in col_retirer){
  plf2019[, col] = NULL
}
head(plf2019)



plf = list(plf2015, plf2016, plf2017, plf2018, plf2019)
plf = rbind.fill(plf)
rm(list = c('plf2015', 'plf2016', 'plf2017', 'plf2018', 'plf2019'))



# Jointure
plf2 = merge(plf, nomenclatures, by = c('Millesime', 'Code_programme'), all.x  = TRUE)
plf2$Ministere = ifelse(is.na(plf2$Ministere.x), plf2$Ministere.y, plf2$Ministere.x)
plf2$Ministere.x = NULL
plf2$Ministere.y = NULL
