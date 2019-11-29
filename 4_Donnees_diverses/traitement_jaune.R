library(XLConnect)
library(plyr)

### Nomenclatures
nomCol2015_2017 = c('phase', 'typeMission', 'mission', 'codeProgramme', 'programme',
                    'codeAction', 'action')

nom2015 = read.csv2('Nomenclatures/PLF2015-Nomenclature_MPA.csv', stringsAsFactors = F)
colnames(nom2015) = nomCol2015_2017
nom2015$phase = "2015"

nom2016 = read.csv2('Nomenclatures/PLF2016-Nomenclature_MPA.csv', stringsAsFactors = F)
colnames(nom2016) = nomCol2015_2017
nom2016$phase = "2016"

nom2017 = read.csv2('Nomenclatures/PLF2017-Nomenclature_MPA.csv', stringsAsFactors = F)
colnames(nom2017) = nomCol2015_2017
nom2017$phase = "2017"

classeur = loadWorkbook('Nomenclatures/NOM_2018_Mission_Programmes_Ministeres.xls', create = F)
nom2018 = readWorksheet(classeur, sheet = 2)
nom2018$ANNEE = NULL
colnames(nom2018) = c('typeMission', "codeMission", 
                      'mission', 'codeProgramme', 'programme', "codeMinistere", "ministere")
nom2018$phase = "2018"

nom2019 = read.csv2('Nomenclatures/plf2019-nomenclature-mpa.csv', stringsAsFactors = F)
colnames(nom2019) = c('phase', 'typeMission', 'codeMission', 'mission', 'codeProgramme', 'programme',
                      'codeAction', 'action', 'codeMinistere', 'ministere')
nom2019$phase = "2019"

## 2020, beaucoup de retraitement
nom2020 = read.csv2('Nomenclatures/PLF_2020_Nomenclature.csv', stringsAsFactors = F)
nom2020$Libelle.abrege = NULL
nom2020$code = gsub('^([0]*)', '', nom2020$code)

missions2020 = subset(nom2020, Type.ligne == 'MSN', select = c('Type.Budget', 'code', 'Libelle'))
colnames(missions2020) = c('typeBudget', 'codeMission', 'mission')
head(missions2020)

programmes2020 = subset(nom2020, Type.ligne == 'PGM', c('code', 'Mission', 'Ministere', 'Libelle'))
colnames(programmes2020) = c('codeProgramme', 'codeMission', 'codeMinistere', 'programme')
head(programmes2020,2)

actions2020 = subset(nom2020, Type.ligne == 'ACT', select = c('code', 'Libelle'))
colnames(actions2020) = c('codeAction', 'action')
actions2020$codeProgramme = gsub('([0-9]*)-([0-9]*)', '\\1', actions2020$codeAction)
actions2020$codeAction = gsub('([0-9]*)-([0-9]*)', '\\2', actions2020$codeAction)

ministeres2020 = subset(nom2020, Type.ligne == "MIN", c('code', 'Libelle'))
colnames(ministeres2020) = c('codeMinistere', "ministere")

nom2020_bis = merge(missions2020, programmes2020, by = ('codeMission'), all = TRUE)
nom2020_bis = merge(nom2020_bis, actions2020, by = ('codeProgramme'), all = TRUE)
nom2020_bis = merge(nom2020_bis, ministeres2020, by = ('codeMinistere'), all = TRUE)

nom2020_bis$phase = 2020

nomenclatures = list(nom2015, nom2016, nom2017, nom2018, nom2019, nom2020_bis)
nomenclatures = rbind.fill(nomenclatures)



### PLF
plf2015 = read.csv2('Jaunes/plf-2015-jaune-effort-financier-de-letat-en-faveur-des-associations-.csv',
                   encoding = "UTF-8", stringsAsFactors = FALSE)
colnames(plf2015) = c("millesime", "typeBudget", "ministere", "codeProgramme",
                      "association", "SIREN", "departement", "ville", "montant",
                      "objet", "convention", "elementsConvention", "mission")
plf2015$millesime = "2015"

plf2016 = read.csv2('Jaunes/plf-2016-jaune-effort-financier-de-letat-en-faveur-des-associations-.csv',
                   encoding = "UTF-8", stringsAsFactors = FALSE)
colnames(plf2016) = c("millesime", "typeBudget", "ministere", "codeProgramme",
            "association", "SIREN", "departement", "ville", "montant",
            "objet", "convention", "elementsConvention", "mission")
plf2016$millesime = "2016"

plf2017 = read.csv2('Jaunes/projet-de-loi-de-finances-pour-2017-plf-2017-jaune-effort-financier-de-letat-en-.csv',
                   encoding = "UTF-8", stringsAsFactors = FALSE)
colnames(plf2017) = c("millesime", "typeBudget", "ministere", "codeProgramme",
             "association", "SIREN", "departement", "ville", "montant",
             "mission")
plf2017$millesime = "2017"

# Plus de ministère, mais présent dans le référentiel.
plf2018 = read.csv2('Jaunes/projet-de-loi-de-finances-pour-2018-plf-2018-donnees-de-lannexe-jaune-effort-fin.csv',
                   encoding = "UTF-8", stringsAsFactors = FALSE)
colnames(plf2018) = c("codeProgramme", "SIREN", "NIC", "association", "montant", 
             "objet", "parlementaire", "reserve", "convention",
             "departement", "commune", "ville", "nomenclatureJuridique",
             "codeNAF", "situationSIRENE", "RNA")
plf2018$millesime = "2018"

############ 2019
plf2019 = read.csv2('Jaunes/projet-de-loi-de-finances-pour-2019-plf-2019-donnees-de-lannexe-jaune-effort-fin.csv',
                   encoding = "Latin-1", stringsAsFactors = FALSE)
colnames(plf2019) = c("codeProgramme", "SIREN", "NIC", "association", "montant", 
             "objet", "parlementaire", "reserve", "convention", "SIRENE01",
             "existeAu01", "SIRENE12", "existeAu12", 
             "departement", "commune", "ville", "codeNAF", "nomenclatureJuridique")
plf2019$millesime = "2019"


############ 2020
plf2020 = read.csv2('Jaunes/credits_aux_associations_2018.csv', skip = 2,
                    encoding = "Latin-1", stringsAsFactors = FALSE)
colnames(plf2020) = c("codeProgramme", "SIREN", "NIC", "association", "montant", 
                      "objet", "convention", "SIRENE01",
                      "existeAu01", "SIRENE12", "existeAu12", 
                      "departement", "commune", "ville", "codeNAF", "nomenclatureJuridique")
plf2020$millesime = "2020"


# Concaténation
plf = list(plf2015, plf2016, plf2017, plf2018, plf2019, plf2020)
plf = rbind.fill(plf)
rm(list = c('plf2015', 'plf2016', 'plf2017', 'plf2018', 'plf2019', 'plf2020'))
plf$codeProgramme = gsub('(.*) - .*', '\\1', plf$codeProgramme)
plf$montant = as.numeric(gsub(',', '.', gsub(' ', '', plf$montant)))


write.csv2(plf, 'nomenclature_20152020.csv', row.names = FALSE)
write.csv2(plf, 'plfJauneAsso_20152020.csv', row.names = FALSE)

# Le programme 163 contient entre autres le FDVA
programme163 = subset(plf, codeProgramme == "163")
write.csv2(programme163, 'plfJauneAsso_pgm163_20152020.csv', row.names = FALSE)
