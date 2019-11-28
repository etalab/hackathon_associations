# Préparation des données du défi Pays de Loire

library(XLConnect)
library(plyr)
options(scipen=999)

# Loire Atlantique - 44
data_loire_atlantique = read.csv2('44-Loire-Atlantique/224400028_subventions-aux-associations-du-departement-de-loire-atlantique.csv',
                                  stringsAsFactors = F, encoding = "UTF-8")
colnames(data_loire_atlantique) = c('nomAttribuant', 'idAttribuant', 'dateConvention',
                                    'referenceDecision', 'nomBeneficiaire', 'idBeneficiaire',
                                    'objet', 'montant', 'nature', 'conditionsVersement', 
                                    'datesPeriodeVersement', 'idRAE', 'notificationUE',
                                    'pourcentageSubvention', 'montantPret', 'montantGarantie',
                                    'detailAvantagesNature', 'estimationAvantageNatures')
data_loire_atlantique$dateConvention = as.character(data_loire_atlantique$dateConvention)

head(data_loire_atlantique)

# Maine et Loire - 49
data_maine_loire = read.csv2('49-Maine-et-Loire/224900019_conventions-de-subventions-du-departement-de-maine-et-loire.csv',
                            stringsAsFactors = F, encoding = "UTF-8")
head(data_maine_loire)
data_maine_loire$Colonne1 = NULL

# Mayenne - 53 
wb_mayenne_2018 = loadWorkbook('53-Mayenne/data.gouv-cd53-21-12-2018.xlsx')
data_mayenne_2018 = readWorksheet(wb_mayenne_2018, 1)
head(data_mayenne_2018)

wb_mayenne_2019 = loadWorkbook('53-Mayenne/data.gouv-cd53-14-10-2019-.xlsx')
data_mayenne_2019 = readWorksheet(wb_mayenne_2019, 1)
head(data_mayenne_2019)

colnames(data_mayenne_2019) = colnames(data_mayenne_2018)
data_mayenne = rbind.fill(data_mayenne_2018, data_mayenne_2019)
data_mayenne$dateConvention = as.character(data_mayenne$dateConvention)

# Sarthe - 72
wb_sarthe = loadWorkbook('72-Sarthe/subvention_progos_2018.xls')
data_sarthe = readWorksheet(wb_sarthe, 1, startRow = 4)
head(data_sarthe)
colnames(data_sarthe) = c('dateConvention', 'referenceDecision', 'nomBeneficiaire', 
                          'idBeneficiaire', 'objet', 'montant', 'nature')
data_sarthe$nomAttribuant = "Conseil dépatemental de la Sarthe"
data_sarthe$idAttribuant = "227 200 029"
data_sarthe$dateConvention = as.character(data_sarthe$dateConvention)

# Vendée - 85
fichiers_vendee = list.files('85-Vendee', full.names = T)
data_vendee = list()
for (fichier in fichiers_vendee){
  chaine = readLines(fichier)
  chaine = gsub(', ([a-z]|[0-9])', ' \\1', chaine)
  
  chaine = gsub(';', '', chaine)
  chaine = gsub('"', '', chaine)
  # Encodage mixte dans certains fichier, on corrige à la main
  chaine = gsub('Ã©', 'é', chaine)
  chaine = gsub('Ã¨', 'è', chaine)
  chaine = gsub('Ã', 'à', chaine)
  chaine = gsub('Â ', ' ', chaine) # espace insécable entre les chiffres
  chaine = gsub('à‰', 'E', chaine)
  chaine = gsub('àª', 'ê', chaine)
  chaine = gsub('([0-9]{1}),(00)', '\\1.00', chaine)
  chaine = gsub('([0-9]{1}),([0-9]{2}),', '\\1.\\2,', chaine)
  
  subventions = read.csv2(text = chaine, sep = ",",quote = "", stringsAsFactors = FALSE)
  colnames(subventions) = gsub('IdRAE', 'idRAE', colnames(subventions))
  
  data_vendee[[length(data_vendee)+1]] = subventions
  
  # print(head(subventions))
  # print('*********')
  
}
data_vendee = do.call(rbind.fill, data_vendee)
data_vendee$dateConvention = as.character(data_vendee$dateConvention)


#######################################################
# On checke que les colonnes portent les "bons" titres
colnames(data_loire_atlantique)
colnames(data_maine_loire)
colnames(data_mayenne)
colnames(data_sarthe)
colnames(data_vendee)

data_dep_pdl = rbind.fill(data_loire_atlantique, data_maine_loire,
                                data_mayenne, data_sarthe, data_vendee)
data_dep_pdl[, ] <- lapply(data_dep_pdl[, ], as.character)


data_dep_pdl$dateConvention =  gsub(' 00\\:00\\:00', '', data_dep_pdl$dateConvention)
data_dep_pdl$datesPeriodeVersement =  gsub(' 00\\:00\\:00', '', data_dep_pdl$datesPeriodeVersement)
data_dep_pdl$dateDeuxiemeVersement =  gsub(' 00\\:00\\:00', '', data_dep_pdl$dateDeuxiemeVersement)
data_dep_pdl$dateSoldeVersement =  gsub(' 00\\:00\\:00', '', data_dep_pdl$dateSoldeVersement)
data_dep_pdl$idAttribuant = gsub(" |'", "", data_dep_pdl$idAttribuant)
data_dep_pdl$idBeneficiaire = gsub(" |'", "", data_dep_pdl$idBeneficiaire)
data_dep_pdl$montant = gsub("[^0-9.]*", "", data_dep_pdl$montant)


#Sauvegarde du fichier
data_dep_pdl = subset(data_dep_pdl, !is.na(idAttribuant))
write.csv2(data_dep_pdl, "donnees_tousDep_pdl.csv", row.names = FALSE,quote = TRUE
)

