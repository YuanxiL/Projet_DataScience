###### Test Allianz Data Scientist ######

### 1. PREPARATION ###
# Sys.setlocale("LC_ALL", 'fr_Fr')
# Set working directory
setwd("~/Desktop/allianz/data")

### 2. IMPORT DES DONNEES ###
## 2.1 caracteristiques 2005-2016
# Lire tous les 'caracteristiques*.csv' files
list_file_caracteristiques<-list.files(pattern=glob2rx("caracteristiques*.csv"))
list_file_caracteristiques

# Lire les 10 premiers lignes des données de l'annee 2014
# On trouve qu'un bug de 'Num_Acc', car gd nombre (integer).
# Les accents français ne sont pas bien affichés.
first_ten_lines <- read.csv(list_file_caracteristiques[10], nrows = 10)
first_ten_lines 
# Num_Acc an mois jour hrmn lum agg int atm col com                      adr gps     lat   long dep
# 1  2.014e+11 14    5    7 2015   1   2   1   1   3  11             route de don   M       0      0 590
# 2  2.014e+11 14    5   31  430   1   2   1   1   6  11         106 ROUTE DE DON   M       0      0 590
# 3  2.014e+11 14    8   23 1800   1   2   9   1   3  52   75 bis rue jean jaures   M       0      0 590
# 4  2.014e+11 14    6   12 1700   1   2   1   1   1  25 rue des Sablonnieres D41   M       0      0 590
# 5  2.014e+11 14    6   23  500   2   1   1   1   1  25                            M       0      0 590
# 6  2.014e+11 14   10   11  630   5   1   6   2   2 550                            M       0      0 590
# 7  2.014e+11 14   10   11 1445   1   2   9   1   7 281         Chemin des Loups   M       0      0 590
# 8  2.014e+11 14   10   16 1200   1   2   4   1   6  51      Rue gabriel P\xe9ri   M 5053223 281490 590
# 9  2.014e+11 14   11    2 1100   1   1   1   1   1 257                            M       0      0 590
# 10 2.014e+11 14   11   30 1930   5   2   1   8   6 524            RUE DE VERDUN   M       0      0 590

## Data.table DT
library(DT)
colnames(first_ten_lines) = paste('<span style="color:red">', colnames(first_ten_lines), '</span>', sep = "")
datatable(first_ten_lines, escape = FALSE)

# Maximum integer pour R: Machine$integer.max = 2147483647
# Num_Acc, ex '201400000001' > max
# Il est suffit de définir la classe de 'Num_Acc'(id) comme un caractère au lieu d'integer.
# On observe que tous les accents ne sont pas correctement affichés.

codepages <- iconvlist() # Test le codage de R
# Le codage européen est ISO_8859-1.

first_ten_lines_caracteristiques <- read.csv(list_file_caracteristiques[10], nrows = 10, fileEncoding = 'ISO_8859-1')
first_ten_lines_caracteristiques

## DT
colnames(first_ten_lines_caracteristiques) = paste('<span style="color:red">', colnames(first_ten_lines_caracteristiques), '</span>', sep = "")
datatable(first_ten_lines_caracteristiques, escape = FALSE)

# Num_Acc an mois jour hrmn lum agg int atm col com                      adr gps     lat   long dep
# 1  2.014e+11 14    5    7 2015   1   2   1   1   3  11             route de don   M       0      0 590
# 2  2.014e+11 14    5   31  430   1   2   1   1   6  11         106 ROUTE DE DON   M       0      0 590
# 3  2.014e+11 14    8   23 1800   1   2   9   1   3  52   75 bis rue jean jaures   M       0      0 590
# 4  2.014e+11 14    6   12 1700   1   2   1   1   1  25 rue des Sablonnieres D41   M       0      0 590
# 5  2.014e+11 14    6   23  500   2   1   1   1   1  25                            M       0      0 590
# 6  2.014e+11 14   10   11  630   5   1   6   2   2 550                            M       0      0 590
# 7  2.014e+11 14   10   11 1445   1   2   9   1   7 281         Chemin des Loups   M       0      0 590
# 8  2.014e+11 14   10   16 1200   1   2   4   1   6  51         Rue gabriel Péri   M 5053223 281490 590
# 9  2.014e+11 14   11    2 1100   1   1   1   1   1 257                            M       0      0 590
# 10 2.014e+11 14   11   30 1930   5   2   1   8   6 524            RUE DE VERDUN   M       0      0 590

# L'adresse est correctement affichée.

# Selon la Description_des_bases_de_donnees_ONISR_-Annees_2005_a_2016,
# Définir la classe de chaque variable (integer, caractere ou facteur).

# Ouvrir la liste de 'caracteristiques*.csv',
# Le séparateur de '2009' est tab, les autres sont comma.
# Test caracteristiques*.csv, on trouve les erreurs.   
# caracteristiques_2005-2008
data_caracteristiques_part1 <- do.call(rbind, lapply(list_file_caracteristiques[c(1,2,3,4)], function(x) cbind(read.csv(x, colClasses = c(Num_Acc="character", lum="factor", dep="factor", com="factor", agg="factor", int="factor", atm="factor", col="factor", adr="character", gps="factor", lat="integer", long="integer"), fileEncoding = "ISO_8859-1", na.strings=c("NA","NaN", "", "0")))))
# caracteristiques_2009
data_caracteristiques_part2 <- read.csv(list_file_caracteristiques[5], colClasses = c(Num_Acc="character", lum="factor", dep="factor", com="factor", agg="factor", int="factor", atm="factor", col="factor", adr="character", gps="factor", lat="integer", long="integer"), fileEncoding = "ISO_8859-1", sep = "\t", na.strings=c("NA","NaN", "", "0")) # Windows-1252
# caracteristiques_2010-2016
data_caracteristiques_part3 <- do.call(rbind, lapply(list_file_caracteristiques[c(6,7,8,9,10,11,12)], function(x) cbind(read.csv(x, colClasses = c(Num_Acc="character", lum="factor", dep="factor", com="factor", agg="factor", int="factor", atm="factor", col="factor", adr="character", gps="factor", lat="integer", long="integer"), fileEncoding = "ISO_8859-1", na.strings=c("NA","NaN", "", "0")))))
data_caracteristiques <- rbind(data_caracteristiques_part1,data_caracteristiques_part2,data_caracteristiques_part3)
# Erreur: '-' n'est pas un integer.
# '61285E7' n'est pas un integer pour longitude.

# Solution:
# On considère long comme caractère, transformer long 'as.integer', obtenir NA pour cette observation.
# Boucle d'import 
data_caracteristiques_part1 <- do.call(rbind, lapply(list_file_caracteristiques[c(1,2,3,4)], function(x) cbind(read.csv(x, colClasses = c(Num_Acc="character", lum="factor", dep="factor", com="factor", agg="factor", int="factor", atm="factor", col="factor", adr="character", gps="factor", lat="integer", long="character"), fileEncoding = "ISO_8859-1", na.strings=c("NA","NaN", "", "0")))))
data_caracteristiques_part2 <- read.csv(list_file_caracteristiques[5], colClasses = c(Num_Acc="character", lum="factor", dep="factor", com="factor", agg="factor", int="factor", atm="factor", col="factor", adr="character", gps="factor", lat="integer", long="character"), fileEncoding = "ISO_8859-1", sep = "\t", na.strings=c("NA","NaN", "", "0")) # Windows-1252
data_caracteristiques_part3 <- do.call(rbind, lapply(list_file_caracteristiques[c(6,7,8,9,10,11,12)], function(x) cbind(read.csv(x, colClasses = c(Num_Acc="character", lum="factor", dep="factor", com="factor", agg="factor", int="factor", atm="factor", col="factor", adr="character", gps="factor", lat="integer", long="character"), fileEncoding = "ISO_8859-1", na.strings=c("NA","NaN", "", "0")))))
data_caracteristiques <- rbind(data_caracteristiques_part1,data_caracteristiques_part2,data_caracteristiques_part3)
data_caracteristiques$long <- as.integer(data_caracteristiques$long)
View(data_caracteristiques)

## DT
colnames(data_caracteristiques) = paste('<span style="color:red">', colnames(data_caracteristiques), '</span>', sep = "")
datatable(head(data_caracteristiques, 50), escape = FALSE)

## 2.2 Lieux 2005-2016
# De même façon pour la suite:

# Lire tous les  lieux*.csv fichiers
list_file_lieux<-list.files(pattern=glob2rx("lieux*.csv"))
list_file_lieux

# Les 10 premiers lignes:
first_ten_lines_lieux <- read.csv(list_file_lieux[10], nrows = 10)
first_ten_lines_lieux
# Num_Acc catr voie v1 v2 circ nbv pr pr1 vosp prof plan lartpc larrout surf infra situ env1
# 1  2.014e+11    3   41 NA NA    2   0  0   0    0    1    1      0      60    1     0    1    0
# 2  2.014e+11    3   41 NA NA    2   2  0   0    0    1    2      0      62    1     0    4   99
# 3  2.014e+11    3   39 NA NA    2   0  0   0    0    1    1      0      60    1     0    1   99
# 4  2.014e+11    3   41 NA NA    2   2  0 800    0    0    1      0      60    1     0    1   99
# 5  2.014e+11    3  141 NA NA    2   0  0   0    0    0    2      0       0    2     0    0    0
# 6  2.014e+11    2   47 NA NA    3   2  0   0    0    0    1      0       0    2     0    1   99
# 7  2.014e+11    4    0 NA NA    2   1  0   0    0    0    2      0       0    6     0    1   99
# 8  2.014e+11    3  145 NA NA    2   2  0 281    0    1    3      0       0    1     5    1   99
# 9  2.014e+11    3  141 NA NA    2   2  0   0    0    1    1      0       0    1     0    1    0
# 10 2.014e+11    4    0 NA NA    2   2  0   0    0    1    4      0       0    2     0    3   99


## DT les 10 premiers lignes
colnames(first_ten_lines_lieux) = paste('<span style="color:red">', colnames(first_ten_lines_lieux), '</span>', sep = "")
datatable(first_ten_lines_lieux, escape = FALSE)

# Test lieux*.csv
# Facteur = 0, => valeur manquante (NA).
# Boucle d'import
data_lieux <- do.call(rbind, lapply(list_file_lieux, function(x) cbind(read.csv(x, colClasses = c(Num_Acc="character", catr="factor", voie="character", v1="integer", v2="factor", circ="factor", nbv="integer", vosp="factor", prof="factor", plan="factor", surf="factor", infra="factor", situ="factor"), fileEncoding = "ISO_8859-1", na.strings=c("NA","NaN", "", "0", 0)))))

# DT les 50 premiers lignes
colnames(data_lieux) = paste('<span style="color:red">', colnames(data_lieux), '</span>', sep = "")
datatable(head(data_lieux, 50), escape = FALSE)


## 2.3 Vehicules 2005-2016
# Lire vehicules*.csv files
list_file_vehicules<-list.files(pattern=glob2rx("vehicules*.csv"))
list_file_vehicules

# Les 10 premiers lignes
first_10_lines_vehicules <- read.csv(list_file_vehicules[10], nrows = 10)
first_10_lines_vehicules
# Num_Acc senc catv occutc obs obsm choc manv num_veh
# 1   2.014e+11    0   33      0   0    2    1    1     A01
# 2   2.014e+11    0    7      0   0    0    6   15     B02
# 3   2.014e+11    0    7      0   1    0    7   13     A01
# 4   2.014e+11    0    2      0   0    2    1    1     A01
# 5   2.014e+11    0    7      0   0    2    7   15     B02
# 6   2.014e+11    0    7      0   0    2    1   13     A01
# 7   2.014e+11    0   38      0   0    2    1    0     B02
# 8   2.014e+11    0    7      0   0    2    2   13     B02
# 9   2.014e+11    0    7      0   0    2    2    1     A01
# 10  2.014e+11    0   17      0   0    0    5    1     B02

## DT
colnames(first_10_lines_vehicules) = paste('<span style="color:red">', colnames(first_10_lines_vehicules), '</span>', sep = "")
datatable(first_10_lines_vehicules, escape = FALSE)

# Test vehicules*.csv
# Boucle d'import
data_vehicules <- do.call(rbind, lapply(list_file_vehicules, function(x) cbind(read.csv(x, colClasses = c(Num_Acc="character", senc="factor", catv="factor", obs="factor", obsm="factor", choc="factor", manv="factor"), fileEncoding = "ISO_8859-1", na.strings=c("NA","NaN", "")))))

colnames(data_vehicules) = paste('<span style="color:red">', colnames(data_vehicules), '</span>', sep = "")
datatable(head(data_vehicules, 50), escape = FALSE)

## 2.4 Usagers 2005-2016
#  Lire tous les  usagers*.csv fichiers
list_file_usagers<-list.files(pattern=glob2rx("usagers*.csv"))
list_file_usagers

# Les 10 premiers lignes
first_10_lines_usagers <- read.csv(list_file_usagers[10], nrows = 10)
first_10_lines_usagers
# Num_Acc place catu grav sexe trajet secu locp actp etatp an_nais num_veh
# 1  2.014e+11     1    1    3    1      5   21    0    0     0    1971     A01
# 2  2.014e+11     1    1    1    1      5   11    0    0     0    1992     B02
# 3  2.014e+11     1    1    4    1      5   11    0    0     0    1983     A01
# 4  2.014e+11     2    2    3    1      0   11    0    0     0    1990     A01
# 5  2.014e+11     1    1    3    1      5   21    0    0     0    1974     A01
# 6  2.014e+11     1    1    1    2      5   11    0    0     0    1970     B02
# 7  2.014e+11     1    1    3    1      0   11    0    0     0    1971     A01
# 8  2.014e+11     1    1    1    1      4   11    0    0     0    1958     B02
# 9  2.014e+11     1    1    1    1      0   11    0    0     0    1995     B02
# 10 2.014e+11     1    1    4    1      1   11    0    0     0    1967     A01

# DT
colnames(first_10_lines_usagers) = paste('<span style="color:red">', colnames(first_10_lines_usagers), '</span>', sep = "")
datatable(first_10_lines_usagers, escape = FALSE)

#  usagers*.csv
# Boucle d'import
data_usagers <- do.call(rbind, lapply(list_file_usagers, function(x) cbind(read.csv(x, colClasses = c(Num_Acc="character", place="factor", catu="factor", grav="factor", sexe="factor", trajet="factor", secu="factor", locp="factor", actp="factor", etatp="factor", num_veh="factor"), fileEncoding = "ISO_8859-1", na.strings=c("NA","NaN", "")))))

colnames(data_usagers) = paste('<span style="color:red">', colnames(data_usagers), '</span>', sep = "")
datatable(head(data_usagers, 50), escape = FALSE)

# ??? Num_veh ??? identifiant duplicate??? => facteur

### 3. EXPLORATION DES DONNEES ###
# Pour la suite on utilisera uniquement la dernière année 2016
data_caracteristiques_2016 <- read.csv(list_file_caracteristiques[12], colClasses = c(Num_Acc="character", lum="factor", dep="factor", com="factor", agg="factor", int="factor", atm="factor", col="factor", adr="character", gps="factor", lat="integer", long="integer"), fileEncoding = "ISO_8859-1", na.strings=c("NA","NaN", "", "0"))
View(data_caracteristiques_2016)

## DT
colnames(data_caracteristiques_2016) = paste('<span style="color:red">', colnames(data_caracteristiques_2016), '</span>', sep = "")
datatable(head(data_caracteristiques_2016, 50), escape = FALSE)

data_lieux_2016 <- read.csv(list_file_lieux[12], colClasses = c(Num_Acc="character", catr="factor", voie="character", v1="integer", v2="factor", circ="factor", nbv="integer", vosp="factor", prof="factor", plan="factor", surf="factor", infra="factor", situ="factor"), fileEncoding = "ISO_8859-1", na.strings=c("NA","NaN", "", "0", 0))
View(data_lieux_2016)

## DT
colnames(data_lieux_2016) = paste('<span style="color:red">', colnames(data_lieux_2016), '</span>', sep = "")
datatable(head(data_lieux_2016, 50), escape = FALSE)

data_vehicules_2016 <- read.csv(list_file_vehicules[12], colClasses = c(Num_Acc="character", senc="factor", catv="factor", obs="factor", obsm="factor", choc="factor", manv="factor"), fileEncoding = "ISO_8859-1", na.strings=c("NA","NaN", ""))
View(data_vehicules_2016)

## DT
colnames(data_vehicules_2016) = paste('<span style="color:red">', colnames(data_vehicules_2016), '</span>', sep = "")
datatable(head(data_vehicules_2016, 50), escape = FALSE)

data_usagers_2016 <- read.csv(list_file_usagers[12], colClasses = c(Num_Acc="character", place="factor", catu="factor", grav="factor", sexe="factor", trajet="factor", secu="factor", locp="factor", actp="factor", etatp="factor", num_veh="factor"), fileEncoding = "ISO_8859-1", na.strings=c("NA","NaN", ""))
View(data_usagers_2016)

## DT
colnames(data_usagers_2016) = paste('<span style="color:red">', colnames(data_usagers_2016), '</span>', sep = "")
datatable(head(data_usagers_2016, 50), escape = FALSE)

# Type de variables
library(data.table)
library(rowr) # use 'cbind.fill'

## 3.1 Structure des données
mysummary <- function(x){
  x_dt <- as.data.table(x) # Transformer x vers data.table
  the_column_name = colnames(x)
  
  result_data_table <- data.table() # résultat initial
  for(i in 1:ncol(x)){
    print(i)
    c = class(x[,i]) # type de colonne i
    column_name_i = the_column_name[i] # nom de colonne i
    print(c)
    
    # non analyse pour identifiant
    if ( c == "character" ) {
      result_column_i_beta = data.table(the_column_name_i=c("class"), N = c(c))
      
      if ( i == 1) {
        result_data_table = result_column_i_beta
      } else {
        result_data_table = cbind.fill(result_data_table, result_column_i_beta, fill = NA)
      }
      
      setnames(result_data_table, old = "the_column_name_i", new = column_name_i)
      column_name_i = paste(column_name_i, "Summary", sep = "_")
      setnames(result_data_table, old = "N", new = column_name_i)
    }
    if ( c == "integer" ) {
      column_i <- x[,i]
      mean_of_i <- mean(column_i[column_i>0] , na.rm = TRUE) # moyenne
      median_of_i <- median(column_i[column_i>0] , na.rm = TRUE) # médiane 
      min_of_i <- min(column_i[column_i>0] , na.rm = TRUE) # min
      max_of_i <- max(column_i[column_i>0] , na.rm = TRUE) # max
      na_of_i <- x_dt[, sum(is.na(.SD)),  .SDcols = eval(i)] # valeurs manquantes
      
      result_column_i_beta = data.table(the_column_name_i=c("class","mean","median","min","max","NA"), N = c(c,mean_of_i,median_of_i,min_of_i,max_of_i,na_of_i))
      
      setnames(result_column_i_beta, old = "the_column_name_i", new = column_name_i)
      column_name_i = paste(column_name_i, "Summary", sep = "_")
      setnames(result_column_i_beta, old = "N", new = column_name_i)
      
      if ( i == 1) {
        result_data_table = result_column_i_beta
      } else {
        result_data_table = cbind.fill(result_data_table, result_column_i_beta, fill = NA)
      }
      
    }
    if ( c == "factor" ) {
      result_column_i_alpha = data.table(the_column_name_i=c("class"), N = c(c))
      result_column_i_beta <- x_dt[, .(.N), keyby = eval(the_column_name[i])] # Nombre de facteur (aggregate).
      
      setnames(result_column_i_alpha, old = "the_column_name_i", new = column_name_i)
      
      column_name_i = paste(column_name_i, "Summary", sep = "_")
      setnames(result_column_i_beta, old = "N", new = column_name_i)
      setnames(result_column_i_alpha, old = "N", new = column_name_i)
      print(result_column_i_alpha)
      print(result_column_i_beta)
      
      # Combiner result_column_i_alpha et result_column_i_beta 
      
      result_column_i_beta <- rbind(result_column_i_alpha, result_column_i_beta)
      
      if ( i == 1) {
        result_data_table = result_column_i_beta
      } else {
        result_data_table = cbind.fill(result_data_table, result_column_i_beta, fill = NA)
      }
    }
  }
  return(result_data_table)
}

## Caracteristiques 2016
data_caracteristiques_2016 <- read.csv(list_file_caracteristiques[12], colClasses = c(Num_Acc="character", lum="factor", dep="factor", com="factor", agg="factor", int="factor", atm="factor", col="factor", adr="character", gps="factor", lat="integer", long="integer"), fileEncoding = "ISO_8859-1", na.strings=c("NA","NaN", "", "0"))
summary_caracteristiques_2016 <- mysummary(data_caracteristiques_2016)
View(summary_caracteristiques_2016)
colnames(summary_caracteristiques_2016) = paste('<span style="color:red">', colnames(summary_caracteristiques_2016), '</span>', sep = "")
datatable(summary_caracteristiques_2016, escape = FALSE)

## Lieux 2016
data_lieux_2016 <- read.csv(list_file_lieux[12], colClasses = c(Num_Acc="character", catr="factor", voie="character", v1="integer", v2="factor", circ="factor", nbv="integer", vosp="factor", prof="factor", plan="factor", surf="factor", infra="factor", situ="factor"), fileEncoding = "ISO_8859-1", na.strings=c("NA","NaN", "", "0", 0))
summary_lieux_2016 <- mysummary(data_lieux_2016)
View(summary_lieux_2016)
# DT
colnames(summary_lieux_2016) = paste('<span style="color:red">', colnames(summary_lieux_2016), '</span>', sep = "")
datatable(summary_lieux_2016, escape = FALSE)

## Vehicules 2016
colnames(data_vehicules_2016) = paste('<span style="color:red">', colnames(data_vehicules_2016), '</span>', sep = "")
summary_vehicules_2016 <- mysummary(data_vehicules_2016)
View(summary_vehicules_2016)
# DT
colnames(summary_vehicules_2016) = paste('<span style="color:red">', colnames(summary_vehicules_2016), '</span>', sep = "")
datatable(summary_vehicules_2016, escape = FALSE)


## Usagers 2016
data_usagers_2016 <- read.csv(list_file_usagers[12], colClasses = c(Num_Acc="character", place="factor", catu="factor", grav="factor", sexe="factor", trajet="factor", secu="factor", locp="factor", actp="factor", etatp="factor", num_veh="factor"), fileEncoding = "ISO_8859-1", na.strings=c("NA","NaN", ""))
summary_usagers_2016 <- mysummary(data_usagers_2016)
View(summary_usagers_2016)
# DT
colnames(summary_usagers_2016) = paste('<span style="color:red">', colnames(summary_usagers_2016), '</span>', sep = "")
datatable(summary_usagers_2016, escape = FALSE)

### 4. Transformation ###
## 4.1 Valeurs abbérantes
data_caracteristiques_2016$lat[data_caracteristiques_2016$lat == 0] <- NA
data_caracteristiques_2016$long[data_caracteristiques_2016$long == 0] <- NA

## 4.2 Variables temporelles
# Dates sont représentées par le nb de jours depuis 01/01/1970
data_caracteristiques_2016$date = as.Date(paste("2016", data_caracteristiques_2016$mois, data_caracteristiques_2016$jour, sep = "-"))

# mj : mois et jour de l’accident (sans an)
data_caracteristiques_2016$mj = format(data_caracteristiques_2016$date, format="%m-%d")

# hms : heure et minute de l’accident en format POSIX. 
# Nb de secondes depuis 00:00:00 elapsed since 00:00:00 Coordinated Universal Time (UTC), Thursday, 1 January 1970.
# Dans le résumé, il n'y a pas de NA

# Le but est de transformer le temps en format de 06:15
data_caracteristiques_2016$hms =  format(as.POSIXlt(paste(data_caracteristiques_2016$date, paste(trunc(data_caracteristiques_2016$hrmn/100),(data_caracteristiques_2016$hrmn %% 100),"00", sep=":"), " ") ), '%H:%M')

# heure : heure de l’accident en format numérique
data_caracteristiques_2016$heure = trunc(data_caracteristiques_2016$hrmn/100)

# jsem : jour de la semaine
data_caracteristiques_2016$jsem = weekdays(data_caracteristiques_2016$date)

### 5. Visualisation ###
library(ggplot2)

## 5.1 
# Méthode 1
# Barre empilée
ggplot(data_lieux_2016, aes(x=factor(0))) + geom_bar(aes(fill=catr), position ="fill") + scale_fill_discrete(name = "Catégorie de route", labels = c("Autoroute","Route Nationale","Route Départementale","Voie Communale","Hors réseau public","Parc de stationnement ouvert à la circulation publique","autre")) + labs(x = "Catégorie de route", y = "pourcentage") + scale_x_discrete(labels = c(""))
# Anneau (beignet)
ggplot(data_lieux_2016, aes(x=factor(0))) + geom_bar(aes(fill=catr), position ="fill", width = 0.2) + scale_fill_discrete(name = "Catégorie de route", labels = c("Autoroute","Route Nationale","Route Départementale","Voie Communale","Hors réseau public","Parc de stationnement ouvert à la circulation publique","autre")) + labs(x = "Catégorie de route", y = "pourcentage") + scale_x_discrete(labels = c("")) + theme(legend.position="bottom") + coord_polar(theta = "y")
# Camembert
ggplot(data_lieux_2016, aes(x=factor(0))) + geom_bar(aes(fill=catr), position ="fill", width = 1) + scale_fill_discrete(name = "Catégorie de route", labels = c("Autoroute","Route Nationale","Route Départementale","Voie Communale","Hors réseau public","Parc de stationnement ouvert à la circulation publique","autre")) + labs(x = "Catégorie de route", y = "pourcentage") + scale_x_discrete(labels = c("")) + theme(legend.position="bottom") + coord_polar(theta = "y") 

# Méthode 2
# Etape 1:  Calcul le percentage de catégorie de route
lieux_catr_2016_summary <- as.data.table(summary_lieux_2016[2:8,3:4])
is.data.table(lieux_catr_2016_summary)
lieux_catr_2016_summary$catr_Summary <- as.integer(as.character(lieux_catr_2016_summary$catr_Summary))
lieux_catr_2016_summary
lieux_catr_2016_summary <- lieux_catr_2016_summary[ , percentage := prop.table(`catr_Summary`)]
lieux_catr_2016_summary
lieux_catr_2016_summary$catr_names <- c("Autoroute","Route Nationale","Route Départementale","Voie Communale","Hors réseau public","Parc de stationnement ouvert à la circulation publique","autre")

# Etape 2:  Ordre de pourcentage
lieux_catr_2016_summary$catr_names = factor(lieux_catr_2016_summary$catr_names, levels = lieux_catr_2016_summary$catr_names[order(lieux_catr_2016_summary$percentage)])
lieux_barplot <- ggplot(lieux_catr_2016_summary, aes(x="", y=percentage, fill=catr_names)) + geom_bar(width = 1, stat = "identity") + labs(x = "Catégorie de route", y = "pourcentage") + scale_fill_discrete(name = "Catégorie de route")
lieux_pie <- lieux_barplot + coord_polar("y", start = 0)
lieux_pie + scale_fill_brewer(palette = "Blues") + theme_minimal() + scale_fill_discrete(name = "Catégorie de route")

# Ajouter les étiquettes:
library(ggplot2)
library(dplyr)
library(ggforce)
library(scales)
library(ggrepel)


lieux_catr_2016_summary$Percentage <- round(lieux_catr_2016_summary$percentage * 100 , 2)
lieux_catr_2016_summary <- lieux_catr_2016_summary[order(-lieux_catr_2016_summary$Percentage),]
lieux_catr_2016_summary$label_position <- 100-(cumsum(lieux_catr_2016_summary$Percentage) -lieux_catr_2016_summary$Percentage/2)
lieux_catr_2016_summary_pie <- ggplot(data=lieux_catr_2016_summary,aes(x=1,y=Percentage,fill=reorder(catr_names,-label_position)))+
  geom_bar(stat = "identity",color='black')+
  geom_label_repel(aes(label = percent(Percentage/100),y = label_position), size=3, show.legend = F, nudge_x = 1)+
  guides(fill = guide_legend(title = "Catégorie de route"))+
  coord_polar("y",start=0)+ 
  labs(x='',y='',title='Catégorie de route')+
  theme(plot.title=element_text(hjust=0.5,face='bold',size=16))+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        legend.title=element_text(size=12,face='bold'))
lieux_catr_2016_summary_pie


