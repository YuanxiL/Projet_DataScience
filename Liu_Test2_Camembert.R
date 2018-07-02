# Graphique de camembert 2016
# Pourcentage des accidents par catégorie de route

setwd("~/Desktop/allianz/data")

library(data.table)
library(rowr) #fonction: cbind.fill, combiner  
# Même résumé que 3.1 Structure des données (1er fichier).
mysummary <- function(x){
  x_dt <- as.data.table(x)
  the_column_name = colnames(x)
  
  result_data_table <- data.table() # initialiser le résultat
  for(i in 1:ncol(x)){
    # print(i)
    c = class(x[,i]) # type de colonne i
    column_name_i = the_column_name[i] # nom de colonne i
    # print(c)
    
    
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
      mean_of_i <- mean(column_i[column_i>0] , na.rm = TRUE)
      median_of_i <- median(column_i[column_i>0] , na.rm = TRUE)
      min_of_i <- min(column_i[column_i>0] , na.rm = TRUE)
      max_of_i <- max(column_i[column_i>0] , na.rm = TRUE)
      na_of_i <- x_dt[, sum(is.na(.SD)),  .SDcols = eval(i)] 
      
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
      result_column_i_beta <- x_dt[, .(.N), keyby = eval(the_column_name[i])] # keyby => aggregate par facteur
      
      setnames(result_column_i_alpha, old = "the_column_name_i", new = column_name_i)
      
      column_name_i = paste(column_name_i, "Summary", sep = "_")
      setnames(result_column_i_beta, old = "N", new = column_name_i)
      setnames(result_column_i_alpha, old = "N", new = column_name_i)
      # print(result_column_i_alpha)
      # print(result_column_i_beta)
      
      # combiner 'result_column_i_alpha' et 'result_column_i_beta'
      
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
data_lieux_2016 <- read.csv("lieux_2016.csv", colClasses = c(Num_Acc="character", catr="factor", voie="character", v1="integer", v2="factor", circ="factor", nbv="integer", vosp="factor", prof="factor", plan="factor", surf="factor", infra="factor", situ="factor"), fileEncoding = "ISO_8859-1", na.strings=c("NA","NaN", "", "0", 0))
summary_lieux_2016 <- mysummary(data_lieux_2016)
lieux_catr_2016_summary <- as.data.table(summary_lieux_2016[2:8,3:4])
lieux_catr_2016_summary$catr_Summary <- as.integer(as.character(lieux_catr_2016_summary$catr_Summary))# catr_Summary => integer
lieux_catr_2016_summary$catr_Names <- c("Autoroute","Route Nationale","Route Départementale","Voie Communale","Hors réseau public","Parc de stationnement ouvert à la circulation publique","autre")
lieux_catr_2016_summary$catr_Names = factor(lieux_catr_2016_summary$catr_Names, levels = lieux_catr_2016_summary$catr_Names[order(-lieux_catr_2016_summary$catr_Summary)]) # Ordre: level de facteur
lieux_catr_2016_summary$catr_Names
lieux_catr_2016_summary
lieux_catr_2016_summary <- lieux_catr_2016_summary[order(lieux_catr_2016_summary$catr_Summary),]

lieux_catr_2016_summary$catr_Percentage <- round(lieux_catr_2016_summary$catr_Summary/sum(lieux_catr_2016_summary$catr_Summary) * 100 , 2)
library(ggplot2)
library(ggrepel)# Pour résoudre le problème de chevauchage
set.seed(42)

# Eviter le problème de chevauchage
lieux_catr_2016_summary$catr_Position = cumsum(lieux_catr_2016_summary$catr_Percentage) - lieux_catr_2016_summary$catr_Percentage/2
lieux_barplot <- ggplot(lieux_catr_2016_summary, aes(x="", y=catr_Percentage, fill=catr_Names)) + geom_bar(width = 1, stat = "identity") + labs(x = "Catégorie de route", y = "pourcentage") + scale_fill_discrete(name = "Catégorie de route")

lieux_pie <- lieux_barplot + coord_polar("y", start = 0) + 
  geom_label_repel(aes(label = paste(lieux_catr_2016_summary$catr_Percentage, "%"), y = lieux_catr_2016_summary$catr_Position), show.legend = F, nudge_x = 1)
lieux_pie
