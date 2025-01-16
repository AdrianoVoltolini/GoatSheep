
#install.packages("geomorph")
#install.packages("caret")
#install.packages("dplyr")
#install.packages("em")
#install.packages("readxl")

#carica librerie
library(geomorph) #per fare Generalized Procrustes Analysis (GPA)
library(caret) #per fare machine learning
library(dplyr) #per utilizzare pipelines (opzionale)
library(em) #per la funzione "flatten". Contiene anche funzioni di statistica 
library(readxl) #per aprire file excel


#importa i landmarks del training set dall'excel o dal csv
#landmarks_raw <- read_excel("landmarks.xlsx")
landmarks_raw <- read.csv("all_landmarks.csv")


#importa il file che definisce quali sono i landmarks "fissi" e quali sono invece semi-landmarks
semi_landmarks <- read.csv("my_curveslide.csv")


#definisce posizione dei semi-landmarks
#NB:bisogna togliere righe che hanno landmarks fissi indicati come slide
#define.sliders(c(1,8:15,3,16:23,5, 24:31,7, 32:39, 6, 40:47,4,48:55,2), write.file = TRUE)


#rimuove le colonne ID, Collection, Groups dalla tabella dei landmarks e le salva a parte
landmarks_character <- subset(landmarks_raw, select=-c(ID, Collection,Groups))
landmarks_meta <- data.frame(subset(landmarks_raw, select=c(ID, Collection,Groups)))


#trasforma la tabella dei landmarks in dataframe di numeri
landmarks_df <- data.frame(lapply(landmarks_character, as.numeric))


#salva numero di righe del dataframe cosicché dopo si possano dividere
#i dati del training set da quelli del test set
initial_nrow <- nrow(landmarks_df)


#importa landmarks che sono dentro la cartella "landmarks m3" (il test set)
#e le aggiunge al dataframe contenente il training set
cartella_test <- "landmarks m3"
filenames <- list.files(path = cartella_test, pattern = "\\.txt$", full.names = TRUE)
for(fname in filenames){
  ftemp <- read.csv(fname, sep="\t", header=FALSE)
  ftemp["V2"] <- -ftemp["V2"]
  dftemp <- data.frame(flatten(ftemp, by="row"))
  colnames(dftemp) <- colnames(landmarks_df)
  landmarks_df[nrow(landmarks_df)+1,] <- dftemp
}


#trasforma il dataframe in un 3D array
#(la funzione "gpagen" accetta solo input di questo tipo)
landmarks_array <- arrayspecs(landmarks_df, 55, 2)


#fa la Generalized Procrustes Analysis (GPA)
gpagen_res <- gpagen(landmarks_array, curves=semi_landmarks)


#funzione per ricavarsi la specie dal gruppo a cui appartiene l'animale
specificatore <- function(x){
  if(x == "Archaeological_Sheep"){
    return("SHEEP") 
  }
  else if(x == "Modern_Sheep"){
    return("SHEEP") 
  }
  else if(x == "Archaeological_Goat"){
    return("GOAT") 
  }
  else if(x == "Modern_Goat"){
    return("GOAT") 
  }
}

#ricava la specie dal gruppo a cui appartiene l'animale
landmarks_meta$Species <- lapply(landmarks_meta$Groups, specificatore)


# mette i landmarks allineati dalla Generalized Procrustes Analysis in un dataframe
# compatibile con le funzioni del pacchetto "caret"
full_df <- t(data.frame(array(gpagen_res$coords, dim=c(110,dim(gpagen_res$coords)[3]))))


# cambia i nomi delle righe e colonne del dataframe
my_colnames <- colnames(landmarks_character)
my_colnames[1:18] <- c("X01", "Y01", "X02", "Y02", "X03", "Y03", "X04", "Y04", "X05", "Y05",
                          "X06", "Y06", "X07", "Y07", "X08", "Y08", "X09", "Y09")
sorted_colnames <- sort(my_colnames)
colnames(full_df) <- sorted_colnames
rownames(full_df) <- seq(nrow(full_df))


#separa i dati del training set da quelli del test set
train_df <- full_df[seq(initial_nrow),]
test_df <- as.data.frame(full_df[seq(initial_nrow+1, nrow(full_df)),])



################################################################################
#MACHINE LEARNING 
################################################################################


#fa addestramento del modello di LDA
lda_fit <- caret::train(x=train_df, y=factor(landmarks_meta$Species, levels=c("GOAT","SHEEP")),
                        method="lda", trControl = trainControl(method = "none"), verbose=TRUE)

#usa il modello per prevedere le classi degli individui del test set
lda_pred <- predict(lda_fit, test_df)

#mette risultati in un dataframe
results_df <- data.frame(filenames)
results_df[,"Predictions"] <- lda_pred

#salva risultati in un file .csv
write.csv(results_df, "risultati.csv", row.names=FALSE)

#osserva i risultati
View(results_df)


#set.seed(80085)

#train_test_partition <- createDataPartition(landmarks_meta$Groups, times=1, p=0.8)

#tv_df <- full_df[train_test_partition[["Resample1"]],]
#tv_meta <- landmarks_meta[train_test_partition[["Resample1"]],]

#test_df <- full_df[-train_test_partition[["Resample1"]],]
#test_meta <- landmarks_meta[-train_test_partition[["Resample1"]],]

#tv_groups <- tv_meta %>% group_by(Groups) %>% summarise(total_count=n(), .groups = 'drop')
#test_groups <- test_meta %>% group_by(Groups) %>% summarise(total_count=n(), .groups = 'drop')

#lda_cv <- caret::train(x=tv_df, y=factor(tv_meta$Species, levels=c("GOAT","SHEEP")),
#                       method="lda", trControl = trainControl(method = "cv"), verbose=TRUE)
# average accuracy of 95% on CV sets

#lda_cv_pred <- predict(lda_cv, test_df)
#conf_matrix <- confusionMatrix(lda_cv_pred, factor(test_meta$Species, levels=c("GOAT", "SHEEP")))
#10 errori: accuracy 94%, sensitivity 82%, specificity 100%, balanced accuracy:91% 

#test_results <- test_meta
#test_results[,"Predictions"] <- lda_cv_pred

#test_results[,"Correct"] <- test_results[,"Species"] == test_results[,"Predictions"]

#View(test_results)


