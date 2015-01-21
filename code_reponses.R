library(foreign)


# ici il faudra changer ton wd pour le dossier qui contient  "01_weights.sav"
wd = "C:/Users/SexyManatee/Desktop/PhD/Cogmaster/r-cogstats/projects" 

# lecture du fichier et importation du data.frame
w <- read.spss(paste(wd,"01-weights.sav", sep = '/'), to.data.frame = TRUE)

#pour vérifier que les noms sont bien là et que tout est en ordre, à comparer avec le fichier de description donné par Lalanne.
names(w)
str(w)
