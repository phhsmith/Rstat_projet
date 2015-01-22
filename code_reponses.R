library(foreign)


# ici il faudra changer ton wd pour le dossier qui contient  "01_weights.sav"
wd = "C:/Users/SexyManatee/Desktop/PhD/Cogmaster/r-cogstats/projects" 

# lecture du fichier et importation du data.frame
w <- read.spss(paste(wd,"01-weights.sav", sep = '/'), to.data.frame = TRUE)

#pour vérifier que les noms sont bien là et que tout est en ordre, à comparer avec le fichier de description donné par Lalanne.
names(w)
str(w)

library(foreign)

wd = "C:/Users/SexyManatee/Desktop/PhD/Cogmaster/r-cogstats/projects"


w <- read.spss(paste(wd,"01-weights.sav", sep = '/'), to.data.frame = TRUE)
names(w)
str(w)


# Q1


#c'est ca que je comprends pour "numerical summary of sample size:
#  frequencies and counts", mais j'avoue que je ne trouve pas la question claire... Regarde si ça te convient.

#counts
func_count = function(x, digits = 1)
  round(length(x), digits = digits)

d_count = aggregate(w$GENDER ~ PARITY, data = w, func_count)
names(d_freq) = c("PARITY", "COUNT")

#frequencies
func_freq = function(x, digits = 1)
  round(length(x)/length(w$GENDER), digits = digits)

d_freq = aggregate(w$GENDER ~ PARITY, data = w, func_freq)
names(d_freq) = c("PARITY", "FREQUENCY")

# summary of baby weights

func_weight = function(x, digits = 1)
  round(c(mean = mean(x), sd = sd(x), range = range(x)), digits = digits)

d_weight = aggregate(WEIGHT ~ PARITY, data = w, func_weight)
# je ne sais pas changer les headers pour range: d_weight indique range1 et range2 quand je voudrais mettre range_min et range_max...

# plots

densityplot(~WEIGHT, data = w, groups = PARITY, auto.key = TRUE)
bwplot(PARITY ~ WEIGHT, data = w, pch = "|")


### Q2

mod = aov(WEIGHT~PARITY, data = w)
summary(mod)

## p < 0.03 : rejet de H0, le poids est bien lié à la parité

# part de la variance expliquée :
# ratio SS(effect)/ss(effect + residual)
part_explained = summary(mod)[[1]][["Sum Sq"]][1]/summary(mod)[[1]][["Sum Sq"]][2]
#1,78%


# Assumptions

# independance is OK : less than 10% of the population, no links between the children
# approximately normal:
# qqplot
qqnorm(w$WEIGHT)
# seems OK
# constant variance:
bwplot(PARITY ~ WEIGHT, data = w, pch = "|")
# seems OK


### Q3
# aggregation des deux derniers groupes

levels(w$PARITY)[4] = levels(w$PARITY)[3]
levels(w$PARITY)[3] = "2 or more siblings"

#aov
mod_2 = aov(WEIGHT~PARITY, data = w)
summary(mod_2)
# p < 0.01 : toujours rejet de H0
#part de la variance
part_explained_2 = summary(mod_2)[[1]][["Sum Sq"]][1]/summary(mod_2)[[1]][["Sum Sq"]][2]
#1,78%
# assumptions
qqnorm(w$WEIGHT)
bwplot(PARITY ~ WEIGHT, data = w, pch = "|")
# pas de changement.


### Q4
summary(m <- lm(w$WEIGHT ~ w$PARITY))
# p < 0.01 existence d'une corrélation linéaire entre poids et parité
# éventuellement on la retrouve vaguement à l'oeil ici, mais je sais pas comment vérifier que je n'ai pas fait de connerie.
xyplot(w$WEIGHT ~ w$PARITY, jitter.x = TRUE, cex = .8, col = "cornflowerblue", alpha=.5)
# extracting R-squared (pas certaine de ce que je fais...)
summary(m)$r.squared
# 0.01734983
# mauvais R² ==> corrélation faible...

