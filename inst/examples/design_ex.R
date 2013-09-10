# load german football-league (Bundesliga) data
library("wikibooks")
data(Bundesliga)
?Bundesliga

# add new variable Y3 reflecting the response which is coded as 
# 1 if the home team wins
# 0 if the game ends up with a tie
# -1 if the home team loses
diff <- Bundesliga$Tore.Heim - Bundesliga$Tore.Gast
Bundesliga$Y3 <- as.ordered(ifelse(diff > 0, 1, 
                                   ifelse(diff < 0, -1, diff)))
buli0405 <- subset(Bundesliga, Saison=="2004/2005")
str(buli0405)

# Design matrix without home advantage
des <- design(buli0405, var1="Heim", var2="Gast", 
              home.advantage="no")
str(des)

# Design matrix with one home advantage parameter for all objects
des.all <- design(buli0405, var1="Heim", var2="Gast", 
                  home.advantage="yes")
str(des.all)

# Design matrix with home advantage parameters for each object
des.spe <- design(buli0405, var1="Heim", var2="Gast",
                  home.advantage="specific")
str(des.spe)

# Design matrix with additional covariable "Spieltag"
des.covs <- design(buli0405, var1="Heim", var2="Gast", 
                   use.vars=c("Spieltag"), home.advantage="no")
str(des.covs)
