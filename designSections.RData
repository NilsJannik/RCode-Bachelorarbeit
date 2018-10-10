# Funktion - Weisst jedem der Elemente in den Clustergruppen die entsprechenden
#            Werte aus dem Datensatz zu
# Eingabe:
# - data: Der gegebene Datensatz (data.frame)
# - clusterResult: Eine Liste bei der in jedem Element ein Vektor mit den 
#                  Indizes der jeweiligen Clustergruppenelemente stehen
# Rueckgabe:
# Eine Liste bei der in jedem Element der zu dieser Gruppe zugehoerige Teil
# der Daten in Form eines data.frame stehen
designSections <- function(data,
                           clusterResult){
  clustersize <- length(clusterResult)
  sections <- list()
  for(i in 1:clustersize){
    chosenID <- sort(clusterResult[[i]])
    sections[[i]] <- data[chosenID,]
  }
  return(sections)
}
