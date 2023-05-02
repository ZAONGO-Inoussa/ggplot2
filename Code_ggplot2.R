##VISUALISATION DES DONNEES AVEC LA BIBLIOTHEQUE GGPLOT2##

  #Importation de la base
quakes= read.csv("C:\\Users\\LENOVO\\Desktop\\expose_R\\ggplot\\my_quakes.csv",sep=",")
View(quakes)
#ou bien le jeu de donnée
data("quakes")
data =quakes

# Installation du package ggplot2
      #install.packages("ggplot2")
#charger le pakacge

  library(ggplot2)

##Présentation des données utilisées pour les exemples

data("quakes")
quakes$mag_catego <- factor(floor(quakes$mag)) # arrondi a l'entier inferieur
quakes$region <- factor(
  ifelse(quakes$long < 175, yes = "Ouest", no = "Est"), 
  levels = c("Ouest", "Est")# tremblement de terre dont la longetidue est <
)
str(quakes)
my_data=quakes
View(my_data)

##Démarche de création d'un graphique

# creation d'un nuage de point

mon_graph1 <- ggplot(data = quakes)
mon_graph <- mon_graph1 + geom_point(mapping = aes(x = mag, y = stations))
mon_graph


#ajout d'axe et de titre
ggplot(data = quakes) + 
  geom_point(mapping = aes(x = mag, y = stations)) +
  labs(
    title = "1000 séismes près de Fidji",          # ou ggtitle("...") +
    x = "magnitude du séisme",                     # ou xlab("...") +
    y = "nombre de stations rapportant le séisme"  # ou ylab("...")
  )

# on peut centrer le graphique

ggplot(data = quakes) + 
  geom_point(mapping = aes(x = mag, y = stations)) +
  labs(
    title = "1000 séismes près de Fidji",
    x = "magnitude du séisme",
    y = "nombre de stations rapportant le séisme"
  ) +
  theme(plot.title = element_text(hjust = 0.5))  # permet de centrer le titre



##Ajout d’une variable associée à une propriété visuelle autre qu’un axe

#Le graphique précédent représente deux variables.
#Ajoutons une troisième variable au graphique, qui fera varier la couleur des points. 
#Si la palette de couleur utilisée par défaut ne nous plaît pas, nous pouvons la changer.

scatterplot <- ggplot(data = quakes) + 
  geom_point(mapping = aes(
    x = mag, 
    y = stations, 
    colour = depth  # permet de faire varier la couleur des points en fonction de depth
  )) +
  labs(
    title = "1000 séismes près de Fidji",
    x = "magnitude du séisme",
    y = "nombre de stations rapportant le séisme",
    colour = "profondeur"  # permet de modifier le titre de la légende
  ) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_colour_viridis_c()       # permet d'utiliser la palette de couleur viridis

scatterplot

# Exemples de graphiques de différents types

library(questionr) # qui contient le jeu de données
library(dplyr) # pour utiliser  %in% 
data(rp2018)

rp <- filter(
  rp2018,
  departement %in% c("Oise", "Rhône", "Hauts-de-Seine", "Lozère", "Bouches-du-Rhône")
)
View(rp)

## Diagramme en barres: fonctions geom-bar et geom-col

ggplot(data = quakes) + 
  geom_bar(mapping = aes(x = mag_catego)) +
  labs(
    x = "classe de magnitude du séisme",
    y = "fréquences"
  )  


# deplacer le graphique

quakes_mag <- as.data.frame(xtabs(~ mag_catego, data = quakes))#pour spécifier que l'on souhaite créer un tableau de contingence à partir de la variable à droite de 
quakes_mag


library(ggplot2)
ggplot(data = quakes_mag) + 
  geom_col(mapping = aes(x = mag_catego, y = Freq)) +  # aesthetic y ajoutée
  coord_flip() +                               # permet d'inverser les axes
  labs(
    x = "classe de magnitude du séisme",
    y = "fréquences"
       )


# Ajouter des annotation au graphique à geom_text
ggplot(data = quakes, mapping = aes(x = mag_catego)) + # aesthetics communs ici
  geom_bar() +
  geom_text(
    mapping = aes(label = after_stat(count)),          # texte ajouté = fréquences
    colour = "blue",
    stat = "count",                                    # calcul des fréquences demandé ici 
    vjust = -0.2                                       # ajustement vertical (deplace l'element vers le haut de 0.2 unité)
  ) +
  labs(
    x = "classe de magnitude du séisme",
    y = "fréquences"
  ) 

#Representation des frequences croisés

ggplot(data = quakes) + 
  geom_bar(mapping = aes(x = mag_catego, fill = region)) +  # aesthetic fill ajoutée
  labs(
    x = "classe de magnitude du séisme",
    y = "fréquences",
    fill = "région"
  ) 


#representation comparative

ggplot(data = quakes) + 
  geom_bar(
    mapping = aes(x = mag_catego, fill = region), 
    position = "dodge"                             # position des bâtons modifiée
  ) +
  labs(
    x = "classe de magnitude du séisme",
    y = "fréquences",
    fill = "région"
  ) 


#présenter des fréquences relatives plutôt que brutes.

ggplot(data = quakes) + 
  geom_bar(
    mapping = aes(
      x = mag_catego, 
      y = after_stat(prop),  # permet le calcul de fréquences relatives (proportions)
      group = region,        # pour avoir les fréq. relatives de mag conditionnelles à region
      fill = region), 
    position = "dodge"
  ) +
  labs(
    title = "Proportions de séismes par classe de magnitude selon la région",
    x = "classe de magnitude du séisme",
    y = "fréquences",
    fill = "région"
  ) 

# Diagramme en secteurs: coordonnées polaires avec coord-polar.

ggplot(data = quakes) +
  geom_bar(mapping = aes(x = 1, fill = mag_catego)) +
  coord_polar(theta = "y") +                           # coordonnées polaires
  theme_void() +                                       # thème vide
  labs(fill = "classe de\nmagnitude")

#Courbes de densité :fonction geom-density

#Courbes de densité à noyau unique

ggplot(rp) + geom_density(aes(x = cadres))

#Courbes de densité à noyau superposées
ggplot(data = quakes) + 
  geom_density(
    mapping = aes(x = mag, fill = region), 
    alpha = 0.5  # niveau d'opacité (0 = transparent, 1 = complètement opaque)
  ) +
  labs(
    x = "magnitude du séisme",
    y = "densité",
    fill = "région"
  )

#Diagrammes en violons : fonction geom-violin
# deux
violinplots <- ggplot(data = quakes) + 
  geom_violin(mapping = aes(x = region, y = mag)) +
  labs(
    x = "région",
    y = "magnitude du séisme"
  )

violinplots
 
# plusieurs
ggplot(rp) + geom_violin(aes(x = departement, y = maison))


#lecture plus fine


ggplot(rp) +
  geom_violin(
    aes(x = departement, y = maison),
    bw = 2
  )

# Diagrammes en boîtes : fonction geom-boxplot

## diagramme en boite unique


ggplot(quakes, aes(x = region[1], y =mag)) + 
  geom_boxplot()


#diagramme en boite juxtaposés.

boxplots <- ggplot(data = quakes) + 
  geom_boxplot(mapping = aes(x = region, y = mag)) +
  labs(
    x = "région",
    y = "magnitude du séisme"
  )

boxplots

# plusieurs

ggplot(rp) + geom_boxplot(aes(x = departement, y = maison))



#personnalisation des sortis

ggplot(rp) +
  geom_boxplot(
    aes(x = departement, y = maison),
    fill = "wheat", color = "tomato4"
  )


#La fonction geom-text

data(rp)
ggplot(rp) +
  geom_text(
    aes(x = dipl_sup, y = cadres, label = commune)
  )



#personnaliser l'apparence

ggplot(rp) +
  geom_text(
    aes(x = dipl_sup, y = cadres, label = commune),
    color = "darkred", size = 2
  )

## La fonction geom-label

library(ggplot2)
ggplot(rp) + geom_label(aes(x = dipl_sup, y = cadres, label = commune))


#La fonction geom-line



data("economics")
str(economics)
ggplot(economics) + geom_line(aes(x = date, y = unemploy))

#La fonction geom-hex et geom-bin2d

#initialisation
ggplot(rp2018) + geom_point(aes(x = cadres, y = dipl_sup))
 #geom_bin22d
ggplot(rp2018) +
  geom_bin2d(aes(x = cadres, y = dipl_sup))

#geom_hex
ggplot(rp2018) +
  geom_hex(aes(x = cadres, y = dipl_sup))


#varier le nombre de zone
###
ggplot(rp2018) +
  geom_bin2d(
    aes(x = cadres, y = dipl_sup),
    bins = 50
  )

###
ggplot(rp2018) +
  geom_hex(
    aes(x = cadres, y = dipl_sup),
    bins = 70
  )


#Représentation de plusieurs geom

# nuage + boxplot

ggplot(rp, aes(x = departement, y = maison)) +
  geom_boxplot() +
  geom_jitter(color = "red", alpha = 0.2)


#regression

ggplot(rp, aes(x = dipl_sup, y = cadres)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm")
#> `geom_smooth()` using formula = 'y ~ x'


# Mappages 
#initialisation
ggplot(rp) +
  geom_point(
    aes(x = dipl_sup, y = cadres)
  )

# varier les couleurs
ggplot(rp) +
  geom_point(
    aes(x = dipl_sup, y = cadres, color = departement)
  )

#varier la taille

ggplot(rp) +
  geom_point(
    aes(x = dipl_sup, y = cadres, color = departement, size = pop_tot)
  )

# associer la transparence
ggplot(rp) +
  geom_point(
    aes(x = dipl_sup, y = cadres, color = departement, size = pop_tot, alpha = maison)
  )

#scale-size
# c(0,20)
ggplot(rp) +
  geom_point(aes(x = dipl_sup, y = cadres, size = pop_tot)) +
  scale_size(range = c(0, 20))
     #VS
# c(2,8)

ggplot(rp) +
  geom_point(aes(x = dipl_sup, y = cadres, size = pop_tot)) +
  scale_size(range = c(2, 8))


# changer la legende par defaut
ggplot(rp) +
  geom_point(aes(x = dipl_sup, y = cadres, size = pop_tot)) +
  scale_size(
    "Population",
    range = c(0, 15)
  )


#scale-x, scale-y

ggplot(rp) +
  geom_point(aes(x = dipl_sup, y = cadres))

#scale-x_continuous, scale-y_continuous pour les variables quantitative

ggplot(rp) +
  geom_point(aes(x = dipl_sup, y = cadres)) +
  scale_x_continuous(limits = c(0,100)) +
  scale_y_continuous(limits = c(0,100))

# scale_x_log10 pour lisser diplome superieur
ggplot(rp) +
  geom_point(aes(x = dipl_sup, y = cadres)) +
  scale_x_log10("Diplômés du supérieur")


# Thèmes

ggplot(data = rp) +
  geom_histogram(aes(x = cadres)) +
  theme_bw() # existant dans R 


# faceting

ggplot(data = rp) +
  geom_histogram(aes(x = cadres)) +
  facet_wrap(vars(departement))



## Représentation graphique d’une fonction

# Statistique dont nous aurons besoin
moy <- mean(quakes$mag)
et <- sd(quakes$mag)

histogramme <- ggplot(data = quakes, mapping = aes(x = mag)) + 
  geom_histogram(
    mapping = aes(y = after_stat(density)), 
    binwidth = 0.1, 
    colour = "black", 
    fill = "white"
  ) +
  labs(
    title = "Densité empirique des magnitudes dans le jeu de données quakes",
    x = "magnitude du séisme",
    y = "densité"
  )

histogramme + stat_function(           # ajout d'une courbe de densité normale 
  fun = dnorm, 
  args = list(mean = moy, sd = et),  # avec paramètres estimés à partir des données
  xlim = c(3.95, 6.45)
)


#ajout d'une courbe gaussienne

histogramme <- histogramme +
  geom_density(
    aes(colour = "density"),  # association de la propriété visuelle colour à une valeur
  ) +
  stat_function(
    aes(colour = "dnorm"),    # association de la propriété visuelle colour à une valeur
    fun = dnorm, 
    args = list(mean = moy, sd = et),
    xlim = c(3.95, 6.45)
  )
histogramme

  )

#Cartes géographiques

library(maps)
monde <- map_data("world")  # va chercher des données provenant du package maps
ggplot() + 
  geom_polygon(
    data = monde,
    aes(x = long, y = lat, group = group), 
    fill = "gray90", 
    col = "black"
  ) +
  geom_point(
    data = quakes, 
    aes(x = long, y = lat, colour = depth), 
    alpha = .3
  ) +
  coord_quickmap(           # ou coord_map() si le package mapproj est installé
    xlim = c(115, 190),  
    ylim = c(-50, 0)
  ) +
  labs(x = "longitude", y = "latitude", colour = "profondeur") +
  theme_bw()

