library(shiny)
library(ggplot2)
library(plotly)
library(stringr)
library(Hmisc)
library(xml2)
library(markdown)
library(shinythemes)
library(htmlwidgets)
library(httr)
library(ngramr)
library(dplyr)
library(htmltools)
library(shinyWidgets)
library(rclipboard)
library(lubridate)
library(leaflet)
#library(shinyURL)
library(shinyjs)


mobileDetect <- function(inputId, value = 0) {
  tagList(
    singleton(tags$head(tags$script(src = "mobile.js"))),
    tags$input(id = inputId,
               class = "mobile-element",
               type = "hidden")
  )
}

shinyUI(bootstrapPage(
  tags$head(includeHTML(("google-analytics.html"))),
  tags$head(includeHTML(("google-search.html"))),
#   tags$style("
#               body {
#     -moz-transform: scale(2, 2); /* Moz-browsers */
#     zoom: 2; /* Other non-webkit browsers */
#     zoom: 200%; /* Webkit browsers */
# }
#               "),
  mobileDetect('isMobile'),
  bootstrapPage(
    useShinyjs(),
    div(rclipboardSetup(),
        p(""),
        div(style="display: inline-block;vertical-align:bottom",actionButton("showSidebar", "",icon = icon("bars"))),
           div(style="display: inline-block;vertical-align:bottom",dropdownButton(tags$h3("Options avancées"),
                                                                                  checkboxInput("barplot", "Distribution des documents de la base de données ", value = FALSE),
                                                                                  div(style = "margin-top: -15px"),
                                                                                  checkboxInput("correlation_test", "Matrices de corrélation", value = FALSE),
                                                                                  div(style = "margin-top: -15px"),
                                                                                  checkboxInput("delta", "Différence de fréquence entre les deux premiers termes F(a)-F(b)", value = FALSE),
                                                                                  div(style = "margin-top: -15px"),
                                                                                  checkboxInput("fraction", "Rapport de fréquence entre les deux premiers termes F(a)/F(b)", value = FALSE),
                                                                                  div(style = "margin-top: -15px"),
                                                                                  checkboxInput("scale", "Rééchelonnement des résultats", value = FALSE),
                                                                                  div(style = "margin-top: -15px"),
                                                                                  checkboxInput("multicourbes", "Afficher toutes les données de la session dans le graphique", value = FALSE),
                                                                                  div(style = "margin-top: -15px"),
                                                                                  checkboxInput("loess", "Lissage loess (tendances)", value = FALSE),
                                                                                  div(style = "margin-top: -15px"),
                                                                                  checkboxInput("histogramme", "Mode histogramme", value = FALSE),
                                                                                  div(style = "margin-top: -15px"),
                                                                                  checkboxInput("spline", "Spline dans le graphe scientifique", value = TRUE),
                                                                                  # div(style = "margin-top: -15px"),
                                                                                  # shinyURL.ui(),
                                                                                  div(style = "margin-top: -15px"),
                                                                                  downloadButton("data_session","Données de la session"),
                                                                                  circle = TRUE, status = "default",
                                                                                  size = "sm",
                                                                                  icon = icon("sliders"), width = "300px",
                                                                                  tooltip = tooltipOptions(title = "Afficher les options avancées")
           )),
           div(style="display: inline-block;vertical-align:bottom;float:right",actionButton("twitter",label = img (src="twitter.png", width="15", height="15"),onclick ="window.open('https://twitter.com/gallicagram', '_blank')")),
           div(style="display: inline-block;vertical-align:bottom;float:right",actionButton("fb",label = img (src="facebook.png", width="15", height="15"),onclick ="window.open('https://www.facebook.com/gallicagram', '_blank')")),
           div(style="display: inline-block;vertical-align:bottom;float:right",rclipButton("clipbtn", "Citation",clipText = "Azoulay, B., & de Courson, B. (2021, December 8). Gallicagram : un outil de lexicométrie pour la recherche. https://doi.org/10.31235/osf.io/84bf3",icon = icon("clipboard"))),
           div(style="display: inline-block;vertical-align:bottom;float:right",actionButton("link", "Article de recherche",onclick ="window.open('https://osf.io/preprints/socarxiv/84bf3/', '_blank')")),
    ),
    div(id ="Sidebar",wellPanel(
      div(style="display: inline-block;vertical-align:bottom;width: 78%;",textInput("mot","Recherche","Joffre&Pétain&Foch")),
      div(style="display: inline-block;vertical-align:bottom;width: 20%;",
          conditionalPanel(condition="input.cooccurrences==1 && (input.doc_type == 1 || input.doc_type == 2 || input.doc_type == 3) && input.search_mode ==1",numericInput("prox","Distance",20))
      ),
      conditionalPanel(condition="(input.doc_type == 1 || input.doc_type == 2 || input.doc_type == 3) && input.search_mode ==1",div(style = "margin-top: -20px")),
      conditionalPanel(condition="(input.doc_type == 1 || input.doc_type == 2 || input.doc_type == 3) && input.search_mode ==1",checkboxInput("cooccurrences", "Explorer les cooccurrences", value = FALSE)),
      
      conditionalPanel(condition="(input.doc_type==1 || input.doc_type==2) && input.search_mode == 3 && input.joker == 1",
                       div(style="display: inline-block;vertical-align:bottom;width: 38%;",numericInput("stpw","Mots vides ignorés",500)),
                       div(style="display: inline-block;vertical-align:bottom;width: 39%;",numericInput("nbJoker","Nombre de jokers",5))
      ),
      conditionalPanel(condition="(input.doc_type==1 || input.doc_type==2) && input.search_mode == 3",div(style = "margin-top: -20px")),
      conditionalPanel(condition="(input.doc_type==1 || input.doc_type==2) && input.search_mode == 3 && input.joker == 0",div(style = "margin-top: -20px")),
      conditionalPanel(condition="(input.doc_type==1 || input.doc_type==2) && input.search_mode == 3",checkboxInput("joker", "Mode joker", value = FALSE)),
      div(style = "margin-top: -15px"),
      uiOutput("instructions"),
      conditionalPanel(condition="input.doc_type == 4",p('Recherche limitée à un seul syntagme')),
      conditionalPanel(condition="(input.doc_type == 2 || input.doc_type == 3) && input.search_mode ==2",p('Recherche limitée à un seul syntagme dans 5 000 documents au maximum')),
      conditionalPanel(condition="((input.doc_type == 2 || input.doc_type == 3) && input.search_mode ==2) || input.doc_type == 4",textOutput("avertissement")),
      div(style="display: inline-block;vertical-align:top;width: 49%;",selectInput("language", "Langue",choices = list("Français" = 1, "Allemand" = 2, "Néerlandais"=3, "Anglais"=4, "Espagnol"=5),selected = 1)),
      div(style="display: inline-block;vertical-align:top;width: 49%;",conditionalPanel(condition="input.language == 1",selectInput("bibli", "Source",choices = list("Gallica"=1,"Bibliothèques nationales"=2,"Bibliothèques régionales, locales ou spécialisées"=3,"Presse contemporaine"=4,"Corpus scientifiques"=5)))),
      div(style="display: inline-block;vertical-align:top;width: 49%;",selectInput("doc_type", "Corpus",choices = list("Presse française / Gallica" = 1,"Recherche par titre de presse / Gallica" = 3, "Corpus personnalisé / Gallica"=4, "Livres / Gallica" = 2,"Livres / Ngram Viewer - Google Books" = 5, "Presse allemande / Europeana" = 6, "Presse néerlandaise / Europeana" = 7, "Presse britannique / BNA" = 8, "Livres / Ngram Viewer Allemand" = 9, "Livres / Ngram Viewer Anglais" = 10, "Presse espagnole / BNE"=11, "Livres / Ngram Viewer Espagnol"=12, "Presse wallonne / KBR"=13, "Presse flamande / KBR"=14, "Presse suisse-romande / Bibliothèque nationale suisse"=15, "Presse suisse-allemande / Bibliothèque nationale suisse"=16, "Presse Auvergne-Rhône-Alpes / Lectura"=17, "Presse du sillon lorrain / Limedia"=18, "Presse méridionale / Mémonum"=19, "Presse de Saint-Denis / Commun-Patrimoine"=20, "Presse de Brest / Yroise"=21, "Presse des Pyrénées / Pireneas"=22, "Presse toulousaine / Rosalis"=23, "Presse diplomatique / Bibliothèque diplomatique numérique"=24, "Presse francophone / RFN"=25, "Presse alsacienne / Numistral"=26, "Presse de Roubaix / BN-R"=27, "Presse québécoise / BAnQ"=28, "Presse austro-hongroise / ANNO"=29, "Le Monde"=30, "Le Figaro"=31,"Cairn.info"=32,"Theses.fr"=33,"HAL-SHS"=34,"Presse australienne / Trove"=35,"Isidore"=36,"Presse américaine / newspapers.com"=37,"Presse canadienne / newspapers.com"=38,"Presse anglaise / newspapers.com"=39,"Presse australienne / newspapers.com"=40),selected = 1)),
      div(style="display: inline-block;vertical-align:top;width: 49%;",selectInput("search_mode", "Mode de recherche",choices = list("Par n-gramme" = 3),selected = 3)),
      
      conditionalPanel(condition="input.doc_type == 3",
                       div(style="display: inline-block;vertical-align:top;width: 49%;",conditionalPanel(condition="input.doc_type == 3",radioButtons("filtre", "",choices = list("Filtre thématique"=1,"Filtre géographique"=2),inline = T))),
                       div(style="display: inline-block;vertical-align:top;width: 49%;",conditionalPanel(condition="input.doc_type == 3",uiOutput("themes_presse"))),
                       conditionalPanel(condition="input.doc_type == 3",uiOutput("titres"))),
      
      conditionalPanel(condition="input.doc_type == 32 || input.doc_type == 33 || input.doc_type == 36",
                       div(style="display: inline-block;vertical-align:top;width: 49%;",conditionalPanel(condition="input.doc_type == 32",
                                                                                                         selectInput("cairn", "Discipline",choices = list("_"=0,"Art" = 70, "Droit" = 2, "Economie, Gestion"=1, "Gégraphie"=30, "Histoire"=3, "Info.-Com."=9, "Intérêt général"=4, "Lettres et linguistique"=5, "Médecine"=139, "Philosophie"=6, "Psychologie"=7,"Santé publique"=141,"Sciences de l'éducation"=8, "Sciences politiques"=10, "Sociologie et société"=11, "Sport et société"=12),selected = 0))),
                       div(style="display: inline-block;vertical-align:top;width: 49%;",conditionalPanel(condition="input.doc_type == 33",
                                                                                                         selectInput("theses", "Discipline",choices = list("_","Médecine","Physique","Informatique","Chimie","Sciences économiques","Histoire","Sciences biologiques et fondamentales appliquées. Psychologie","Sciences appliquées","Sciences de gestion","Psychologie","Pharmacie","Sociologie","Droit public","Droit privé","Philosophie","Droit","Mathématiques","Géographie","Sciences de l'éducation","Sciences juridiques","Mécanique","Science politique","Terre, océan, espace","Littérature française","Neurosciences","Sciences du langage","Sciences médicales","Mathématiques appliquées","Sciences de l'information et de la communication","Biologie","Sciences de la vie et de la sante","Sciences","Linguistique","Aspects moléculaires et cellulaires de la biologie","Chimie organique","Génie électrique","Histoire de l'art","Médecine générale","Génie des procédés","Électronique","Immunologie","Sciences de la vie et de la santé","Sciences et techniques","Génie civil","Sciences des matériaux","Aspects moleculaires et cellulaires de la biologie","Lettres","Sciences de la vie","Arts","Sciences biologiques fondamentales et appliquées","Sciences de l'univers","Biologie Santé","Automatique","Art et archéologie","Biologie cellulaire et moléculaire","Ethnologie","Mécanique des fluides","Littérature comparée","Chimie des matériaux","Histoire et civilisations","Chimie - Physique","Génie mécanique","Sciences de Gestion","Sciences et techniques communes","Sciences economiques","Biologie cellulaire","Langues et litteratures etrangeres","Geographie","Mathematiques","Electronique, microelectronique, optique et lasers, optoelectronique microondes robotique","Sciences politiques","Sciences et techniques des activités physiques et sportives","Archéologie","Études anglophones","Biochimie","Sciences de la vie et de la santé","Sciences de l'education","Electronique","Langues et litteratures francaises","Droit privé et sciences criminelles","Acoustique","Sciences pour l'ingénieur","Microbiologie","Physiologie et biologie des organismes - populations - interactions","Sciences physiques","Musicologie","Anthropologie","Gestion","Sciences de la Terre","Sociologie, demographie","Sciences agronomiques","Histoire du droit","Sciences du langage - linguistique","Géosciences","Histoire moderne et contemporaine","Biochimie et biologie moléculaire","Sciences biologiques","Études germaniques","Histoire contemporaine","Études anglaises","Sciences et génie des matériaux","Littérature générale et comparée","Sciences biologiques et fondamentales appliquées","Énergétique","Anthropologie sociale et ethnologie","Chimie organique, minérale, industrielle","Sciences de la Vie et de la Santé","Sciences biologiques","Philosophie, epistemologie","Sciences et technologie industrielles","Droit international","Biologie des populations et écologie","Microélectronique","Langue et littérature françaises","Energétique","Archeologie, ethnologie, prehistoire","Matériaux","Lettres modernes","Chimie analytique","Mécanique et énergétique","Dynamique des fluides","Odontologie","Medecine","Littératures française et francophone","Mathématiques et applications","Génie industriel","Science des matériaux","Microbiologie","Pharmacologie","Physique théorique","Sciences de l'ingénieur","Physique des particules","Sciences chimiques","Architecture","Études ibériques","Physico-Chimie de la Matière Condensée","Robotique","Sciences religieuses","Génie Civil","Génie des matériaux","Chimie physique","Économie","Sciences appliquées. Physique","Informatique et applications","Astrophysique","Sciences cognitives","Traitement du signal et des images","Génétique","Nanophysique","Recherche clinique, innovation technologique, sante publique","Milieux denses et matériaux","Sciences des religions","Génie biologique et médical","Littérature et civilisation françaises","Géophysique","Traitement du signal et télécommunications","Mecanique des fluides, energetique, thermique, combustion, acoustique","Etudes anglophones","Mécanique des solides, des matériaux, des structures et des surfaces","Géologie","Lettres, sciences humaines et sociales","Physiologie et physiopathologie","Informatique et réseaux","Sciences naturelles","Histoire du droit et des institutions","Musique et musicologie","Physiologie","Chimie Physique","Matériaux, mécanique, génie civil, électrochimie","Matériaux, Mécanique, Génie civil, Electrochimie","Droit Public","?","Arts plastiques","Génie des procédés et des produits","Sciences Economiques","Mathématiques pures","Traitement du signal et télécommunications","Électronique, optronique et systèmes","Science et génie des matériaux","Astronomie et Astrophysique","Histoire médiévale","Études grecques","Électrochimie","Genie electrique","Biochimie. Biologie moléculaire et cellulaire","Études nord-américaines","Staps","Anglais","Litterature generale et comparee","Informatique, données, IA","Études latines","Cinéma","Chimie théorique, physique, analytique","Études théâtrales","Préhistoire","Sciences de l'information","Études italiennes","Cancérologie","Mathématiques fondamentales","Sciences de l'environnement"),selected = "_"))),
                       div(style="display: inline-block;vertical-align:top;width: 49%;",conditionalPanel(condition="input.doc_type == 36",
                                                                                                         div(style = "margin-top: -15px"),
                                                                                                         selectInput("isidore", "Discipline",choices = list("_","Archéologie et Préhistoire"="archeo","Architecture, aménagement de l'espace"="archi","Art et histoire de l'art"="art","Anthropologie biologique"="anthro-bio","Etudes classiques"="class","Sciences de l'information et de la communication"="info","Héritage culturel et muséologie"="museo","Démographie"="demo","Economies et finances"="eco","Education"="edu","Etudes de l'environnement"="envir","Etudes sur le genre"="genre","Géographie"="geo","Histoire"="hist","Histoire, Philosophie et Sociologie des sciences"="hisphilso","Droit"="droit","Linguistique"="langue","Littératures"="litt","Gestion et management"="gestion","Méthodes et statistiques"="stat","Musique, musicologie et arts de la scène"="musiq","Philosophie"="phil","Science politique"="scipo","Psychologie"="psy","Religions"="relig","Anthropologie sociale et ethnologie"="anthro-se","Sociologie"="socio"),selected = "_")))
      ),
      div(style = "margin-top: -15px"),
      conditionalPanel(condition="input.doc_type == 4",fileInput('target_upload','', 
                                                                 accept = c(
                                                                   'text/csv',
                                                                   'text/comma-separated-values',
                                                                   '.csv'
                                                                 ),buttonLabel='Importer', placeholder='un rapport de recherche')),
      div(style="display: inline-block;vertical-align:bottom;width: 49%;",conditionalPanel(condition = "input.doc_type != 30 && input.doc_type != 31",numericInput("beginning","Début",1914))),
      div(style="display: inline-block;vertical-align:bottom;width: 49%;",conditionalPanel(condition = "input.doc_type != 30 && input.doc_type != 31",numericInput("end","Fin",1920))),
      conditionalPanel(condition = "input.doc_type == 30 || input.doc_type == 31",div(style = "margin-top: -15px")),
      conditionalPanel(condition = "input.doc_type == 30 || input.doc_type == 31", dateRangeInput('dateRange',
                                                                                                  label = '\n',
                                                                                                  start = as.Date.character("2021-01-01"), end = as.character(Sys.Date()),
                                                                                                  separator="à", startview = "decade")),
      div(style="display: inline-block;vertical-align:top;width: 49%;",radioButtons("resolution", label = "Résolution", choices = c("Année","Mois"),inline=T)),
      div(style="display: inline-block;vertical-align:top;width: 49%;",conditionalPanel(condition="input.doc_type == 2 && input.search_mode==1",uiOutput("theme"))),
      conditionalPanel(condition="input.search_mode == 3 || input.doc_type == 1 || (input.doc_type == 3 && input.search_mode == 1) || input.doc_type == 5 || input.doc_type == 6 || input.doc_type == 7 || input.doc_type == 8 || input.doc_type == 9 || input.doc_type == 10 || input.doc_type == 11 || input.doc_type == 12 || input.doc_type == 13 || input.doc_type == 14 || input.doc_type == 15 || input.doc_type == 16 || input.doc_type == 17 || input.doc_type == 18 || input.doc_type == 19 || input.doc_type == 20 || input.doc_type == 21 || input.doc_type == 22 || input.doc_type == 23 || input.doc_type == 24 || input.doc_type == 25 || input.doc_type == 26 || input.doc_type == 27 || input.doc_type == 28 || input.doc_type == 29 || input.doc_type == 30 || input.doc_type == 31 || input.doc_type == 32 ||input.doc_type == 33 ||input.doc_type == 34 ||input.doc_type == 35 ||input.doc_type == 36 ||input.doc_type == 37 ||input.doc_type == 38 ||input.doc_type == 39 ||input.doc_type == 40 || (input.doc_type == 2 && input.search_mode == 1) || (input.doc_type == 4 && output.fileUploaded == 1 && output.avertissement.includes('Modifiez')==false) || ((input.doc_type == 2 || (input.doc_type == 3  && input.titres.length <= 15)) && input.search_mode == 2 && output.avertissement.includes('Modifiez')==false)",actionButton("do","Générer le graphique"))
      
    )),
    
        div(plotlyOutput("plot")),
    div(
           conditionalPanel(condition="(input.doc_type==1 || input.doc_type==2) && input.search_mode == 3 && input.joker == 1",switchInput(inputId = "histoJoker",size = "mini",label = "Dynamique",value=F)),
           sliderInput("span","Lissage de la courbe",min = 0,max = 10,value = 0),
           p(""),
           div(style="display: inline-block;vertical-align:bottom",downloadButton('downloadData', 'Données')),
           div(style="display: inline-block;vertical-align:bottom",downloadButton('downloadPlot', 'Graphique interactif')),
           div(style="display: inline-block;vertical-align:bottom",downloadButton('downloadSPlot', 'Graphique scientifique')),
           p(""),
           
           h2(textOutput("currentTime"), style="color:white")
           )
  )))