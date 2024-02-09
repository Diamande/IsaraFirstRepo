library(shiny)

ui <- fluidPage(
  titlePanel("Lance le dé pour changer la couleur des cases"),  # Titre de la page
  br(),  # Saut de ligne
  mainPanel( 
    actionButton("roll", "Lancer le dé"),  # Bouton pour lancer le dé
    br(),  # Saut de ligne
    br(),  
    br(), 
    br(),  
    uiOutput("caseButtons")  # Affichage des cases
  )
)

server <- function(input, output, session) {
  # Fonction pour générer une couleur aléatoire
  roll_die <- function() {
    sample(c("red", "blue", "green", "yellow", "orange"), 1)
  }
  
  # Liste pour stocker les couleurs pour chaque bouton de couleur
  case_buttons <- list()
  
  # Initialisation des boutons de couleur
  for (i in 0:5) {
    # Spécifier la taille des boutons initiaux
    case_buttons[[as.character(i)]] <- reactiveVal(paste("width:100px; height:100px; background-color: grey; border: 1px solid black;"))
  }
  
  # Changements de couleur des boutons et les afficher
  output$caseButtons <- renderUI({
    buttons <- lapply(0:5, function(i) {
      actionButton(paste0("case", i), "", style = case_buttons[[as.character(i)]]())  # Création d'une liste de boutons, un pour chaque case
    })
    do.call(tagList, buttons)  # Assembler tous les boutons en une liste pour les afficher ensemble
  })
  
  # Clics sur le bouton pour lancer le dé
  observeEvent(input$roll, {
    roll_result <- input$roll %% 6  # Résultat du lancer de dé
    if (roll_result == 0) roll_result <- 6  # Si le résultat est 0, le dé revient à 6
    # Mettre à jour la couleur du bouton actuel
    current_case <- (roll_result - 1) %% 6  # Calcul de la case actuelle
    case_buttons[[as.character(current_case)]](paste("width:100px; height:100px; background-color:", roll_die(), "; border: 1px solid black;"))  # Mise à jour du style pour la case actuelle
  })
}

# Lancement de l'application
shinyApp(ui, server)