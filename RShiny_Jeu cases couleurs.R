library(shiny)

ui <- fluidPage(
  titlePanel("Lance le dé pour avancer et changer la couleur des cases"),  # Titre
  br(),  # Saut de ligne
  mainPanel( 
    actionButton("roll", "Lancer le dé"),  # Bouton pour lancer le dé
    br(),  # Saut de ligne
    br(),  
    br(), 
    br(),  
    uiOutput("caseButtons"),  # Affichage des cases
    textOutput("roll_result_text"),  # Affichage de la case où on est
    textOutput("roll_text") # Affichage du roll du dé (de 1 à 6)
  )
)

server <- function(input, output, session) {
  # Fonction pour générer une couleur aléatoire
  roll_die <- function() {
    sample(c("red", "blue", "green", "yellow", "orange"), 1)
  }
  
  # Nombre total de cases
  num_cases <- 198
  
  # Initialiser la position actuelle du joueur à la première case
  roll_result <- reactiveVal(1)
  
  # Liste pour stocker les couleurs pour chaque bouton de couleur
  case_buttons <- list()
  
  # Initialisation des boutons de couleur
  for (i in 1:num_cases) {
    # Spécification de la taille, couleur des boutons initiaux
    case_buttons[[as.character(i)]] <- reactiveVal(paste("width:15px; height:15px; background-color: grey; border: 1px solid black;"))
  }
  
  # Changements de couleur des boutons et les afficher
  output$caseButtons <- renderUI({
    buttons <- lapply(1:num_cases, function(i) { #créer une liste de boutons représentant chaque case du jeu
      #change la couleur là où l'utilisateur est en fonction de la position d'avant et le dé fait
      style <- ifelse(i == roll_result(), paste("width:15px; height:15px; background-color:", roll_die(), "; border: 1px solid black;"), paste("width:15px; height:15px; background-color: grey; border: 1px solid black;"))
      actionButton(paste0("case", i), "", style = style)  
    })
    do.call(tagList, buttons)  # Assembler tous les boutons en une liste pour les afficher ensemble
  })
  
  # Lorsqu'on clique sur le bouton pour lancer le dé
  observeEvent(input$roll, {
    roll_value <- sample(1:6, 1)  # Lancer un dé de 1 à 6
    roll_result(roll_result() + roll_value)  # Mettre à jour la position avec la position d'avant et le dé fait
    output$roll_text <- renderText({
      paste("Vous avez fait le roll :", roll_value) #renseigne par un texte le roll fait
    })
    
    # Vérifier si le joueur a gagné
    if (roll_result() >= num_cases) {
      showModal(modalDialog(
        title = "Félicitations !",
        "Vous avez gagné !",
        footer = tagList(  # Création d'une liste de boutons pour le pied de la fenêtre modale
          actionButton("restart", "Recommencer"),  # Bouton pour recommencer le jeu
          actionButton("quit", "Quitter")  # Bouton pour quitter le jeu
        ),
        easyClose = TRUE  # Permet de fermer la fenêtre apparue en cliquant à l'extérieur
      ))
    }
  })
  
  # Action quand le joueur clique sur "Recommencer"
  observeEvent(input$restart, {
    roll_result(1)  # Réinitialiser la position à la première case
    removeModal()  # Fermer la fenêtre apparue
  })
  
  # Action quand le joueur clique sur "Quitter"
  observeEvent(input$quit, {
    showModal(modalDialog(  # Afficher une nouvelle fenêtre modale pour confirmer la sortie
      title = "Confirmation",  # Titre de la fenêtre modale
      "Êtes-vous sûr de vouloir quitter ?",  # Message de confirmation
      footer = tagList(  # Création d'une liste de boutons pour le pied de la fenêtre modale
        actionButton("cancel", "Annuler"),  # Bouton pour annuler la sortie
        actionButton("confirm_quit", "Quitter définitivement")  # Bouton pour confirmer la sortie
      ),
      easyClose = TRUE  # Permet de fermer la fenêtre modale en cliquant à l'extérieur
    ))
  })
  
  # Action quand le joueur confirme qu'il veut quitter
  observeEvent(input$confirm_quit, {
    session$close()  # Fermer l'application
  })
  
  # Action quand le joueur annule la demande de quitter
  observeEvent(input$cancel, {
    removeModal()  # Fermer la fenêtre apparue
  })
  
  # Afficher le résultat du dé
  output$roll_result_text <- renderText({
    paste("Vous êtes à la case :", roll_result()) #renseigne par un texte la case où le joueur se trouve
  })
}

# Lancement de l'application
shinyApp(ui, server)
