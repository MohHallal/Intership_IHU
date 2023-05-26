library(shiny)
shinyServer(function(input, output, session) {
  source('add_subset.R') # Le script qui a les fonctions qui effectuent l'ajout des echantillons à la base des données
  source('Delete_sample_mysql.R') # Le script qui a les fonctions qui effectuent la suppression d'un echantillon
  source('Interrogat.R') # Le script qui a les fonctions qui effectuent l'interrogation de la base des données en fonctions des parametres
   observeEvent(input$addButton, {
     # Le bouton 'Add the subset' (ajouter des échantillons à la BD)
               filePath <- input$file$datapath # le fichier qui contient les echantillons à ajouter à la BD
               message <- add_set(filePath)
               output$message_add <-renderText(message)
               # Mise à jour du tableau à afficher aprés l'ajout des échantillons
               data <- Interrogating()
               output$result <- renderTable({data})
               # Mise à jour de l'affichage de nombre de résultats aprés l'ajout des échantillons
               size <- nrow(data)
               message_size <- sprintf("%d result(s)",size)
               output$size_database <- renderText(message_size)
               # Mise à jour des diagramme aprés l'ajout des échantillons
               output$plot_family <- renderPlot({
                 frequency_family <-sort(table(data$Family), decreasing = TRUE)
                 pie(frequency_family, labels= paste(names(frequency_family), formatC(100*frequency_family/sum(frequency_family), format = "f", digits = 2), "%"), 
                     main = "Pie chart represents the percentage of each family", cex= 0.8, init.angle = 48, col = rainbow(length(names(frequency_family))))
               })
               output$plot_genus <- renderPlot({
                 frequency_genus <-sort(table(data$Genus), decreasing = TRUE)
                 pie(frequency_genus, labels = paste(names(frequency_genus), formatC(100 * frequency_genus / sum(frequency_genus), format = "f", digits = 2), "%"), 
                     main = "Pie chart represents the percentage of each genus", cex= 0.8, init.angle = 48, col = rainbow(length(names(frequency_genus))))
               })
               output$plot_body_part <- renderPlot({
                 frequency_body_part <-sort(table(data$Body_part), decreasing = TRUE)
                 pie(frequency_body_part, labels = paste(names(frequency_body_part), formatC(100 * frequency_body_part / sum(frequency_body_part), 
                format = "f", digits = 2), "%"), main = "Pie chart represents the percentage of each body part", cex= 1, init.angle = 48, 
                col = rainbow(length(names(frequency_body_part))))
               })
               output$plot_arthropod_type <- renderPlot({
                 frequency_arthropod_type <-sort(table(data$Arthropod_type), decreasing = TRUE)
                 pie(frequency_arthropod_type, labels= paste(names(frequency_arthropod_type),formatC(100 * frequency_arthropod_type/sum(frequency_arthropod_type), 
                     format = "f", digits = 2), "%"), main = "Pie chart represents the percentage of each arthropod type", cex= 1, init.angle = 48, 
                     col = rainbow(length(names(frequency_arthropod_type))))
               })
                })
   observeEvent(input$deleteButton, {
     # Le bouton 'Delete the sample" (supprimer un échantillon de la BD)
                message <- deleter(input$sample_name) # Suppression de l'echantillon
                output$message_delete <-renderText(message)
                # Mise à jour de la table à afficher aprés la suppression de l'echantillon
                data <- Interrogating()
                output$result <- renderTable({data})
                # Mise à jour de l'affichage de nombre de résultats aprés la suppression d'un échantillon
                size <- nrow(data)
                message_size <- sprintf("%d result(s)",size)
                output$size_database <- renderText(message_size)
                # Mise à jour des diagramme aprés la suppression
                output$plot_family <- renderPlot({
                  frequency_family <-sort(table(data$Family), decreasing = TRUE)
                  pie(frequency_family, labels = paste(names(frequency_family), formatC(100 * frequency_family / sum(frequency_family), 
                  format = "f", digits = 2), "%"), main = "Diagram chart represents the percentage of each family", cex= 0.8, init.angle = 48, 
                  col = rainbow(length(names(frequency_family))))
                })
                output$plot_genus <- renderPlot({
                  frequency_genus <-sort(table(data$Genus), decreasing = TRUE)
                  pie(frequency_genus, labels= paste(names(frequency_genus),formatC(100 * frequency_genus / sum(frequency_genus), format = "f", digits = 2), "%"),
                      main = "Diagram chart represents the percentage of each genus", cex= 0.8, init.angle = 48, col = rainbow(length(names(frequency_genus))))
                })
                output$plot_body_part <- renderPlot({
                  frequency_body_part <-sort(table(data$Body_part), decreasing = TRUE)
                  pie(frequency_body_part, labels = paste(names(frequency_body_part), formatC(100 * frequency_body_part / sum(frequency_body_part), 
                  format = "f", digits = 2), "%"), main = "Diagram chart represents the percentage of each body part", cex= 1, init.angle = 48, 
                  col = rainbow(length(names(frequency_body_part))))
                })
                output$plot_arthropod_type <- renderPlot({
                  frequency_arthropod_type <-sort(table(data$Arthropod_type), decreasing = TRUE)
                  pie(frequency_arthropod_type, labels= paste(names(frequency_arthropod_type),formatC(100 * frequency_arthropod_type/sum(frequency_arthropod_type), 
                  format = "f", digits = 2), "%"), main = "Diagram chart represents the percentage of each arthropod type", cex= 1, init.angle = 48, 
                  col = rainbow(length(names(frequency_arthropod_type))))
                })
               })
   observeEvent(input$filter, {
     # Le bouton 'Filter' (pour interroger la BD)
     data <- Interrogating(Sample_name = input$Sample_Name, Genus = input$genus, Species = input$species, Arthropod_type = input$arthropod_type, 
                           Body_part = input$body_part, Developmental_stage = input$dvlpt_stage, Protocol_ID = input$protocol_id, Location_ID = input$location_id, 
                           Family = input$family)
     output$result <- renderTable({data})
     size <- nrow(data)
     message_size <- sprintf("%d result(s)",size)
     output$size_database <- renderText(message_size)
     output$plot_family <- renderPlot({
       frequency_family <-sort(table(data$Family), decreasing = TRUE)
       pie(frequency_family, labels = paste(names(frequency_family), formatC(100 * frequency_family / sum(frequency_family), format = "f", digits = 2), "%"), 
           main = "Diagram chart represents the percentage of each family", cex= 0.8, init.angle = 48, col = rainbow(length(names(frequency_family))))
     })
     output$plot_genus <- renderPlot({
       frequency_genus <-sort(table(data$Genus), decreasing = TRUE)
       pie(frequency_genus, labels = paste(names(frequency_genus), formatC(100 * frequency_genus / sum(frequency_genus), format = "f", digits = 2), "%"), 
           main = "Diagram chart represents the percentage of each genus", cex= 0.8, init.angle = 48, col = rainbow(length(names(frequency_genus))))
     })
     output$plot_body_part <- renderPlot({
       frequency_body_part <-sort(table(data$Body_part), decreasing = TRUE)
       pie(frequency_body_part, labels= paste(names(frequency_body_part),formatC(100 * frequency_body_part/sum(frequency_body_part), format = "f", digits= 2),"%"), 
           main = "Diagram chart represents the percentage of each body part", cex= 1, init.angle = 48, col = rainbow(length(names(frequency_body_part))))
     })
     output$plot_arthropod_type <- renderPlot({
       frequency_arthropod_type <-sort(table(data$Arthropod_type), decreasing = TRUE)
       pie(frequency_arthropod_type, labels = paste(names(frequency_arthropod_type), formatC(100 * frequency_arthropod_type / sum(frequency_arthropod_type), 
        format = "f", digits = 2), "%"), main = "Diagram chart represents the percentage of each arthropod type", cex= 1, init.angle = 48, 
        col = rainbow(length(names(frequency_arthropod_type))))
     })
   })
   observeEvent(input$reset, {
     # Le bouton 'reset' (pour mettre à zéro)
     data <- Interrogating()
     output$result <- renderTable({data})
     updateTextInput(session, "Sample_Name",value = NA)
     updateTextInput(session, "family",value = NA)
     updateTextInput(session, "genus",value = NA)
     updateTextInput(session, "species",value = NA)
     updateTextInput(session, "arthropod_type",value = NA)
     updateTextInput(session, "body_part",value = NA)
     updateTextInput(session, "dvlpt_stage",value = NA)
     updateTextInput(session, "protocol_id",value = 0)
     updateTextInput(session, "location_id",value = 0)
     size <- nrow(data)
     message_size <- sprintf("%d result(s)",size)
     output$size_database <- renderText(message_size)
     output$plot_family <- renderPlot({
       frequency_family <-sort(table(data$Family), decreasing = TRUE)
       pie(frequency_family, labels = paste(names(frequency_family), formatC(100 * frequency_family / sum(frequency_family), 
        format = "f", digits = 2), "%"), main = "Diagram chart represents the percentage of each family", cex= 0.8, init.angle = 48, 
        col = rainbow(length(names(frequency_family))))
     })
     output$plot_genus <- renderPlot({
       frequency_genus <-sort(table(data$Genus), decreasing = TRUE)
       pie(frequency_genus, labels = paste(names(frequency_genus), formatC(100 * frequency_genus / sum(frequency_genus), format = "f", digits = 2), "%"), 
           main = "Diagram chart represents the percentage of each genus", cex= 0.8, init.angle = 48, col = rainbow(length(names(frequency_genus))))
     })
     output$plot_body_part <- renderPlot({
       frequency_body_part <-sort(table(data$Body_part), decreasing = TRUE)
       pie(frequency_body_part, labels=paste(names(frequency_body_part), formatC(100*frequency_body_part/sum(frequency_body_part), format = "f", digits = 2), "%"), 
           main = "Diagram chart represents the percentage of each body part", cex= 1, init.angle = 48, col = rainbow(length(names(frequency_body_part))))
     })
     output$plot_arthropod_type <- renderPlot({
       frequency_arthropod_type <-sort(table(data$Arthropod_type), decreasing = TRUE)
       pie(frequency_arthropod_type, labels = paste(names(frequency_arthropod_type), formatC(100 * frequency_arthropod_type / sum(frequency_arthropod_type), 
          format = "f", digits = 2), "%"), main = "Diagram chart represents the percentage of each arthropod type", cex= 1, init.angle = 48, 
          col = rainbow(length(names(frequency_arthropod_type))))
     })
   })
   output$plot_family <- renderPlot({
     frequency_family <-sort(table(data$Family), decreasing = TRUE)
     pie(frequency_family, labels = paste(names(frequency_family), formatC(100 * frequency_family / sum(frequency_family), format = "f", digits = 2), "%"), 
         main = "Diagram chart represents the percentage of each family", cex= 0.8, init.angle = 48, col = rainbow(length(names(frequency_family))))
   })
   output$plot_genus <- renderPlot({
     frequency_genus <-sort(table(data$Genus), decreasing = TRUE)
     pie(frequency_genus, labels = paste(names(frequency_genus), formatC(100 * frequency_genus / sum(frequency_genus), format = "f", digits = 2), "%"), 
         main = "Diagram chart represents the percentage of each genus", cex= 0.8, init.angle = 48, col = rainbow(length(names(frequency_genus))))
   })
   output$plot_body_part <- renderPlot({
     frequency_body_part <-sort(table(data$Body_part), decreasing = TRUE)
     pie(frequency_body_part, labels=paste(names(frequency_body_part), formatC(100*frequency_body_part/sum(frequency_body_part), format = "f", digits = 2), "%"), 
         main = "Diagram chart represents the percentage of each body part", cex= 1, init.angle = 48, col = rainbow(length(names(frequency_body_part))))
   })
   output$plot_arthropod_type <- renderPlot({
     frequency_arthropod_type <-sort(table(data$Arthropod_type), decreasing = TRUE)
     pie(frequency_arthropod_type, labels = paste(names(frequency_arthropod_type), formatC(100 * frequency_arthropod_type / sum(frequency_arthropod_type), 
    format = "f", digits = 2), "%"), main = "Diagram chart represents the percentage of each arthropod type", cex= 1, init.angle = 48, 
    col = rainbow(length(names(frequency_arthropod_type))))
   })
   # Les lignes d'aprés font le premier affichage du tableau lorsqu'on lance l'application^
   data <- Interrogating()
   output$result <- renderTable({data})
   size <- nrow(data)
   message_size <- sprintf("%d result(s)",size)
   output$size_database <- renderText(message_size)
        })
