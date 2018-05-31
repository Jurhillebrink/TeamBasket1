
#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(dplyr)
library(shiny)
library(shinyjs)
library(shinydashboard)
library(ggplot2)
library(openssl)
library(shinyjs) #to hide side bar
library(grid) # for rastergrob, to set court as background of plot
library(mailR)
library(plotly)
library(tidyr)

require("DT")

source("./global.R")

shinyServer(function(input, output, session) {
  #On app start
  observe({
    getShotResults()
    #Render the UI
    renderAdmin()
    renderPublicEvent()
    renderHeatMap()
    #renderPlayerEvent()
    renderLastEvent()
    renderPlayerInfo()
    renderHeatmap()
    renderAnalyses()
    
  })
  
  #Method to render after logging in
  loginRender <- function(){
    renderPlayerInfo()
    renderLastEvent()
    renderHeatmap()
    renderAnalyses
   
  }
  
  savedPdf <<- NULL
  values <- reactiveValues(authenticated = FALSE)
  
  # Return the UI for a modal dialog with data selection input. If 'failed'
  # is TRUE, then display a message that the previous value was invalid.
  dataModal <- function(failed = FALSE) {
    modalDialog(
      img(src='logo.jpg', align = "right", width = 160),
      textInput("uiUsername", "Username:"),
      passwordInput("uiPassword", "Password:"),
      textOutput('warning'),
      tags$head(tags$style("#warning{color: red;}")),
      footer = tagList(hidden(actionButton("forgotPass", "Forgot your password?", style = "float:left")), actionButton("ok", "Login"))
    )
  }
  
  dataModal1 <- function(failed = FALSE) {
    modalDialog(
      img(src='logo.jpg', align = "right", width = 100),
      span("Enter your email and a new password will be sent."),
      textInput("uiEmail", "Email:"),
      textOutput('warningPassForgot'),
      tags$head(tags$style("#warningPassForgot{color: red;}")),
      footer = tagList(actionButton("back", "Back", style = "float:left"), actionButton("sendMail", "Send mail"))
    )
  }
  
  # Show modal when button is clicked.
  # This `observe` is suspended only whith right user credential
  obs1 <- observe({
    showModal(dataModal())
  })
  
  #check if email is valid by regexp
  isValidEmail <- function(x) {
    grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", as.character(x), 
          ignore.case=TRUE)
  }
  
  #send an email with new password if the mailaddres is correct
  observeEvent(input$sendMail, {
    if(isValidEmail(input$uiEmail)){ 
      output$warningPassForgot <-
        renderText({
          paste("")
        })
      samp<-c(1:9,letters,LETTERS,"!")
      newPass <- paste(sample(samp,8),collapse="")
      
      query <- paste0(
        "exec GETACCOUNTWITHEMAIL
        @email = ?email"
      )
      sql <- sqlInterpolate(conn, query, email = input$uiEmail)
      rs <- dbGetQuery(conn, sql)
      if(nrow(rs) == 1){
        newPassEncrypt <- toString(sha256(toString(newPass), key = NULL))
        query <- paste0(
          "exec UPDATEPASSWORDS
        @ACCOUNTID = ?accountid,
        @PASSWORD = ?password,
        @UNHASHEDPASSWORD = ?unhashedpassword"
        )
        sql <- sqlInterpolate(conn, query,
                              accountid = rs,
                              password = newPassEncrypt,
                              unhashedpassword = newPass)
        dbSendUpdate(conn, sql)
        updateTextInput(session, "uiUsername", value = input$uiEmail)
        removeModal()
        showModal(dataModal())
        send.mail(from = "passresetcto@gmail.com",
                  to = input$uiEmail,
                  subject = "Password reset",
                  body = paste0("Hello,<br> You have requested a new password.<br><br>This is your new password: ", newPass, "<br><br>Kind regards"),
                  smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = "passresetcto@gmail.com", passwd = "z0ik3Rt9", ssl = TRUE), authenticate = TRUE,
                  html = TRUE,
                  send = TRUE)
      }else{
        removeModal()
        showModal(dataModal())
      }
    }else{
      output$warningPassForgot <-
        renderText({
          paste("No valid email address!")
        })
    }
  })
  
  # When OK button is pressed, attempt to authenticate. If successful,
  # remove the modal.
  obs2 <- observe({
    req(input$ok)
    isolate({
      username <- input$uiUsername
      password <- input$uiPassword
    })
    #Encrypt the password
    password = toString(sha256(password, key = NULL))

    query <- paste0(
      "exec LOGIN 
        @USERNAME = ?email, 
        @PASSWORD = ?pass"
    )
    
    sql <- sqlInterpolate(conn, query, email = username, pass = password)
    rs <- dbGetQuery(conn, sql)
    
    #If result is 1 row long the user is authenticated
    if (nrow(rs) == 1) {
      print("Logged In")
      currentUser <<- rs
      getShotResults()
      Logged <<- TRUE
      values$authenticated <- TRUE
      obs1$suspend()
      removeModal()
      
      getShotResults()
      
      #no more warning for wrong credentials
      output$warning <-
        renderText({
          paste("")
        })
      
      checkData()
      loginRender()
      
    } else {
      #User not authenticated
      values$authenticated <- FALSE
      print("Wrong Credentials")
      output$warning <-
        renderText({
          paste("Credentials are wrong")
        })
    }
    
  })
  
  #show forgot password modal
  obs2 <- observe({
    req(input$forgotPass)
    showModal(dataModal1())
  })  
  
  #back button in forgot password modal to login modal
  obs3 <- observe({
    req(input$back)
    showModal(dataModal())
  }) 
  
  
  output$teammates <- renderDataTable(teammates)
  
  
  output$dataInfo <- renderPrint({
    if (values$authenticated)
      "OK!!!!!"
    else
      "You are NOT authenticated"
  })
  
  ################################################################################
  
  #Buttons for plus minus
  observeEvent(input$total_minus, {
    if(input$total > 0){ 
      new <- input$total - 1
      updateNumericInput(session, "total", value = new)
    }
  })
  
  observeEvent(input$total_plus, {
    new <- input$total + 1
    updateNumericInput(session, "total", value = new)
  })
  
  observeEvent(input$succeed_minus, {
    if(input$succeed > 0){
      new <- input$succeed - 1
      updateNumericInput(session, "succeed", value = new)
    }
  })
  
  observeEvent(input$succeed_plus, {
    if(input$succeed < input$total){
      new <- input$succeed + 1
      updateNumericInput(session, "succeed", value = new)
    }
  })
  
  ################################################################################
  #Modaldialog for the confirm input dialog.
  
  confirminputmodal <- modalDialog(
      tags$h2("Please make sure your values are correct!"),
      tags$h3("Name"),
      tags$b(tags$h4(textOutput('confirmName'))),
      tags$head(tags$style("#confirmName{color: #009900;}")),
      tags$h3("Type"),
      tags$b(tags$h4(textOutput('confirmType'))),
      tags$head(tags$style("#confirmType{color: #009900;}")),
      tags$h3("Score"),
      tags$b(tags$h4(textOutput('confirmScore'))),
      tags$head(tags$style("#confirmScore{color: #009900;}")),
      tags$h3("Position"),
      tags$b(tags$h4(textOutput('confirmPosition'))),
      tags$head(tags$style("#confirmPosition{color: #009900;}")),
      footer = tagList(
        actionButton("cancelConfirm", "Cancel"),
        actionButton("confirmValues", "Confirm, Next player")
      )
    )
  
  #Add the values to the modal dialog
  confirminputrender <- function(){
    titles <- c("Free throw","Catch & Shoot", "From dribble")
    values <- c("free_throw","catch_shoot","dribble")
    df <- data.frame(x = values,
    y = titles)
    
    rown <- which(df$x == input$typeselector)
    
    titleName <- titles[rown]
    
    if(input$typeselector == "free_throw"){
      inputpositiondialog <- 0;
    }else{
      inputpositiondialog <- input$sliderPosition;
    }
    
    output$confirmName <- renderText({ paste(allPlayers[allPlayers$accountid == input$radio,]$firstname,allPlayers[allPlayers$accountid == input$radio,]$lastname, sep=" " ) })
    output$confirmType <- renderText({ paste(titleName) })
    output$confirmScore <- renderText({ paste(input$succeed, input$total, sep="/") })
    output$confirmPosition <- renderText({ paste(inputpositiondialog) })
  }
  
  #On confirm shot input.
  observeEvent(input$insertShotbtn, {
    if(input$succeed <= input$total){
      
      confirminputrender()

      showModal(confirminputmodal)
    }else {
      showModaldal(
      modalDialog(
        title = "Invalid data.",
        "Succeeded shots exceeded total shots.",
        easyClose = TRUE,
        footer = NULL
      )
      )
    }
    
  })
  
  #Cancel the input.
  observeEvent(input$cancelConfirm, {
    removeModal()
  })
  
  #Call the stored procedure to input the actual data in the database.
  observeEvent(input$confirmValues, {
    if(input$typeselector == "free_throw"){
      inputposition <- 0;
    }else{
      inputposition <- input$sliderPosition;
    }
    
    query <- paste0(
      "exec INSERTSHOTVALUES 
      @USERID = ?userid,
      @EVENTID = ?eventid,
      @SUCCEED = ?succeed,
      @TOTAL = ?total,
      @POSITION = ?position,
      @TYPE = ?type"
    )
    sql <- sqlInterpolate(conn, query, 
                          userid = input$radio, 
                          eventid = latestEventid,
                          succeed = input$succeed,
                          total = input$total,
                          position = inputposition,
                          type = input$typeselector)
    
    dbSendUpdate(conn, sql)

    removeModal()
    show("vinkje")
    Sys.sleep(2)
    hide("vinkje")
    
  })

  
  #On logout button click
  observeEvent(input$logoutBtn, {
    #Check if user is in an event
    if(inEvent == FALSE){
      #Clear the menu and remove auth values, also reload the session.
      output$menu <- renderMenu({
        sidebarMenu(menuItem(""))
      })
      
      values$authenticated <- FALSE
      obs1 <- observe({
        showModal(dataModal())
      })
      session$reload()
    } else {
      #Show dialog when in a training.
      showModal(
        modalDialog(
          title = "Can't logout.",
          "Can't logout during a training session.",
          easyClose = TRUE,
          footer = NULL
        )
      )
    }
    
  })
  

  #to the next screen of test input
  observeEvent(input$switchtab, {
    if (!is.null(input$playersInEvent)) {
      query <- paste0(
        "exec STARTEVENT"
      )
      dbSendUpdate(conn, query)
      
      ###############################
      
      query <- paste0(
        "exec GETLASTEVENT"
      )
      rsevent <- dbGetQuery(conn, query)
      
      latestEventid <<- rsevent$eventid
      print(latestEventid)
      
      playersInEvent <<- input$playersInEvent
      for (player in input$playersInEvent){
        query <- paste0(
          "exec CREATEUSEREVENT 
          @ACCOUNTID = ?accountid,
          @EVENTID = ?eventid"
        )
        sql <- sqlInterpolate(conn, query, 
                              accountid = player, 
                              eventid = latestEventid)
        dbSendUpdate(conn, sql)
        
        print(paste("insert", player, latestEventid ,sep=" "))
      }
      
      lastnames <-
        allPlayers[allPlayers$accountid %in% playersInEvent, ]$fullname
      accountids <-
        allPlayers[allPlayers$accountid %in% playersInEvent, ]$accountid
      species <- data.frame(lastnames, accountids)
      eventPlayersChoices <-
        setNames(as.numeric(species$accountids), species$lastnames)
      updateRadioButtons(session, 'radio', choices = eventPlayersChoices)
      
      inEvent <<- TRUE# needed to prevent loggin out during event
      
      # change the menu during a session.
      # because there should not be able to switch pages
      output$menu <- renderMenu({
        sidebarMenu(hidden(
          menuItem(
            "InvoerSchoten",
            tabName = "InvoerSchoten2",
            icon = icon("plus-circle"),
            selected = TRUE
          )
        ),
        "Menu not available during a training.")
      })
      # hide menu
      shinyjs::addClass(selector = "body", class = "sidebar-collapse")
    } else {
      showModal(
        modalDialog(
          title = "Invalid data.",
          "No players were selected",
          easyClose = TRUE,
          footer = NULL
        )
      )
    }
  })
  
  #Modal to end the session
  endSessionModal <- modalDialog(
    passwordInput("eventPassword", "Password:"),
    textOutput('warningEvent'),
    tags$head(tags$style("#warningEvent{color: red;}")),
    footer = tagList(
      actionButton("cancelEnd", "Cancel"),
      actionButton("endEvent", "End")
    )
  )
  
  #to end a session
  observeEvent(input$closeTestEvent, {
    showModal(endSessionModal)
  })
  #Hide the modal
  observeEvent(input$cancelEnd, {
    removeModal()
  })
  
  #show players when a team is selected
  observeEvent(input$teamSelected,{
    result <- subset(allTeams, teamcode == input$teamSelected)
    if(!empty(result)){
      result <- allPlayers%>%
        select(accountid, teamid) %>%
        filter(teamid == as.numeric(result["teamid"]))
      updateSelectizeInput(session, "playersInEvent", selected = setNames(as.numeric(result$accountid), result$accountid))
    }
  })
  
  
  
  # refresh the player list in a event
  observeEvent(input$refreshPlayers, {
    getAllPlayers()
    query <- paste0(
      "exec GETPLAYERSINEVENT 
      @EVENTID = ?eventid"
    )
    
    sql <- sqlInterpolate(conn, query,
                          eventid = latestEventid)
    
    rs <- dbGetQuery(conn, sql)
    
    playersInEvent <<- rs$accountid
    print(rs)
    renderPublicEvent()
    #session$reload()
    #shinyjs::reset("playerSelect")
  })
  
  #add a player during an event
  observeEvent(input$addPlayerInEvent, {
    for (player in input$addPlayers){
      query <- paste0(
        "exec CREATEUSEREVENT 
        @ACCOUNTID = ?accountid,
        @EVENTID = ?eventid"
      )
      sql <- sqlInterpolate(conn, query, 
                            accountid = player, 
                            eventid = latestEventid)
      dbSendUpdate(conn, sql)
      playersInEvent <- append(playersInEvent, input$addPlayers)
      renderPublicEvent()
    }  
  })
  
  # to end a event
  observeEvent(input$endEvent, {
    #check for pass of the trainer
    isolate({
      password <- input$eventPassword
    })

    password = toString(sha256(password, key = NULL))
    query <- paste0(
      "exec LOGIN 
      @USERNAME = ?email, 
      @PASSWORD = ?pass"
    )
    sql <- sqlInterpolate(conn, query, 
                          email = currentUser$email, 
                          pass = password)
    rs <- dbGetQuery(conn, sql)
    
    if (nrow(rs) == 1) {
      endEvent()
      getShotResults()
    } else {
      print("Wrong Password")
      output$warningEvent <-
        renderText({
          paste("Wrong password")
        })
    }
    
  })
  
  #Ending event
  endEvent <- function() {
    inEvent <<- FALSE
    query <- paste0(
      "exec ENDEVENT 
      @EVENTID = ?eventid"
    )
    sql <- sqlInterpolate(conn, query,
                          eventid = latestEventid)
    dbSendUpdate(conn, sql)
    #Render the menu again, but select the last event tab.
    output$menu <- renderMenu({
      sidebarMenu(
        menuItem(
          "InvoerSchoten",
          tabName = "InvoerSchoten1",
          icon = icon("plus-circle")
        ),
        menuItem(
          "Analyse players",
          icon = icon("bar-chart"),
          # menuSubItem(
          #   "Last event",
          #   tabName = "lastEventCoach",
          #   icon = icon("dribbble"),
          #   selected = TRUE # direct to last event page
          # ),
          menuSubItem(
            "Shot results",
            tabName = "shotAnalyse",
            icon = icon("bar-chart")
          )
        )
      )
    })
    # show menu
    shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
    removeModal()
    getShotResults()
    renderLastEvent()
  }
  
  ##############################################################
  
  #Insert the team
  observeEvent(input$insertTeam, {
    #Check if the teamcode is not empty
    if (input$teamcode != "") {
      
      isolate({
        teamcode <- input$teamcode
      })
      
      query <- paste0(
        "exec CREATETEAM 
        @TEAMCODE = ?code"
      )
      sql <- sqlInterpolate(conn, query, 
                            code = teamcode)
      dbSendUpdate(conn, sql)
      
      showModal(
        modalDialog(
          title = "Team added.",
          "The team was successfully added.",
          easyClose = TRUE,
          footer = NULL
        )
      )
      
      renderAdmin()
    } else {
      showModal(
        modalDialog(
          title = "Invalid data.",
          "Teamcode was empty.",
          easyClose = TRUE,
          footer = NULL
        )
      )
    }
  })
  
  #Insert the users
  observeEvent(input$insertUser, {
    isolate({
      email     <- input$useremail
      password  <- input$userpassword
      role      <- input$userrole
      firstname <- input$userfirstname
      lastname  <- input$userlastname
      birthday  <- input$userbirthday
      phone     <- input$userphonenumber
      teamid    <- as.numeric(input$userteam)
    })
    
    password = toString(sha256(password, key = NULL))
    birthday = toString(birthday)
    teamid   = as.numeric(teamid)
    
    print(password)
    
    if (input$useremail != "") {
      query <- paste0(
        "exec CREATEUSER
          @EMAIL = ?email,
          @PASSWORD = ?pass,
          @ROLE = ?role,
          @FIRSTNAME = ?fname,
          @LASTNAME = ?lname,
          @BIRTHDAY = ?bday,
          @PHONE = ?phone,
          @TEAMID = ?teamid"
      )
      print(query)
      sql <- sqlInterpolate(conn, query, 
                            email = email, 
                            pass = password, 
                            role = role, 
                            fname= firstname, 
                            lname = lastname,
                            bday= birthday,
                            phone = phone,
                            teamid = teamid)
      dbSendUpdate(conn, sql)
      
      showModal(
        modalDialog(
          title = "Account created.",
          "The user was added to the system.",
          easyClose = TRUE,
          footer = NULL
        )
      )
      renderAdmin()
    } else {
      showModal(
        modalDialog(
          title = "Invalid data.",
          "E-mail was empty.",
          easyClose = TRUE,
          footer = NULL
        )
      )
    }
    
  })
  
  
  
  
  ##############################################################
  
  checkData <- function() {
    print(currentUser$role)
    query <- paste0(
      "exec GETPLAYER 
      @TEAMID = ?teamid"
    )
    sql <- sqlInterpolate(conn, query, teamid = currentUser$teamid)
    rs <- dbGetQuery(conn, sql)
    teammates <<- rs
    
    output$menu <- renderMenu({
      if (currentUser$role == "a") {
        #User is an admin
        #Render the sidebar 
        sidebarMenu(tags$div(
          style="margin-top:180%; bottom: 0; position: fixed;",
          menuItem(
            img(src='logo_final.png', width = 230)
          )
        ),id = "tabs",
                    menuItem(
                      "Admin",
                      tabName = "admin",
                      icon = icon("database"),
                      selected = TRUE
                    ))
      } else if (currentUser$role == "c") {
        #User is a coach/trainer
        #Render the sidebar 
        sidebarMenu(
          menuItem(
            "Start Training",
            tabName = "InvoerSchoten1",
            icon = icon("plus-circle"),
            selected = TRUE
          ),
          menuItem(
            "Analyse",
            icon = icon("bar-chart"),
            menuSubItem(
              "Shot results",
              tabName = "shotAnalyse",
              icon = icon("bar-chart")
            ), 
            menuSubItem(
               "IPP Dashboard",
               tabName = "performance",
               icon = icon("dribbble")
             ),
            menuSubItem(
              "heatmapjur",
              tabName = "heatmapgoer",
              icon = icon("dribbble")
            )
            
            
          ), 
          tags$div(
            style="margin-top:180%; bottom: 0; position: fixed;",
            menuItem(
              img(src='logo_final.png', width = 230)
            )
          )
        )
      } else if (currentUser$role == "p") {
        #User is a player
        #Get the events of the current user.
        eventsOfPlayer     <<-
          rsShotResult[currentUser$accountid == rsShotResult$accountid, ]
        #Get all starttimes and format them
        eventsOfPlayer$starttime <- as.POSIXct(strptime(eventsOfPlayer$starttime, "%Y-%m-%d %H:%M:%S"))
        #order the events of the user by date
        eventSubset <<- eventsOfPlayer[order(eventsOfPlayer$starttime, decreasing = FALSE),]
        #Set the latest event
        latestEvent        <<-
          max(rsShotResult[currentUser$accountid == rsShotResult$accountid & rsShotResult$value != 0, ]$eventid, na.rm = TRUE)
        #Render the sidebar 
        sidebarMenu(
          tags$div(
            style="margin-top:180%; bottom: 0; position: fixed;",
            menuItem(
              img(src='logo_final.png', width = 230)
            )
          ),
          id = "tabs",
          menuItem(
            "Home",
            tabName = "homePlayer",
            icon = icon("home"),
            selected = TRUE
          ),
          menuItem("Analysis",
                   icon = icon("bar-chart"),
                   menuSubItem(
                     "Heatmap",
                     tabName = "heatMapPlayer",
                     icon = icon("thermometer-3")
                   )      
          )#,
          #menuItem(
          #  "Events",
          #  tabName = "playerEvent",
          #  icon = icon("plus-circle")
          #)
        )
      }
    })
    
    query <- paste0(
      "exec GETPLAYERLIST"
    )
    sql <- sqlInterpolate(conn, query)
    rs <- dbGetQuery(conn, sql)
    
    lastnames <- paste(rs$firstname, ' ' , rs$lastname)
    accountids <- rs$accountid
    species <- data.frame(lastnames, accountids)
    choicesSpecies <-
      setNames(as.numeric(species$accountids), species$lastnames)
    
    updateRadioButtons(session, 'radio', choices = choicesSpecies)
  }
  
  #Render the player info
  renderLastEvent <- function(){
    # get the id of the last event
    query <- paste0(
      "exec GETLASTDATE"
    )
    sql <- sqlInterpolate(conn, query)
    latestDate <- dbGetQuery(conn, sql)$starttime
    
    eventData <- rsShotResult[rsShotResult$starttime == latestDate,] # select dtata of last date
    
    # render the page
    output$last_event_coach <- renderUI({
      lastEventLayout(eventData, currentUser)
    })
    
    # render the player specific plot
    output$last_event_per_player <- renderPlot({
      eventDataSelectedPlayer <- eventData[eventData$fullname == input$select_player_last_event, ] # filter by player
      # combine on position and type
      eventDataSelectedPlayer <- with(eventDataSelectedPlayer,
                                     aggregate(
                                       list(
                                         totalTaken = as.integer(value2),
                                         totalMade = as.integer(value)
                                       ),
                                       list(
                                         accountid = accountid,
                                         fullname = fullname,
                                         position = value3,
                                         type = value4
                                       ),
                                       sum
                                     ))
      #calc %
      eventDataSelectedPlayer$percentage <- ((eventDataSelectedPlayer$totalMade/eventDataSelectedPlayer$totalTaken)*100)
      
      #make plot
      ggplot(eventDataSelectedPlayer,
          aes(x = position,
            y = percentage,
            fill = type)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(fill = 'Type of shot')+
      scale_y_continuous(limits = c(0, 100))
    })
    
    # render the position specific plot
    output$last_event_per_position <- renderPlot({
      
      selectedPosition <- input$select_position_last_event
      if(is.null(selectedPosition)){selectedPosition = 1}

      
      eventDataOfPosition <- eventData[eventData$value3 == selectedPosition,] # filter by selected position
      
      # combine on player and type
      eventDataOfPosition <-with(eventDataOfPosition,
                                 aggregate(
                                   list(
                                     totalTaken = as.integer(value2),
                                     totalMade = as.integer(value)
                                   ),
                                   list(
                                     accountid = accountid,
                                     firstname = firstname,
                                     fullname = fullname,
                                     type = value4
                                   ),
                                   sum
                                 ))
      # calc %
      eventDataOfPosition$percentage <- ((eventDataOfPosition$totalMade/eventDataOfPosition$totalTaken)*100)
      
      #make plot
      ggplot(eventDataOfPosition,
             aes(x = fullname,
                 y = percentage,
                 fill = type)) +
        geom_bar(stat = "identity", position = "dodge") +
        geom_text(aes(label=round(percentage, digits = 0)), position = position_dodge(width = 0.9) ,vjust=2)+
        labs(fill = 'Type of shot', x="Players")+
        theme(axis.text.x=element_text(angle = -45, hjust = 0))+
        scale_y_continuous(limits = c(0, 100))
    })
    
  }
  
  #Render the player info
  renderPlayerInfo <- function(){
    #Render the home layout for the current user.
    output$player_home <- renderUI({
      playerHomeLayout(currentUser)
    })
    
    #Render the select dialog for the training sessions.
    output$trainingSelectorOutput <- renderUI(
      renderTrainingSelector(setNames(as.numeric(eventSubset$eventid),eventSubset$starttime))
    )
    
    #Render the percentage text of the free throws
    output$freeThrowPercentage <- renderText({
      #Calculate the total made free throws of the training.
      made  <- sum(as.integer(eventSubset[eventSubset$eventid == input$trainingselector & eventSubset$value4 == "free_throw" ,]$value), na.rm = TRUE)
      #Calculate the total taken free throws of the training.
      taken <- sum(as.integer(eventSubset[eventSubset$eventid == input$trainingselector & eventSubset$value4 == "free_throw", ]$value2), na.rm = TRUE)
      #Calculate the percentage based on the made and taken shots.
      percentage <- as.integer((made / taken) * 100)
      #Print the actual percentage with the percentage sign to the text output.
      paste(percentage, "%", sep = "")
    })
    
    #Render the text below the percentage with the made and taken shots.
    output$freeThrowCount <- renderText({
      #Calculate the total taken free throws of the training.
      takenshots <- toString(sum(as.integer(eventSubset[eventSubset$eventid == input$trainingselector & eventSubset$value4 == "free_throw", ]$value), na.rm = TRUE))
      #Calculate the total made free throws of the training.
      madeshots  <- toString(sum(as.integer(eventSubset[eventSubset$eventid == input$trainingselector & eventSubset$value4 == "free_throw" ,]$value2), na.rm = TRUE))
      #Print the actual taken shots and the made shots with a slash sign between them to the text output.
      paste(takenshots, "/", madeshots , sep = "")
    })
    
    output$dribblePercentage <- renderText({
      made  <- sum(as.integer(eventSubset[eventSubset$eventid == input$trainingselector & eventSubset$value4 == "dribble" ,]$value), na.rm = TRUE)
      taken <- sum(as.integer(eventSubset[eventSubset$eventid == input$trainingselector & eventSubset$value4 == "dribble", ]$value2), na.rm = TRUE)
      percentage <- as.integer((made / taken) * 100)
      paste(percentage, "%", sep = "")
    })
    output$dribbleCount <- renderText({
      takenshots <- toString(sum(as.integer(eventSubset[eventSubset$eventid == input$trainingselector & eventSubset$value4 == "dribble", ]$value), na.rm = TRUE))
      madeshots  <- toString(sum(as.integer(eventSubset[eventSubset$eventid == input$trainingselector & eventSubset$value4 == "dribble" ,]$value2), na.rm = TRUE))
      paste(takenshots, "/", madeshots , sep = "")
    })
    
    output$catchThrowPercentage <- renderText({
      made  <- sum(as.integer(eventSubset[eventSubset$eventid == input$trainingselector & eventSubset$value4 == "catch_shoot" ,]$value), na.rm = TRUE)
      taken <- sum(as.integer(eventSubset[eventSubset$eventid == input$trainingselector & eventSubset$value4 == "catch_shoot", ]$value2), na.rm = TRUE)
      percentage <- as.integer((made / taken) * 100)
      paste(percentage, "%", sep = "")
    })
    output$catchThrowCount <- renderText({
      takenshots <- toString(sum(as.integer(eventSubset[eventSubset$eventid == input$trainingselector & eventSubset$value4 == "catch_shoot", ]$value), na.rm = TRUE))
      madeshots  <- toString(sum(as.integer(eventSubset[eventSubset$eventid == input$trainingselector & eventSubset$value4 == "catch_shoot" ,]$value2), na.rm = TRUE))
      paste(takenshots, "/", madeshots , sep = "")
    })
    
    output$home_graph_player <- renderPlot({
      dataPlayerHomeGraph <- rsShotResult[rsShotResult$value4 == input$typeselectorHomeGraph, ]
      dataPlayerHomeGraph[dataPlayerHomeGraph$accountid != currentUser$accountid,]$accountid <- 0
      dataPlayerHomeGraph[dataPlayerHomeGraph$accountid != currentUser$accountid,]$fullname <- "Team"
      
      dataPlayerHomeGraph <-
        with(dataPlayerHomeGraph,
             aggregate(
               list(
                 totalTaken = as.integer(value2),
                 totalMade = as.integer(value)
               ),
               list(
                 accountid = accountid,
                 fullname = fullname,
                 eventid = eventid,
                 eventdate = starttime
               ),
               sum
             ))
      dataPlayerHomeGraph$percentage <- (dataPlayerHomeGraph$totalMade/dataPlayerHomeGraph$totalTaken) *100
      
      ggplot(dataPlayerHomeGraph,
             aes(x = eventdate,
                 y = percentage,
                 group = fullname)) +
        geom_line((aes(color=fullname)))+ 
        geom_point((aes(color=fullname)), size=3)+
        theme(axis.text.x=element_text(angle = -45, hjust = 0))+
        scale_y_continuous(limits = c(0, 100))
    })

  }
  
  #RENDER THE ADMIN PAGE
  renderAdmin <- function() {
    query <- paste0(
      "exec GETTEAMLIST"
    )
    sql <- sqlInterpolate(conn, query)
    rs <- dbGetQuery(conn, sql)
    
    x <- setNames(as.numeric(rs$teamid), rs$teamcode)
    
    # Can use character(0) to remove all choices
    if (is.null(x))
      x <- character(0)
    
    output$admin_input <- renderUI({
      adminUiLayout(x)
    })
  }
  
  renderPublicEvent <- function() {
    print(paste("latest event: ", latestEventid))
    query <- paste0(
      "exec GETLASTEVENT"
    )
    sql <- sqlInterpolate(conn, query)
    result <- dbGetQuery(conn, sql)
    
    query <- paste0(
      "exec GETPLAYERSINEVENT 
      @EVENTID = ?eventid"
    )
    
    sql <- sqlInterpolate(conn, query,
                          eventid = result$eventid)
    result <- dbGetQuery(conn, sql)
    playersInEvent <<- as.numeric(result$accountid)
    output$public_event <- renderUI({
      publicEventUiLayout(playersInEvent)
    })
  }
  
  #RENDER THE HEATMAP PAGE
  renderHeatMap <- function() {
    output$heatmap_player <- renderUI({
      heatmapUiLayout(eventsofPlayer)
    })
  }
  
  # coach analysis
  renderAnalyses <- function(){
    # make plot
    
    teamData <- rsShotResult[as.Date(rsShotResult$TrainingDate) <= input$shotAnalyseDate[2]
                             &
                               as.Date(rsShotResult$TrainingDate) >= input$shotAnalyseDate[1]
                             &
                               rsShotResult$ShotType == input$typeselector1
                             , ]
    
    
    # output$heatMapJurIsLekker <- renderPlot({
    
    library(dplyr)
    resultPerPosition <- group_by(teamData, Position)
    resultPerPosition <- summarize(resultPerPosition, meanposition = round(mean(ShotAverage)))
    
    
    names(resultPerPosition)[1] <- "positions"# rename so it can be merged
    resultPerPosition <-
      merge(positionLocations, resultPerPosition, by = "positions") # merge with position locations
    

    
    # resultPerPosition <-
    #   resultPerPosition[rep(row.names(resultPerPosition),
    #                         resultPerPosition$percentage),] # repeat amount of percentage to create heat on that point
    
    image <- png::readPNG("www/field.png")
    shotpercentagePlot <- ggplot(resultPerPosition,
                      aes(x = locationX,
                          y = locationY,
                          fill = meanposition)) +
      guides(alpha = 0.4, size = FALSE) +
      annotation_custom(rasterGrob(
        image,
        width = unit(1, "npc"),
        height = unit(1, "npc")
      ),-Inf,
      Inf,
      -Inf,
      Inf) +
      scale_fill_gradientn(
        colors = c("steelblue", "blue", "hotpink"),
        labels = NULL,
        name = ""
      ) +
      theme(
        aspect.ratio = 0.673,
        axis.title.x = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()
      ) +
      coord_fixed(ylim = c(0, 100), xlim = c(0, 100)) +
      xlim(c(-10, 110)) +
      ylim(c(-10, 110)) +
      labs(x = "", y = "", fill = "") +
      geom_text(label=paste0(round(resultPerPosition$meanposition, digits = 0),'%'), size=25)
    
    # })

    ################################renderui test
    output$shotpercentage <- renderUI({
      img(renderImage({
        
        # Generate the PNG1703 x 1146
        png("www\\shotpercentage.png", width = 1703, height = 1146)
        plot(shotpercentagePlot)
        dev.off()
        
        list(src = "www\\shotpercentage.png",
             contentType = 'image/png',
             width = 309,
             height = 204,
             usemap = "#nameMap1")
        
      },deleteFile = TRUE), id = "fieldImage1")})
    
      output$shotAnalyse <- renderPlot({
      if(input$typeselector1 == "free_throw"){
        position <- 0
      } else{
        position <- input$sliderPosition1
        
      }
             if(input$staafOfLijnShotAnalyse1 == 1){
               # The next lines are to locally save a pdf. We have not found a better way that works yet
               # Now the actaul graph for output
               barplot <- ggplot(rsShotResult[rsShotResult$Fullname %in% input$shotAnalysePlayers
                                   &
                                     rsShotResult$TrainingDate <= input$shotAnalyseDate[2]
                                   &
                                     rsShotResult$TrainingDate >= input$shotAnalyseDate[1]
                                   &
                                     rsShotResult$Position == position
                                   & 
                                     rsShotResult$ShotType == input$typeselector1
                                   , ],
                      
                      aes(x = TrainingDateTime,
                 y = ShotAverage,
                 fill = Fullname)) +
        geom_bar(stat = "identity", position = "dodge") +
                 theme(axis.text.x = element_text(angle = 45, hjust = 1, size=12)) +
                 labs(fill = 'Names', x = "Date", y = "Shot Percentage")
               
               locallySavePdf(barplot)
               barplot
               
             }
   
    else {
      rsShotResult$TrainingDate <- as.Date(rsShotResult$TrainingDate)
     # rsShotResult$TrainingDateTime <- as.Date(rsShotResult$TrainingDateTime, format = "%Y-%m-%d %H:%M:%S")
      # The next lines are to locally save a pdf. We have not found a better way that works yet
      teamData <- rsShotResult[as.Date(rsShotResult$TrainingDate) <= input$shotAnalyseDate[2]
                               &
                                 as.Date(rsShotResult$TrainingDate) >= input$shotAnalyseDate[1]
                               &
                                 rsShotResult$Position == position
                               &
                                 rsShotResult$ShotType == input$typeselector1
                               , ]
      
      
      # Now the actaul graph for output
      lineplot <- ggplot(rsShotResult[rsShotResult$Fullname %in% input$shotAnalysePlayers
                                     &
                                       rsShotResult$TrainingDate <= input$shotAnalyseDate[2]
                                     &
                                       rsShotResult$TrainingDate >= input$shotAnalyseDate[1]
                                     &
                                       rsShotResult$Position == position
                                     & 
                                       rsShotResult$ShotType == input$typeselector1
                                     , ],
              aes(TrainingDateTime, ShotAverage, col = as.factor(Player_skey))) +
        geom_point() +
        geom_line(aes(group = Player_skey)) +
        # scale_x_datetime(date_labels = "%Y-%m-%d %H:%M:%OS") +
        stat_summary(data=teamData, fun.y="mean", geom="line", size=1, color='red') +
        
        xlab("TrainingDateTime") +
        scale_colour_manual(
          values = palette("default"),
          name = "Players",
          breaks = rsShotResult$Player_skey,
          labels =rsShotResult$Fullname
        ) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, size=12)) +
        labs(x = "Date", y = "Shot Percentage")
      
      locallySavePdf(lineplot)
      lineplot
    }
    })
      
  }
  
  # render the heatmap
  renderHeatmap <- function(){
    output$heatMapPlayer <- renderPlot({
      #make dataset
      resultPerPosition <-
        with(eventsOfPlayer, aggregate(
          list(
            totalTaken = as.integer(value2),
            totalMade  = as.integer(value)
          ),
          list(
            value3    = value3,
            yearMonth = substr(starttime,1,7)
          ),
          sum
        ))
            resultPerPosition$percentage <-
        ((
          as.integer(resultPerPosition$totalMade) / as.integer(resultPerPosition$totalTaken)
        ) * 100) # calculate percentage
      names(resultPerPosition)[1] <- "positions"# rename so it can be merged
      resultPerPosition <-
        merge(positionLocations, resultPerPosition, by = "positions") # merge with position locations
      
      resultPerPosition <- resultPerPosition[resultPerPosition$yearMonth == input$heatMapSlider,]
      
      resultPerPosition <-
        resultPerPosition[rep(row.names(resultPerPosition),
                              resultPerPosition$percentage),] # repeat amount of percentage to create heat on that point
      
      image <- png::readPNG("www/field.png")
      ggplot(resultPerPosition,
             aes(x = locationX,
                 y = locationY,
                 fill = percentage)) +
        guides(alpha = 0.4, size = FALSE) +
        annotation_custom(rasterGrob(
          image,
          width = unit(1, "npc"),
          height = unit(1, "npc")
        ),-Inf,
        Inf,
        -Inf,
        Inf) +
        scale_fill_gradientn(
          colors = c("steelblue", "blue", "hotpink"),
          labels = NULL,
          name = ""
        ) +
        stat_density_2d(
          geom = "raster",
          aes(fill = ..density..),
          alpha = 0.8,
          contour = FALSE,
          ylim = c(0, 100)
        ) +
        theme(
          aspect.ratio = 0.673,
          axis.title.x = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank()
        ) +
        coord_fixed(ylim = c(0, 100), xlim = c(0, 100)) +
        xlim(c(-10, 110)) +
        ylim(c(-10, 110)) +
        labs(x = "", y = "", fill = "")
    })
  }
  

  
 
  
  # get all results
  getShotResults <- function(x){

  }
  
  
  # Make pdf file
  makePdf <- function(){
   
    pdf("pdfdata.pdf",width=7,height=5, title = "Graph", onefile= T)
    print(savedPdf)
    dev.off()
  }
  
  # Download file in browser  
  output$pdfButton <- downloadHandler(
    filename = "grafiek.pdf",
    content = function(file) {
      makePdf()
      file.copy("pdfdata.pdf", file)
    }
  )
  
  locallySavePdf <- function(pdfSave) {
    savedPdf <<- pdfSave
  }
  
  #displays just the trainings the player attended
  observeEvent(c(input$player, input$month), {
    
    
    detailedList6 <- dplyr::filter(shots, grepl(input$player, Fullname) & grepl(input$month, Month))
    detailedList7 <- data.frame()
    detailedList7 <- rbind(detailedList7, detailedList6)
    
    
    positionlist <- c()
    positionlist <- detailedList7$Position
    
    positionlist <- sort(positionlist, decreasing = FALSE)
    
    
    #selects latest available training month 
    
    updateSelectInput(session, "position",
                      choices = positionlist,
                      selected = positionlist[1])
    
  })
  
  
  observeEvent(c(input$player, input$position), {
    
    
    player3 <- input$player
    position3 <- input$position
    detailedList4 <- dplyr::filter(shots, grepl(player3, Fullname) & grepl(position3, Position))
    detailedList5 <- data.frame()
    detailedList5 <- rbind(detailedList5, detailedList4)
    
    
    monthlist <- detailedList5$Month
    
    #selects latest available training month 
    monthlist <- sort(monthlist, decreasing = TRUE)
    
    print(monthlist)
    
    updateSelectInput(session, "month",
                      choices = monthlist,
                      selected = monthlist[1])
    
  })
  
  
  #for the graph
  
  observeEvent(c(input$month, input$player, input$position), {
    
    
    month <- input$month
    player <- input$player
    position <- input$position
    
    
    #for the position selection in the UI
    print(month)
    print(player)
    print(position)
    
    
    
    # #for the  graphhhhhhhhhh
    detailedList <- data.frame()
    detailedList1 <- dplyr::filter(shots, grepl(month, TrainingDateTime) & grepl(player, Fullname) & grepl(position, Position))
    detailedList <- rbind(detailedList, detailedList1)
    
    output$startmsg<- renderValueBox({ valueBox( value=tags$p(paste0("Overview"), style = "font-size: 150%;"), subtitle = tags$p(paste0("Statistics ", player, detailedList$LastName, " of ", month, " at ", position),style = "font-size: 120%;"), width = 2, color="blue", icon("user", class = NULL, lib = "font-awesome"))})
    
    
    #total position stats
    
    totalshots <- sum(detailedList$ShotsNumber)
    shotsmade <- sum(detailedList$ShotsMade)
    shotsmissed <- totalshots - shotsmade
    
    output$totalshots<- renderValueBox({ valueBox( value=totalshots, subtitle = paste0("Total number of shots taken by ",player ), width = 2, color="blue", icon("dribbble", class = NULL, lib = "font-awesome"))})
    output$shotsmade<- renderValueBox({ valueBox( value=shotsmade, subtitle = paste0("Total number of scored shots by ",player ), width = 2, color="green", icon("dribbble", class = NULL, lib = "font-awesome"))})
    output$shotsmissed<- renderValueBox({ valueBox( value=shotsmissed, subtitle = paste0("Total number of shots missed by ",player ), width = 2, color="red", icon("dribbble", class = NULL, lib = "font-awesome"))})
    
    
    #cleaning the training date
    detailedList$TrainingDateTime <- gsub("T", "\n", detailedList$TrainingDateTime)
    detailedList$TrainingDateTime <- gsub("Z", "", detailedList$TrainingDateTime)
    
    
    #monthly average percentage
    monthlyavg <- sum(detailedList$ShotsMade) / sum(detailedList$ShotsNumber) * 100
    monthlyavg <- round(monthlyavg, digits = 1)
    
    output$playeraverage<- renderValueBox({ valueBox( value=tags$p(paste0(monthlyavg, "%"), style = "font-size: 150%;"), subtitle = tags$p(paste0("Monthly Percentage ",player ),style = "font-size: 120%;"), width = 2, color="blue", icon("user", class = NULL, lib = "font-awesome"))})
    
    #monthly team average
    groupList2 <- data.frame()
    groupList3 <- dplyr::filter(shots, grepl(month, TrainingDateTime) & grepl(position, Position))
    groupList2 <- rbind(groupList2, groupList3)
    
    #bar
    groupbar <- aggregate(groupList2[, 17:18], list(groupList2$Fullname), sum)
    groupbar$Percentage <- with(groupbar, groupbar$ShotsMade / groupbar$ShotsNumber * 100)
    groupbar$Percentage <- round(groupbar$Percentage, digits = 1)
    
    colnames(groupbar) <- c("Name", "ShotsMade", "ShotsNumber", "Percentage")
    
    groupbardf <- data.frame()
    groupbardf <- rbind(groupbardf, groupbar[,c(1,4)])
    
    
    barr <- gather(groupbardf, Percentage, value, -Name)
    barr$Legend <- with(barr, ifelse(barr$Name == player, "Player","Teamplayers"))
    
    
    
    
    
    #barplotttt 
    output$bar <- renderPlot({
      output$playervsplayers= downloadHandler(
        filename = function() {paste0(player,"_vs_Players_", month, "_", position,".pdf")},
        content = function(file) {
          ggsave(file, plot=p, device = "pdf", width=14, height=8.5)
        }
      )
      
      
      p <- ggplot(barr, aes(Name, value, fill=Legend)) +   
        geom_bar( position = "dodge", stat = "identity") +
        scale_fill_manual(values = c("Player" = "#F07D00", "Teamplayers" = "#00bfff")) +
        
        geom_text(aes(label=value, group=Name), position=position_dodge(width=0.9), vjust=-0.25) +
        xlab("Player") +
        ylab("Percentage") +
        coord_cartesian(ylim = c(0, 100)) +
        
        
        
        theme(
          panel.border = element_blank(),
          # legend.key = element_blank(),
          axis.ticks = element_blank(),
          legend.position="none",
          
          panel.grid = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_blank(),
          plot.background = element_rect(fill = "transparent",colour = NA))
      
      p
      
      
      
      
      
      
    })
    
    
    
    
    
    
    
    #total information of team
    totalshotsgroup <- sum(groupList2$ShotsNumber)
    shotsmadegroup <- sum(groupList2$ShotsMade)
    shotsmissedgroup <- totalshotsgroup - shotsmadegroup
    
    #valueboxes of the team
    output$totalshotsgroup<- renderValueBox({ valueBox( value=totalshotsgroup, subtitle = paste0("Total number of shots taken by Team"), width = 2, color="blue", icon("users", class = NULL, lib = "font-awesome"))})
    output$shotsmadegroup<- renderValueBox({ valueBox( value=shotsmadegroup, subtitle = paste0("Total number of scored shots by Team "), width = 2, color="green", icon("users", class = NULL, lib = "font-awesome"))})
    output$shotsmissedgroup<- renderValueBox({ valueBox( value=shotsmissedgroup, subtitle = paste0("Total number of shots missed by Team"), width = 2, color="red", icon("users", class = NULL, lib = "font-awesome"))})
    
    #difference of player compared to team
    playertakendif <- totalshots / totalshotsgroup * 100
    playermadedif <- shotsmade / shotsmadegroup * 100
    playermisseddif <- shotsmissed / shotsmissedgroup * 100
    
    #some rounding
    playertakendif <- round(playertakendif, digits = 1)
    playermadedif <- round(playermadedif, digits = 1)
    playermisseddif <- round(playermisseddif, digits = 1)
    
    #valueboxes that display the differences
    output$playertakendif<- renderValueBox({ valueBox( value=paste0(playertakendif, "%"), subtitle = paste0("Of the total shots are taken by ", player), width = 2, color="blue", icon("dribbble", class = NULL, lib = "font-awesome"))})
    output$playermadedif<- renderValueBox({ valueBox( value=paste0(playermadedif, "%"), subtitle = paste0("Of the total shots are scored by ", player), width = 2, color="green", icon("dribbble", class = NULL, lib = "font-awesome"))})
    output$playermisseddif<- renderValueBox({ valueBox( value=paste0(playermisseddif, "%"), subtitle = paste0("Of the total shots are missed by ", player), width = 2, color="red", icon("dribbble", class = NULL, lib = "font-awesome"))})
    
    #tean average of the month
    monthlyteamavg <- sum(groupList2$ShotsMade) / sum(groupList2$ShotsNumber) * 100
    monthlyteamavg <- round(monthlyteamavg, digits = 1)
    
    #orderign
    order(groupbardf$Percentage)
    order(groupbardf$Percentage, decreasing = TRUE)   
    groupbardf <-  groupbardf[order(groupbardf$Percentage, decreasing = TRUE),]
    groupbardf$Rank <- 1:nrow(groupbardf) 
    
    
    #topplayers of the month per position
    topplayers <- sqldf(sprintf("select * from groupbardf where Percentage >= '%s'", monthlyteamavg))
    
    #bottom players of the month on specific position
    bottomplayers <- sqldf(sprintf("select * from groupbardf where Percentage < '%s'", monthlyteamavg))
    bottomplayers <-  bottomplayers[order(bottomplayers$Rank, decreasing = TRUE),]
    
    #leaderboards of the month table
    output$topplayertable<- renderTable( spacing = "m",{topplayers[,c(3,1,2)]}, width = "50%")
    output$bottomplayertable<- renderTable( spacing = "m",{bottomplayers[,c(3,1,2)]}, width = "50%")
    
    
    #csv
    output$downloadCsv2 <- downloadHandler(
      filename = function() {
        paste0("Top_players_", position, "_", month,".csv")
      },
      content = function(file) {
        write.csv2(topplayers[,c(1,2)], file, row.names=FALSE, sep="\t")
      }
    )
    
    
    output$downloadCsv3 <- downloadHandler(
      filename = function() {
        paste0("Bottom_players_", position, "_", month,".csv")
      },
      content = function(file) {
        write.csv2(bottomplayers[,c(1,2)], file, row.names=FALSE, sep="\t")
      }
    )
    
    #monthly team percentage
    output$teamaverage<- renderValueBox({ valueBox( value=tags$p(paste0(monthlyteamavg, "%"), style = "font-size: 150%;"), subtitle = tags$p("Monthly team percentage",style = "font-size: 120%;"), width = 2, color="blue", icon("users", class = NULL, lib = "font-awesome"))})
    
    #difference average percentage
    differenceavg <- monthlyavg - monthlyteamavg
    differenceavg <- round(differenceavg, digits =1)
    differenceavg <- paste0(differenceavg, "%")
    
    
    if(differenceavg < 0) {
      output$differenceavg<- renderValueBox({ valueBox( value=tags$p(paste0(differenceavg), style = "font-size: 150%;"), subtitle = tags$p("Difference compared to team",style = "font-size: 120%;"), width = 2, color="red",icon("arrow-down", class = NULL, lib = "font-awesome"))})
    } else if(differenceavg == 0 | differenceavg < 0.1) {
      
      output$differenceavg<- renderValueBox({ valueBox( value=tags$p(paste0(differenceavg),style = "font-size: 150%;"), subtitle = tags$p("Difference compared to team",style = "font-size: 120%;"), width = 2, color="blue")})
      
    } else {
      output$differenceavg<- renderValueBox({ valueBox( value=tags$p(paste0("+",differenceavg),style = "font-size: 150%;"), subtitle = tags$p("Difference compared to team",style = "font-size: 120%;"), width = 2, color="green",icon("arrow-up", class = NULL, lib = "font-awesome"))})
      
    }
    
    
    
    
    #plotting the actual graph
    output$performance <- renderPlotly({
      
      output$playerreport= downloadHandler(
        filename = function() {paste0("player_report_", month, "_", position, ".pdf")},
        content = function(file) {
          ggsave(file, device = "pdf", width=12, height=8.5)
        }
      )
      
      withProgress(message = 'Making plot for CTO Basketball', value = 0, {
        # Number of times we'll go through the loop
        n <- 10
        
        for (i in 1:n) {
          # Each time through the loop, add another row of data. This is
          # a stand-in for a long-running computation.
          
          # Increment the progress bar, and update the detail text.
          incProgress(1/n, detail = paste("Preparing data", i))
          # Pause for 0.05 seconds to simulate a long computation.
          Sys.sleep(0.05) 
          
        }
        
      })
      
      #color for the playeraverage in graph
      
      if (monthlyavg < monthlyteamavg) {
        
        colorplayer <- paste0("red")
      } 
      
      
      else {
        
        colorplayer <- paste0("#32cd32")
      }
      
      
      
      p1 <-     ggplotly(
        
        
        ggplot() +
          xlab("Training") +
          ylab("Percentage") +
          geom_point(data=detailedList, aes(x=detailedList$TrainingDateTime, y=detailedList$Percentage, color="Player shot percentage"))  +
          geom_hline(aes(yintercept = monthlyavg, color="Monthly player average"),linetype="dotted", show_guide=TRUE) +
          geom_hline(aes(yintercept = monthlyteamavg, color="Monthly team average"), show_guide=TRUE) +
          coord_cartesian(ylim = c(0, 100)) +
          scale_color_manual(values = c("Player shot percentage" = "black", "Monthly player average" = paste0(colorplayer),"Monthly team average" = "blue")) +
          
          
          
          
          theme(
            legend.justification = c(1, 0), legend.position = c(1, 0),
            
            panel.border = element_blank(),
            # legend.key = element_blank(),
            #axis.ticks = element_blank(),
            axis.text.x=element_text(angle=40,vjust=0.6),
            
            panel.grid = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank(),
            panel.background = element_blank(),
            plot.background = element_rect(fill = "transparent",colour = NA))
        
      )
      
      p1
      
      
      
      
    })
    
    
    
    
    
    
    #Leaderboarddddd graph
    
    output$leaderboard <- renderPlotly({
      
      
      
      t <- list(
        family = "sans serif",
        size = 10,
        color = toRGB("grey50"))
      
      p3 <- plot_ly(groupbardf, x = groupbardf$Rank, y = groupbardf$Percentage, type="scatter", mode = "markers", marker=list(color=ifelse(groupbardf$Percentage<monthlyteamavg,"red","green") , opacity=0.5 , size=30) ) %>%
        add_text(textfont = t, text=groupbardf$Name,textposition = "top") %>%
        add_trace(x =c(min(groupbardf$Rank), max(groupbardf$Rank)), y= monthlyteamavg, mode = "lines") %>%
        layout(showlegend=F)
      
      
      
      p3
      
    }
    
    )
    
    
  }) # END observe evwnt
  
  
  
  
  
  
  
  
  #tableeeeeee position specific
  observeEvent(c(input$month, input$player, input$position), {
    
    
    month1 <- input$month
    player1 <- input$player
    position1 <- input$position
    
    fortable1 <- sqldf(sprintf("select Position, Fullname, LastName, TrainingDateTime, ShotsMade, ShotsNumber, Percentage, Month from shots where Month is '%s'", month1, "AND Fullname is '$s'", player1))
    
    tableplayerdf1 <- data.frame()
    tableplayerdf1 <- rbind(tableplayerdf1, fortable1)
    # hi <- sqldf("SELECT * FROM lol WHERE Training = 'month'")
    
    
    
    
    #for the individual tablw
    detailedList2 <- data.frame()
    detailedList3 <- filter(shots, grepl(month1, TrainingDateTime) & grepl(player1, Fullname) & grepl(position1, Position))
    
    
    
    
    detailedList2 <- rbind(detailedList2, detailedList3)
    
    #colnames(detailedList2) <- c("Position", "Name", "Surname", "Test", "Training", "Shots Made", "Total Shots", "Percentage (%)", "Month")
    
    # detailedList2$Training <- gsub("T", " | ", detailedList2$Training)
    # detailedList2$Training <- gsub("Z", "", detailedList2$Training)
    
    
    print(detailedList2)
    
    
    output$playertable <- renderTable( spacing = "m",{detailedList2[,c(2,3,11,15,17,18,19)]}, width = "100%")
    
    
    output$downloadCsv <- downloadHandler(
      filename = function() {
        paste0(player1,"_results_", position1, "_", month1,".csv")
      },
      content = function(file) {
        write.csv2(detailedList2[,c(2,3,11,15,17,18,19)], file, row.names=FALSE, sep="\t")
      }
    )
    
    
    
    
    
    
    
    
    #Position information
    
  })
  
  observeEvent(input$positionInfo, {
    showModal(modalDialog(
      align="center",
      title = "Field positions",
      
      img(
        id = "fieldImage",
        src = "field.png",
        align = "center",
        usemap = "#nameMap",
        height = "300px",
        width = "500px"
        
      ),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  
  
  

})

