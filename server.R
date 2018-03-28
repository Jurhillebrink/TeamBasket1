
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
    renderAnalyses()
  }
  
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
      footer = tagList(actionButton("forgotPass", "Forgot your password?", style = "float:left"), actionButton("ok", "Login"))
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
      showModal(
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
    
    showModal(
      modalDialog(
        title = "Values added",
        "The values were successfully added to the system.",
        easyClose = TRUE,
        footer = NULL
      )
    )
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
    print(paste("latest event: ", latestEventid))
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
          menuSubItem(
            "Last event",
            tabName = "lastEventCoach",
            icon = icon("dribbble"),
            selected = TRUE # direct to last event page
          ),
          menuSubItem(
            "Shot results",
            tabName = "shotAnalyse",
            icon = icon("bar-chart")
          ),
          menuSubItem(
            "Shot results 2",
            tabName = "shotAnalyse2",
            icon = icon("line-chart")
          )
        )
      )
    })
    # show menu
    shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
    removeModal()
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
              "Last event",
              tabName = "lastEventCoach",
              icon = icon("dribbble")
            ),
            menuSubItem(
              "Shot results",
              tabName = "shotAnalyse",
              icon = icon("bar-chart")
            ),
            menuSubItem(
              "results 2",
              tabName = "shotAnalyse2",
              icon = icon("line-chart")
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
      "exec GETLASTEVENT"
    )
    sql <- sqlInterpolate(conn, query)
    latestEvent <- dbGetQuery(conn, sql)$eventid
    
    #latestEvent <- 401 #### REMOVE THIS LATER!!!!!!!! # this is done because there only is 1 legit event right now
    
    eventData <- rsShotResult[rsShotResult$eventid == latestEvent,] # select dtata of last event
    
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
      print(paste("selected position", selectedPosition))
      
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
    
    print(result)
    playersInEvent <<- result$accountid
    x <- result$accountid
    print(paste("playersInEvent", playersInEvent))
    print(paste("allplayers filtered met x: ", allPlayers[allPlayers$accountid %in% x, ]$firstname))
    print("---")
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
    output$shotAnalyse <- renderPlot({
      print(input$shotAnalyseDate)
      print(input$shotAnalyseShotType)
      if(input$shotAnalyseShotType == "free_throw"){
        position <- 0
        updateSelectInput(session, "shotAnalysePosition", selected = 0)
      } else{
        position <- input$shotAnalysePosition
      }
      ggplot(rsShotResult[rsShotResult$fullname %in% input$shotAnalysePlayers
                          &
                            as.Date(rsShotResult$startdate) <= input$shotAnalyseDate[2]
                          &
                            as.Date(rsShotResult$startdate) >= input$shotAnalyseDate[1]
                          &
                            rsShotResult$value3 == position
                          & 
                            rsShotResult$value4 == input$shotAnalyseShotType
                          , ],
             aes(x = starttime,
                 y = percentage,
                 fill = fullname)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(fill = 'Names')
    })
    
    output$shotAnalyse2 <- renderPlot({
      
      
      head(rsShotResult)
      
      
      if(input$shotAnalyse2ShotType == "free_throw"){
        position <- 0
        updateSelectInput(session, "shotAnalyse2Position", selected = 0)
      } else{
        position <- input$shotAnalyse2Position
      }
      ggplot(rsShotResult[rsShotResult$fullname %in% input$shotAnalyse2Players
                          &
                            as.Date(rsShotResult$startdate) <= input$shotAnalyse2Date[2]
                          &
                            as.Date(rsShotResult$startdate) >= input$shotAnalyse2Date[1]
                          &
                            rsShotResult$value3 == position
                          & 
                            rsShotResult$value4 == input$shotAnalyse2ShotType
                          , ],
             aes(x = strptime(starttime, format="%Y-%m-%d"),
                 y = percentage)) +
        geom_line(aes(colour = as.character(accountid))) +
        geom_point(aes(colour = as.character(accountid))) +
        xlab("starttime") +
        scale_colour_manual(
          values = palette("default"),
          name = "Players",
          breaks = rsShotResult$accountid
        )
      
      
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
      print(resultPerPosition)
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
      print(resultPerPosition)
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
    query <- paste0(
      "exec GETSHOTRESULTS"
    )
    sql <- sqlInterpolate(conn, query)
    rsShotResult <- dbGetQuery(conn, sql)
  
    # transform data
    rsShotResult$startdate <- substring(rsShotResult$starttime, 1, 11)
    rsShotResult$fullname <-
      paste(rsShotResult$firstname, rsShotResult$lastname, sep = " ")
    rsShotResult$percentage <-
      ((
        as.integer(rsShotResult$value) / as.integer(rsShotResult$value2)
      ) * 100)# calculate percentage
    
    #calculate made points
    rsShotResult$points <-
      ifelse(
        rsShotResult$value3 %in% positionsInCircle,
        (2 * as.integer(rsShotResult$value)),
        ifelse(
          rsShotResult$value3 %in% positionsOutCircle,
          (3 * as.integer(rsShotResult$value)),
          NA
        )
      )
    
    rsShotResult <<-
      rsShotResult[rsShotResult$percentage <= 100,]#filter only viable percentage
  }

})

