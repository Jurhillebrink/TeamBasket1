
#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinyWidgets) 
library(shinyjs) # to hide side bar
require("DT")

source("./global.R")

#query = sqlInterpolate(
 # conn,
  #"SELECT account.accountid, user.firstname, user.lastname, user.teamid FROM account JOIN user ON user.accountid = account.accountid WHERE role = 'p'"
#)
#result = dbSendQuery(conn, query)
#rs <- dbFetch(result, n = -1)

#lastnames <- paste(rs$firstname, ' ' , rs$lastname)
#accountids <- rs$accountid
#species <- data.frame(lastnames, accountids)
#choicesSpecies <-
 # setNames(as.numeric(species$accountids), species$lastnames)


dashboardUI <<- fluidPage(
  style="overflow: auto; margin: 0; padding:0; width:100%;",
  align="center",
  #the js scripts
  tags$head(tags$script(src = "mapster.js")),
  tags$head(tags$script(src = "script.js")),
    dashboardPage(
      skin = "red",
      dashboardHeader(
        title = "CTO",
        tags$li(h5(""),
                class = "dropdown"),
        # the logout button
        tags$li(
          actionButton("logoutBtn", icon("log-out", lib = "glyphicon"), 
          style = "background-color:rgba(0, 0, 0, 0); outline:0; padding: 14px; border: none; color: white;"),
          class = "dropdown"
        )
      ),
      dashboardSidebar(sidebarMenu(id = "tabs", sidebarMenuOutput("menu"))),
      dashboardBody(
        useShinyjs(),# to (dis)able side bar
        tabItems(
          # select players for a session
          tabItem(tabName = "InvoerSchoten1",
                  fluidPage(
                  box(
                    width = 6,
                    selectizeInput(
                      "playersInEvent",
                      "Players",
                      choices = allPlayerChoices,
                      selected = allPlayers[1, "Playernames"],
                      multiple = TRUE
                    ),
                    actionButton('switchtab', 'Start event')
                  ))),
          # the actual shot event page
          tabItem(tabName = "InvoerSchoten2",
                  fluidRow(
                    uiOutput('public_event'), #select player name
                    box(
                      title = "Select Position",
                      id = "content-body",
                      width = 8,
                      solidHeader = TRUE,
                      #type of shot
                      radioGroupButtons(inputId = "typeselector", 
                                        label = "Type", 
                                        status = "danger",
                                        choices = setNames(c("free_throw","catch_shoot","dribble"),c("Free throw","Catch & Shoot", "From dribble")),
                                        selected = "catch_shoot"),
                      #map selector
                      img(
                        id = "fieldImage",
                        src = "field.png",
                        align = "center",
                        usemap = "#nameMap",
                        height = "300px",
                        width = "300px"
                        
                      ),
                      tags$map( id = "imageMaps", name= "nameMap",
                       tags$area( name="location1", shape="rect", coords="7,6,255,593", href="http://www.image-maps.com/1"),
                       tags$area( name="location6", shape="rect", coords="1448,7,1696,594", href="http://www.image-maps.com/6"),
                       tags$area( name="location2", shape="rect", coords="258,6,577,353", href="http://www.image-maps.com/2"),
                       tags$area( name="location5", shape="rect", coords="1126,6,1445,353", href="http://www.image-maps.com/5"),
                       tags$area( name="location3", shape="rect", coords="579,7,849,354", href="http://www.image-maps.com/3"),
                       tags$area( name="location4", shape="rect", coords="854,6,1124,353", href="http://www.image-maps.com/4"),
                       tags$area( name="location8", shape="rect", coords="579,354,849,701", href="http://www.image-maps.com/8"),
                       tags$area( name="location9", shape="rect", coords="853,353,1123,700", href="http://www.image-maps.com/9"),
                       tags$area( name="location7", shape="poly", coords="259,357,259,596,288,652,323,703,361,752,388,778,426,815,478,852,532,886,576,905,576,356,577,355,261,355", href="http://www.image-maps.com/7"),
                       tags$area( name="location10", shape="poly", coords="1126,352,1447,353,1446,593,1423,638,1397,682,1376,708,1349,742,1308,784,1267,820,1212,858,1163,887,1127,904", href="http://www.image-maps.com/10"),
                       tags$area( name="location11", shape="poly", coords="578,701,579,907,628,927,687,944,744,955,808,961,856,964,924,957,990,948,1064,929,1110,913,1125,908,1124,699", href="http://www.image-maps.com/11"),
                       tags$area( name="location12", shape="poly", coords="6,597,256,597,286,652,312,692,331,719,363,756,393,786,428,819,469,849,510,873,554,897,578,908,578,1142,4,1140", href="http://www.image-maps.com/12"),
                       tags$area( name="location13", shape="poly", coords="579,909,577,1141,1124,1141,1122,907,1083,923,1041,937,994,948,951,956,912,961,867,962,818,963,765,957,712,948,655,935,610,923", href="http://www.image-maps.com/13"),
                       tags$area( name="location14", shape="poly", coords="1124,906,1124,1140,1696,1140,1697,596,1446,595,1432,621,1411,660,1383,700,1350,740,1318,775,1280,811,1238,844,1183,879", href="http://www.image-maps.com/14")
                      ),
                      hidden(
                        textInput("sliderPosition", '', value= 1, width = NULL, placeholder = NULL)
                      ),
                      tags$style(
                        HTML(
                          ".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #485563}"
                        )
                      ),
                      # amount taken shots input form, with buttons
                      h4("Total"),
                      fluidRow(
                        align="center",
                        column(1, offset = 4,
                               actionButton("total_minus","", icon=icon("minus"))
                        ),
                        column(2,
                               numericInput(
                                 "total",
                                 label = NULL,
                                 value = 10,
                                 min = 0,
                                 max = NA
                               )
                        ),
                        column(1,
                               actionButton("total_plus","", icon=icon("plus"))
                        )
                      ),
                      # amount successful shots input form, with buttons
                      h4("Succeed"),
                      fluidRow(
                        align="center",
                        column(1, offset = 4,
                               actionButton("succeed_minus","", icon=icon("minus"))
                        ),
                        column(2,
                               numericInput(
                                 "succeed",
                                 label = NULL,
                                 value = 0,
                                 min = 0,
                                 max = NA
                               )
                        ),
                        column(1,
                               actionButton("succeed_plus","", icon=icon("plus"))
                        )
                      ),
                      actionButton("insertShotbtn", "Save")
                    )
                  )),
          # admin input page
          tabItem(tabName = "admin",
                  uiOutput('admin_input')),
          #in development
          tabItem(tabName = "playerEvent",
                  uiOutput('playerEvent')),
          #shot analysis page for coaches
          tabItem(tabName = "shotAnalyse",
                  fluidRow(
                    #input and filter options for the graph
                    box(
                      width = 12,
                      selectizeInput(
                        "shotAnalysePlayers",
                        "Players",
                        c(allPlayers$full),
                        selected = allPlayers[1, "Playername"],
                        multiple = TRUE
                      ),
                      dateRangeInput(
                        'shotAnalyseDate',
                        label = 'Date range input: yyyy-mm-dd',
                        start = "2017-03-12",
                        end = format(Sys.Date(), format = "%Y-%m-%d")
                      ),
                      
                      selectInput(
                        "shotAnalysePosition", "Shot position:",
                                  c(1:14)
                      ),
                      
                      selectInput("shotAnalyseShotType", "Type of shot",
                                  c("free_throw", "catch_shoot", "dribble"))
                    ), #end of box
                    #the bar chart
                    box(width = 12,
                        plotOutput("shotAnalyse"))
                  )),
          tabItem(tabName = "shotAnalyse2",
                  fluidRow(
                    #input and filter options for the graph
                    box(
                      width = 12,
                      selectizeInput(
                        "shotAnalyse2Players",
                        "Players",
                        c(allPlayers$full),
                        selected = allPlayers[1, "Playername"],
                        multiple = TRUE
                      ),
                      dateRangeInput(
                        'shotAnalyse2Date',
                        label = 'Date range input: yyyy-mm-dd',
                        start = "2017-03-12",
                        end = format(Sys.Date(), format = "%Y-%m-%d")
                      ),
                      selectInput(
                        "shotAnalyse2Position",
                        "Shot position:",
                        list(
                          `All` = c("All"),
                          `Angle` = c("Left", "Center", "Right"),
                          `Circle` = c("Inside circle", "Outside circle")
                        ),
                        multiple = TRUE,
                        selected = c("All")
                      ),
                    
                    
                    selectInput("shotAnalyse2ShotType", "Type of shot",
                                c("free_throw", "catch_shoot", "dribble"))
                  ), #end of box
                    #the bar chart
                    box(width = 12,
                        plotOutput("shotAnalyse2"))
                  )),
          #home page of a player
          tabItem(
            tabName = "homePlayer",
            fluidPage(
              uiOutput('player_home'))
          ),
          #monthly heatmap player page
          tabItem(tabName = "heatMapPlayer",
                  uiOutput('heatmap_player')),
          #teammates contact list is still in development
          tabItem(
            tabName = "teammates",
            box(width = 12,
                title = "Teammate contact info",
                tableOutput('teammates'))
          ), 
          tabItem(
            tabName = "lastEventCoach",
            fluidPage(
              uiOutput('last_event_coach'))
          )
        )
      )
  )
)

ui <<- shinyUI(
  dashboardUI
)

# returns a ui with a heatmap that plays over time
heatmapUiLayout <<- function(x){
  return(
    box(
      width = 12,
      title = "Heatmap",
      plotOutput("heatMapPlayer"),
      # makes the heat maps play over time
      sliderTextInput(
        inputId = "heatMapSlider", 
        label = "Select event:", 
        grid = TRUE, 
        force_edges = TRUE,
        choices = unique(substr(eventsOfPlayer$starttime,1,7)),
        animate=TRUE
      )
    )
  )
}

#Box to select a player in a public event
publicEventUiLayout <<- function(x){
  return(
    box(
      title = "Select Player",
      width = 4,
      solidHeader = TRUE,
      align="left",
      radioButtons(
        "radio",
        label = "",
        choices = 
          setNames(
            as.numeric(allPlayers[allPlayers$accountid %in% x, ]$accountid),
            paste(allPlayers[allPlayers$accountid %in% x, ]$firstname, allPlayers[allPlayers$accountid %in% x, ]$lastname, sep=" "))
        ,
        selected = as.numeric(x[1])
      ),
      actionLink("refreshPlayers", icon("refresh")),# refresh the player list
      hr(),
      # fluidRow(column(3, verbatimTextOutput("value")))
      actionButton("closeTestEvent", "End event")
    )
  )
}

# the admin insert page
adminUiLayout <<- function(x){
  return(fluidRow(
    #ADD USER
    box(
      title = "Add User",
      width = 6,
      solidHeader = TRUE,
      align = "left",
      textInput(
        "userfirstname",
        "Firstname",
        value = "",
        placeholder = ""
      ),
      textInput(
        "userlastname",
        "LastName",
        value = "",
        placeholder = ""
      ),
      dateInput(
        "userbirthday",
        "Birthday",
        value = NULL,
        min = NULL,
        max = NULL,
        format = "yyyy-mm-dd",
        startview = "month",
        weekstart = 0,
        language = "en",
        width = NULL
      ),
      textInput(
        "userphonenumber",
        "Phonenumber",
        value = "",
        placeholder = ""
      ),
      selectInput("userteam", "Team",
                  x),
      textInput(
        "useremail",
        "Email",
        value = "",
        placeholder = ""
      ),
      passwordInput(
        "userpassword",
        "Password",
        value = "",
        placeholder = ""
      ),
      selectInput(
        "userrole",
        "Role",
        c(
          "Admin" = "a",
          "Player" = "p",
          "Coach" = "c"
        )
      ),
      actionButton("insertUser", "Add User")
    ),
    #ADD TEAM
    box(
      title = "Add Team",
      width = 6,
      solidHeader = TRUE,
      align = "left",
      textInput(
        "teamcode",
        "Team Code",
        value = "",
        placeholder = "ALE01"
      ),
      actionButton("insertTeam", "Add Team")
    )
  ))
}

# the page of the data of the latest event
lastEventLayout <<- function(eventData, user){
  # convert data to numerics to calculate
  eventData$value  <- as.numeric(eventData$value)
  eventData$value2 <- as.numeric(eventData$value2)
  
  return(
    fluidPage(
      fluidRow(
        #list used positions
        box(
          title = paste("Training",eventData[1,'startdate']),width = 3,
          "Positions: ", 
          paste(sort(unique(as.numeric(eventData$value3)), decreasing=FALSE), collapse=" ")
        ),
        #total amount of shots
        box(
          title = "Total shots",width = 3, background = "red",
          h2(sum(as.numeric(eventData$value)))
        ), 
        #total amount scored
        box(
          title = "Shots made",width = 3, background = "red",
          h2(sum(as.numeric(eventData$value2)))
        ), 
        #total percentage of the whole team
        box(
          title = "Percentage",width = 3, background = "red",
          h2(
            paste(
              round((sum(as.numeric(eventData$value)) /sum(as.numeric(eventData$value2))*100),1)),"%") 
        )
      ),
      # player specific data
      fluidRow(
        # choose a player
        box(
          title = "Select player",width = 3, background = "red",
          radioButtons("select_player_last_event", "",
                       choices=
                           unique(
                             eventData$fullname),
                       selected = eventData[1,]$fullname)
        ), 
        # plot the data of that player on all positions and shhot types
        box(
          width = 9,
          plotOutput('last_event_per_player')
        )
      ),
      #position specific data
      fluidRow(
        # choose a position
        box(
          title = "Pick position",width = 3, background = "red",
          radioButtons("select_position_last_event", "",
                       choices=sort(
                         as.numeric(
                           unique(
                             eventData$value3)), 
                         decreasing = FALSE),
                       selected = min(
                         as.numeric(
                           unique(
                             eventData$value3))))
        ), 
        # plot graph with all results of that position
        box(
          width = 9,
          plotOutput('last_event_per_position')
        )
      )
    )
  )
}

# home and landing page of a player
playerHomeLayout <<- function(user){
  
  username <- h2(paste("Welcome", currentUser$firstname, currentUser$lastname, sep = " "))
  # form personal data
  str1 <- p(paste("Firstname", user$firstname, sep = " :  "))
  str2 <- p(paste("Lastname", user$lastname, sep = " :  "))
  str3 <- p(paste("Phone", user$phone, sep = " :  "))
  str4 <- p(paste("E-mail", user$email, sep = " :  "))
  str5 <- p(paste("Birthday", user$birthday, sep = " :  "))
  return(
    fluidPage(
      username,
      fluidRow(
        # personal data
        box(
          width = 6,
          title = "Personal info",
              HTML(paste(str1, str2, str3, str4, str5, sep = '')))),
      
      #quick data on events last time
      h4("Your results"),
      uiOutput("trainingSelectorOutput"), #event selector
      fluidRow(
        # output the quick data(percentages)
        box(width = 4,
            title = "Free Throw",
            h1(textOutput("freeThrowPercentage")),
            h3(textOutput("freeThrowCount"))),
        box(width = 4,
            title = "Dribble",
            h1(textOutput("dribblePercentage")),
            h3(textOutput("dribbleCount"))),
        box(width = 4,
            title = "Catch & Shoot",
            h1(textOutput("catchShootPercentage")),
            h3(textOutput("catchShootCount")))
      ),
      # compare to the team.
      h4("Compare to your team"),
      # selec type of shot
      radioGroupButtons(inputId = "typeselectorHomeGraph", 
                        label = "Type", 
                        status = "danger",
                        choices = setNames(c("free_throw","catch_shoot","dribble"),c("Free throw","Catch & Shoot", "From dribble")),
                        selected = "catch_shoot"),
      #plot graph
      fluidRow(
        box(
          width = 12,
          plotOutput('home_graph_player')
        )
      )
    )
  )
}


renderTrainingSelector <<- function(x){
  return(selectInput("trainingselector", "Training", x, selected = 1))
}
#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinyWidgets) 
library(shinyjs) # to hide side bar
require("DT")

source("./global.R")

#query = sqlInterpolate(
 # conn,
  #"SELECT account.accountid, user.firstname, user.lastname, user.teamid FROM account JOIN user ON user.accountid = account.accountid WHERE role = 'p'"
#)
#result = dbSendQuery(conn, query)
#rs <- dbFetch(result, n = -1)

#lastnames <- paste(rs$firstname, ' ' , rs$lastname)
#accountids <- rs$accountid
#species <- data.frame(lastnames, accountids)
#choicesSpecies <-
#setNames(as.numeric(species$accountids), species$lastnames)


dashboardUI <<- fluidPage(
  includeCSS("www/myStyle.css"),
  style="overflow: auto; margin: 0; padding:0; width:100%;",
  align="center",
  #the js scripts
  tags$head(tags$script(src = "mapster.js")),
  tags$head(tags$script(src = "script.js")),
  dashboardPage(
      dashboardHeader(
        title = "CTO",
        tags$li(h5(""),
                class = "dropdown"),
        # the logout button
        tags$li(
          actionButton("logoutBtn", icon("log-out", lib = "glyphicon"), 
          style = "background-color:rgba(0, 0, 0, 0); outline:0; padding: 14px; border: none; color: white;"),
          class = "dropdown"
        )
      ),
      dashboardSidebar(sidebarMenu(id = "tabs", sidebarMenuOutput("menu"))),
      dashboardBody(
        useShinyjs(),# to (dis)able side bar
        tabItems(
          # select players for a session
          tabItem(tabName = "InvoerSchoten1",
                  fluidRow(
                    #input and filter options for the graph
                    box(
                      width = 6,
                      selectizeInput(
                        "teamSelected",
                        "Team",
                        choices = allTeams$teamcode,
                        #choices = allPlayerChoices,
                        multiple = FALSE,
                        options = list(
                          placeholder = 'Click here if you want to select a whole team',
                          onInitialize = I('function() { this.setValue(""); }')
                        )
                      ),
                        selectizeInput(
                          "playersInEvent",
                          "Players",
                          choices = allPlayerChoices,
                          selected = allPlayers[1, "Playernames"],
                          multiple = TRUE,
                          options = list(
                            placeholder = 'Or select individual players',
                            onInitialize = I('function() { this.setValue(""); }')
                          )
                        ),
                  
                  
                  
                  
                  
                    actionButton('switchtab', 'Start event')
                  ))),
          # the actual shot event page
          tabItem(tabName = "InvoerSchoten2",
                  fluidRow(
                    uiOutput('public_event'), #select player name
                    box(
                      title = "Select Position",
                      id = "content-body",
                      width = 8,
                      solidHeader = TRUE,
                      #type of shot
                      radioGroupButtons(inputId = "typeselector", 
                                        label = "Type", 
                                        status = "danger",
                                        choices = setNames(c("free_throw","catch_shoot","dribble"),c("Free throw","Catch & Shoot", "From dribble")),
                                        selected = "catch_shoot"),
                      #map selector
                      img(
                        id = "fieldImage",
                        src = "field.png",
                        align = "center",
                        usemap = "#nameMap",
                        height = "300px",
                        width = "300px"
                        
                      ),
                      tags$map( id = "imageMaps", name= "nameMap",
                       tags$area( name="location1", shape="rect", coords="7,6,255,593", href="http://www.image-maps.com/1"),
                       tags$area( name="location6", shape="rect", coords="1448,7,1696,594", href="http://www.image-maps.com/6"),
                       tags$area( name="location2", shape="rect", coords="258,6,577,353", href="http://www.image-maps.com/2"),
                       tags$area( name="location5", shape="rect", coords="1126,6,1445,353", href="http://www.image-maps.com/5"),
                       tags$area( name="location3", shape="rect", coords="579,7,849,354", href="http://www.image-maps.com/3"),
                       tags$area( name="location4", shape="rect", coords="854,6,1124,353", href="http://www.image-maps.com/4"),
                       tags$area( name="location8", shape="rect", coords="579,354,849,701", href="http://www.image-maps.com/8"),
                       tags$area( name="location9", shape="rect", coords="853,353,1123,700", href="http://www.image-maps.com/9"),
                       tags$area( name="location7", shape="poly", coords="259,357,259,596,288,652,323,703,361,752,388,778,426,815,478,852,532,886,576,905,576,356,577,355,261,355", href="http://www.image-maps.com/7"),
                       tags$area( name="location10", shape="poly", coords="1126,352,1447,353,1446,593,1423,638,1397,682,1376,708,1349,742,1308,784,1267,820,1212,858,1163,887,1127,904", href="http://www.image-maps.com/10"),
                       tags$area( name="location11", shape="poly", coords="578,701,579,907,628,927,687,944,744,955,808,961,856,964,924,957,990,948,1064,929,1110,913,1125,908,1124,699", href="http://www.image-maps.com/11"),
                       tags$area( name="location12", shape="poly", coords="6,597,256,597,286,652,312,692,331,719,363,756,393,786,428,819,469,849,510,873,554,897,578,908,578,1142,4,1140", href="http://www.image-maps.com/12"),
                       tags$area( name="location13", shape="poly", coords="579,909,577,1141,1124,1141,1122,907,1083,923,1041,937,994,948,951,956,912,961,867,962,818,963,765,957,712,948,655,935,610,923", href="http://www.image-maps.com/13"),
                       tags$area( name="location14", shape="poly", coords="1124,906,1124,1140,1696,1140,1697,596,1446,595,1432,621,1411,660,1383,700,1350,740,1318,775,1280,811,1238,844,1183,879", href="http://www.image-maps.com/14")
                      ),
                      hidden(
                        textInput("sliderPosition", '', value= 1, width = NULL, placeholder = NULL)
                      ),
                      tags$style(
                        HTML(
                          ".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #485563}"
                        )
                      ),
                      # amount taken shots input form, with buttons
                      h4("Total"),
                      fluidRow(
                        align="center",
                        column(1, offset = 4,
                               actionButton("total_minus","", icon=icon("minus"))
                        ),
                        column(2,
                               numericInput(
                                 "total",
                                 label = NULL,
                                 value = 10,
                                 min = 0,
                                 max = NA
                               )
                        ),
                        column(1,
                               actionButton("total_plus","", icon=icon("plus"))
                        )
                      ),
                      # amount successful shots input form, with buttons
                      h4("Succeed"),
                      fluidRow(
                        align="center",
                        column(1, offset = 4,
                               actionButton("succeed_minus","", icon=icon("minus"))
                        ),
                        column(2,
                               numericInput(
                                 "succeed",
                                 label = NULL,
                                 value = 0,
                                 min = 0,
                                 max = NA
                               )
                        ),
                        column(1,
                               actionButton("succeed_plus","", icon=icon("plus"))
                        )
                      ),
                      actionButton("insertShotbtn", "Save")
                    )
                  )),
          # admin input page
          tabItem(tabName = "admin",
                  uiOutput('admin_input')),
          #in development
          tabItem(tabName = "playerEvent",
                  uiOutput('playerEvent')),
          #shot analysis page for coaches
          tabItem(tabName = "shotAnalyse",
                  fluidRow(
                    #input and filter options for the graph
                    box(
                      width = 12,
                      selectizeInput(
                        "shotAnalysePlayers",
                        "Players",
                        c(allPlayers$full),
                        selected = allPlayers[1, "Playername"],
                        multiple = TRUE
                      ),
                      dateRangeInput(
                        'shotAnalyseDate',
                        label = 'Date range input: yyyy-mm-dd',
                        start = "2017-03-12",
                        end = format(Sys.Date(), format = "%Y-%m-%d")
                      ),
                      
                      selectInput(
                        "shotAnalysePosition", "Shot position:",
                                  c(1:14)
                      ),
                      
                      selectInput("shotAnalyseShotType", "Type of shot",
                                  c("free_throw", "catch_shoot", "dribble"))
                    ), #end of box
                    #the bar chart
                    box(width = 12,
                        plotOutput("shotAnalyse"))
                  )),
          tabItem(tabName = "shotAnalyse2",
                  fluidRow(
                    #input and filter options for the graph
                    box(
                      width = 12,
                      selectizeInput(
                        "shotAnalyse2Players",
                        "Players",
                        c(allPlayers$full),
                        selected = allPlayers[1, "Playername"],
                        multiple = TRUE
                      ),
                      dateRangeInput(
                        'shotAnalyse2Date',
                        label = 'Date range input: yyyy-mm-dd',
                        start = "2017-03-12",
                        end = format(Sys.Date(), format = "%Y-%m-%d")
                      ),
                      selectInput(
                        "shotAnalyse2Position",
                        "Shot position:",
                        list(
                          `All` = c("All"),
                          `Angle` = c("Left", "Center", "Right"),
                          `Circle` = c("Inside circle", "Outside circle")
                        ),
                        multiple = TRUE,
                        selected = c("All")
                      ),
                    
                    
                    selectInput("shotAnalyse2ShotType", "Type of shot",
                                c("free_throw", "catch_shoot", "dribble"))
                  ), #end of box
                    #the bar chart
                    box(width = 12,
                        plotOutput("shotAnalyse2"))
                  )),
          #home page of a player
          tabItem(
            tabName = "homePlayer",
            fluidPage(
              uiOutput('player_home'))
          ),
          #monthly heatmap player page
          tabItem(tabName = "heatMapPlayer",
                  uiOutput('heatmap_player')),
          #teammates contact list is still in development
          tabItem(
            tabName = "teammates",
            box(width = 12,
                title = "Teammate contact info",
                tableOutput('teammates'))
          ), 
          tabItem(
            tabName = "lastEventCoach",
            fluidPage(
              uiOutput('last_event_coach'))
          )
        )
      )
  )
)

ui <<- shinyUI(
  dashboardUI
)

# returns a ui with a heatmap that plays over time
heatmapUiLayout <<- function(x){
  return(
    box(
      width = 12,
      title = "Heatmap",
      plotOutput("heatMapPlayer"),
      # makes the heat maps play over time
      sliderTextInput(
        inputId = "heatMapSlider", 
        label = "Select event:", 
        grid = TRUE, 
        force_edges = TRUE,
        choices = unique(substr(eventsOfPlayer$starttime,1,7)),
        animate=TRUE
      )
    )
  )
}

#Box to select a player in a public event
publicEventUiLayout <<- function(x){
  return(
    
    box(
      title = "Select Player",
      width = 4,
      solidHeader = TRUE,
      align="left",
      radioButtons(
        "radio",
        label = "",
        choices = 
          setNames(
            as.numeric(allPlayers[allPlayers$accountid %in% x, ]$accountid),
            paste(allPlayers[allPlayers$accountid %in% x, ]$firstname, allPlayers[allPlayers$accountid %in% x, ]$lastname, sep=" "))
        ,
        selected = as.numeric(x[1])
      ),
      selectizeInput(
        "addPlayers",
        "Select players to add",
        choices = allPlayerChoices [! allPlayerChoices %in% as.numeric(playersInEvent)],
        selected = allPlayers[1, "Playernames"],
        multiple = TRUE
      ),
      actionButton('addPlayerInEvent', 'Add selected players'),
      hr(),
      # fluidRow(column(3, verbatimTextOutput("value")))
      actionButton("closeTestEvent", "End event")
)
      
    
  )
}

# the admin insert page
adminUiLayout <<- function(x){
  return(fluidRow(
    #ADD USER
    box(
      title = "Add User",
      width = 6,
      solidHeader = TRUE,
      align = "left",
      textInput(
        "userfirstname",
        "Firstname",
        value = "",
        placeholder = ""
      ),
      textInput(
        "userlastname",
        "LastName",
        value = "",
        placeholder = ""
      ),
      dateInput(
        "userbirthday",
        "Birthday",
        value = NULL,
        min = NULL,
        max = NULL,
        format = "yyyy-mm-dd",
        startview = "month",
        weekstart = 0,
        language = "en",
        width = NULL
      ),
      textInput(
        "userphonenumber",
        "Phonenumber",
        value = "",
        placeholder = ""
      ),
      selectInput("userteam", "Team",
                  x),
      textInput(
        "useremail",
        "Email",
        value = "",
        placeholder = ""
      ),
      passwordInput(
        "userpassword",
        "Password",
        value = "",
        placeholder = ""
      ),
      selectInput(
        "userrole",
        "Role",
        c(
          "Admin" = "a",
          "Player" = "p",
          "Coach" = "c"
        )
      ),
      actionButton("insertUser", "Add User")
    ),
    #ADD TEAM
    box(
      title = "Add Team",
      width = 6,
      solidHeader = TRUE,
      align = "left",
      textInput(
        "teamcode",
        "Team Code",
        value = "",
        placeholder = "ALE01"
      ),
      actionButton("insertTeam", "Add Team")
    )
  ))
}

# the page of the data of the latest event
lastEventLayout <<- function(eventData, user){
  # convert data to numerics to calculate
  eventData$value  <- as.numeric(eventData$value)
  eventData$value2 <- as.numeric(eventData$value2)
  
  return(
    fluidPage(
      fluidRow(
        #list used positions
        box(
          title = paste("Training",eventData[1,'startdate']),width = 3, background = "orange",
          "Positions: ", 
          paste(sort(unique(as.numeric(eventData$value3)), decreasing=FALSE), collapse=" ")
        ),
        #total amount of shots
        box(
          title = "Total made",width = 3, background = "orange",
          h2(sum(as.numeric(eventData$value)))
        ), 
        #total amount scored
        box(
          title = "Total Taken",width = 3, background = "orange",
          h2(sum(as.numeric(eventData$value2)))
        ), 
        #total percentage of the whole team
        box(
          title = "Percentage",width = 3, background = "orange",
          h2(
            paste(
              round((sum(as.numeric(eventData$value)) /sum(as.numeric(eventData$value2))*100),1)),"%") 
        )
      ),
      # player specific data
      fluidRow(
        # choose a player
        box(
          title = "Select player",width = 3, background = "orange",
          radioButtons("select_player_last_event", "",
                       choices=
                           unique(
                             eventData$fullname),
                       selected = eventData[1,]$fullname)
        ), 
        # plot the data of that player on all positions and shhot types
        box(
          width = 9,
          plotOutput('last_event_per_player')
        )
      ),
      #position specific data
      fluidRow(
        # choose a position
        box(
          title = "Pick position",width = 3, background = "orange",
          radioButtons("select_position_last_event", "", 
                       choices=sort(
                         as.numeric(
                           unique(
                             eventData$value3)), 
                         decreasing = FALSE),
                       selected = min(
                         as.numeric(
                           unique(
                             eventData$value3))))
        ), 
        # plot graph with all results of that position
        box(
          width = 9,
          plotOutput('last_event_per_position')
        )
      )
    )
  )
}

# home and landing page of a player
playerHomeLayout <<- function(user){
  
  username <- h2(paste("Welcome", currentUser$firstname, currentUser$lastname, sep = " "))
  # form personal data
  str1 <- p(paste("Firstname", user$firstname, sep = " :  "))
  str2 <- p(paste("Lastname", user$lastname, sep = " :  "))
  str3 <- p(paste("Phone", user$phone, sep = " :  "))
  str4 <- p(paste("E-mail", user$email, sep = " :  "))
  str5 <- p(paste("Birthday", user$birthday, sep = " :  "))
  return(
    fluidPage(
      username,
      fluidRow(
        # personal data
        box(
          width = 6,
          title = "Personal info",
              HTML(paste(str1, str2, str3, str4, str5, sep = '')))),
      
      #quick data on events last time
      h4("Your results"),
      uiOutput("trainingSelectorOutput"), #event selector
      fluidRow(
        # output the quick data(percentages)
        box(width = 4,
            title = "Free Throw",
            h1(textOutput("freeThrowPercentage")),
            h3(textOutput("freeThrowCount"))),
        box(width = 4,
            title = "Dribble",
            h1(textOutput("dribblePercentage")),
            h3(textOutput("dribbleCount"))),
        box(width = 4,
            title = "Catch & Shoot",
            h1(textOutput("catchShootPercentage")),
            h3(textOutput("catchShootCount")))
      ),
      # compare to the team.
      h4("Compare to your team"),
      # selec type of shot
      radioGroupButtons(inputId = "typeselectorHomeGraph", 
                        label = "Type", 
                        status = "danger",
                        choices = setNames(c("free_throw","catch_shoot","dribble"),c("Free throw","Catch & Shoot", "From dribble")),
                        selected = "catch_shoot"),
      #plot graph
      fluidRow(
        box(
          width = 12,
          plotOutput('home_graph_player')
        )
      )
    )
  )
}


renderTrainingSelector <<- function(x){
  return(selectInput("trainingselector", "Training", x, selected = 1))

}