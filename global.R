
#connect to MySQL

library(RJDBC)
library(RODBC)
library(DBI)
library(shinythemes)
library(shinydashboard)
library(plyr)

options(java.parameters = "-Xmx2g")

#run this in sqlserver to delete all test events, do not do this if there are more real events in the database!!!!
#delete from testresult4values where eventid != 401
#delete from userEvent where eventid != 401
#delete from event where eventid != 401


#driver on local pc
drv <- JDBC("com.microsoft.sqlserver.jdbc.SQLServerDriver", "./opt/sqljdbc/sqljdbc4-2.0.jar")

#driver on online server
#drv <- JDBC("com.microsoft.sqlserver.jdbc.SQLServerDriver", "C:/999 DB/20 Mijn MSSQLserver/sqljdbc4-2.0.jar")

#connection with local server
conn <- dbConnect(drv, "jdbc:sqlserver://localhost;databaseName=ztrieruc001;user=basketbal;password=Password1!")

#connection with online server
#conn <- dbConnect(drv, "jdbc:sqlserver://145.92.162.226;databaseName=ztrieruc001;user=trieruc001;password=TTBGPfqU6gsOJG")

#positions
positionsAll       <- c(1:14)
positionsLeft      <- c(1, 2, 3, 7, 8, 12)
positionsRight     <- c(4, 5, 6, 9, 10, 14)
positionsCenter    <- c(3, 4, 8, 9, 11, 13)
positionsInCircle  <- c(2, 3, 4, 5, 7, 8, 9, 10, 11)
positionsOutCircle <- c(1, 6, 12, 13, 14)
locationX          <- c(4, 21, 41, 59, 79, 98, 21, 41, 59, 79, 50, 10, 50, 92)
locationY          <- c(77, 88, 88, 88, 88, 77, 50, 55, 55, 50, 25, 18, 3, 18)

inEvent <<- FALSE

positionLocations  <-
  data.frame(positions = positionsAll, locationX, locationY)

playersInEvent <- ''
latestEventid  <- ''


getAllPlayers <- function(){
  
  query <- paste0(
    "exec GETPLAYERLIST"
  )
  allPlayers <<- dbGetQuery(conn, query)
  
  allPlayers$fullname <<-
    paste(allPlayers$firstname, allPlayers$lastname, sep = " ")
  #allPlayers <- data.frame(allPlayers$fullname, allPlayers$accountid)
  allPlayerChoices <<-
    setNames(as.numeric(allPlayers$accountid), allPlayers$fullname)
  
}

getAllTeams <- function(){
  query <- paste0(
    "exec GETTEAMLISt"
  )
  allTeams <<- dbGetQuery(conn, query)
}
getAllPlayers()
getAllTeams()

