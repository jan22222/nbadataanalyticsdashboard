library(rsconnect)
list.files()
deployApp(
  appDir = "C:/Users/siegf/Desktop/nba", 
  appPrimaryDoc = "app.R"
)
1
rsconnect::deployApp(appName = "nba-dashboard")