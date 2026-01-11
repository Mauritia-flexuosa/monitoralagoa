library(rsconnect)

rsconnect::setAccountInfo(
  name='marciocure',
  token='********************************',
  secret='*************************************')

rsconnect::deployApp(
  appDir = ".../monitoralagoa/",
  appName = "monitoralagoa",
  account = "marciocure"
)