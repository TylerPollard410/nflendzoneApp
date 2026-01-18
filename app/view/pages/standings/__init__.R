box::use(
  box[export],
)

box::use(
  app / view / pages / standings / server[server],
  app / view / pages / standings / ui[ui],
)

standings_ui <- ui
standings_server <- server

export(ui, server, standings_ui, standings_server)
