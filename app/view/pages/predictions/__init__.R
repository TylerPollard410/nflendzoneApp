box::use(
  box[export],
)

box::use(
  app / view / pages / predictions / server[server],
  app / view / pages / predictions / ui[ui],
)

predictions_ui <- ui
predictions_server <- server

export(ui, server, predictions_ui, predictions_server)
