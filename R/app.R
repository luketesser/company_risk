#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

library(shiny)
library(bs4Dash)
library(ggplot2)
library(dplyr)
library(lubridate)
library(ggthemes)
library(vroom)
# source("R/helpers.R")

credit_data <- readRDS("../Data/credit_data_list.rds")

# Application UI ---------------------------------------------------------------

ui <- bs4Dash::dashboardPage(

  dark = TRUE,

  help = NULL,

# Header -----------------------------------------------------------------------

  header = bs4Dash::dashboardHeader(

    title = tagList(
      span(class = "logo-lg", "Risco de Crédito Corporativo")
    ),

    titleWidth = "300px",

    # Message box top-right corner

    rightUi = bs4Dash::dropdownMenu(

      type = "tasks",
      icon = shiny::icon("circle-chevron-down"),

      bs4Dash::messageItem(
        from    = "Open Source",
        message = "Veja no Github",
        icon    = icon("github"),
        time    = as.character(Sys.time()),
        href    = "https://github.com/luketesser/company_risk"
      ),

      bs4Dash::messageItem(
        from    = "Reta Asset",
        message = "Conheça a Reta Asset",
        icon    = icon("chart-line"),
        time    = as.character(Sys.time()),
        href    = "https://www.retaasset.com"
      )

    )

  ),

# Sidebar ----------------------------------------------------------------------

  sidebar = bs4Dash::dashboardSidebar(

    bs4Dash::sidebarMenu(

      bs4Dash::menuItem("Indicadores", tabName = "indicadores", icon = shiny::icon("chart-simple")), # fontawesome.com/search?q=chart&o=r&m=free
      bs4Dash::menuItem("WACC", tabName = "wacc", icon = shiny::icon("chart-simple"))),

    bs4Dash::sidebarMenu(shiny::selectInput(inputId = "company", label = "Empresa", choices = unique(credit_data$margem$nome), selectize = TRUE)),

    bs4Dash::sidebarMenu(shiny::selectInput(inputId = "period", label = "Período", choices = unique(tibble::as_tibble_col(names(credit_data), column_name = "names")$names), selectize = TRUE))

  ),



# Body -------------------------------------------------------------------------

  body = bs4Dash::dashboardBody(

    bs4Dash::tabItems(
    #Indicadores
      bs4Dash::tabItem(

        tabName = "indicadores", # must match the tabname in the sidebar

        shiny::fluidRow(bs4Dash::infoBoxOutput(outputId = "divliq_pat")),

        shiny::fluidRow(bs4Dash::infoBoxOutput(outputId = "div_ebitda")),

        shiny::fluidRow(bs4Dash::infoBoxOutput(outputId = "liq_corrente")),

        shiny::fluidRow(bs4Dash::infoBoxOutput(outputId = "liq_seca")),

        shiny::fluidRow(bs4Dash::infoBoxOutput(outputId = "liq_geral")),

        shiny::fluidRow(bs4Dash::infoBoxOutput(outputId = "div_cp")),

        shiny::fluidRow(bs4Dash::infoBoxOutput(outputId = "div_lp")),

        shiny::fluidRow(bs4Dash::infoBoxOutput(outputId = "margem_ebitda")),

        shiny::fluidRow(bs4Dash::infoBoxOutput(outputId = "margem_liq")),

        shiny::fluidRow(bs4Dash::infoBoxOutput(outputId = "fcl_divcp"))

        # shiny::fluidRow(bs4Dash::box(shiny::plotOutput("test_plot"), title = "test", collapsible = T, collapsed = T, solidHeader = T, status = "primary"))



      ),

     bs4Dash::tabItem(

       tabName = "wacc" # must match the tabname in the sidebar

     )

    )

  ),

# Footer -----------------------------------------------------------------------

  footer = bs4Dash::dashboardFooter(

    left  = shiny::p("Desenvolvido por ", shiny::strong("Reta Asset LTDA")),
    right  = shiny::p("Porto Alegre, Rio Grande do Sul, ", shiny::strong(lubridate::today()))

  )

)

# Server -----------------------------------------------------------------------

server <- function(input, output){

credit_data_reactive <- shiny::reactive({

    credit_data[[input$period]] |>
    dplyr::filter(nome == input$company)


})

  # output$test_plot <- shiny::renderPlot({
  #
  #   cucu_data <- reactive({
  #
  #     cucu1 <- credit_data |>
  #       filter(nome == input$company)
  #
  #     return(cucu1)
  #
  #   })
  #
  #   cucu_data() |>
  #     ggplot2::ggplot(ggplot2::aes(x = date,  y = est_cap_pl_consolid_sim)) +
  #     ggplot2::geom_col() +
  #     ggthemes::theme_economist()
  #
  # })

  output$divliq_pat <- bs4Dash::renderInfoBox({

    divliq_pat <-  credit_data_reactive() |>
      dplyr::mutate(divliq_pat = div_tt_lq_em_moeda_orig_em_milhares_consolid_sim/patrim_liq_em_moeda_orig_em_milhares_consolid_sim)

    bs4Dash::infoBox(title = "Dívida Líquida/PL", value = scales::number(dplyr::last(divliq_pat$divliq_pat), accuracy = 0.01, big.mark = ".", decimal.mark = ",", suffix = "%"),
                     icon = shiny::icon("chart-line"))

  })

  output$div_ebitda <- bs4Dash::renderInfoBox({

    bs4Dash::infoBox(title = "Dívida Líquida vs EBITDA 3 meses em %", value = scales::number(dplyr::last(credit_data_reactive()$div_lq_ebitda_de_3_meses_consolid_sim), accuracy = 0.01, big.mark = ".", decimal.mark = ",", suffix = "%"),
                     icon = shiny::icon("chart-line"))

 })

  output$liq_corrente <- bs4Dash::renderInfoBox({

    bs4Dash::infoBox(title = "Liquidez Corrente (em vezes)", value = scales::number(dplyr::last(credit_data_reactive()$liq_cor_consolid_sim), accuracy = 0.01, big.mark = ".", decimal.mark = ","),
                     icon = shiny::icon("chart-line"))

  })

  output$liq_seca <- bs4Dash::renderInfoBox({

    bs4Dash::infoBox(title = "Liquidez Seca (em vezes)", value = scales::number(dplyr::last(credit_data_reactive()$liq_sec_consolid_sim), accuracy = 0.01, big.mark = ".", decimal.mark = ","),
                     icon = shiny::icon("chart-line"))

  })

  output$liq_geral <- bs4Dash::renderInfoBox({

    bs4Dash::infoBox(title = "Liquidez Geral (em vezes)", value = scales::number(dplyr::last(credit_data_reactive()$liq_ger_consolid_sim), accuracy = 0.01, big.mark = ".", decimal.mark = ","),
                     icon = shiny::icon("chart-line"))

  })

 output$div_cp <- bs4Dash::renderInfoBox({

   bs4Dash::infoBox(title = "Dívida no Curto Prazo (%)", value = scales::number(dplyr::last(credit_data_reactive()$div_cp_div_tt_consolid_sim), accuracy = 0.01, big.mark = ".", decimal.mark = ",", suffix = "%"),
                    icon = shiny::icon("chart-line"))

 })

 output$div_lp <- bs4Dash::renderInfoBox({

   bs4Dash::infoBox(title = "Dívida no Longo Prazo (%)", value = scales::number(dplyr::last(credit_data_reactive()$div_cp_div_tt_consolid_sim), accuracy = 0.01, big.mark = ".", decimal.mark = ",", suffix = "%"),
                    icon = shiny::icon("chart-line"))

 })

 output$margem_ebitda <- bs4Dash::renderInfoBox({

   bs4Dash::infoBox(title = "Margem Ebitda (%)", value = scales::number(dplyr::last(credit_data_reactive()$mrg_ebitda_de_3_meses_consolid_sim), accuracy = 0.01, big.mark = ".", decimal.mark = ",", suffix = "%"),
                    icon = shiny::icon("chart-line"))

 })

 output$margem_liq <- bs4Dash::renderInfoBox({

   bs4Dash::infoBox(title = "Margem Líquida (%)", value = scales::number(dplyr::last(credit_data_reactive()$margem_liquida_de_3_meses_consolid_sim), accuracy = 0.01, big.mark = ".", decimal.mark = ",", suffix = "%"),
                    icon = shiny::icon("chart-line"))

 })

 output$fcl_divcp <- bs4Dash::renderInfoBox({

   value <- dplyr::last(credit_data_reactive()$fcl_em_moeda_orig_no_exercicio_consolid_sim) /
            dplyr::last(credit_data_reactive()$div_cp_div_tt_consolid_sim)

   bs4Dash::infoBox(title = "Caixa/Dívida de CP", value = scales::number(value, accuracy = 0.01, big.mark = ".", decimal.mark = ","),
                    icon = shiny::icon("chart-line"))

 })

}

# Shiny App --------------------------------------------------------------------

shinyApp(ui, server)


# Fazer uma aba em que ve as mesmas coisas só que podendo filtrar por setor.
