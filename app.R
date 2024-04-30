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

credit_data <- readRDS("Data/credit_data_list.rds")
credit_data_setor <- readRDS("Data/credit_data_list_setor.rds")
wacc_5f <- readRDS("Data/wacc_5f.rds")
merton_data <- readRDS("Data/merton_data.rds")
simulated_merton <- readRDS("Data/simulated_merton.rds")

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
      bs4Dash::menuItem("Setor", tabName = "setor", icon = shiny::icon("chart-simple")),
      bs4Dash::menuItem("WACC e Modelo de Merton", tabName = "wacc", icon = shiny::icon("chart-simple"))),

    bs4Dash::sidebarMenu(shiny::selectInput(inputId = "company", label = "Empresa", choices = unique(credit_data$margem$nome), selectize = TRUE)),

    bs4Dash::sidebarMenu(shiny::selectInput(inputId = "period", label = "Período", choices = unique(tibble::as_tibble_col(names(credit_data), column_name = "names")$names), selectize = TRUE)),

    bs4Dash::sidebarMenu(shiny::selectInput(inputId = "setor", label = "Setor", choices = unique(credit_data_setor$margem$subsetor_bovespa), selectize = TRUE))

  ),



# Body -------------------------------------------------------------------------

  body = bs4Dash::dashboardBody(

    bs4Dash::tabItems(
    #Indicadores
      bs4Dash::tabItem(

        tabName = "indicadores", # must match the tabname in the sidebar

        shiny::fluidRow(bs4Dash::column(width = 6,
                                        bs4Dash::infoBoxOutput(outputId = "divliq_pat"),
                                        bs4Dash::infoBoxOutput(outputId = "div_ebitda"),
                                        bs4Dash::infoBoxOutput(outputId = "liq_corrente"),
                                        bs4Dash::infoBoxOutput(outputId = "liq_corrente"),
                                        bs4Dash::infoBoxOutput(outputId = "liq_seca"),
                                        bs4Dash::infoBoxOutput(outputId = "liq_geral"),
                                        bs4Dash::infoBoxOutput(outputId = "div_cp"),
                                        bs4Dash::infoBoxOutput(outputId = "div_lp"),
                                        bs4Dash::infoBoxOutput(outputId = "margem_ebitda"),
                                        bs4Dash::infoBoxOutput(outputId = "margem_liq"),
                                        bs4Dash::infoBoxOutput(outputId = "fcl_divcp")
                                        ),

                        bs4Dash::column(width = 6,
                                        bs4Dash::infoBoxOutput(outputId = "divliq_pat_setor"),
                                        bs4Dash::infoBoxOutput(outputId = "div_ebitda_setor"),
                                        bs4Dash::infoBoxOutput(outputId = "liq_corrente_setor"),
                                        bs4Dash::infoBoxOutput(outputId = "liq_seca_setor"),
                                        bs4Dash::infoBoxOutput(outputId = "liq_geral_setor"),
                                        bs4Dash::infoBoxOutput(outputId = "div_cp_setor"),
                                        bs4Dash::infoBoxOutput(outputId = "div_lp_setor"),
                                        bs4Dash::infoBoxOutput(outputId = "margem_ebitda_setor"),
                                        bs4Dash::infoBoxOutput(outputId = "margem_liq_setor"),
                                        bs4Dash::infoBoxOutput(outputId = "fcl_divcp_setor")
                                        )


        )
      ),

     bs4Dash::tabItem(

       tabName = "setor",

       shiny::fluidRow(

         bs4Dash::infoBoxOutput(outputId = "divliq_pat_tabsetor"),
         bs4Dash::infoBoxOutput(outputId = "div_ebitda_tabsetor"),
         bs4Dash::infoBoxOutput(outputId = "liq_corrente_tabsetor"),
         bs4Dash::infoBoxOutput(outputId = "liq_seca_tabsetor"),
         bs4Dash::infoBoxOutput(outputId = "liq_geral_tabsetor"),
         bs4Dash::infoBoxOutput(outputId = "div_cp_tabsetor"),
         bs4Dash::infoBoxOutput(outputId = "div_lp_tabsetor"),
         bs4Dash::infoBoxOutput(outputId = "margem_ebitda_tabsetor"),
         bs4Dash::infoBoxOutput(outputId = "margem_liq_tabsetor"),
         bs4Dash::infoBoxOutput(outputId = "fcl_divcp_tabsetor")

       )

     ),

     bs4Dash::tabItem(

       tabName = "wacc",

       shiny::fluidRow(

         bs4Dash::infoBoxOutput(outputId = "we"),
         bs4Dash::infoBoxOutput(outputId = "wd"),
         bs4Dash::infoBoxOutput(outputId = "re"),
         bs4Dash::infoBoxOutput(outputId = "rd"),
         bs4Dash::infoBoxOutput(outputId = "wacc_5f"),
         bs4Dash::infoBoxOutput(outputId = "merton"),
         bs4Dash::infoBoxOutput(outputId = "value"),
         bs4Dash::infoBoxOutput(outputId = "merton_delta"),
         bs4Dash::infoBoxOutput(outputId = "merton_theta"),
         bs4Dash::infoBoxOutput(outputId = "merton_vega"),
         bs4Dash::infoBoxOutput(outputId = "merton_rho"),
         bs4Dash::infoBoxOutput(outputId = "risk")

       ),

       shiny::fluidRow(

         bs4Dash::box(shiny::plotOutput(outputId = "merton_v"), title = "Sensitivity to Market Value"),
         bs4Dash::box(shiny::plotOutput(outputId = "merton_k"), title = "Sensitivity Debt Value"),
         bs4Dash::box(shiny::plotOutput(outputId = "merton_rf"), title = "Sensitivity Interest Rate"),
         bs4Dash::box(shiny::plotOutput(outputId = "merton_t"), title = "Sensitivity Debt Maturity"),
         bs4Dash::box(shiny::plotOutput(outputId = "merton_vol"), title = "Sensitivity to stock volatility")

       )

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

credit_data_reactive_setor <- shiny::reactive({

  credit_data_setor[[input$period]]

})

credit_data_reactive_tabsetor <- shiny::reactive({

  credit_data_setor[[input$period]] |>
    dplyr::filter(subsetor_bovespa == input$setor)

})

wacc_rf_reactive <- shiny::reactive({

  wacc_5f |>
    dplyr::filter(nome == input$company)

})

merton_reactive <- shiny::reactive({

  merton_data |>
    dplyr::filter(nome == input$company)

})

simulated_reactive <- shiny::reactive({

  which(simulated_merton$companies == input$company)

})

  output$divliq_pat <- bs4Dash::renderInfoBox({

    divliq_pat <-  credit_data_reactive() |>
      dplyr::mutate(divliq_pat = div_tt_lq_em_moeda_orig_em_milhares_consolid_sim/patrim_liq_em_moeda_orig_em_milhares_consolid_sim)

    bs4Dash::infoBox(title = "Dívida Líquida/PL", value = scales::number(dplyr::last(divliq_pat$divliq_pat), accuracy = 0.01, big.mark = ".", decimal.mark = ",", suffix = "%"),
                     icon = shiny::icon("chart-line"))

  })

  output$divliq_pat_setor <- bs4Dash::renderInfoBox({

    setor <- credit_data_reactive()$subsetor_bovespa[1]

    value <- credit_data_reactive_setor() |>
      dplyr::filter(subsetor_bovespa == setor) |>
      dplyr::mutate(divliq_pat = div_tt_lq_em_moeda_orig_em_milhares_consolid_sim/patrim_liq_em_moeda_orig_em_milhares_consolid_sim)

    value <- dplyr::last(value)$divliq_pat

    bs4Dash::infoBox(title = paste("Dívida Líquida/PL", setor, sep = " - "), value = scales::number(value, accuracy = 0.01, big.mark = ".", decimal.mark = ",", suffix = "%"),
                     icon = shiny::icon("chart-line"))

  })

  output$div_ebitda <- bs4Dash::renderInfoBox({

    bs4Dash::infoBox(title = "Dívida Líquida vs EBITDA 3 meses em %", value = scales::number(dplyr::last(credit_data_reactive()$div_lq_ebitda_de_3_meses_consolid_sim), accuracy = 0.01, big.mark = ".", decimal.mark = ",", suffix = "%"),
                     icon = shiny::icon("chart-line"))

 })

  output$div_ebitda_setor <- bs4Dash::renderInfoBox({

    setor <- credit_data_reactive()$subsetor_bovespa[1]

    value <- credit_data_reactive_setor() |>
      dplyr::filter(subsetor_bovespa == setor)

    value <- value$div_lq_ebitda_de_3_meses_consolid_sim

    bs4Dash::infoBox(title = paste("Dívida Líquida vs EBITDA 3 meses em %", setor, sep = " - "), value = scales::number(value, accuracy = 0.01, big.mark = ".", decimal.mark = ",", suffix = "%"),
                     icon = shiny::icon("chart-line"))

  })

  output$liq_corrente <- bs4Dash::renderInfoBox({

    bs4Dash::infoBox(title = "Liquidez Corrente (em vezes)", value = scales::number(dplyr::last(credit_data_reactive()$liq_cor_consolid_sim), accuracy = 0.01, big.mark = ".", decimal.mark = ","),
                     icon = shiny::icon("chart-line"))

  })

  output$liq_corrente_setor <- bs4Dash::renderInfoBox({

    setor <- credit_data_reactive()$subsetor_bovespa[1]

    value <- credit_data_reactive_setor() |>
      dplyr::filter(subsetor_bovespa == setor)

    value <- value$liq_cor_consolid_sim

    bs4Dash::infoBox(title = paste("Liquidez Corrente (em vezes)", setor, sep = " - "), value = scales::number(value, accuracy = 0.01, big.mark = ".", decimal.mark = ","),
                     icon = shiny::icon("chart-line"))

  })

  output$liq_seca <- bs4Dash::renderInfoBox({

    bs4Dash::infoBox(title = "Liquidez Seca (em vezes)", value = scales::number(dplyr::last(credit_data_reactive()$liq_sec_consolid_sim), accuracy = 0.01, big.mark = ".", decimal.mark = ","),
                     icon = shiny::icon("chart-line"))

  })

  output$liq_seca_setor <- bs4Dash::renderInfoBox({

    setor <- credit_data_reactive()$subsetor_bovespa[1]

    value <- credit_data_reactive_setor() |>
      dplyr::filter(subsetor_bovespa == setor)

    value <- value$liq_sec_consolid_sim

    bs4Dash::infoBox(title = paste("Liquidez Seca (em vezes)", setor, sep = " - "), value = scales::number(value, accuracy = 0.01, big.mark = ".", decimal.mark = ","),
                     icon = shiny::icon("chart-line"))

  })

 output$liq_geral <- bs4Dash::renderInfoBox({

    bs4Dash::infoBox(title = "Liquidez Geral (em vezes)", value = scales::number(dplyr::last(credit_data_reactive()$liq_ger_consolid_sim), accuracy = 0.01, big.mark = ".", decimal.mark = ","),
                     icon = shiny::icon("chart-line"))

  })

 output$liq_geral_setor <- bs4Dash::renderInfoBox({

   setor <- credit_data_reactive()$subsetor_bovespa[1]

   value <- credit_data_reactive_setor() |>
     dplyr::filter(subsetor_bovespa == setor)

   value <- value$liq_ger_consolid_sim

   bs4Dash::infoBox(title = paste("Liquidez Geral (em vezes)", setor, sep = " - "), value = scales::number(value, accuracy = 0.01, big.mark = ".", decimal.mark = ","),
                    icon = shiny::icon("chart-line"))

 })

 output$div_cp <- bs4Dash::renderInfoBox({

   bs4Dash::infoBox(title = "Dívida no Curto Prazo (%)", value = scales::number(dplyr::last(credit_data_reactive()$div_cp_div_tt_consolid_sim), accuracy = 0.01, big.mark = ".", decimal.mark = ",", suffix = "%"),
                    icon = shiny::icon("chart-line"))

 })

 output$div_cp_setor <- bs4Dash::renderInfoBox({

   setor <- credit_data_reactive()$subsetor_bovespa[1]

   value <- credit_data_reactive_setor() |>
     dplyr::filter(subsetor_bovespa == setor)

   value <- value$div_cp_div_tt_consolid_sim

   bs4Dash::infoBox(title = paste("Dívida no Curto Prazo (%)", setor, sep = " - "), value = scales::number(value, accuracy = 0.01, big.mark = ".", decimal.mark = ",", suffix = "%"),
                    icon = shiny::icon("chart-line"))

 })

 output$div_lp <- bs4Dash::renderInfoBox({

   bs4Dash::infoBox(title = "Dívida no Longo Prazo (%)", value = scales::number(dplyr::last(credit_data_reactive()$div_lp_div_tt_consolid_sim), accuracy = 0.01, big.mark = ".", decimal.mark = ",", suffix = "%"),
                    icon = shiny::icon("chart-line"))

 })

 output$div_lp_setor <- bs4Dash::renderInfoBox({

   setor <- credit_data_reactive()$subsetor_bovespa[1]

   value <- credit_data_reactive_setor() |>
     dplyr::filter(subsetor_bovespa == setor)

   value <- value$div_lp_div_tt_consolid_sim

   bs4Dash::infoBox(title = paste("Dívida no Longo Prazo (%)", setor, sep = " - "), value = scales::number(value, accuracy = 0.01, big.mark = ".", decimal.mark = ",", suffix = "%"),
                    icon = shiny::icon("chart-line"))

 })

 output$margem_ebitda <- bs4Dash::renderInfoBox({

   bs4Dash::infoBox(title = "Margem Ebitda (%)", value = scales::number(dplyr::last(credit_data_reactive()$mrg_ebitda_de_3_meses_consolid_sim), accuracy = 0.01, big.mark = ".", decimal.mark = ",", suffix = "%"),
                    icon = shiny::icon("chart-line"))

 })

 output$margem_ebitda_setor <- bs4Dash::renderInfoBox({

   setor <- credit_data_reactive()$subsetor_bovespa[1]

   value <- credit_data_reactive_setor() |>
     dplyr::filter(subsetor_bovespa == setor)

   value <- value$mrg_ebitda_de_3_meses_consolid_sim

     bs4Dash::infoBox(title = paste("Margem Ebitda (%)", setor, sep = " - "), value = scales::number(value, accuracy = 0.01, big.mark = ".", decimal.mark = ",", suffix = "%"),
                      icon = shiny::icon("chart-line"))

 })

 output$margem_liq <- bs4Dash::renderInfoBox({

   bs4Dash::infoBox(title = "Margem Líquida (%)", value = scales::number(dplyr::last(credit_data_reactive()$margem_liquida_de_3_meses_consolid_sim), accuracy = 0.01, big.mark = ".", decimal.mark = ",", suffix = "%"),
                    icon = shiny::icon("chart-line"))

 })

 output$margem_liq_setor <- bs4Dash::renderInfoBox({

   setor <- credit_data_reactive()$subsetor_bovespa[1]

   value <- credit_data_reactive_setor() |>
     dplyr::filter(subsetor_bovespa == setor)

   value <- value$margem_liquida_de_3_meses_consolid_sim

   bs4Dash::infoBox(title = paste("Margem Líquida (%)", setor, sep = " - "), value = scales::number(value, accuracy = 0.01, big.mark = ".", decimal.mark = ",", suffix = "%"),
                    icon = shiny::icon("chart-line"))

 })

 output$fcl_divcp <- bs4Dash::renderInfoBox({

   value <- dplyr::last(credit_data_reactive()$fcl_em_moeda_orig_no_exercicio_consolid_sim) /
            dplyr::last(credit_data_reactive()$div_cp_div_tt_consolid_sim)

   bs4Dash::infoBox(title = "Caixa/Dívida de CP", value = scales::number(value, accuracy = 0.01, big.mark = ".", decimal.mark = ","),
                    icon = shiny::icon("chart-line"))

 })

 output$fcl_divcp_setor <- bs4Dash::renderInfoBox({

   setor <- credit_data_reactive()$subsetor_bovespa[1]

   value <- credit_data_reactive_setor() |>
     dplyr::filter(subsetor_bovespa == setor) |>
     dplyr::mutate(value = (fcl_em_moeda_orig_no_exercicio_consolid_sim) /
                           (div_cp_div_tt_consolid_sim))

   value <- dplyr::last(value)$value

   bs4Dash::infoBox(title = paste("Caixa/Dívida de CP", setor, sep = " - "), value = scales::number(value, accuracy = 0.01, big.mark = ".", decimal.mark = ","),
                    icon = shiny::icon("chart-line"))

 })

 output$divliq_pat_tabsetor <- bs4Dash::renderInfoBox({

   divliq_pat <- credit_data_reactive_tabsetor() |>
     dplyr::mutate(divliq_pat = div_tt_lq_em_moeda_orig_em_milhares_consolid_sim/patrim_liq_em_moeda_orig_em_milhares_consolid_sim)

   bs4Dash::infoBox(title = "Dívida Líquida/PL", value = scales::number(dplyr::last(divliq_pat$divliq_pat), accuracy = 0.01, big.mark = ".", decimal.mark = ",", suffix = "%"),
                    icon = shiny::icon("chart-line"))

 })

 output$div_ebitda_tabsetor <- bs4Dash::renderInfoBox({

   bs4Dash::infoBox(title = "Dívida Líquida vs EBITDA 3 meses em %", value = scales::number(dplyr::last(credit_data_reactive_tabsetor()$div_lq_ebitda_de_3_meses_consolid_sim), accuracy = 0.01, big.mark = ".", decimal.mark = ",", suffix = "%"),
                    icon = shiny::icon("chart-line"))

 })

 output$liq_corrente_tabsetor <- bs4Dash::renderInfoBox({

   bs4Dash::infoBox(title = "Liquidez Corrente (em vezes)", value = scales::number(dplyr::last(credit_data_reactive_tabsetor()$liq_cor_consolid_sim), accuracy = 0.01, big.mark = ".", decimal.mark = ","),
                    icon = shiny::icon("chart-line"))

 })

 output$liq_seca_tabsetor <- bs4Dash::renderInfoBox({

   bs4Dash::infoBox(title = "Liquidez Seca (em vezes)", value = scales::number(dplyr::last(credit_data_reactive_tabsetor()$liq_sec_consolid_sim), accuracy = 0.01, big.mark = ".", decimal.mark = ","),
                    icon = shiny::icon("chart-line"))

 })

 output$liq_geral_tabsetor <- bs4Dash::renderInfoBox({

   bs4Dash::infoBox(title = "Liquidez Geral (em vezes)", value = scales::number(dplyr::last(credit_data_reactive_tabsetor()$liq_ger_consolid_sim), accuracy = 0.01, big.mark = ".", decimal.mark = ","),
                    icon = shiny::icon("chart-line"))

 })

 output$div_cp_tabsetor <- bs4Dash::renderInfoBox({

   bs4Dash::infoBox(title = "Dívida no Curto Prazo (%)", value = scales::number(dplyr::last(credit_data_reactive_tabsetor()$div_cp_div_tt_consolid_sim), accuracy = 0.01, big.mark = ".", decimal.mark = ",", suffix = "%"),
                    icon = shiny::icon("chart-line"))

 })

 output$div_lp_tabsetor <- bs4Dash::renderInfoBox({

   bs4Dash::infoBox(title = "Dívida no Longo Prazo (%)", value = scales::number(dplyr::last(credit_data_reactive_tabsetor()$div_cp_div_tt_consolid_sim), accuracy = 0.01, big.mark = ".", decimal.mark = ",", suffix = "%"),
                    icon = shiny::icon("chart-line"))

 })

 output$margem_ebitda_tabsetor <- bs4Dash::renderInfoBox({

   bs4Dash::infoBox(title = "Margem Ebitda (%)", value = scales::number(dplyr::last(credit_data_reactive_tabsetor()$mrg_ebitda_de_3_meses_consolid_sim), accuracy = 0.01, big.mark = ".", decimal.mark = ",", suffix = "%"),
                    icon = shiny::icon("chart-line"))

 })

 output$margem_liq_tabsetor <- bs4Dash::renderInfoBox({

   bs4Dash::infoBox(title = "Margem Líquida (%)", value = scales::number(dplyr::last(credit_data_reactive_tabsetor()$margem_liquida_de_3_meses_consolid_sim), accuracy = 0.01, big.mark = ".", decimal.mark = ",", suffix = "%"),
                    icon = shiny::icon("chart-line"))

 })

 output$fcl_divcp_tabsetor <- bs4Dash::renderInfoBox({

   value <- dplyr::last(credit_data_reactive_tabsetor()$fcl_em_moeda_orig_no_exercicio_consolid_sim) /
     dplyr::last(credit_data_reactive_tabsetor()$div_cp_div_tt_consolid_sim)

   bs4Dash::infoBox(title = "Caixa/Dívida de CP", value = scales::number(value, accuracy = 0.01, big.mark = ".", decimal.mark = ","),
                    icon = shiny::icon("chart-line"))

 })

 output$we <- bs4Dash::renderInfoBox({

   bs4Dash::infoBox(title = "WE (%) - Equity Cost Weight", value = scales::number(wacc_rf_reactive()$we[1]*100, accuracy = 0.01, big.mark = ".", decimal.mark = ","),
                    icon = shiny::icon("chart-line"))

 })

 output$wd <- bs4Dash::renderInfoBox({

   bs4Dash::infoBox(title = "WD (%) - Debt Cost Weight", value = scales::number(wacc_rf_reactive()$wd[1]*100, accuracy = 0.01, big.mark = ".", decimal.mark = ","),
                    icon = shiny::icon("chart-line"))

 })

 output$re <- bs4Dash::renderInfoBox({

   bs4Dash::infoBox(title = "RE (%) - Equity Cost", value = scales::number(wacc_rf_reactive()$re[1]*100, accuracy = 0.01, big.mark = ".", decimal.mark = ","),
                    icon = shiny::icon("chart-line"))

 })

 output$rd <- bs4Dash::renderInfoBox({

   bs4Dash::infoBox(title = "RD (%) - Debt Cost (tax free)", value = scales::number(wacc_rf_reactive()$rd[1]*(1 - .34)*100, accuracy = 0.01, big.mark = ".", decimal.mark = ","),
                    icon = shiny::icon("chart-line"))

 })

 output$wacc_5f <- bs4Dash::renderInfoBox({

   bs4Dash::infoBox(title = "WACC (%) - Real Terms", value = scales::number(wacc_rf_reactive()$wacc[1]*100, accuracy = 0.01, big.mark = ".", decimal.mark = ","),
                    icon = shiny::icon("chart-line"))

 })

 output$merton <- bs4Dash::renderInfoBox({

   bs4Dash::infoBox(title = "Valuation", value = scales::number(merton_reactive()$merton_value, accuracy = 0.01, big.mark = ".", decimal.mark = ","),
                    icon = shiny::icon("chart-line"))

 })

 output$value <- bs4Dash::renderInfoBox({

   bs4Dash::infoBox(title = "Valor de Mercado", value = scales::number(merton_reactive()$stock_price, accuracy = 0.01, big.mark = ".", decimal.mark = ","),
                    icon = shiny::icon("chart-line"))

 })

 output$merton_delta <- bs4Dash::renderInfoBox({

   bs4Dash::infoBox(title = "Delta", value = scales::number(merton_reactive()$merton_delta, accuracy = 0.01, big.mark = ".", decimal.mark = ","),
                    icon = shiny::icon("chart-line"))

 })

 output$merton_vega <-  bs4Dash::renderInfoBox({

   bs4Dash::infoBox(title = "Vega", value = scales::number(merton_reactive()$merton_vega, accuracy = 0.01, big.mark = ".", decimal.mark = ","),
                    icon = shiny::icon("chart-line"))

 })

 output$merton_theta <-  bs4Dash::renderInfoBox({

   bs4Dash::infoBox(title = "Theta", value = scales::number(merton_reactive()$merton_theta, accuracy = 0.01, big.mark = ".", decimal.mark = ","),
                    icon = shiny::icon("chart-line"))

 })

 output$merton_rho <-  bs4Dash::renderInfoBox({

   bs4Dash::infoBox(title = "Rho", value = scales::number(merton_reactive()$merton_rho, accuracy = 0.01, big.mark = ".", decimal.mark = ","),
                    icon = shiny::icon("chart-line"))

 })

 output$risk <- bs4Dash::renderInfoBox({

   bs4Dash::infoBox(title = "Risk", value = scales::number((merton_reactive()$stock_price/merton_reactive()$merton_value - 1)*100, accuracy = 0.01, big.mark = ".", decimal.mark = ",", suffix = "%"),
                    icon = shiny::icon("chart-line"))

 })

 output$merton_v <- shiny::renderPlot({

   n = simulated_reactive()

   data <- tibble::as_tibble(simulated_merton$v[n], .name_repair = "minimal") |>
     dplyr::rename(v = c(1))

   data |>
     ggplot2::ggplot(ggplot2::aes(x = seq(from = 1, to = nrow(data), by = 1), y = v)) +
     ggplot2::geom_line() +
     ggplot2::labs(x = "50% a 150% do valor observado.", y = "Merton Valuation") +
     ggthemes::theme_economist()

 })

 output$merton_k <- shiny::renderPlot({

   n = simulated_reactive()

   data <- tibble::as_tibble(simulated_merton$k[n], .name_repair = "minimal") |>
     dplyr::rename(k = c(1))

   data |>
     ggplot2::ggplot(ggplot2::aes(x = seq(from = 1, to = nrow(data), by = 1), y = k)) +
     ggplot2::geom_line() +
     ggplot2::labs(x = "50% a 150% do valor observado.", y = "Merton Valuation") +
     ggthemes::theme_economist()

 })

 output$merton_rf <- shiny::renderPlot({

   n = simulated_reactive()

   data <- tibble::as_tibble(simulated_merton$rf[n], .name_repair = "minimal") |>
     dplyr::rename(rf = c(1))

   data |>
     ggplot2::ggplot(ggplot2::aes(x = seq(from = 1, to = nrow(data), by = 1), y = rf)) +
     ggplot2::geom_line() +
     ggplot2::labs(x = "50% a 150% do valor observado.", y = "Merton Valuation") +
     ggthemes::theme_economist()

 })

 output$merton_t <- shiny::renderPlot({

   n = simulated_reactive()

   data <- tibble::as_tibble(simulated_merton$t[n], .name_repair = "minimal") |>
     dplyr::rename(t = c(1))

   data |>
     ggplot2::ggplot(ggplot2::aes(x = seq(from = 1, to = nrow(data), by = 1), y = t)) +
     ggplot2::geom_line() +
     ggplot2::labs(x = "50% a 150% do valor observado.", y = "Merton Valuation") +
     ggthemes::theme_economist()

 })

 output$merton_vol <- shiny::renderPlot({

   n = simulated_reactive()

   data <- tibble::as_tibble(simulated_merton$vol[n], .name_repair = "minimal") |>
     dplyr::rename(vol = c(1))

   data |>
     ggplot2::ggplot(ggplot2::aes(x = seq(from = 1, to = nrow(data), by = 1), y = vol)) +
     ggplot2::geom_line() +
     ggplot2::labs(x = "50% a 150% do valor observado.", y = "Merton Valuation") +
     ggthemes::theme_economist()

 })

}

# Shiny App --------------------------------------------------------------------

shinyApp(ui, server)

# Incluir os valores para o setor da empresa
# Talvez incluir o gráfico da distribuição do wacc indicando o ponto onde a empresa se encontra. Dentro do setor tb?
# Merton Model
