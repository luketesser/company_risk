# Financial statemaents Data from Economatica ----------------------------------

# url <- "https://api.data.economatica.com/1/oficial/datafeed/download/1/a7VM3DNNG4f9CX6YtOqJMc4rYEuysOYTn1592Ruoh4YKDFpSUAx3uaWQQt6%2FdbGdu79HNTjBqXGy8UaNc7AL%2BPMIxuq9zHbMdd2ANQAP8yrtil%2FM95c%2F3MMpv3GkdRENxqkXnZ16EqI6F2UA8v%2BXkQasM%2B%2FyJ97C2d43zEfxyY1gu622uzuq5XCEPim1oF8rmTVE9ub5yPkCGHSC1fu63Hwbb5diB2rXL%2FFVQfdrWPPl%2FeSTHOlmN7QwBGNICI8HFnbqZtqAnxywKlZGuMV6%2F7sPFqeaDp7hN6deq5WTFLHgzzofNBOURdthU%2FnL0JrVygHlUlYxuhkpBk5V4nRpTg%3D%3D"

url <- "https://api.data.economatica.com/1/oficial/datafeed/download/1/mdZmSrKkd8%2B%2FgpHLsy%2BYgB5W%2FiKXxQT85DHWmX6DrEq%2Blj3SAeSpj3J77HCO9%2BpwTrYIw5EfZbNeWBtV5gDnmatTA3dnmIBkQty2vvE6LzJHQaBvQ20REA7RYUitWBmg4E3CiB8auOodMQcthhr2w0qt%2BExAM9P5r9%2B60vJ0zdSngZ0E0ZSTbg4Y7Fu97hbnJMBl1BeS9Ss6JUf1awjAGalpwOOvCc%2Fis0HM6axhjq8OQhJi3WYAQAhYS%2Fulk9jrWisShWEy2IRiLskxskOEF%2FvTntqpHY3bLWpnDLU6WZ9uMtBShCRcUjCQYn9IEBdNoWyWfhIsBqJp94xmz0LoAw%3D%3D"

# url2 <- "https://api.data.economatica.com/1/oficial/datafeed/download/1/gwLQ37fNt88xFQzAEDvgHbNKFajTXJ6GhSVu6IS5osOfqHayUOhOYOZ0aU85iFKOp9siH0C1jkRSZeVsOJdqTnxtQBOlQ7Ri0Y7DHiRgtx7aHXIrpR%2Fl9bVSfkM%2B7beTlP%2BJGDkNd7HYxLwn50RFGDvXHy%2BaCif3E9JB2IKzpZrPtxs%2F4tSpUsj0eW2TeCQ6ATKKIITeB8kuJOQUL%2BTX3nxanvh9p5eB3IZQ1TklXO8gvXSJvhXAqOqXjpofspCCYG19AQc4Lo2a4%2BzNbjHoHKx9WEo5l5mzk%2F40ZzH%2B8R%2BQC6JFTFf8SYSL4njD%2Fbu%2BwfheJc%2FH%2BeBOXtJtwvE9tg%3D%3D"

url2 <- "https://api.data.economatica.com/1/oficial/datafeed/download/1/TrFrvsaBVvj0cCt7yROZY31S7z6EALX1daODKIpgk07F5Z%2F5h3dIrVc2BoZFZPrydR5zFPFfeXgl8UOAkFbrI1at2KZzySumQ1FCOpJA8EgesqrI60ctCElwD9EJTlSCy%2BDsLNs0YYz8Vw5Uz7AOJa6IV0bicUeVHw%2FyX0hVeOQugOJh6mjUetkm6lgqJtlxduXuV0nPHBsUR7XKV44bvrBkEugtZJjBUBHOCTqEwxOdMZH9mk5X9Uql8AuBHbskzoGHguWXuqw%2F4tRpNerQPsvJ0WSEU9JkuNVhGFnjjpiC08T3dSGJL1ml5UUqaJdb58aLIeSGA3nlvNgNJ4YTAw%3D%3D"

# url2 <- "https://api.data.economatica.com/1/oficial/datafeed/download/1/kDTT31rRCcBvGk753nfs9FU%2BbPflomweKgEplA0Hhl9uxWn0Z6h7zs57152ep8j%2BXXQ8j1vHX3ubjV04eY2mwr1VcqUKg3ysmyLAvrmDG%2F015NZe9MUORqw4%2BoEcptpWHtpuXiwOYwHD8iRKwc3wdFDwtLOygt0kt344yAAM3vX3uw7vOcSmkjCupyfT2mljBTilfqQvej7l%2F15d8fB1GH9KK6GnFvvScOo1gSap84jicUCX%2B0JVed%2Bly1wHru%2BJXRDcNz7OyXpEGeM20O12EShs8YVwE4y3fW23ZxHsga6twe9qh%2FUIWZ5STIF2yQlSKOck9M6bUOUf%2BWTJfo6Nog%3D%3D"

data <-  vroom::vroom(file = url, delim = ",", locale = vroom::locale(encoding = "latin1")) |>
  janitor::clean_names()

data2 <-  vroom::vroom(file = url2, delim = ",", locale = vroom::locale(encoding = "latin1")) |>
  janitor::clean_names()

credit_data <- data |>
  dplyr::mutate(quarter = as.double(substr(data, 1, 1)), year = substr(data, 3, 6), date = as.Date(paste0(year, "-", (quarter - 1)*3 + 1, "-01"))) |>
  dplyr::select(-quarter, -year) |>
  dplyr::left_join(dplyr::select(data2, ativo, nome, codigo, cnpj, subsetor_bovespa)) |>
  dplyr::select(dplyr::last_col(4), dplyr::last_col(3), dplyr::last_col(2), dplyr::last_col(1), dplyr::last_col(), everything()) |>
  dplyr::mutate(dplyr::across(c(8:dplyr::last_col()), as.double))

setores <- credit_data |>
  dplyr::group_by(nome) |>
  dplyr::filter(date == max(date)) |>
  dplyr::select(nome, subsetor_bovespa, cnpj)

credit_data_setor <- credit_data |>
  dplyr::group_by(nome) |>
  dplyr::filter(date == max(date)) |>
  dplyr::ungroup() |>
  dplyr::group_by(subsetor_bovespa) |>
  dplyr::mutate(subsetor_bovespa = dplyr::if_else(subsetor_bovespa == "-", "Outros", subsetor_bovespa)) |>
  dplyr::summarise(dplyr::across(c(8:50), mean, na.rm = T))

credit_data_12m <- credit_data |>
  dplyr::group_by(nome) |>
  dplyr::slice((dplyr::n() - 3):dplyr::n()) |>
  dplyr::summarise(dplyr::across(c(7:50), mean , na.rm = T)) |>
  dplyr::inner_join(setores)

credit_data_12m_setor <- credit_data_12m |>
  dplyr::group_by(subsetor_bovespa) |>
  dplyr::mutate(subsetor_bovespa = dplyr::if_else(subsetor_bovespa == "-", "Outros", subsetor_bovespa)) |>
  dplyr::summarise(dplyr::across(c(2:45), mean, na.rm = T))

credit_data_24m <- credit_data |>
  dplyr::group_by(nome) |>
  dplyr::slice((dplyr::n() - 7):dplyr::n()) |>
  dplyr::summarise(dplyr::across(c(7:50), mean , na.rm = T)) |>
  dplyr::inner_join(setores)

credit_data_24m_setor <- credit_data_24m |>
  dplyr::group_by(subsetor_bovespa) |>
  dplyr::mutate(subsetor_bovespa = dplyr::if_else(subsetor_bovespa == "-", "Outros", subsetor_bovespa)) |>
  dplyr::summarise(dplyr::across(c(2:45), mean, na.rm = T))

credit_data_36m <- credit_data |>
  dplyr::group_by(nome) |>
  dplyr::slice((dplyr::n() - 11):dplyr::n()) |>
  dplyr::summarise(dplyr::across(c(7:50), mean , na.rm = T)) |>
  dplyr::inner_join(setores)

credit_data_36m_setor <- credit_data_36m |>
  dplyr::group_by(subsetor_bovespa) |>
  dplyr::mutate(subsetor_bovespa = dplyr::if_else(subsetor_bovespa == "-", "Outros", subsetor_bovespa)) |>
  dplyr::summarise(dplyr::across(c(2:45), mean, na.rm = T))

credit_data_60m <- credit_data |>
  dplyr::group_by(nome) |>
  dplyr::slice((dplyr::n() - 19):dplyr::n()) |>
  dplyr::summarise(dplyr::across(c(7:50), mean , na.rm = T)) |>
  dplyr::inner_join(setores)

credit_data_60m_setor <- credit_data_60m |>
  dplyr::group_by(subsetor_bovespa) |>
  dplyr::mutate(subsetor_bovespa = dplyr::if_else(subsetor_bovespa == "-", "Outros", subsetor_bovespa)) |>
  dplyr::summarise(dplyr::across(c(2:45), mean, na.rm = T))

credit_data_list <- list(margem = credit_data, anual = credit_data_12m, bianual = credit_data_24m, trianual = credit_data_36m, pentanual = credit_data_60m)
credit_data_list_setor <- list(margem = credit_data_setor, anual = credit_data_12m_setor, bianual = credit_data_24m_setor, trianual = credit_data_36m_setor, pentanual = credit_data_60m_setor)


saveRDS(credit_data, "Data/credit_data.rds")
saveRDS(credit_data_list, "Data/credit_data_list.rds")
saveRDS(credit_data_list_setor, "Data/credit_data_list_setor.rds")

# Wacc calculations ------------------------------------------------------------

url_beta <- "https://api.data.economatica.com/1/oficial/datafeed/download/1/ZjHJPf%2Fjg1c3PFNFwUfzRpHynmcQCyPO2OaaaHIafW%2Fj8bbCkkY4B4HrhHGFbYwMrEkE%2FOw1lBcfTU%2BzgyyQt1LTj4sAHENBM8nN7sGY0Gj7w8DFZGxZGbfRRa0Vn%2FXkhTA1R5IE41OiEZ0XjmBawv3AZefUIEs9HQD9XDKIPZ2TDFwFrkgXRxPVG6Uqn4qt%2BH3Ipxq4Rl2trhsLsspxVi3eEiSGrJuq0OCqe0W4Ir6qHp%2BOlMGejGWQ7u7kxkrn%2Byb2EzY8FMB15KpGELxNVYf%2BVCrXIuCuLjr2IpgiVAPVm7cNmaJo%2FtELMJmyqaA81bIeOOyh%2BrG%2FAg%2FESfjvAg%3D%3D"

betas <- vroom::vroom(file = url_beta, delim = ",", locale = vroom::locale(encoding = "latin1")) |>
  janitor::clean_names() |>
  dplyr::rename(beta_lev = beta_me_ul_ng_2_anos_em_moeda_orig) |>
  dplyr::rename(div = div_tt_bruta_mais_recente_em_moeda_orig_em_milhares_consolid_sim) |>
  dplyr::rename(pl = patrim_liq_mais_recente_em_moeda_orig_em_milhares_consolid_sim) |>
  dplyr::mutate(div = as.double(div)) |>
  dplyr::mutate(taxes = .34) |>
  dplyr::mutate(endiv = div/pl) |>
  dplyr::mutate(beta_unlev  = beta_lev/(1 + (1 - taxes)*(endiv)))

yc_anbima <- GetTDData::get.yield.curve()

rf_ntnb <- yc_anbima |>
  dplyr::filter(n.biz.days == 2520 & type == "real_return")

inflation <- yc_anbima |>
  dplyr::filter(n.biz.days == 504 & type == "implicit_inflation")

curve_real <- yc_anbima |>
  dplyr::filter(type == "real_return")

curve_nom <- yc_anbima |>
  dplyr::filter(type == "nominal_return")

# PRM

prm <- 6.617  / 100 # from kenneth french's data

# Using five factor model

nefin_factors <- nefin::risk_factors()

rf <- nefin_factors |>
  dplyr::select(date, risk_free) |>
  dplyr::mutate(geomean = prod(1 + risk_free)^(1/dplyr::n()) - 1) |>
  dplyr::mutate(annual = (1 + geomean)^252 - 1)

rm_minus_rf <- nefin_factors |>
  dplyr::select(date, rm_minus_rf) |>
  dplyr::mutate(geomean = prod(1 + rm_minus_rf)^(1/dplyr::n()) - 1) |>
  dplyr::mutate(annual = (1 + geomean)^252 - 1)

smb <- nefin_factors |>
  dplyr::select(date, smb) |>
  dplyr::mutate(geomean = prod(1 + smb)^(1/dplyr::n()) - 1) |>
  dplyr::mutate(annual = (1 + geomean)^252 - 1)

hml <- nefin_factors |>
  dplyr::select(date, hml) |>
  dplyr::mutate(geomean = prod(1 + hml)^(1/dplyr::n()) - 1) |>
  dplyr::mutate(annual = (1 + geomean)^252 - 1)

wml <- nefin_factors |>
  dplyr::select(date, wml) |>
  dplyr::mutate(geomean = prod(1 + wml)^(1/dplyr::n()) - 1) |>
  dplyr::mutate(annual = (1 + geomean)^252 - 1)

iml <- nefin_factors |>
  dplyr::select(date, iml) |>
  dplyr::mutate(geomean = prod(1 + iml)^(1/dplyr::n()) - 1) |>
  dplyr::mutate(annual = (1 + geomean)^252 - 1)


url_stocks <- "https://api.data.economatica.com/1/oficial/datafeed/download/1/fOrcA8M2zpawEP7zcTL%2BDdGXi0jAf01w782gXzSyS6ML8bXcv6yESKggAQx8sizbDQxdfG0W9p2zc0azxU8%2Fql7P7GuGkE3cp27dA2Yhf40IRSax1p9kP3QpxYg0psVeCbGid69Ifdh6gbhnMrzdK1KzFP2LeyJKxetgdY0AWVI5I4V8B8WAcGQ1yIQMb5KeEe5mSHdDdfvxfYJEsc8qHc1O3%2FDHcPrAtF%2Fqqone8CywAskSuUUoWG2fTBqelsdU7ISlNb%2BjwXlY8KCG4SI2TXEROwTUT%2FKAnjvHbTzo6sfcmQnohl0y1kCFwrZxAWnfTW%2Bp3XxrgkxSr%2FlAzFVAuQ%3D%3D"

url_stocks_scr <- "https://api.data.economatica.com/1/oficial/datafeed/download/1/fRsbiptEzPKHPMrvLdAWRHzh9XN%2B%2Ffqq7e8gxbyVOgoCCk94GYzNuEOrh4A%2FmXl5ASxpkKwjJyOMJ6SAdw%2FbBEC8Ds%2FPgBinPGqRl96qE41512gnszmqI4%2B5B9QG4T2F0qqgGLo%2Bh9sMFAK%2FFzoXc6YyQx1ReYkjj%2BZtTUs5Z53JI980DY0%2BNjBCRdHbN24N9IRKjGcaI6UuM%2B8OWlea9RXjkjbfOT6icb44xnJHSQCNQwjX5bo0xX%2BOUbO68Y6mjWvbBN9KX5roZEDgI%2FHxlDyt4KmsYZbdNGOx4vEboiV%2BGlB4yuZ%2FohMgHj0jSeFhRQfysT0Oo7mrnen45u86Fg%3D%3D"

stocks_scr <- vroom::vroom(file = url_stocks_scr, delim = ",", locale = vroom::locale(encoding = "latin1")) |>
  janitor::clean_names()

stocks <- vroom::vroom(file = url_stocks, delim = ",", locale = vroom::locale(encoding = "latin1")) |>
  janitor::clean_names() |>
  dplyr::rename(date = data) |>
  dplyr::rename(price = fechamento_ajust_p_prov_em_moeda_orig) |>
  dplyr::mutate(price = as.double(price)) |>
  dplyr::group_by(ativo) |>
  dplyr::mutate(r = tidyquant::RETURN(price))


factors_stocks <- dplyr::inner_join(stocks, nefin_factors, by = "date") |>
  dplyr::filter(!is.na(r))

run_regression <- function(data) {
  lm_model <- stats::lm(r ~ rm_minus_rf + smb + hml + wml + iml, data = data)
  return(summary(lm_model))  # Return summary of regression
}

run_regression1 <- function(data) {
  lm_model <- stats::lm(r ~ rm_minus_rf, data  =  data)
  return(summary(lm_model))
}

betas5 <- factors_stocks |>
  dplyr::group_by(ativo) |>
  tidyr::nest() |>
  dplyr::mutate(regression = purrr::map(data, run_regression))

betas5_10y <- factors_stocks |>
  dplyr::filter(date >= Sys.Date() - 10*365) |>
  dplyr::group_by(ativo) |>
  tidyr::nest() |>
  dplyr::mutate(regression = purrr::map(data, run_regression))

re_5_factors <- function(data, alpha, names) {

  re <- matrix(NA, nrow = length(data), ncol = 6)

  sig <- matrix(NA, nrow = length(data), ncol = 5)

  for (i in 1:length(data)) {
    sig[i, 1] <- data[[i]]$coefficients[20]
    sig[i, 2] <- data[[i]]$coefficients[21]
    sig[i, 3] <- data[[i]]$coefficients[22]
    sig[i, 4] <- data[[i]]$coefficients[23]
    sig[i, 5] <- data[[i]]$coefficients[24]
  }

  for (i in 1:length(data)) {
    re[i, 1] <- ifelse(sig[i, 1] <= alpha, data[[i]]$coefficients[2]*rm_minus_rf$annual[1], 0)
    re[i, 2] <- ifelse(sig[i, 2] <= alpha, data[[i]]$coefficients[3]*smb$annual[1], 0)
    re[i, 3] <- ifelse(sig[i, 3] <= alpha, data[[i]]$coefficients[4]*hml$annual[1], 0)
    re[i, 4] <- ifelse(sig[i, 4] <= alpha, data[[i]]$coefficients[5]*wml$annual[1], 0)
    re[i, 5] <- ifelse(sig[i, 5] <= alpha, data[[i]]$coefficients[6]*iml$annual[1], 0)
    re[i, 6] <- rf_ntnb$value/100 + re[i, 1] + re[i, 2] + re[i, 3] + re[i, 4] + re[i, 5]
  }

  re_names <- names

  re <- cbind(re_names, re)

  colnames(re) <- c("name", "rm_minus_rf", "smb", "hml", "wml", "iml", "re")

  return(re)

}

names <- matrix(betas5$ativo)
names_10y <- matrix(betas5_10y$ativo)

re_5f <- tibble::as_tibble(re_5_factors(betas5$regression, .05, names)) |>
  dplyr::mutate(dplyr::across(c(2:7), list(as.double))) |>
  dplyr::select(1, c(8:13)) |>
  dplyr::rename(rm_minus_rf = rm_minus_rf_1, smb = smb_1, hml = hml_1, wml = wml_1, iml = iml_1, re = re_1) |>
  dplyr::mutate(prm = rm_minus_rf + smb + hml + wml + iml)

re_5f_10y <- tibble::as_tibble(re_5_factors(betas5_10y$regression, .05, names_10y)) |>
  dplyr::mutate(dplyr::across(c(2:7), list(as.double))) |>
  dplyr::select(1, c(8:13)) |>
  dplyr::rename(rm_minus_rf = rm_minus_rf_1, smb = smb_1, hml = hml_1, wml = wml_1, iml = iml_1, re = re_1) |>
  dplyr::mutate(prm = rm_minus_rf + smb + hml + wml + iml)

# graph prm
re_5f |>
  ggplot2::ggplot(ggplot2::aes(x = prm)) +
  ggplot2::geom_density() +
  ggplot2::geom_vline(ggplot2::aes(xintercept = median(prm, na.rm = T)), color = "red", linetype = "dashed", size = .5) +
  ggplot2::geom_text(ggplot2::aes(x = median(prm, na.rm = T), y = 80, label = scales::number(median(prm, na.rm = T)*100, .0001, suffix = "%"))) +
  ggplot2::labs(title = "Market Risk Premium 5 Factor Model")

# graph re
re_5f |>
  ggplot2::ggplot(ggplot2::aes(x = re)) +
  ggplot2::geom_density() +
  ggplot2::geom_vline(ggplot2::aes(xintercept = median(re, na.rm = T)), color = "red", linetype = "dashed", size = .5) +
  ggplot2::geom_text(ggplot2::aes(x = median(re, na.rm = T), y = 80, label = scales::number(median(re, na.rm = T)*100, .0001, suffix = "%"))) +
  ggplot2::labs(title = "Equity Cost 5 Factor Model")


# Using CAPM
re_capm <- betas |>
  dplyr::mutate(rf = rf_ntnb$value / 100) |>
  dplyr::mutate(prm = prm) |>
  dplyr::mutate(inf = inflation$value / 100) |>
  dplyr::mutate(re = rf + beta_lev*prm)

re_capm |>
  ggplot2::ggplot(ggplot2::aes(x = re)) +
  ggplot2::geom_density() +
  ggplot2::geom_vline(ggplot2::aes(xintercept = median(re, na.rm = T)), color = "red", linetype = "dashed", size = .5) +
  ggplot2::geom_text(ggplot2::aes(x = median(re, na.rm = T), y = 9, label = scales::number(median(re, na.rm = T)*100, .0001, suffix = "%"))) +
  ggplot2::labs(title = "Equity Cost CAPM Model")


# Cost of Debt

url_deb <- "https://api.data.economatica.com/1/oficial/datafeed/download/1/rOr8rIEqy0wSnMlXCrirYd0lyzLn6xxGSXDotzTKMomqnDv6YanJHzBj3lg3xAkDUFxl4LbESTgdx8qIrtWagrYuJw1%2FK27QjmZuivJ2iwfa1hqCW5g5zCGw0va2zlgGmgtcYd7u%2FYeDXSJBZmoxl7nTD5OpoW8qnAnNKhoRfZ5kjTiDnWidjGkOSPNZDvi2lYSwTMEZpj4hKJ5JckF1Qa%2B%2BRSV8dp6ygAlh%2BvZIi9%2Bh0DU4OoU6Dj5AzZUg6Pmqwjd7DFNAxEACZl4zXVtxZKYbn3bIE6zP5W%2FEWJO8d0AGXDv4BPzEtjMTGgfJ6ns0s30e9kaHA%2BlmL18eBQPJ9w%3D%3D"

url_screenning_deb <- "https://api.data.economatica.com/1/oficial/datafeed/download/1/qVe9Vb7LjA3bOOZG7sFJKKxg6peM65ioli4jysdWu9pkiwWDcf%2BLLz0DcC2S8ekyGsvoNs1OiVyTUs7nPyu%2BLaetpVKiSl%2FjZ6ySWK7LmxlmGNPxpUpVcc%2BS8jd%2F0si0tkpEI7B3pJkFcktlDo7oOg%2FgWBzWlDxCf6GdKjFiX2mOGuyaBn7XKflU2wj9QlU7OrSh9%2BYBn2YA%2BjZ2xEjow06QMjIddrjV5T1x2m8bcCNZrZmkN4n7mm7HbUN1Xq87Z38RNrs0hYpwuTAyu1%2Fhn%2BOba4IiZOgkVlAuyXhaEhGr%2BNob9xfQx6RQjkfZSIJPtLqCMumQjMB0tZqySyYzOw%3D%3D"

deb <- vroom::vroom(file = url_deb, delim = ",", locale = vroom::locale(encoding = "latin1")) |>
  janitor::clean_names()

deb_src <- vroom::vroom(file = url_screenning_deb, delim = ",", locale = vroom::locale(encoding = "latin1")) |>
  janitor::clean_names()

convert_rate <- function(type, y, y_nom, implicit) {
  out <- dplyr::case_when(
    type == "IPCA Spread" ~ y,
    type == "DI Spread" ~ ((1 + y/100)*(1 + y_nom/100)/(1 + implicit/100) - 1)*100,
    type == "DI Percentual" ~ ((1 + y_nom/100*y/100)/(1 + implicit/100) - 1)*100,
    TRUE ~ NA_real_
  )
  return(out)
}

deb_data <-  deb_src |>
  dplyr::mutate(taxa_ytm_em_percent = as.double(taxa_ytm_em_percent)) |>
  dplyr::mutate(duration_em_dias_uteis = as.double(duration_em_dias_uteis)) |>
  dplyr::filter(duration_em_dias_uteis >= 504) |>
  dplyr::mutate(y_nom = stats::spline(x = curve_nom$n.biz.days, y = curve_nom$value, xout = duration_em_dias_uteis, method = "natural")$y) |>
  dplyr::mutate(y_real = stats::spline(x = curve_real$n.biz.days, y = curve_real$value, xout = duration_em_dias_uteis, method = "natural")$y) |>
  dplyr::mutate(implicit = ((1 + y_nom/100)/(1 + y_real/100) - 1)*100) |>
  dplyr::mutate(converted_rate = convert_rate(tipo_de_debenture, taxa_ytm_em_percent, y_nom, implicit))

rd_deb <- deb_data |>
  dplyr::select(c(1:3), 7, 8, dplyr::last_col()) |>
  dplyr::group_by(naics) |>
  dplyr::mutate(rd_by_sector = mean(converted_rate))

rd_deb |>
  ggplot2::ggplot(ggplot2::aes(x = rd_by_sector)) +
  ggplot2::geom_density() +
  ggplot2::geom_vline(ggplot2::aes(xintercept = median(rd_by_sector, na.rm = T)), color = "red", linetype = "dashed", size = .5) +
  ggplot2::geom_text(ggplot2::aes(x = median(rd_deb$rd_by_sector, na.rm = T), y = 1.2), label = scales::number(median(rd_deb$rd_by_sector, na.rm = T), .0001, suffix = "%")) +
  ggplot2::labs(title = "Debt Cost (Debêntures)")

rd_data <- credit_data |>
  dplyr::select(c(1:7), exg_at_consolid_sim, ativo_tot_em_moeda_orig_em_milhares_consolid_sim, fin_pag_em_moeda_orig_em_milhares_no_exercicio_consolid_sim) |>
  dplyr::mutate(exig = exg_at_consolid_sim/100*ativo_tot_em_moeda_orig_em_milhares_consolid_sim) |>
  dplyr::mutate(rd_raw = -fin_pag_em_moeda_orig_em_milhares_no_exercicio_consolid_sim/exig)

percentiles <- rd_data |>
  dplyr::group_by(subsetor_bovespa) |>
  dplyr::summarise(q_25 = stats::quantile(rd_raw, .25, na.rm = T), q_75 = stats::quantile(rd_raw, .75, na.rm = T))

rd_data <- rd_data |>
  dplyr::left_join(percentiles, by = "subsetor_bovespa") |>
  dplyr::filter(rd_raw >= q_25 & rd_raw <= q_75) |>
  # dplyr::filter(date >= (Sys.Date() - 1000)) |>
  dplyr::group_by(subsetor_bovespa) |>
  dplyr::mutate(rd = mean(rd_raw, na.rm = T)) |>
  dplyr::mutate(rd_real = (1 + rd)/(1 + inflation$value/100) - 1) |>
  dplyr::group_by(nome) |>
  dplyr::filter(date == max(date, na.rm = T))

# graph
rd_data |>
  ggplot2::ggplot(ggplot2::aes(x = rd_real*100)) +
  ggplot2::geom_density() +
  ggplot2::geom_vline(ggplot2::aes(xintercept = median(rd_real, na.rm = T)*100), color = "red", linetype = "dashed", size = .5) +
  ggplot2::geom_text(ggplot2::aes(x = median(rd_data$rd_real, na.rm = T)*100, y = .15), label = scales::number(median(rd_data$rd_real, na.rm = T)*100, .0001, suffix = "%")) +
  ggplot2::labs(title = "Debt Cost (from fin statement)")

length(unique(rd_data$nome))

rd <- rd_deb |>
  dplyr::group_by(nome, cnpj) |>
  dplyr::summarise(nome = dplyr::first(nome), cnpj = dplyr::first(cnpj), setor = dplyr::first(naics), rd_db = dplyr::first(rd_by_sector)) |>
  dplyr::full_join(rd_data) |>
  dplyr::mutate(rd_fin = rd_real*100) |>
  dplyr::select(c(1:4), subsetor_bovespa ,rd_fin) |>
  dplyr::mutate(rd = dplyr::case_when(
    is.na(rd_db) & is.na(rd_fin) ~ NA,
    is.na(rd_db) ~ rd_fin,
    is.na(rd_fin) ~ rd_db,
    rd_db >= rd_fin ~ rd_db,
    TRUE ~ rd_fin))

# graph
rd |>
  ggplot2::ggplot(ggplot2::aes(x = rd)) +
  ggplot2::geom_density() +
  ggplot2::geom_vline(ggplot2::aes(xintercept = median(rd, na.rm = T)), color = "red", linetype = "dashed", size = .5) +
  ggplot2::geom_text(ggplot2::aes(x = median(rd, na.rm = T), y = .4), label = scales::number(median(rd$rd, na.rm = T), .0001, suffix = "%")) +
  ggplot2::labs(title = "Debt Cost")

# Wacc

wd <- credit_data |>
  dplyr::group_by(cnpj) |>
  dplyr::filter(date == max(date)) |>
  dplyr::select(c(1:8)) |>
  dplyr::rename(wd = est_cap_pl_consolid_sim) |>
  dplyr::mutate(wd = wd/100) |>
  dplyr::mutate(we = 1 - wd) |>
  dplyr::select(cnpj, wd, we)


wacc_5f <- re_5f |>
  dplyr::rename(ativo = name) |>
  dplyr::inner_join(stocks_scr, by = "ativo") |>
  dplyr::select(ativo, nome, codigo, cnpj, c(2:8)) |>
  dplyr::left_join(dplyr::select(rd, c("cnpj", "setor", "subsetor_bovespa", "rd"), by = "cnpj")) |>
  dplyr::select(ativo, nome, codigo, cnpj,setor, subsetor_bovespa, re, rd) |>
  dplyr::filter(!is.na(re) & !is.na(rd)) |>
  dplyr::filter(re > 0 & rd > 0) |>
  dplyr::mutate(rd = rd/100) |>
  dplyr::inner_join(wd, by = "cnpj") |>
  dplyr::mutate(wacc = we*re + wd*rd*(1 - .34))

# graph
wacc_5f |>
  ggplot2::ggplot(ggplot2::aes(x = wacc*100)) +
  ggplot2::geom_density() +
  ggplot2::geom_vline(ggplot2::aes(xintercept = median(wacc, na.rm = T)*100), color = "red", linetype = "dashed", size = .5) +
  ggplot2::geom_text(ggplot2::aes(x = median(wacc_5f$wacc, na.rm = T)*100, y = .22), label = scales::number(median(wacc_5f$wacc, na.rm = T)*100, .0001, suffix = "%")) +
  ggplot2::labs(title = "WACC Real")

saveRDS(wacc_5f, "Data/wacc_5f.rds")

# Merton Model -----------------------------------------------------------------

merton_data <- data2 |>
  dplyr::select(c(1:5), divida_no_curto_prazo_em_milhares, divida_no_longo_prazo_em_milhares, prazo_medio_fornecedores_em_dias, c(58), divida_total_liquida_em_milhares, c(59)) |> # names change every day...
  dplyr::rename(divcp = divida_no_curto_prazo_em_milhares, divlp = divida_no_longo_prazo_em_milhares, pm = prazo_medio_fornecedores_em_dias, v = c(9), k = c(10), vol = c(11)) |>
  dplyr::mutate(dplyr::across(c(6:dplyr::last_col()), as.double)) |>
  dplyr::mutate(t_years = dplyr::if_else(is.na(pm), 5, pm/365 + divlp/divcp)) |> # prazo médio em anos + quantas dívidas de cp (1 ano) tem na dívida de LP.
  dplyr::mutate(t_years = dplyr::if_else(t_years > 20, 20, t_years)) |>
  dplyr::mutate(rf  = stats::spline(x = curve_nom$n.biz.days, y = curve_nom$value, xout = t_years*252, method = "natural")$y) |>
  dplyr::filter(v >= 0 & k >= 0) |>
  dplyr::mutate(vol = dplyr::if_else(is.na(vol), 20, vol)) |>
  dplyr::group_by(nome) |>
  dplyr::mutate(merton_value = mean(purrr::map_dbl(data, ~ RQuantLib::EuropeanOption(type = "call", underlying = v, strike = k, dividendYield = 0, riskFreeRate = rf/100, maturity = t_years, volatility = vol/100)$value)),
                merton_delta = mean(purrr::map_dbl(data, ~ RQuantLib::EuropeanOption(type = "call", underlying = v, strike = k, dividendYield = 0, riskFreeRate = rf/100, maturity = t_years, volatility = vol/100)$delta)),
                merton_gamma = mean(purrr::map_dbl(data, ~ RQuantLib::EuropeanOption(type = "call", underlying = v, strike = k, dividendYield = 0, riskFreeRate = rf/100, maturity = t_years, volatility = vol/100)$gamma)),
                merton_vega = mean(purrr::map_dbl(data, ~ RQuantLib::EuropeanOption(type = "call", underlying = v, strike = k, dividendYield = 0, riskFreeRate = rf/100, maturity = t_years, volatility = vol/100)$vega)),
                merton_theta = mean(purrr::map_dbl(data, ~ RQuantLib::EuropeanOption(type = "call", underlying = v, strike = k, dividendYield = 0, riskFreeRate = rf/100, maturity = t_years, volatility = vol/100)$theta)),
                merton_rho = mean(purrr::map_dbl(data, ~ RQuantLib::EuropeanOption(type = "call", underlying = v, strike = k, dividendYield = 0, riskFreeRate = rf/100, maturity = t_years, volatility = vol/100)$rho))
                )


saveRDS(merton_data, "Data/merton_data.rds")






# Define the number of simulations
num_simulations <- 100  # You can adjust this number as needed

# Initialize lists to store the output values for each variable
merton_values_v <- list()
merton_values_k <- list()
merton_values_rf <- list()
merton_values_t_years <- list()
menton_values_vol <- list()

# Loop through each row in the data frame
for (i in 1:nrow(merton_data)) {
  # Extract observed values for the current row
  obs_v <- merton_data$v[i]
  obs_k <- merton_data$k[i]
  obs_rf <- merton_data$rf[i]
  obs_t_years <- merton_data$t_years[i]
  obs_volatility <- merton_data$vol[i]

  # Define the range for simulated values based on the observed values
  v_min <- min(merton_data$v * 0.5, na.rm = TRUE)
  v_max <- max(merton_data$v * 1.5, na.rm = TRUE)
  k_min <- min(merton_data$k * 0.5, na.rm = TRUE)
  k_max <- max(merton_data$k * 1.5, na.rm = TRUE)
  rf_min <- min(merton_data$rf * 0.5, na.rm = TRUE)
  rf_max <- max(merton_data$rf * 1.5, na.rm = TRUE)
  t_years_min <- min(merton_data$t_years * 0.5, na.rm = TRUE)
  t_years_max <- max(merton_data$t_years * 1.5, na.rm = TRUE)
  vol_min <- min(merton_data$vol * 0.5, na.rm = TRUE)
  vol_max <- max(merton_data$vol * 1.5, na.rm = TRUE)

  # Generate simulated values within the defined range for 'v'
  simulated_v <- seq(v_min, v_max, length.out = num_simulations)
  simulated_k <- seq(k_min, k_max, length.out = num_simulations)
  simulated_rf <- seq(rf_min, rf_max, length.out = num_simulations)
  simulated_t_years <- seq(t_years_min, t_years_max, length.out = num_simulations)
  simulated_vol <- seq(vol_min, vol_max, length.out = num_simulations)

  # Initialize vectors to store merton values for each variable for the current row
  merton_values_v_row <- numeric(length(simulated_v))
  merton_values_k_row <- numeric(length(simulated_v))
  merton_values_rf_row <- numeric(length(simulated_v))
  merton_values_t_years_row <- numeric(length(simulated_v))
  merton_values_vol_row <- numeric(length(simulated_v))

  # Loop through each simulated value of 'v'
  for (j in seq_along(simulated_v)) {
    # Run the EuropeanOption function on the simulated data for 'v'
    merton_v <- RQuantLib::EuropeanOption(
      type = "call",
      underlying = simulated_v[j],
      strike = obs_k,
      dividendYield = 0,
      riskFreeRate = obs_rf,
      maturity = obs_t_years,
      volatility = obs_volatility
    )

    # Store the value output in the vector
    merton_values_v_row[j] <- merton_v$value

    # Store the observed values for other variables in separate vectors
    # k_values <- rep(obs_k, length(simulated_v))
    # rf_values <- rep(obs_rf, length(simulated_v))
    # t_years_values <- rep(obs_t_years, length(simulated_v))
    # vol_values <- rep(obs_v,length(simulated_v))

    # Run the EuropeanOption function on the simulated data for 'k', 'rf', and 't_years'
    merton_k <- RQuantLib::EuropeanOption(
      type = "call",
      underlying = obs_v,
      strike = simulated_k[j],
      dividendYield = 0,
      riskFreeRate = obs_rf,
      maturity = obs_t_years,
      volatility = obs_volatility
    )
    # Store the value output in the vector
    merton_values_k_row[j] <- merton_k$value

    merton_rf <- RQuantLib::EuropeanOption(
      type = "call",
      underlying = obs_v,
      strike = obs_k,
      dividendYield = 0,
      riskFreeRate = simulated_rf[j],
      maturity = obs_t_years,
      volatility = obs_volatility
    )
    # Store the value output in the vector
    merton_values_rf_row[j] <- merton_rf$value

    merton_t_years <- RQuantLib::EuropeanOption(
      type = "call",
      underlying = obs_v,
      strike = obs_k,
      dividendYield = 0,
      riskFreeRate = obs_rf,
      maturity = simulated_t_years[j],
      volatility = obs_volatility
    )
    # Store the value output in the vector
    merton_values_t_years_row[j] <- merton_t_years$value

    merton_vol <- RQuantLib::EuropeanOption(
      type = "call",
      underlying = obs_v,
      strike = obs_k,
      dividendYield = 0,
      riskFreeRate = obs_rf,
      maturity = obs_t_years,
      volatility = simulated_vol[j]
    )
    # Store the value output in the vector
    merton_values_vol_row[j] <- merton_vol$value
  }

  # Store the merton values for 'v', 'k', 'rf', and 't_years' for the current row in the respective lists
  merton_values_v[[i]] <- merton_values_v_row
  merton_values_k[[i]] <- merton_values_k_row
  merton_values_rf[[i]] <- merton_values_rf_row
  merton_values_t_years[[i]] <- merton_values_t_years_row
  menton_values_vol[[i]] <- merton_values_vol_row
}

simulated_data <- list(
 companies = merton_data$nome,
 v = merton_values_v,  # Merton values for 'v'
 k =  merton_values_k,  # Merton values for 'k'
 rf = merton_values_rf, # Merton values for 'rf'
 t = merton_values_t_years,  # Merton values for 't_years'
 vol = menton_values_vol
)

saveRDS(simulated_data, "Data/simulated_merton.rds")
