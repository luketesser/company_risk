# Financial statemaents Data from Economatica ----------------------------------

url <- "https://api.data.economatica.com/1/oficial/datafeed/download/1/a7VM3DNNG4f9CX6YtOqJMc4rYEuysOYTn1592Ruoh4YKDFpSUAx3uaWQQt6%2FdbGdu79HNTjBqXGy8UaNc7AL%2BPMIxuq9zHbMdd2ANQAP8yrtil%2FM95c%2F3MMpv3GkdRENxqkXnZ16EqI6F2UA8v%2BXkQasM%2B%2FyJ97C2d43zEfxyY1gu622uzuq5XCEPim1oF8rmTVE9ub5yPkCGHSC1fu63Hwbb5diB2rXL%2FFVQfdrWPPl%2FeSTHOlmN7QwBGNICI8HFnbqZtqAnxywKlZGuMV6%2F7sPFqeaDp7hN6deq5WTFLHgzzofNBOURdthU%2FnL0JrVygHlUlYxuhkpBk5V4nRpTg%3D%3D"

url2 <- "https://api.data.economatica.com/1/oficial/datafeed/download/1/gwLQ37fNt88xFQzAEDvgHbNKFajTXJ6GhSVu6IS5osOfqHayUOhOYOZ0aU85iFKOp9siH0C1jkRSZeVsOJdqTnxtQBOlQ7Ri0Y7DHiRgtx7aHXIrpR%2Fl9bVSfkM%2B7beTlP%2BJGDkNd7HYxLwn50RFGDvXHy%2BaCif3E9JB2IKzpZrPtxs%2F4tSpUsj0eW2TeCQ6ATKKIITeB8kuJOQUL%2BTX3nxanvh9p5eB3IZQ1TklXO8gvXSJvhXAqOqXjpofspCCYG19AQc4Lo2a4%2BzNbjHoHKx9WEo5l5mzk%2F40ZzH%2B8R%2BQC6JFTFf8SYSL4njD%2Fbu%2BwfheJc%2FH%2BeBOXtJtwvE9tg%3D%3D"

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

credit_data_12m <- credit_data |>
  dplyr::group_by(nome) |>
  dplyr::slice((dplyr::n() - 3):dplyr::n()) |>
  dplyr::summarise(dplyr::across(c(7:49), mean , na.rm = T))

credit_data_24m <- credit_data |>
  dplyr::group_by(nome) |>
  dplyr::slice((dplyr::n() - 7):dplyr::n()) |>
  dplyr::summarise(dplyr::across(c(7:49), mean , na.rm = T))

credit_data_36m <- credit_data |>
  dplyr::group_by(nome) |>
  dplyr::slice((dplyr::n() - 11):dplyr::n()) |>
  dplyr::summarise(dplyr::across(c(7:49), mean , na.rm = T))

credit_data_60m <- credit_data |>
  dplyr::group_by(nome) |>
  dplyr::slice((dplyr::n() - 19):dplyr::n()) |>
  dplyr::summarise(dplyr::across(c(7:49), mean , na.rm = T))

credit_data_list <- list(margem = credit_data, anual = credit_data_12m, bianual = credit_data_24m, trianual = credit_data_36m, pentanual = credit_data_60m)


saveRDS(credit_data, "Data/credit_data.rds")
saveRDS(credit_data_list, "Data/credit_data_list.rds")

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
  ggplot2::geom_text(ggplot2::aes(x = median(prm, na.rm = T), y = 80, label = scales::number(median(prm, na.rm = T)*100, .0001, suffix = "%")))

# graph re
re_5f |>
  ggplot2::ggplot(ggplot2::aes(x = re)) +
  ggplot2::geom_density() +
  ggplot2::geom_vline(ggplot2::aes(xintercept = median(re, na.rm = T)), color = "red", linetype = "dashed", size = .5) +
  ggplot2::geom_text(ggplot2::aes(x = median(re, na.rm = T), y = 80, label = scales::number(median(re, na.rm = T)*100, .0001, suffix = "%")))


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
  ggplot2::geom_text(ggplot2::aes(x = median(re, na.rm = T), y = 9, label = scales::number(median(re, na.rm = T)*100, .0001, suffix = "%")))


# Debêntures

# url_deb <- "https://api.data.economatica.com/1/oficial/datafeed/download/1/rOr8rIEqy0wSnMlXCrirYd0lyzLn6xxGSXDotzTKMomqnDv6YanJHzBj3lg3xAkDUFxl4LbESTgdx8qIrtWagrYuJw1%2FK27QjmZuivJ2iwfa1hqCW5g5zCGw0va2zlgGmgtcYd7u%2FYeDXSJBZmoxl7nTD5OpoW8qnAnNKhoRfZ5kjTiDnWidjGkOSPNZDvi2lYSwTMEZpj4hKJ5JckF1Qa%2B%2BRSV8dp6ygAlh%2BvZIi9%2Bh0DU4OoU6Dj5AzZUg6Pmqwjd7DFNAxEACZl4zXVtxZKYbn3bIE6zP5W%2FEWJO8d0AGXDv4BPzEtjMTGgfJ6ns0s30e9kaHA%2BlmL18eBQPJ9w%3D%3D"
#
# url_screenning_deb <- "https://api.data.economatica.com/1/oficial/datafeed/download/1/ATYVbVKuQ9is3DHzKvLM2d%2FLr%2Fhw%2FVJqnytrZlTsog0jHv5ar36rZgz7Xpeadg43Oj%2BJvDZa%2FphWcggM%2Bu%2B1iTRTS0RlII5IlULHnT5rrfbZE5uVVvWw38%2B%2BUWwO19nrzcGNhHbInOrc2rf0TqwdgkdtwqPaNSzh%2FXOfmHxQPffLj2ul0M%2FbNcwaA84imXR2QOfxIO3ZtDrdkDyjjEzr9pTa0rXm56vPMugtri196Cv3g%2B2yvhz%2BR7WSUUS23%2BcwN8byMZQ7MGF%2BtNkyWjLl7B5RyRnnFkN1Rajt%2FlzO%2BK1TcUTXTjBTmN6dcLbCofrOHCviZXTsyzvwroV%2BSFxEKg%3D%3D"
#
# deb <- vroom::vroom(file = url_deb, delim = ",", locale = vroom::locale(encoding = "latin1")) |>
#   janitor::clean_names()
#
# deb_src <- vroom::vroom(file = url_screenning_deb, delim = ",", locale = vroom::locale(encoding = "latin1")) |>
#   janitor::clean_names()
#
#   Quem sabe fazer por setor então

