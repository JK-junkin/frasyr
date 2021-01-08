#' Convert Japanese number unit
#' @export
keta <- function(num) {
    options(scipen = 100)
    rn <- round(num, digits = 0)
    nc <- nchar(rn)
    rs <- nc %% 4
    u <- dplyr::case_when(nc == 1 ~ "一",
                          nc >= 2 & nc <= 4 ~ "",
                          (nc - 1) %/% 4 == 1 ~ "万",
                          (nc - 1) %/% 4 == 2 ~ "億",
                          (nc - 1) %/% 4 == 3 ~ "兆",
                          (nc - 1) %/% 4 == 4 ~ "京",
                          (nc - 1) %/% 4 == 5 ~ "垓",
                          (nc - 1) %/% 4 == 6 ~ "𥝱",
                          (nc - 1) %/% 4 == 7 ~ "穣",
                          (nc - 1) %/% 4 == 8 ~ "溝",
                          (nc - 1) %/% 4 == 9 ~ "澗",
                          (nc - 1) %/% 4 == 10 ~ "正",
                          (nc - 1) %/% 4 == 11 ~ "載",
                          (nc - 1) %/% 4 == 12 ~ "極",
                          (nc - 1) %/% 4 == 13 ~ "恒河沙",
                          (nc - 1) %/% 4 == 14 ~ "阿僧祇",
                          (nc - 1) %/% 4 == 15 ~ "那由多",
                          (nc - 1) %/% 4 == 16 ~ "不可思議",
                          (nc - 1) %/% 4 == 17 ~ "無量大数",
                          TRUE ~ as.character(nc))
    a <- dplyr::case_when(nc >= 73 ~ "桁数: ",
                          rs == 1 ~ "",
                          rs == 2 ~ "十",
                          rs == 3 ~ "百",
                          rs == 0 ~ "千",
                          TRUE ~ as.character(rs))
    options(scipen = 6)
    paste0(a, u)
}

#' Extract years by keys
#' @export
yrs_used <- function(start, end,
                     regime    = use_regime_model,
                     yr_switch = regime_year_MSY,
                     key       = regime_key_MSY,
                     which_key = future_regime_MSY) {

    whole <- start:end

    if (regime == 1) {
        point <- c(start, yr_switch, end)
        len <- length(point)
        if (len != length(key) + 1) {
            stop("length(regime_key_MSY) must be ", length(yr_switch) + 1, ". Abort.")
        } else {
            tmp <- data.frame(year = whole, keys = NA_real_)
            for(i in seq_along(key)) {
                tmp <- tmp %>%
                    dplyr::mutate(keys = dplyr::if_else(is.na(keys) & 
                                                            year >= point[i] &
                                                            year < point[i + 1],
                                                       key[i], keys))
            }
            tmp %>%
                # 最終年endの処理
                dplyr::mutate(keys = dplyr::if_else(is.na(keys), key[length(key)], keys)) %>%
                dplyr::filter(keys == which_key) %>%
                dplyr::pull(year)
        }
    } else if (regime == 0) {
        whole
    } else {
        stop("use_regime_model must be 0 or 1. Abort.")
    }
}
