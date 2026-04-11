freq_spss <- 
function (data, var, as_flextable = FALSE) 
{
    if (!is.data.frame(data)) 
        stop("`data` must be a data frame.")
    variable <- data %>% dplyr::pull({
        {
            var
        }
    })
    label <- paste0(data %>% dplyr::select({
        {
            var
        }
    }) %>% names, " - ", labelled::var_label(variable))
    var_all <- variable %>% labelled::to_factor(levels = "prefixed") %>% 
        forcats::fct_drop()
    var_no_NA <- variable %>% labelled::to_factor(levels = "prefixed", 
        user_na_to_na = TRUE) %>% forcats::fct_drop()
    user_na_vals <- attr(variable, "na_values")
    if (is.null(user_na_vals) || length(user_na_vals) == 0) {
        var_only_NA <- factor(character(0))
    }
    else {
        var_only_NA <- variable[variable %in% user_na_vals] %>% 
            labelled::to_factor(levels = "prefixed") %>% forcats::fct_drop()
    }
    valid_tbl <- var_no_NA %>% stats::na.omit() %>% forcats::fct_count(prop = TRUE) %>% 
        dplyr::filter(!is.na(f)) %>% dplyr::mutate(cum_p = paste0(round(cumsum(p) * 
        100, 2), "%"), p = paste0(round(p * 100, 2), "%"), type = "valid")
    user_na_tbl <- var_only_NA %>% forcats::fct_count(prop = FALSE) %>% 
        dplyr::mutate(type = "user_na")
    total_tbl <- var_all %>% forcats::fct_count(prop = TRUE) %>% 
        dplyr::select(-n) %>% dplyr::rename(total_p = p)
    table <- valid_tbl %>% dplyr::full_join(user_na_tbl, by = "f") %>% 
        dplyr::full_join(total_tbl, by = "f") %>% dplyr::mutate(n = dplyr::coalesce(n.x, 
        n.y), type = dplyr::coalesce(type.x, type.y)) %>% dplyr::select(-n.x, 
        -n.y, -type.x, -type.y) %>% dplyr::arrange(desc(type == 
        "valid"), f) %>% dplyr::mutate(cum_p_total = paste0(round(cumsum(total_p) * 
        100, 2), "%"), total_p = paste0(round(total_p * 100, 
        2), "%")) %>% dplyr::select(type, f, n, p, cum_p, total_p, 
        cum_p_total)
    labels_full <- table %>% dplyr::pull(f)
    max_break_appearance <- labels_full %>% as.character() %>% 
        stringr::str_count("]") %>% max
    if (max_break_appearance == 1 && length(labels_full) > 0) {
        splits <- labels_full %>% stringr::str_split("] ", n = 2)
        labels <- purrr::map_chr(splits, ~if (length(.x) >= 2) 
            .x[2]
        else .x[1])
        value_part <- purrr::map_chr(splits, ~.x[1]) %>% stringr::str_remove("\\[")
        match <- purrr::map2_lgl(labels, value_part, ~.x == .y)
        recoding <- labels_full %>% as.character()
        new_names <- dplyr::case_when(match ~ labels, !match ~ 
            as.character(labels_full))
        names(recoding) <- new_names
        recoding <- recoding[!duplicated(names(recoding))]
        table <- table %>% dplyr::mutate(f = forcats::fct_recode(f, 
            !!!recoding))
    }
    names(table)[2] <- label
    if (isTRUE(as_flextable)) {
        table <- .freq_format_table(table)
    }
    return(table)
}

.freq_format_table <- 
function (tbl) 
{
    tbl %>% flextable::flextable() %>% flextable::autofit() %>% 
        flextable::bg(i = ~type == "user_na", j = NULL, bg = "lightgrey") %>% 
        flextable::bg(i = ~type != "user_na", j = NULL, bg = "lightblue") %>% 
        flextable::bg(part = "header", bg = "lightgrey") %>% 
        flextable::delete_columns("type") %>% flextable::set_header_labels(n = "Frequency", 
        p = "Percentage", cum_p = "Cumulative %", total_p = "Total %", 
        cum_p_total = "Total Cumulative %") %>% flextable::align(j = -1, 
        align = "right", part = "all") %>% flextable::vline(j = c(1, 
        2, 4), part = "all")
}

