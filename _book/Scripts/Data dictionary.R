df_count <- 
function (data) 
{
    map(names(data), ~data %>% pull(all_of(.x)) %>% to_factor() %>% 
        fct_count() %>% mutate(name = .x, .before = 1) %>% rename(levels = f) %>% 
        filter(n != 0) %>% mutate(id = row_number()) %>% filter(id %in% 
        1:10) %>% mutate(Count = paste0(levels, ";  N = ", n)) %>% 
        select(-id, -n, -levels)) %>% bind_rows
}

unique_vals <- 
function (data, max_values = 30) 
{
    vect <- data %>% map(., ~unique(to_factor(.x))) %>% map(., 
        function(xxx) {
            vec_length <- length(xxx)
            if (vec_length > max_values) {
                vec_length <- max_values
            }
            result <- paste0(xxx[1:vec_length], collapse = ", ")
            return(result)
        }) %>% as_tibble %>% pivot_longer(cols = everything())
    count_unique <- data %>% map(., ~length(unique(to_factor(.x)))) %>% 
        as_tibble() %>% pivot_longer(cols = everything()) %>% 
        rename(n_unique = value)
    vect <- full_join(count_unique, vect, by = "name")
    names(vect)[3] <- paste("First", max_values, "unique values")
    return(vect)
}

fullsome <- 
function (data, max_values = 30, count = F, unique_vals = T) 
{
    components <- list(map(data, ~sum(is.na(.x))) %>% as_tibble() %>% 
        pivot_longer(cols = everything()) %>% rename(n_miss = value) %>% 
        mutate(p_miss = paste0(round(n_miss/nrow(data) * 100, 
            2), "%")), map(data, ~paste0(class(.x), collapse = ", ")) %>% 
        as_tibble() %>% pivot_longer(cols = everything()) %>% 
        rename(class = value), map(data, ~var_label(.x)) %>% 
        as_tibble() %>% pivot_longer(cols = everything()) %>% 
        rename(label = value))
    if (unique_vals == T) {
        components <- append(components, list(unique_vals(data, 
            max_values = max_values)))
    }
    results <- reduce(components, full_join, by = "name") %>% 
        relocate(name, label, any_of("n_unique"), n_miss, p_miss, 
            class)
    if (count == T) {
        counts <- data %>% df_count()
        results <- full_join(results, counts, by = "name")
    }
    return(results)
}

overview_hux <- 
function (data, flex = F) 
{
    number_of_cols <- ncol(data)
    ht <- as_hux(data)
    ht <- set_valign(ht, everywhere, 1:number_of_cols, "middle")
    group_ids <- data$name
    change <- group_ids == lag(group_ids)
    change[1] <- TRUE
    change
    group_colours <- factor(c("lightblue", "lightgreen"))
    group_colours
    colours <- vector()
    for (i in 1:length(group_ids)) {
        if (change[i] == FALSE) {
            group_colours <- rev(group_colours)
        }
        colours <- append(colours, as.character(group_colours[1]))
    }
    colours
    ht <- set_background_color(ht, row = 2:nrow(ht), col = 1:ncol(ht), 
        colours)
    ht <- set_background_color(ht, row = 1, col = everywhere, 
        "lightgrey")
    if ("Count" %in% names(data)) {
        r <- rle(group_ids)
        r <- r$lengths
        merge_start <- 2
        for (i in 1:length(r)) {
            ht <- set_rowspan(ht, row = merge_start, col = 1:(number_of_cols - 
                1), value = r[i])
            merge_start <- merge_start + r[i]
        }
    }
    ht <- set_width(set_align(set_all_borders(ht, 1), everywhere, 
        everywhere, "center"), 0.8)
    if (flex == T) {
        return(ht %>% as_flextable())
    }
    else {
        return(ht)
    }
}

