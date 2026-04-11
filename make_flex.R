make_flex <- 
function (df, caption = F, digits = 0) 
{
    table_width <- ncol(df)
    ft <- df %>% flextable::flextable() %>% flextable::set_table_properties(layout = "autofit", 
        width = 1) %>% flextable::theme_zebra(odd_header = "grey90", 
        odd_body = "white", even_body = "#f3f8fc") %>% border(part = "body", 
        border = fp_border(color = "grey80", width = 1)) %>% 
        flextable::hline(i = 1, part = "header", border = fp_border(color = "black", 
            width = 1)) %>% flextable::hline_bottom(part = "body", 
        border = fp_border(color = "black", width = 1)) %>% flextable::colformat_double(digits = digits)
    if (caption != F) {
        ft <- ft %>% flextable::set_caption(caption = caption, 
            autonum = run_autonum(seq_id = "tab"), word_stylename = "Table: Heading row_")
    }
    return(ft)
}
