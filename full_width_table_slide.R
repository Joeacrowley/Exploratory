full_width_table_slide <- 
function (slides, title, text, table, footer = NULL, control_height = T) 
{
    if (control_height == T) {
        table_height_managed <- table %>% fit_to_height(max_height = 3.314961) %>% 
            flextable::fit_to_width(max_width = 30.86, unit = "cm")
    }
    else {
        table_height_managed <- table
    }
    slides <- ph_with(ph_with(ph_with(add_slide(slides, layout = "Title and full width table with description", 
        master = "Office Theme"), title, ph_location_label(ph_label = "Title 1")), 
        text, ph_location_type(type = "body")), table_height_managed, 
        ph_location_type(type = "tbl"))
    if (!is.null(footer)) {
        slides <- ph_with(slides, footer, ph_location_label(ph_label = "Footer Placeholder 4"))
    }
}
