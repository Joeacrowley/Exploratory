double_text_column_slide <- 
function (slides, title, left_text = NULL, right_text = NULL, 
    footer = NULL) 
{
    slides <- ph_with(add_slide(slides, layout = "Title and two column content", 
        master = "Sage Mint Theme"), title, ph_location_label(ph_label = "Title 1"))
    if (!is.null(left_text)) {
        slides <- ph_with(slides, left_text, ph_location_type(type = "body", 
            type_idx = 3))
    }
    if (!is.null(right_text)) {
        slides <- ph_with(slides, right_text, ph_location_type(type = "body", 
            type_idx = 4))
    }
    if (!is.null(footer)) {
        slides <- ph_with(slides, footer, ph_location_label(ph_label = "Footer"))
    }
}
