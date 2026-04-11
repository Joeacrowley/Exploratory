full_width_chart_slide <- 
function (slides, title, text = NULL, chart, source = NULL, footer = NULL) 
{
    slides <- ph_with(ph_with(add_slide(slides, layout = "Title and full width chart with description", 
        master = "Office Theme"), title, ph_location_label(ph_label = "Title 1")), 
        chart, ph_location_type(type = "chart"))
    if (!is.null(text)) {
        slides <- ph_with(slides, text, ph_location_type(type = "body", 
            type_idx = 1))
    }
    if (!is.null(source)) {
        slides <- ph_with(slides, source, ph_location_type(type = "body", 
            type_idx = 2))
    }
    if (!is.null(footer)) {
        slides <- ph_with(slides, footer, ph_location_label(ph_label = "Footer Placeholder 4"))
    }
}
