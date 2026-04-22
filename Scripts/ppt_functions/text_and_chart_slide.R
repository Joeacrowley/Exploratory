text_and_chart_slide <- 
function (slides, title, text, chart, source = NULL, footer = NULL) 
{
    slides <- ph_with(ph_with(ph_with(add_slide(slides, layout = "Text and chart with source", 
        master = "Sage Mint Theme"), title, ph_location_label(ph_label = "Title 1")), 
        text, ph_location_type(type = "body", type_idx = 2), 
        ), chart, ph_location_type(type = "chart"))
    if (!is.null(source)) {
        slides <- ph_with(slides, source, ph_location_type(type = "body", 
            type_idx = 3))
    }
    if (!is.null(footer)) {
        slides <- ph_with(slides, footer, ph_location_label(ph_label = "Footer"))
    }
}
