add_divider_slide <- 
function (slides, text, footer = NULL, header = NULL) 
{
    slides <- ph_with(add_slide(slides, layout = "Divider: Mint Green", 
        master = "Office Theme"), text, ph_location_label(ph_label = "Text Placeholder 9"))
    if (!is.null(footer)) {
        slides <- ph_with(slides, footer, ph_location_label(ph_label = "Straight Connector 3"))
    }
    if (!is.null(header)) {
        slides <- ph_with(slides, header, ph_location_label(ph_label = "Subtitle 2"))
    }
}
