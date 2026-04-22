add_title_slide <- 
function (slides, title, subtitle = NULL) 
{
    slides <- ph_with(add_slide(slides, layout = "Title: Green", 
        master = "Sage Mint Theme"), title, ph_location_label(ph_label = "Text Placeholder 9"))
    if (!is.null(subtitle)) {
        slides <- ph_with(slides, subtitle, ph_location_label(ph_label = "Subtitle 2"))
    }
}
