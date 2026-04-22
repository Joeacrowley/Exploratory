full_width_content_slide <- 
function (slides, title, text, footer = NULL) 
{
    slides <- ph_with(ph_with(add_slide(slides, layout = "Title and full width content", 
        master = "Sage Mint Theme"), title, ph_location_label(ph_label = "Title 1")), 
        text, ph_location_label(ph_label = "Content Placeholder 2"))
    if (!is.null(footer)) {
        slides <- ph_with(slides, footer, ph_location_label(ph_label = "Footer"))
    }
}
