available_styles <- 
function (slides) 
{
    layout_properties(slides) %>% filter(grepl("mint", name, 
        ignore.case = T) | name %in% c("Title and full width content", 
        "Title and two column content", "Title and full width table with description", 
        "Title and full width chart with description", "Text and chart with source")) %>% 
        select(name, type, type_idx, id, ph_label)
}
