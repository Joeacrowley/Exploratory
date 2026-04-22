fit_to_height <- 
function (ft, max_height, add_header = TRUE) 
{
    dims <- flextable::flextable_dim(ft)
    natural_height <- dims$height
    if (natural_height > max_height) {
        ft <- flextable::padding(ft, padding.top = 1, padding.bottom = 1, 
            part = "all")
        dims <- flextable::flextable_dim(ft)
        natural_height <- dims$height
    }
    if (natural_height > max_height) {
        n_rows <- flextable::nrow_part(ft, part = "body")
        if (add_header) {
            n_rows <- n_rows + flextable::nrow_part(ft, part = "header")
        }
        row_height <- max_height/n_rows
        ft <- flextable::height_all(ft, height = row_height)
    }
    return(ft)
}
