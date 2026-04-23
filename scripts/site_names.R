melidos_sites <- 
  tibble(
    site = names(melidos_countries),
    site_name =
      paste(melidos_cities, 
            c("(SE)", "(ES)", "(DE)", "(DE)", "(DE)", "(NL)", "(TR)", "(GH)", "(CR)")
      ) |> 
      replace_values("San Pedro, San José (CR)" ~ "San José (CR)")
  )

melidos_order <- c("RISE", "THUAS", "BAUA", "MPI", "TUM", "FUSPCEU", 
                   "IZTECH", "UCR", "KNUST")

site_conversion <- function(x){
  x |> 
    replace_values(
      from = melidos_sites$site,
      to = melidos_sites$site_name
    )
}

site_conv_mutate <- function(data, site = site, rev = TRUE, other.levels = NULL){
  
  if(rev) melidos_order <- rev(melidos_order)
  
  if(!is.null(other.levels)) {
    melidos_order <- c(other.levels, melidos_order)
  }
  
  factor_conv <- function(x) {
    if(!inherits(x, "factor")) {
      x <- fct(x, levels = melidos_order)
    }
    inject(fct_relevel(x, !!!melidos_order)) |> 
      fct_relabel(site_conversion)
  }
  # browser()
  data |> 
    mutate({{ site }} := {{ site }} |> factor_conv())
}

melidos_colors <- 
  melidos_colors |> set_names(melidos_sites$site_name)