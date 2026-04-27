require(legendry)
require(ggtext)
Brown_bracket <- primitive_bracket(
  # Keys determine what is displayed
  key = key_range_manual(start = c(0, 1.0001,250), 
                         end = c(1, 10, Inf), 
                         name = c("sleep", "evening", "daytime")),
  bracket = "square",
  theme = theme(
    legend.text = element_text(angle = 90, hjust = 0.5),
    axis.text.y.left = element_text(angle = 90, hjust = 0.5)
  )
)

Brown_caption <- 
    "<i>daytime</i>, <i>evening</i>, and <i>sleep</i> indicate 
          recommendations for healthy light exposure (Brown et al., 2022)."
