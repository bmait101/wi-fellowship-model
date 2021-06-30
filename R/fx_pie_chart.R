
# function to build pie charts for cost sharing

## load color pallet
source(here::here("R", "col_palette.R"))


pie_bake_fellows <- function (data) 
  
{
  if (tibble::is_tibble(data)) {
    data <- as.data.frame(data)
  }
  
  data_n <- data %>% select(group, value)
  # names(data) <- c("group", "value")
  data_n <- arrange(data_n, desc(group))
  data_n[, 2] <- round(data_n[, 2]/sum(data_n[, 2]) * 100, 0)
  ggplot(data_n, aes(x = "", y = value, fill = data_n[, 1])) + 
    geom_bar(
      stat = "identity", 
      width = 1, 
      color = "grey60"
      ) +
    coord_polar(
      "y",
      start = 0
      ) + 
    scale_fill_manual(
      values = colz %>% filter(group %in% data$group) %>% pull(cols), 
      name = "**Cost Share**"
      ) +
    geom_text(
      aes(
        y = value/2 + c(0, cumsum(value)[-length(value)]),
        label = (scales::label_percent(accuracy = 1))(value/sum(value))),
      color = "black", 
      fontface = "bold", 
      size = 7.5/.pt
      ) + 
  theme_void(base_family = "sans") +
  theme(
    legend.title = ggtext::element_markdown(size = 10, color = "black", face = "bold"),
    legend.key.height= unit(.25, 'cm'),
    legend.key.width= unit(.25, 'cm'),
    legend.text = element_text(size = 10),
    legend.margin = margin(0,0,0,-10),
    plot.title = ggtext::element_markdown(hjust = 0.5, color = "black", size = 12, face = "bold", 
                                          margin = margin(t = 0, b = 0)),
    plot.margin = margin(0,5,0,0)
  )
      
    }


