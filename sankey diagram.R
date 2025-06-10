# Mobile App Data Analysis and Visualization
# Author: Owais Raza (took help with Git's copilot)
# Date: June 01 2025
# Listening to: Coke Studio Season 7| Hans Dhuni| Ustad Raees Khan
# Description: Creating sankey diagram

required_packages <- c("dplyr", "tidyr", "stringr", "networkD3", "htmlwidgets", "webshot2")
missing <- setdiff(required_packages, rownames(installed.packages()))
if (length(missing)) install.packages(missing)
lapply(required_packages, function(pkg) suppressPackageStartupMessages(library(pkg, character.only = TRUE)))

df <- readRDS("cleaned_appdata_with_haddon.rds")

create_sankey_diagram <- function(
    data, 
    static_export = TRUE, 
    export_file = "sankey.png"
) {
  layer1 <- c("Navigation", "Education", "Travel", "Lifestyle & Entertainment", "Utilities", "Business", "Other")
  layer2 <- c("pre-event", "event", "post-event")
  layer3 <- c("Human", "Vehicle", "Physical Environment", "Social Environment")
  sankey_data <- data %>%
    filter(!is.na(UnifiedCategory), !is.na(Final_Stage), !is.na(Haddon_top_factor)) %>%
    mutate(
      UnifiedCategory = str_trim(as.character(UnifiedCategory)),
      primary_phase = strsplit(as.character(Final_Stage), " *& *"),
      haddon_factor = strsplit(as.character(Haddon_top_factor), " *& *|;\\s*")
    ) %>%
    unnest(primary_phase) %>%
    unnest(haddon_factor) %>%
    mutate(
      primary_phase = str_trim(tolower(primary_phase)),
      haddon_factor = str_trim(haddon_factor)
    ) %>%
    filter(
      UnifiedCategory %in% layer1,
      primary_phase %in% layer2,
      haddon_factor %in% layer3
    ) %>%
    count(UnifiedCategory, primary_phase, haddon_factor, name="value") %>%
    filter(value > 0)
  nodes <- data.frame(
    name = c(layer1, layer2, layer3),
    group = c(rep("category", length(layer1)),
              rep("phase", length(layer2)),
              rep("factor", length(layer3))),
    color_id = c(
      paste0("category_", gsub(" ", "_", layer1)),
      paste0("phase_", gsub(" ", "_", layer2)),
      paste0("factor_", gsub(" ", "_", layer3))
    ),
    stringsAsFactors = FALSE
  )
  links_cat_phase <- sankey_data %>%
    group_by(UnifiedCategory, primary_phase) %>%
    summarise(value = sum(value), .groups = "drop") %>%
    mutate(
      source = match(UnifiedCategory, nodes$name) - 1,
      target = match(primary_phase, nodes$name) - 1
    )
  links_phase_factor <- sankey_data %>%
    group_by(primary_phase, haddon_factor) %>%
    summarise(value = sum(value), .groups = "drop") %>%
    mutate(
      source = match(primary_phase, nodes$name) - 1,
      target = match(haddon_factor, nodes$name) - 1
    )
  links <- bind_rows(links_cat_phase, links_phase_factor)
  links$link_group <- nodes$color_id[links$target + 1]
  my_color <- 'd3.scaleOrdinal()
.domain([
  "category_Navigation", "category_Education", "category_Travel", "category_Lifestyle_&_Entertainment",
  "category_Utilities", "category_Business", "category_Other",
  "phase_pre-event", "phase_event", "phase_post-event",
  "factor_Human", "factor_Vehicle", "factor_Physical_Environment", "factor_Social_Environment"
])
.range([
  "#C94C4C", "#7D5FB2", "#5A82B2", "#F9A875", "#FFD86B", "#4CB174",   "#B474B2",
  "#FF0066", "#990033", "#1A000A",
  "#E63946", "#3A86FF", "#06D6A0", "#FFA600"])'
  sankey_plot <- networkD3::sankeyNetwork(
    Links = links, Nodes = nodes,
    Source = "source", Target = "target", Value = "value",
    NodeID = "name", NodeGroup = "color_id",
    colourScale = my_color,
    sinksRight = F, fontSize = 18,
    nodeWidth = 45, nodePadding = 20,
    margin = list(top = 50, right = 50, bottom = 50, left = 50)
  )
  if (static_export) {
    temp_html <- tempfile(fileext = ".html")
    htmlwidgets::saveWidget(sankey_plot, file = temp_html, selfcontained = TRUE)
    webshot2::webshot(temp_html, file = export_file, vwidth = 9000, vheight = 9000)
    message("Static Sankey diagram exported to ", export_file)
  } else {
    return(sankey_plot)
  }
}
create_sankey_diagram(df, static_export = TRUE, export_file = "sankey.png")