
#Libraries
library(dplyr)
library(ggbump)

.y_spread_ties <- function(data, gap_size=0, upper_clamp = Inf){
  df <- data %>% group_by(y, max_rank) %>%
    mutate(max_rank = pmin(max_rank - gap_size/n() * row_number(), upper_clamp))
  return(df)
}

.x_spread <- function(data, column, spread_amt = 1.25, type = "centered"){
  df <- data %>% slice(rep(1:n(), each = 2))
  if (type == "centered"){
    evens = spread_amt 
    odds = spread_amt * -1
  } else if (type == "forwards"){
    evens = spread_amt * 2
    odds = spread_amt 
  } else if (type == "backwards"){
    evens = spread_amt * -1
    odds = spread_amt * -2
  } else {
    stop(paste(type,"is not a defined spreading type."))
  }
  for (i in seq_len(nrow(df))){
    if (i %% 2 == 0){
      df[[column]][i] = as.numeric(df[[column]][i]) + evens
    } else {
      df[[column]][i] = as.numeric(df[[column]][i]) + odds
    }
  }
  return(df)
}



background_bump <- function(data,
                            x="x",
                            y="y",
                            group="group",
                            breaks = NA,
                            n.breaks = 6,
                            bump.yn = TRUE,
                            bump.pal = NA,
                            bin.pal = NA,
                            trail.yn = FALSE,
                            repel.yn = TRUE,
                            gap.size = 0
){
  data <- data %>%
    rename("x"=paste(x),"y"=paste(y),"group"=paste(group)) %>%
    select(x,y,group)
  x<-"x"
  y<-"y"
  group<-"group"
  
  ranked <- data %>%
    group_by(x) %>%
    mutate(rank = rank(y, ties.method = "random")) %>%
    ungroup() %>%
    mutate(x = as.numeric(x), y = as.numeric(y))
 
  final_rank <- ranked %>%
    group_by(group) %>%
    filter(x==max(x))
  
  if(trail.yn == TRUE){
    ranked <- ranked %>%
      tidyr::complete(x, group, fill=list(y = min(data[y]), rank = 0))
  }
  
  x_unique <- unique(ranked[[x]])
  group_unique <- unique(ranked[[group]])
  
  if (bump.yn==TRUE){
    if (is.na(bump.pal[1])) {
      bump.pal <- scales::seq_gradient_pal("moccasin", "lightsalmon4", "Lab")(seq(0,1,length.out=length(unique(data[[group]]))))
    }
    bump <- ggplot(data = ranked) +
      geom_bump(aes(x=x, y=rank, color = fct_reorder(as.factor(group), rank)), linewidth = 1.5, lineend = "round", show.legend = FALSE) +
      scale_colour_manual(values=bump.pal) +
      scale_y_continuous(breaks = 1:length(unique(data[[group]])), name = "Rank")
  }
  if (is.na(breaks[1])) {
    breaks <- round((max(data[[y]])-min(data[[y]]))/n.breaks*c(0:n.breaks)+min(data[[y]]),2)
  }
  if (is.na(bin.pal[1])) {
    bin.pal <- scales::div_gradient_pal(low = "#f7faff", mid = "#6baed6", high = "#072f6b", "Lab")(seq(0,1,length.out=length(breaks)-1))
  }
  bcc <- bin.pal 
  cc <- bump.pal
  
  
  spreads <- expand.grid(x= x_unique, y= breaks[-1])
  max_rank <- c()
  for(i in 1:nrow(spreads)){
    current_max <- ranked[ranked$x == spreads[[x]][i] & ranked$y <= spreads[[y]][i],]
    if(nrow(current_max) > 0){
      current_max <- current_max %>%
        arrange(desc(rank))
      current_max <- as.numeric(current_max[1,"rank"])
    } else{
      current_max <- 0
    }
    max_rank <- c(max_rank, current_max)
  }
  spreads <- mutate(spreads, max_rank = max_rank)
  
  spread <- spreads %>%
    arrange(x, max_rank, desc = TRUE) %>%
    .y_spread_ties(gap.size, upper_clamp = 20) %>%
    .x_spread(x, spread_amt = min(diff(x_unique))/4)
  
  p <- ggplot(spread, aes()) +
    geom_ribbon(position = 'identity', aes(x=x, ymax=max_rank+0.5, ymin=0, fill=fct_reorder(as.factor(y), y, .desc = TRUE))) +
    scale_fill_manual(values = bcc, name = "y<=") +
    scale_y_continuous(breaks = c(1:max(ranked$rank)), labels = c(max(ranked$rank):1), name = "Rank") +
    scale_x_continuous(breaks = x_unique) +
    theme_bw() +
    theme(legend.key.height= unit(0.75, 'cm'))
  if(bump.yn == TRUE){
    p <- p +
      geom_bump(data = ranked, aes(x=x, y=rank, color = fct_reorder(as.factor(group), rank)), linewidth = 1.5, lineend = "round", show.legend = FALSE) +
      scale_colour_manual(values=cc) +
      coord_cartesian(xlim = c(min(x_unique)-min(diff(x_unique))/4, max(x_unique)+min(diff(x_unique))/4),
                      ylim = c(1, max(ranked[["rank"]])))
    
    if (repel.yn == TRUE)
      p <- p +
        ggrepel::geom_label_repel(data = final_rank, aes(label = group, x=x, y=rank), size = 2, min.segment.length = 0.1, box.padding = 0.2, force = 0.8, alpha = 0.7)
  }
  
  return(p)
  
}



