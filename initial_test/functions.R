tax_rename <- function(data, tax.class = NULL, tax.empty = "best", tax.level = "Genus"){
  
  tax = data["Taxonomy"]
  tax$Taxonomy <- as.factor(gsub(".__", "",tax$Taxonomy))
  tax <- separate(tax, Taxonomy, c("Domain", "Phylum", "Class", "Order", "Family", "Genus", "Species"), sep = "\\|")
  
  ## First make sure that all entries are strings
  for ( i in 1:ncol(tax) ){
    tax[,i] <- as.character(tax[,i])  
  }
  
  ## Change a specific phylum to class level
  if(!is.null(tax.class)){
    for (i in 1:nrow(tax)){
      if (!is.na(tax$Phylum[i]) & tax$Phylum[i] %in% tax.class){
        tax$Phylum[i] <- tax$Class[i]   
      }
    }
  }
  
  ## Remove uncultured names from the data  
  tax$Domain <- gsub("uncultured", "", tax$Domain)
  tax$Phylum <- gsub("uncultured", "", tax$Phylum)
  tax$Phylum <- gsub("uncultured", "", tax$Phylum)
  tax$Class <- gsub("uncultured", "", tax$Class)
  tax$Order <- gsub("uncultured", "", tax$Order)
  tax$Family <- gsub("uncultured", "", tax$Family)
  tax$Genus <- gsub("uncultured", "", tax$Genus)
  
  ## Check if there is a species level otherwise add it for consistency
  if (!is.null(tax$Species)){
    tax$Species <- gsub("s__", "", tax$Species)
  } else {
    tax$Species <- ""
  }
  
  tax[is.na(tax)] <- ""
  
  ## How to handle empty taxonomic assignments
  if (tax.empty == "OTU"){
    for (i in 1:nrow(tax)) {
      if (tax[i,"Species"] == "") {tax[i,"Species"] <- rownames(tax)[i]}
      if (tax[i,"Genus"] == "") {tax[i,"Genus"] <- rownames(tax)[i]}
      if (tax[i,"Family"] == "") {tax[i,"Family"] <- rownames(tax)[i]}
      if (tax[i,"Order"] == "") {tax[i,"Order"] <- rownames(tax)[i]}
      if (tax[i,"Class"] == "") {tax[i,"Class"] <- rownames(tax)[i]}
      if (tax[i,"Phylum"] == "") {tax[i,"Phylum"] <- rownames(tax)[i]}
    }
  }
  
  ## Handle empty taxonomic strings
  if(tax.empty == "best"){
    tax <- mutate(tax, Domain, Domain = ifelse(Domain == "", "Unclassified", Domain)) %>%
      mutate(Phylum, Phylum = ifelse(Phylum == "", paste("Unclassified"), Phylum)) %>%
      mutate(Class, Class = ifelse(Class == "", ifelse(grepl("__", Phylum), Phylum, paste("Unclassified")), Class)) %>%
      mutate(Order, Order = ifelse(Order == "", ifelse(grepl("__", Class), Class, paste("Unclassified")), Order)) %>%
      mutate(Family, Family = ifelse(Family == "", ifelse(grepl("__", Order), Order, paste("Unclassified")), Family)) %>%
      mutate(Genus, Genus = ifelse(Genus == "", ifelse(grepl("__", Family), Family, paste("Unclassified")), Genus)) %>%
      mutate(Species, Species = ifelse(Species == "", ifelse(grepl("__", Genus), Genus, paste("Unclassified")), Species))
  }
  tax[,c(1:7)] <- as.data.frame(lapply(tax[,c(1:7)], as.factor))
  data <- tax
  #data[["tax"]] <<- tax
  
  return(data)
}

load_data <- function(abund, tax, percent = FALSE){
  
  #abundances to percent, must be done AFTER rarefy
  if(percent == TRUE) {
    abund_pct <- as.data.frame(sapply(abund, function(x) x/sum(x)*100))
    #keep the rownames!
    rownames(abund_pct) <- rownames(abund)
    abund <- abund_pct
  }
  data <- data.frame(tax, abund)#list(abund = abund, tax = tax)
  return(data)
  
}

pore_heatmap <- function(data, group = "Sample", normalise = NULL, scale = NULL, tax.aggregate = "Phylum", tax.add = NULL, tax.show = 10, tax.class = NULL, tax.empty = "best", order.x = NULL, order.y = NULL, plot.numbers = T, plot.breaks = NULL, plot.colorscale = "log10", plot.na = T, scale.seq = 100, output = "plot",plot.text.size = 4, plot.theme = "normal", calc = "mean", min.abundance = 0.1, max.abundance = NULL, sort.by = NULL, color.vector = NULL, round = 1){
  
  ## Extract the data into separate objects for readability
  abund <- data[["abund"]]
  tax <- data[["tax"]]
  sample <- "Sample 1"
  sample <- rep(sample, length(abund$abund)) ### FIX THIS!
  ## Scale the data by a selected metadata sample variable
  if (!is.null(scale)){
    variable <- as.numeric(sample[,scale])
    abund <- t(t(abund)*variable)
  }
  
  ## Make a name variable that can be used instead of tax.aggregate to display multiple levels 
  suppressWarnings(
    if (!is.null(tax.add)){
      if (tax.add != tax.aggregate) {
        tax <- data.frame(tax, Display = apply(tax[,c(tax.add,tax.aggregate)], 1, paste, collapse="; "))
      }
    } else {
      tax <- data.frame(tax, Display = tax[,tax.aggregate])
    }
  )  
  
  # Aggregate to a specific taxonomic level
  abund3 <- cbind.data.frame(Display = tax[,"Display"], abund) %>%
    melt(id.var = "Display", value.name= "Abundance", variable.name = "Sample")
  
  abund3 <- data.table(abund3)[, sum:=sum(Abundance), by=list(Display, Sample)] %>%
    setkey(Display, Sample) %>%
    unique() %>% 
    as.data.frame()
  
  ## Take the average to group level
  #abund6 <- abund3

  abund6 <- data.frame(abund3, Group = abund3$Sample)
  TotalCounts <- group_by(abund6, Display) %>%
    summarise(Abundance = sum(Abundance)) %>%
    arrange(desc(Abundance))
  
  
  ## Subset to X most abundant levels
  if (is.numeric(tax.show)){
    if (tax.show > nrow(TotalCounts)){  
      tax.show <- nrow(TotalCounts)
    }
    abund7 <- filter(abund6, Display %in% TotalCounts$Display[1:tax.show])
  }
  
  ## Subset to a list of level names
  if (!is.numeric(tax.show)){
    if (tax.show != "all"){
      abund7 <- filter(abund6, Display %in% tax.show)    
    }
    ### Or just show all  
    if (tax.show == "all"){
      tax.show <- nrow(TotalCounts)  
      abund7 <- filter(abund6, Display %in% TotalCounts$Display[1:tax.show]) 
    }
  }
  abund7 <- as.data.frame(abund7)
  
  
  ## Order.y
  if (is.null(order.y)){
    abund7$Display <- factor(abund7$Display, levels = rev(TotalCounts$Display))
  }
  if (!is.null(order.y)){
    if ((length(order.y) == 1) && (order.y != "cluster")){       
      temp1 <- filter(abund7, Group == order.y) %>%
        group_by(Display) %>%
        summarise(Mean = mean(Abundance)) %>%
        arrange(desc(Mean))
      
      abund7$Display <- factor(abund7$Display, levels = rev(temp1$Display))
    }
    if (length(order.y) > 1){
      abund7$Display <- factor(abund7$Display, levels = order.y)
    }
    if ((length(order.y) == 1) && (order.y == "cluster")){
      if (is.null(max.abundance)){max.abundance <- max(abund7$Abundance)}
      tdata <- mutate(abund7, 
                      Abundance = ifelse(Abundance < min.abundance, min.abundance, Abundance),
                      Abundance = ifelse(Abundance > max.abundance, max.abundance, Abundance))
      tdata <- dcast(tdata, Display~Group, value.var = "Abundance")
      rownames(tdata) <- tdata$Display
      tdata2 <- tdata[,-1]
      tclust <- hclust(dist(tdata2))
      tnames <- levels(droplevels(tdata$Display))[tclust$order]
      abund7$Display <- factor(abund7$Display, levels = tnames)
    }
  }
  
  ## Order.x
  if (!is.null(order.x)){
    if ((length(order.x) == 1) && (order.x != "cluster")){
      temp1 <- filter(abund7, Display == order.x) %>%
        group_by(Group) %>%
        summarise(Mean = mean(Abundance)) %>%
        arrange(desc(Mean))
      abund7$Group <- factor(abund7$Group, levels = as.character(temp1$Group))
    }    
    if (length(order.x) > 1){
      abund7$Group <- factor(abund7$Group, levels = order.x)
    }
    if ((length(order.x) == 1) && (order.x == "cluster")){
      if (is.null(max.abundance)){max.abundance <- max(abund7$Abundance)}
      tdata <- mutate(abund7, 
                      Abundance = ifelse(Abundance < min.abundance, min.abundance, Abundance),
                      Abundance = ifelse(Abundance > max.abundance, max.abundance, Abundance))
      tdata <- dcast(tdata, Display~Group, value.var = "Abundance")
      rownames(tdata) <- tdata$Display
      tdata2 <- tdata[,-1]
      tclust <- hclust(dist(t(tdata2)))
      tnames <- tclust$labels[tclust$order]
      abund7$Group <- factor(abund7$Group, levels = tnames) 
    }
  }
  
  ## Handle NA values
  if(plot.na == F){ plot.na <- "grey50" }else{ if(!is.null(color.vector)) {plot.na <-color.vector[1]} else {plot.na <-"#67A9CF"}}  
  
  ## Scale to percentages if not normalised and scaled
  
  if (is.null(scale) & is.null(normalise)){
    abund7[,3] <- abund7[,3]/scale.seq*100
  }
  
  if (length(group) > 1 ){ abund7 <- merge(abund7, oldGroup)}
  
  if (is.null(min.abundance)){
    min.abundance <- ifelse(min(abund7$Abundance) > 0.001, min(abund7$Abundance), 0.001)
  }
  if (is.null(max.abundance)){
    max.abundance <- max(abund7$Abundance)
  }
  
  ## Make a heatmap style plot
  p <- ggplot(abund7, aes_string(x = "Group", y = "Display", label = formatC("Abundance", format = "f", digits = 1))) +     
    geom_tile(aes(fill = Abundance), colour = "white", size = 0.5) +
    theme(axis.text.x = element_text(size = 10, hjust = 1, angle = 90)) + 
    theme(axis.text.y = element_text(size = 12)) + 
    theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))
  
  ## Get colorpalette for colorscale or set default
  if (!is.null(color.vector)){
    color.pal = color.vector
  } else {
    color.pal = rev(brewer.pal(3, "RdBu"))
  }
  
  if (plot.numbers == T){
    abund8 <- abund7
    abund8$Abundance <- round(abund8$Abundance, round)
    p <- p + geom_text(data = abund8, size = plot.text.size, colour = "grey10", check_overlap = TRUE)
  }
  if (is.null(plot.breaks)){
    p <- p +scale_fill_gradientn(colours = color.pal, trans = plot.colorscale, na.value=plot.na, oob = squish, limits = c(min.abundance, max.abundance))
  }
  if (!is.null(plot.breaks)){
    p <- p +scale_fill_gradientn(colours = color.pal, trans = plot.colorscale, breaks=plot.breaks, na.value=plot.na , oob = squish, limits = c(min.abundance, max.abundance))
  }
  
  
  if (is.null(normalise)){
    p <- p + labs(x = "", y = "", fill = "% Read\nAbundance")  
  }
  if (!is.null(normalise)){
    p <- p + labs(x = "", y = "", fill = "Relative")  
  }
  
  if(plot.theme == "clean"){
    p <- p + theme(legend.position = "none",
                   axis.text.y = element_text(size = 8, color = "black"),
                   axis.text.x = element_text(size = 8, color = "black", vjust = 0.5),
                   axis.title = element_blank(),
                   text = element_text(size = 8, color = "black"),
                   axis.ticks.length = unit(1, "mm"),
                   plot.margin = unit(c(0,0,0,0), "mm"),
                   title = element_text(size = 8)
    )
  }
  
  ## Define the output 
  if (output == "complete"){
    outlist <- list(heatmap = p, data = abund7)
    return(outlist)  
  }
  if (output == "plot"){
    return(p)
  }
}
