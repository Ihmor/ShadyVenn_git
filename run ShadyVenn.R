rm(Venn4er_newbase)
rm(ShadyVenn)
#source(paste0(getwd(),"/","ShadyVenn.R"))
source(paste0("C:/Users/Phillip Ihmor/polybox/ShadyVenn","/","ShadyVenn.R"))

setwd("C:/Users/Phillip Ihmor/polybox/ShadyVenn")
#example input data
input_lists  <- list("SetA" = 1:180, "SetB" = 157:202, "SetC" = c(77:203,200:202), "SetD" = c(50:100, 10:20, 200:210))  # list of 2 to 4 lists of either strings or integers
color <- "crimson"		                   #base color of the plot
venn_type  <- c("2er","3er","4er")[3]       # If not provided, the skipt will use the maximal number depending on the input lists
hide_values <- FALSE                   #option to hide the concrete numbers within the fields; default is FALSE
file_out <- "4er_test"		             #filename of the to-be-exported plot
fontSize <- 26

#plot and export it
#ShadyVenn(input_lists, file_out)       #using the default settings
ShadyVenn(input_lists, file_out, color, venn_type, hide_values, fontSize )  # with specified settings

#support function: gives you all the colors available
ShadyVenn.get_colors()

#library(ggplot2)
#gg_color_hue(3)

package.skeleton(name = "ShadyVenn", code_files = "ShadyVenn.R")

ShadyVenn.colors()

ShadyVenn.print_colors()