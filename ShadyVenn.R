# Copyright by Phillip Ihmor
# 01.01.2015
# p.ihmor@gmail.com
################


# This script's main function is ShadyVenn(), which takes a basic svg file and adapts it according to the input lists using heavily gsub commands
ShadyVenn <- function(input, file_out, color = "red", type = "default", hide_values = FALSE, fontSize = "default", hide_stroke = FALSE){
	#test input
	if (class(input)  != "list")  {cat("Error: Input must be a list! ");return()}
	if ((length(input) > 4) & (length(input) < 2)) {cat("Error: Input must contain 2 to 4 sets! ");return()}
	
	
	#formate input
	set_names <- gsub("[[:space:]]", " ", names(input))  #get names without any whitespace
	input_lists <- list( "A" =	unlist_F(input[1]), "B" =	unlist_F(input[2]),"C" =	unlist_F(input[3]),"D" =	unlist_F(input[4]))
	if (type == "default") { type <- paste0(length(input),"er") }  #plot Venns with as many factors as possible
	#if (substring(file_out,2,2) != ":") {wdir <- getwd()}
		
	# calculate overlap of the lists
	sets <- data.frame(
		"A"  =  length(setdiff(input_lists$A, union(union(input_lists$B,input_lists$C),input_lists$D))),
		"B"  =  length(setdiff(input_lists$B, union(union(input_lists$A,input_lists$C),input_lists$D))),
		"C"  =  length(setdiff(input_lists$C, union(union(input_lists$B,input_lists$A),input_lists$D))),
		"D"  =  length(setdiff(input_lists$D, union(union(input_lists$B,input_lists$C),input_lists$A))),
		"AB" =	length(intersect(input_lists$A, input_lists$B)),
		"AC" =	length(intersect(input_lists$A, input_lists$C)),
		"AD" =	length(intersect(input_lists$A, input_lists$D)),
		"BC" =	length(intersect(input_lists$B, input_lists$C)),
		"BD" =	length(intersect(input_lists$B, input_lists$D)),
		"CD" =	length(intersect(input_lists$C, input_lists$D)),
		"ABC" =	length(intersect(intersect(input_lists$A, input_lists$B), input_lists$C )),
		"ABD" =	length(intersect(intersect(input_lists$A, input_lists$B), input_lists$D )),
		"ACD" =	length(intersect(intersect(input_lists$A, input_lists$C), input_lists$D )),
		"BCD" =	length(intersect(intersect(input_lists$B, input_lists$C), input_lists$D )),
		"ABCD" =length(intersect(intersect(input_lists$A, input_lists$B),intersect(input_lists$C, input_lists$D)))
	)
	sets <- rbind(sets, "ratio" = sets/max(sets))
	row.names(sets) <- c("counts", "ratios")
	#print(sets)
	
	#change base_svg to the values supplied in the input data
	if (type == "4er") {
		svg_text <- Venn4er_base()
	} else if (type == "3er") {
		svg_text <- Venn3er_base()
	} else if (type == "2er") {
		svg_text <- Venn2er_base()
	} else {print("Error: could not identify VennType")}
	
	#change color
	svg_text <- gsub("fill:#ff0000",     paste0("fill:",ShadyVenn.colors()[[color]]), svg_text)
	
	#change set names
	svg_text <- gsub("name_A<",     paste0(set_names[1],"<")  , svg_text)
	svg_text <- gsub("name_B<",     paste0(set_names[2],"<")  , svg_text)
	svg_text <- gsub("name_C<",     paste0(set_names[3],"<")  , svg_text)
	svg_text <- gsub("name_D<",     paste0(set_names[4],"<")  , svg_text)
	
	#change font of set names
	if (fontSize == "default") {
		maxFont <- 44
		baseFont <- 12
		nchars <- 2*sapply(set_names,nchar)
		nchars[nchars>maxFont]  <- maxFont
		fontSize <- min(baseFont + (maxFont- nchars))
	}
	#print(fontSize)
	
	svg_text <- gsub("font-size:fontSize_[ABCD]px",     paste0("font-size:",fontSize,"px")  , svg_text)
	#if individual font sizes should be used
	#svg_text <- gsub("font-size:fontSize_Apx",     paste0("font-size:",fontSize_sets[1],"px")  , svg_text)
	#svg_text <- gsub("font-size:fontSize_Bpx",     paste0("font-size:",fontSize_sets[2],"px")  , svg_text)
	#svg_text <- gsub("font-size:fontSize_Cpx",     paste0("font-size:",fontSize_sets[3],"px")  , svg_text)
	#svg_text <- gsub("font-size:fontSize_Dpx",     paste0("font-size:",fontSize_sets[4],"px")  , svg_text)
	
	#change opacity of the fields
	if 	(	length(regmatches(svg_text,regexpr("opacity:opacity_A;",svg_text)))) {
		svg_text <- gsub("opacity:opacity_A;",      paste0("opacity:", sets[2,]$A,";")  , svg_text)
		svg_text <- gsub("opacity:opacity_B;",      paste0("opacity:", sets[2,]$B,";")  , svg_text)
		svg_text <- gsub("opacity:opacity_C;",      paste0("opacity:", sets[2,]$C,";")  , svg_text)
		svg_text <- gsub("opacity:opacity_D;",      paste0("opacity:", sets[2,]$D,";")  , svg_text)
		svg_text <- gsub("opacity:opacity_AB;",     paste0("opacity:", sets[2,]$AB,";") , svg_text)
		svg_text <- gsub("opacity:opacity_AC;",     paste0("opacity:", sets[2,]$AC,";") , svg_text)
		svg_text <- gsub("opacity:opacity_BC;",     paste0("opacity:", sets[2,]$BC,";") , svg_text)
		svg_text <- gsub("opacity:opacity_AD;",     paste0("opacity:", sets[2,]$AD,";")  , svg_text)
		svg_text <- gsub("opacity:opacity_BD;",     paste0("opacity:", sets[2,]$BD,";")  , svg_text)
		svg_text <- gsub("opacity:opacity_CD;",     paste0("opacity:", sets[2,]$CD,";")  , svg_text)
		svg_text <- gsub("opacity:opacity_ABC;",    paste0("opacity:", sets[2,]$ABC,";"), svg_text)
		svg_text <- gsub("opacity:opacity_ABD;",    paste0("opacity:", sets[2,]$ABD,";"), svg_text)
		svg_text <- gsub("opacity:opacity_ACD;",    paste0("opacity:", sets[2,]$ACD,";"), svg_text)
		svg_text <- gsub("opacity:opacity_BCD;",    paste0("opacity:", sets[2,]$BCD,";"), svg_text)
		svg_text <- gsub("opacity:opacity_ABCD;",   paste0("opacity:", sets[2,]$ABCD,";"), svg_text)	
	} else if (	length(regmatches(svg_text,regexpr("opacity:opacity_A\"",svg_text)))){
		svg_text <- gsub("opacity:opacity_A\"",     paste0("opacity:", sets[2,]$A,"\"")  , svg_text)
		svg_text <- gsub("opacity:opacity_B\"",     paste0("opacity:", sets[2,]$B,"\"")  , svg_text)
		svg_text <- gsub("opacity:opacity_C\"",     paste0("opacity:", sets[2,]$C,"\"")  , svg_text)
		svg_text <- gsub("opacity:opacity_D\"",     paste0("opacity:", sets[2,]$D,"\"")  , svg_text)
		svg_text <- gsub("opacity:opacity_AB\"",    paste0("opacity:", sets[2,]$AB,"\"") , svg_text)
		svg_text <- gsub("opacity:opacity_AC\"",    paste0("opacity:", sets[2,]$AC,"\"") , svg_text)
		svg_text <- gsub("opacity:opacity_BC\"",    paste0("opacity:", sets[2,]$BC,"\"") , svg_text)
		svg_text <- gsub("opacity:opacity_AD\"",    paste0("opacity:", sets[2,]$AD,"\"")  , svg_text)
		svg_text <- gsub("opacity:opacity_BD\"",    paste0("opacity:", sets[2,]$BD,"\"")  , svg_text)
		svg_text <- gsub("opacity:opacity_CD\"",    paste0("opacity:", sets[2,]$CD,"\"")  , svg_text)
		svg_text <- gsub("opacity:opacity_ABC\"",   paste0("opacity:", sets[2,]$ABC,"\""), svg_text)
		svg_text <- gsub("opacity:opacity_ABD\"",   paste0("opacity:", sets[2,]$ABD,"\""), svg_text)
		svg_text <- gsub("opacity:opacity_ACD\"",   paste0("opacity:", sets[2,]$ACD,"\""), svg_text)
		svg_text <- gsub("opacity:opacity_BCD\"",   paste0("opacity:", sets[2,]$BCD,"\""), svg_text)
		svg_text <- gsub("opacity:opacity_ABCD\"",  paste0("opacity:", sets[2,]$ABCD,"\""), svg_text)
	}
	else{
		print("Error: cant identify opacity tags in svg_base template")
	}
	
	
	#change values in the subsets
	if 	(!hide_values) {
		svg_text <- gsub("value_A<",     paste0(sets[1,]$A,"<")  , svg_text)
		svg_text <- gsub("value_B<",     paste0(sets[1,]$B,"<")  , svg_text)
		svg_text <- gsub("value_C<",     paste0(sets[1,]$C,"<")  , svg_text)
		svg_text <- gsub("value_D<",     paste0(sets[1,]$D,"<")  , svg_text)
		svg_text <- gsub("value_AB<",    paste0(sets[1,]$AB,"<") , svg_text)
		svg_text <- gsub("value_AC<",    paste0(sets[1,]$AC,"<") , svg_text)
		svg_text <- gsub("value_BC<",    paste0(sets[1,]$BC,"<") , svg_text)
		svg_text <- gsub("value_AD<",    paste0(sets[1,]$AD,"<") , svg_text)
		svg_text <- gsub("value_BD<",    paste0(sets[1,]$BD,"<") , svg_text)
		svg_text <- gsub("value_CD<",    paste0(sets[1,]$CD,"<") , svg_text)
		svg_text <- gsub("value_ABC<",   paste0(sets[1,]$ABC,"<"), svg_text)
		svg_text <- gsub("value_ACD<",   paste0(sets[1,]$ACD,"<"), svg_text)
		svg_text <- gsub("value_ABD<",   paste0(sets[1,]$ABD,"<"), svg_text)
		svg_text <- gsub("value_BCD<",   paste0(sets[1,]$BCD,"<"), svg_text)
		svg_text <- gsub("value_ABCD<",  paste0(sets[1,]$ABCD,"<"), svg_text)
	} else {
		svg_text <- gsub("value_[ABCD]*<",     paste0("<")  ,  svg_text)
	}
	
	# set stroke widths to 0 if hide_stroke attribute is true
	if 	(hide_stroke) {
		svg_text <- gsub("stroke:#000000;stroke-width:2",     "stroke:#ffffff;stroke-width:2"  , svg_text)
	}
	
	
	#save updated svg
	file_path <- paste0(getwd(),"/",file_out, ".svg")
	write(svg_text, file = file_path)
	print(round(sets,3))
	cat("Saved ", type, " venn, colored in ", color, ", to ",file_path,"\n", sep = "")
}


##### quick unlist function, without passing on the names
unlist_F <- function(x){
	unlist(x, use.names = FALSE)
}

#### print available colors
ShadyVenn.print_colors <- function(){
	cat("List of available colors: ")		
	cat(names(ShadyVenn.colors()), sep = "\t-\t")
}

### return all avaliable colors
ShadyVenn.colors <- function(){
	return (list("neonyellow" = "#ccff00", "red" = "#ff0000", "purple" = "#6500ff", "blue" = "#000080", "lightblue" = "#007cd2", "grassy-green" = "#7cd200", "crimson" = "#ba1a1a", "lilac" = "#d5bee0", "babyblue" = "#aed5fc", "grey" = "#808080", "green" = "#008000", "teal" = "#00ac93","black" = "#000000", "brown" = "#561a05", "aquamarine" = "#7FFFD4", "azure" = "#007FFF", "greenyellow" = "#ADFF2F", "lemon" = "#FFF700", "mint" = "#98FF98", "neongreen" = "#39FF14", "orange" = "#FF6700", "yellow" = "#FFFF00"))
}



#### get non-modified SVG template.
Venn3er_base <- function() {
	return( 
		"<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>
		<!-- Created with Inkscape (http://www.inkscape.org/) -->
		
		<svg
		xmlns:dc=\"http://purl.org/dc/elements/1.1/\"
		xmlns:cc=\"http://creativecommons.org/ns#\"
		xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"
		xmlns:svg=\"http://www.w3.org/2000/svg\"
		xmlns=\"http://www.w3.org/2000/svg\"
		xmlns:sodipodi=\"http://sodipodi.sourceforge.net/DTD/sodipodi-0.dtd\"
		xmlns:inkscape=\"http://www.inkscape.org/namespaces/inkscape\"
		width=\"750\"
		height=\"700\"
		id=\"svg3121\"
		version=\"1.1\"
		inkscape:version=\"0.48.4 r9939\"
		sodipodi:docname=\"3erVenn_base.svg\">
		<defs
		id=\"defs3123\" />
		<sodipodi:namedview
		id=\"base\"
		pagecolor=\"#ffffff\"
		bordercolor=\"#666666\"
		borderopacity=\"1.0\"
		inkscape:pageopacity=\"0.0\"
		inkscape:pageshadow=\"2\"
		inkscape:zoom=\"1.2\"
		inkscape:cx=\"350\"
		inkscape:cy=\"200\"
		inkscape:document-units=\"px\"
		inkscape:current-layer=\"layer1\"
		showgrid=\"false\"
		inkscape:window-width=\"1527\"
		inkscape:window-height=\"877\"
		inkscape:window-x=\"65\"
		inkscape:window-y=\"-8\"
		inkscape:window-maximized=\"0\" />
		<metadata
		id=\"metadata3126\">
		<rdf:RDF>
		<cc:Work
		rdf:about=\"\">
		<dc:format>image/svg+xml</dc:format>
		<dc:type
		rdf:resource=\"http://purl.org/dc/dcmitype/StillImage\" />
		<dc:title />
		</cc:Work>
		</rdf:RDF>
		</metadata>
		<g
		inkscape:label=\"Layer 1\"
		inkscape:groupmode=\"layer\"
		id=\"layer1\">
		<path
		style=\"fill:#ff0000;fill-opacity:opacity_AB;stroke:#000000;stroke-width:1.5;stroke-miterlimit:4;stroke-opacity:1;stroke-dasharray:none\"
		d=\"m 366.15179,131.24554 c -44.29534,25.77671 -74.09375,73.74486 -74.09375,128.6875 0.0522,11.8207 1.51953,17.35198 3.03125,24.46875 22.90556,-14.39159 50.01425,-22.71875 79.0625,-22.71875 23.36675,0 45.44801,5.43288 65.125,15.03125 13.75057,-57.1194 -33.36681,-121.27664 -73.125,-145.46875 z\"
		id=\"AB\"
		inkscape:connector-curvature=\"0\"
		sodipodi:nodetypes=\"cccscc\" />
		<path
		style=\"fill:#ff0000;fill-opacity:opacity_C;stroke:#000000;stroke-width:1.5;stroke-miterlimit:4;stroke-opacity:1;stroke-dasharray:none\"
		d=\"m 520.96363,386.24536 c -22.90732,14.39426 -50.01205,22.71875 -79.0625,22.71875 -27.22984,0 -52.76258,-7.32356 -74.71875,-20.09375 -21.96034,12.77687 -47.51249,20.09375 -74.75,20.09375 -23.73415,0 -46.15698,-5.58528 -66.0625,-15.46875 -0.65171,5.65255 -1,11.39157 -1,17.21875 0,82.18463 66.62787,148.81241 148.8125,148.81241 82.18463,0 148.78125,-66.62778 148.78125,-148.81241 0,-8.33542 -0.68311,-16.50667 -2,-24.46875 z\"
		id=\"C\"
		inkscape:connector-curvature=\"0\" />
		<path
		style=\"fill:#ff0000;fill-opacity:opacity_BC;stroke:#000000;stroke-width:1.5;stroke-miterlimit:4;stroke-opacity:1;stroke-dasharray:none\"
		d=\"m 439.72768,276.41965 c -5.48816,47.65558 -31.34264,88.81245 -70.95089,111.85714 21.96063,12.78038 47.47312,20.09375 74.71875,20.09375 29.05636,0 56.18413,-8.31975 79.09375,-22.71875 -5.83735,-52.9511 -71.3752,-107.73705 -82.86161,-109.23214 z\"
		id=\"BC\"
		inkscape:connector-curvature=\"0\"
		sodipodi:nodetypes=\"ccscc\" />
		<path
		style=\"fill:#ff0000;fill-opacity:opacity_AC;stroke:#000000;stroke-width:1.5;stroke-miterlimit:4;stroke-opacity:1;stroke-dasharray:none\"
		d=\"m 294.08929,284.43304 c -37.31404,23.44083 -63.49553,62.94602 -68.78125,108.84375 19.91804,9.8946 42.37599,15.46875 66.125,15.46875 27.24039,0 52.75899,-7.31237 74.71875,-20.09375 -37.57597,-21.86227 -64.69144,-59.7027 -72.0625,-104.21875 z\"
		id=\"AC\"
		inkscape:connector-curvature=\"0\"
		sodipodi:nodetypes=\"ccscc\" />
		<path
		style=\"fill:#ff0000;fill-opacity:opacity_A;stroke:#000000;stroke-width:1.5;stroke-miterlimit:4;stroke-opacity:1;stroke-dasharray:none\"
		d=\"m 290.37917,109.76823 c -82.18464,0 -148.8125,66.62787 -148.8125,148.8125 0,58.44053 33.70154,108.99876 82.71874,133.34375 5.28613,-45.8961 35.01475,-84.00446 68.75001,-108.875 -1.31813,-7.96302 -2.50783,-16.14605 -2.03125,-24.46875 3.03046,-52.92233 29.79841,-102.91079 74.09375,-128.6875 -21.96278,-12.78076 -47.47676,-20.125 -74.71875,-20.125 z\"
		id=\"A\"
		inkscape:connector-curvature=\"0\"
		sodipodi:nodetypes=\"ssccscs\" />
		<path
		style=\"fill:#ff0000;fill-opacity:opacity_B;stroke:#000000;stroke-width:1.5;stroke-miterlimit:4;stroke-opacity:1;stroke-dasharray:none\"
		d=\"m 442.83716,112.48669 c -27.22785,0 -52.73181,7.32927 -74.6875,20.09375 44.28257,25.77996 74.0625,73.75443 74.0625,128.6875 0,5.82084 -0.34972,11.57217 -1,17.21875 42.01063,20.87614 72.76861,61.02033 80.6875,108.875 41.897,-26.32634 69.75,-72.95958 69.75,-126.09375 0,-82.18463 -66.62787,-148.78125 -148.8125,-148.78125 z\"
		id=\"B\"
		inkscape:connector-curvature=\"0\" />
		<path
		style=\"fill:#ff0000;fill-opacity:opacity_ABC;stroke:#000000;stroke-width:1.5;stroke-miterlimit:4;stroke-opacity:1;stroke-dasharray:none\"
		d=\"m 373.62724,262.44925 c -29.04825,0 -56.15694,8.32716 -79.0625,22.71875 7.36375,44.52922 34.47798,82.38275 72.0625,104.25 39.60825,-23.04469 67.60559,-63.84442 73.09375,-111.5 -19.91497,-9.89077 -42.34965,-15.46875 -66.09375,-15.46875 z\"
		id=\"ABC\"
		inkscape:connector-curvature=\"0\" />
		<text
		xml:space=\"preserve\"
		style=\"font-size:24px;font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;line-height:125%;letter-spacing:0px;word-spacing:0px;fill:#000000;fill-opacity:1;stroke:none;font-family:Arial;-inkscape-font-specification:Arial;text-anchor:middle;text-align:center\"
		x=\"181.1945\"
		y=\"241.8965\"
		id=\"textA\"
		sodipodi:linespacing=\"125%\"><tspan
		sodipodi:role=\"line\"
		id=\"tspan3048\"
		x=\"210\"
		y=\"240\">value_A</tspan></text>
		<text
		xml:space=\"preserve\"
		style=\"font-size:24px;font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;line-height:125%;letter-spacing:0px;word-spacing:0px;fill:#000000;fill-opacity:1;stroke:none;font-family:Arial;-inkscape-font-specification:Arial;text-anchor:middle;text-align:center\"
		x=\"474.05173\"
		y=\"236.18221\"
		id=\"textB\"
		sodipodi:linespacing=\"125%\"><tspan
		sodipodi:role=\"line\"
		id=\"tspan3052\"
		x=\"515\"
		y=\"240\">value_B</tspan></text>
		<text
		xml:space=\"preserve\"
		style=\"font-size:24px;font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;line-height:125%;letter-spacing:0px;word-spacing:0px;fill:#000000;fill-opacity:1;stroke:none;font-family:Arial;-inkscape-font-specification:Arial;text-anchor:middle;text-align:center\"
		x=\"336.70987\"
		y=\"209.18224\"
		id=\"textAB\"
		sodipodi:linespacing=\"125%\"><tspan
		sodipodi:role=\"line\"
		id=\"tspan3056\"
		x=\"365\"
		y=\"205\">value_AB</tspan></text>
		<text
		xml:space=\"preserve\"
		style=\"font-size:24px;font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;line-height:125%;letter-spacing:0px;word-spacing:0px;fill:#000000;fill-opacity:1;stroke:none;font-family:Arial;-inkscape-font-specification:Arial;text-anchor:middle;text-align:center\"
		x=\"339.57993\"
		y=\"327.75366\"
		id=\"textABC\"
		sodipodi:linespacing=\"125%\"><tspan
		sodipodi:role=\"line\"
		id=\"tspan3060\"
		x=\"365\"
		y=\"330\">value_ABC</tspan></text>
		<text
		xml:space=\"preserve\"
		style=\"font-size:24px;font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;line-height:125%;letter-spacing:0px;word-spacing:0px;fill:#000000;fill-opacity:1;stroke:none;font-family:Arial;-inkscape-font-specification:Arial;text-anchor:middle;text-align:center\"
		x=\"251.1945\"
		y=\"368.89655\"
		id=\"textAC\"
		sodipodi:linespacing=\"125%\"><tspan
		sodipodi:role=\"line\"
		id=\"tspan3064\"
		x=\"270\"
		y=\"375\">value_AC</tspan></text>
		<text
		xml:space=\"preserve\"
		style=\"font-size:24px;font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;line-height:125%;letter-spacing:0px;word-spacing:0px;fill:#000000;fill-opacity:1;stroke:none;font-family:Arial;-inkscape-font-specification:Arial;text-anchor:middle;text-align:center\"
		x=\"435.48026\"
		y=\"364.79395\"
		id=\"textBC\"
		sodipodi:linespacing=\"125%\"><tspan
		sodipodi:role=\"line\"
		id=\"tspan3068\"
		x=\"460\"
		y=\"375\">value_BC</tspan></text>
		<text
		xml:space=\"preserve\"
		style=\"font-size:24px;font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;line-height:125%;letter-spacing:0px;word-spacing:0px;fill:#000000;fill-opacity:1;stroke:none;font-family:Arial;-inkscape-font-specification:Arial;text-anchor:middle;text-align:center\"
		x=\"341.68024\"
		y=\"476.3251\"
		id=\"textC\"
		sodipodi:linespacing=\"125%\"
		inkscape:label=\"#textC\"><tspan
		sodipodi:role=\"line\"
		id=\"tspan3072\"
		x=\"370\"
		y=\"495\">value_C</tspan></text>
		<text
		xml:space=\"preserve\"
		style=\"font-size:fontSize_Apx;font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;line-height:125%;letter-spacing:0px;word-spacing:0px;fill:#000000;fill-opacity:1;stroke:none;font-family:Arial;-inkscape-font-specification:Arial;text-anchor:middle;text-align:center\"
		x=\"125.71429\"
		y=\"110.93362\"
		id=\"text4238\"
		sodipodi:linespacing=\"125%\"><tspan
		sodipodi:role=\"line\"
		id=\"tspan4240\"
		x=\"100\"
		y=\"100\">name_A</tspan></text>
		<text
		xml:space=\"preserve\"
		style=\"font-size:fontSize_Bpx;font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;line-height:125%;letter-spacing:0px;word-spacing:0px;fill:#000000;fill-opacity:1;stroke:none;font-family:Arial;-inkscape-font-specification:Arial;text-anchor:middle;text-align:center\"
		x=\"534.26483\"
		y=\"99.535728\"
		id=\"text4238-2\"
		sodipodi:linespacing=\"125%\"><tspan
		sodipodi:role=\"line\"
		id=\"tspan4240-2\"
		x=\"580\"
		y=\"100\">name_B</tspan></text>
		<text
		xml:space=\"preserve\"
		style=\"font-size:fontSize_Cpx;font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;line-height:125%;letter-spacing:0px;word-spacing:0px;fill:#000000;fill-opacity:1;stroke:none;font-family:Arial;-inkscape-font-specification:Arial;text-anchor:middle;text-align:center\"
		x=\"324.2648\"
		y=\"603.82147\"
		id=\"text4238-1\"
		sodipodi:linespacing=\"125%\"><tspan
		sodipodi:role=\"line\"
		id=\"tspan4240-6\"
		x=\"375\"
		y=\"615\">name_C</tspan></text>
		</g>
		</svg>
		")
}


Venn2er_base <- function() {
	return( 
		"<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>
		<!-- Created with Inkscape (http://www.inkscape.org/) -->
		
		<svg
		xmlns:dc=\"http://purl.org/dc/elements/1.1/\"
		xmlns:cc=\"http://creativecommons.org/ns#\"
		xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"
		xmlns:svg=\"http://www.w3.org/2000/svg\"
		xmlns=\"http://www.w3.org/2000/svg\"
		xmlns:sodipodi=\"http://sodipodi.sourceforge.net/DTD/sodipodi-0.dtd\"
		xmlns:inkscape=\"http://www.inkscape.org/namespaces/inkscape\"
		width=\"750\"
		height=\"550\"
		id=\"svg3009\"
		version=\"1.1\"
		inkscape:version=\"0.48.4 r9939\"
		sodipodi:docname=\"Venn2er_base.svg\">
		<defs
		id=\"defs3011\" />
		<sodipodi:namedview
		id=\"base\"
		pagecolor=\"#ffffff\"
		bordercolor=\"#666666\"
		borderopacity=\"1.0\"
		inkscape:pageopacity=\"0.0\"
		inkscape:pageshadow=\"2\"
		inkscape:zoom=\"1.5004828\"
		inkscape:cx=\"350\"
		inkscape:cy=\"300\"
		inkscape:document-units=\"px\"
		inkscape:current-layer=\"layer1\"
		showgrid=\"false\"
		inkscape:window-width=\"1527\"
		inkscape:window-height=\"877\"
		inkscape:window-x=\"65\"
		inkscape:window-y=\"-8\"
		inkscape:window-maximized=\"0\" />
		<metadata
		id=\"metadata3014\">
		<rdf:RDF>
		<cc:Work
		rdf:about=\"\">
		<dc:format>image/svg+xml</dc:format>
		<dc:type
		rdf:resource=\"http://purl.org/dc/dcmitype/StillImage\" />
		<dc:title></dc:title>
		</cc:Work>
		</rdf:RDF>
		</metadata>
		<g
		inkscape:label=\"Layer 1\"
		inkscape:groupmode=\"layer\"
		id=\"layer1\">
		<path
		inkscape:connector-curvature=\"0\"
		style=\"opacity:0.8;fill:#ff0000;fill-opacity:opacity_B;stroke:#000000;stroke-width:1;stroke-miterlimit:4;stroke-opacity:1;stroke-dasharray:none\"
		d=\"m 460.34451,107.4375 c -27.46194,0 -53.18553,7.40847 -75.3125,20.3125 44.50818,25.95362 74.4375,74.19969 74.4375,129.4375 0,55.23781 -29.92932,103.48388 -74.4375,129.4375 22.12697,12.90403 47.85056,20.3125 75.3125,20.3125 82.69732,0 149.75,-67.05268 149.75,-149.75 0,-82.69732 -67.05268,-149.75 -149.75,-149.75 z\"
		id=\"opacity_B\"
		inkscape:label=\"opacity_B\" />
		<path
		inkscape:connector-curvature=\"0\"
		style=\"opacity:0.8;fill:#ff0000;fill-opacity:opacity_A;stroke:#000000;stroke-width:1;stroke-miterlimit:4;stroke-opacity:1;stroke-dasharray:none\"
		d=\"m 309.21951,107.53125 c -82.69732,0 -149.71875,67.05268 -149.71875,149.75 0,82.69732 67.02143,149.75 149.71875,149.75 27.45951,0 53.18685,-7.41059 75.3125,-20.3125 -44.49539,-25.95685 -74.40625,-74.20928 -74.40625,-129.4375 0,-55.22822 29.91086,-103.48065 74.40625,-129.4375 -22.12565,-12.90191 -47.85299,-20.3125 -75.3125,-20.3125 z\"
		id=\"opacity_A\"
		inkscape:label=\"opacity_A\" />
		<path
		style=\"opacity:0.8;fill:#ff0000;fill-opacity:opacity_AB;stroke:#000000;stroke-width:1.01015258;stroke-miterlimit:4;stroke-opacity:1;stroke-dasharray:none\"
		d=\"m 376.15167,378.44212 c -11.6993,-7.88909 -30.92334,-27.36443 -38.75688,-39.26352 -28.90832,-43.91154 -33.00481,-96.65934 -11.20289,-144.25217 10.07566,-21.99481 30.27418,-45.79054 50.04081,-58.95272 9.86041,-6.56584 8.50573,-6.76769 22.11569,3.2954 30.85809,22.81625 51.96267,58.42393 57.84478,97.59581 1.389,9.25006 1.389,31.36892 0,40.61898 -6.09278,40.5749 -26.79506,74.82358 -59.92316,99.13351 -11.45808,8.40812 -10.48606,8.31996 -20.11835,1.82471 z\"
		id=\"opacity_AB\"
		inkscape:connector-curvature=\"0\"
		sodipodi:nodetypes=\"cssssssscc\"
		inkscape:label=\"opacity_AB\" />
		<text
		xml:space=\"preserve\"
		style=\"font-size:fontSize_Apx;font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;line-height:125%;letter-spacing:0px;word-spacing:0px;fill:#000000;fill-opacity:1;stroke:none;font-family:Arial;-inkscape-font-specification:Arial;text-anchor:middle;text-align:center\"
		x=\"81.762344\"
		y=\"108.87971\"
		id=\"name_A\"
		sodipodi:linespacing=\"125%\"
		inkscape:label=\"name_A\"><tspan
		sodipodi:role=\"line\"
		id=\"tspan3039\"
		x=\"120\"
		y=\"108\">name_A</tspan></text>
		<text
		xml:space=\"preserve\"
		style=\"font-size:fontSize_Bpx;font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;line-height:125%;letter-spacing:0px;word-spacing:0px;fill:#000000;fill-opacity:1;stroke:none;font-family:Arial;-inkscape-font-specification:Arial;text-anchor:middle;text-align:center\"
		x=\"574.71674\"
		y=\"108.87971\"
		id=\"name_B\"
		sodipodi:linespacing=\"125%\"
		inkscape:label=\"name_B\"><tspan
		sodipodi:role=\"line\"
		id=\"tspan3043\"
		x=\"600\"
		y=\"108\">name_B</tspan></text>
		<text
		xml:space=\"preserve\"
		style=\"font-size:30px;font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;line-height:125%;letter-spacing:0px;word-spacing:0px;fill:#000000;fill-opacity:1;stroke:none;font-family:Arial;-inkscape-font-specification:Arial;text-anchor:middle;text-align:center\"
		x=\"203.99081\"
		y=\"266.86621\"
		id=\"value_A\"
		sodipodi:linespacing=\"125%\"
		inkscape:label=\"value_A\"><tspan
		sodipodi:role=\"line\"
		id=\"tspan3047\"
		x=\"235.99081\"
		y=\"266.86621\"
		style=\"font-size:25px\">value_A</tspan></text>
		<text
		xml:space=\"preserve\"
		style=\"font-size:30px;font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;line-height:125%;letter-spacing:0px;word-spacing:0px;fill:#000000;fill-opacity:1;stroke:none;font-family:Arial;-inkscape-font-specification:Arial;text-anchor:middle;text-align:center\"
		x=\"334.33908\"
		y=\"266.86621\"
		id=\"value_AB\"
		sodipodi:linespacing=\"125%\"
		inkscape:label=\"value_AB\"><tspan
		sodipodi:role=\"line\"
		id=\"tspan3047-4\"
		x=\"375.33908\"
		y=\"266.86621\"
		style=\"font-size:25px\">value_AB</tspan></text>
		<text
		xml:space=\"preserve\"
		style=\"font-size:30px;font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;line-height:125%;letter-spacing:0px;word-spacing:0px;fill:#000000;fill-opacity:1;stroke:none;font-family:Arial;-inkscape-font-specification:Arial;text-anchor:middle;text-align:center\"
		x=\"464.68738\"
		y=\"266.86621\"
		id=\"value_B\"
		sodipodi:linespacing=\"125%\"
		inkscape:label=\"value_B\"><tspan
		sodipodi:role=\"line\"
		id=\"tspan3047-9\"
		x=\"510.68738\"
		y=\"266.86621\"
		style=\"font-size:25px\">value_B</tspan></text>
		</g>
		</svg>")
}

Venn4er_base <- function() {
	return( 
		"<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>
		<!-- Created with Inkscape (http://www.inkscape.org/) -->
		
		<svg
		xmlns:dc=\"http://purl.org/dc/elements/1.1/\"
		xmlns:cc=\"http://creativecommons.org/ns#\"
		xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"
		xmlns:svg=\"http://www.w3.org/2000/svg\"
		xmlns=\"http://www.w3.org/2000/svg\"
		xmlns:sodipodi=\"http://sodipodi.sourceforge.net/DTD/sodipodi-0.dtd\"
		xmlns:inkscape=\"http://www.inkscape.org/namespaces/inkscape\"
		width=\"744.09448819\"
		height=\"1052.3622047\"
		id=\"svg2\"
		version=\"1.1\"
		inkscape:version=\"0.48.4 r9939\"
		sodipodi:docname=\"Base to R.svg\">
		<defs
		id=\"defs4\" />
		<sodipodi:namedview
		id=\"base\"
		pagecolor=\"#ffffff\"
		bordercolor=\"#666666\"
		borderopacity=\"1.0\"
		inkscape:pageopacity=\"0.0\"
		inkscape:pageshadow=\"2\"
		inkscape:zoom=\"1\"
		inkscape:cx=\"293.26053\"
		inkscape:cy=\"750.8174\"
		inkscape:document-units=\"px\"
		inkscape:current-layer=\"layer1\"
		showgrid=\"false\"
		inkscape:window-width=\"1538\"
		inkscape:window-height=\"877\"
		inkscape:window-x=\"54\"
		inkscape:window-y=\"-8\"
		inkscape:window-maximized=\"1\" />
		<metadata
		id=\"metadata7\">
		<rdf:RDF>
		<cc:Work
		rdf:about=\"\">
		<dc:format>image/svg+xml</dc:format>
		<dc:type
		rdf:resource=\"http://purl.org/dc/dcmitype/StillImage\" />
		<dc:title></dc:title>
		</cc:Work>
		</rdf:RDF>
		</metadata>
		<g
		inkscape:label=\"Layer 1\"
		inkscape:groupmode=\"layer\"
		id=\"layer1\">
		<g
		id=\"g3764\"
		transform=\"translate(358,-4)\" />
		<g
		id=\"g3807\"
		transform=\"matrix(0.90083118,0,0,0.90083118,39.595228,62.593598)\">
		<path
		inkscape:label=\"opacity_A\"
		sodipodi:nodetypes=\"cssssccsscsssscscc\"
		inkscape:connector-curvature=\"0\"
		id=\"opacity_A\"
		d=\"m 323.1595,626.90442 c -42.0524,-6.5475 -86.4399,-27.96663 -125.88883,-54.25593 -25.69859,-17.1258 -39.94414,-28.7628 -61.01034,-49.8385 -44.91879,-44.939 -73.84309,-92.72265 -87.11474,-143.91573 -18.73837,-72.28002 4.28765,-132.4589 60.58544,-158.34103 5.59577,-2.57258 13.65105,-7.35201 21.67839,-8.67653 -4.51845,14.60184 -2.74344,10.62586 -4.5357,20.13755 -2.77561,15.55721 -2.03442,45.0826 1.59587,63.57143 2.69614,13.73127 11.30704,40.40303 17.52598,54.28571 l 4.15966,9.28572 -3.33262,10.71428 c -8.64452,27.79185 -9.74144,34.87905 -9.72874,62.85715 0.0107,23.49322 0.38378,27.69791 3.35939,37.85715 14.72389,50.2697 50.15,80.6115 105.88044,90.6844 l 9.2857,1.6784 6.4286,10.051 c 14.7356,23.039 43.89767,49.21941 90.73833,56.67091 0.33804,0.95297 -21.24227,-0.58296 -29.62683,-2.76598 z\"
		style=\"fill-opacity:opacity_A;fill:#ff0000;stroke-opacity:1;stroke:#000000;stroke-width:2\" />
		<path
		inkscape:label=\"opacity_ABCD\"
		sodipodi:nodetypes=\"cccscsc\"
		inkscape:connector-curvature=\"0\"
		id=\"opacity_ABCD\"
		d=\"m 369.38068,286.59609 c -67.06508,54.17202 -113.56959,126.60598 -122.84375,191.90625 37.7053,32.39813 81.41532,56.11738 123.125,70.4375 22.81383,-7.54315 46.15052,-17.94989 69.40625,-31.375 17.26475,-9.96663 33.6055,-21.08673 48.875,-33.0625 -4.93458,-46.74344 -27.9976,-99.15551 -68.875,-148.1875 -15.24644,-18.28794 -31.98845,-34.92485 -49.6875,-49.71875 z\"
		style=\"fill-opacity:opacity_ABCD;fill:#ff0000;stroke:#000000;stroke-opacity:1;stroke-width:1.5\" />
		<path
		inkscape:label=\"opacity_B\"
		sodipodi:nodetypes=\"sscccs\"
		inkscape:connector-curvature=\"0\"
		id=\"opacity_B\"
		d=\"m 265.3664,128.0464 c -27.33223,-0.0865 -52.76082,5.37882 -74.53125,16.96875 -27.53229,14.65738 -48.41929,36.88688 -57.78125,64.625 35.90141,-8.69109 80.09307,-4.15206 122.64645,10.59375 35.87051,-29.77716 72.79355,-52.75719 113.25979,-69.0625 -35.27578,-14.93433 -70.8056,-23.02129 -103.59374,-23.125 z\"
		style=\"fill-opacity:opacity_B;fill:#ff0000;stroke:#000000;stroke-opacity:1;stroke-width:1.5\" />
		<path
		inkscape:label=\"opacity_C\"
		sodipodi:nodetypes=\"cccc\"
		inkscape:connector-curvature=\"0\"
		id=\"opacity_C\"
		d=\"m 371.22434,151.5297 c 38.1286,16.14212 75.9662,40.30305 109.6875,71.15625 47.7982,-17.13306 92.00158,-22.45688 129.75938,-13.72629 C 581.22932,128.50865 480.94564,107.67169 371.22434,151.5297 z\"
		style=\"fill-opacity:opacity_C;fill:#ff0000;stroke:#000000;stroke-opacity:1;stroke-width:1.5\" />
		<path
		inkscape:label=\"opacity_ABC\"
		sodipodi:nodetypes=\"ccccc\"
		inkscape:connector-curvature=\"0\"
		id=\"path3526-2-6\"
		d=\"m 153.72946,359.7622 c 22.94865,47.09193 53.19052,87.5234 92.875,117.53125 9.275,-65.30644 54.2994,-137.20119 121.375,-191.375 -35.1319,-29.3655 -74.0312,-51.47125 -112.5313,-64.8125 -47.4254,40.74932 -82.9872,89.90784 -101.7187,138.65625 z\"
		style=\"fill-opacity:opacity_ABC;fill:#ff0000;stroke:#000000;stroke-opacity:1;stroke-width:1.5\" />
		<path
		inkscape:label=\"opacity_BCD\"
		sodipodi:nodetypes=\"csccscsc\"
		inkscape:connector-curvature=\"0\"
		id=\"opacity_BCD\"
		d=\"m 370.91319,286.67702 c 17.6985,14.79363 34.4414,31.43126 49.68754,49.71875 40.8861,49.04253 63.9159,101.43646 68.8437,148.1875 38.7406,-30.38258 70.5922,-66.41972 93.18762,-103.90625 -10.6415,-38.53543 -32.14282,-79.18367 -64.43762,-117.8125 -11.6395,-13.92251 -24.1177,-26.91951 -37.2187,-38.90625 -19.8259,7.10649 -40.0022,16.51257 -60.125,28.25 -17.71494,10.33293 -34.41664,21.93318 -49.93754,34.46875 z\"
		style=\"fill-opacity:opacity_BCD;fill:#ff0000;stroke:#000000;stroke-opacity:1;stroke-width:1.5\" />
		<path
		inkscape:label=\"opacity_ABD\"
		sodipodi:nodetypes=\"csccc\"
		inkscape:connector-curvature=\"0\"
		id=\"opacity_ABD\"
		d=\"m 488.75749,486.03034 c -15.2674,11.97363 -31.5817,23.06617 -48.8437,33.03125 -23.2674,13.43181 -46.6128,23.86223 -69.4375,31.40625 37.1224,12.7478 73.8485,17.6445 106.6875,13.4062 10.8024,-22.38313 14.6299,-49.03926 11.5937,-77.8437 z\"
		style=\"fill-opacity:opacity_ABD;fill:#ff0000;stroke:#000000;stroke-opacity:1;stroke-width:1.5\" />
		<path
		inkscape:label=\"opacity_ACD\"
		sodipodi:nodetypes=\"ccscc\"
		inkscape:connector-curvature=\"0\"
		id=\"opacity_ACD\"
		d=\"m 368.39422,549.31083 c -41.70999,-14.32322 -84.92099,-38.06794 -122.62499,-70.46875 -3.5001,24.64425 -1.8798,48.36215 5.5312,69.8125 1.7775,5.14475 3.8642,10.02315 6.2188,14.68745 33.0685,3.7938 75.2009,0.9166 110.87499,-14.0312 z\"
		style=\"fill-opacity:opacity_ACD;fill:#ff0000;stroke:#000000;stroke-opacity:1;stroke-width:1.5\" />
		<path
		inkscape:label=\"opacity_AD\"
		sodipodi:nodetypes=\"cssccccsccc\"
		inkscape:connector-curvature=\"0\"
		id=\"opacity_AD\"
		d=\"m 346.35679,628.32473 c 3.3587,0.1771 7.9293,1.3324 11.2336,1.3431 13.2171,0.043 25.9725,-0.1932 38.0555,-2.9061 12.083,-2.7129 23.4936,-6.8417 34.0212,-12.462 m 0,0 c 21.9683,-11.728 37.9114,-28.8122 47.9063,-49.5312 -32.8309,4.2373 -73.0432,-1.16547 -110.1563,-13.90625 -25.6914,8.49458 -47.2227,13.80893 -70.7383,15.04946 -11.7577,0.62026 -27.37918,-0.7136 -38.27207,-1.86695 10.74483,25.9151 47.28527,52.60795 78.07297,61.4425 5.1071,1.3949 9.933,2.7306 10.7187,2.9687\"
		style=\"fill-opacity:opacity_AD;fill:#ff0000;stroke:#000000;stroke-opacity:1;stroke-width:1.5\" />
		<path
		inkscape:label=\"opacity_BD\"
		sodipodi:nodetypes=\"cccsc\"
		id=\"opacity_BD\"
		d=\"m 582.31676,383.67969 c -22.5949,37.48219 -54.4512,71.49567 -93.1876,101.87497 3.0415,28.8088 -0.7939,57.4561 -11.5937,79.8437 18.0405,-2.3283 34.8941,-7.3955 50,-15.4375 56.6533,-30.1605 74.5941,-94.5336 54.7813,-166.28117 z\"
		style=\"fill-opacity:opacity_BD;fill:#ff0000;stroke:#000000;stroke-opacity:1;stroke-width:1.5\"
		inkscape:connector-curvature=\"0\" />
		<path
		inkscape:label=\"opacity_D\"
		sodipodi:nodetypes=\"ccscscssc\"
		inkscape:connector-curvature=\"0\"
		id=\"opacity_D\"
		d=\"m 614.98601,211.80086 c 24.68677,75.8996 -15.25658,142.91216 -32.1562,169.9375 19.8128,71.74758 0.372,139.12067 -56.2813,169.28125 -15.1059,8.04194 -31.9595,13.10911 -50,15.4375 -9.9949,20.719 -25.938,37.8032 -47.9063,49.5312 -17.3108,9.2417 -34.0067,12.4234 -55.1562,13.9376 48.5662,3.2748 103.2513,-9.1569 160.4688,-42.5313 123.1615,-71.8389 197.4067,-204.21007 165.8124,-295.65625 -13.9838,-40.47457 -42.3601,-70.12015 -84.7812,-79.9375 z\"
		style=\"fill-opacity:opacity_D;fill:#ff0000;stroke:#000000;stroke-opacity:1;stroke-width:1.5\" />
		<path
		inkscape:label=\"opacity_AC\"
		sodipodi:nodetypes=\"cccscsc\"
		inkscape:connector-curvature=\"0\"
		id=\"opacity_AC\"
		d=\"m 256.1343,561.90625 c -2.3535,-4.66284 -4.4419,-9.57581 -6.2187,-14.71875 -7.4111,-21.45038 -9.0001,-45.13696 -5.5,-69.78125 -16.5835,-14.24929 -32.2971,-30.06226 -46.7188,-47.3125 -19.2241,-22.99454 -34.1159,-47.22329 -45.6875,-70.78125 -16.3324,42.47661 -20.3785,85.12887 -7.8125,121.125 16.6257,47.6256 58.2689,75.31166 111.9375,81.46875 z\"
		style=\"fill-opacity:opacity_AC;fill:#ff0000;stroke:#000000;stroke-opacity:1;stroke-width:1.5\" />
		<path
		inkscape:label=\"opacity_CD\"
		sodipodi:nodetypes=\"cscccsc\"
		inkscape:connector-curvature=\"0\"
		id=\"opacity_CD\"
		d=\"m 482.48113,223.21151 c 13.0954,11.98282 25.5837,24.95796 37.2187,38.875 32.2948,38.62882 53.7961,79.30832 64.4376,117.84375 34.5372,-57.30553 47.4564,-118.01909 30.2812,-167.21875 -0.2028,-0.58097 -0.4148,-1.14371 -0.625,-1.71875 -12.9888,-3.00594 -26.9287,-4.52878 -41.5625,-4.53125 -27.9143,-0.005 -58.3834,5.51209 -89.75,16.75 z\"
		style=\"fill-opacity:opacity_CD;fill:#ff0000;stroke:#000000;stroke-opacity:1;stroke-width:1.5\" />
		<path
		inkscape:label=\"opacity_AB\"
		sodipodi:nodetypes=\"ccscc\"
		inkscape:connector-curvature=\"0\"
		id=\"opacity_AB\"
		d=\"m 151.9468,359.9375 c 18.73404,-48.74722 54.29306,-97.90905 101.7188,-138.65625 -27.38726,-9.49104 -54.56316,-14.54422 -80.0313,-14.625 -14.10127,-0.0447 -27.67442,1.46491 -40.46875,4.5625 -14.08214,41.74795 -7.69454,94.82494 18.78125,148.71875 z\"
		style=\"fill-opacity:opacity_AB;fill:#ff0000;stroke:#000000;stroke-opacity:1;stroke-width:1.5\" />
		<path
		inkscape:label=\"opacity_BC\"
		sodipodi:nodetypes=\"ccccc\"
		inkscape:connector-curvature=\"0\"
		id=\"opacity_BC\"
		d=\"m 369.61496,152.48896 c -41.72527,17.02528 -81.00578,40.40402 -112.93741,67.64829 38.5019,13.34105 77.3976,36.85978 112.5312,66.22671 34.6364,-27.5823 72.4763,-48.99514 112.21513,-63.39461 -34.43001,-28.73786 -73.67733,-54.33327 -111.80892,-70.48039 z\"
		style=\"fill-opacity:opacity_BC;fill:#ff0000;stroke:#000000;stroke-opacity:1;stroke-width:1.5\" />
		</g>
		</g>
		<g
		inkscape:groupmode=\"layer\"
		id=\"layer2\"
		inkscape:label=\"text\">
		<text
		id=\"text3045\"
		x=\"521.3645\"
		y=\"153.93195\"
		style=\"font-size:14.08419703999999900px;text-anchor:middle;text-align:center\">
		<tspan
		id=\"tspan3047\"
		sodipodi:role=\"line\"
		style=\"font-size:fontSize_Cpx;font-variant:normal;font-weight:normal;writing-mode:lr-tb;fill:#000000;fill-opacity:1;fill-rule:nonzero;stroke:none;font-family:Helvetica;-inkscape-font-specification:Helvetica;text-anchor:middle;text-align:center\"
		x=\"482.52118\"
		y=\"153.93195\">name_C</tspan>
		</text>
		<text
		id=\"text3057\"
		x=\"684.18121\"
		y=\"249.48895\"
		style=\"font-size:14.08419703999999900px;text-anchor:middle;text-align:center\">
		<tspan
		id=\"tspan3059\"
		sodipodi:role=\"line\"
		style=\"font-size:fontSize_Dpx;font-variant:normal;font-weight:normal;writing-mode:lr-tb;fill:#000000;fill-opacity:1;fill-rule:nonzero;stroke:none;font-family:Helvetica;-inkscape-font-specification:Helvetica;text-anchor:middle;text-align:center\"
		x=\"645.48236\"
		y=\"249.48895\">name_D</tspan>
		</text>
		<text
		id=\"text3069\"
		x=\"117.11016\"
		y=\"395.27802\"
		style=\"font-size:20px;text-align:center;text-anchor:middle\">
		<tspan
		id=\"tspan3071\"
		sodipodi:role=\"line\"
		style=\"font-size:20px;font-variant:normal;font-weight:normal;text-align:center;writing-mode:lr-tb;text-anchor:middle;fill:#000000;fill-opacity:1;fill-rule:nonzero;stroke:none;font-family:Helvetica;-inkscape-font-specification:Helvetica\"
		x=\"117.11016\"
		y=\"395.27802\">value_A</tspan>
		</text>
		<text
		id=\"text3073\"
		x=\"249.2789\"
		y=\"225.60635\"
		style=\"font-size:20px;text-align:center;text-anchor:middle\">
		<tspan
		id=\"tspan3075\"
		sodipodi:role=\"line\"
		style=\"font-size:20px;font-variant:normal;font-weight:normal;text-align:center;writing-mode:lr-tb;text-anchor:middle;fill:#000000;fill-opacity:1;fill-rule:nonzero;stroke:none;font-family:Helvetica;-inkscape-font-specification:Helvetica\"
		x=\"249.2789\"
		y=\"225.60635\">value_B</tspan>
		</text>
		<text
		id=\"text3077\"
		x=\"504.80731\"
		y=\"225.60635\"
		style=\"font-size:20px;text-align:center;text-anchor:middle\">
		<tspan
		id=\"tspan3079\"
		sodipodi:role=\"line\"
		style=\"font-size:20px;font-variant:normal;font-weight:normal;text-align:center;writing-mode:lr-tb;text-anchor:middle;fill:#000000;fill-opacity:1;fill-rule:nonzero;stroke:none;font-family:Helvetica;-inkscape-font-specification:Helvetica\"
		x=\"504.80731\"
		y=\"225.60635\">value_C</tspan>
		</text>
		<text
		id=\"text3081\"
		x=\"625.57294\"
		y=\"397.27802\"
		style=\"font-size:20px;text-align:center;text-anchor:middle\">
		<tspan
		id=\"tspan3083\"
		sodipodi:role=\"line\"
		style=\"font-size:20px;font-variant:normal;font-weight:normal;text-align:center;writing-mode:lr-tb;text-anchor:middle;fill:#000000;fill-opacity:1;fill-rule:nonzero;stroke:none;font-family:Helvetica;-inkscape-font-specification:Helvetica\"
		x=\"625.57294\"
		y=\"397.27802\">value_D</tspan>
		</text>
		<text
		id=\"text3085\"
		x=\"189.14771\"
		y=\"295.64047\"
		style=\"font-size:20px;text-align:center;text-anchor:middle\">
		<tspan
		id=\"tspan3087\"
		y=\"295.64047\"
		x=\"186.13441\"
		style=\"font-size:20px;font-variant:normal;font-weight:normal;text-align:center;writing-mode:lr-tb;text-anchor:middle;fill:#000000;fill-opacity:1;fill-rule:nonzero;stroke:none;font-family:Helvetica;-inkscape-font-specification:Helvetica\">value_AB</tspan>
		</text>
		<text
		id=\"text3089\"
		x=\"209.14909\"
		y=\"499.15149\"
		style=\"font-size:20px;text-align:center;text-anchor:middle\">
		<tspan
		id=\"tspan3091\"
		y=\"499.15149\"
		x=\"205.81029\"
		style=\"font-size:20px;font-variant:normal;font-weight:normal;text-align:center;writing-mode:lr-tb;text-anchor:middle;fill:#000000;fill-opacity:1;fill-rule:nonzero;stroke:none;font-family:Helvetica;-inkscape-font-specification:Helvetica\">value_AC</tspan>
		</text>
		<text
		id=\"text3093\"
		x=\"370.88644\"
		y=\"603.19421\"
		style=\"font-size:20px;text-align:center;text-anchor:middle\">
		<tspan
		id=\"tspan3095\"
		sodipodi:role=\"line\"
		style=\"font-size:20px;font-variant:normal;font-weight:normal;text-align:center;writing-mode:lr-tb;text-anchor:middle;fill:#000000;fill-opacity:1;fill-rule:nonzero;stroke:none;font-family:Helvetica;-inkscape-font-specification:Helvetica\"
		x=\"370.88644\"
		y=\"603.19421\">value_AD</tspan>
		</text>
		<text
		id=\"text3097\"
		x=\"370.81818\"
		y=\"269.62668\"
		style=\"font-size:20px;text-align:center;text-anchor:middle\">
		<tspan
		id=\"tspan3099\"
		sodipodi:role=\"line\"
		style=\"font-size:20px;font-variant:normal;font-weight:normal;text-align:center;writing-mode:lr-tb;text-anchor:middle;fill:#000000;fill-opacity:1;fill-rule:nonzero;stroke:none;font-family:Helvetica;-inkscape-font-specification:Helvetica\"
		x=\"370.81818\"
		y=\"269.62668\">value_BC</tspan>
		</text>
		<text
		id=\"text3101\"
		x=\"525.47522\"
		y=\"499.15149\"
		style=\"font-size:20px;text-align:center;text-anchor:middle\">
		<tspan
		id=\"tspan3103\"
		y=\"499.15149\"
		x=\"522.13641\"
		style=\"font-size:20px;font-variant:normal;font-weight:normal;text-align:center;writing-mode:lr-tb;text-anchor:middle;fill:#000000;fill-opacity:1;fill-rule:nonzero;stroke:none;font-family:Helvetica;-inkscape-font-specification:Helvetica\">value_BD</tspan>
		</text>
		<text
		id=\"text3105\"
		x=\"562.42499\"
		y=\"295.64047\"
		style=\"font-size:20px;text-anchor:middle;text-align:center\">
		<tspan
		id=\"tspan3107\"
		y=\"295.64047\"
		x=\"555.48584\"
		style=\"font-size:20px;font-variant:normal;font-weight:normal;writing-mode:lr-tb;fill:#000000;fill-opacity:1;fill-rule:nonzero;stroke:none;font-family:Helvetica;-inkscape-font-specification:Helvetica;text-anchor:middle;text-align:center\">value_CD</tspan>
		</text>
		<text
		id=\"text3109\"
		x=\"245.15263\"
		y=\"386.09192\"
		style=\"font-size:20px;text-align:center;text-anchor:middle\">
		<tspan
		id=\"tspan3111\"
		y=\"386.09192\"
		x=\"247.8654\"
		style=\"font-size:20px;font-variant:normal;font-weight:normal;text-align:center;writing-mode:lr-tb;text-anchor:middle;fill:#000000;fill-opacity:1;fill-rule:nonzero;stroke:none;font-family:Helvetica;-inkscape-font-specification:Helvetica\">value_ABC</tspan>
		</text>
		<text
		id=\"text3113\"
		x=\"443.39343\"
		y=\"553.04474\"
		style=\"font-size:20px;text-align:center;text-anchor:middle\">
		<tspan
		id=\"tspan3115\"
		y=\"553.04474\"
		x=\"440.38013\"
		style=\"font-size:20px;font-variant:normal;font-weight:normal;text-align:center;writing-mode:lr-tb;text-anchor:middle;fill:#000000;fill-opacity:1;fill-rule:nonzero;stroke:none;font-family:Helvetica;-inkscape-font-specification:Helvetica\">value_ABD</tspan>
		</text>
		<text
		id=\"text3117\"
		x=\"299.25342\"
		y=\"553.04474\"
		style=\"font-size:20px;text-align:center;text-anchor:middle\">
		<tspan
		id=\"tspan3119\"
		y=\"553.04474\"
		x=\"296.24011\"
		style=\"font-size:20px;font-variant:normal;font-weight:normal;text-align:center;writing-mode:lr-tb;text-anchor:middle;fill:#000000;fill-opacity:1;fill-rule:nonzero;stroke:none;font-family:Helvetica;-inkscape-font-specification:Helvetica\">value_ACD</tspan>
		</text>
		<text
		id=\"text3121\"
		x=\"495.37305\"
		y=\"386.09192\"
		style=\"font-size:20px;text-align:center;text-anchor:middle\">
		<tspan
		id=\"tspan3123\"
		y=\"386.09192\"
		x=\"492.08582\"
		style=\"font-size:20px;font-variant:normal;font-weight:normal;text-align:center;writing-mode:lr-tb;text-anchor:middle;fill:#000000;fill-opacity:1;fill-rule:nonzero;stroke:none;font-family:Helvetica;-inkscape-font-specification:Helvetica\">value_BCD</tspan>
		</text>
		<text
		id=\"text3125\"
		x=\"371.26285\"
		y=\"463.37106\"
		style=\"font-size:20px;text-align:center;text-anchor:middle\">
		<tspan
		id=\"tspan3127\"
		y=\"463.37106\"
		x=\"370.97562\"
		style=\"font-size:20px;font-variant:normal;font-weight:normal;text-align:center;writing-mode:lr-tb;text-anchor:middle;fill:#000000;fill-opacity:1;fill-rule:nonzero;stroke:none;font-family:Helvetica;-inkscape-font-specification:Helvetica\">value_ABCD</tspan>
		</text>
		<text
		id=\"text3045-9\"
		x=\"242.5423\"
		y=\"153.93195\"
		style=\"font-size:20px;text-anchor:middle;text-align:center\">
		<tspan
		id=\"tspan3047-0\"
		sodipodi:role=\"line\"
		style=\"font-size:fontSize_Bpx;font-variant:normal;font-weight:normal;writing-mode:lr-tb;fill:#000000;fill-opacity:1;fill-rule:nonzero;stroke:none;font-family:Helvetica;-inkscape-font-specification:Helvetica;text-anchor:middle;text-align:center\"
		x=\"204.42625\"
		y=\"153.93195\">name_B</tspan>
		</text>
		<text
		id=\"text3045-0\"
		x=\"67.119965\"
		y=\"249.48895\"
		style=\"font-size:14.08419703999999900px;text-anchor:middle;text-align:center\">
		<tspan
		id=\"tspan3047-6\"
		sodipodi:role=\"line\"
		style=\"font-size:fontSize_Apx;font-variant:normal;font-weight:normal;writing-mode:lr-tb;fill:#000000;fill-opacity:1;fill-rule:nonzero;stroke:none;font-family:Helvetica;-inkscape-font-specification:Helvetica;text-anchor:middle;text-align:center\"
		x=\"28.426247\"
		y=\"249.48895\">name_A</tspan>
		</text>
		</g>
		</svg>
		")}
