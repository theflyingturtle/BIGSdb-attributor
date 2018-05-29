library(ReporteRs)
library(dplyr)
library(reshape2)
library(ggplot2)
library(cowplot)
library(RColorBrewer)
library(zoo)

#TODO: In py script head id column in ancs output; add date column (filling as per usual); must manually add species?

# Define colour palette
set_fill_colours = scale_fill_manual(values = c("Cattle" = "#6FBAFF", "Chicken" = "#FFF697", "Environment" = "#A4FF53", "Sheep" = "#B3B3B3", "Turkey" = "#000000", "Duck" = "#FF9E36", "Goose" = "#FF6666", "Pig" = "#ffc0e9", "Wild.bird" = "#803C20", "Other animal" = "#800080", "Dog" = "#008080", "Ruminant" = "#6FBAFF"))
# Alternative qualitative palette
# NOTE if this is used, must ensure that colour mapping is consistent across all graphs
# set_fill_colours = scale_fill_brewer(type="qual", palette="Set3", direction=1)

### File processing ###
# Open C. jejuni file, convert missing data to NA and drop rows without dates
# Note that PubMLST dates are in ISO 8601 format (for xx/xx/xxxx use format=format='%d-%m-%Y')
ancs_cj = read.csv("/Users/MelissaJvR/Desktop/FSA_Final/RInputFiles/OXC_Long_Cj_inferred-ancestry.csv", header=TRUE, check.names=TRUE)
ancs_cj[ancs_cj==""] <- NA
ancs_cj$date <- as.Date(ancs_cj$date)
cj = ancs_cj[!is.na(ancs_cj$date),]
# Repeat for C. coli
ancs_cc = read.csv("/Users/MelissaJvR/Desktop/FSA_Final/RInputFiles/OXC_Long_Cc_inferred-ancestry.csv", header=TRUE, check.names=TRUE)
ancs_cc[ancs_cc==""] <- NA
ancs_cc$date <- as.Date(ancs_cc$date)
cc = ancs_cc[!is.na(ancs_cc$date),]

### Directory setup ###
# Get current directory and name output directory
# NOTE: the script will fail here if the output directory already exists.  Remove/rename the existing directory and try again.  If you've deleted the file, make sure that your current working directory isn't set to Trash!  To do this, enter "getwd()" and then run the command "setwd(PATH/TO/DESIRED/DIRECTORY)" to move to the right directory.
current_dir = sprintf("%s",getwd())
output_dir = "AttributionReport"
# Create output diretory
dir.create(file.path(current_dir, output_dir))
# Set output directory as current directory
setwd(file.path(current_dir, output_dir))

### Data processing ###
## Isolate counts for original and cleaned datasets ##
# C. jejuni
no_ancs_cj = nrow(ancs_cj)
no_cj = nrow(cj)
cj_missing_dates = no_ancs_cj - no_cj
cj_missing_dates_prop = round((cj_missing_dates/no_ancs_cj*100), digits=2)
# C. coli
no_ancs_cc = nrow(ancs_cc)
no_cc = nrow(cc)
cc_missing_dates = no_ancs_cc - no_cc
cc_missing_dates_prop = round((cc_missing_dates/no_ancs_cc*100), digits=2)
## Max and min dates
# C. jejuni
max_date_cj = format(max(as.Date(cj$date, format="%d/%m/%Y")), "%B %Y")
min_date_cj = format(min(as.Date(cj$date, format="%d/%m/%Y")), "%B %Y")
# C. coli
max_date_cc = format(max(as.Date(cc$date, format="%d/%m/%Y")), "%B %Y")
min_date_cc = format(min(as.Date(cc$date, format="%d/%m/%Y")), "%B %Y")

## Overall summaries ##
# Generate summary plot and table for C. jejuni
# Get only source columns for overall summaries
cj_sources <- ancs_cj %>%
	select(-matches('id'))
cj_sources <- cj_sources %>%
	select(-matches('date'))
cj_sources <- cj_sources %>%
	select(-matches('species'))
# Tranform data to long form
cj_ancs_long = melt(cj_sources)
# Get proportions for each source
cj_overall_summary = melt(tapply(cj_ancs_long$value, cj_ancs_long$variable, mean), varnames="Source", value.name="Proportion")
# Set order: by source, decreasing proportion
cj_overall_summary = cj_overall_summary[order(-cj_overall_summary$Proportion),]
cj_overall_summary$Source = factor(cj_overall_summary$Source, levels=unique(as.character(cj_overall_summary$Source)))
# Generate bar plot showing overall proportions
cj_overall_plot = ggplot(cj_overall_summary) + geom_bar(aes(x=Source, y=Proportion), stat="identity", fill="black") + theme_grey(base_size = 14) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ylab("Proportion")

# Repeat for C. coli
cc_sources <- ancs_cc %>%
	select(-matches('id'))
cc_sources <- cc_sources %>%
	select(-matches('date'))
cc_sources <- cc_sources %>%
	select(-matches('species'))
cc_ancs_long = melt(cc_sources)
cc_overall_summary = melt(tapply(cc_ancs_long$value, cc_ancs_long$variable, mean), varnames="Source", value.name="Proportion")
cc_overall_summary = cc_overall_summary[order(-cc_overall_summary$Proportion),]
cc_overall_summary$Source = factor(cc_overall_summary$Source, levels=unique(as.character(cc_overall_summary$Source)))
cc_overall_plot = ggplot(cc_overall_summary) + geom_bar(aes(x=Source, y=Proportion), stat="identity", fill="black") + theme_grey(base_size = 14) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ylab("Proportion")

# Plot C. jejuni and C. coli graphs on same axis
overall_plot = plot_grid(cj_overall_plot, cc_overall_plot, labels=c("A", "B"), align="h")
# Save figure to output directory
ggsave("Figure1.svg", plot=overall_plot, device="svg", width=210, height=148, units="mm", dpi=300)

# Generate combined summary table
overall_table = full_join(cj_overall_summary, cc_overall_summary, by = "Source")
colnames(overall_table) <- c("Source", "C. jejuni", "C. coli")
# Get significant figures
overall_table$`C. jejuni` = signif(overall_table$`C. jejuni`, 3)
overall_table$`C. coli` = signif(overall_table$`C. coli`, 3)

# Generate individual ancestries plots
# C. jejuni
# Retrieve sources dataframe
# Add "poprank" column that indicates the most likely source (in the case of a tie, 'which' will pick the first column)
cj_sources$poprank = colnames(cj_sources)[apply(cj_sources,1,which.max)]
# Add "max prob" column giving max value to order on later
cj_sources[, "maxprob"] = apply(within(cj_sources, rm(poprank)), 1, max)
# Reorder the dataframe from most to least common overall source, first by most likely source, and within each source 
cj_order = order(ordered(cj_sources$poprank, levels=cj_overall_summary$Source), -cj_sources$maxprob)
cj_sources = cj_sources[cj_order,]

# Add rank column after ordering to give order to ggplot
cj_sources$rank <- seq.int(nrow(cj_sources))
# Melt data to long form
cj_ind_ancs_long = melt(cj_sources, id=c("rank", "poprank", "maxprob"), variable.name = "Source", value.name = "Proportion")

# Generate individual ancestries plot
cj_ind_ancs_plot = ggplot(cj_ind_ancs_long, aes(x=rank, y=Proportion, fill=Source, width=1)) + geom_bar(stat="identity") + labs(x="Human disease isolates", y="Source probability") + set_fill_colours

# Repeat for C. coli
cc_sources$poprank = colnames(cc_sources)[apply(cc_sources,1,which.max)]
cc_sources[, "maxprob"] = apply(within(cc_sources, rm(poprank)), 1, max) 
cc_order = order(ordered(cc_sources$poprank, levels=cc_overall_summary$Source), -cc_sources$maxprob)
cc_sources = cc_sources[cc_order,]
cc_sources$rank <- seq.int(nrow(cc_sources))
cc_ind_ancs_long = melt(cc_sources, id=c("rank", "poprank", "maxprob"), variable.name = "Source", value.name = "Proportion")

# Generate plot
cc_ind_ancs_plot = ggplot(cc_ind_ancs_long, aes(x=rank, y=Proportion, fill=Source, width=1)) + geom_bar(stat="identity") + labs(x="Human disease isolates", y="Source probability") + set_fill_colours

# Plot C. jejuni and C. coli individual ancestry graphs in single figure
overall_ind_ancs_plot = plot_grid(cj_ind_ancs_plot, cc_ind_ancs_plot, labels=c("A", "B"), nrow = 2, align = "v")
# Save figure to output directory
ggsave("Figure2.svg", plot=overall_ind_ancs_plot, device="svg", width=210, height=148, units="mm", dpi=300)

## Annual breakdown ##
# C. jejuni
# Add years column to date-complete dataset
cj_years = cj
cj_years$Year = format(cj_years$date,format="%Y")
# Drop unnecessary columns
cj_years <- cj_years %>%
    select(-matches('id'))
cj_years <- cj_years %>%
    select(-matches('species'))
cj_years <- cj_years %>%
    select(-matches('date'))
# Get averages by year
cj_years_mean = aggregate(.~Year, cj_years, mean)
# Convert to long form
cj_years_mean_long = melt(cj_years_mean, id="Year", variable.name = "Source", value.name = "Proportion")
# Read year as numeric to get proper x-axis labels in the area plot
cj_years_mean_long$Year = as.numeric(cj_years_mean_long$Year)

# Re-order the dataframe so that bars will be stacked from major (bottom) to minor (top) sources
cj_years_order = order(ordered(cj_years_mean_long$Source, levels=cj_overall_summary$Source), decreasing = TRUE)
cj_years_mean_long = cj_years_mean_long[cj_years_order,]
cj_years_mean_long$Source = factor(cj_years_mean_long$Source, levels=unique(as.character(cj_years_mean_long$Source)))

# Generate area plot and manipulate x-axis to display years correctly
cj_years_plot = ggplot(cj_years_mean_long, aes(x=Year, y=Proportion, fill=Source)) + geom_area() + scale_x_continuous(breaks=as.numeric(unique(cj_years_mean_long$Year)), labels=c(as.character(unique(cj_years_mean_long$Year)))) + labs(x="Year", y="Proportion of isolates") + set_fill_colours + theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Alternative code for bar graph
#cj_years_bar_plot = ggplot(cj_years_mean_long, aes(x=Year, y=Proportion, fill=Source)) + geom_bar(stat="identity") + set_fill_colours

# Count C. jejuni isolates per year
cj_years_count = aggregate(.~Year, cj_years, FUN= length)
colnames(cj_years_count)[2] <- "Count"
retain = c("Year","Count")
cj_years_count = cj_years_count[,retain]

# Repeat for C. coli
# Set up data
cc_years = cc
cc_years$Year = format(cc_years$date,format="%Y")
cc_years <- cc_years %>%
    select(-matches('id'))
cc_years <- cc_years %>%
    select(-matches('species'))
cc_years <- cc_years %>%
    select(-matches('date'))
cc_years_mean = aggregate(.~Year, cc_years, mean)
cc_years_mean_long = melt(cc_years_mean, id="Year", variable.name = "Source", value.name = "Proportion")
cc_years_mean_long$Year = as.numeric(cc_years_mean_long$Year)

# Re-order
cc_years_order = order(ordered(cc_years_mean_long$Source, levels=cc_overall_summary$Source), decreasing = TRUE)
cc_years_mean_long = cc_years_mean_long[cc_years_order,]
cc_years_mean_long$Source = factor(cc_years_mean_long$Source, levels=unique(as.character(cc_years_mean_long$Source)))

# Generate plot
cc_years_plot = ggplot(cc_years_mean_long, aes(x=Year, y=Proportion, fill=Source)) + geom_area() + scale_x_continuous(breaks=as.numeric(unique(cc_years_mean_long$Year)), labels=c(as.character(unique(cc_years_mean_long$Year)))) + labs(x="Year", y="Proportion of isolates") + set_fill_colours + theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Alternative code for bar graph
#cc_years_bar_plot = ggplot(cc_years_mean_long, aes(x=Year, y=Proportion, fill=Source)) + geom_bar(stat="identity") + set_fill_colours

# Count C. coli isolates per year
cc_years_count = aggregate(.~Year, cc_years, FUN= length)
colnames(cc_years_count)[2] <- "Count"
retain = c("Year","Count")
cc_years_count = cc_years_count[,retain]

# Plot C. jejuni and C. coli annual breakdown in single figure
yearly_plot = plot_grid(cj_years_plot, cc_years_plot, labels=c("A", "B"), nrow = 2, align = "v")
# Save figure to output directory
ggsave("Figure4.svg", plot=yearly_plot, device="svg", width=210, height=148, units="mm", dpi=300)
# Additional code for saving bar graphs
#yearly_bar_plot = plot_grid(cj_years_bar_plot, cc_years_bar_plot, labels=c("A", "B"), nrow = 2, align = "v")
#ggsave("Figure4BarGraph.svg", plot=yearly_bar_plot, device="svg", width=210, height=148, units="mm", dpi=300)

# Generate combined summary table of number of isolates per year
overall_years_table = full_join(cj_years_count, cc_years_count, by = "Year")
colnames(overall_years_table) <- c("Year", "C. jejuni", "C. coli")

# Plot number of isolates per year
# Prepare data
overall_years = overall_years_table
# Convert years to integers for graphing
overall_years$Year = as.integer(overall_years$Year)
# Convert data to long form
overall_years_long = melt(overall_years, id = "Year", variable.name = "Species", value.name = "Count")
overall_year_counts_plot = ggplot(data=overall_years_long, aes(x = Year, y = Count, group = Species)) + geom_line(aes(linetype=Species)) + labs(x="Year", y="Number of isolates") + theme_grey(base_size = 14) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("Figure3.svg", plot=overall_year_counts_plot, device="svg", width=210, height=148, units="mm", dpi=300)

## Quarterly breakdown ##
# C. jejuni
# Add quarters column to date-complete dataset
cj_quarters = cj
cj_quarters$Quarter = as.yearqtr(cj_quarters$date, format = "%Y-%m-%d")
# Coerce quarter into graphable format
cj_quarters$Quarter = format(cj_quarters$Quarter, format = "%Y.%q")
# Drop unnecessary columns
cj_quarters <- cj_quarters %>%
    select(-matches('id'))
cj_quarters <- cj_quarters %>%
    select(-matches('species'))
cj_quarters <- cj_quarters %>%
    select(-matches('date'))
# Get mean by quarter over time
cj_quarters_mean = aggregate(.~Quarter, cj_quarters, mean)
# Convert to long form and force quarter to be read as numeric
cj_quarters_mean_long = melt(cj_quarters_mean, id="Quarter", variable.name = "Source", value.name = "Proportion")
cj_quarters_mean_long$Quarter = as.numeric(cj_quarters_mean_long$Quarter)

# Re-order
cj_quarters_order = order(ordered(cj_quarters_mean_long$Source, levels=cj_overall_summary$Source), decreasing = TRUE)
cj_quarters_mean_long = cj_quarters_mean_long[cj_quarters_order,]
cj_quarters_mean_long$Source = factor(cj_quarters_mean_long$Source, levels=unique(as.character(cj_quarters_mean_long$Source)))

# Generate area plot
cj_quarters_plot = ggplot(cj_quarters_mean_long, aes(x=Quarter, y=Proportion, fill=Source)) + geom_area() + set_fill_colours + labs(x="Time", y="Proportion of isolates")

# Generate bar graph
# Basic plot - needs changes to x-axis
# cj_quarters_bar_plot = ggplot(cj_quarters_mean_long, aes(x=Quarter, y=Proportion, fill=Source)) + geom_bar(stat="identity") + set_fill_colours

# Count C. jejuni isolates per quarter
cj_quarters_count = aggregate(.~Quarter, cj_quarters, FUN= length)
colnames(cj_quarters_count)[2] <- "Count"
retain = c("Quarter","Count")
cj_quarters_count = cj_quarters_count[,retain]

# Repeat for C. coli
cc_quarters = cc
cc_quarters$Quarter = as.yearqtr(cc_quarters$date, format = "%Y-%m-%d")
cc_quarters$Quarter = format(cc_quarters$Quarter, format = "%Y.%q")
cc_quarters <- cc_quarters %>%
    select(-matches('id'))
cc_quarters <- cc_quarters %>%
    select(-matches('species'))
cc_quarters <- cc_quarters %>%
    select(-matches('date'))
cc_quarters_mean = aggregate(.~Quarter, cc_quarters, mean)
cc_quarters_mean_long = melt(cc_quarters_mean, id="Quarter", variable.name = "Source", value.name = "Proportion")
cc_quarters_mean_long$Quarter = as.numeric(cc_quarters_mean_long$Quarter)
cc_quarters_order = order(ordered(cc_quarters_mean_long$Source, levels=cc_overall_summary$Source), decreasing = TRUE)
cc_quarters_mean_long = cc_quarters_mean_long[cc_quarters_order,]
cc_quarters_mean_long$Source = factor(cc_quarters_mean_long$Source, levels=unique(as.character(cc_quarters_mean_long$Source)))

# Generate area plot
cc_quarters_plot = ggplot(cc_quarters_mean_long, aes(x=Quarter, y=Proportion, fill=Source)) + geom_area() + set_fill_colours + labs(x="Time", y="Proportion of isolates")

# Generate bar graph
# Basic plot - needs changes to x-axis
# cc_quarters_bar_plot = ggplot(cc_quarters_mean_long, aes(x=Quarter, y=Proportion, fill=Source)) + geom_bar(stat="identity") + set_fill_colours

# Count C. coli isolates per quarter
cc_quarters_count = aggregate(.~Quarter, cc_quarters, FUN= length)
colnames(cc_quarters_count)[2] <- "Count"
retain = c("Quarter","Count")
cc_quarters_count = cc_quarters_count[,retain]

# Plot C. jejuni and C. coli quarterly breakdown in single figure
quarterly_plot = plot_grid(cj_quarters_plot, cc_quarters_plot, labels=c("A", "B"), nrow = 2, align = "v")
# Save figure to output directory
ggsave("Figure6.svg", plot=yearly_plot, device="svg", width=210, height=148, units="mm", dpi=300)

# Generate combined summary table of number of isolates per quarter
overall_quarters_table = full_join(cj_quarters_count, cc_quarters_count, by = "Quarter")
colnames(overall_quarters_table) <- c("Year.Quarter", "C. jejuni", "C. coli")

# Plot number of isolates per year
# Prepare data
overall_quarters = overall_quarters_table
# Convert years to numeric for graphing
overall_quarters$`Year.Quarter` = as.numeric(overall_quarters$`Year.Quarter`)
# Convert data to long form
overall_quarters_long = melt(overall_quarters, id = "Year.Quarter", variable.name = "Species", value.name = "Count")
overall_quarter_counts_plot = ggplot(data=overall_quarters_long, aes(x = `Year.Quarter`, y = Count, group = Species)) + geom_line(aes(linetype=Species)) + labs(x="Time", y="Number of isolates") + theme_grey(base_size = 14) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("Figure5.svg", plot=overall_year_counts_plot, device="svg", width=210, height=148, units="mm", dpi=300)

### Report generation ###
## Create document and title ##
doc = docx(title="Report_Skeleton")
doc = addTitle(doc , 'Source attribution of campylobacteriosis isolates', level=1)

## Introduction ##
doc = addTitle(doc, 'Introduction', level=2)
doc = addParagraph(doc, 'This is an automated report summarizing the population genetic assignment of isolates to their source based on population genetic analysis using Structure or iSource software and reference datasets selected by the user. It was developed using funding from the Food Standards Agency (FS101013) and the NIHR Health Protection Research Unit in Gastrointestinal Infections. These analyses can be repeated on any data placed on the PubMLST database.', stylename = 'Normal')

## Methods ##
doc = addTitle(doc , 'Methods', level=2)
# Describe datasets #
doc = addTitle(doc, 'Datasets', level=3)
doc = addTitle(doc, 'Reference isolates', level=4)
doc = addParagraph(doc, 'Add your description here.', stylename = 'Normal')
doc = addTitle(doc, 'Human disease isolates', level=4)
doc = addParagraph(doc, 'Add your description here.', stylename = 'Normal')
# Describe attribution
doc = addTitle(doc, 'Source attribution', level=3)
doc = addParagraph(doc, 'Unless altered by the user, isolates were assigned to putative host sources using STRUCTURE or iSource, based on analysis of MLST data. The algorithms were run separately for C. jejuni and C. coli. For STRUCTURE analyses, the no-admixture model was used and the program was run using a burn-in period of 1,000 cycles followed by 10,000 iterations.  For iSource, the Asymmetric Island model was used and the program was run for 10,000 iterations without thinning, using a symmetric Dirichlet prior.\n\rThe probabilities for attribution to each source are summed as an arithmetic mean across the cases of human infection (e.g. Figure 1), presented for individual isolates (e.g. Figure 2), and summed over time (e.g. Figure 3 onwards).\n\rThe attribution carries the assumption that all isolates came from one of the sources in the analysis.  Isolates from sources not represented will be assigned to sources represented in the reference sets according to their genetic similarity.  The results presented do not include any adjustment for bias that may occur in population genetic attribution with the included reference isolates. If results from a validation study estimating bias are available, results should be considered in the light of that.', stylename = 'Normal')
doc <- addPageBreak(doc)

## Results ##
doc = addTitle(doc, 'Results', level=2)

# Data cleaning
doc = addTitle(doc, 'Post-attribution data cleaning', level=3)
doc = addParagraph(doc, sprintf('A total of %s C. jejuni and %s C. coli isolates were attributed to animal and/or environmental sources; however, %s (%s %%) C. jejuni and %s (%s %%) C. coli isolates without dates of isolation/laboratory receipt dates were excluded from date-based analyses.', no_ancs_cj, no_ancs_cc, cj_missing_dates, cj_missing_dates_prop, cc_missing_dates, cc_missing_dates_prop), stylename='Normal')

# Overall summary
doc = addTitle(doc, 'Overall summary', level=3)
doc = addParagraph(doc, 'Add text summary here.', stylename = 'Normal')
doc = addParagraph(doc, 'Tabulated data for Figure 1 are provided in the Appendices.', stylename = 'Normal')
# Overall summary plot
doc = addPlot(doc , fun=print, x=overall_plot, width=6, height=4)
doc = addParagraph(doc, sprintf('Figure 1. Estimated proportion of human disease isolates attributed to putative sources.  Probabilistic assignment of (A) %s C. jejuni collected between %s and %s, and (B) %s C. coli collected between %s and %s.', no_ancs_cj, min_date_cj, max_date_cj, no_ancs_cc, min_date_cc, max_date_cc), stylename='Normal')
# Overall ancestries plot
doc = addPlot(doc , fun=print, x=overall_ind_ancs_plot, width=6, height=5)
doc = addParagraph(doc, sprintf('Figure 2. Source probabilities for individual human disease isolates. Probabilistic assignment of (A) %s C. jejuni and (B) %s C. coli isolates. Isolates are represented as vertical bars coloured according to the estimated probability for each source as shown in the legends. Isolates are ordered horizontally to aid visualisation, first by most likely source and then by decreasing probability within each source.', no_ancs_cj, no_ancs_cc), stylename='Normal')

doc <- addPageBreak(doc)

# Annual breakdown section
doc = addTitle(doc, 'Annual breakdown', level=3)
doc = addParagraph(doc, 'Add text summary here.', stylename = 'Normal')
doc = addParagraph(doc, 'Tabulated data for all figures in this section are provided in the Appendices.', stylename = 'Normal')
# Annual isolate count plot
doc = addPlot(doc , fun=print, x=overall_year_counts_plot, width=5, height=3, par.properties = parProperties(text.align = "left"))
doc = addParagraph(doc, 'Figure 3. Number of C. jejuni and C. coli isolates per year.', stylename='Normal')

# Annual attribution plot
doc = addPlot(doc , fun=print, x=yearly_plot)
doc = addParagraph(doc, sprintf('Figure 4. Estimated proportion of human disease isolates attributed to putative sources over time. Proportion of (A) %s C. jejuni collected between %s and %s, and (B) %s C. coli collected between %s and %s. Bars are ordered from major (bottom) to minor (top) sources to aid visualisation.', no_cj, min_date_cj,max_date_cj, no_cc, min_date_cc, max_date_cc), stylename='Normal')

doc <- addPageBreak(doc)

# Quarterly breakdown section
doc = addTitle(doc, 'Quarterly breakdown', level=3)
doc = addParagraph(doc, 'Add text summary here.', stylename = 'Normal')
doc = addParagraph(doc, 'Tabulated data for all figures in this section are provided in the Appendices.', stylename = 'Normal')
# Quarterly isolate count plot
doc = addPlot(doc , fun=print, x=overall_quarter_counts_plot, width=5, height=3, par.properties = parProperties(text.align = "left"))
doc = addParagraph(doc, 'Figure 5. Number of C. jejuni and C. coli isolates per quarter.', stylename='Normal')

# Quarterly isolate count plot
doc = addPlot(doc , fun=print, x=quarterly_plot)
doc = addParagraph(doc, sprintf('Figure 6. Estimated proportion of human disease isolates attributed to putative sources over calendar quarters. Proportion of (A) %s C. jejuni collected between %s and %s, and (B) %s C. coli collected between %s and %s. Bars are ordered from major (bottom) to minor (top) sources to aid visualisation.', no_cj, min_date_cj,max_date_cj, no_cc, min_date_cc, max_date_cc), stylename='Normal')

doc <- addPageBreak(doc)

## Appendices ##
doc = addTitle(doc, 'Appendices', level=2)
# Overall summary tables
doc = addTitle(doc, 'Overall summary', level=3)
doc = addParagraph(doc, sprintf('Table A1. Estimated proportion of %s C. jejuni and %s C. coli human disease isolates attributed to putative sources.', no_cj, no_cc), stylename='Normal')
doc = addFlexTable(doc, vanilla.table(overall_table))
doc = addParagraph(doc, '\r\n', stylename=)

doc <- addPageBreak(doc)

# Annual breakdown tables
doc = addTitle(doc, 'Annual breakdown', level=3)
# Breakdown of number of isolates per year
doc = addParagraph(doc, 'Table A2. Number of human disease isolates per year', stylename='Normal')
doc = addFlexTable(doc, vanilla.table(overall_years_table))
doc = addParagraph(doc, '\r\n', stylename=)

# Breakdown of attribution per year
# C. jejuni
# Get significant figures
cj_years_mean[,-1] = signif(cj_years_mean[,-1], 3)
# Display table
doc = addParagraph(doc, sprintf('Table A3. Proportion of %s C. jejuni isolates attributed to putative sources per year between %s and %s', no_cj, min_date_cj, max_date_cj), stylename='Normal')
doc = addFlexTable(doc, vanilla.table(cj_years_mean))
doc = addParagraph(doc, '\r\n', stylename=)
# C. coli
# Get significant figures
cc_years_mean[,-1] = signif(cc_years_mean[,-1], 3)
# Display table
doc = addParagraph(doc, sprintf('Table A4. Proportion of %s C. coli isolates attributed to putative sources per year between %s and %s.', no_cc, min_date_cc, max_date_cc), stylename='Normal')
doc = addFlexTable(doc, vanilla.table(cc_years_mean))
doc = addParagraph(doc, '\r\n', stylename=)

# Quarterly breakdown tables
doc = addTitle(doc, 'Quarterly breakdown', level=3)
# Breakdown of number of isolates per quarter
doc = addParagraph(doc, 'Table A5. Number of human disease isolates per quarter', stylename='Normal')
doc = addFlexTable(doc, vanilla.table(overall_quarters_table))
doc = addParagraph(doc, '\r\n', stylename=)

doc <- addPageBreak(doc)

# Breakdown of attribution per quarter
# C. jejuni
# Get significant figures
cj_quarters_mean[,-1] = signif(cj_quarters_mean[,-1], 3)
# Display table
doc = addParagraph(doc, sprintf('Table A3. Proportion of %s C. jejuni isolates attributed to putative sources per quarter between %s and %s', no_cj, min_date_cj, max_date_cj), stylename='Normal')
doc = addFlexTable(doc, vanilla.table(cj_quarters_mean))
doc = addParagraph(doc, '\r\n', stylename=)
# C. coli
# Get significant figures
cc_quarters_mean[,-1] = signif(cc_quarters_mean[,-1], 3)
# Display table
doc = addParagraph(doc, sprintf('Table A4. Proportion of %s C. coli isolates attributed to putative sources per quarter between %s and %s.', no_cc, min_date_cc, max_date_cc), stylename='Normal')
doc = addFlexTable(doc, vanilla.table(cc_quarters_mean))
doc = addParagraph(doc, '\r\n', stylename=)

writeDoc(doc, "Report_Skeleton.docx")
