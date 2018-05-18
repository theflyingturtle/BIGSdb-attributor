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
ancs_cj = read.csv("/Users/MelissaJvR/Desktop/FSA_Final/RInputFiles/FSA_Cj_inferred-ancestry.csv", header=TRUE, check.names=TRUE)
ancs_cj[ancs_cj==""] <- NA
ancs_cj$date <- as.Date(ancs_cj$date)
cj = ancs_cj[!is.na(ancs_cj$date),]
# Repeat for C. coli
ancs_cc = read.csv("/Users/MelissaJvR/Desktop/FSA_Final/RInputFiles/FSA_Cc_inferred-ancestry.csv", header=TRUE, check.names=TRUE)
ancs_cc[ancs_cc==""] <- NA
ancs_cc$date <- as.Date(ancs_cc$date)
cc = ancs_cc[!is.na(ancs_cc$date),]

### Directory setup ###
# Get current directory and name output directory
# NOTE: the script will fail here if the output directory already exists.  Remove/rename the existing directory and try again.  If you've deleted the file, make sure that your current working directory isn't set to Trash!  To do this, enter "getwd()" and then run the command "setwd(PATH/TO/DESIRED/DIRECTORY)" to move to the right directory.
current_dir = sprintf("%s",getwd())
output_dir = "AttributionReportFSA"
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
all_cj_oxc = sum(ancs_cj$site == 'Oxfordshire')
all_cj_nwc = sum(ancs_cj$site == 'Newcastle')
dated_cj_oxc = sum(cj$site == 'Oxfordshire')
dated_cj_nwc = sum(cj$site == 'Newcastle')
cj_oxc_missing_dates = all_cj_oxc - dated_cj_oxc 
cj_nwx_missing_dates = all_cj_nwc - dated_cj_nwc
# C. coli
no_ancs_cc = nrow(ancs_cc)
no_cc = nrow(cc)
cc_missing_dates = no_ancs_cc - no_cc
cc_missing_dates_prop = round((cc_missing_dates/no_ancs_cc*100), digits=2)
all_cc_oxc = sum(ancs_cc$site == 'Oxfordshire')
all_cc_nwc = sum(ancs_cc$site == 'Newcastle')
dated_cc_oxc = sum(cc$site == 'Oxfordshire')
dated_cc_nwc = sum(cc$site == 'Newcastle')
cc_oxc_missing_dates = all_cc_oxc - dated_cc_oxc 
cc_nwx_missing_dates = all_cc_nwc - dated_cc_nwc
## Max and min dates
# C. jejuni
max_date_cj = format(max(as.Date(cj$date, format="%d/%m/%Y")), "%B %Y")
min_date_cj = format(min(as.Date(cj$date, format="%d/%m/%Y")), "%B %Y")
# C. coli
max_date_cc = format(max(as.Date(cc$date, format="%d/%m/%Y")), "%B %Y")
min_date_cc = format(min(as.Date(cc$date, format="%d/%m/%Y")), "%B %Y")

## Overall summaries ##

# Summary plots and tables for C. jejuni

# Site-by-site summaries
# Get only source columns for overall summaries
cj_sources <- ancs_cj %>%
	select(-matches('id'))
cj_sources <- cj_sources %>%
	select(-matches('date'))
cj_sources <- cj_sources %>%
	select(-matches('species'))
# Get proportions for each source, site by site and convert to long form
cj_anc_sites = (aggregate(.~site, cj_sources, mean))
cj_anc_sites_long = melt(cj_anc_sites, id="site", variable.name="Source", value.name="Proportion")
# Create site-by-site table
#cj_site_names = cj_anc_sites$site
#cj_site_summary = as.data.frame(t(cj_anc_sites[,-1]))
#colnames(cj_site_summary) = cj_site_names
# Transform data
cj_sbs_summary = setNames(data.frame(t(cj_anc_sites[,-1])), cj_anc_sites[,1])

# Generate grouped bar graph showing site-by-site proportions
cj_sbs_overall_plot = ggplot(cj_anc_sites_long, aes(x = reorder(Source, -Proportion), y = Proportion)) + geom_bar(aes(fill = site), stat="identity", position = position_dodge(width=0.8), width = 0.8) + scale_fill_grey() + theme_grey(base_size = 14) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + labs(x="Sources", y="Proportion") + guides(fill=guide_legend(title=NULL)) +
    theme(legend.position="bottom", legend.direction = "horizontal")

# Overall summary
# Drop sites to get overall proportions
cj_overall <- cj_sources %>%
	select(-matches('site'))
# Get proportions for each source
cj_ancs_long = melt(cj_overall)
cj_overall_summary = melt(tapply(cj_ancs_long$value, cj_ancs_long$variable, mean), varnames="Source", value.name="Proportion")
# Set order: by source, decreasing proportion
cj_overall_summary = cj_overall_summary[order(-cj_overall_summary$Proportion),]
cj_overall_summary$Source = factor(cj_overall_summary$Source, levels=unique(as.character(cj_overall_summary$Source)))
# Generate bar graph showing overall proportions
cj_overall_plot = ggplot(cj_overall_summary) + geom_bar(aes(x=Source, y=Proportion), stat="identity", fill="black") + theme_grey(base_size = 14) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ylab("Proportion")

# Repeat for C. coli
# Site-by-site summaries
cc_sources <- ancs_cc %>%
	select(-matches('id'))
cc_sources <- cc_sources %>%
	select(-matches('date'))
cc_sources <- cc_sources %>%
	select(-matches('species'))
cc_anc_sites = (aggregate(.~site, cc_sources, mean))
cc_anc_sites_long = melt(cc_anc_sites, id="site", variable.name="Source", value.name="Proportion")
# Site-by-site summary
cc_sbs_summary = setNames(data.frame(t(cc_anc_sites[,-1])), cc_anc_sites[,1])
# Site-by-site plot
cc_sbs_overall_plot = ggplot(cc_anc_sites_long, aes(x = reorder(Source, -Proportion), y = Proportion)) + geom_bar(aes(fill = site), stat="identity", position = position_dodge(width=0.8), width = 0.8) + scale_fill_grey() + theme_grey(base_size = 14) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + labs(x="Sources", y="Proportion") + guides(fill=guide_legend(title=NULL)) +
    theme(legend.position="bottom", legend.direction = "horizontal")

# Overall summary
cc_overall <- cc_sources %>%
	select(-matches('site'))
cc_ancs_long = melt(cc_overall)
cc_overall_summary = melt(tapply(cc_ancs_long$value, cc_ancs_long$variable, mean), varnames="Source", value.name="Proportion")
cc_overall_summary = cc_overall_summary[order(-cc_overall_summary$Proportion),]
cc_overall_summary$Source = factor(cc_overall_summary$Source, levels=unique(as.character(cc_overall_summary$Source)))
cc_overall_plot = ggplot(cc_overall_summary) + geom_bar(aes(x=Source, y=Proportion), stat="identity", fill="black") + theme_grey(base_size = 14) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ylab("Proportion")

# Plot C. jejuni and C. coli graphs on same axis
# Overall plots
overall_plot = plot_grid(cj_overall_plot, cc_overall_plot, labels=c("A", "B"), align="h")
# Save figure to output directory
ggsave("Figure1.svg", plot=overall_plot, device="svg", width=210, height=148, units="mm", dpi=300)

# Site-by-site overall plots
overall_sites_plot = plot_grid(cj_sbs_overall_plot, cc_sbs_overall_plot, labels=c("A", "B"), align="h")
ggsave("Figure2.svg", plot=overall_sites_plot, device="svg", width=210, height=148, units="mm", dpi=300)

# Generate combined overall summary table
overall_table = full_join(cj_overall_summary, cc_overall_summary, by = "Source")
colnames(overall_table) <- c("Source", "C. jejuni", "C. coli")
# Get significant figures
overall_table$`C. jejuni` = signif(overall_table$`C. jejuni`, 3)
overall_table$`C. coli` = signif(overall_table$`C. coli`, 3)

# Generate site-by-site summary table
sbs_table = merge(cj_sbs_summary, cc_sbs_summary, by.x="row.names", by.y="row.names", all=TRUE, suffixes = c(" (Cj)"," (Cc)"))
# Get significant figures
sbs_table[,-1] = signif(sbs_table[,-1], 3)
# Correct header
colnames(sbs_table)[1] <- "Sources"

# Generate individual ancestries plots
# C. jejuni
# Retrieve sources dataframe
# Add "poprank" column that indicates the most likely source (in the case of a tie, 'which' will pick the first column)
cj_sources$poprank = colnames(cj_sources)[apply(cj_sources,1,which.max)]
# Separate data into Oxfordshire and Newcastle
cj_sources_oxc = cj_sources[cj_sources$site == 'Oxfordshire',]
cj_sources_nwc = cj_sources[cj_sources$site == 'Newcastle',]
# Drop site column
cj_sources_oxc <- cj_sources_oxc %>%
	select(-matches('site'))
cj_sources_nwc <- cj_sources_nwc %>%
	select(-matches('site'))
# Add "max prob" column giving max value to order on later
cj_sources_oxc[, "maxprob"] = apply(within(cj_sources_oxc, rm(poprank)), 1, max)
cj_sources_nwc[, "maxprob"] = apply(within(cj_sources_nwc, rm(poprank)), 1, max)
# Reorder the dataframe from most to least common overall source, first by most likely source, and within each source 
cj_order_oxc = order(ordered(cj_sources_oxc$poprank, levels=cj_overall_summary$Source), -cj_sources_oxc$maxprob)
cj_sources_oxc = cj_sources_oxc[cj_order_oxc,]
cj_order_nwc = order(ordered(cj_sources_nwc$poprank, levels=cj_overall_summary$Source), -cj_sources_nwc$maxprob)
cj_sources_nwc = cj_sources_nwc[cj_order_nwc,]

# Add rank column after ordering to give order to ggplot
cj_sources_oxc$rank <- seq.int(nrow(cj_sources_oxc))
cj_sources_nwc$rank <- seq.int(nrow(cj_sources_nwc))
# Melt data to long form
cj_ind_ancs_long_oxc = melt(cj_sources_oxc, id=c("rank", "poprank", "maxprob"), variable.name = "Source", value.name = "Proportion")
cj_ind_ancs_long_nwc = melt(cj_sources_nwc, id=c("rank", "poprank", "maxprob"), variable.name = "Source", value.name = "Proportion")

# Generate individual ancestries plots
cj_ind_ancs_plot_oxc = ggplot(cj_ind_ancs_long_oxc, aes(x=rank, y=Proportion, fill=Source, width=1)) + geom_bar(stat="identity") + labs(x="Human disease isolates", y="Source probability") + set_fill_colours
cj_ind_ancs_plot_nwc = ggplot(cj_ind_ancs_long_nwc, aes(x=rank, y=Proportion, fill=Source, width=1)) + geom_bar(stat="identity") + labs(x="Human disease isolates", y="Source probability") + set_fill_colours

# Repeat for C. coli
# Retrieve data and add extra columns for sorting
cc_sources$poprank = colnames(cc_sources)[apply(cc_sources,1,which.max)]
cc_sources_oxc = cc_sources[cc_sources$site == 'Oxfordshire',]
cc_sources_nwc = cc_sources[cc_sources$site == 'Newcastle',]
cc_sources_oxc <- cc_sources_oxc %>%
	select(-matches('site'))
cc_sources_nwc <- cc_sources_nwc %>%
	select(-matches('site'))
cc_sources_oxc[, "maxprob"] = apply(within(cc_sources_oxc, rm(poprank)), 1, max)
cc_sources_nwc[, "maxprob"] = apply(within(cc_sources_nwc, rm(poprank)), 1, max)
cc_order_oxc = order(ordered(cc_sources_oxc$poprank, levels=cc_overall_summary$Source), -cc_sources_oxc$maxprob)
cc_sources_oxc = cc_sources_oxc[cc_order_oxc,]
cc_order_nwc = order(ordered(cc_sources_nwc$poprank, levels=cc_overall_summary$Source), -cc_sources_nwc$maxprob)
cc_sources_nwc = cc_sources_nwc[cc_order_nwc,]
cc_sources_oxc$rank <- seq.int(nrow(cc_sources_oxc))
cc_sources_nwc$rank <- seq.int(nrow(cc_sources_nwc))
# Melt data to long form
cc_ind_ancs_long_oxc = melt(cc_sources_oxc, id=c("rank", "poprank", "maxprob"), variable.name = "Source", value.name = "Proportion")
cc_ind_ancs_long_nwc = melt(cc_sources_nwc, id=c("rank", "poprank", "maxprob"), variable.name = "Source", value.name = "Proportion")

# Generate individual ancestries plots
cc_ind_ancs_plot_oxc = ggplot(cc_ind_ancs_long_oxc, aes(x=rank, y=Proportion, fill=Source, width=1)) + geom_bar(stat="identity") + labs(x="Human disease isolates", y="Source probability") + set_fill_colours
cc_ind_ancs_plot_nwc = ggplot(cc_ind_ancs_long_nwc, aes(x=rank, y=Proportion, fill=Source, width=1)) + geom_bar(stat="identity") + labs(x="Human disease isolates", y="Source probability") + set_fill_colours

# Plot C. jejuni and C. coli individual ancestry graphs in single figure
overall_ind_ancs_plot = plot_grid(cj_ind_ancs_plot_nwc, cj_ind_ancs_plot_oxc, cc_ind_ancs_plot_nwc, cc_ind_ancs_plot_oxc, labels = c("A", "B", "C", "D"), ncol = 1)
# Save figure to output directory
ggsave("Figure3.svg", plot=overall_ind_ancs_plot, device="svg", width=210, height=148, units="mm", dpi=300)

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
# Separate data into Oxfordshire and Newcastle
cj_years_oxc = cj_years[cj_years$site == 'Oxfordshire',]
cj_years_nwc = cj_years[cj_years$site == 'Newcastle',]
# Drop site column
cj_years_oxc <- cj_years_oxc %>%
	select(-matches('site'))
cj_years_nwc <- cj_years_nwc %>%
	select(-matches('site'))

# Get averages by year
cj_years_oxc_mean = aggregate(.~Year, cj_years_oxc, mean)
cj_years_nwc_mean = aggregate(.~Year, cj_years_nwc, mean)
# Convert to long form
cj_years_oxc_mean_long = melt(cj_years_oxc_mean, id="Year", variable.name = "Source", value.name = "Proportion")
cj_years_nwc_mean_long = melt(cj_years_nwc_mean, id="Year", variable.name = "Source", value.name = "Proportion")
# Read year as numeric to get proper x-axis labels in the area plot
cj_years_oxc_mean_long$Year = as.numeric(cj_years_oxc_mean_long$Year)
cj_years_nwc_mean_long$Year = as.numeric(cj_years_nwc_mean_long$Year)

# Re-order the dataframe so that bars will be stacked from major (bottom) to minor (top) sources
cj_years_oxc_order = order(ordered(cj_years_oxc_mean_long$Source, levels=cj_overall_summary$Source), decreasing = TRUE)
cj_years_oxc_mean_long = cj_years_oxc_mean_long[cj_years_oxc_order,]
cj_years_oxc_mean_long$Source = factor(cj_years_oxc_mean_long$Source, levels=unique(as.character(cj_years_oxc_mean_long$Source)))
cj_years_nwc_order = order(ordered(cj_years_nwc_mean_long$Source, levels=cj_overall_summary$Source), decreasing = TRUE)
cj_years_nwc_mean_long = cj_years_nwc_mean_long[cj_years_nwc_order,]
cj_years_nwc_mean_long$Source = factor(cj_years_nwc_mean_long$Source, levels=unique(as.character(cj_years_nwc_mean_long$Source)))

# Generate area plot and manipulate x-axis to display years correctly
cj_years_oxc_plot = ggplot(cj_years_oxc_mean_long, aes(x=Year, y=Proportion, fill=Source)) + geom_area() + scale_x_continuous(breaks=as.numeric(unique(cj_years_oxc_mean_long$Year)), labels=c(as.character(unique(cj_years_oxc_mean_long$Year)))) + labs(x="Year", y="Proportion of isolates") + set_fill_colours + theme(axis.text.x = element_text(angle = 45, hjust = 1))
cj_years_nwc_plot = ggplot(cj_years_nwc_mean_long, aes(x=Year, y=Proportion, fill=Source)) + geom_area() + scale_x_continuous(breaks=as.numeric(unique(cj_years_nwc_mean_long$Year)), labels=c(as.character(unique(cj_years_nwc_mean_long$Year)))) + labs(x="Year", y="Proportion of isolates") + set_fill_colours + theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Repeat for C. coli
cc_years = cc
cc_years$Year = format(cc_years$date,format="%Y")
# Drop unnecessary columns
cc_years <- cc_years %>%
    select(-matches('id'))
cc_years <- cc_years %>%
    select(-matches('species'))
cc_years <- cc_years %>%
    select(-matches('date'))
# Separate data into Oxfordshire and Newcastle
cc_years_oxc = cc_years[cc_years$site == 'Oxfordshire',]
cc_years_nwc = cc_years[cc_years$site == 'Newcastle',]
# Drop site column
cc_years_oxc <- cc_years_oxc %>%
	select(-matches('site'))
cc_years_nwc <- cc_years_nwc %>%
	select(-matches('site'))

# Get averages by year
cc_years_oxc_mean = aggregate(.~Year, cc_years_oxc, mean)
cc_years_nwc_mean = aggregate(.~Year, cc_years_nwc, mean)
# Convert to long form
cc_years_oxc_mean_long = melt(cc_years_oxc_mean, id="Year", variable.name = "Source", value.name = "Proportion")
cc_years_nwc_mean_long = melt(cc_years_nwc_mean, id="Year", variable.name = "Source", value.name = "Proportion")
# Read year as numeric to get proper x-axis labels in the area plot
cc_years_oxc_mean_long$Year = as.numeric(cc_years_oxc_mean_long$Year)
cc_years_nwc_mean_long$Year = as.numeric(cc_years_nwc_mean_long$Year)

# Re-order the dataframe so that bars will be stacked from major (bottom) to minor (top) sources
cc_years_oxc_order = order(ordered(cc_years_oxc_mean_long$Source, levels=cc_overall_summary$Source), decreasing = TRUE)
cc_years_oxc_mean_long = cc_years_oxc_mean_long[cc_years_oxc_order,]
cc_years_oxc_mean_long$Source = factor(cc_years_oxc_mean_long$Source, levels=unique(as.character(cc_years_oxc_mean_long$Source)))
cc_years_nwc_order = order(ordered(cc_years_nwc_mean_long$Source, levels=cc_overall_summary$Source), decreasing = TRUE)
cc_years_nwc_mean_long = cc_years_nwc_mean_long[cc_years_nwc_order,]
cc_years_nwc_mean_long$Source = factor(cc_years_nwc_mean_long$Source, levels=unique(as.character(cc_years_nwc_mean_long$Source)))

# Generate area plot and manipulate x-axis to display years correctly
cc_years_oxc_plot = ggplot(cc_years_oxc_mean_long, aes(x=Year, y=Proportion, fill=Source)) + geom_area() + scale_x_continuous(breaks=as.numeric(unique(cc_years_oxc_mean_long$Year)), labels=c(as.character(unique(cc_years_oxc_mean_long$Year)))) + labs(x="Year", y="Proportion of isolates") + set_fill_colours + theme(axis.text.x = element_text(angle = 45, hjust = 1))
cc_years_nwc_plot = ggplot(cc_years_nwc_mean_long, aes(x=Year, y=Proportion, fill=Source)) + geom_area() + scale_x_continuous(breaks=as.numeric(unique(cc_years_nwc_mean_long$Year)), labels=c(as.character(unique(cc_years_nwc_mean_long$Year)))) + labs(x="Year", y="Proportion of isolates") + set_fill_colours + theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot C. jejuni and C. coli annual breakdown in single figure
yearly_plot = plot_grid(cj_years_nwc_plot, cj_years_oxc_plot, cc_years_nwc_plot, cc_years_oxc_plot,  labels=c("A", "B", "C", "D"), ncol = 1)
# Save figure to output directory
ggsave("Figure4.svg", plot=yearly_plot, device="svg", width=210, height=148, units="mm", dpi=300)

# Generate combined summary tables of yearly attribution
cj_years_mean = merge(cj_years_nwc_mean, cj_years_oxc_mean, by.x="Year", by.y="Year", all=TRUE, suffixes = c(" (NWC)", " (OXC)"))
cc_years_mean = merge(cc_years_nwc_mean, cc_years_oxc_mean, by.x="Year", by.y="Year", all=TRUE, suffixes = c(" (NWC)", " (OXC)"))

# Generate counts per year
# Count C. jejuni isolates per year
retain = c("Year","Count")
cj_years_oxc_count = aggregate(.~Year, cj_years_oxc, FUN= length)
colnames(cj_years_oxc_count)[2] <- "Count"
cj_years_oxc_count = cc_years_oxc_count[,retain]
cj_years_nwc_count = aggregate(.~Year, cj_years_nwc, FUN= length)
colnames(cj_years_nwc_count)[2] <- "Count"
cj_years_nwc_count = cc_years_nwc_count[,retain]
# Repeat for C. coli
cc_years_oxc_count = aggregate(.~Year, cc_years_oxc, FUN= length)
colnames(cc_years_oxc_count)[2] <- "Count"
cc_years_oxc_count = cc_years_oxc_count[,retain]
cc_years_nwc_count = aggregate(.~Year, cc_years_nwc, FUN= length)
colnames(cc_years_nwc_count)[2] <- "Count"
cc_years_nwc_count = cc_years_nwc_count[,retain]



### Report generation ###
## Create document and title ##
doc = docx(title="FSA_Report_Skeleton")
doc = addTitle(doc , 'FSA-101013: Source attribution of campylobacteriosis isolates from Oxfordshire and Newcastle/North Tyneside', level=1)

## Introduction ##
doc = addTitle(doc, 'Introduction', level=2)
doc = addParagraph(doc, 'Add your text here.', stylename = 'Normal')

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
doc = addParagraph(doc, 'Add your description here.', stylename = 'Normal')
# Previous descriptions used in FSA reports:
# STRUCTURE. Human disease isolates were assigned to putative host sources using the no-admixture model in STRUCTURE, based on analysis of MLST data.  STRUCTURE was run separately for C. coli and C. jejuni. The program was run using a burn-in period of 1,000 cycles followed by 10,000 iterations.
# iSOURCE. Human disease isolates were assigned to putative host sources using the Asymmetric Island model implemented in iSource, based on analysis of MLST data.  The algorithm was run separately for C. coli and C. jejuni. The program was run for 10,000 iterations without thinning, using a symmetric Dirichlet prior.
doc <- addPageBreak(doc)

## Results ##
doc = addTitle(doc, 'Results', level=2)

# Data cleaning
doc = addTitle(doc, 'Post-attribution data cleaning', level=3)
doc = addParagraph(doc, sprintf('A total of %s C. jejuni and %s C. coli isolates were attributed to animal and/or environmental sources; however, isolates without dates of isolation/laboratory receipt dates were excluded from date-based analyses presented below.  Following data cleaning, %s and %s C. jejuni from Newcastle/North Tyneside and Oxfordshire, respectively, were excluded, as were %s and %s C. coli from Newcastle/North Tyneside and Oxfordshire.', no_ancs_cj, no_ancs_cc, cj_nwx_missing_dates, cj_oxc_missing_dates, cc_nwx_missing_dates, cc_oxc_missing_dates), stylename='Normal')

# Overall summary
doc = addTitle(doc, 'Overall summary', level=3)
doc = addParagraph(doc, 'Add text summary here.', stylename = 'Normal')
doc = addParagraph(doc, 'Tabulated data for Figures 1 and 2 are provided in the Appendices.', stylename = 'Normal')
# Overall summary plot
# Overall summary plot
doc = addPlot(doc , fun=print, x=overall_plot, width=6, height=4)
doc = addParagraph(doc, sprintf('Figure 1. Estimated proportion of human disease isolates attributed to putative sources.  Probabilistic assignment of (A) %s C. jejuni collected between %s and %s, and (B) %s C. coli collected between %s and %s.', no_ancs_cj, min_date_cj, max_date_cj, no_ancs_cc, min_date_cc, max_date_cc), stylename='Normal')

# Overall site-by-site plot
doc = addPlot(doc, fun=print, x=overall_sites_plot, width=6, height=4)
doc = addParagraph(doc, sprintf('Figure 2. Estimated proportion of human disease isolates from Oxfordshire and Newcastle/North Tyneside attributed to putative sources. Probabilistic assignment of (A) %s and %s C. jejuni collected in Newcastle/North Tyneside and Oxfordshire, respectively, between %s and %s, and (B) %s and %s C. coli collected in Newcastle/North Tyneside and Oxfordshire, respectively, between %s and %s.', all_cj_nwc, all_cj_oxc, min_date_cj, max_date_cj, all_cc_nwc, all_cc_oxc, min_date_cc, max_date_cc), stylename='Normal')

# Overall individual ancestries plot
doc = addPlot(doc , fun=print, x=overall_ind_ancs_plot, width=6.5, height=10)
doc = addParagraph(doc, sprintf('Figure 3. Source probabilities for individual human disease isolates. Probabilistic assignment of (A, B) %s and %s C. jejuni isolates from Newcastle/North Tyneside and Oxfordshire, and (C, D) %s and %s C. coli isolates from Newcastle/North Tyneside and Oxfordshire. Isolates are represented as vertical bars coloured according to the estimated probability for each source as shown in the legends. Isolates are ordered horizontally to aid visualisation, first by most likely source and then by decreasing probability within each source.', all_cj_nwc, all_cj_oxc, all_cc_nwc, all_cc_oxc), stylename='Normal')

doc <- addPageBreak(doc)

# Annual breakdown section
doc = addTitle(doc, 'Annual breakdown', level=3)
doc = addParagraph(doc, 'Add text summary here.', stylename = 'Normal')
doc = addParagraph(doc, 'Tabulated data for all figures in this section are provided in the Appendices.', stylename = 'Normal')

# Annual attribution plot
doc = addPlot(doc , fun=print, x=yearly_plot, width=6.5, height=10)
doc = addParagraph(doc, sprintf('Figure 4. Estimated proportion of human disease isolates attributed to putative sources over time. Proportion of (A, B) %s and %s C. jejuni isolates from Newcastle/North Tyneside and Oxfordshire, and (C, D) %s and %s C. coli isolates from Newcastle/North Tyneside and Oxfordshire.  Bars are ordered from major (bottom) to minor (top) sources based on the overall proportions shown in Figure 1.', dated_cj_nwc, dated_cj_oxc, dated_cc_nwc, dated_cc_oxc), stylename='Normal')

doc <- addPageBreak(doc)

## Appendices ##
doc = addTitle(doc, 'Appendices', level=2)
# Overall summary
# Overall summary table
doc = addTitle(doc, 'Overall summary', level=3)
doc = addParagraph(doc, sprintf('Table A1. Estimated proportion of %s C. jejuni and %s C. coli human disease isolates attributed to putative sources', no_cj, no_cc), stylename='Normal')
doc = addFlexTable(doc, vanilla.table(overall_table))
doc = addParagraph(doc, '\r\n', stylename=)
# Overall site-by-site table
doc = addParagraph(doc, sprintf('Table A2. Estimated proportion of C. jejuni (Cj) and C. coli (Cc) human disease isolates from Newcastle/North Tyneside (n = %s; n = %s) and Oxfordshire (n = %s; n = %s) attributed to putative sources', all_cj_nwc, all_cc_nwc, all_cj_oxc, all_cc_oxc), stylename='Normal')
doc = addFlexTable(doc, vanilla.table(sbs_table))
doc = addParagraph(doc, '\r\n', stylename=)

# Annual breakdown tables
doc = addTitle(doc, 'Annual breakdown', level=3)
# Breakdown of number of isolates per year


# Breakdown of attribution per year
# C. jejuni
# Get significant figures
cj_years_mean[,-1] = signif(cj_years_mean[,-1], 3)
# Display table
doc = addParagraph(doc, sprintf('Table A3. Proportion of %s and %s C. jejuni isolates from Newcastle/North Tyneside (NWC) and Oxfordshire (OXC) attributed to putative sources per year between %s and %s', dated_cj_nwc, dated_cj_oxc, min_date_cj, max_date_cj), stylename='Normal')
doc = addFlexTable(doc, vanilla.table(cj_years_mean))
doc = addParagraph(doc, '\r\n', stylename=)
# C. coli
# Get significant figures
cc_years_mean[,-1] = signif(cc_years_mean[,-1], 3)
# Display table
doc = addParagraph(doc, sprintf('Table A4. Proportion of %s and %s C. coli isolates from Newcastle/North Tyneside (NWC) and Oxfordshire (OXC) attributed to putative sources per year between %s and %s', dated_cc_nwc, dated_cc_oxc, min_date_cc, max_date_cc), stylename='Normal')
doc = addFlexTable(doc, vanilla.table(cc_years_mean))
doc = addParagraph(doc, '\r\n', stylename=)

writeDoc(doc, "FSA_Report_Skeleton.docx")
