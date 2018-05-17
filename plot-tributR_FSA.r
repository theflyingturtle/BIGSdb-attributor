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
ancs_cc = read.csv("/Users/MelissaJvR/Desktop/FSA_Final/RInputFiles/FSA_Cj_inferred-ancestry.csv", header=TRUE, check.names=TRUE)
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
# C. coli
no_ancs_cc = nrow(ancs_cc)
no_cc = nrow(cc)
cc_missing_dates = no_ancs_cc - no_cc
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
cj_sbs_overall_plot = ggplot(cj_anc_sites_long, aes(x = reorder(Source, -Proportion), y = Proportion)) + geom_bar(aes(fill = site), stat="identity", position = position_dodge(width=0.8), width = 0.8) + scale_fill_grey() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + labs(x="Sites", y="Proportion") + guides(fill=guide_legend(title=NULL)) +
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
cj_overall_plot = ggplot(cj_overall_summary) + geom_bar(aes(x=Source, y=Proportion), stat="identity", fill="black") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ylab("Proportion")

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
cc_sbs_overall_plot = ggplot(cc_anc_sites_long, aes(x = reorder(Source, -Proportion), y = Proportion)) + geom_bar(aes(fill = site), stat="identity", position = position_dodge(width=0.8), width = 0.8) + scale_fill_grey() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + labs(x="Sites", y="Proportion") + guides(fill=guide_legend(title=NULL)) +
    theme(legend.position="bottom", legend.direction = "horizontal")

# Overall summary
cc_overall <- cc_sources %>%
	select(-matches('site'))
cc_ancs_long = melt(cc_overall)
cc_overall_summary = melt(tapply(cc_ancs_long$value, cc_ancs_long$variable, mean), varnames="Source", value.name="Proportion")
cc_overall_summary = cc_overall_summary[order(-cc_overall_summary$Proportion),]
cc_overall_summary$Source = factor(cc_overall_summary$Source, levels=unique(as.character(cc_overall_summary$Source)))
cc_overall_plot = ggplot(cc_overall_summary) + geom_bar(aes(x=Source, y=Proportion), stat="identity", fill="black") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ylab("Proportion")

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
sbs_table = merge(cj_sbs_summary, cc_sbs_summary, by.x="row.names", by.y="row.names", all=TRUE, suffixes = c(".Cj",".Cc"))
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
overall_ind_ancs_plot = plot_grid(cj_ind_ancs_plot_nwc, cj_ind_ancs_plot_oxc, cc_ind_ancs_plot_nwc, cc_ind_ancs_plot_oxc, labels = c("A", "B", "C", "D"), ncol = 2)
# Save figure to output directory
ggsave("Figure3.svg", plot=overall_ind_ancs_plot, device="svg", width=210, height=148, units="mm", dpi=300)


# # FOR ALL DATE-BASED SUMMARIES, USE cj and cc dataframes
# ## Annual breakdown ##
# # C. jejuni
# # Add years column to date-complete dataset
# cj_years = cj
# cj_years$Year = format(cj_years$date,format="%Y")
# # Drop unnecessary columns
# cj_years <- cj_years %>%
#     select(-matches('id'))
# cj_years <- cj_years %>%
#     select(-matches('species'))
# cj_years <- cj_years %>%
#     select(-matches('date'))
# # Get averages by year
# cj_years_mean = aggregate(.~Year, cj_years, mean)
# # Convert to long form
# cj_years_mean_long = melt(cj_years_mean, id="Year", variable.name = "Source", value.name = "Proportion")
# # Read year as numeric to get proper x-axis labels in the area plot
# cj_years_mean_long$Year = as.numeric(cj_years_mean_long$Year)

# # Re-order the dataframe so that bars will be stacked from major (bottom) to minor (top) sources
# cj_years_order = order(ordered(cj_years_mean_long$Source, levels=cj_overall_summary$Source), decreasing = TRUE)
# cj_years_mean_long = cj_years_mean_long[cj_years_order,]
# cj_years_mean_long$Source = factor(cj_years_mean_long$Source, levels=unique(as.character(cj_years_mean_long$Source)))

# # Generate area plot and manipulate x-axis to display years correctly
# cj_years_plot = ggplot(cj_years_mean_long, aes(x=Year, y=Proportion, fill=Source)) + geom_area() + scale_x_continuous(breaks=as.numeric(unique(cj_years_mean_long$Year)), labels=c(as.character(unique(cj_years_mean_long$Year)))) + labs(x="Year", y="Proportion of isolates") + set_fill_colours
# # Alternative code for bar graph
# #cj_years_bar_plot = ggplot(cj_years_mean_long, aes(x=Year, y=Proportion, fill=Source)) + geom_bar(stat="identity") + set_fill_colours

# # Count C. jejuni isolates per year
# cj_years_count = aggregate(.~Year, cj_years, FUN= length)
# colnames(cj_years_count)[2] <- "No. isolates"
# retain = c("Year","No. isolates")
# cj_years_count = cj_years_count[,retain]

# # Repeat for C. coli
# # Set up data
# cc_years = cc
# cc_years$Year = format(cc_years$date,format="%Y")
# cc_years <- cc_years %>%
#     select(-matches('id'))
# cc_years <- cc_years %>%
#     select(-matches('species'))
# cc_years <- cc_years %>%
#     select(-matches('date'))
# cc_years_mean = aggregate(.~Year, cc_years, mean)
# cc_years_mean_long = melt(cc_years_mean, id="Year", variable.name = "Source", value.name = "Proportion")
# cc_years_mean_long$Year = as.numeric(cc_years_mean_long$Year)

# # Re-order
# cc_years_order = order(ordered(cc_years_mean_long$Source, levels=cc_overall_summary$Source), decreasing = TRUE)
# cc_years_mean_long = cc_years_mean_long[cc_years_order,]
# cc_years_mean_long$Source = factor(cc_years_mean_long$Source, levels=unique(as.character(cc_years_mean_long$Source)))

# # Generate plot
# cc_years_plot = ggplot(cc_years_mean_long, aes(x=Year, y=Proportion, fill=Source)) + geom_area() + scale_x_continuous(breaks=as.numeric(unique(cc_years_mean_long$Year)), labels=c(as.character(unique(cc_years_mean_long$Year)))) + labs(x="Year", y="Proportion of isolates") + set_fill_colours
# # Alternative code for bar graph
# #cc_years_bar_plot = ggplot(cc_years_mean_long, aes(x=Year, y=Proportion, fill=Source)) + geom_bar(stat="identity") + set_fill_colours

# # Count C. coli isolates per year
# cc_years_count = aggregate(.~Year, cc_years, FUN= length)
# colnames(cc_years_count)[2] <- "No. isolates"
# retain = c("Year","No. isolates")
# cc_years_count = cc_years_count[,retain]

# # Plot C. jejuni and C. coli annual breakdown in single figure
# yearly_plot = plot_grid(cj_years_plot, cc_years_plot, labels=c("A", "B"), nrow = 2, align = "v")
# # Save figure to output directory
# ggsave("Figure3.svg", plot=yearly_plot, device="svg", width=210, height=148, units="mm", dpi=300)
# # Additional code for saving bar graphs
# #yearly_bar_plot = plot_grid(cj_years_bar_plot, cc_years_bar_plot, labels=c("A", "B"), nrow = 2, align = "v")
# #ggsave("Figure3BarGraph.svg", plot=yearly_bar_plot, device="svg", width=210, height=148, units="mm", dpi=300)

# # Generate combined summary table of no. isolates per year
# overall_years_table = full_join(cj_years_count, cc_years_count, by = "Year")
# colnames(overall_years_table) <- c("Year", "C. jejuni", "C. coli")

# # FOR ALL DATE-BASED SUMMARIES, USE cj and cc dataframes
# ## Quarterly breakdown ##
# # C. jejuni
# # Add quarters column to date-complete dataset
# cj_quarters = cj
# cj_quarters$Quarter = as.yearqtr(cj_quarters$date, format = "%Y-%m-%d")
# # Drop unnecessary columns
# cj_quarters <- cj_quarters %>%
#     select(-matches('id'))
# cj_quarters <- cj_quarters %>%
#     select(-matches('species'))
# cj_quarters <- cj_quarters %>%
#     select(-matches('date'))
# # Get mean by quarter over time
# cj_quarters_mean = aggregate(.~Quarter, cj_quarters, mean)
# # Convert to long form
# cj_quarters_mean_long = melt(cj_quarters_mean, id="Quarter", variable.name = "Source", value.name = "Proportion")

# # Generate area plot
# # Should convert quarters to numeric, but they're categorical...
# # Attempt 1 - x-axis issue as for bar graphs
# # ggplot(cj_quarters_mean_long, aes(x=Quarter, y=Proportion, fill=Source)) + geom_area() + set_fill_colours

# # Generate bar graph
# # Basic plot with incorrect x-axis
# # cj_quarters_bar_plot = ggplot(cj_quarters_mean_long, aes(x=Quarter, y=Proportion, fill=Source)) + geom_bar(stat="identity") + set_fill_colours
# # As above but with rotated x-axis labels
# # ggplot(cj_quarters_mean_long, aes(x=Quarter, y=Proportion, fill=Source)) + geom_bar(stat="identity") + set_fill_colours + theme(axis.text.x = element_text(angle = 45, hjust = 1))
# # Attempt at sorting out axis labels into proper quarter format (requires zoo)
# # ggplot(cj_quarters_mean_long, aes(x=Quarter, y=Proportion, fill=Source)) + geom_bar(stat="identity") + set_fill_colours + scale_x_yearqtr(format="%Y Q%q")

### Report generation ###
## Create document and title ##
doc = docx(title="FSA_Report_Skeleton")
doc = addTitle(doc , 'Source attribution of campylobacteriosis isolates from Oxfordshire and Newcastle/North Tyneside', level=1)

## Describe datasets ##
doc = addTitle(doc, 'Datasets', level=2)
doc = addTitle(doc, 'Human disease isolates', level=3)
doc = addParagraph(doc, 'Add your description here.', stylename = 'Normal')
doc = addTitle(doc, 'Reference isolates', level=3)
doc = addParagraph(doc, 'Add your description here.', stylename = 'Normal')
doc <- addPageBreak(doc)

## Describe methods ##
doc = addTitle(doc , 'Methods', level=2)
doc = addParagraph(doc, 'Add your description of the methods here.', stylename = 'Normal')
# Previous descriptions used in FSA reports:
# STRUCTURE. Human disease isolates were assigned to putative host sources using the no-admixture model in STRUCTURE, based on analysis of MLST data.  STRUCTURE was run separately for C. coli and C. jejuni. The program was run using a burn-in period of 1,000 cycles followed by 10,000 iterations.
# iSOURCE. Human disease isolates were assigned to putative host sources using the Asymmetric Island model implemented in iSource, based on analysis of MLST data.  The algorithm was run separately for C. coli and C. jejuni. The program was run for 10,000 iterations without thinning, using a symmetric Dirichlet prior.
doc <- addPageBreak(doc)

## Present results ##
doc = addTitle(doc, 'Results', level=2)
doc = addParagraph(doc, 'Add any preamble to results here.', stylename = 'Normal')

# Numbers of isolates before and after data cleaning
doc = addTitle(doc, 'Data cleaning', level=3)
doc = addParagraph(doc, sprintf('Inferred ancestries for %s C. jejuni and %s C. coli isolates were loaded for summarising. All isolates are included in the "Overall summaries" section. Later sections required isolation/laboratory receipt dates, so %s C. jejuni and %s C. coli isolates with missing data were excluded. In total, %s C. jejuni and %s C. coli were included in the date-based summaries.', no_ancs_cj, no_ancs_cc, cj_missing_dates, cc_missing_dates, no_cj, no_cc), stylename='Normal')

# Overall summaries section
doc = addTitle(doc, 'Overall summaries', level=3)
# Overall summary table
doc = addParagraph(doc, 'Table 1. Estimated proportion of human disease isolates attributed to animal and environmental sources.', stylename='Normal')
doc = addFlexTable(doc, vanilla.table(overall_table))
doc = addParagraph(doc, '\r\n', stylename=)
# Overall summary plot
doc = addPlot(doc , fun=print, x=overall_plot, width=7, height=4)
doc = addParagraph(doc, sprintf('Figure 1. Estimated proportion of human disease isolates attributed to animal and environmental sources.  Probabilistic assignment of (A) %s C. jejuni collected between %s and %s, and (B) %s C. coli collected between %s and %s.', no_ancs_cj, min_date_cj, max_date_cj, no_ancs_cc, min_date_cc, max_date_cc), stylename='Normal')

# Site-by-site overall summaries section
# Overall site-by-site table
doc = addParagraph(doc, 'Table 2. ADD TITLE.', stylename='Normal')
doc = addFlexTable(doc, vanilla.table(sbs_table))
doc = addParagraph(doc, '\r\n', stylename=)
# Overall site-by-site plot
doc = addPlot(doc, fun=print, x=overall_sites_plot, width=7, height=4)
doc = addParagraph(doc, sprintf('Figure 2. ADD LEGEND.'), stylename='Normal')
# # Overall ancestries plot
# doc = addPlot(doc , fun=print, x=overall_ind_ancs_plot)
# doc = addParagraph(doc, sprintf('Figure . Source probabilities for individual human disease isolates. Probabilistic assignment of (A) %s C. jejuni and (B) %s C. coli isolates. Isolates are represented as vertical bars coloured according to the estimated probability for each source. Isolates are ordered horizontally by most likely source, and by decreasing probability within each source.', no_ancs_cj, no_ancs_cc), stylename='Normal')
# doc <- addPageBreak(doc)

# # Annual breakdown section
# doc = addTitle(doc, 'Annual breakdown', level=3)
# # Breakdown of number of isolates per year
# doc = addParagraph(doc, 'Table 2. Number of human disease isolates per year.', stylename='Normal')
# doc = addFlexTable(doc, vanilla.table(overall_years_table))
# doc = addParagraph(doc, '\r\n', stylename=)
# # Annual breakdown plot
# doc = addPlot(doc , fun=print, x=yearly_plot)
# doc = addParagraph(doc, sprintf('Figure . Proportion of (A) %s C. jejuni and (B) %s C. coli human disease isolates attributed to animal and environmental sources over time. Bars are ordered from major (bottom) to minor (top) sources.', no_cj, no_cc), stylename='Normal')

# ## Appendices ##
# doc = addTitle(doc, 'Appendices', level=2)
# # Annual breakdown tables
# doc = addTitle(doc, 'Annual breakdown', level=3)
# # C. jejuni
# # Get significant figures
# cj_years_mean[,-1] = signif(cj_years_mean[,-1], 3)
# # Display table
# doc = addParagraph(doc, sprintf('Table A1. Proportion of %s C. jejuni attributed to animal and environmental sources between %s and %s.', no_cj, min_date_cj, max_date_cj), stylename='Normal')
# doc = addFlexTable(doc, vanilla.table(cj_years_mean))
# doc = addParagraph(doc, '\r\n', stylename=)
# # C. coli
# # Get significant figures
# cc_years_mean[,-1] = signif(cc_years_mean[,-1], 3)
# # Display table
# doc = addParagraph(doc, sprintf('Table A2. Proportion of %s C. coli attributed to animal and environmental sources between %s and %s.', no_cc, min_date_cc, max_date_cc), stylename='Normal')
# doc = addFlexTable(doc, vanilla.table(cc_years_mean))
# doc = addParagraph(doc, '\r\n', stylename=)


writeDoc(doc, "FSA_Report_Skeleton.docx")
