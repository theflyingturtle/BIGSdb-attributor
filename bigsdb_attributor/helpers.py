import textwrap

from itertools import groupby, cycle
from matplotlib import pyplot as plt

# Define colour palette
COLOURS = {
    "cattle": "#6FBAFF",
    "chicken": "#FFF697",
    "environment": "#A4FF53",
    "sheep": "#B3B3B3",
    "turkey": "#000000",
    "duck": "#FF9E36",
    "goose": "#FF6666",
    "pig": "#ffc0e9",
    "wild bird": "#803C20",
    "other animal": "#800080",
    "dog": "#008080",
    "ruminant": "#6FBAFF"}
EXTRA_COLOURS = cycle(['#1B38E7', '#FF0000', '#4C4C4C',
                       '#408000', '#FFFFFF', '#19FF80'])


def colour_of(pop): return COLOURS.get(
    pop.lower().strip(),
    None) or next(EXTRA_COLOURS)


def add_line(ax, xpos, ypos):
    line = plt.Line2D([xpos, xpos], [ypos + .1, ypos],
                      transform=ax.transAxes, color='black')
    line.set_clip_on(False)
    ax.add_line(line)


def label_len(my_index, level):
    labels = my_index.get_level_values(level)
    return [(k, sum(1 for i in g)) for k, g in groupby(labels)]


def label_group_bar_table(ax, df):
    # remove labels already there
    labels = ['' for item in ax.get_xticklabels()]
    ax.set_xticklabels(labels)
    ax.set_xlabel('')

    # add magical new labels manually
    ypos = -.05
    scale = 1. / df.index.size
    first = True
    for level in range(df.index.nlevels)[::-1]:
        pos = 0
        for label, rpos in label_len(df.index, level):
            lxpos = (pos + .5 * rpos) * scale
            ax.text(lxpos,
                    ypos,
                    label,
                    size='x-small' if first else 'small',
                    ha='left' if first else 'center',
                    transform=ax.transAxes,
                    rotation=270 if first else 0)
            if not first:
                add_line(ax, pos * scale, ypos)
            pos += rpos
        add_line(ax, pos * scale, ypos)
        ypos -= .15
        first = False


def plot_individual_ancestry(data, path, title=None, xlabel=None):
    # Reorder data by most common population (i.e. column)
    source_order = data.mean().sort_values(ascending=False).index.values.tolist()

    # dict(zip(source_order,range(len(source_order))))
    data['PopRank'] = data.idxmax(axis=1).map(source_order.index)
    data['MaxProb'] = data.max(axis=1)
    data = data.sort_values(by=["PopRank", "MaxProb"], ascending=[
                            True, False]).loc[:, source_order]

    # Generate graph of assignment probabilities for individual isolates
    colors = list(data.columns.map(colour_of))
    plot = data.plot.bar(stacked=True, color=colors, width=1.0, linewidth=0.0)

    # Draw black vertical lines between populations
    def blackvlines(location): return plt.axvline(location, color='#000000')
    data.idxmax(axis=1).value_counts().reindex(
        source_order).cumsum().map(blackvlines)

    if xlabel:
        plot.set_xlabel(xlabel)

    # Remove x-axis labels and ticks
    plot.xaxis.set_visible(False)
    plot.yaxis.set_ticks_position('left')
    plot.xaxis.set_ticks_position('none')
    plt.ylim((0, 1))

    if title:
        plt.title("\n".join(textwrap.wrap(title)))

    plot.legend(loc='upper center', bbox_to_anchor=(0.5, -0.05),
                ncol=3, frameon=True).get_frame().set_linewidth(1)

    # Save graph
    plt.savefig(path, bbox_inches='tight', dpi=300, format='svg')

    plt.clf()
