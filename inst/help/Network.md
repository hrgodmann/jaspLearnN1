# How Are Symptoms Connected?

This analysis represents a perceived causal network (PECAN): the connections describe how a person believes their problems influence each other. They are not estimates of causal effects from repeated measurements.

## Problems and connections

Define each problem and its severity, from 0 to 1. Every selected problem appears in each completed network, including problems with no entered connections. Its severity remains available for the plot's color, size, and opacity settings. Severity is shared across all assessment tabs; the current interface does not record separate severity ratings for each occasion.

Each tab represents an assessment occasion. Name the tab to identify the occasion or reference period, for example, "Assessment 1: past two weeks". Use the same reference period and context for every connection within that tab. Ratings refer to perceived relationship strength, rather than confidence that a relationship exists.

For each connection, consider: *During this period, when the source problem increases, does the target problem increase or decrease, and how strongly?* Positive ratings indicate an increasing relationship; negative ratings indicate a decreasing relationship. The scale runs from -1 (the strongest decreasing relationship) through 0 (no perceived relationship) to 1 (the strongest increasing relationship). Positive and negative do not automatically mean harmful and helpful. Interpret the signs in relation to how each problem is defined.

In the manual connection list, select a source problem, a different target problem, and a connection strength. Complete or remove unfinished rows. To create a network with no connections, remove all connection rows. This produces a plot containing all selected problems and no arrows. The current interface has no separate unknown/unrated connection value; do not use zero to represent uncertainty or an unassessed relationship. Complete the assessment before interpreting its connection summaries.

The **All possible connections** option supplies strengths for all directed pairs of different problems. In either input mode, a strength of zero produces no arrow, regardless of the plot's color, width, or opacity settings. The rating remains recorded in the edge weight table and CSV export. All selected problems remain visible, including those with only zero-rated connections.

## Connection summaries

Select **Connection summaries** to display one table for each completed assessment. The table includes every selected problem, in the order entered, with severity shown separately from four connection summaries:

| Direction | Absolute strength | Signed sum |
| --- | --- | --- |
| Outgoing: connections from this problem to others | Sum of the absolute values of outgoing ratings | Sum of outgoing ratings with their signs retained |
| Incoming: connections from other problems to this one | Sum of the absolute values of incoming ratings | Sum of incoming ratings with their signs retained |

Absolute strength describes the total magnitude of entered connections, ignoring their signs. Signed sum describes the balance of increasing and decreasing ratings. These measures answer different questions and are displayed together. They are sums, not averages, connection counts, percentages, or probabilities; their magnitude can exceed 1.

For example, outgoing ratings of +0.7 and -0.7 give **absolute strength 1.4** and **signed sum 0**. The problem is connected to two others even though the signed ratings cancel. A problem with no entered connections has zero for all four connection summaries, while its severity is still displayed. Neither situation establishes that the problem has no real-world influence or clinical importance.

The signed sum is the calculation used for one-step expected influence, applied separately to incoming and outgoing connections here. The table uses the descriptive label "Signed sum" because the ratings express perceived relationships and do not establish actual intervention effects. Severity does not weight either measure. This presentation uses established network summaries; it does not reproduce the original PECAN protocol's severity-weighted centrality formula or its allocation of causal percentages.

Use the graph, individual connections, severity, and summaries to support collaborative case formulation. Consider the person's priorities, the feasibility of change, and clinical judgment when choosing treatment targets. The table does not supply a combined priority score or predicted treatment benefit. Compare assessments with care if the selected problems, their definitions, the rating scale, or the reference period change.

## Output for unconnected problems

If no connections were entered for a time point, the edge weight table contains no connection rows and explains why. Both Circular and Sugiyama layouts support these networks.

## CSV export

Choose a **CSV destination**, then press **Export CSV / Save again**. Only that button writes a file. Choosing or changing the destination, editing network data, changing display options, and opening a saved analysis do not export automatically. Pressing the button replaces an existing file at the selected destination; choose a different file name if you want to keep an earlier export.

The CSV file contains `type`, `time`, `name`, `severity`, `from`, `to`, and `weight` columns:

- `node`: one row per selected problem, including unconnected problems; `name` and `severity` describe the problem.
- `edge`: one row per entered connection in a completed time point; `time`, `from`, `to`, and `weight` describe the connection.
- `network`: one metadata row for each completed time point with no connections; only `time` is populated. This preserves the time-point name without inventing an edge.

Only completed assessments are exported. The **Network export** status identifies those saved and names each assessment omitted because it contains unfinished or invalid connections. An omitted assessment contributes no connection or network rows, including any completed connections within it. Node rows contain the selected problems shared across assessments. A deliberately empty assessment is complete and receives a `network` record.

If no assessments are complete, no CSV is written and any existing file is left unchanged. If writing fails, the export status explains the problem while valid analysis plots and tables remain available. Check the destination folder and permissions, or choose another destination, then click **Export CSV / Save again** to retry. Fixing the problem alone does not trigger another write. The new CSV is fully written to a temporary file before replacing an existing export.

After network input or the destination changes, the status says **Changes have not been exported** until you press the button again. Changing only plot appearance or table visibility leaves the export status unchanged. Each button press makes one attempt, including repeated saves to the same destination. Older saved analyses may contain an export request without a recorded outcome; they show an unknown-outcome message and require an explicit button press to export.

## References

Vogel et al. (2025). *How perceived causal networks can complement case conceptualization, diagnostic classification, and data-based networks*. The guidance recommends presenting all nodes selected by the individual (p. 849). https://doi.org/10.1037/abn0001036

Robinaugh, Millner, and McNally (2016). *Identifying highly influential nodes in the complicated grief network*. Describes absolute strength and one-step expected influence, including the importance of retaining edge signs. This supports the distinction between the summaries, not validation of treatment selection from PECAN ratings. https://doi.org/10.1037/abn0000181

Klintwall, Bellander, and Cervin (2023; first published online 2021). *Perceived causal problem networks: Reliability, central problems, and clinical utility for depression*. Describes the original nonnegative causal-allocation ratings and severity-weighted centrality score. https://doi.org/10.1177/10731911211039281

Seewald et al. (2025). *Networks for treatment selection in psychotherapy: Providing a manual for process-based perceived causal networks*. Describes collaborative consideration of connections alongside other clinical factors. https://doi.org/10.1080/16506073.2025.2568000
