# ===================================================================
# analysis/04_report.R -- writes the LaTeX source of the report (report/report.tex)
# and compiles it to report/report.pdf
# run AFTER 01_cleaning.R, 02_descriptive.R and 03_analysis.R, from the repository folder
# (the one that contains analysis/ and report/), because the report loads the tables
# and figures those scripts saved in report/tables and report/figures
# authors: Ryan David and Joseph Muchnick
# ===================================================================

file_tex <- file.path("report", "report.tex")

report_text <- r"---(\documentclass[11pt,a4paper]{article}

\usepackage[utf8]{inputenc}
\usepackage[T1]{fontenc}
\usepackage{lmodern}
\usepackage[margin=2.5cm]{geometry}
\usepackage{booktabs}
\usepackage{graphicx}
\usepackage{caption}
\usepackage{amsmath}
\usepackage[hidelinks]{hyperref}
\usepackage{float}
\usepackage{pdflscape}

\captionsetup{font=small,labelfont=bf}
\setlength{\parindent}{0pt}
\setlength{\parskip}{0.6em}
\graphicspath{{figures/}}

% ---------------------------------------------------------------
% Marks a number that is NOT in the exported tables and must be
% copied from the R console (shown in bold brackets, same colour as the text).
% ---------------------------------------------------------------
\newcommand{\TODO}[1]{\textbf{[#1]}}

% ---------------------------------------------------------------
% \intable{file}{caption}{label}: inputs a table exported from R and
% adds the caption around it (the exported files have no caption).
% ---------------------------------------------------------------
\newsavebox{\tabbox}
\newcommand{\intable}[3]{%
  \begingroup
  \def\tabcap{#2}\def\tablab{#3}%
  \renewenvironment{table}[1][]{\par\medskip\begin{lrbox}{\tabbox}}%
    {\end{lrbox}\par\centering
     \ifdim\wd\tabbox>\linewidth
       \resizebox{\linewidth}{!}{\usebox{\tabbox}}%
     \else
       \usebox{\tabbox}%
     \fi
     \par\captionof{table}{\tabcap}\label{\tablab}\medskip}%
  \input{tables/#1}%
  \endgroup}

% \figurebox{file}{caption}{label}
\newcommand{\figurebox}[3]{%
  \begin{figure}[H]
    \centering
    \includegraphics[width=0.85\textwidth]{#1}
    \caption{#2}\label{#3}
  \end{figure}}

\title{What goes with a Prague caf\'e's Google rating?\\[0.3em]
\large Chapter ``Rating'': ten analyses of 189 caf\'es}
\author{Ryan David \and Joseph Muchnick}
\date{November 2026}

\begin{document}
\maketitle

\begin{abstract}
\noindent
This report asks how the Google rating of a Prague caf\'e relates to how many people rated it and to what kind of caf\'e it is.
Using 189 caf\'es collected by students (186 with a rating), I find that the average rating is high (4.54 rating points) and only slightly above 4.5.
The average rating hardly changes with the number of ratings, but the shape of the relationship matters: caf\'es with 100 to 1,000 ratings are rated highest, and caf\'es with many ratings are less likely to be top-rated (4.7 or higher).
International chains are rated about 0.3 points lower than independent caf\'es, although only eight of them are in the sample.
All findings describe this sample and are associations, not causes.
\end{abstract}

\tableofcontents
\newpage

%====================================================================
\section{Introduction}
%====================================================================
Prices, location, size and speciality are one side of a caf\'e. The other side is how well customers rate it. This chapter of the project on the Prague caf\'e market asks what a Google rating tells us, and what it is related to.

A Google rating is an average of stars given by customers, and it is only as reliable as the number of people behind it. A caf\'e with five ratings of 5.0 may simply be lucky, while a caf\'e with two thousand ratings averaging 4.6 is a much more trustworthy measure of quality. The analyses therefore follow one story in ten steps:
\begin{enumerate}
  \item How high are ratings in Prague, in general? (A1)
  \item Do popular caf\'es (many ratings) get lower ratings? (A2, A4, A6, A7)
  \item Does it matter if we correct the rating for the number of ratings? (A3, A9)
  \item Who is more likely to reach the top? (A5, A10)
  \item Does the type of caf\'e (independent or chain) play a role? (A8)
\end{enumerate}
The ten research questions are listed in Section~\ref{sec:results}, each with its hypotheses.

%====================================================================
\section{Data and cleaning}
%====================================================================
\subsection{Data}
The data were collected in 2026 by ten students. Each row is one caf\'e, and each student recorded, among other things, the caf\'e's name, district, coordinates, Google rating, number of Google ratings and caf\'e type (independent, local chain, international chain). The sample is a convenience sample and not a random sample of Prague caf\'es.

\subsection{Cleaning decisions}
Cleaning was done in one R script (\texttt{01\_cleaning.R}) that reads the raw file, which was not edited by hand, and writes \texttt{dataset.csv}. The raw file had 299 rows, of which 200 were real caf\'es (the others were empty formula rows). The following choices were made and could change the results if made differently.
\begin{itemize}
  \item \textbf{Text first.} All columns were read as text and converted to numbers only after listing every value that was not a plain number. Original columns were never overwritten; each cleaned variable is a new column. Missing values stay missing and were never replaced by 0.
  \item \textbf{Labels.} Districts were rebuilt from their number (``prauge 1'' and ``Prague 1'' both became ``Prague 1''). Caf\'e type was restricted to the three allowed labels, and a blank caf\'e type was set to missing (one caf\'e).
  \item \textbf{Several values in one cell.} Where a price cell had several values (``65/85'', ``85, 95, 105''), the first listed price was kept. A spreadsheet had also merged ``115,125,135'' into 115125135; this was cut to its first three digits. These price decisions do not affect the Rating chapter, which does not use prices.
  \item \textbf{Numbers.} Thousands separators were removed from the number of ratings and decimal commas replaced in the rating. After conversion no value had become newly missing, and the number of ratings contained only whole numbers.
  \item \textbf{Coordinates.} The coordinates were split into latitude and longitude, and all caf\'es lie inside Prague.
  \item \textbf{Duplicates.} The same caf\'e was sometimes entered twice with a different spelling, so comparing names found only a few. Instead, every pair of caf\'es was compared and flagged if the two were less than 40~m apart and had about the same number of ratings (within 10\%, or one missing). Eleven such pairs were checked by hand and confirmed, and the later entry of each pair was dropped. This reduced the data from 200 to \textbf{189 caf\'es}. Neighbouring caf\'es in the same building have different numbers of ratings and were kept.
\end{itemize}

\subsection{Variables used}
\begin{itemize}
  \item \textbf{Rating}: Google rating, 1 to 5 (observed range 3.6 to 5.0). \textbf{Number of ratings}: Google number of ratings.
  \item \textbf{Adjusted Rating} $=(n\times R + 50\times C)/(n+50)$, where $R$ is the caf\'e's rating, $n$ its number of ratings and $C$ the mean rating of all caf\'es ($C\approx 4.54$, computed in code). It pulls ratings based on few reviews towards the average.
  \item \textbf{Top-rated}: rating of 4.7 or higher (1 = yes, 0 = no).
  \item \textbf{Above/below median number of ratings}: compared with the median number of ratings, computed in code (446.0, taken from the descriptive table). A caf\'e exactly at the median is in neither group.
  \item \textbf{Number-of-ratings groups}: few (under 100), medium (100 to 1,000), many (over 1,000).
\end{itemize}

\subsection{Sample and missing values}
After cleaning, 189 caf\'es remain. Three have no Google rating and are excluded from every analysis that uses the rating, which leaves 186 (Table~\ref{tab:D}). The number of ratings is missing for two caf\'es, and one caf\'e has no caf\'e type. Each analysis below states how many caf\'es it uses and how many were lost. Table~\ref{tab:Dcat} gives the categorical variables, and Appendix~\ref{app:desc} gives the full descriptive statistics.

The rating is skewed to the left, with most caf\'es between 4.3 and 4.9 (mean 4.54, SD 0.25, median 4.60, IQR 0.27). The number of ratings is extremely skewed (median 446, mean 986, maximum 23,951), which is why the questions compare groups of caf\'es rather than using the number of ratings as a typical bell-shaped variable.

\figurebox{D_histograms.pdf}{Distribution of the Google rating and the number of ratings (histograms). The rating piles up at the top of the scale and has a long tail towards low values; the number of ratings has a very long tail towards high values.}{fig:Dhist}

%====================================================================
\section{Methods}
%====================================================================
The significance level is $\alpha=.05$ in every analysis. All confidence intervals are 95\%. Bootstrap intervals are percentile intervals from 5,000 resamples with the seed set to 123. Hypotheses were formulated before the data were examined; a one-tailed alternative is used only where the question names a direction (``lower'', ``more likely''). For a one-tailed test the figure and table still show the two-sided interval, and the $p$-value is one-tailed and was fixed in advance.

\subsection{Parametric or non-parametric}
For each analysis, normality was judged from the histogram, the Q--Q plot and the skewness within each compared group (and for the differences in a paired test). Where normality was not met, the central limit theorem was considered to apply if the smallest group had at least about 30 caf\'es and the distribution of 5,000 bootstrapped means was approximately symmetric (Appendix~\ref{app:desc}). Table~\ref{tab:Ddec} gives the result: A1 to A4 are analysed with parametric methods and A6 (Spearman's correlation) with a non-parametric method. The ratings of all caf\'es, the above- and below-median groups, the paired differences and the medium and many groups are skewed, but they have far more than 30 caf\'es and their bootstrapped means are close to normal. The group with few ratings (22 caf\'es) is small, but its ratings are approximately symmetric (skewness $-0.19$). The number of ratings is too skewed for the central limit theorem to apply even with 187 caf\'es, so the correlation uses Spearman's $\rho$.

\intable{D_decisions_table.tex}{Method chosen for each analysis in the descriptive step (parametric or non-parametric).}{tab:Ddec}

\subsection{Tests used}
The analyses use the following methods; the checks of their assumptions are in Appendix~\ref{app:checks}.
\begin{itemize}
  \item A1: one-sample $t$-test. A2: Welch's $t$-test (one-tailed). A3: paired $t$-test.
  \item A4: Welch's ANOVA, with Games--Howell pairwise comparisons because it was significant.
  \item A5: $\chi^2$ test for a difference in proportions (one-tailed), Cram\'er's $V$ as effect size.
  \item A6: Spearman's $\rho$ with a bootstrap confidence interval.
  \item A7--A9: linear regression, with heteroscedasticity-robust (HC3) standard errors if the Breusch--Pagan test indicated non-constant variance.
  \item A10: logistic regression with the average marginal effect and a bootstrap confidence interval.
\end{itemize}
For differences between two groups, the direction is stated in words, and the effect size is Cohen's $d$ (or $d_z$ for paired data). Following the checklist, results are given as an estimate, its uncertainty, an effect size where required, and then the test.

%====================================================================
\section{Results}\label{sec:results}
%====================================================================
Table~\ref{tab:summary} summarises the ten analyses. Each subsection below gives the hypotheses, the estimate with its confidence interval and effect size, the supporting test, one key figure and the interpretation. Complete tables are in Appendix~\ref{app:tables}.

\begin{landscape}
\intable{A_summary_table.tex}{Main results of the ten analyses. Estimates are followed by their 95\% confidence interval in brackets. $p$-values for A2 and A5 are one-tailed (fixed in advance), all others two-tailed.}{tab:summary}
\end{landscape}

%--------------------------------------------------------------------
\subsection{A1: Is the average rating different from 4.5?}
%--------------------------------------------------------------------
$H_0$: the mean Google rating of Prague caf\'es is 4.5. $H_1$: it is not 4.5 (two-tailed).

In this sample the mean rating is $M=4.54$ rating points, 95\% CI [4.51, 4.58], $n=186$ (3 caf\'es without a rating were lost); the bootstrap interval is the same. The effect size is Cohen's $d=0.17$. A one-sample $t$-test (parametric, see Table~\ref{tab:Ddec}) gives $t(185)=2.36$, $p=.019$.

\figurebox{A1_figure.pdf}{Distribution of the Google rating of 186 caf\'es with the sample mean (4.54) and its 95\% confidence interval, and the benchmark of 4.5.}{fig:A1}

\textbf{Interpretation.} We found support that the average rating in this sample is above 4.5, but the difference is about 0.04 rating points. The interval excludes 4.5 only narrowly, and $d=0.17$ is small. In practice, a typical caf\'e in the sample is rated about 4.5, and ratings are high everywhere. Cafés in the same district are not independent, which would make the interval too narrow.

%--------------------------------------------------------------------
\subsection{A2: Do caf\'es with many ratings have lower ratings?}
%--------------------------------------------------------------------
$H_0$: the mean rating of caf\'es with an above-median number of ratings equals that of caf\'es with a below-median number. $H_1$: it is lower (one-tailed, because the question asks for ``lower'').

The mean rating is 4.52 for the 93 caf\'es above the median and 4.57 for the 92 caf\'es below it. The difference (above minus below) is $-0.04$ rating points, 95\% CI [$-0.12$, 0.03], $d=-0.17$. Welch's $t$-test (one-tailed) gives $t(176.28)=-1.17$, $p=.123$. Four caf\'es were lost (three with no rating, and one caf\'e at the median has no group).

\figurebox{A2_figure.pdf}{Google rating of caf\'es with a below-median and an above-median number of ratings (points), with each group's mean and 95\% confidence interval.}{fig:A2}

\textbf{Interpretation.} The difference points in the expected direction, but we found no evidence that caf\'es with more ratings are rated lower. The interval allows a difference from about $-0.12$ to $+0.03$ points. This is not evidence that there is no difference.

%--------------------------------------------------------------------
\subsection{A3: Is the Adjusted Rating different from the raw rating?}
%--------------------------------------------------------------------
$H_0$: the mean difference between Adjusted Rating and raw rating is zero. $H_1$: it is not zero (two-tailed).

For the 186 caf\'es with a rating, the mean raw rating is 4.544 and the mean Adjusted Rating is 4.554. The mean difference (adjusted minus raw) is $+0.010$ rating points, 95\% CI [$-0.003$, 0.024], $d_z=0.11$; the paired $t$-test gives $t(185)=1.54$, $p=.125$.

\figurebox{A3_figure.pdf}{Differences between the Adjusted Rating and the raw rating (points) for 186 caf\'es, with the mean difference and its 95\% confidence interval.}{fig:A3}

\textbf{Interpretation.} We found no evidence that the adjustment changes the average rating. The adjustment moves individual caf\'es, mostly those with few ratings (individual differences range from $-0.23$ to $+0.58$ points), but increases and decreases roughly cancel out (median difference $-0.004$). The mean difference is also affected by a few caf\'es with large changes, since the distribution of the differences is very skewed.

%--------------------------------------------------------------------
\subsection{A4: Does the rating differ by number of ratings?}
%--------------------------------------------------------------------
$H_0$: the mean rating is the same in the groups with few (under 100), medium (100 to 1,000) and many (over 1,000) ratings. $H_1$: at least one group mean is different (two-tailed).

The mean ratings are 4.32 for few ratings ($n=22$, 95\% CI [4.17, 4.47]), 4.61 for medium ($n=114$, [4.57, 4.66]) and 4.48 for many ($n=50$, [4.42, 4.54]). Welch's ANOVA gives $F(2.00, 49.96)=12.05$, $p<.001$. Three caf\'es were lost (no rating or no number of ratings). Games--Howell comparisons (Table~\ref{tab:A4post}) show that medium-sized caf\'es are rated 0.29 points higher than caf\'es with few ratings ([0.10, 0.48], $p=.002$) and 0.13 points higher than caf\'es with many ratings ([0.05, 0.22], $p=.001$). The difference between many and few ratings, 0.16 points ([$-0.03$, 0.35], $p=.124$), is not clearly different from zero.

\figurebox{A4_figure.pdf}{Google rating by number-of-ratings group (points), with each group's mean and 95\% confidence interval.}{fig:A4}

\textbf{Interpretation.} We found support that ratings differ between the groups, and the pattern is not a straight line: caf\'es with 100 to 1,000 ratings are rated highest, while newer caf\'es (few ratings) and the most popular caf\'es are rated lower. The few-ratings group has only 22 caf\'es, so its estimate is imprecise. These are associations; for example, caf\'es with many ratings may attract more tourists, who rate differently.

%--------------------------------------------------------------------
\subsection{A5: Are caf\'es with fewer ratings more likely to be top-rated?}
%--------------------------------------------------------------------
$H_0$: the share of top-rated caf\'es (rating of 4.7 or higher) is the same for caf\'es with a below-median number of ratings and for other caf\'es. $H_1$: it is higher for below-median caf\'es (one-tailed).

41 of 92 caf\'es with a below-median number of ratings (44.6\%) are top-rated, against 28 of 94 other caf\'es (29.8\%). The difference (below-median minus other) is 14.8 percentage points, 95\% CI [1.0, 28.5], Cram\'er's $V=0.15$. The smallest expected count is 34.13, so the $\chi^2$ test is valid: $\chi^2(1,N=186)=4.35$, $p=.018$ (one-tailed). Three caf\'es were lost for a missing rating.

\figurebox{A5_figure.pdf}{Share of top-rated caf\'es (rating of 4.7 or higher) among caf\'es with a below-median number of ratings and other caf\'es, in per cent, with 95\% confidence intervals.}{fig:A5}

\textbf{Interpretation.} We found support that, in this sample, caf\'es with fewer ratings are more likely to be top-rated, by about 15 percentage points. The interval is wide (from about 1 to 28 points), and $V=0.15$ is a small association. This may partly reflect that ratings based on few reviews vary more, so extreme values, including very high ones, are more likely; the data cannot separate this from a real difference in quality.

%--------------------------------------------------------------------
\subsection{A6: Is the rating correlated with the number of ratings?}
%--------------------------------------------------------------------
$H_0$: Spearman's $\rho$ between the rating and the number of ratings is zero. $H_1$: it is not zero (two-tailed).

Spearman's $\rho=-0.12$, 95\% bootstrap CI [$-0.28$, 0.03], $n=186$; $\rho^2=0.01$, so the number of ratings shares about 1\% of the ranked variation with the rating. The test gives $S=1{,}203{,}604$, $p=.096$. Spearman was used because the number of ratings is far too skewed for a parametric method (Table~\ref{tab:Ddec}). Three caf\'es were lost.

\figurebox{A6_figure.pdf}{Google rating against the number of ratings for 186 caf\'es (each point is one caf\'e), with the Spearman correlation and its 95\% bootstrap confidence interval.}{fig:A6}

\textbf{Interpretation.} The correlation is weak and negative, but we found no evidence of a monotonic relationship: the interval runs from $-0.28$ to $0.03$. A correlation is not an effect, and it does not suggest that more ratings cause a lower rating.

%--------------------------------------------------------------------
\subsection{A7: By how many points does the rating change per 1,000 ratings?}
%--------------------------------------------------------------------
$H_0$: the slope of the rating on the number of ratings (in thousands) is zero. $H_1$: it is not zero (two-tailed).

In the simple regression the slope is $-0.011$ rating points per 1,000 ratings, 95\% CI [$-0.029$, 0.006], $t(184)=-1.27$, $p=.206$; the intercept is 4.555 and $R^2=0.009$ ($n=186$). The Breusch--Pagan test gives $p=.318$, so there is no evidence of non-constant variance and the usual standard errors are used (not robust ones).

\figurebox{A7_figure.pdf}{Google rating against the number of ratings (in thousands) with the fitted regression line and its 95\% confidence band.}{fig:A7}

\textbf{Interpretation.} A caf\'e with 1,000 more ratings has a rating that is about 0.01 points lower on average in this sample, a trivial amount, and we found no evidence that the slope differs from zero. Because the number of ratings has a maximum of nearly 24,000 and a median of 446, the line is influenced by a few very popular caf\'es (see Appendix~\ref{app:checks}).

%--------------------------------------------------------------------
\subsection{A8: Does the rating differ by caf\'e type, once the number of ratings is accounted for?}
%--------------------------------------------------------------------
$H_0$: the mean rating is the same for independent caf\'es, local chains and international chains, holding the number of ratings fixed. $H_1$: at least one type differs (two-tailed).

Compared with independent caf\'es, local chains have a rating that is $-0.007$ points different ([$-0.090$, 0.076], $p=.865$) and international chains have a rating that is $0.287$ points lower ([$-0.467$, $-0.108$], $p=.002$), with the number of ratings in the model ($n=185$, $R^2=0.061$; one caf\'e had no type and three no rating). The joint test of caf\'e type is $F(2,181)=5.04$, $p=.007$. Without the number of ratings, the international-chain difference is $-0.282$ points, so accounting for the number of ratings changes almost nothing (Table~\ref{tab:A8}).

\figurebox{A8_figure.pdf}{Google rating by caf\'e type (points) with each type's mean and 95\% confidence interval.}{fig:A8}

\textbf{Interpretation.} We found support that the type of caf\'e is related to the rating: international chains are rated about 0.3 points lower than independent caf\'es, and almost all of that difference remains after accounting for the number of ratings. Local chains are no different from independent caf\'es. Only eight international chains are in the sample, so the estimate is imprecise and depends on a few brands.

%--------------------------------------------------------------------
\subsection{A9: Does the raw--Adjusted Rating relationship differ between caf\'es with few and many ratings?}
%--------------------------------------------------------------------
$H_0$: the slope of the Adjusted Rating on the raw rating is the same for caf\'es with few (under 100) and many (100 or more) ratings. $H_1$: the slopes differ (two-tailed).

For caf\'es with many ratings the slope is 0.892 (95\% CI [0.863, 0.921]), meaning one more rating point goes with 0.89 more Adjusted Rating points. For caf\'es with few ratings it is 0.396 ([0.304, 0.487]). The interaction (few minus many) is $-0.497$ points, 95\% CI [$-0.592$, $-0.401$], $t(182)=-10.24$, $p<.001$ ($n=186$, $R^2=0.983$).

\figurebox{A9_figure.pdf}{Adjusted Rating against the raw rating, for caf\'es with few (under 100) and many (100 or more) ratings, with the fitted lines of both groups.}{fig:A9}

\textbf{Interpretation.} For caf\'es with many ratings the Adjusted Rating is almost the raw rating, while for caf\'es with few ratings it is pulled strongly towards the average, as it was designed to. This finding is largely true by construction, because the Adjusted Rating is computed from the raw rating and the number of ratings, so it shows that the shrinkage works rather than a discovery about caf\'es.

%--------------------------------------------------------------------
\subsection{A10: Does the chance of being top-rated change with the number of ratings?}
%--------------------------------------------------------------------
$H_0$: the number of ratings (per 1,000) has no association with the probability that a caf\'e is top-rated (rating of 4.7 or higher). $H_1$: it does (two-tailed).

Of the 186 caf\'es, 69 are top-rated. In the logistic regression, each additional 1,000 ratings lowers the log-odds by 0.578 (SE 0.225); the odds ratio is 0.56, 95\% CI [0.36, 0.87]. The average marginal effect is $-12.8$ percentage points per 1,000 ratings, 95\% bootstrap CI [$-23.7$, $-5.5$]. The Wald test gives $z=-2.57$, $p=.010$.

\figurebox{A10_figure.pdf}{Probability of being top-rated (rating of 4.7 or higher) against the number of ratings (in thousands), with the fitted logistic curve and its 95\% confidence band; points are individual caf\'es.}{fig:A10}

\textbf{Interpretation.} We found support that caf\'es with more ratings are less likely to be top-rated: 1,000 more ratings go with a probability about 13 percentage points lower in this sample. The effect is an average over the observed range, and most caf\'es have fewer than 1,000 ratings, so it should not be extrapolated to very popular caf\'es.

%====================================================================
\section{Discussion and conclusion}
%====================================================================
\subsection{How the results fit together}
Ratings in Prague are high (A1). The average rating changes little with the number of ratings, whether we compare above- and below-median caf\'es (A2), use a correlation (A6) or a regression line (A7), and all three intervals include zero. Yet the number of ratings matters in two other ways. First, the relationship is not a straight line: medium-sized caf\'es are rated highest, and caf\'es with few ratings and caf\'es with many ratings are rated lower (A4), which a linear model cannot capture. Second, caf\'es with fewer ratings are more likely to be top-rated (A5, A10), as would be expected if a rating based on few reviews is more variable. The Adjusted Rating corrects for this by pulling such ratings towards the average (A3, A9), and it leaves the overall mean practically unchanged. Finally, international chains are rated about 0.3 points lower than independent caf\'es, and this is not explained by their number of ratings (A8).

\subsection{Conclusion}
In this sample of Prague caf\'es, a Google rating mostly says that a caf\'e is good: the typical rating is about 4.5. The number of ratings is hardly related to the rating itself, but caf\'es with many ratings are less likely to be top-rated, and the highest ratings are found among caf\'es with a medium number of ratings. International chains are rated lower.

\subsection{Limits}
\begin{itemize}
  \item \textbf{Convenience sample.} The caf\'es were chosen by students and are not a random sample of Prague caf\'es, so the results describe this sample.
  \item \textbf{Observational data.} All results are associations; none shows that a number of ratings or a caf\'e type causes a rating.
  \item \textbf{Dependence within districts.} Caf\'es in the same district are not independent (similar customers, similar tourist traffic). Every method here assumes independent caf\'es, so standard errors are probably too small and the confidence intervals too narrow, and $p$-values too small. The effect is likely larger for variables that differ strongly between districts, such as the number of ratings.
  \item \textbf{Small groups.} Only 22 caf\'es have few ratings, and only 8 are international chains.
  \item \textbf{Many tests.} Ten tests at $\alpha=.05$ mean that about one significant result could appear by chance even if nothing was related. The results close to the boundary ($p=.018$ to $.019$) deserve the most caution. All ten analyses were planned in advance; the check of influential caf\'es in Appendix~\ref{app:checks} was added after seeing the diagnostics and is exploratory..
  \item \textbf{Google ratings.} Ratings are voluntary and may reflect tourists or particular customers, and the ratings were copied at the time of each visit.
\end{itemize}

%====================================================================
\appendix
%====================================================================
\section{Full descriptive analysis}\label{app:desc}
%====================================================================
Table~\ref{tab:Dstats} gives the descriptive statistics of every variable and group used in the analyses, and Table~\ref{tab:D} the normality and central-limit checks that lead to the choice of method (a group is judged normal if it has an absolute skewness below 0.5 and an absolute excess kurtosis below 1; if not, the central limit theorem is considered to apply when the group has at least 30 caf\'es and the bootstrapped means, 5,000 resamples with seed 123, are approximately symmetric). The Shapiro--Wilk test is shown but does not override the plots.

\intable{D_stats_table.tex}{Descriptive statistics of the variables used (mean, standard deviation, median, interquartile range, minimum and maximum, with the number of observations and of missing values).}{tab:Dstats}

\intable{D_table.tex}{Normality and central-limit checks, and the resulting method, for every variable and compared group.}{tab:D}

\intable{D_categorical_table.tex}{Counts and percentages of the categorical variables (of 189 caf\'es).}{tab:Dcat}

\figurebox{D_boxplots.pdf}{Boxplots of the Google rating and the number of ratings.}{fig:Dbox}
\figurebox{D_groups.pdf}{Distributions of the Google rating in the groups that are compared (above/below median number of ratings; few, medium and many ratings).}{fig:Dgroups}
\figurebox{D_qqplots.pdf}{Normal Q--Q plots for each variable and group. Points on the line indicate a normal distribution.}{fig:Dqq}
\figurebox{D_bootstrap_means.pdf}{Distribution of 5,000 bootstrapped means for each variable and group (seed 123), used to judge whether the central limit theorem applies.}{fig:Dboot}

%====================================================================
\section{Checks of assumptions}\label{app:checks}
%====================================================================
\subsection{Group comparisons (A1--A4)}
The variances of the groups were compared with Levene's test for A4; Welch's tests do not assume equal variances and were used for A2 and A4. For A4, Levene's test shows that the variances differ between the three groups, $F(2,183)=6.02$, $p=.003$, so Welch's ANOVA and the Games--Howell comparisons are the right choice (the standard ANOVA would not be). A2 used Welch's $t$-test, which does not assume equal variances, so no separate variance test was needed.. For A3 the differences are very skewed, but the 186 pairs and their approximately normal bootstrapped means (Table~\ref{tab:D}) justify the paired $t$-test.

\subsection{Contingency table (A5)}
The smallest expected count is 34.13, well above 5, so the $\chi^2$ test is used and Fisher's exact test is not needed.

\subsection{Regressions (A7--A9) and logistic regression (A10)}
Residual-versus-fitted plots and normal Q--Q plots of the residuals are shown in Figures~\ref{fig:A7r}--\ref{fig:A9q}. The Breusch--Pagan test gives $p=.318$ for A7 and $p=.368$ for A8, so the variance is approximately constant and the usual standard errors are used. For A9 it gives $p<.001$, so heteroscedasticity-robust (HC3) standard errors are used in Table~\ref{tab:A9} and in the interaction test.. . \textbf{Influential caf\'es.} Cook's distance identifies one caf\'e that stands out in each regression. In A7 and A8 it is Caf\'e Louvre (Cook's $D=2.03$ in A7 and $1.20$ in A8; all other caf\'es are below 0.25), which has by far the most ratings (23,951). In A9 it is Kavarna-cukrarna Coffee House (D = 1.16; 3 ratings, rating 4.0), followed by PAUL Karl\'in (0.61). These checks were made after the main analysis and are \textbf{exploratory}. Re-estimating without Caf\'e Louvre, the slope in A7 is $-0.029$ rating points per 1,000 ratings, 95\% CI [$-0.059$, 0.001], $p=.055$, which is about three times larger than $-0.011$ in the main analysis but still not clearly different from zero. In A8 the international-chain difference is almost unchanged ($-0.291$, CI [$-0.470$, $-0.113$]), while the slope of the number of ratings becomes $-0.032$ per 1,000 ratings, CI [$-0.061$, $-0.002$], $p=.036$. So the conclusion about caf\'e type is robust, but the weak negative link between the number of ratings and the rating depends on this one caf\'e and should be treated as a lead, not a finding. In A9 dropping the two caf\'es with Kavarna in their name leaves the interaction almost the same ($-0.472$).

For A10 the log-odds are assumed to be linear in the number of ratings; the number of ratings is highly skewed, so a few very popular caf\'es have high leverage.

Since A9's outcome is computed from its predictor (Adjusted Rating from the raw rating), the high $R^2=0.983$ and the residual patterns are partly by construction.

\begin{figure}[H]
\centering
\includegraphics[width=0.48\textwidth]{A7_resid_figure.pdf}\hfill
\includegraphics[width=0.48\textwidth]{A7_qq_figure.pdf}
\caption{A7: residuals against fitted values (left) and normal Q--Q plot of the residuals (right).}\label{fig:A7r}\label{fig:A7q}
\end{figure}
\begin{figure}[H]
\centering
\includegraphics[width=0.48\textwidth]{A8_resid_figure.pdf}\hfill
\includegraphics[width=0.48\textwidth]{A8_qq_figure.pdf}
\caption{A8: residuals against fitted values (left) and normal Q--Q plot of the residuals (right).}\label{fig:A8r}\label{fig:A8q}
\end{figure}
\begin{figure}[H]
\centering
\includegraphics[width=0.48\textwidth]{A9_resid_figure.pdf}\hfill
\includegraphics[width=0.48\textwidth]{A9_qq_figure.pdf}
\caption{A9: residuals against fitted values (left) and normal Q--Q plot of the residuals (right).}\label{fig:A9r}\label{fig:A9q}
\end{figure}

\subsection{Independence}
All methods assume that caf\'es are independent. Caf\'es in the same district share customers and tourist traffic, so this is violated to some degree. The likely effect is that standard errors are too small, so intervals are too narrow and $p$-values too small; results close to $\alpha=.05$ (A1, A5, A10) should be read with the most caution.

%====================================================================
\section{Full result tables}\label{app:tables}
%====================================================================
\intable{A1_table.tex}{A1: one-sample comparison of the mean rating with 4.5.}{tab:A1}
\intable{A2_table.tex}{A2: rating of caf\'es with above- and below-median number of ratings.}{tab:A2}
\intable{A3_table.tex}{A3: Adjusted Rating minus raw rating (paired).}{tab:A3}
\intable{A4_table.tex}{A4: rating by number-of-ratings group, with Welch's ANOVA.}{tab:A4}
\intable{A4_posthoc_table.tex}{A4: pairwise comparisons (Games--Howell) of the rating between number-of-ratings groups.}{tab:A4post}
\intable{A5_table.tex}{A5: top-rated caf\'es among caf\'es with a below-median number of ratings and other caf\'es.}{tab:A5}
\intable{A6_table.tex}{A6: Spearman correlation between rating and number of ratings.}{tab:A6}
\intable{A7_table.tex}{A7: simple regression of the rating on the number of ratings (per 1,000).}{tab:A7}
\intable{A8_table.tex}{A8: rating by caf\'e type, without and with the number of ratings (reference: independent caf\'es).}{tab:A8}
\intable{A9_table.tex}{A9: Adjusted Rating on the raw rating, by group (few: under 100 ratings, many: 100 or more); ``rating:few'' is the interaction.}{tab:A9}
\intable{A10_table.tex}{A10: logistic regression of being top-rated on the number of ratings (per 1,000).}{tab:A10}

\end{document}
)---"

writeLines(report_text, file_tex, useBytes = TRUE)
cat("Saved:", file_tex, "\n")

# compile to PDF (needs a LaTeX installation, e.g. TinyTeX or MacTeX; if it fails,
# open report/report.tex in TeXShop / Overleaf and compile it there)
old_wd <- getwd()
setwd("report")
system("pdflatex -interaction=nonstopmode report.tex")
system("pdflatex -interaction=nonstopmode report.tex")   # second run: table of contents and references
setwd(old_wd)
