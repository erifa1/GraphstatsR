# graphstatsr 2.6.0

**Added Features :**

- Bar plots with the same behavior as box plots; more informative for samples without replicates.
- Option to add value labels to each bar in bar plots.
- Download button (PDF and JPEG).

**Improvement:**

- Simplified archive download for JPEG files.

# graphstatsr 2.5.0

**Added Features :**

- Boxplots (EasyStats) with continuous scale: Boxplots can now be generated using a continuous X scale, which makes it possible to visualize kinetics when needed. (Select Type of variable X: continuous)
- PNG export buttons for EnrC13 (IsoPlot)
- Interactive Plotly output for EnrC13 (IsoPlot)
- Simplified downloaded archives (results are now placed directly at the root of the archive)

**Debugs:**

- Results from different analysis sessions ending up in the same archive
- Boxplot generation (EasyStats) failing when no data is available in the first metabolite or first condition group
- Boxplot coloring in preview blocking the rest of the process

**Unresolved, known issue:**

- Bug related to double spaces in metadata: in HTML, consecutive spaces are automatically collapsed into a single one, causing inconsistencies between the original dataset and the information displayed/rendered by the web application.

**Conclusion:** Avoid using multiple spaces in metadata.

# graphstatsr 2.2.0

**NEW MODULE**

**MSPT:** submodule of Isoplot, able to analysis "pascal triangle" sample to validate experiment.
- reference: Millard, Pierre, et al. 2014. “Isotopic Studies of Metabolic Systems by Mass Spectrometry: Using Pascal’s Triangle To Produce Biological Standards with Fully Controlled Labeling Patterns.” Analytical Chemistry 86 (20): 10288–95. See :
- https://doi.org/10.1021/ac502490g; 
- https://github.com/llegregam/PascalTriangle

# graphstatsr 2.0.0

**NEW MODULE**

**Isoplot:** sub module to generate plot for isotopic MS data

# graphstatsr 1.10.0

**Added Features :**

- rbase graphics show faster rendering than ggplot.
- users can apply yaxis minimum / maximum limits and steps size, for ggplots figures and rbase graphics
- boxplot with ggstatsplots, pdf and image outputs
- metadata template based on features table vertical / horizontal display, labels size control for ggplot outputs.
- download all boxplots in png/jpg format in a single- archive
- button to download tests datasets
- widget to output specific features
- conditionnal popping buttons avoiding confusion
    new button to download all ACP files run / update- ACP with one button.

**Fixes:**

- metadata filters
- disabling statistics tests when plotting individuals - boxplots.

# graphstatsr 1.5.0

**Added Features:**

- New widget allowing user to sum quantification of compounds belonging the same group
- PCA: combining factors, display ellipses or not
- Boxplots: pngs output for each graphic fixes

# graphstatsr 1.4.1

**Added Features:**

- add new input modules with filters widgets (datamods package) add options for boxplots: 
    - coloring boxplots, 
    - custom y axis label 
    - inform outliers or not 
    - reordering/stashing boxplot conditions (sortable package) 
    - modularizing code

# graphstatsr 1.3.2

**Added Features:**

- New widget to export up to four box plots per PDF page.
- Box plots available in interactive Plotly mode.
- Option to export all plots in PNG format, named by metabolite.
- Tooltips to display outlier samples in box plots.
- Labels to identify outlier samples in box plots.
- Improved handling of missing values (NA).

**Debugs:**

- Fixed NA handling in PCA when some columns contain only NA values.
- Fixed uniqueness of row names in the feature table.
- Proper handling of zero values in the weighting factor (treated as NA).
- Clicking rows in the merged table now allows filtering of outliers.
- Box plots: added an option to display all conditions (even those without values) or hide them.
- Fixed handling of the µ character.

# graphstatsr 1.0.1

* Added a `NEWS.md` file to track changes to the package.
