# making_do_with_less

This repository provides small example of training a random forest model for use in multifrequency acoustic euphausiid identification. 
It accompanies the Fisheries Research manuscript "Making do with less: extending an acoustic-based time series of euphausiid abundance using an uncrewed surface vehicle with fewer frequencies" by Mike Levine and Alex De Robertis.

In the Bering Sea, euphausiids are usually identified using methods that rely on measurements made at four acoustic frequencies collected from a standard research vessel. In 2020, measurements were instead collected at only two acoustic frequencies from Saildrone uncrewed surface vehicles. We found that simply applying the existing method using less acoustic information provided an overestimate of euphausiid abundance and did not allow for consistency with other observations in the time series. We therefore developed an alternative euphausiid identification method using a random forest classifier that allowed for comparable abundance estimates. This example uses a small subset of the total training data used to demonstrate this approach. We hope that it is useful to other researchers. Feel free to contact us with any questions at mike.levine@noaa.gov.

**To run this example:**
This example requires R, and was constructed using R version 4.2.3.
It is structured as an RStudio project for easy use- if you do use RStudio, simply pull the code from this repository, open up the project (*making_do_with_less.Rproj*), and open up the script *example_rf_model.R*. 
If you don't use RStudio, the only modifications you'll need are to instead to ignore the project file, open the script *example_rf_model.R* and change the file paths at L43 and L46 to appropriate paths on your system.


## Disclaimer

“The United States Department of Commerce (DOC) GitHub project code is provided on an ‘as is’ basis and the user assumes responsibility for its use. DOC has relinquished control of the information and no longer has responsibility to protect the integrity, confidentiality, or availability of the information. Any claims against the Department of Commerce stemming from the use of its GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.”


<img src="https://raw.githubusercontent.com/nmfs-fish-tools/nmfspalette/main/man/figures/noaa-fisheries-rgb-2line-horizontal-small.png" height="75" alt="NOAA Fisheries">

[U.S. Department of Commerce](https://www.commerce.gov/) | [National Oceanographic and Atmospheric Administration](https://www.noaa.gov) | [NOAA Fisheries](https://www.fisheries.noaa.gov/)
