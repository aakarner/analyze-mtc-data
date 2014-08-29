analyze-mtc-data
================

License: [GPL-3](http://www.gnu.org/licenses/gpl.html)

Much travel model data is locked away in proprietary software like Citlabs's CUBE and Caliper Corporation's TransCAD. These organizations provide educational licenses, so that academics can potentially access data, but they are often limited. It's not easy to move between the proprietary and more open formats (like plain text!). What's worse is that planning agencies often analyze their data using the mother of all proprietary stats packages, SAS. SAS is profoundly expensive and is no longer of much use in light of available free, open source, and powerful alternatives.

The aim of this script is to unlock some publicly available travel model output using open source software. The Metropolitan Transportation Commission (MTC), the metropolitan planning organization (MPO) for the nine county San Francisco Bay Area [freely publishes some of the outputs from their activity-based model data](http://mtcgis.mtc.ca.gov/foswiki/Main/DataRepository). This is a pretty major innovation. Most MPOs publish no data and make it pretty difficult to get your hands on it.

The single script here creates a MonetDB database and loads in all of the data from a number of transportation-land use scenarios that MTC employed in their last regional plan update. R is used as a front-end for database access. It can also be used for some analysis.

The code is basically for reference only -- it would require some modification to work with the data that MTC has posted on the web. Let me know if you're interested in seeing that happen.

Note: MonetDB has to be installed on your system first. Follow the instructions [here](https://github.com/ajdamico/usgsd/blob/master/MonetDB/monetdb%20installation%20instructions.R). 