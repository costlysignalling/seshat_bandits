# Stationary bandits - useful code bits
bits of code for CSH spring school (so far only R)

Raw equinox 2020 dataset available from http://seshatdatabank.info/databrowser/downloads.html is used (named "equinox2020_dataset_seshat.csv" in my folder) is used but is not part of this repository and needs to be downloaded separately

It a good idea to start from the **autoencoders_seshat.Rproj** file - it will open R studio with the equivalent settings as mine 

**01_process_data.R** can help you convert the original raw entries to a wide data format, where each row is one of the seshat places at a single timestep and each column in one of the variables.

There are **5 possible outputs**
**1: Just labels** (treats absent as "absent" and nominal allegiance in suprapolity relations as " nominal allegiance ")
**2: Numeric data not filled** (simply "absent" is 0, "present" is 1, ordered variables - they are not many - are assigned the steps - e.g. "nominal allegiance" corresponds to 2 in the respective variable)
**3: Numeric data filled** (the missing data are filled with the arithmetic mean) just for the record: Filling the data with arithmetic mean is stupid (early missing data are overrepresented), but we will fix this later :D For now we need to have quick anything.
**In these datasets, 1:4 columns are identifiers (place, time), 5:8 are "utility function" noncultural variables (popsize etc), 9:13 quntitative variables (mostly levels), 14:15 multistate variables, 16:128 (end) simple binary variables

**4: Indicator binary data**: that are perfect e.g. for autoencoders (more info here https://bradleyboehmke.github.io/HOML/autoencoders.html). There is a column "x.absent" next to the "x.present" column, when one is 0, the other is 1 and vice versa.
**5: Indicator binary data filled**: The mean imputation is done in the same way as above, so the sum for each pair of present×absent columns is 1.
**In these datasets, 1:4 columns are identifiers, 5:8 are still "utility function" variables, 9:56 expanded quntitative variables (each level as column), 57:66 expanded multistate variables, 67:291 binary variables**
