# PhysAccel
The PhysAccel package provides the option to generate different Physical Activity metrics from unprocessed acceleration (Axivity cwa files) and functionality to derive day by day or average day summary statistics.

A dedicated software for generating PA metrics from cwa files has been developed in Matlab and is thus required for this package to work. The software can be downloaded from

<a href="http://video4coach.com/software/SDU/PhysAccel_mcr_Win.zip">Windows</a>

<a href="http://video4coach.com/software/SDU/PhysAccel_mcr_OSX.zip">OSX</a>

<a href="http://video4coach.com/software/SDU/PhysAccel_mcr_Linux.zip">Linux</a>

The Matlab software provides the option to generate ActiGraph counts (AG), Actigraph counts with intermittent adjustment (AGi), Mean Average Devation (MAD), Euclidean Norm Minus one (ENMO) and Activity type classification using thigh acceleration (Skotte et al 2014 and Brønd et al. 2019). All data is stored in individual csv files (ActiLife csv format and header). The files are named as the cwa file but adding an _ag, _agi, _mad, _enmo or _skotte.

This wrapper is created by Jan Christian Brønd (Phd, Msc.), Esben Lykke Skovgaard and Malthe Andreas Roswall (University of Southern Denmark, Department RICH/EXE, Institute of sports science and clinical biomechanics)
<b>Requirements</b>
<br>
Installing the package requires the devtools package. Use the following command to install the package:

install.packages("devtools")


<B>Installing the PhysAccel package</B>
<br>
The R package can be installed using the following command

devtools::install_github("jbrond/PhysAccel")
