# PhysAccel
Generating Physical Activity metrics from R using Matlab compiled software

The Matlab software for processing the Axivity CWA files can be downloaded from

<a href="http://video4coach.com/software/SDU/PhysAccel_mcr_Win.zip">Windows</a>

<a href="http://video4coach.com/software/SDU/PhysAccel_mcr_OSX.zip">OSX</a>

<a href="http://video4coach.com/software/SDU/PhysAccel_mcr_Linux.zip">Linux</a>

With the software it is possible to generate ActiGraph counts (AG), Actigraph counts with intermittent adjustemt (AGi), Mean Average Devation (MAD), Euclidean Norm Minus one (ENMO) and Activity type classification using thigh acceleration (Skotte et al 2014 and Brønd et al. 2019). All data is stored in individual csv files (ActiLife csv format and header) with the same name as the cwa file but adding an _ag, _agi, _mad, _enmo or _skotte.

This wrapper is created by Jan Christian Brønd (Phd, Msc.)
University of Southern Denmark

<b>Requirements</b>
<br>
Installing the package requires the devtools package. Use the following command to install the package:

install.packages("devtools")


<B>Installing the PhysAccel package</B>
<br>
The R package can be installed using the following command

devtools::install_github("jbrond/PhysAccel")
