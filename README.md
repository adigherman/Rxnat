# xnatR: Query and retrieves neuroimaging sets from XNAT projects

## What is XNAT?
XNAT is an open source imaging informatics platform developed by the Neuroinformatics Research Group at Washington University. XNAT was originally developed in the Buckner Lab at Washington University, now at Harvard University. It facilitates common management, productivity, and quality assurance tasks for imaging and associated data. Thanks to its extensibility, XNAT can be used to support a wide range of imaging-based projects.

## Who is using XNAT?
There are several projects that currently use XNAT to manage a vast number of datasets. 

* [NITRC](https://nitrc.org) - Neuroimaging Informatics Tools and Resources Clearinghouse is currently a free one-stop-shop collaboratory for science researchers that need resources such as neuroimaging analysis software, publicly available data sets, or computing power. 
* [ConnectomeDB](https://db.humanconnectome.org/) -  The Human Connectome Project (HCP) is a project to construct a map of the complete structural and functional neural connections in vivo within and across individuals. 
* [XNAT Central](https://central.xnat.org) - XNAT Central is a database for sharing neuroimaging and related data with select collaborators or the general community. 

For a more complete list of XNAT implementations around the world you can click [here](https://www.xnat.org/about/xnat-implementations.php).

## Installing the xnatR package

You can install `xnatR` from github with:
``` {r}
# install.packages("devtools")
devtools::install_github("adigherman/xnatR")
```

## Accessing XNAT Data

XNAT projects can be public or private. In order to access a private repository a set of credentials/keys are required. The `xnatR` package will accept 

In order to access the NITRC data, a user account is required. One can be requested [here](https://www.nitrc.org/account/register.php). Some of the projects are public and can be accessed right away, while some others will need an additional access request.