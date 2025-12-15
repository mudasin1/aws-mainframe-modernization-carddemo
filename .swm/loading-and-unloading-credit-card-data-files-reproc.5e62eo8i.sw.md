---
title: Loading and Unloading Credit Card Data Files (REPROC)
---
The REPROC job manages loading and unloading of VSAM files containing credit card data. It processes control instructions to move data records into or out of system datasets, supporting batch jobs that handle credit card account management. For example, it can load updated transaction data into the system for processing.

# Dependencies

```mermaid
graph TD
  iakcg("Repro Test Job (REPRTEST)") --> ibnvu("REPROC"):::currentEntity
click iakcg openCode "samples/jcl/REPRTEST.jcl:1"
2s3qq("TRANREPT") --> ibnvu("REPROC"):::currentEntity
click 2s3qq openCode "app/jcl/TRANREPT.jcl:1"
xpesi("TRANBKP") --> ibnvu("REPROC"):::currentEntity
click xpesi openCode "app/jcl/TRANBKP.jcl:1"
uc2n1("PRTCATBL") --> ibnvu("REPROC"):::currentEntity
click uc2n1 openCode "app/jcl/PRTCATBL.jcl:1"
  
  
click ibnvu openCode "app/proc/REPROC.prc:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   iakcg("Repro Test Job (REPRTEST)") --> ibnvu("REPROC"):::currentEntity
%% click iakcg openCode "<SwmPath>[samples/jcl/REPRTEST.jcl](samples/jcl/REPRTEST.jcl)</SwmPath>:1"
%% 2s3qq("TRANREPT") --> ibnvu("REPROC"):::currentEntity
%% click 2s3qq openCode "<SwmPath>[app/jcl/TRANREPT.jcl](app/jcl/TRANREPT.jcl)</SwmPath>:1"
%% xpesi("TRANBKP") --> ibnvu("REPROC"):::currentEntity
%% click xpesi openCode "<SwmPath>[app/jcl/TRANBKP.jcl](app/jcl/TRANBKP.jcl)</SwmPath>:1"
%% uc2n1("PRTCATBL") --> ibnvu("REPROC"):::currentEntity
%% click uc2n1 openCode "<SwmPath>[app/jcl/PRTCATBL.jcl](app/jcl/PRTCATBL.jcl)</SwmPath>:1"
%%   
%%   
%% click ibnvu openCode "<SwmPath>[app/proc/REPROC.prc](app/proc/REPROC.prc)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

## Manage Credit Card Data Files

Step in this section: `PRC001`.

This section manages the loading or unloading of credit card data files by processing data records as specified by control instructions. It ensures that data can be moved into or extracted from system datasets as needed for credit card account management.

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBYXdzLW1haW5mcmFtZS1tb2Rlcm5pemF0aW9uLWNhcmRkZW1vJTNBJTNBbXVkYXNpbjE=" repo-name="aws-mainframe-modernization-carddemo"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
