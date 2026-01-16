---
title: Missing Dependencies Report
---
# Missing programs

 1. `MQOPEN`, called by:
    1. <SwmPath>[app/…/cbl/CODATE01.cbl](app/app-vsam-mq/cbl/CODATE01.cbl)</SwmPath>
    2. <SwmPath>[app/…/cbl/COACCT01.cbl](app/app-vsam-mq/cbl/COACCT01.cbl)</SwmPath>
    3. <SwmPath>[app/…/cbl/COPAUA0C.cbl](app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl)</SwmPath>
 2. `MQGET`, called by:
    1. <SwmPath>[app/…/cbl/CODATE01.cbl](app/app-vsam-mq/cbl/CODATE01.cbl)</SwmPath>
    2. <SwmPath>[app/…/cbl/COACCT01.cbl](app/app-vsam-mq/cbl/COACCT01.cbl)</SwmPath>
    3. <SwmPath>[app/…/cbl/COPAUA0C.cbl](app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl)</SwmPath>
 3. `MQPUT`, called by:
    1. <SwmPath>[app/…/cbl/CODATE01.cbl](app/app-vsam-mq/cbl/CODATE01.cbl)</SwmPath>
    2. <SwmPath>[app/…/cbl/COACCT01.cbl](app/app-vsam-mq/cbl/COACCT01.cbl)</SwmPath>
 4. `MQCLOSE`, called by:
    1. <SwmPath>[app/…/cbl/CODATE01.cbl](app/app-vsam-mq/cbl/CODATE01.cbl)</SwmPath>
    2. <SwmPath>[app/…/cbl/COACCT01.cbl](app/app-vsam-mq/cbl/COACCT01.cbl)</SwmPath>
    3. <SwmPath>[app/…/cbl/COPAUA0C.cbl](app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl)</SwmPath>
 5. `CEEDAYS`, called by:
    1. <SwmPath>[app/cbl/CSUTLDTC.cbl](app/cbl/CSUTLDTC.cbl)</SwmPath>
 6. `CBLTDLI`, called by:
    1. <SwmPath>[app/…/cbl/PAUDBLOD.CBL](app/app-authorization-ims-db2-mq/cbl/PAUDBLOD.CBL)</SwmPath>
    2. <SwmPath>[app/…/cbl/DBUNLDGS.CBL](app/app-authorization-ims-db2-mq/cbl/DBUNLDGS.CBL)</SwmPath>
    3. <SwmPath>[app/…/cbl/PAUDBUNL.CBL](app/app-authorization-ims-db2-mq/cbl/PAUDBUNL.CBL)</SwmPath>
 7. `CEE3ABD`, called by:
     1. <SwmPath>[app/cbl/CBIMPORT.cbl](app/cbl/CBIMPORT.cbl)</SwmPath>
     2. <SwmPath>[app/cbl/CBEXPORT.cbl](app/cbl/CBEXPORT.cbl)</SwmPath>
     3. <SwmPath>[app/cbl/CBCUS01C.cbl](app/cbl/CBCUS01C.cbl)</SwmPath>
     4. <SwmPath>[app/cbl/CBTRN03C.cbl](app/cbl/CBTRN03C.cbl)</SwmPath>
     5. <SwmPath>[app/cbl/CBTRN02C.cbl](app/cbl/CBTRN02C.cbl)</SwmPath>
     6. <SwmPath>[app/cbl/CBACT01C.cbl](app/cbl/CBACT01C.cbl)</SwmPath>
     7. <SwmPath>[app/cbl/CBACT03C.cbl](app/cbl/CBACT03C.cbl)</SwmPath>
     8. <SwmPath>[app/cbl/CBACT02C.cbl](app/cbl/CBACT02C.cbl)</SwmPath>
     9. <SwmPath>[app/cbl/CBACT04C.cbl](app/cbl/CBACT04C.cbl)</SwmPath>
    10. <SwmPath>[app/cbl/CBSTM03A.CBL](app/cbl/CBSTM03A.CBL)</SwmPath>
    11. <SwmPath>[app/cbl/CBTRN01C.cbl](app/cbl/CBTRN01C.cbl)</SwmPath>
 8. `COBDATFT`, called by:
    1. <SwmPath>[app/cbl/CBACT01C.cbl](app/cbl/CBACT01C.cbl)</SwmPath>
 9. `MVSWAIT`, called by:
    1. <SwmPath>[app/cbl/COBSWAIT.cbl](app/cbl/COBSWAIT.cbl)</SwmPath>
10. `MQPUT1`, called by:
    1. <SwmPath>[app/…/cbl/COPAUA0C.cbl](app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl)</SwmPath>

# Missing copybooks

1. `CMQGMOV`, used by:
   1. <SwmPath>[app/…/cbl/CODATE01.cbl](app/app-vsam-mq/cbl/CODATE01.cbl)</SwmPath>
   2. <SwmPath>[app/…/cbl/COACCT01.cbl](app/app-vsam-mq/cbl/COACCT01.cbl)</SwmPath>
   3. <SwmPath>[app/…/cbl/COPAUA0C.cbl](app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl)</SwmPath>
2. `CMQPMOV`, used by:
   1. <SwmPath>[app/…/cbl/CODATE01.cbl](app/app-vsam-mq/cbl/CODATE01.cbl)</SwmPath>
   2. <SwmPath>[app/…/cbl/COACCT01.cbl](app/app-vsam-mq/cbl/COACCT01.cbl)</SwmPath>
   3. <SwmPath>[app/…/cbl/COPAUA0C.cbl](app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl)</SwmPath>
3. `CMQMDV`, used by:
   1. <SwmPath>[app/…/cbl/CODATE01.cbl](app/app-vsam-mq/cbl/CODATE01.cbl)</SwmPath>
   2. <SwmPath>[app/…/cbl/COACCT01.cbl](app/app-vsam-mq/cbl/COACCT01.cbl)</SwmPath>
   3. <SwmPath>[app/…/cbl/COPAUA0C.cbl](app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl)</SwmPath>
   4. <SwmPath>[app/…/cbl/COPAUA0C.cbl](app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl)</SwmPath>
4. `CMQODV`, used by:
   1. <SwmPath>[app/…/cbl/CODATE01.cbl](app/app-vsam-mq/cbl/CODATE01.cbl)</SwmPath>
   2. <SwmPath>[app/…/cbl/COACCT01.cbl](app/app-vsam-mq/cbl/COACCT01.cbl)</SwmPath>
   3. <SwmPath>[app/…/cbl/COPAUA0C.cbl](app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl)</SwmPath>
   4. <SwmPath>[app/…/cbl/COPAUA0C.cbl](app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl)</SwmPath>
5. `CMQV`, used by:
   1. <SwmPath>[app/…/cbl/CODATE01.cbl](app/app-vsam-mq/cbl/CODATE01.cbl)</SwmPath>
   2. <SwmPath>[app/…/cbl/COACCT01.cbl](app/app-vsam-mq/cbl/COACCT01.cbl)</SwmPath>
   3. <SwmPath>[app/…/cbl/COPAUA0C.cbl](app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl)</SwmPath>
6. `CMQTML`, used by:
   1. <SwmPath>[app/…/cbl/CODATE01.cbl](app/app-vsam-mq/cbl/CODATE01.cbl)</SwmPath>
   2. <SwmPath>[app/…/cbl/COACCT01.cbl](app/app-vsam-mq/cbl/COACCT01.cbl)</SwmPath>
   3. <SwmPath>[app/…/cbl/COPAUA0C.cbl](app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl)</SwmPath>
7. `DFHAID`, used by:
    1. <SwmPath>[app/cbl/COUSR03C.cbl](app/cbl/COUSR03C.cbl)</SwmPath>
    2. <SwmPath>[app/cbl/COTRN02C.cbl](app/cbl/COTRN02C.cbl)</SwmPath>
    3. <SwmPath>[app/cbl/COUSR01C.cbl](app/cbl/COUSR01C.cbl)</SwmPath>
    4. <SwmPath>[app/…/cbl/COTRTLIC.cbl](app/app-transaction-type-db2/cbl/COTRTLIC.cbl)</SwmPath>
    5. <SwmPath>[app/cbl/COUSR02C.cbl](app/cbl/COUSR02C.cbl)</SwmPath>
    6. <SwmPath>[app/cbl/COCRDSLC.cbl](app/cbl/COCRDSLC.cbl)</SwmPath>
    7. <SwmPath>[app/cbl/COTRN01C.cbl](app/cbl/COTRN01C.cbl)</SwmPath>
    8. <SwmPath>[app/cbl/CORPT00C.cbl](app/cbl/CORPT00C.cbl)</SwmPath>
    9. <SwmPath>[app/cbl/COTRN00C.cbl](app/cbl/COTRN00C.cbl)</SwmPath>
   10. <SwmPath>[app/cbl/COADM01C.cbl](app/cbl/COADM01C.cbl)</SwmPath>
   11. <SwmPath>[app/cbl/COSGN00C.cbl](app/cbl/COSGN00C.cbl)</SwmPath>
   12. <SwmPath>[app/cbl/COMEN01C.cbl](app/cbl/COMEN01C.cbl)</SwmPath>
   13. <SwmPath>[app/cbl/COBIL00C.cbl](app/cbl/COBIL00C.cbl)</SwmPath>
   14. <SwmPath>[app/cbl/COUSR00C.cbl](app/cbl/COUSR00C.cbl)</SwmPath>
   15. <SwmPath>[app/cbl/COCRDUPC.cbl](app/cbl/COCRDUPC.cbl)</SwmPath>
   16. <SwmPath>[app/…/cbl/COPAUS1C.cbl](app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl)</SwmPath>
   17. <SwmPath>[app/cbl/COCRDLIC.cbl](app/cbl/COCRDLIC.cbl)</SwmPath>
   18. <SwmPath>[app/…/cbl/COPAUS0C.cbl](app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl)</SwmPath>
   19. <SwmPath>[app/…/cbl/COTRTUPC.cbl](app/app-transaction-type-db2/cbl/COTRTUPC.cbl)</SwmPath>
   20. <SwmPath>[app/cbl/COACTUPC.cbl](app/cbl/COACTUPC.cbl)</SwmPath>
   21. <SwmPath>[app/cbl/COACTVWC.cbl](app/cbl/COACTVWC.cbl)</SwmPath>
8. `DFHBMSCA`, used by:
    1. <SwmPath>[app/cbl/COUSR03C.cbl](app/cbl/COUSR03C.cbl)</SwmPath>
    2. <SwmPath>[app/cbl/COTRN02C.cbl](app/cbl/COTRN02C.cbl)</SwmPath>
    3. <SwmPath>[app/cbl/COUSR01C.cbl](app/cbl/COUSR01C.cbl)</SwmPath>
    4. <SwmPath>[app/…/cbl/COTRTLIC.cbl](app/app-transaction-type-db2/cbl/COTRTLIC.cbl)</SwmPath>
    5. <SwmPath>[app/cbl/COUSR02C.cbl](app/cbl/COUSR02C.cbl)</SwmPath>
    6. <SwmPath>[app/cbl/COCRDSLC.cbl](app/cbl/COCRDSLC.cbl)</SwmPath>
    7. <SwmPath>[app/cbl/COTRN01C.cbl](app/cbl/COTRN01C.cbl)</SwmPath>
    8. <SwmPath>[app/cbl/CORPT00C.cbl](app/cbl/CORPT00C.cbl)</SwmPath>
    9. <SwmPath>[app/cbl/COTRN00C.cbl](app/cbl/COTRN00C.cbl)</SwmPath>
   10. <SwmPath>[app/cbl/COADM01C.cbl](app/cbl/COADM01C.cbl)</SwmPath>
   11. <SwmPath>[app/cbl/COSGN00C.cbl](app/cbl/COSGN00C.cbl)</SwmPath>
   12. <SwmPath>[app/cbl/COMEN01C.cbl](app/cbl/COMEN01C.cbl)</SwmPath>
   13. <SwmPath>[app/cbl/COBIL00C.cbl](app/cbl/COBIL00C.cbl)</SwmPath>
   14. <SwmPath>[app/cbl/COUSR00C.cbl](app/cbl/COUSR00C.cbl)</SwmPath>
   15. <SwmPath>[app/cbl/COCRDUPC.cbl](app/cbl/COCRDUPC.cbl)</SwmPath>
   16. <SwmPath>[app/…/cbl/COPAUS1C.cbl](app/app-authorization-ims-db2-mq/cbl/COPAUS1C.cbl)</SwmPath>
   17. <SwmPath>[app/cbl/COCRDLIC.cbl](app/cbl/COCRDLIC.cbl)</SwmPath>
   18. <SwmPath>[app/…/cbl/COPAUS0C.cbl](app/app-authorization-ims-db2-mq/cbl/COPAUS0C.cbl)</SwmPath>
   19. <SwmPath>[app/…/cbl/COTRTUPC.cbl](app/app-transaction-type-db2/cbl/COTRTUPC.cbl)</SwmPath>
   20. <SwmPath>[app/cbl/COACTUPC.cbl](app/cbl/COACTUPC.cbl)</SwmPath>
   21. <SwmPath>[app/cbl/COACTVWC.cbl](app/cbl/COACTVWC.cbl)</SwmPath>
9. `SQLCA`, used by:
   1. <SwmPath>[app/…/cbl/COPAUS2C.cbl](app/app-authorization-ims-db2-mq/cbl/COPAUS2C.cbl)</SwmPath>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBYXdzLW1haW5mcmFtZS1tb2Rlcm5pemF0aW9uLWNhcmRkZW1vJTNBJTNBbXVkYXNpbjE=" repo-name="aws-mainframe-modernization-carddemo"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
