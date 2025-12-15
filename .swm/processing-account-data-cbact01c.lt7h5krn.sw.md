---
title: Processing Account Data (CBACT01C)
---
# Overview

This document describes the flow of processing account records in the credit card management system. Account data is read from an input file, transformed according to business rules, and written to multiple output files for downstream use.

## Dependencies

### Programs

- <SwmToken path="app/cbl/CBACT01C.cbl" pos="141:14:14" line-data="           DISPLAY &#39;START OF EXECUTION OF PROGRAM CBACT01C&#39;.">`CBACT01C`</SwmToken> (<SwmPath>[app/cbl/CBACT01C.cbl](app/cbl/CBACT01C.cbl)</SwmPath>)
- COBDATFT
- <SwmToken path="app/cbl/CBACT01C.cbl" pos="410:4:4" line-data="           CALL &#39;CEE3ABD&#39; USING ABCODE, TIMING.">`CEE3ABD`</SwmToken>

### Copybooks

- <SwmToken path="app/cbl/CBACT01C.cbl" pos="89:3:3" line-data="       COPY CVACT01Y.">`CVACT01Y`</SwmToken> (<SwmPath>[scripts/markers/CVACT01Y](scripts/markers/CVACT01Y)</SwmPath>)
- CODATECN (<SwmPath>[app/cpy/CODATECN.cpy](app/cpy/CODATECN.cpy)</SwmPath>)

# Where is this program used?

This program is used once, as represented in the following diagram:

```mermaid
graph TD
  w2h8z("READACCT") --> cqhdr("Processing Account Data (CBACT01C)"):::currentEntity
click w2h8z openCode "app/jcl/READACCT.jcl:1"
  
  
click cqhdr openCode "app/cbl/CBACT01C.cbl:1"
    classDef currentEntity color:#000000,fill:#7CB9F4

%% Swimm:
%% graph TD
%%   w2h8z("READACCT") --> cqhdr("Processing Account Data (<SwmToken path="app/cbl/CBACT01C.cbl" pos="141:14:14" line-data="           DISPLAY &#39;START OF EXECUTION OF PROGRAM CBACT01C&#39;.">`CBACT01C`</SwmToken>)"):::currentEntity
%% click w2h8z openCode "<SwmPath>[app/jcl/READACCT.jcl](app/jcl/READACCT.jcl)</SwmPath>:1"
%%   
%%   
%% click cqhdr openCode "<SwmPath>[app/cbl/CBACT01C.cbl](app/cbl/CBACT01C.cbl)</SwmPath>:1"
%%     classDef currentEntity color:#000000,fill:#7CB9F4
```

## Input and Output Tables/Files used in the Program

| Table / File Name                                                                                                                             | Type | Description                                           | Usage Mode | Key Fields / Layout Highlights |
| --------------------------------------------------------------------------------------------------------------------------------------------- | ---- | ----------------------------------------------------- | ---------- | ------------------------------ |
| <SwmToken path="app/cbl/CBACT01C.cbl" pos="166:3:5" line-data="           READ ACCTFILE-FILE INTO ACCOUNT-RECORD.">`ACCTFILE-FILE`</SwmToken> | File | Indexed file of credit card account master records    | Input      | File resource                  |
| <SwmToken path="app/cbl/CBACT01C.cbl" pos="169:3:7" line-data="               INITIALIZE ARR-ARRAY-REC">`ARR-ARRAY-REC`</SwmToken>            | File | Output record with account ID and period balances     | Output     | File resource                  |
| <SwmToken path="app/cbl/CBACT01C.cbl" pos="354:5:7" line-data="           OPEN OUTPUT ARRY-FILE">`ARRY-FILE`</SwmToken>                       | File | Sequential file for account balances across periods   | Output     | File resource                  |
| <SwmToken path="app/cbl/CBACT01C.cbl" pos="243:3:7" line-data="           WRITE OUT-ACCT-REC.">`OUT-ACCT-REC`</SwmToken>                      | File | Output record with account summary and status fields  | Output     | File resource                  |
| <SwmToken path="app/cbl/CBACT01C.cbl" pos="336:5:7" line-data="           OPEN OUTPUT OUT-FILE">`OUT-FILE`</SwmToken>                         | File | Sequential file for processed account summary records | Output     | File resource                  |
| <SwmToken path="app/cbl/CBACT01C.cbl" pos="289:9:11" line-data="           MOVE VBRC-REC1 TO VBR-REC(1:WS-RECD-LEN).">`VBR-REC`</SwmToken>    | File | Variable-length record for account ID and key fields  | Output     | File resource                  |
| <SwmToken path="app/cbl/CBACT01C.cbl" pos="372:5:7" line-data="           OPEN OUTPUT VBRC-FILE">`VBRC-FILE`</SwmToken>                       | File | Variable-length file for compact account extracts     | Output     | File resource                  |

&nbsp;

# Workflow

# Starting the Account Processing Routine

```mermaid
%%{init: {"flowchart": {"defaultRenderer": "elk"}} }%%
flowchart TD
    node1["Start program and display start message"]
    click node1 openCode "app/cbl/CBACT01C.cbl:140:142"
    node1 --> node2["Opening and Validating the Account File"]
    
    node2 --> node3["Opening the Output File for Account Data"]
    
    node3 --> node4["Opening the Array Output File"]
    
    node4 --> node6["Opening the Variable Block Output File"]
    
    node6 --> node7["Begin account record processing"]
    click node7 openCode "app/cbl/CBACT01C.cbl:147:154"
    subgraph loop1["For each account record until END-OF-FILE = 'Y'"]
        node7 --> node10{"Is END-OF-FILE = 'N'?"}
        click node10 openCode "app/cbl/CBACT01C.cbl:148:153"
        node10 -->|"Yes"| node8["Reading and Processing the Next Account Record"]
        
        node8 --> node11{"Is END-OF-FILE = 'N' after reading?"}
        click node11 openCode "app/cbl/CBACT01C.cbl:150:152"
        node11 -->|"Yes"| node12["Display account record"]
        click node12 openCode "app/cbl/CBACT01C.cbl:151:152"
        node12 --> node7
        node11 -->|"No"| node7
        node10 -->|"No"| node9["Close files and display end message"]
    end
    click node9 openCode "app/cbl/CBACT01C.cbl:156:160"

classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
click node2 goToHeading "Opening and Validating the Account File"
node2:::HeadingStyle
click node3 goToHeading "Opening the Output File for Account Data"
node3:::HeadingStyle
click node4 goToHeading "Opening the Array Output File"
node4:::HeadingStyle
click node6 goToHeading "Opening the Variable Block Output File"
node6:::HeadingStyle
click node8 goToHeading "Reading and Processing the Next Account Record"
node8:::HeadingStyle

%% Swimm:
%% %%{init: {"flowchart": {"defaultRenderer": "elk"}} }%%
%% flowchart TD
%%     node1["Start program and display start message"]
%%     click node1 openCode "<SwmPath>[app/cbl/CBACT01C.cbl](app/cbl/CBACT01C.cbl)</SwmPath>:140:142"
%%     node1 --> node2["Opening and Validating the Account File"]
%%     
%%     node2 --> node3["Opening the Output File for Account Data"]
%%     
%%     node3 --> node4["Opening the Array Output File"]
%%     
%%     node4 --> node6["Opening the Variable Block Output File"]
%%     
%%     node6 --> node7["Begin account record processing"]
%%     click node7 openCode "<SwmPath>[app/cbl/CBACT01C.cbl](app/cbl/CBACT01C.cbl)</SwmPath>:147:154"
%%     subgraph loop1["For each account record until <SwmToken path="app/cbl/CBACT01C.cbl" pos="147:5:9" line-data="           PERFORM UNTIL END-OF-FILE = &#39;Y&#39;">`END-OF-FILE`</SwmToken> = 'Y'"]
%%         node7 --> node10{"Is <SwmToken path="app/cbl/CBACT01C.cbl" pos="147:5:9" line-data="           PERFORM UNTIL END-OF-FILE = &#39;Y&#39;">`END-OF-FILE`</SwmToken> = 'N'?"}
%%         click node10 openCode "<SwmPath>[app/cbl/CBACT01C.cbl](app/cbl/CBACT01C.cbl)</SwmPath>:148:153"
%%         node10 -->|"Yes"| node8["Reading and Processing the Next Account Record"]
%%         
%%         node8 --> node11{"Is <SwmToken path="app/cbl/CBACT01C.cbl" pos="147:5:9" line-data="           PERFORM UNTIL END-OF-FILE = &#39;Y&#39;">`END-OF-FILE`</SwmToken> = 'N' after reading?"}
%%         click node11 openCode "<SwmPath>[app/cbl/CBACT01C.cbl](app/cbl/CBACT01C.cbl)</SwmPath>:150:152"
%%         node11 -->|"Yes"| node12["Display account record"]
%%         click node12 openCode "<SwmPath>[app/cbl/CBACT01C.cbl](app/cbl/CBACT01C.cbl)</SwmPath>:151:152"
%%         node12 --> node7
%%         node11 -->|"No"| node7
%%         node10 -->|"No"| node9["Close files and display end message"]
%%     end
%%     click node9 openCode "<SwmPath>[app/cbl/CBACT01C.cbl](app/cbl/CBACT01C.cbl)</SwmPath>:156:160"
%% 
%% classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
%% click node2 goToHeading "Opening and Validating the Account File"
%% node2:::HeadingStyle
%% click node3 goToHeading "Opening the Output File for Account Data"
%% node3:::HeadingStyle
%% click node4 goToHeading "Opening the Array Output File"
%% node4:::HeadingStyle
%% click node6 goToHeading "Opening the Variable Block Output File"
%% node6:::HeadingStyle
%% click node8 goToHeading "Reading and Processing the Next Account Record"
%% node8:::HeadingStyle
```

This section ensures that all required files are successfully opened and validated before any account processing begins. It governs the flow of account record processing, including error handling for file operations and the orderly processing of each account record until completion or error.

| Category        | Rule Name                     | Description                                                                                                                                                                                                                                                                              |
| --------------- | ----------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Data validation | Account File Validation       | The account input file must be successfully opened and validated before any processing can occur. If the file cannot be opened, the program must terminate and display an error message.                                                                                                 |
| Data validation | Output File Validation        | All required output files (account data, array output, variable block output) must be successfully opened before processing begins. If any file cannot be opened, the program must terminate and display an error message.                                                               |
| Business logic  | Start Message Display         | The program must display a start message to indicate the beginning of the account processing routine.                                                                                                                                                                                    |
| Business logic  | Sequential Account Processing | Each account record must be processed in sequence until the end-of-file condition is reached, as indicated by the <SwmToken path="app/cbl/CBACT01C.cbl" pos="147:5:9" line-data="           PERFORM UNTIL END-OF-FILE = &#39;Y&#39;">`END-OF-FILE`</SwmToken> variable being set to 'Y'. |
| Business logic  | End Message and File Closure  | Upon reaching the end of the account file, the program must close all open files and display an end message to indicate successful completion of processing.                                                                                                                             |

<SwmSnippet path="/app/cbl/CBACT01C.cbl" line="140">

---

In <SwmToken path="app/cbl/CBACT01C.cbl" pos="140:1:3" line-data="       PROCEDURE DIVISION.">`PROCEDURE DIVISION`</SwmToken>, we start by opening the account file to make sure we can read data before doing anything else. If it fails, we bail early.

```cobol
       PROCEDURE DIVISION.
           DISPLAY 'START OF EXECUTION OF PROGRAM CBACT01C'.
           PERFORM 0000-ACCTFILE-OPEN.
           PERFORM 2000-OUTFILE-OPEN.
           PERFORM 3000-ARRFILE-OPEN.
           PERFORM 4000-VBRFILE-OPEN.
```

---

</SwmSnippet>

## Opening and Validating the Account File

```mermaid
%%{init: {"flowchart": {"defaultRenderer": "elk"}} }%%
flowchart TD
    node1["Attempt to open account file"] --> node2{"Is ACCTFILE-STATUS '00'?"}
    click node1 openCode "app/cbl/CBACT01C.cbl:317:319"
    node2 -->|"Yes"| node3["Set business status to OK (APPL-RESULT = 0)"]
    click node2 openCode "app/cbl/CBACT01C.cbl:320:324"
    click node3 openCode "app/cbl/CBACT01C.cbl:321:321"
    node2 -->|"No"| node4["Set business status to error (APPL-RESULT = 12)"]
    click node4 openCode "app/cbl/CBACT01C.cbl:323:323"
    node3 --> node5{"Is business status OK (APPL-AOK)?"}
    node4 --> node5
    click node5 openCode "app/cbl/CBACT01C.cbl:325:332"
    node5 -->|"Yes"| node6["Continue"]
    click node6 openCode "app/cbl/CBACT01C.cbl:326:326"
    node5 -->|"No"| node7["Display error and abort program"]
    click node7 openCode "app/cbl/CBACT01C.cbl:328:331"
    node7 --> node8["Display file I/O status"]
    click node8 openCode "app/cbl/CBACT01C.cbl:413:426"
    node8 --> node9["Abort program"]
    click node9 openCode "app/cbl/CBACT01C.cbl:331:331"
classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;

%% Swimm:
%% %%{init: {"flowchart": {"defaultRenderer": "elk"}} }%%
%% flowchart TD
%%     node1["Attempt to open account file"] --> node2{"Is <SwmToken path="app/cbl/CBACT01C.cbl" pos="167:3:5" line-data="           IF  ACCTFILE-STATUS = &#39;00&#39;">`ACCTFILE-STATUS`</SwmToken> '00'?"}
%%     click node1 openCode "<SwmPath>[app/cbl/CBACT01C.cbl](app/cbl/CBACT01C.cbl)</SwmPath>:317:319"
%%     node2 -->|"Yes"| node3["Set business status to OK (<SwmToken path="app/cbl/CBACT01C.cbl" pos="168:7:9" line-data="               MOVE 0 TO APPL-RESULT">`APPL-RESULT`</SwmToken> = 0)"]
%%     click node2 openCode "<SwmPath>[app/cbl/CBACT01C.cbl](app/cbl/CBACT01C.cbl)</SwmPath>:320:324"
%%     click node3 openCode "<SwmPath>[app/cbl/CBACT01C.cbl](app/cbl/CBACT01C.cbl)</SwmPath>:321:321"
%%     node2 -->|"No"| node4["Set business status to error (<SwmToken path="app/cbl/CBACT01C.cbl" pos="168:7:9" line-data="               MOVE 0 TO APPL-RESULT">`APPL-RESULT`</SwmToken> = 12)"]
%%     click node4 openCode "<SwmPath>[app/cbl/CBACT01C.cbl](app/cbl/CBACT01C.cbl)</SwmPath>:323:323"
%%     node3 --> node5{"Is business status OK (<SwmToken path="app/cbl/CBACT01C.cbl" pos="186:3:5" line-data="           IF  APPL-AOK">`APPL-AOK`</SwmToken>)?"}
%%     node4 --> node5
%%     click node5 openCode "<SwmPath>[app/cbl/CBACT01C.cbl](app/cbl/CBACT01C.cbl)</SwmPath>:325:332"
%%     node5 -->|"Yes"| node6["Continue"]
%%     click node6 openCode "<SwmPath>[app/cbl/CBACT01C.cbl](app/cbl/CBACT01C.cbl)</SwmPath>:326:326"
%%     node5 -->|"No"| node7["Display error and abort program"]
%%     click node7 openCode "<SwmPath>[app/cbl/CBACT01C.cbl](app/cbl/CBACT01C.cbl)</SwmPath>:328:331"
%%     node7 --> node8["Display file <SwmToken path="app/cbl/CBACT01C.cbl" pos="163:3:5" line-data="      * I/O ROUTINES TO ACCESS A KSDS, VSAM DATA SET...               *">`I/O`</SwmToken> status"]
%%     click node8 openCode "<SwmPath>[app/cbl/CBACT01C.cbl](app/cbl/CBACT01C.cbl)</SwmPath>:413:426"
%%     node8 --> node9["Abort program"]
%%     click node9 openCode "<SwmPath>[app/cbl/CBACT01C.cbl](app/cbl/CBACT01C.cbl)</SwmPath>:331:331"
%% classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
```

This section ensures that the account file is available for processing by validating the file open operation and handling any errors that occur during this step. It is a critical gatekeeper for subsequent business logic that depends on file access.

| Category        | Rule Name                 | Description                                                                                                                                                                                                                                                                                                     |
| --------------- | ------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Data validation | Successful File Open      | If the account file is successfully opened, the business status must be set to OK (<SwmToken path="app/cbl/CBACT01C.cbl" pos="168:7:9" line-data="               MOVE 0 TO APPL-RESULT">`APPL-RESULT`</SwmToken> = 0).                                                                                          |
| Data validation | Gatekeeper for Processing | Processing must only continue if the business status is OK (<SwmToken path="app/cbl/CBACT01C.cbl" pos="168:7:9" line-data="               MOVE 0 TO APPL-RESULT">`APPL-RESULT`</SwmToken> = 0 / <SwmToken path="app/cbl/CBACT01C.cbl" pos="186:3:5" line-data="           IF  APPL-AOK">`APPL-AOK`</SwmToken>). |

<SwmSnippet path="/app/cbl/CBACT01C.cbl" line="317">

---

In <SwmToken path="app/cbl/CBACT01C.cbl" pos="317:1:5" line-data="       0000-ACCTFILE-OPEN.">`0000-ACCTFILE-OPEN`</SwmToken> we set up a status flag, try to open the account file for input, and update <SwmToken path="app/cbl/CBACT01C.cbl" pos="318:7:9" line-data="           MOVE 8 TO APPL-RESULT.">`APPL-RESULT`</SwmToken> based on whether the open succeeded or failed. This sets up the next error handling step if needed.

```cobol
       0000-ACCTFILE-OPEN.
           MOVE 8 TO APPL-RESULT.
           OPEN INPUT ACCTFILE-FILE
           IF  ACCTFILE-STATUS = '00'
               MOVE 0 TO APPL-RESULT
           ELSE
               MOVE 12 TO APPL-RESULT
           END-IF
```

---

</SwmSnippet>

<SwmSnippet path="/app/cbl/CBACT01C.cbl" line="325">

---

After trying to open the account file, if it fails, we display an error, copy the file status to <SwmToken path="app/cbl/CBACT01C.cbl" pos="329:9:11" line-data="               MOVE ACCTFILE-STATUS TO IO-STATUS">`IO-STATUS`</SwmToken>, and call <SwmToken path="app/cbl/CBACT01C.cbl" pos="330:3:9" line-data="               PERFORM 9910-DISPLAY-IO-STATUS">`9910-DISPLAY-IO-STATUS`</SwmToken> to show the exact IO error before terminating. This makes error tracking easier.

```cobol
           IF  APPL-AOK
               CONTINUE
           ELSE
               DISPLAY 'ERROR OPENING ACCTFILE'
               MOVE ACCTFILE-STATUS TO IO-STATUS
               PERFORM 9910-DISPLAY-IO-STATUS
               PERFORM 9999-ABEND-PROGRAM
           END-IF
           EXIT.
```

---

</SwmSnippet>

<SwmSnippet path="/app/cbl/CBACT01C.cbl" line="413">

---

<SwmToken path="app/cbl/CBACT01C.cbl" pos="413:1:7" line-data="       9910-DISPLAY-IO-STATUS.">`9910-DISPLAY-IO-STATUS`</SwmToken> checks if the IO status is non-numeric or starts with '9', then builds a custom status code for display using <SwmToken path="app/cbl/CBACT01C.cbl" pos="416:9:13" line-data="               MOVE IO-STAT1 TO IO-STATUS-04(1:1)">`IO-STATUS-04`</SwmToken> and <SwmToken path="app/cbl/CBACT01C.cbl" pos="417:7:11" line-data="               MOVE 0        TO TWO-BYTES-BINARY">`TWO-BYTES-BINARY`</SwmToken>. Otherwise, it resets the status and displays it. This makes error codes easier to interpret.

```cobol
       9910-DISPLAY-IO-STATUS.
           IF  IO-STATUS NOT NUMERIC
           OR  IO-STAT1 = '9'
               MOVE IO-STAT1 TO IO-STATUS-04(1:1)
               MOVE 0        TO TWO-BYTES-BINARY
               MOVE IO-STAT2 TO TWO-BYTES-RIGHT
               MOVE TWO-BYTES-BINARY TO IO-STATUS-0403
               DISPLAY 'FILE STATUS IS: NNNN' IO-STATUS-04
           ELSE
               MOVE '0000' TO IO-STATUS-04
               MOVE IO-STATUS TO IO-STATUS-04(3:2)
               DISPLAY 'FILE STATUS IS: NNNN' IO-STATUS-04
           END-IF
           EXIT.
```

---

</SwmSnippet>

## Opening the Output File for Account Data

```mermaid
%%{init: {"flowchart": {"defaultRenderer": "elk"}} }%%
flowchart TD
    node1["Mark file as not yet opened (result code: 8)"] --> node2["Attempt to open output file"]
    click node1 openCode "app/cbl/CBACT01C.cbl:335:335"
    click node2 openCode "app/cbl/CBACT01C.cbl:336:336"
    node2 --> node3{"Did file open succeed?"}
    click node3 openCode "app/cbl/CBACT01C.cbl:337:341"
    node3 -->|"OUTFILE-STATUS = '00'"| node4["Set result code to Success (0)"]
    click node4 openCode "app/cbl/CBACT01C.cbl:338:338"
    node3 -->|"OUTFILE-STATUS != '00'"| node5["Set result code to Failure (12)"]
    click node5 openCode "app/cbl/CBACT01C.cbl:340:340"
    node4 --> node6{"Is result code Success (0)?"}
    node5 --> node6
    click node6 openCode "app/cbl/CBACT01C.cbl:342:349"
    node6 -->|"APPL-RESULT = 0"| node7["Continue"]
    click node7 openCode "app/cbl/CBACT01C.cbl:343:343"
    node6 -->|"APPL-RESULT != 0"| node8["Show error message"]
    click node8 openCode "app/cbl/CBACT01C.cbl:345:345"
    node8 --> node9["Display IO status"]
    click node9 openCode "app/cbl/CBACT01C.cbl:346:347"
    node9 --> node10["Abort program"]
    click node10 openCode "app/cbl/CBACT01C.cbl:348:348"

classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;

%% Swimm:
%% %%{init: {"flowchart": {"defaultRenderer": "elk"}} }%%
%% flowchart TD
%%     node1["Mark file as not yet opened (result code: 8)"] --> node2["Attempt to open output file"]
%%     click node1 openCode "<SwmPath>[app/cbl/CBACT01C.cbl](app/cbl/CBACT01C.cbl)</SwmPath>:335:335"
%%     click node2 openCode "<SwmPath>[app/cbl/CBACT01C.cbl](app/cbl/CBACT01C.cbl)</SwmPath>:336:336"
%%     node2 --> node3{"Did file open succeed?"}
%%     click node3 openCode "<SwmPath>[app/cbl/CBACT01C.cbl](app/cbl/CBACT01C.cbl)</SwmPath>:337:341"
%%     node3 -->|"<SwmToken path="app/cbl/CBACT01C.cbl" pos="245:3:5" line-data="           IF OUTFILE-STATUS NOT = &#39;00&#39; AND OUTFILE-STATUS NOT = &#39;10&#39;">`OUTFILE-STATUS`</SwmToken> = '00'"| node4["Set result code to Success (0)"]
%%     click node4 openCode "<SwmPath>[app/cbl/CBACT01C.cbl](app/cbl/CBACT01C.cbl)</SwmPath>:338:338"
%%     node3 -->|"<SwmToken path="app/cbl/CBACT01C.cbl" pos="245:3:5" line-data="           IF OUTFILE-STATUS NOT = &#39;00&#39; AND OUTFILE-STATUS NOT = &#39;10&#39;">`OUTFILE-STATUS`</SwmToken> != '00'"| node5["Set result code to Failure (12)"]
%%     click node5 openCode "<SwmPath>[app/cbl/CBACT01C.cbl](app/cbl/CBACT01C.cbl)</SwmPath>:340:340"
%%     node4 --> node6{"Is result code Success (0)?"}
%%     node5 --> node6
%%     click node6 openCode "<SwmPath>[app/cbl/CBACT01C.cbl](app/cbl/CBACT01C.cbl)</SwmPath>:342:349"
%%     node6 -->|"<SwmToken path="app/cbl/CBACT01C.cbl" pos="168:7:9" line-data="               MOVE 0 TO APPL-RESULT">`APPL-RESULT`</SwmToken> = 0"| node7["Continue"]
%%     click node7 openCode "<SwmPath>[app/cbl/CBACT01C.cbl](app/cbl/CBACT01C.cbl)</SwmPath>:343:343"
%%     node6 -->|"<SwmToken path="app/cbl/CBACT01C.cbl" pos="168:7:9" line-data="               MOVE 0 TO APPL-RESULT">`APPL-RESULT`</SwmToken> != 0"| node8["Show error message"]
%%     click node8 openCode "<SwmPath>[app/cbl/CBACT01C.cbl](app/cbl/CBACT01C.cbl)</SwmPath>:345:345"
%%     node8 --> node9["Display IO status"]
%%     click node9 openCode "<SwmPath>[app/cbl/CBACT01C.cbl](app/cbl/CBACT01C.cbl)</SwmPath>:346:347"
%%     node9 --> node10["Abort program"]
%%     click node10 openCode "<SwmPath>[app/cbl/CBACT01C.cbl](app/cbl/CBACT01C.cbl)</SwmPath>:348:348"
%% 
%% classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
```

This section governs how the system attempts to open the output file for account data, determines the outcome, and handles any errors that may occur during the process. It ensures that the file is ready for subsequent operations or that failures are clearly communicated and handled.

| Category        | Rule Name            | Description                                                                            |
| --------------- | -------------------- | -------------------------------------------------------------------------------------- |
| Data validation | Successful File Open | If the output file is opened successfully, the result code must be set to 0 (success). |
| Data validation | File Open Failure    | If the output file fails to open, the result code must be set to 12 (failure).         |

<SwmSnippet path="/app/cbl/CBACT01C.cbl" line="334">

---

In <SwmToken path="app/cbl/CBACT01C.cbl" pos="334:1:5" line-data="       2000-OUTFILE-OPEN.">`2000-OUTFILE-OPEN`</SwmToken> we set <SwmToken path="app/cbl/CBACT01C.cbl" pos="335:7:9" line-data="           MOVE 8 TO APPL-RESULT.">`APPL-RESULT`</SwmToken> to 8, try to open the output file, and update the status to 0 or 12 depending on the result. This sets up error handling if the file can't be opened.

```cobol
       2000-OUTFILE-OPEN.
           MOVE 8 TO APPL-RESULT.
           OPEN OUTPUT OUT-FILE
           IF   OUTFILE-STATUS = '00'
               MOVE 0 TO APPL-RESULT
           ELSE
               MOVE 12 TO APPL-RESULT
           END-IF
```

---

</SwmSnippet>

<SwmSnippet path="/app/cbl/CBACT01C.cbl" line="342">

---

After trying to open the output file, if it fails, we display the error status, copy <SwmToken path="app/cbl/CBACT01C.cbl" pos="345:11:13" line-data="               DISPLAY &#39;ERROR OPENING OUTFILE&#39;  OUTFILE-STATUS">`OUTFILE-STATUS`</SwmToken> to <SwmToken path="app/cbl/CBACT01C.cbl" pos="346:9:11" line-data="               MOVE  OUTFILE-STATUS TO IO-STATUS">`IO-STATUS`</SwmToken>, and call <SwmToken path="app/cbl/CBACT01C.cbl" pos="347:3:9" line-data="               PERFORM 9910-DISPLAY-IO-STATUS">`9910-DISPLAY-IO-STATUS`</SwmToken> for diagnostics before terminating. This helps pinpoint file issues.

```cobol
           IF  APPL-AOK
               CONTINUE
           ELSE
               DISPLAY 'ERROR OPENING OUTFILE'  OUTFILE-STATUS
               MOVE  OUTFILE-STATUS TO IO-STATUS
               PERFORM 9910-DISPLAY-IO-STATUS
               PERFORM 9999-ABEND-PROGRAM
           END-IF
           EXIT.
```

---

</SwmSnippet>

## Opening the Array Output File

```mermaid
%%{init: {"flowchart": {"defaultRenderer": "elk"}} }%%
flowchart TD
    node1["Set initial business status"]
    click node1 openCode "app/cbl/CBACT01C.cbl:353:353"
    node1 --> node2["Attempt to open array file for output"]
    click node2 openCode "app/cbl/CBACT01C.cbl:354:354"
    node2 --> node3{"Was file opened successfully?"}
    click node3 openCode "app/cbl/CBACT01C.cbl:355:359"
    node3 -->|"Yes ('00')"| node4["Set business status to Success"]
    click node4 openCode "app/cbl/CBACT01C.cbl:356:357"
    node3 -->|"No"| node5["Set business status to File Open Failed"]
    click node5 openCode "app/cbl/CBACT01C.cbl:358:359"
    node4 --> node6{"Did business operation succeed?"}
    node5 --> node6
    click node6 openCode "app/cbl/CBACT01C.cbl:360:367"
    node6 -->|"Yes (APPL-AOK)"| node7["Continue normal business flow"]
    click node7 openCode "app/cbl/CBACT01C.cbl:361:361"
    node6 -->|"No"| node8["Display error and handle failure"]
    click node8 openCode "app/cbl/CBACT01C.cbl:363:366"

classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;

%% Swimm:
%% %%{init: {"flowchart": {"defaultRenderer": "elk"}} }%%
%% flowchart TD
%%     node1["Set initial business status"]
%%     click node1 openCode "<SwmPath>[app/cbl/CBACT01C.cbl](app/cbl/CBACT01C.cbl)</SwmPath>:353:353"
%%     node1 --> node2["Attempt to open array file for output"]
%%     click node2 openCode "<SwmPath>[app/cbl/CBACT01C.cbl](app/cbl/CBACT01C.cbl)</SwmPath>:354:354"
%%     node2 --> node3{"Was file opened successfully?"}
%%     click node3 openCode "<SwmPath>[app/cbl/CBACT01C.cbl](app/cbl/CBACT01C.cbl)</SwmPath>:355:359"
%%     node3 -->|"Yes ('00')"| node4["Set business status to Success"]
%%     click node4 openCode "<SwmPath>[app/cbl/CBACT01C.cbl](app/cbl/CBACT01C.cbl)</SwmPath>:356:357"
%%     node3 -->|"No"| node5["Set business status to File Open Failed"]
%%     click node5 openCode "<SwmPath>[app/cbl/CBACT01C.cbl](app/cbl/CBACT01C.cbl)</SwmPath>:358:359"
%%     node4 --> node6{"Did business operation succeed?"}
%%     node5 --> node6
%%     click node6 openCode "<SwmPath>[app/cbl/CBACT01C.cbl](app/cbl/CBACT01C.cbl)</SwmPath>:360:367"
%%     node6 -->|"Yes (<SwmToken path="app/cbl/CBACT01C.cbl" pos="186:3:5" line-data="           IF  APPL-AOK">`APPL-AOK`</SwmToken>)"| node7["Continue normal business flow"]
%%     click node7 openCode "<SwmPath>[app/cbl/CBACT01C.cbl](app/cbl/CBACT01C.cbl)</SwmPath>:361:361"
%%     node6 -->|"No"| node8["Display error and handle failure"]
%%     click node8 openCode "<SwmPath>[app/cbl/CBACT01C.cbl](app/cbl/CBACT01C.cbl)</SwmPath>:363:366"
%% 
%% classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
```

This section governs how the application attempts to open the array output file and determines the business flow based on the success or failure of this operation. It ensures that file access issues are detected early and handled in a way that supports diagnostics and system reliability.

| Category       | Rule Name                | Description                                                                                       |
| -------------- | ------------------------ | ------------------------------------------------------------------------------------------------- |
| Business logic | File Open Success Status | If the array output file is opened successfully, the business status must be set to 'Success'.    |
| Business logic | File Open Failure Status | If the array output file fails to open, the business status must be set to 'File Open Failed'.    |
| Business logic | Continue on Success      | If the business status is 'Success', the application must continue with the normal business flow. |

<SwmSnippet path="/app/cbl/CBACT01C.cbl" line="352">

---

In <SwmToken path="app/cbl/CBACT01C.cbl" pos="352:1:5" line-data="       3000-ARRFILE-OPEN.">`3000-ARRFILE-OPEN`</SwmToken> we set <SwmToken path="app/cbl/CBACT01C.cbl" pos="353:7:9" line-data="           MOVE 8 TO APPL-RESULT.">`APPL-RESULT`</SwmToken>, try to open the array output file, and update the status depending on success or failure. This sets up error handling for array file access.

```cobol
       3000-ARRFILE-OPEN.
           MOVE 8 TO APPL-RESULT.
           OPEN OUTPUT ARRY-FILE
           IF   ARRYFILE-STATUS = '00'
               MOVE 0 TO APPL-RESULT
           ELSE
               MOVE 12 TO APPL-RESULT
           END-IF
```

---

</SwmSnippet>

<SwmSnippet path="/app/cbl/CBACT01C.cbl" line="360">

---

After trying to open the array file, if it fails, we display the error status, copy <SwmToken path="app/cbl/CBACT01C.cbl" pos="363:11:13" line-data="               DISPLAY &#39;ERROR OPENING ARRAYFILE&#39;  ARRYFILE-STATUS">`ARRYFILE-STATUS`</SwmToken> to <SwmToken path="app/cbl/CBACT01C.cbl" pos="364:9:11" line-data="               MOVE  ARRYFILE-STATUS TO IO-STATUS">`IO-STATUS`</SwmToken>, and call <SwmToken path="app/cbl/CBACT01C.cbl" pos="365:3:9" line-data="               PERFORM 9910-DISPLAY-IO-STATUS">`9910-DISPLAY-IO-STATUS`</SwmToken> for diagnostics before terminating. This helps pinpoint file issues.

```cobol
           IF  APPL-AOK
               CONTINUE
           ELSE
               DISPLAY 'ERROR OPENING ARRAYFILE'  ARRYFILE-STATUS
               MOVE  ARRYFILE-STATUS TO IO-STATUS
               PERFORM 9910-DISPLAY-IO-STATUS
               PERFORM 9999-ABEND-PROGRAM
           END-IF
           EXIT.
```

---

</SwmSnippet>

## Preparing for Variable Block Output

```mermaid
%%{init: {"flowchart": {"defaultRenderer": "elk"}} }%%
flowchart TD
    node1["Start execution of credit card management program"] --> node2["Open account file for business data"]
    click node1 openCode "app/cbl/CBACT01C.cbl:141:141"
    click node2 openCode "app/cbl/CBACT01C.cbl:142:142"
    node2 --> node3["Open output file for results"]
    click node3 openCode "app/cbl/CBACT01C.cbl:143:143"
    node3 --> node4["Open arrangement file for arrangements"]
    click node4 openCode "app/cbl/CBACT01C.cbl:144:144"
    node4 --> node5["Open variable record file for business records"]
    click node5 openCode "app/cbl/CBACT01C.cbl:145:145"

classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;

%% Swimm:
%% %%{init: {"flowchart": {"defaultRenderer": "elk"}} }%%
%% flowchart TD
%%     node1["Start execution of credit card management program"] --> node2["Open account file for business data"]
%%     click node1 openCode "<SwmPath>[app/cbl/CBACT01C.cbl](app/cbl/CBACT01C.cbl)</SwmPath>:141:141"
%%     click node2 openCode "<SwmPath>[app/cbl/CBACT01C.cbl](app/cbl/CBACT01C.cbl)</SwmPath>:142:142"
%%     node2 --> node3["Open output file for results"]
%%     click node3 openCode "<SwmPath>[app/cbl/CBACT01C.cbl](app/cbl/CBACT01C.cbl)</SwmPath>:143:143"
%%     node3 --> node4["Open arrangement file for arrangements"]
%%     click node4 openCode "<SwmPath>[app/cbl/CBACT01C.cbl](app/cbl/CBACT01C.cbl)</SwmPath>:144:144"
%%     node4 --> node5["Open variable record file for business records"]
%%     click node5 openCode "<SwmPath>[app/cbl/CBACT01C.cbl](app/cbl/CBACT01C.cbl)</SwmPath>:145:145"
%% 
%% classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
```

<SwmSnippet path="/app/cbl/CBACT01C.cbl" line="140">

---

Back in <SwmToken path="app/cbl/CBACT01C.cbl" pos="140:1:3" line-data="       PROCEDURE DIVISION.">`PROCEDURE DIVISION`</SwmToken>, after opening the array file, we move on to <SwmToken path="app/cbl/CBACT01C.cbl" pos="145:3:7" line-data="           PERFORM 4000-VBRFILE-OPEN.">`4000-VBRFILE-OPEN`</SwmToken> to get the variable block output file ready. This file is needed for writing account data in a flexible format.

```cobol
       PROCEDURE DIVISION.
           DISPLAY 'START OF EXECUTION OF PROGRAM CBACT01C'.
           PERFORM 0000-ACCTFILE-OPEN.
           PERFORM 2000-OUTFILE-OPEN.
           PERFORM 3000-ARRFILE-OPEN.
           PERFORM 4000-VBRFILE-OPEN.
```

---

</SwmSnippet>

## Opening the Variable Block Output File

```mermaid
%%{init: {"flowchart": {"defaultRenderer": "elk"}} }%%
flowchart TD
  node1["Start file open operation"]
  click node1 openCode "app/cbl/CBACT01C.cbl:370:371"
  node1 --> node2["Set status: Attempting to open file"]
  click node2 openCode "app/cbl/CBACT01C.cbl:371:372"
  node2 --> node3{"Was file opened successfully?"}
  click node3 openCode "app/cbl/CBACT01C.cbl:372:373"
  node3 -->|"Yes ('00')"| node4["Mark operation as successful"]
  click node4 openCode "app/cbl/CBACT01C.cbl:374:375"
  node3 -->|"No"| node5["Mark operation as failed"]
  click node5 openCode "app/cbl/CBACT01C.cbl:376:377"
  node4 --> node6{"Is application status OK (APPL-RESULT = 0)?"}
  click node6 openCode "app/cbl/CBACT01C.cbl:378:379"
  node5 --> node6
  node6 -->|"Yes"| node7["Continue with process"]
  click node7 openCode "app/cbl/CBACT01C.cbl:379:380"
  node6 -->|"No"| node8["Show error, display file status, abort operation"]
  click node8 openCode "app/cbl/CBACT01C.cbl:381:385"

classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;

%% Swimm:
%% %%{init: {"flowchart": {"defaultRenderer": "elk"}} }%%
%% flowchart TD
%%   node1["Start file open operation"]
%%   click node1 openCode "<SwmPath>[app/cbl/CBACT01C.cbl](app/cbl/CBACT01C.cbl)</SwmPath>:370:371"
%%   node1 --> node2["Set status: Attempting to open file"]
%%   click node2 openCode "<SwmPath>[app/cbl/CBACT01C.cbl](app/cbl/CBACT01C.cbl)</SwmPath>:371:372"
%%   node2 --> node3{"Was file opened successfully?"}
%%   click node3 openCode "<SwmPath>[app/cbl/CBACT01C.cbl](app/cbl/CBACT01C.cbl)</SwmPath>:372:373"
%%   node3 -->|"Yes ('00')"| node4["Mark operation as successful"]
%%   click node4 openCode "<SwmPath>[app/cbl/CBACT01C.cbl](app/cbl/CBACT01C.cbl)</SwmPath>:374:375"
%%   node3 -->|"No"| node5["Mark operation as failed"]
%%   click node5 openCode "<SwmPath>[app/cbl/CBACT01C.cbl](app/cbl/CBACT01C.cbl)</SwmPath>:376:377"
%%   node4 --> node6{"Is application status OK (<SwmToken path="app/cbl/CBACT01C.cbl" pos="168:7:9" line-data="               MOVE 0 TO APPL-RESULT">`APPL-RESULT`</SwmToken> = 0)?"}
%%   click node6 openCode "<SwmPath>[app/cbl/CBACT01C.cbl](app/cbl/CBACT01C.cbl)</SwmPath>:378:379"
%%   node5 --> node6
%%   node6 -->|"Yes"| node7["Continue with process"]
%%   click node7 openCode "<SwmPath>[app/cbl/CBACT01C.cbl](app/cbl/CBACT01C.cbl)</SwmPath>:379:380"
%%   node6 -->|"No"| node8["Show error, display file status, abort operation"]
%%   click node8 openCode "<SwmPath>[app/cbl/CBACT01C.cbl](app/cbl/CBACT01C.cbl)</SwmPath>:381:385"
%% 
%% classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
```

This section governs the business rules for opening the variable block output file, ensuring that the application only proceeds if the file is available and accessible. It also ensures that any issues encountered during the file open operation are clearly communicated and handled according to business requirements.

| Category        | Rule Name                   | Description                                                                                                                                                                                                                                        |
| --------------- | --------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Data validation | File Open Required          | The application must attempt to open the variable block output file before proceeding with any operations that require file output.                                                                                                                |
| Business logic  | Successful File Open Status | If the file is opened successfully (file status '00'), the application status must be set to indicate success (<SwmToken path="app/cbl/CBACT01C.cbl" pos="168:7:9" line-data="               MOVE 0 TO APPL-RESULT">`APPL-RESULT`</SwmToken> = 0). |
| Business logic  | Failed File Open Status     | If the file open fails (file status not '00'), the application status must be set to indicate failure (<SwmToken path="app/cbl/CBACT01C.cbl" pos="168:7:9" line-data="               MOVE 0 TO APPL-RESULT">`APPL-RESULT`</SwmToken> = 12).        |
| Business logic  | Continue on Success         | If the application status is OK (<SwmToken path="app/cbl/CBACT01C.cbl" pos="168:7:9" line-data="               MOVE 0 TO APPL-RESULT">`APPL-RESULT`</SwmToken> = 0), the process continues without interruption.                                   |

<SwmSnippet path="/app/cbl/CBACT01C.cbl" line="370">

---

In <SwmToken path="app/cbl/CBACT01C.cbl" pos="370:1:5" line-data="       4000-VBRFILE-OPEN.">`4000-VBRFILE-OPEN`</SwmToken> we set <SwmToken path="app/cbl/CBACT01C.cbl" pos="371:7:9" line-data="           MOVE 8 TO APPL-RESULT.">`APPL-RESULT`</SwmToken> to 8, try to open the variable block output file, and update the status to 0 or 12 depending on the result. This sets up error handling if the file can't be opened.

```cobol
       4000-VBRFILE-OPEN.
           MOVE 8 TO APPL-RESULT.
           OPEN OUTPUT VBRC-FILE
           IF   VBRCFILE-STATUS = '00'
               MOVE 0 TO APPL-RESULT
           ELSE
               MOVE 12 TO APPL-RESULT
           END-IF
```

---

</SwmSnippet>

<SwmSnippet path="/app/cbl/CBACT01C.cbl" line="378">

---

After trying to open the variable block file, if it fails, we display the error status, copy <SwmToken path="app/cbl/CBACT01C.cbl" pos="381:13:15" line-data="               DISPLAY &#39;ERROR OPENING VBRC FILE&#39;  VBRCFILE-STATUS">`VBRCFILE-STATUS`</SwmToken> to <SwmToken path="app/cbl/CBACT01C.cbl" pos="382:9:11" line-data="               MOVE  VBRCFILE-STATUS TO IO-STATUS">`IO-STATUS`</SwmToken>, and call <SwmToken path="app/cbl/CBACT01C.cbl" pos="383:3:9" line-data="               PERFORM 9910-DISPLAY-IO-STATUS">`9910-DISPLAY-IO-STATUS`</SwmToken> for diagnostics before terminating. This helps pinpoint file issues.

```cobol
           IF  APPL-AOK
               CONTINUE
           ELSE
               DISPLAY 'ERROR OPENING VBRC FILE'  VBRCFILE-STATUS
               MOVE  VBRCFILE-STATUS TO IO-STATUS
               PERFORM 9910-DISPLAY-IO-STATUS
               PERFORM 9999-ABEND-PROGRAM
           END-IF
           EXIT.
```

---

</SwmSnippet>

## Processing Account Records

```mermaid
%%{init: {"flowchart": {"defaultRenderer": "elk"}} }%%
flowchart TD
    subgraph loop1["Repeat until END-OF-FILE = 'Y'"]
        node2{"Is END-OF-FILE = 'N'?"}
        click node2 openCode "app/cbl/CBACT01C.cbl:147:148"
        node2 -->|"Yes"| node3["Get next account record"]
        click node3 openCode "app/cbl/CBACT01C.cbl:149:149"
        node3 --> node4{"Is END-OF-FILE still 'N'?"}
        click node4 openCode "app/cbl/CBACT01C.cbl:150:150"
        node4 -->|"Yes"| node5["Display account record"]
        click node5 openCode "app/cbl/CBACT01C.cbl:151:151"
        node5 --> node2
        node4 -->|"No"| node2
        node2 -->|"No"| node6["Finish processing"]
        click node6 openCode "app/cbl/CBACT01C.cbl:154:154"
    end
classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;

%% Swimm:
%% %%{init: {"flowchart": {"defaultRenderer": "elk"}} }%%
%% flowchart TD
%%     subgraph loop1["Repeat until <SwmToken path="app/cbl/CBACT01C.cbl" pos="147:5:9" line-data="           PERFORM UNTIL END-OF-FILE = &#39;Y&#39;">`END-OF-FILE`</SwmToken> = 'Y'"]
%%         node2{"Is <SwmToken path="app/cbl/CBACT01C.cbl" pos="147:5:9" line-data="           PERFORM UNTIL END-OF-FILE = &#39;Y&#39;">`END-OF-FILE`</SwmToken> = 'N'?"}
%%         click node2 openCode "<SwmPath>[app/cbl/CBACT01C.cbl](app/cbl/CBACT01C.cbl)</SwmPath>:147:148"
%%         node2 -->|"Yes"| node3["Get next account record"]
%%         click node3 openCode "<SwmPath>[app/cbl/CBACT01C.cbl](app/cbl/CBACT01C.cbl)</SwmPath>:149:149"
%%         node3 --> node4{"Is <SwmToken path="app/cbl/CBACT01C.cbl" pos="147:5:9" line-data="           PERFORM UNTIL END-OF-FILE = &#39;Y&#39;">`END-OF-FILE`</SwmToken> still 'N'?"}
%%         click node4 openCode "<SwmPath>[app/cbl/CBACT01C.cbl](app/cbl/CBACT01C.cbl)</SwmPath>:150:150"
%%         node4 -->|"Yes"| node5["Display account record"]
%%         click node5 openCode "<SwmPath>[app/cbl/CBACT01C.cbl](app/cbl/CBACT01C.cbl)</SwmPath>:151:151"
%%         node5 --> node2
%%         node4 -->|"No"| node2
%%         node2 -->|"No"| node6["Finish processing"]
%%         click node6 openCode "<SwmPath>[app/cbl/CBACT01C.cbl](app/cbl/CBACT01C.cbl)</SwmPath>:154:154"
%%     end
%% classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
```

<SwmSnippet path="/app/cbl/CBACT01C.cbl" line="147">

---

Back in <SwmToken path="app/cbl/CBACT01C.cbl" pos="140:1:3" line-data="       PROCEDURE DIVISION.">`PROCEDURE DIVISION`</SwmToken>, after opening all files, we enter a loop to process each account record. We call <SwmToken path="app/cbl/CBACT01C.cbl" pos="149:3:9" line-data="                   PERFORM 1000-ACCTFILE-GET-NEXT">`1000-ACCTFILE-GET-NEXT`</SwmToken> to fetch and handle the next record, repeating until <SwmToken path="app/cbl/CBACT01C.cbl" pos="147:5:9" line-data="           PERFORM UNTIL END-OF-FILE = &#39;Y&#39;">`END-OF-FILE`</SwmToken> is set.

```cobol
           PERFORM UNTIL END-OF-FILE = 'Y'
               IF  END-OF-FILE = 'N'
                   PERFORM 1000-ACCTFILE-GET-NEXT
                   IF  END-OF-FILE = 'N'
                       DISPLAY ACCOUNT-RECORD
                   END-IF
               END-IF
           END-PERFORM.
```

---

</SwmSnippet>

## Reading and Processing the Next Account Record

This section governs the retrieval and preparation of the next account record for downstream processing, ensuring that only valid records are processed and that appropriate status codes are set for end-of-file or error conditions.

| Category        | Rule Name               | Description                                                                                                                                                                      |
| --------------- | ----------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Data validation | Successful Account Read | If the account file status is '00', the account record is considered successfully read and is eligible for further processing and output.                                        |
| Business logic  | Account Data Formatting | When a record is successfully read, the account data must be formatted and prepared for output, including reformatting the reissue date to meet business reporting requirements. |

<SwmSnippet path="/app/cbl/CBACT01C.cbl" line="165">

---

In <SwmToken path="app/cbl/CBACT01C.cbl" pos="165:1:7" line-data="       1000-ACCTFILE-GET-NEXT.">`1000-ACCTFILE-GET-NEXT`</SwmToken> we read the next account record, display it, and then call <SwmToken path="app/cbl/CBACT01C.cbl" pos="171:3:9" line-data="               PERFORM 1300-POPUL-ACCT-RECORD">`1300-POPUL-ACCT-RECORD`</SwmToken> to format and prep the data for output. This sets up the rest of the output steps.

```cobol
       1000-ACCTFILE-GET-NEXT.
           READ ACCTFILE-FILE INTO ACCOUNT-RECORD.
           IF  ACCTFILE-STATUS = '00'
               MOVE 0 TO APPL-RESULT
               INITIALIZE ARR-ARRAY-REC
               PERFORM 1100-DISPLAY-ACCT-RECORD
               PERFORM 1300-POPUL-ACCT-RECORD
               PERFORM 1350-WRITE-ACCT-RECORD
               PERFORM 1400-POPUL-ARRAY-RECORD
               PERFORM 1450-WRITE-ARRY-RECORD
               INITIALIZE VBRC-REC1
               PERFORM 1500-POPUL-VBRC-RECORD
               PERFORM 1550-WRITE-VB1-RECORD
               PERFORM 1575-WRITE-VB2-RECORD
           ELSE
               IF  ACCTFILE-STATUS = '10'
                   MOVE 16 TO APPL-RESULT
               ELSE
                   MOVE 12 TO APPL-RESULT
               END-IF
```

---

</SwmSnippet>

### Populating the Output Account Record

```mermaid
%%{init: {"flowchart": {"defaultRenderer": "elk"}} }%%
flowchart TD
    node1["Copy all account details to output record except reissue date and current cycle debit"]
    click node1 openCode "app/cbl/CBACT01C.cbl:216:222"
    node1 --> node2["Format reissue date for output"]
    click node2 openCode "app/cbl/CBACT01C.cbl:223:231"
    node2 --> node3["Copy formatted reissue date to output"]
    click node3 openCode "app/cbl/CBACT01C.cbl:233:233"
    node3 --> node4["Copy current cycle credit to output"]
    click node4 openCode "app/cbl/CBACT01C.cbl:235:235"
    node4 --> node5{"Is current cycle debit zero?"}
    click node5 openCode "app/cbl/CBACT01C.cbl:236:236"
    node5 -->|"Yes"| node6["Set current cycle debit to 2525.00 in output"]
    click node6 openCode "app/cbl/CBACT01C.cbl:237:237"
    node5 -->|"No"| node7["Copy current cycle debit to output"]
    click node7 openCode "app/cbl/CBACT01C.cbl:238:238"
    node6 --> node8["Copy group ID to output"]
    node7 --> node8
    click node8 openCode "app/cbl/CBACT01C.cbl:239:239"
classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;

%% Swimm:
%% %%{init: {"flowchart": {"defaultRenderer": "elk"}} }%%
%% flowchart TD
%%     node1["Copy all account details to output record except reissue date and current cycle debit"]
%%     click node1 openCode "<SwmPath>[app/cbl/CBACT01C.cbl](app/cbl/CBACT01C.cbl)</SwmPath>:216:222"
%%     node1 --> node2["Format reissue date for output"]
%%     click node2 openCode "<SwmPath>[app/cbl/CBACT01C.cbl](app/cbl/CBACT01C.cbl)</SwmPath>:223:231"
%%     node2 --> node3["Copy formatted reissue date to output"]
%%     click node3 openCode "<SwmPath>[app/cbl/CBACT01C.cbl](app/cbl/CBACT01C.cbl)</SwmPath>:233:233"
%%     node3 --> node4["Copy current cycle credit to output"]
%%     click node4 openCode "<SwmPath>[app/cbl/CBACT01C.cbl](app/cbl/CBACT01C.cbl)</SwmPath>:235:235"
%%     node4 --> node5{"Is current cycle debit zero?"}
%%     click node5 openCode "<SwmPath>[app/cbl/CBACT01C.cbl](app/cbl/CBACT01C.cbl)</SwmPath>:236:236"
%%     node5 -->|"Yes"| node6["Set current cycle debit to <SwmToken path="app/cbl/CBACT01C.cbl" pos="237:3:5" line-data="               MOVE 2525.00         TO   OUT-ACCT-CURR-CYC-DEBIT">`2525.00`</SwmToken> in output"]
%%     click node6 openCode "<SwmPath>[app/cbl/CBACT01C.cbl](app/cbl/CBACT01C.cbl)</SwmPath>:237:237"
%%     node5 -->|"No"| node7["Copy current cycle debit to output"]
%%     click node7 openCode "<SwmPath>[app/cbl/CBACT01C.cbl](app/cbl/CBACT01C.cbl)</SwmPath>:238:238"
%%     node6 --> node8["Copy group ID to output"]
%%     node7 --> node8
%%     click node8 openCode "<SwmPath>[app/cbl/CBACT01C.cbl](app/cbl/CBACT01C.cbl)</SwmPath>:239:239"
%% classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
```

This section ensures that all necessary account information is accurately transferred to the output record, applying business rules for date formatting and default values to maintain data consistency and meet downstream processing requirements.

| Category       | Rule Name                | Description                                                                                                                                                                                                                                                                             |
| -------------- | ------------------------ | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Business logic | Field Copy Consistency   | All account fields are copied from the input record to the output record, except for the reissue date and current cycle debit, which are handled by specific rules.                                                                                                                     |
| Business logic | Reissue Date Formatting  | The reissue date in the output record must be formatted according to the required output format, regardless of the input format.                                                                                                                                                        |
| Business logic | Default Debit Value      | If the current cycle debit in the input record is zero, the output record must set the current cycle debit to <SwmToken path="app/cbl/CBACT01C.cbl" pos="237:3:5" line-data="               MOVE 2525.00         TO   OUT-ACCT-CURR-CYC-DEBIT">`2525.00`</SwmToken> as a default value. |
| Business logic | Debit Value Preservation | If the current cycle debit in the input record is not zero, the value is copied as-is to the output record.                                                                                                                                                                             |

<SwmSnippet path="/app/cbl/CBACT01C.cbl" line="215">

---

We copy fields and call 'COBDATFT' to format the reissue date for the output record.

```cobol
       1300-POPUL-ACCT-RECORD.
           MOVE   ACCT-ID                 TO   OUT-ACCT-ID.
           MOVE   ACCT-ACTIVE-STATUS      TO   OUT-ACCT-ACTIVE-STATUS.
           MOVE   ACCT-CURR-BAL           TO   OUT-ACCT-CURR-BAL.
           MOVE   ACCT-CREDIT-LIMIT       TO   OUT-ACCT-CREDIT-LIMIT.
           MOVE   ACCT-CASH-CREDIT-LIMIT  TO OUT-ACCT-CASH-CREDIT-LIMIT.
           MOVE   ACCT-OPEN-DATE          TO   OUT-ACCT-OPEN-DATE.
           MOVE   ACCT-EXPIRAION-DATE     TO   OUT-ACCT-EXPIRAION-DATE.
           MOVE   ACCT-REISSUE-DATE       TO   CODATECN-INP-DATE
                                               WS-REISSUE-DATE.
           MOVE   '2'                     TO   CODATECN-TYPE.
           MOVE   '2'                     TO   CODATECN-OUTTYPE.

      *---------------------------------------------------------------*
      *CALL ASSEMBLER PROGRAM FOR DATE FORMATTING                     *
      *---------------------------------------------------------------*
           CALL 'COBDATFT'       USING CODATECN-REC.

           MOVE   CODATECN-0UT-DATE       TO   OUT-ACCT-REISSUE-DATE.

           MOVE   ACCT-CURR-CYC-CREDIT    TO   OUT-ACCT-CURR-CYC-CREDIT.
```

---

</SwmSnippet>

<SwmSnippet path="/app/cbl/CBACT01C.cbl" line="236">

---

After formatting and copying fields, if the input debit is zero, we set <SwmToken path="app/cbl/CBACT01C.cbl" pos="237:9:17" line-data="               MOVE 2525.00         TO   OUT-ACCT-CURR-CYC-DEBIT">`OUT-ACCT-CURR-CYC-DEBIT`</SwmToken> to <SwmToken path="app/cbl/CBACT01C.cbl" pos="237:3:5" line-data="               MOVE 2525.00         TO   OUT-ACCT-CURR-CYC-DEBIT">`2525.00`</SwmToken> as a default. The rest of the fields are copied straight over.

```cobol
           IF  ACCT-CURR-CYC-DEBIT EQUAL TO ZERO
               MOVE 2525.00         TO   OUT-ACCT-CURR-CYC-DEBIT
           END-IF.
           MOVE   ACCT-GROUP-ID           TO   OUT-ACCT-GROUP-ID.
           EXIT.
```

---

</SwmSnippet>

### Writing and Populating Output Records

```mermaid
%%{init: {"flowchart": {"defaultRenderer": "elk"}} }%%
flowchart TD
    node1["Read next account record"] --> node2{"Was record read successfully?"}
    click node1 openCode "app/cbl/CBACT01C.cbl:185:198"
    node2 -->|"Yes (APPL-AOK)"| node3["Write account record to output file"]
    click node2 openCode "app/cbl/CBACT01C.cbl:185:198"
    node2 -->|"No"| node4{"Is end of file reached?"}
    click node4 openCode "app/cbl/CBACT01C.cbl:189:191"
    node4 -->|"Yes (APPL-EOF)"| node5["Set END-OF-FILE to 'Y'"]
    click node5 openCode "app/cbl/CBACT01C.cbl:190:191"
    node4 -->|"No"| node6["Display error and abort"]
    click node6 openCode "app/cbl/CBACT01C.cbl:192:196"
    node3 --> node7{"Did account file write fail?"}
    click node3 openCode "app/cbl/CBACT01C.cbl:242:251"
    node7 -->|"Yes"| node6
    node7 -->|"No"| node8["Update account data arrays"]
    click node8 openCode "app/cbl/CBACT01C.cbl:253:261"
    node8 --> node9["Write array record to output file"]
    click node9 openCode "app/cbl/CBACT01C.cbl:263:274"
    node9 --> node10{"Did array file write fail?"}
    click node10 openCode "app/cbl/CBACT01C.cbl:263:274"
    node10 -->|"Yes"| node6
    node10 -->|"No"| node11["Write VB1 record to output file"]
    click node11 openCode "app/cbl/CBACT01C.cbl:287:300"
    node11 --> node12{"Did VB1 file write fail?"}
    click node12 openCode "app/cbl/CBACT01C.cbl:287:300"
    node12 -->|"Yes"| node6
    node12 -->|"No"| node13["Write VB2 record to output file"]
    click node13 openCode "app/cbl/CBACT01C.cbl:302:315"
    node13 --> node14{"Did VB2 file write fail?"}
    click node14 openCode "app/cbl/CBACT01C.cbl:302:315"
    node14 -->|"Yes"| node6
    node14 -->|"No"| node15["Continue processing"]
classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;

%% Swimm:
%% %%{init: {"flowchart": {"defaultRenderer": "elk"}} }%%
%% flowchart TD
%%     node1["Read next account record"] --> node2{"Was record read successfully?"}
%%     click node1 openCode "<SwmPath>[app/cbl/CBACT01C.cbl](app/cbl/CBACT01C.cbl)</SwmPath>:185:198"
%%     node2 -->|"Yes (<SwmToken path="app/cbl/CBACT01C.cbl" pos="186:3:5" line-data="           IF  APPL-AOK">`APPL-AOK`</SwmToken>)"| node3["Write account record to output file"]
%%     click node2 openCode "<SwmPath>[app/cbl/CBACT01C.cbl](app/cbl/CBACT01C.cbl)</SwmPath>:185:198"
%%     node2 -->|"No"| node4{"Is end of file reached?"}
%%     click node4 openCode "<SwmPath>[app/cbl/CBACT01C.cbl](app/cbl/CBACT01C.cbl)</SwmPath>:189:191"
%%     node4 -->|"Yes (<SwmToken path="app/cbl/CBACT01C.cbl" pos="189:3:5" line-data="               IF  APPL-EOF">`APPL-EOF`</SwmToken>)"| node5["Set <SwmToken path="app/cbl/CBACT01C.cbl" pos="147:5:9" line-data="           PERFORM UNTIL END-OF-FILE = &#39;Y&#39;">`END-OF-FILE`</SwmToken> to 'Y'"]
%%     click node5 openCode "<SwmPath>[app/cbl/CBACT01C.cbl](app/cbl/CBACT01C.cbl)</SwmPath>:190:191"
%%     node4 -->|"No"| node6["Display error and abort"]
%%     click node6 openCode "<SwmPath>[app/cbl/CBACT01C.cbl](app/cbl/CBACT01C.cbl)</SwmPath>:192:196"
%%     node3 --> node7{"Did account file write fail?"}
%%     click node3 openCode "<SwmPath>[app/cbl/CBACT01C.cbl](app/cbl/CBACT01C.cbl)</SwmPath>:242:251"
%%     node7 -->|"Yes"| node6
%%     node7 -->|"No"| node8["Update account data arrays"]
%%     click node8 openCode "<SwmPath>[app/cbl/CBACT01C.cbl](app/cbl/CBACT01C.cbl)</SwmPath>:253:261"
%%     node8 --> node9["Write array record to output file"]
%%     click node9 openCode "<SwmPath>[app/cbl/CBACT01C.cbl](app/cbl/CBACT01C.cbl)</SwmPath>:263:274"
%%     node9 --> node10{"Did array file write fail?"}
%%     click node10 openCode "<SwmPath>[app/cbl/CBACT01C.cbl](app/cbl/CBACT01C.cbl)</SwmPath>:263:274"
%%     node10 -->|"Yes"| node6
%%     node10 -->|"No"| node11["Write <SwmToken path="app/cbl/CBACT01C.cbl" pos="177:7:7" line-data="               PERFORM 1550-WRITE-VB1-RECORD">`VB1`</SwmToken> record to output file"]
%%     click node11 openCode "<SwmPath>[app/cbl/CBACT01C.cbl](app/cbl/CBACT01C.cbl)</SwmPath>:287:300"
%%     node11 --> node12{"Did <SwmToken path="app/cbl/CBACT01C.cbl" pos="177:7:7" line-data="               PERFORM 1550-WRITE-VB1-RECORD">`VB1`</SwmToken> file write fail?"}
%%     click node12 openCode "<SwmPath>[app/cbl/CBACT01C.cbl](app/cbl/CBACT01C.cbl)</SwmPath>:287:300"
%%     node12 -->|"Yes"| node6
%%     node12 -->|"No"| node13["Write <SwmToken path="app/cbl/CBACT01C.cbl" pos="178:7:7" line-data="               PERFORM 1575-WRITE-VB2-RECORD">`VB2`</SwmToken> record to output file"]
%%     click node13 openCode "<SwmPath>[app/cbl/CBACT01C.cbl](app/cbl/CBACT01C.cbl)</SwmPath>:302:315"
%%     node13 --> node14{"Did <SwmToken path="app/cbl/CBACT01C.cbl" pos="178:7:7" line-data="               PERFORM 1575-WRITE-VB2-RECORD">`VB2`</SwmToken> file write fail?"}
%%     click node14 openCode "<SwmPath>[app/cbl/CBACT01C.cbl](app/cbl/CBACT01C.cbl)</SwmPath>:302:315"
%%     node14 -->|"Yes"| node6
%%     node14 -->|"No"| node15["Continue processing"]
%% classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
```

<SwmSnippet path="/app/cbl/CBACT01C.cbl" line="242">

---

<SwmToken path="app/cbl/CBACT01C.cbl" pos="242:1:7" line-data="       1350-WRITE-ACCT-RECORD.">`1350-WRITE-ACCT-RECORD`</SwmToken> writes the output account record and checks the file status. If it's not '00' or '10', we display the error, show IO status, and terminate. This catches write errors right away.

```cobol
       1350-WRITE-ACCT-RECORD.
           WRITE OUT-ACCT-REC.

           IF OUTFILE-STATUS NOT = '00' AND OUTFILE-STATUS NOT = '10'
              DISPLAY 'ACCOUNT FILE WRITE STATUS IS:'  OUTFILE-STATUS
              MOVE OUTFILE-STATUS  TO IO-STATUS
              PERFORM 9910-DISPLAY-IO-STATUS
              PERFORM 9999-ABEND-PROGRAM
           END-IF.
           EXIT.
```

---

</SwmSnippet>

<SwmSnippet path="/app/cbl/CBACT01C.cbl" line="253">

---

<SwmToken path="app/cbl/CBACT01C.cbl" pos="253:1:7" line-data="       1400-POPUL-ARRAY-RECORD.">`1400-POPUL-ARRAY-RECORD`</SwmToken> fills the array record with the account ID, current balance, and a mix of hardcoded and variable debit values. These constants are baked in and affect the output arrays.

```cobol
       1400-POPUL-ARRAY-RECORD.
           MOVE   ACCT-ID         TO   ARR-ACCT-ID.
           MOVE   ACCT-CURR-BAL   TO   ARR-ACCT-CURR-BAL(1).
           MOVE   1005.00         TO   ARR-ACCT-CURR-CYC-DEBIT(1).
           MOVE   ACCT-CURR-BAL   TO   ARR-ACCT-CURR-BAL(2).
           MOVE   1525.00         TO   ARR-ACCT-CURR-CYC-DEBIT(2).
           MOVE   -1025.00        TO   ARR-ACCT-CURR-BAL(3).
           MOVE   -2500.00        TO   ARR-ACCT-CURR-CYC-DEBIT(3).
           EXIT.
```

---

</SwmSnippet>

<SwmSnippet path="/app/cbl/CBACT01C.cbl" line="263">

---

<SwmToken path="app/cbl/CBACT01C.cbl" pos="263:1:7" line-data="       1450-WRITE-ARRY-RECORD.">`1450-WRITE-ARRY-RECORD`</SwmToken> writes the array record and checks the file status. If it's not '00' or '10', we display the error, show IO status, and terminate. This catches write errors right away.

```cobol
       1450-WRITE-ARRY-RECORD.
           WRITE ARR-ARRAY-REC.

           IF ARRYFILE-STATUS NOT = '00'
                        AND ARRYFILE-STATUS NOT = '10'
              DISPLAY 'ACCOUNT FILE WRITE STATUS IS:'
                                        ARRYFILE-STATUS
              MOVE ARRYFILE-STATUS TO IO-STATUS
              PERFORM 9910-DISPLAY-IO-STATUS
              PERFORM 9999-ABEND-PROGRAM
           END-IF.
           EXIT.
```

---

</SwmSnippet>

<SwmSnippet path="/app/cbl/CBACT01C.cbl" line="287">

---

<SwmToken path="app/cbl/CBACT01C.cbl" pos="287:1:7" line-data="       1550-WRITE-VB1-RECORD.">`1550-WRITE-VB1-RECORD`</SwmToken> sets the record length, moves the data, writes the record, and checks the file status. If it's not '00' or '10', we show the error and terminate. This enforces correct record sizing and error handling.

```cobol
       1550-WRITE-VB1-RECORD.
           MOVE 12 TO WS-RECD-LEN.
           MOVE VBRC-REC1 TO VBR-REC(1:WS-RECD-LEN).
           WRITE VBR-REC.

           IF VBRCFILE-STATUS NOT = '00'
                        AND VBRCFILE-STATUS NOT = '10'
              DISPLAY 'ACCOUNT FILE WRITE STATUS IS:'
                                        VBRCFILE-STATUS
              MOVE VBRCFILE-STATUS TO IO-STATUS
              PERFORM 9910-DISPLAY-IO-STATUS
              PERFORM 9999-ABEND-PROGRAM
           END-IF.
           EXIT.
```

---

</SwmSnippet>

<SwmSnippet path="/app/cbl/CBACT01C.cbl" line="302">

---

<SwmToken path="app/cbl/CBACT01C.cbl" pos="302:1:7" line-data="       1575-WRITE-VB2-RECORD.">`1575-WRITE-VB2-RECORD`</SwmToken> sets the record length to 39, moves the data, writes the record, and checks the file status. If it's not '00' or '10', we show the error and terminate. This enforces correct record sizing and error handling.

```cobol
       1575-WRITE-VB2-RECORD.
           MOVE 39 TO WS-RECD-LEN.
           MOVE VBRC-REC2 TO VBR-REC(1:WS-RECD-LEN).
           WRITE VBR-REC.

           IF VBRCFILE-STATUS NOT = '00'
                        AND VBRCFILE-STATUS NOT = '10'
              DISPLAY 'ACCOUNT FILE WRITE STATUS IS:'
                                        VBRCFILE-STATUS
              MOVE VBRCFILE-STATUS TO IO-STATUS
              PERFORM 9910-DISPLAY-IO-STATUS
              PERFORM 9999-ABEND-PROGRAM
           END-IF.
           EXIT.
```

---

</SwmSnippet>

<SwmSnippet path="/app/cbl/CBACT01C.cbl" line="185">

---

After processing, we check for errors and show IO status if needed before terminating.

```cobol
           END-IF
           IF  APPL-AOK
               CONTINUE
           ELSE
               IF  APPL-EOF
                   MOVE 'Y' TO END-OF-FILE
               ELSE
                   DISPLAY 'ERROR READING ACCOUNT FILE'
                   MOVE ACCTFILE-STATUS TO IO-STATUS
                   PERFORM 9910-DISPLAY-IO-STATUS
                   PERFORM 9999-ABEND-PROGRAM
               END-IF
           END-IF
           EXIT.
```

---

</SwmSnippet>

## Finalizing and Closing Processing

<SwmSnippet path="/app/cbl/CBACT01C.cbl" line="156">

---

After finishing, we close the account file to clean up before exiting.

```cobol
           PERFORM 9000-ACCTFILE-CLOSE.

           DISPLAY 'END OF EXECUTION OF PROGRAM CBACT01C'.

           GOBACK.
```

---

</SwmSnippet>

# Closing the Account File

```mermaid
%%{init: {"flowchart": {"defaultRenderer": "elk"}} }%%
flowchart TD
    node1["Attempt to close account file"] --> node2{"Was account file closed successfully?"}
    click node1 openCode "app/cbl/CBACT01C.cbl:389:390"
    node2 -->|"Success ('00')"| node3["Mark operation as successful (business status OK)"]
    click node2 openCode "app/cbl/CBACT01C.cbl:391:391"
    click node3 openCode "app/cbl/CBACT01C.cbl:392:392"
    node2 -->|"Failure (not '00')"| node4["Mark operation as failed (business status Error)"]
    click node4 openCode "app/cbl/CBACT01C.cbl:394:394"
    node3 --> node5{"Is business status OK?"}
    node4 --> node5
    click node5 openCode "app/cbl/CBACT01C.cbl:396:396"
    node5 -->|"OK (APPL-RESULT = 0)"| node6["Operation complete"]
    click node6 openCode "app/cbl/CBACT01C.cbl:404:404"
    node5 -->|"Error (APPL-RESULT ≠ 0)"| node7["Show error, display status, perform error routines"]
    click node7 openCode "app/cbl/CBACT01C.cbl:399:403"
classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;

%% Swimm:
%% %%{init: {"flowchart": {"defaultRenderer": "elk"}} }%%
%% flowchart TD
%%     node1["Attempt to close account file"] --> node2{"Was account file closed successfully?"}
%%     click node1 openCode "<SwmPath>[app/cbl/CBACT01C.cbl](app/cbl/CBACT01C.cbl)</SwmPath>:389:390"
%%     node2 -->|"Success ('00')"| node3["Mark operation as successful (business status OK)"]
%%     click node2 openCode "<SwmPath>[app/cbl/CBACT01C.cbl](app/cbl/CBACT01C.cbl)</SwmPath>:391:391"
%%     click node3 openCode "<SwmPath>[app/cbl/CBACT01C.cbl](app/cbl/CBACT01C.cbl)</SwmPath>:392:392"
%%     node2 -->|"Failure (not '00')"| node4["Mark operation as failed (business status Error)"]
%%     click node4 openCode "<SwmPath>[app/cbl/CBACT01C.cbl](app/cbl/CBACT01C.cbl)</SwmPath>:394:394"
%%     node3 --> node5{"Is business status OK?"}
%%     node4 --> node5
%%     click node5 openCode "<SwmPath>[app/cbl/CBACT01C.cbl](app/cbl/CBACT01C.cbl)</SwmPath>:396:396"
%%     node5 -->|"OK (<SwmToken path="app/cbl/CBACT01C.cbl" pos="168:7:9" line-data="               MOVE 0 TO APPL-RESULT">`APPL-RESULT`</SwmToken> = 0)"| node6["Operation complete"]
%%     click node6 openCode "<SwmPath>[app/cbl/CBACT01C.cbl](app/cbl/CBACT01C.cbl)</SwmPath>:404:404"
%%     node5 -->|"Error (<SwmToken path="app/cbl/CBACT01C.cbl" pos="168:7:9" line-data="               MOVE 0 TO APPL-RESULT">`APPL-RESULT`</SwmToken> ≠ 0)"| node7["Show error, display status, perform error routines"]
%%     click node7 openCode "<SwmPath>[app/cbl/CBACT01C.cbl](app/cbl/CBACT01C.cbl)</SwmPath>:399:403"
%% classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
```

This section ensures that the account file is closed properly at the end of processing, and that any issues encountered during the close operation are detected, reported, and handled according to business requirements.

| Category        | Rule Name                  | Description                                                                                                                       |
| --------------- | -------------------------- | --------------------------------------------------------------------------------------------------------------------------------- |
| Data validation | Business status validation | The business status must only be marked as OK if the internal result variable equals zero after the close operation.              |
| Business logic  | Successful file close      | If the account file is closed successfully (status code '00'), the business status must be marked as OK and no error is reported. |

<SwmSnippet path="/app/cbl/CBACT01C.cbl" line="388">

---

In <SwmToken path="app/cbl/CBACT01C.cbl" pos="388:1:5" line-data="       9000-ACCTFILE-CLOSE.">`9000-ACCTFILE-CLOSE`</SwmToken> we set <SwmToken path="app/cbl/CBACT01C.cbl" pos="389:11:13" line-data="           ADD 8 TO ZERO GIVING APPL-RESULT.">`APPL-RESULT`</SwmToken> to 8, close the account file, and update the status to 0 or 12 depending on the result. This sets up error handling for file close issues.

```cobol
       9000-ACCTFILE-CLOSE.
           ADD 8 TO ZERO GIVING APPL-RESULT.
           CLOSE ACCTFILE-FILE
           IF  ACCTFILE-STATUS = '00'
               SUBTRACT APPL-RESULT FROM APPL-RESULT
           ELSE
               ADD 12 TO ZERO GIVING APPL-RESULT
           END-IF
```

---

</SwmSnippet>

<SwmSnippet path="/app/cbl/CBACT01C.cbl" line="396">

---

After closing the account file, if it fails, we display the error status, copy <SwmToken path="app/cbl/CBACT01C.cbl" pos="400:3:5" line-data="               MOVE ACCTFILE-STATUS TO IO-STATUS">`ACCTFILE-STATUS`</SwmToken> to <SwmToken path="app/cbl/CBACT01C.cbl" pos="400:9:11" line-data="               MOVE ACCTFILE-STATUS TO IO-STATUS">`IO-STATUS`</SwmToken>, and call <SwmToken path="app/cbl/CBACT01C.cbl" pos="401:3:9" line-data="               PERFORM 9910-DISPLAY-IO-STATUS">`9910-DISPLAY-IO-STATUS`</SwmToken> for diagnostics before terminating. This helps pinpoint file issues.

```cobol
           IF  APPL-AOK
               CONTINUE
           ELSE
               DISPLAY 'ERROR CLOSING ACCOUNT FILE'
               MOVE ACCTFILE-STATUS TO IO-STATUS
               PERFORM 9910-DISPLAY-IO-STATUS
               PERFORM 9999-ABEND-PROGRAM
           END-IF
           EXIT.
```

---

</SwmSnippet>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBYXdzLW1haW5mcmFtZS1tb2Rlcm5pemF0aW9uLWNhcmRkZW1vJTNBJTNBbXVkYXNpbjE=" repo-name="aws-mainframe-modernization-carddemo"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
