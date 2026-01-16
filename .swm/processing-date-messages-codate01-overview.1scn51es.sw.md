---
title: Processing Date Messages (CODATE01) - Overview
---
# Overview

This document describes how requests for the current system date and time are processed. Incoming messages are received from a queue, a reply with the formatted date and time is generated, and the reply is sent to the output queue. Errors are logged, and all queues are closed at the end of processing.

```mermaid
flowchart TD
    node1["Startup and Queue Preparation"]:::HeadingStyle
    click node1 goToHeading "Startup and Queue Preparation"
    node1 --> node2{"Can all queues be opened?"}
    node2 -->|"No"| node4["Cleanup and Shutdown"]:::HeadingStyle
    click node4 goToHeading "Cleanup and Shutdown"
    node2 -->|"Yes"| node3["Request Retrieval"]:::HeadingStyle
    click node3 goToHeading "Request Retrieval"
    click node3 goToHeading "Reply Generation"
    node3 --> node5{"Are there more messages?"}
    node5 -->|"Yes"| node3
    node5 -->|"No"| node4
classDef HeadingStyle fill:#777777,stroke:#333,stroke-width:2px;
```

## Dependencies

### Programs

- CODATE01 (<SwmPath>[app/…/cbl/CODATE01.cbl](app/app-vsam-mq/cbl/CODATE01.cbl)</SwmPath>)
- MQOPEN
- MQGET
- MQPUT
- MQCLOSE

### Copybooks

- CMQGMOV
- CMQPMOV
- CMQMDV
- CMQODV
- CMQV
- CMQTML

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBYXdzLW1haW5mcmFtZS1tb2Rlcm5pemF0aW9uLWNhcmRkZW1vJTNBJTNBbXVkYXNpbjE=" repo-name="aws-mainframe-modernization-carddemo"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
