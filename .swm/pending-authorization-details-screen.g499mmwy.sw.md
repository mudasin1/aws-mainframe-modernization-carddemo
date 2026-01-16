---
title: Pending Authorization Details Screen
---
The Pending Authorization Details screen provides a detailed, read-only view of a specific credit card authorization, including transaction, merchant, and fraud information, to support review and fraud management actions.

## Screen Preview

```
Tran: CPVD              AWS Mainframe Modernization          Date: mm/dd/yy
Prog: COPAUS1C                   CardDemo                   Time: hh:mm:ss

                           View Authorization Details

 Card #: ________________      Auth Date: __________  Auth Time: __________

 Auth Resp: _   Resp Reason: ____________________   Auth Code: ______

 Amount: ____________   POS Entry Mode: ____   Source   : __________

 MCC Code: ____   Card Exp. Date: _____   Auth Type: ______________

 Tran Id: ________________   Match Status: _   Fraud Status: __________

 Merchant Details ------------------------------------------------------

 Name: ___________________________   Merchant ID: ________________

 City: ___________________________   State: __   Zip: __________


[Error/Status Message Area]

 F3=Back  F5=Mark/Remove Fraud  F8=Next Auth
```

## Fields

### Transaction ID (Tran:)

- Fixed label. Value is always 'CPVD' as set by WS-CICS-TRANID.
- Not editable by the user.

### Program Name (Prog:)

- Fixed label. Value is always 'COPAUS1C' as set by WS-PGM-AUTH-DTL.
- Not editable by the user.

### Title 1 (AWS Mainframe Modernization)

- Fixed label. Value is always 'AWS Mainframe Modernization' as set by CCDA-TITLE01.
- Not editable by the user.

### Title 2 (CardDemo)

- Fixed label. Value is always 'CardDemo' as set by CCDA-TITLE02.
- Not editable by the user.

### Date (Date:)

- Format: mm/dd/yy
- Populated from current date at runtime.
- Not editable by the user.

### Time (Time:)

- Format: hh:mm:ss
- Populated from current time at runtime.
- Not editable by the user.

### View Authorization Details (Screen Title)

- Fixed label. Not editable by the user.

### Card Number (Card #:)

- Length: 16 characters
- Populated from PA-CARD-NUM
- Not editable by the user.

### Authorization Date (Auth Date:)

- Length: 10 characters
- Format: mm/dd/yy
- Populated from PA-AUTH-ORIG-DATE, formatted as mm/dd/yy
- Not editable by the user.

### Authorization Time (Auth Time:)

- Length: 10 characters
- Format: hh:mm:ss
- Populated from PA-AUTH-ORIG-TIME, formatted as hh:mm:ss
- Not editable by the user.

### Authorization Response (Auth Resp:)

- Length: 1 character
- Value is 'A' for approved (PA-AUTH-RESP-CODE = '00'), 'D' for declined (otherwise)
- Not editable by the user.

### Response Reason (Resp Reason:)

- Length: 20 characters
- Populated by matching PA-AUTH-RESP-REASON to a code/description table
- Format: '####-Description' (e.g., '3100-INVALID CARD')
- Not editable by the user.

### Authorization Code (Auth Code:)

- Length: 6 characters
- Populated from PA-PROCESSING-CODE
- Not editable by the user.

### Amount (Amount:)

- Length: 12 characters
- Populated from PA-APPROVED-AMT, formatted as -zzzzzzz9.99
- Not editable by the user.

### POS Entry Mode (POS Entry Mode:)

- Length: 4 characters
- Populated from PA-POS-ENTRY-MODE
- Not editable by the user.

### Source (Source   :)

- Length: 10 characters
- Populated from PA-MESSAGE-SOURCE
- Not editable by the user.

### MCC Code (MCC Code:)

- Length: 4 characters
- Populated from PA-MERCHANT-CATAGORY-CODE
- Not editable by the user.

### Card Expiry Date (Card Exp. Date:)

- Length: 5 characters
- Format: mm/yy
- Populated from PA-CARD-EXPIRY-DATE, formatted as mm/yy
- Not editable by the user.

### Authorization Type (Auth Type:)

- Length: 14 characters
- Populated from PA-AUTH-TYPE
- Not editable by the user.

### Transaction ID (Tran Id:)

- Length: 15 characters
- Populated from PA-TRANSACTION-ID
- Not editable by the user.

### Match Status (Match Status:)

- Length: 1 character
- Populated from PA-MATCH-STATUS
- Values: 'P' (Pending), 'D' (Declined), 'E' (Pending Expired), 'M' (Matched)
- Not editable by the user.

### Fraud Status (Fraud Status:)

- Length: 10 characters
- Populated from PA-AUTH-FRAUD and PA-FRAUD-RPT-DATE
- Format: 'F-YYYYMMDD' for confirmed fraud, 'R-YYYYMMDD' for removed fraud, '-' otherwise
- Not editable by the user.

### Merchant Name (Name:)

- Length: 25 characters
- Populated from PA-MERCHANT-NAME
- Not editable by the user.

### Merchant ID (Merchant ID:)

- Length: 15 characters
- Populated from PA-MERCHANT-ID
- Not editable by the user.

### Merchant City (City:)

- Length: 25 characters
- Populated from PA-MERCHANT-CITY
- Not editable by the user.

### Merchant State (State:)

- Length: 2 characters
- Populated from PA-MERCHANT-STATE
- Not editable by the user.

### Merchant Zip (Zip:)

- Length: 10 characters
- Populated from PA-MERCHANT-ZIP
- Not editable by the user.

### Error/Status Message Area

- Length: 78 characters
- Populated with status or error messages (e.g., invalid key, system errors, fraud marking status)
- Not editable by the user.

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBYXdzLW1haW5mcmFtZS1tb2Rlcm5pemF0aW9uLWNhcmRkZW1vJTNBJTNBbXVkYXNpbjE=" repo-name="aws-mainframe-modernization-carddemo"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
