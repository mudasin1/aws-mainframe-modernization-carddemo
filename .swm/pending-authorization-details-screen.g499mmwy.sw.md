---
title: Pending Authorization Details Screen
---
The Pending Authorization Details screen provides a detailed view of a single credit card authorization, including transaction, merchant, and fraud information. It is used by operators to review, mark, or remove fraud status, and navigate through pending authorizations.

## Screen Preview

```
Tran: CPVD              AWS Mainframe Modernization          Date: mm/dd/yy
Prog: COPAUS1C                    CardDemo                   Time: hh:mm:ss

                           View Authorization Details

  Card #: ________________      Auth Date: __________  Auth Time: __________
  Auth Resp: _   Resp Reason: ____________________   Auth Code: ______
  Amount: ____________   POS Entry Mode: ____   Source   : __________
  MCC Code: ____   Card Exp. Date: _____   Auth Type: ______________
  Tran Id: ________________   Match Status: _   Fraud Status: __________

  Merchant Details --------------------------------------------------------
  Name: ___________________________   Merchant ID: ________________
  City: ___________________________   State: __   Zip: __________


[Error/Status Message Area]

 F3=Back  F5=Mark/Remove Fraud  F8=Next Auth
```

## Fields

### Transaction ID (Tran:)

- Fixed label. Value is always 'CPVD' as set in the COBOL variable WS-CICS-TRANID.
- Not editable by the user.

### Program Name (Prog:)

- Fixed label. Value is always 'COPAUS1C' as set in the COBOL variable WS-PGM-AUTH-DTL.
- Not editable by the user.

### Title 1 (AWS Mainframe Modernization)

- Fixed label. Value is always 'AWS Mainframe Modernization' as set in CCDA-TITLE01.
- Not editable by the user.

### Title 2 (CardDemo)

- Fixed label. Value is always 'CardDemo' as set in CCDA-TITLE02.
- Not editable by the user.

### Date (Date:)

- Displays the current date in mm/dd/yy format.
- Populated at runtime using the system date.
- Not editable by the user.

### Time (Time:)

- Displays the current time in hh:mm:ss format.
- Populated at runtime using the system time.
- Not editable by the user.

### View Authorization Details (Screen Title)

- Fixed label. Not editable by the user.

### Card Number (Card #:)

- 16-character field.
- Populated from PA-CARD-NUM in the IMS segment.
- Not editable by the user.

### Authorization Date (Auth Date:)

- 10-character field (mm/dd/yy).
- Populated from PA-AUTH-ORIG-DATE, formatted in COBOL logic.
- Not editable by the user.

### Authorization Time (Auth Time:)

- 10-character field (hh:mm:ss).
- Populated from PA-AUTH-ORIG-TIME, formatted in COBOL logic.
- Not editable by the user.

### Authorization Response (Auth Resp:)

- 1-character field.
- 'A' for approved (PA-AUTH-RESP-CODE = '00'), 'D' for declined (otherwise).
- Color changes: green for approved, red for declined.
- Not editable by the user.

### Response Reason (Resp Reason:)

- 20-character field.
- Populated by matching PA-AUTH-RESP-REASON to a lookup table in COBOL (WS-DECLINE-REASON-TAB).
- Format: code-description (e.g., '3100-INVALID CARD').
- Not editable by the user.

### Authorization Code (Auth Code:)

- 6-character field.
- Populated from PA-PROCESSING-CODE.
- Not editable by the user.

### Amount (Amount:)

- 12-character field, formatted as -zzzzzzz9.99.
- Populated from PA-APPROVED-AMT.
- Not editable by the user.

### POS Entry Mode

- 4-character field.
- Populated from PA-POS-ENTRY-MODE.
- Not editable by the user.

### Source

- 10-character field.
- Populated from PA-MESSAGE-SOURCE.
- Not editable by the user.

### MCC Code

- 4-character field.
- Populated from PA-MERCHANT-CATAGORY-CODE.
- Not editable by the user.

### Card Expiry Date (Card Exp. Date:)

- 5-character field (mm/yy).
- Populated from PA-CARD-EXPIRY-DATE, formatted in COBOL logic.
- Not editable by the user.

### Authorization Type (Auth Type:)

- 14-character field.
- Populated from PA-AUTH-TYPE.
- Not editable by the user.

### Transaction ID (Tran Id:)

- 15-character field.
- Populated from PA-TRANSACTION-ID.
- Not editable by the user.

### Match Status

- 1-character field.
- Populated from PA-MATCH-STATUS.
- Values: 'P' (Pending), 'D' (Declined), 'E' (Pending Expired), 'M' (Matched).
- Not editable by the user.

### Fraud Status

- 10-character field.
- If PA-FRAUD-CONFIRMED or PA-FRAUD-REMOVED, displays the fraud flag, a dash, and the fraud report date (e.g., 'F-20230719'). Otherwise, displays '-'.
- Not editable by the user.

### Merchant Name (Name:)

- 25-character field.
- Populated from PA-MERCHANT-NAME.
- Not editable by the user.

### Merchant ID

- 15-character field.
- Populated from PA-MERCHANT-ID.
- Not editable by the user.

### Merchant City (City:)

- 25-character field.
- Populated from PA-MERCHANT-CITY.
- Not editable by the user.

### Merchant State (State:)

- 2-character field.
- Populated from PA-MERCHANT-STATE.
- Not editable by the user.

### Merchant Zip (Zip:)

- 10-character field.
- Populated from PA-MERCHANT-ZIP.
- Not editable by the user.

### Error/Status Message Area

- 78-character field at the bottom of the screen.
- Populated with error or status messages from the COBOL logic (e.g., invalid key, system errors, fraud tagging status).
- Not editable by the user.

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBYXdzLW1haW5mcmFtZS1tb2Rlcm5pemF0aW9uLWNhcmRkZW1vJTNBJTNBbXVkYXNpbjE=" repo-name="aws-mainframe-modernization-carddemo"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
