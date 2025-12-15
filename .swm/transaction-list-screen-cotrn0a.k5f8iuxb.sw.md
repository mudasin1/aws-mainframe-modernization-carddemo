---
title: Transaction List Screen (COTRN0A)
---
The Transaction List screen (COTRN0A) provides users with a paginated view of their recent credit card transactions, allowing them to search by transaction ID and select a transaction for detailed viewing. It supports navigation through multiple pages and displays relevant status or error messages.

## Screen Preview

```
Tran: CT00           AWS Mainframe Modernization           Date: mm/dd/yy
Prog: COTRN00C                  CardDemo                  Time: hh:mm:ss

                              List Transactions                  Page: ________

     Search Tran ID: ________________

  Sel  Transaction ID    Date      Description               Amount   
  ---  ----------------  --------  --------------------------  ------------
   _   ________________  ________  __________________________  ____________
   _   ________________  ________  __________________________  ____________
   _   ________________  ________  __________________________  ____________
   _   ________________  ________  __________________________  ____________
   _   ________________  ________  __________________________  ____________
   _   ________________  ________  __________________________  ____________
   _   ________________  ________  __________________________  ____________
   _   ________________  ________  __________________________  ____________
   _   ________________  ________  __________________________  ____________
   _   ________________  ________  __________________________  ____________

            Type 'S' to View Transaction details from the list

[Error/Status Message Area]

ENTER=Continue  F3=Back  F7=Backward  F8=Forward
```

## Fields

### Transaction ID (Tran: CT00)

- Fixed value: 'CT00' (from WS-TRANID)
- Display only, not editable by user.

### Program Name (Prog: COTRN00C)

- Fixed value: 'COTRN00C' (from WS-PGMNAME)
- Display only, not editable by user.

### Title 1 (AWS Mainframe Modernization)

- Fixed value: 'AWS Mainframe Modernization' (from CCDA-TITLE01)
- Display only, not editable by user.

### Title 2 (CardDemo)

- Fixed value: 'CardDemo' (from CCDA-TITLE02)
- Display only, not editable by user.

### Date (Date: mm/dd/yy)

- Populated with current date in MM/DD/YY format (from WS-CURDATE-MM-DD-YY)
- Display only, not editable by user.

### Time (Time: hh:mm:ss)

- Populated with current time in HH:MM:SS format (from WS-CURTIME-HH-MM-SS)
- Display only, not editable by user.

### Page Number (Page: \_______\_)

- Shows the current page number (from PAGENUMI)
- Display only, not editable by user.

### Search Tran ID (TRNIDIN)

- Input field, 16 characters, underlined
- Accepts only numeric values (validated in COBOL: 'Tran ID must be Numeric ...')
- If left blank, no filtering is applied
- If non-numeric, error message is shown and field is highlighted.

### Transaction List Table (SELxxxx, TRNIDxx, TDATExx, TDESCxx, TAMTxxx)

- 10 rows, each with:
  - Sel: 1-character input, underlined (user can type 'S' or 's' to select a transaction)
  - Transaction ID: 16-character display, not editable
  - Date: 8-character display, not editable
  - Description: 26-character display, not editable
  - Amount: 12-character display, not editable
- Only 'Sel' column is user-editable; all others are display only.
- Only one 'S' selection is processed at a time; if invalid value is entered, error message is shown.
- If no transactions, rows are blank.

### Instructional Message (Type 'S' to View Transaction details from the list)

- Fixed instructional text
- Display only, not editable by user.

### Error/Status Message Area (ERRMSG)

- 78-character area at the bottom
- Populated with error or status messages from the COBOL program (e.g., invalid key, invalid selection, navigation messages)
- Display only, not editable by user.

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBYXdzLW1haW5mcmFtZS1tb2Rlcm5pemF0aW9uLWNhcmRkZW1vJTNBJTNBbXVkYXNpbjE=" repo-name="aws-mainframe-modernization-carddemo"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
