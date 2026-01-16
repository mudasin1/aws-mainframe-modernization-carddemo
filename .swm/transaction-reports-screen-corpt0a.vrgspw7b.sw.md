---
title: Transaction Reports Screen (CORPT0A)
---
The Transaction Reports screen (CORPT0A) allows users to request printing of transaction reports for the current month, year, or a custom date range. Users select the report type, optionally enter a date range, and confirm submission for batch processing.

## Screen Preview

```
Tran: CR00              AWS Mainframe Modernization          Date: mm/dd/yy
Prog: CORPT00C                        CardDemo                   Time: hh:mm:ss

                              Transaction Reports

          _   Monthly (Current Month)

          _   Yearly (Current Year)

          _   Custom (Date Range)

               Start Date : __ / __ / ____   (MM/DD/YYYY)
                 End Date : __ / __ / ____   (MM/DD/YYYY)

      The Report will be submitted for printing. Please confirm: _ (Y/N)

[Error/Status Message Area]

ENTER=Continue  F3=Back
```

## Fields

### Transaction ID (Tran:)

- Fixed value: 'CR00' (from WS-TRANID)
- Display only, not editable by user.

### Program Name (Prog:)

- Fixed value: 'CORPT00C' (from WS-PGMNAME)
- Display only, not editable by user.

### Title 1 (AWS Mainframe Modernization)

- Fixed value: 'AWS Mainframe Modernization' (from CCDA-TITLE01)
- Display only, not editable by user.

### Title 2 (CardDemo)

- Fixed value: 'CardDemo' (from CCDA-TITLE02)
- Display only, not editable by user.

### Date (Date:)

- Format: mm/dd/yy
- Populated from current date at runtime
- Display only, not editable by user.

### Time (Time:)

- Format: hh:mm:ss
- Populated from current time at runtime
- Display only, not editable by user.

### Monthly Report Selection (Monthly)

- Single character input field (usually blank or marked with 'X' or similar)
- Only one of Monthly, Yearly, or Custom should be selected
- Underlined, green color
- No validation in BMS, but COBOL ensures only one report type is selected.

### Yearly Report Selection (Yearly)

- Single character input field (usually blank or marked with 'X' or similar)
- Only one of Monthly, Yearly, or Custom should be selected
- Underlined, green color
- No validation in BMS, but COBOL ensures only one report type is selected.

### Custom Report Selection (Custom)

- Single character input field (usually blank or marked with 'X' or similar)
- Only one of Monthly, Yearly, or Custom should be selected
- Underlined, green color
- No validation in BMS, but COBOL ensures only one report type is selected.

### Start Date (Custom Date Range)

- Three fields: MM (2 digits), DD (2 digits), YYYY (4 digits)
- All fields required if Custom is selected
- Must be numeric
- MM: 01-12, DD: 01-31, YYYY: 4 digits
- Validated in COBOL for presence, numeric, and valid ranges
- Additional validation via date conversion subroutine (CSUTLDTC)
- Error message shown if invalid.

### End Date (Custom Date Range)

- Three fields: MM (2 digits), DD (2 digits), YYYY (4 digits)
- All fields required if Custom is selected
- Must be numeric
- MM: 01-12, DD: 01-31, YYYY: 4 digits
- Validated in COBOL for presence, numeric, and valid ranges
- Additional validation via date conversion subroutine (CSUTLDTC)
- Error message shown if invalid.

### Confirmation (Please confirm: \_ (Y/N))

- Single character input field
- Required before submitting the report for printing
- Accepts 'Y'/'y' or 'N'/'n'
- Any other value triggers an error message
- Underlined, green color

### Error/Status Message Area

- Up to 78 characters
- Used to display validation errors, status, or instructions
- Populated by COBOL program logic
- Not editable by user.

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBYXdzLW1haW5mcmFtZS1tb2Rlcm5pemF0aW9uLWNhcmRkZW1vJTNBJTNBbXVkYXNpbjE=" repo-name="aws-mainframe-modernization-carddemo"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
