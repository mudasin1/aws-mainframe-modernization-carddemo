---
title: Bill Payment Screen (COBIL0A)
---
The Bill Payment screen (COBIL0A) allows users to pay their credit card balance in full by entering their account ID, viewing their current balance, and confirming the payment. It guides users through the payment process and provides feedback on errors or successful transactions.

## Screen Preview

```
Tran: CB00                      AWS Mainframe Modernization          Date: mm/dd/yy
Prog: COBIL00C                            CardDemo                   Time: hh:mm:ss

                                   Bill Payment

     Enter Acct ID: ___________

     -----------------------------------------------------------------

          Your current balance is: ______________


          Do you want to pay your balance now. Please confirm: _ (Y/N)




[Error/Status Message Area]

ENTER=Continue  F3=Back  F4=Clear
```

## Fields

### Transaction ID (TRNNAME)

- Displays the transaction ID for this screen, which is 'CB00'.
- Fixed value, not editable by the user.
- Used internally for navigation and tracking.

### Program Name (PGMNAME)

- Displays the program name, which is 'COBIL00C'.
- Fixed value, not editable by the user.
- Used for technical reference and debugging.

### Date (CURDATE)

- Displays the current date in mm/dd/yy format.
- Populated dynamically at runtime.
- Not editable by the user.

### Time (CURTIME)

- Displays the current time in hh:mm:ss format.
- Populated dynamically at runtime.
- Not editable by the user.

### Main Title (TITLE01)

- Displays the main title: 'AWS Mainframe Modernization'.
- Fixed value, not editable by the user.

### Subtitle (TITLE02)

- Displays the subtitle: 'CardDemo'.
- Fixed value, not editable by the user.

### Bill Payment Section Header

- Displays the section header 'Bill Payment'.
- Fixed value, not editable by the user.

### Account ID Input Field (ACTIDIN)

- User input field for entering the Account ID.
- Length: 11 characters (numeric).
- Required field: If left blank, an error message is shown ('Acct ID can NOT be empty...').
- Underlined, green color for emphasis.
- Cursor is positioned here by default.
- Validated for presence; must correspond to an existing account in the system.

### Current Balance Display (CURBAL)

- Displays the current balance for the entered account.
- Length: 14 characters, formatted as signed numeric (+9999999999.99).
- Read-only field, populated after a valid Account ID is entered and found.
- If the account is not found, an error message is shown.
- If the balance is zero or less, an error message is shown ('You have nothing to pay...').

### Payment Confirmation (CONFIRM)

- User input field to confirm payment.
- Length: 1 character.
- Accepts 'Y' or 'N' (case-insensitive).
- Underlined, green color for emphasis.
- If left blank, defaults to 'N' or prompts again.
- If invalid value is entered, error message is shown ('Invalid value. Valid values are (Y/N)...').

### Error/Status Message Area (ERRMSG)

- Displays error or status messages to the user.
- Length: 78 characters.
- Populated dynamically based on validation or system status.
- Examples: 'Acct ID can NOT be empty...', 'Account ID NOT found...', 'You have nothing to pay...', 'Payment successful. Your Transaction ID is ...', etc.

### Function Key Instructions

- Displays available function key actions at the bottom of the screen.
- Fixed text: 'ENTER=Continue  F3=Back  F4=Clear'.
- Not editable by the user.

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBYXdzLW1haW5mcmFtZS1tb2Rlcm5pemF0aW9uLWNhcmRkZW1vJTNBJTNBbXVkYXNpbjE=" repo-name="aws-mainframe-modernization-carddemo"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
