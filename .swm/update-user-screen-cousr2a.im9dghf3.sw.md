---
title: Update User Screen (COUSR2A)
---
The Update User screen (COUSR2A) allows administrators to fetch and update user details in the CardDemo application. It provides fields for entering and modifying user information, with validations to ensure data integrity.

## Screen Preview

```
Tran: CU02           AWS Mainframe Modernization               Date: mm/dd/yy
Prog: COUSR02C                     CardDemo                   Time: hh:mm:ss

                                   Update User

      Enter User ID: ________

      ***********************************************
      ***********************

      First Name:  ____________________   Last Name:  ____________________

      Password:  ________   (8 Char)

      User Type: _   (A=Admin, U=User)



[Error/Status Message Area]

ENTER=Fetch  F3=Save&Exit  F4=Clear  F5=Save  F12=Cancel
```

## Fields

### Transaction ID (Tran: CU02)

- Fixed value: 'CU02' (from WS-TRANID)
- Display only, not editable
- Used for transaction routing and identification

### Program Name (Prog: COUSR02C)

- Fixed value: 'COUSR02C' (from WS-PGMNAME)
- Display only, not editable
- Indicates the backend program handling the screen

### Date (Date: mm/dd/yy)

- Format: mm/dd/yy
- Populated dynamically from system date
- Display only, not editable

### Time (Time: hh:mm:ss)

- Format: hh:mm:ss
- Populated dynamically from system time
- Display only, not editable

### Title (AWS Mainframe Modernization / CardDemo)

- Fixed titles from CCDA-TITLE01 and CCDA-TITLE02
- Display only, not editable

### Screen Function Title (Update User)

- Fixed label
- Display only, not editable

### User ID Input (Enter User ID)

- Input field, 8 characters max
- Required: must not be empty (validated in COBOL)
- Underlined, green color
- If not found, error message displayed
- Used to fetch and update user details

### First Name Input (First Name)

- Input field, 20 characters max
- Required: must not be empty (validated in COBOL)
- Underlined, green color
- Populated from user record after fetch

### Last Name Input (Last Name)

- Input field, 20 characters max
- Required: must not be empty (validated in COBOL)
- Underlined, green color
- Populated from user record after fetch

### Password Input (Password)

- Input field, 8 characters max
- Required: must not be empty (validated in COBOL)
- Underlined, green color, dark attribute
- Populated from user record after fetch
- Must be exactly 8 characters

### User Type Input (User Type)

- Input field, 1 character
- Required: must not be empty (validated in COBOL)
- Underlined, green color
- Accepts 'A' (Admin) or 'U' (User) only
- Validation performed in COBOL

### Error/Status Message Area

- Output field, 78 characters max
- Used to display error or status messages
- Populated by COBOL logic (e.g., validation errors, update status)
- Color: Red for errors, Neutral/Green for status

### Function Key Instructions

- Fixed instructions at bottom of screen
- ENTER=Fetch, F3=Save&Exit, F4=Clear, F5=Save, F12=Cancel
- Not editable, for user guidance

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBYXdzLW1haW5mcmFtZS1tb2Rlcm5pemF0aW9uLWNhcmRkZW1vJTNBJTNBbXVkYXNpbjE=" repo-name="aws-mainframe-modernization-carddemo"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
