---
title: Add User Screen (COUSR1A)
---
The Add User screen (COUSR1A) allows administrators to create new user accounts by entering required personal and security information. It validates all fields and provides feedback for errors or successful creation, supporting both regular and admin user types.

## Screen Preview

```
Tran: CU01              AWS Mainframe Modernization          Date: mm/dd/yy
Prog: COUSR01C                         CardDemo                   Time: hh:mm:ss

                                   Add User

      First Name: ____________________    Last Name: ____________________

      User ID: ________ (8 Char)         Password: ________ (8 Char)

      User Type: _ (A=Admin, U=User)




[Error/Status Message Area]

ENTER=Add User  F3=Back  F4=Clear  F12=Exit
```

## Fields

### Transaction ID (Tran:)

- Fixed value: 'CU01' (from WS-TRANID)
- Displayed at the top left of the screen
- Not editable by the user.

### Program Name (Prog:)

- Fixed value: 'COUSR01C' (from WS-PGMNAME)
- Displayed at the top left, second row
- Not editable by the user.

### Date (Date:)

- Format: mm/dd/yy
- Populated dynamically from the system date (WS-CURDATE-MM-DD-YY)
- Not editable by the user.

### Time (Time:)

- Format: hh:mm:ss
- Populated dynamically from the system time (WS-CURTIME-HH-MM-SS)
- Not editable by the user.

### Title (AWS Mainframe Modernization / CardDemo)

- Fixed titles from CCDA-TITLE01 and CCDA-TITLE02
- Not editable by the user.

### Add User (Screen Section Title)

- Fixed label 'Add User' centered on the screen
- Not editable by the user.

### First Name (First Name:)

- Input field, 20 characters max
- Required field: cannot be empty (validated in COBOL)
- Underlined, green color
- Cursor starts here on new screen
- If empty, error message is shown and cursor returns here.

### Last Name (Last Name:)

- Input field, 20 characters max
- Required field: cannot be empty (validated in COBOL)
- Underlined, green color
- If empty, error message is shown and cursor returns here.

### User ID (User ID:)

- Input field, 8 characters max
- Required field: cannot be empty (validated in COBOL)
- Underlined, green color
- If empty, error message is shown and cursor returns here
- If duplicate, error message is shown and cursor returns here.

### Password (Password:)

- Input field, 8 characters max
- Required field: cannot be empty (validated in COBOL)
- Underlined, green color, dark attribute (input is masked)
- If empty, error message is shown and cursor returns here.

### User Type (User Type:)

- Input field, 1 character
- Required field: cannot be empty (validated in COBOL)
- Accepts only 'A' (Admin) or 'U' (User) (hint shown on screen)
- Underlined, green color
- If empty, error message is shown and cursor returns here.

### Error/Status Message Area (\[Error/Status Message Area\])

- 78-character area at the bottom of the screen
- Displays validation errors, status, or success messages
- Color: Red for errors, Green for success
- Populated by WS-MESSAGE in COBOL
- Not editable by the user.

### Function Key Instructions (ENTER=Add User  F3=Back  F4=Clear  F12=Exit)

- Fixed instruction line at the bottom
- Not editable by the user.
- Explains available function keys:
  - ENTER: Add User
  - F3: Back
  - F4: Clear
  - F12: Exit

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBYXdzLW1haW5mcmFtZS1tb2Rlcm5pemF0aW9uLWNhcmRkZW1vJTNBJTNBbXVkYXNpbjE=" repo-name="aws-mainframe-modernization-carddemo"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
