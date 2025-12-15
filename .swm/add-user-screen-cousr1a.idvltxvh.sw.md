---
title: Add User Screen (COUSR1A)
---
The Add User screen (COUSR1A) allows administrators to create new user accounts by entering personal and security details. It validates all required fields and provides feedback for errors or successful operations.

## Screen Preview

```
Tran: CU01            AWS Mainframe Modernization           Date: mm/dd/yy
Prog: COUSR01C                      CardDemo                Time: hh:mm:ss

                                   Add User

      First Name: ____________________      Last Name: ____________________

      User ID: ________   (8 Char)          Password: ________   (8 Char)

      User Type: _ (A=Admin, U=User)




[Error/Status Message Area]

ENTER=Add User  F3=Back  F4=Clear  F12=Exit
```

## Fields

### First Name (FNAME)

- Input field, length: 20 characters
- Required field; cannot be empty (validated in COBOL)
- Underlined, green color
- No explicit format restrictions beyond non-empty

### Last Name (LNAME)

- Input field, length: 20 characters
- Required field; cannot be empty (validated in COBOL)
- Underlined, green color
- No explicit format restrictions beyond non-empty

### User ID (USERID)

- Input field, length: 8 characters
- Required field; cannot be empty (validated in COBOL)
- Underlined, green color
- Must be unique (duplicate check performed when writing to file)
- No explicit format restrictions beyond non-empty

### Password (PASSWD)

- Input field, length: 8 characters
- Required field; cannot be empty (validated in COBOL)
- Underlined, green color, dark attribute (DRK)
- No explicit format restrictions beyond non-empty

### User Type (USRTYPE)

- Input field, length: 1 character
- Required field; cannot be empty (validated in COBOL)
- Underlined, green color
- Must be 'A' for Admin or 'U' for User (validated by context)
- No other values accepted

### Error/Status Message Area (ERRMSG)

- Output field, length: 78 characters
- Displays validation errors, status, or success messages
- Color: Red for errors, Green for success
- Content is set dynamically by the program

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBYXdzLW1haW5mcmFtZS1tb2Rlcm5pemF0aW9uLWNhcmRkZW1vJTNBJTNBbXVkYXNpbjE=" repo-name="aws-mainframe-modernization-carddemo"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
