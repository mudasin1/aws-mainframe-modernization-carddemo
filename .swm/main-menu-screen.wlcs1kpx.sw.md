---
title: Main Menu Screen
---
The Main Menu screen provides users with access to all primary functions of the CardDemo application, allowing them to select from a list of available operations such as viewing accounts, updating information, managing credit cards, and more.

## Screen Preview

```
Tran: CM00           AWS Mainframe Modernization           Date: mm/dd/yy
Prog: COMEN01C                  CardDemo                  Time: hh:mm:ss

                                   Main Menu

                    1. Account View
                    2. Account Update
                    3. Credit Card List
                    4. Credit Card View
                    5. Credit Card Update
                    6. Transaction List
                    7. Transaction View
                    8. Transaction Add
                    9. Transaction Reports
                   10. Bill Payment
                   11. Pending Authorization View

               Please select an option : __



[Error/Status Message Area]

ENTER=Continue  F3=Exit
```

## Fields

### Transaction ID (Tran:)

- Fixed value: 'CM00' (from WS-TRANID)
- Display only, not editable by the user.

### Program Name (Prog:)

- Fixed value: 'COMEN01C' (from WS-PGMNAME)
- Display only, not editable by the user.

### Title Line 1 (AWS Mainframe Modernization)

- Fixed value: 'AWS Mainframe Modernization' (from CCDA-TITLE01)
- Display only, not editable by the user.

### Title Line 2 (CardDemo)

- Fixed value: 'CardDemo' (from CCDA-TITLE02)
- Display only, not editable by the user.

### Date (Date:)

- Format: mm/dd/yy (populated from WS-CURDATE-MM-DD-YY)
- Display only, not editable by the user.

### Time (Time:)

- Format: hh:mm:ss (populated from WS-CURTIME-HH-MM-SS)
- Display only, not editable by the user.

### Main Menu Header

- Fixed value: 'Main Menu'
- Display only, not editable by the user.

### Menu Options (1-11)

- Each line displays a menu option in the format 'N. Option Name'
- Option names are:
  1. Account View
  2. Account Update
  3. Credit Card List
  4. Credit Card View
  5. Credit Card Update
  6. Transaction List
  7. Transaction View
  8. Transaction Add
  9. Transaction Reports

10. Bill Payment
11. Pending Authorization View

- Display only, not editable by the user.

### Option Input Field

- Input field for user to enter the option number (2 characters, right-justified, zero-filled if needed)
- Only numeric values 1-11 are valid (must be within menu count)
- Validation: Must be numeric, not zero, and not greater than the number of options (11)
- If invalid, error message is shown
- Field is underlined and highlighted for input.

### Error/Status Message Area

- Up to 78 characters
- Used to display error messages (e.g., invalid key, invalid option, no access)
- Populated by WS-MESSAGE in the COBOL code
- Display only, not editable by the user.

### Function Key Instructions

- Fixed value: 'ENTER=Continue  F3=Exit'
- Display only, not editable by the user.

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBYXdzLW1haW5mcmFtZS1tb2Rlcm5pemF0aW9uLWNhcmRkZW1vJTNBJTNBbXVkYXNpbjE=" repo-name="aws-mainframe-modernization-carddemo"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
