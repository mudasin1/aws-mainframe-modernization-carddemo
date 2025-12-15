---
title: Transaction View Screen (COTRN1A)
---
The Transaction View screen (COTRN1A) allows users to enter a transaction ID and view all details of a specific credit card transaction, including card, merchant, and processing information. It is used for inquiry and validation of transaction records within the CardDemo application.

## Screen Preview

```
Tran: CT01              AWS Mainframe Modernization           Date: mm/dd/yy
Prog: COTRN01C                        CardDemo               Time: hh:mm:ss

                              View Transaction

     Enter Tran ID: ________________

     ----------------------------------------------------------------------

     Transaction ID: ________________   Card Number: ________________

     Type CD: __  Category CD: ____  Source: __________

     Description:                                                          
                  ____________________________________________________________

     Amount: ____________   Orig Date: __________   Proc Date: __________

     Merchant ID: _________   Merchant Name: ________________________________

     Merchant City: __________________________   Merchant Zip: __________


[Error/Status Message Area]

ENTER=Fetch  F3=Back  F4=Clear  F5=Browse Tran.
```

## Fields

### Transaction Name (TRNNAME)

- Fixed value: 'CT01' (from WS-TRANID)
- Display only, not editable.

### Title Line 1 (TITLE01)

- Fixed value: 'AWS Mainframe Modernization' (from CCDA-TITLE01)
- Display only.

### Date (CURDATE)

- Format: mm/dd/yy
- Populated from current date at runtime.
- Display only.

### Program Name (PGMNAME)

- Fixed value: 'COTRN01C' (from WS-PGMNAME)
- Display only.

### Title Line 2 (TITLE02)

- Fixed value: 'CardDemo' (from CCDA-TITLE02)
- Display only.

### Time (CURTIME)

- Format: hh:mm:ss
- Populated from current time at runtime.
- Display only.

### Enter Tran ID (TRNIDIN)

- Input field, 16 characters max.
- Required field. If left empty, error message is shown: 'Tran ID can NOT be empty...'
- Underlined, green color.
- On ENTER, triggers transaction lookup.

### Transaction ID (TRNID)

- Output field, 16 characters.
- Populated after successful lookup.
- Display only.

### Card Number (CARDNUM)

- Output field, 16 characters.
- Populated after successful lookup.
- Display only.

### Type Code (TTYPCD)

- Output field, 2 characters.
- Populated after successful lookup.
- Display only.

### Category Code (TCATCD)

- Output field, 4 characters.
- Populated after successful lookup.
- Display only.

### Source (TRNSRC)

- Output field, 10 characters.
- Populated after successful lookup.
- Display only.

### Description (TDESC)

- Output field, 60 characters (from TRAN-DESC, truncated if longer).
- Populated after successful lookup.
- Display only.

### Amount (TRNAMT)

- Output field, 12 characters, formatted as +99999999.99
- Populated after successful lookup.
- Display only.

### Orig Date (TORIGDT)

- Output field, 10 characters (from TRAN-ORIG-TS, likely formatted as date/time).
- Populated after successful lookup.
- Display only.

### Proc Date (TPROCDT)

- Output field, 10 characters (from TRAN-PROC-TS, likely formatted as date/time).
- Populated after successful lookup.
- Display only.

### Merchant ID (MID)

- Output field, 9 characters.
- Populated after successful lookup.
- Display only.

### Merchant Name (MNAME)

- Output field, 30 characters (from TRAN-MERCHANT-NAME, truncated if longer).
- Populated after successful lookup.
- Display only.

### Merchant City (MCITY)

- Output field, 25 characters (from TRAN-MERCHANT-CITY, truncated if longer).
- Populated after successful lookup.
- Display only.

### Merchant Zip (MZIP)

- Output field, 10 characters.
- Populated after successful lookup.
- Display only.

### Error/Status Message Area (ERRMSG)

- Output field, 78 characters.
- Used to display error or status messages (e.g., 'Tran ID can NOT be empty...', 'Transaction ID NOT found...', 'Invalid key pressed. Please see below...').
- Display only.

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBYXdzLW1haW5mcmFtZS1tb2Rlcm5pemF0aW9uLWNhcmRkZW1vJTNBJTNBbXVkYXNpbjE=" repo-name="aws-mainframe-modernization-carddemo"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
