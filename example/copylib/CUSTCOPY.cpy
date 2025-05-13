      *----------------------------------------------------------------*
      * CUSTCOPY - CUSTOMER RECORD LAYOUT                             *
      *----------------------------------------------------------------*
       01  CUSTOMER-DETAILS.
           05  CUST-ID                PIC X(06).
           05  CUST-NAME              PIC X(30).
           05  CUST-ADDR              PIC X(30).
           05  CUST-PHONE             PIC X(13).
           05  CUST-BALANCE           PIC S9(07)V99 COMP-3.
           05  CUST-CREDIT-LIMIT      PIC S9(07)V99 COMP-3.
           05  CUST-LAST-UPDATED.
               10  CUST-LU-YEAR       PIC 9(04).
               10  CUST-LU-MONTH      PIC 9(02).
               10  CUST-LU-DAY        PIC 9(02).
           05  CUST-STATUS            PIC X(01).
               88  CUST-ACTIVE        VALUE 'A'.
               88  CUST-INACTIVE      VALUE 'I'.
               88  CUST-SUSPENDED     VALUE 'S'.
           05  FILLER                 PIC X(05).
