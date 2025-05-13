      *----------------------------------------------------------------*
      * CUSTMNT - CUSTOMER MAINTENANCE PROGRAM                        *
      *                                                                *
      * THIS PROGRAM HANDLES CUSTOMER DATA MAINTENANCE OPERATIONS      *
      * INCLUDING ADD, UPDATE, DELETE, AND INQUIRY FUNCTIONS.          *
      *----------------------------------------------------------------*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUSTMNT.
       AUTHOR. LABELMATE EXAMPLE.
       DATE-WRITTEN. 2025-05-13.
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-MAINFRAME.
       OBJECT-COMPUTER. IBM-MAINFRAME.
       
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTOMER-FILE ASSIGN TO CUSTFILE
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CUST-ID
               FILE STATUS IS WS-FILE-STATUS.
           
           SELECT TRANSACTION-FILE ASSIGN TO TRANFILE
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRAN-STATUS.
           
           SELECT REPORT-FILE ASSIGN TO RPTFILE
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-REPORT-STATUS.
       
       DATA DIVISION.
       FILE SECTION.
       FD  CUSTOMER-FILE
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 100 CHARACTERS.
       01  CUSTOMER-RECORD.
           COPY CUSTCOPY.
       
       FD  TRANSACTION-FILE
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 80 CHARACTERS.
       01  TRANSACTION-RECORD.
           05  TRAN-CODE              PIC X(01).
               88  TRAN-ADD           VALUE 'A'.
               88  TRAN-UPDATE        VALUE 'U'.
               88  TRAN-DELETE        VALUE 'D'.
               88  TRAN-INQUIRY       VALUE 'I'.
           05  TRAN-CUST-ID           PIC X(06).
           05  TRAN-CUST-NAME         PIC X(30).
           05  TRAN-CUST-ADDR         PIC X(30).
           05  TRAN-CUST-PHONE        PIC X(13).
           
       FD  REPORT-FILE
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 132 CHARACTERS.
       01  REPORT-RECORD              PIC X(132).
       
       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS             PIC X(02) VALUE SPACES.
           88  FILE-SUCCESS           VALUE '00'.
           88  FILE-EOF               VALUE '10'.
           88  FILE-NOT-FOUND         VALUE '23'.
           88  FILE-ALREADY-EXISTS    VALUE '22'.
       
       01  WS-TRAN-STATUS             PIC X(02) VALUE SPACES.
           88  TRAN-SUCCESS           VALUE '00'.
           88  TRAN-EOF               VALUE '10'.
       
       01  WS-REPORT-STATUS           PIC X(02) VALUE SPACES.
           88  REPORT-SUCCESS         VALUE '00'.
       
       01  WS-SWITCHES.
           05  WS-END-OF-FILE-SW      PIC X(01) VALUE 'N'.
               88  END-OF-FILE        VALUE 'Y'.
           05  WS-VALID-DATA-SW       PIC X(01) VALUE 'Y'.
               88  VALID-DATA         VALUE 'Y'.
       
       01  WS-COUNTERS.
           05  WS-ADD-COUNT           PIC 9(05) VALUE ZEROS.
           05  WS-UPDATE-COUNT        PIC 9(05) VALUE ZEROS.
           05  WS-DELETE-COUNT        PIC 9(05) VALUE ZEROS.
           05  WS-INQUIRY-COUNT       PIC 9(05) VALUE ZEROS.
           05  WS-ERROR-COUNT         PIC 9(05) VALUE ZEROS.
       
       01  WS-CURRENT-DATE.
           05  WS-CURRENT-YEAR        PIC 9(04).
           05  WS-CURRENT-MONTH       PIC 9(02).
           05  WS-CURRENT-DAY         PIC 9(02).
       
       01  WS-REPORT-HEADER.
           05  FILLER                 PIC X(20) VALUE 'CUSTOMER MAINTENANCE '.
           05  FILLER                 PIC X(06) VALUE 'REPORT'.
           05  FILLER                 PIC X(20) VALUE SPACES.
           05  FILLER                 PIC X(04) VALUE 'DATE'.
           05  FILLER                 PIC X(01) VALUE SPACES.
           05  WS-HEADER-DATE.
               10  WS-HEADER-MONTH    PIC 9(02).
               10  FILLER             PIC X(01) VALUE '/'.
               10  WS-HEADER-DAY      PIC 9(02).
               10  FILLER             PIC X(01) VALUE '/'.
               10  WS-HEADER-YEAR     PIC 9(04).
           05  FILLER                 PIC X(73) VALUE SPACES.
       
       01  WS-COLUMN-HEADER.
           05  FILLER                 PIC X(06) VALUE 'TRAN  '.
           05  FILLER                 PIC X(08) VALUE 'CUST ID '.
           05  FILLER                 PIC X(32) VALUE 'CUSTOMER NAME                    '.
           05  FILLER                 PIC X(32) VALUE 'ADDRESS                          '.
           05  FILLER                 PIC X(15) VALUE 'PHONE          '.
           05  FILLER                 PIC X(39) VALUE SPACES.
       
       01  WS-DETAIL-LINE.
           05  WS-DL-TRAN-CODE        PIC X(06).
           05  WS-DL-CUST-ID          PIC X(08).
           05  WS-DL-CUST-NAME        PIC X(32).
           05  WS-DL-CUST-ADDR        PIC X(32).
           05  WS-DL-CUST-PHONE       PIC X(15).
           05  FILLER                 PIC X(39) VALUE SPACES.
       
       01  WS-SUMMARY-LINE.
           05  FILLER                 PIC X(20) VALUE 'PROCESSING SUMMARY: '.
           05  FILLER                 PIC X(10) VALUE 'ADDS:     '.
           05  WS-SL-ADD-COUNT        PIC ZZ,ZZ9.
           05  FILLER                 PIC X(10) VALUE ' UPDATES: '.
           05  WS-SL-UPDATE-COUNT     PIC ZZ,ZZ9.
           05  FILLER                 PIC X(10) VALUE ' DELETES: '.
           05  WS-SL-DELETE-COUNT     PIC ZZ,ZZ9.
           05  FILLER                 PIC X(11) VALUE ' INQUIRIES:'.
           05  WS-SL-INQUIRY-COUNT    PIC ZZ,ZZ9.
           05  FILLER                 PIC X(10) VALUE ' ERRORS:  '.
           05  WS-SL-ERROR-COUNT      PIC ZZ,ZZ9.
           05  FILLER                 PIC X(37) VALUE SPACES.
       
       PROCEDURE DIVISION.
       0000-MAIN-PROCESS.
           PERFORM 1000-INITIALIZATION
           PERFORM 2000-PROCESS-TRANSACTIONS
               UNTIL END-OF-FILE
           PERFORM 3000-TERMINATION
           GOBACK
           .
       
       1000-INITIALIZATION.
           OPEN INPUT TRANSACTION-FILE
                I-O   CUSTOMER-FILE
                OUTPUT REPORT-FILE
                
           IF NOT FILE-SUCCESS
               DISPLAY 'ERROR OPENING CUSTOMER FILE: ' WS-FILE-STATUS
               MOVE 'Y' TO WS-END-OF-FILE-SW
           END-IF
           
           IF NOT TRAN-SUCCESS
               DISPLAY 'ERROR OPENING TRANSACTION FILE: ' WS-TRAN-STATUS
               MOVE 'Y' TO WS-END-OF-FILE-SW
           END-IF
           
           IF NOT REPORT-SUCCESS
               DISPLAY 'ERROR OPENING REPORT FILE: ' WS-REPORT-STATUS
               MOVE 'Y' TO WS-END-OF-FILE-SW
           END-IF
           
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE
           MOVE WS-CURRENT-MONTH TO WS-HEADER-MONTH
           MOVE WS-CURRENT-DAY TO WS-HEADER-DAY
           MOVE WS-CURRENT-YEAR TO WS-HEADER-YEAR
           
           WRITE REPORT-RECORD FROM WS-REPORT-HEADER
           WRITE REPORT-RECORD FROM WS-COLUMN-HEADER
           
           READ TRANSACTION-FILE
               AT END MOVE 'Y' TO WS-END-OF-FILE-SW
           END-READ
           .
           
       2000-PROCESS-TRANSACTIONS.
           EVALUATE TRUE
               WHEN TRAN-ADD
                   PERFORM 2100-ADD-CUSTOMER
               WHEN TRAN-UPDATE
                   PERFORM 2200-UPDATE-CUSTOMER
               WHEN TRAN-DELETE
                   PERFORM 2300-DELETE-CUSTOMER
               WHEN TRAN-INQUIRY
                   PERFORM 2400-INQUIRY-CUSTOMER
               WHEN OTHER
                   MOVE 'INVALID' TO WS-DL-TRAN-CODE
                   MOVE TRAN-CUST-ID TO WS-DL-CUST-ID
                   MOVE SPACES TO WS-DL-CUST-NAME
                   MOVE SPACES TO WS-DL-CUST-ADDR
                   MOVE SPACES TO WS-DL-CUST-PHONE
                   WRITE REPORT-RECORD FROM WS-DETAIL-LINE
                   ADD 1 TO WS-ERROR-COUNT
           END-EVALUATE
           
           READ TRANSACTION-FILE
               AT END MOVE 'Y' TO WS-END-OF-FILE-SW
           END-READ
           .
           
       2100-ADD-CUSTOMER.
           MOVE 'ADD' TO WS-DL-TRAN-CODE
           MOVE TRAN-CUST-ID TO CUST-ID
           MOVE TRAN-CUST-ID TO WS-DL-CUST-ID
           
           READ CUSTOMER-FILE
               INVALID KEY
                   PERFORM 2110-PERFORM-ADD
               NOT INVALID KEY
                   MOVE TRAN-CUST-ID TO WS-DL-CUST-ID
                   MOVE 'ALREADY EXISTS' TO WS-DL-CUST-NAME
                   MOVE SPACES TO WS-DL-CUST-ADDR
                   MOVE SPACES TO WS-DL-CUST-PHONE
                   WRITE REPORT-RECORD FROM WS-DETAIL-LINE
                   ADD 1 TO WS-ERROR-COUNT
           END-READ
           .
           
       2110-PERFORM-ADD.
           MOVE TRAN-CUST-ID TO CUST-ID
           MOVE TRAN-CUST-NAME TO CUST-NAME
           MOVE TRAN-CUST-ADDR TO CUST-ADDR
           MOVE TRAN-CUST-PHONE TO CUST-PHONE
           MOVE FUNCTION CURRENT-DATE TO CUST-LAST-UPDATED
           
           WRITE CUSTOMER-RECORD
               INVALID KEY
                   MOVE 'WRITE ERROR' TO WS-DL-CUST-NAME
                   MOVE WS-FILE-STATUS TO WS-DL-CUST-ADDR
                   MOVE SPACES TO WS-DL-CUST-PHONE
                   ADD 1 TO WS-ERROR-COUNT
               NOT INVALID KEY
                   MOVE TRAN-CUST-NAME TO WS-DL-CUST-NAME
                   MOVE TRAN-CUST-ADDR TO WS-DL-CUST-ADDR
                   MOVE TRAN-CUST-PHONE TO WS-DL-CUST-PHONE
                   ADD 1 TO WS-ADD-COUNT
           END-WRITE
           
           WRITE REPORT-RECORD FROM WS-DETAIL-LINE
           .
           
       2200-UPDATE-CUSTOMER.
           MOVE 'UPDATE' TO WS-DL-TRAN-CODE
           MOVE TRAN-CUST-ID TO CUST-ID
           MOVE TRAN-CUST-ID TO WS-DL-CUST-ID
           
           READ CUSTOMER-FILE
               INVALID KEY
                   MOVE 'NOT FOUND' TO WS-DL-CUST-NAME
                   MOVE SPACES TO WS-DL-CUST-ADDR
                   MOVE SPACES TO WS-DL-CUST-PHONE
                   WRITE REPORT-RECORD FROM WS-DETAIL-LINE
                   ADD 1 TO WS-ERROR-COUNT
               NOT INVALID KEY
                   PERFORM 2210-PERFORM-UPDATE
           END-READ
           .
           
       2210-PERFORM-UPDATE.
           MOVE TRAN-CUST-NAME TO CUST-NAME
           MOVE TRAN-CUST-ADDR TO CUST-ADDR
           MOVE TRAN-CUST-PHONE TO CUST-PHONE
           MOVE FUNCTION CURRENT-DATE TO CUST-LAST-UPDATED
           
           REWRITE CUSTOMER-RECORD
               INVALID KEY
                   MOVE 'REWRITE ERROR' TO WS-DL-CUST-NAME
                   MOVE WS-FILE-STATUS TO WS-DL-CUST-ADDR
                   MOVE SPACES TO WS-DL-CUST-PHONE
                   ADD 1 TO WS-ERROR-COUNT
               NOT INVALID KEY
                   MOVE TRAN-CUST-NAME TO WS-DL-CUST-NAME
                   MOVE TRAN-CUST-ADDR TO WS-DL-CUST-ADDR
                   MOVE TRAN-CUST-PHONE TO WS-DL-CUST-PHONE
                   ADD 1 TO WS-UPDATE-COUNT
           END-REWRITE
           
           WRITE REPORT-RECORD FROM WS-DETAIL-LINE
           .
           
       2300-DELETE-CUSTOMER.
           MOVE 'DELETE' TO WS-DL-TRAN-CODE
           MOVE TRAN-CUST-ID TO CUST-ID
           MOVE TRAN-CUST-ID TO WS-DL-CUST-ID
           
           READ CUSTOMER-FILE
               INVALID KEY
                   MOVE 'NOT FOUND' TO WS-DL-CUST-NAME
                   MOVE SPACES TO WS-DL-CUST-ADDR
                   MOVE SPACES TO WS-DL-CUST-PHONE
                   WRITE REPORT-RECORD FROM WS-DETAIL-LINE
                   ADD 1 TO WS-ERROR-COUNT
               NOT INVALID KEY
                   MOVE CUST-NAME TO WS-DL-CUST-NAME
                   MOVE CUST-ADDR TO WS-DL-CUST-ADDR
                   MOVE CUST-PHONE TO WS-DL-CUST-PHONE
                   
                   DELETE CUSTOMER-FILE
                       INVALID KEY
                           MOVE 'DELETE ERROR' TO WS-DL-CUST-NAME
                           MOVE WS-FILE-STATUS TO WS-DL-CUST-ADDR
                           MOVE SPACES TO WS-DL-CUST-PHONE
                           ADD 1 TO WS-ERROR-COUNT
                       NOT INVALID KEY
                           ADD 1 TO WS-DELETE-COUNT
                   END-DELETE
                   
                   WRITE REPORT-RECORD FROM WS-DETAIL-LINE
           END-READ
           .
           
       2400-INQUIRY-CUSTOMER.
           MOVE 'INQUIRE' TO WS-DL-TRAN-CODE
           MOVE TRAN-CUST-ID TO CUST-ID
           MOVE TRAN-CUST-ID TO WS-DL-CUST-ID
           
           READ CUSTOMER-FILE
               INVALID KEY
                   MOVE 'NOT FOUND' TO WS-DL-CUST-NAME
                   MOVE SPACES TO WS-DL-CUST-ADDR
                   MOVE SPACES TO WS-DL-CUST-PHONE
                   ADD 1 TO WS-ERROR-COUNT
               NOT INVALID KEY
                   MOVE CUST-NAME TO WS-DL-CUST-NAME
                   MOVE CUST-ADDR TO WS-DL-CUST-ADDR
                   MOVE CUST-PHONE TO WS-DL-CUST-PHONE
                   ADD 1 TO WS-INQUIRY-COUNT
           END-READ
           
           WRITE REPORT-RECORD FROM WS-DETAIL-LINE
           .
           
       3000-TERMINATION.
           MOVE WS-ADD-COUNT TO WS-SL-ADD-COUNT
           MOVE WS-UPDATE-COUNT TO WS-SL-UPDATE-COUNT
           MOVE WS-DELETE-COUNT TO WS-SL-DELETE-COUNT
           MOVE WS-INQUIRY-COUNT TO WS-SL-INQUIRY-COUNT
           MOVE WS-ERROR-COUNT TO WS-SL-ERROR-COUNT
           
           WRITE REPORT-RECORD FROM SPACES
           WRITE REPORT-RECORD FROM WS-SUMMARY-LINE
           
           CLOSE CUSTOMER-FILE
                 TRANSACTION-FILE
                 REPORT-FILE
           .
