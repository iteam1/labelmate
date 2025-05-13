      *----------------------------------------------------------------*
      * INVNTRY - INVENTORY MANAGEMENT PROGRAM                        *
      *                                                                *
      * THIS PROGRAM HANDLES INVENTORY MANAGEMENT OPERATIONS           *
      * INCLUDING STOCK UPDATES, REORDER PROCESSING, AND REPORTING.    *
      *----------------------------------------------------------------*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. INVNTRY.
       AUTHOR. LABELMATE EXAMPLE.
       DATE-WRITTEN. 2025-05-13.
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-MAINFRAME.
       OBJECT-COMPUTER. IBM-MAINFRAME.
       
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INVENTORY-FILE ASSIGN TO INVFILE
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS INV-ITEM-CODE
               FILE STATUS IS WS-INV-STATUS.
           
           SELECT TRANSACTION-FILE ASSIGN TO TRANFILE
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRAN-STATUS.
           
           SELECT REPORT-FILE ASSIGN TO RPTFILE
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-REPORT-STATUS.
           
           SELECT REORDER-FILE ASSIGN TO REORDER
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-REORDER-STATUS.
       
       DATA DIVISION.
       FILE SECTION.
       FD  INVENTORY-FILE
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 150 CHARACTERS.
       01  INVENTORY-RECORD.
           COPY INVCOPY.
       
       FD  TRANSACTION-FILE
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 80 CHARACTERS.
       01  TRANSACTION-RECORD.
           05  TRAN-CODE              PIC X(01).
               88  TRAN-RECEIPT       VALUE 'R'.
               88  TRAN-ISSUE         VALUE 'I'.
               88  TRAN-ADJUST        VALUE 'A'.
           05  TRAN-ITEM-CODE         PIC X(10).
           05  TRAN-QUANTITY          PIC S9(05) COMP-3.
           05  TRAN-DATE.
               10  TRAN-YEAR          PIC 9(04).
               10  TRAN-MONTH         PIC 9(02).
               10  TRAN-DAY           PIC 9(02).
           05  TRAN-REFERENCE         PIC X(15).
           05  TRAN-REASON-CODE       PIC X(03).
           05  TRAN-LOCATION          PIC X(05).
           05  FILLER                 PIC X(33).
           
       FD  REPORT-FILE
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 132 CHARACTERS.
       01  REPORT-RECORD              PIC X(132).
       
       FD  REORDER-FILE
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 80 CHARACTERS.
       01  REORDER-RECORD             PIC X(80).
       
       WORKING-STORAGE SECTION.
       01  WS-INV-STATUS              PIC X(02) VALUE SPACES.
           88  INV-SUCCESS            VALUE '00'.
           88  INV-EOF                VALUE '10'.
           88  INV-NOT-FOUND          VALUE '23'.
       
       01  WS-TRAN-STATUS             PIC X(02) VALUE SPACES.
           88  TRAN-SUCCESS           VALUE '00'.
           88  TRAN-EOF               VALUE '10'.
       
       01  WS-REPORT-STATUS           PIC X(02) VALUE SPACES.
           88  REPORT-SUCCESS         VALUE '00'.
       
       01  WS-REORDER-STATUS          PIC X(02) VALUE SPACES.
           88  REORDER-SUCCESS        VALUE '00'.
       
       01  WS-SWITCHES.
           05  WS-END-OF-FILE-SW      PIC X(01) VALUE 'N'.
               88  END-OF-FILE        VALUE 'Y'.
       
       01  WS-COUNTERS.
           05  WS-RECEIPT-COUNT       PIC 9(05) VALUE ZEROS.
           05  WS-ISSUE-COUNT         PIC 9(05) VALUE ZEROS.
           05  WS-ADJUST-COUNT        PIC 9(05) VALUE ZEROS.
           05  WS-ERROR-COUNT         PIC 9(05) VALUE ZEROS.
           05  WS-REORDER-COUNT       PIC 9(05) VALUE ZEROS.
       
       01  WS-CURRENT-DATE.
           05  WS-CURRENT-YEAR        PIC 9(04).
           05  WS-CURRENT-MONTH       PIC 9(02).
           05  WS-CURRENT-DAY         PIC 9(02).
       
       01  WS-REPORT-HEADER.
           05  FILLER                 PIC X(20) VALUE 'INVENTORY MANAGEMENT '.
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
           05  FILLER                 PIC X(05) VALUE 'TRAN '.
           05  FILLER                 PIC X(12) VALUE 'ITEM CODE   '.
           05  FILLER                 PIC X(31) VALUE 'DESCRIPTION                    '.
           05  FILLER                 PIC X(10) VALUE 'QUANTITY  '.
           05  FILLER                 PIC X(10) VALUE 'ON HAND   '.
           05  FILLER                 PIC X(10) VALUE 'UNIT COST '.
           05  FILLER                 PIC X(15) VALUE 'REFERENCE      '.
           05  FILLER                 PIC X(39) VALUE SPACES.
       
       01  WS-DETAIL-LINE.
           05  WS-DL-TRAN-CODE        PIC X(05).
           05  WS-DL-ITEM-CODE        PIC X(12).
           05  WS-DL-DESCRIPTION      PIC X(31).
           05  WS-DL-QUANTITY         PIC Z(04)9-.
           05  WS-DL-ON-HAND          PIC Z(04)9.
           05  WS-DL-UNIT-COST        PIC $Z,ZZ9.99.
           05  WS-DL-REFERENCE        PIC X(15).
           05  FILLER                 PIC X(39) VALUE SPACES.
       
       01  WS-SUMMARY-LINE.
           05  FILLER                 PIC X(20) VALUE 'PROCESSING SUMMARY: '.
           05  FILLER                 PIC X(10) VALUE 'RECEIPTS: '.
           05  WS-SL-RECEIPT-COUNT    PIC ZZ,ZZ9.
           05  FILLER                 PIC X(10) VALUE ' ISSUES:  '.
           05  WS-SL-ISSUE-COUNT      PIC ZZ,ZZ9.
           05  FILLER                 PIC X(10) VALUE ' ADJUSTS: '.
           05  WS-SL-ADJUST-COUNT     PIC ZZ,ZZ9.
           05  FILLER                 PIC X(10) VALUE ' ERRORS:  '.
           05  WS-SL-ERROR-COUNT      PIC ZZ,ZZ9.
           05  FILLER                 PIC X(10) VALUE ' REORDERS:'.
           05  WS-SL-REORDER-COUNT    PIC ZZ,ZZ9.
           05  FILLER                 PIC X(27) VALUE SPACES.
       
       01  WS-REORDER-LINE.
           05  FILLER                 PIC X(10) VALUE 'REORDER: '.
           05  WS-RL-ITEM-CODE        PIC X(10).
           05  FILLER                 PIC X(02) VALUE SPACES.
           05  WS-RL-DESCRIPTION      PIC X(30).
           05  FILLER                 PIC X(02) VALUE SPACES.
           05  WS-RL-QUANTITY         PIC Z(04)9.
           05  FILLER                 PIC X(21) VALUE SPACES.
       
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
                I-O   INVENTORY-FILE
                OUTPUT REPORT-FILE
                OUTPUT REORDER-FILE
                
           IF NOT INV-SUCCESS
               DISPLAY 'ERROR OPENING INVENTORY FILE: ' WS-INV-STATUS
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
           
           IF NOT REORDER-SUCCESS
               DISPLAY 'ERROR OPENING REORDER FILE: ' WS-REORDER-STATUS
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
           MOVE TRAN-ITEM-CODE TO INV-ITEM-CODE
           
           READ INVENTORY-FILE
               INVALID KEY
                   MOVE 'ERROR' TO WS-DL-TRAN-CODE
                   MOVE TRAN-ITEM-CODE TO WS-DL-ITEM-CODE
                   MOVE 'ITEM NOT FOUND' TO WS-DL-DESCRIPTION
                   MOVE ZEROS TO WS-DL-QUANTITY
                   MOVE ZEROS TO WS-DL-ON-HAND
                   MOVE ZEROS TO WS-DL-UNIT-COST
                   MOVE SPACES TO WS-DL-REFERENCE
                   WRITE REPORT-RECORD FROM WS-DETAIL-LINE
                   ADD 1 TO WS-ERROR-COUNT
               NOT INVALID KEY
                   EVALUATE TRUE
                       WHEN TRAN-RECEIPT
                           PERFORM 2100-PROCESS-RECEIPT
                       WHEN TRAN-ISSUE
                           PERFORM 2200-PROCESS-ISSUE
                       WHEN TRAN-ADJUST
                           PERFORM 2300-PROCESS-ADJUSTMENT
                       WHEN OTHER
                           MOVE 'ERROR' TO WS-DL-TRAN-CODE
                           MOVE TRAN-ITEM-CODE TO WS-DL-ITEM-CODE
                           MOVE INV-DESCRIPTION TO WS-DL-DESCRIPTION
                           MOVE ZEROS TO WS-DL-QUANTITY
                           MOVE INV-ON-HAND TO WS-DL-ON-HAND
                           MOVE INV-UNIT-COST TO WS-DL-UNIT-COST
                           MOVE 'INVALID TRAN' TO WS-DL-REFERENCE
                           WRITE REPORT-RECORD FROM WS-DETAIL-LINE
                           ADD 1 TO WS-ERROR-COUNT
                   END-EVALUATE
           END-READ
           
           READ TRANSACTION-FILE
               AT END MOVE 'Y' TO WS-END-OF-FILE-SW
           END-READ
           .
           
       2100-PROCESS-RECEIPT.
           MOVE 'RCPT' TO WS-DL-TRAN-CODE
           MOVE TRAN-ITEM-CODE TO WS-DL-ITEM-CODE
           MOVE INV-DESCRIPTION TO WS-DL-DESCRIPTION
           MOVE TRAN-QUANTITY TO WS-DL-QUANTITY
           
           ADD TRAN-QUANTITY TO INV-ON-HAND
           
           MOVE INV-ON-HAND TO WS-DL-ON-HAND
           MOVE INV-UNIT-COST TO WS-DL-UNIT-COST
           MOVE TRAN-REFERENCE TO WS-DL-REFERENCE
           
           REWRITE INVENTORY-RECORD
               INVALID KEY
                   MOVE 'ERROR' TO WS-DL-TRAN-CODE
                   MOVE 'REWRITE ERROR' TO WS-DL-REFERENCE
                   ADD 1 TO WS-ERROR-COUNT
               NOT INVALID KEY
                   ADD 1 TO WS-RECEIPT-COUNT
           END-REWRITE
           
           WRITE REPORT-RECORD FROM WS-DETAIL-LINE
           .
           
       2200-PROCESS-ISSUE.
           MOVE 'ISSUE' TO WS-DL-TRAN-CODE
           MOVE TRAN-ITEM-CODE TO WS-DL-ITEM-CODE
           MOVE INV-DESCRIPTION TO WS-DL-DESCRIPTION
           MOVE TRAN-QUANTITY TO WS-DL-QUANTITY
           
           IF TRAN-QUANTITY > INV-ON-HAND
               MOVE 'ERROR' TO WS-DL-TRAN-CODE
               MOVE 'INSUFFICIENT QTY' TO WS-DL-REFERENCE
               MOVE INV-ON-HAND TO WS-DL-ON-HAND
               MOVE INV-UNIT-COST TO WS-DL-UNIT-COST
               ADD 1 TO WS-ERROR-COUNT
           ELSE
               SUBTRACT TRAN-QUANTITY FROM INV-ON-HAND
               
               MOVE INV-ON-HAND TO WS-DL-ON-HAND
               MOVE INV-UNIT-COST TO WS-DL-UNIT-COST
               MOVE TRAN-REFERENCE TO WS-DL-REFERENCE
               
               REWRITE INVENTORY-RECORD
                   INVALID KEY
                       MOVE 'ERROR' TO WS-DL-TRAN-CODE
                       MOVE 'REWRITE ERROR' TO WS-DL-REFERENCE
                       ADD 1 TO WS-ERROR-COUNT
                   NOT INVALID KEY
                       ADD 1 TO WS-ISSUE-COUNT
                       
                       IF INV-ON-HAND <= INV-REORDER-POINT
                           PERFORM 2400-GENERATE-REORDER
                       END-IF
               END-REWRITE
           END-IF
           
           WRITE REPORT-RECORD FROM WS-DETAIL-LINE
           .
           
       2300-PROCESS-ADJUSTMENT.
           MOVE 'ADJST' TO WS-DL-TRAN-CODE
           MOVE TRAN-ITEM-CODE TO WS-DL-ITEM-CODE
           MOVE INV-DESCRIPTION TO WS-DL-DESCRIPTION
           MOVE TRAN-QUANTITY TO WS-DL-QUANTITY
           
           ADD TRAN-QUANTITY TO INV-ON-HAND
           
           MOVE INV-ON-HAND TO WS-DL-ON-HAND
           MOVE INV-UNIT-COST TO WS-DL-UNIT-COST
           MOVE TRAN-REFERENCE TO WS-DL-REFERENCE
           
           REWRITE INVENTORY-RECORD
               INVALID KEY
                   MOVE 'ERROR' TO WS-DL-TRAN-CODE
                   MOVE 'REWRITE ERROR' TO WS-DL-REFERENCE
                   ADD 1 TO WS-ERROR-COUNT
               NOT INVALID KEY
                   ADD 1 TO WS-ADJUST-COUNT
                   
                   IF INV-ON-HAND <= INV-REORDER-POINT
                       PERFORM 2400-GENERATE-REORDER
                   END-IF
           END-REWRITE
           
           WRITE REPORT-RECORD FROM WS-DETAIL-LINE
           .
           
       2400-GENERATE-REORDER.
           MOVE INV-ITEM-CODE TO WS-RL-ITEM-CODE
           MOVE INV-DESCRIPTION TO WS-RL-DESCRIPTION
           
           COMPUTE WS-RL-QUANTITY = INV-REORDER-QUANTITY - INV-ON-HAND
           
           IF WS-RL-QUANTITY <= 0
               MOVE INV-REORDER-QUANTITY TO WS-RL-QUANTITY
           END-IF
           
           WRITE REORDER-RECORD FROM WS-REORDER-LINE
           
           ADD 1 TO WS-REORDER-COUNT
           .
           
       3000-TERMINATION.
           MOVE WS-RECEIPT-COUNT TO WS-SL-RECEIPT-COUNT
           MOVE WS-ISSUE-COUNT TO WS-SL-ISSUE-COUNT
           MOVE WS-ADJUST-COUNT TO WS-SL-ADJUST-COUNT
           MOVE WS-ERROR-COUNT TO WS-SL-ERROR-COUNT
           MOVE WS-REORDER-COUNT TO WS-SL-REORDER-COUNT
           
           WRITE REPORT-RECORD FROM SPACES
           WRITE REPORT-RECORD FROM WS-SUMMARY-LINE
           
           CLOSE INVENTORY-FILE
                 TRANSACTION-FILE
                 REPORT-FILE
                 REORDER-FILE
           .
