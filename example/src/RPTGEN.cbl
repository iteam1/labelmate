      *----------------------------------------------------------------*
      * RPTGEN - MONTHLY SALES REPORT GENERATOR                       *
      *                                                                *
      * THIS PROGRAM GENERATES MONTHLY SALES REPORTS BY REGION,        *
      * PRODUCT CATEGORY, AND SALESPERSON.                             *
      *----------------------------------------------------------------*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RPTGEN.
       AUTHOR. LABELMATE EXAMPLE.
       DATE-WRITTEN. 2025-05-13.
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-MAINFRAME.
       OBJECT-COMPUTER. IBM-MAINFRAME.
       
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SALES-FILE ASSIGN TO SALESIN
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-SALES-STATUS.
           
           SELECT REGION-REPORT ASSIGN TO RGNRPT
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-REGION-STATUS.
           
           SELECT PRODUCT-REPORT ASSIGN TO PRODRPT
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-PRODUCT-STATUS.
           
           SELECT SALESPERSON-REPORT ASSIGN TO SALESRPT
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-SALESPERSON-STATUS.
       
       DATA DIVISION.
       FILE SECTION.
       FD  SALES-FILE
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 100 CHARACTERS.
       01  SALES-RECORD.
           05  SALES-DATE.
               10  SALES-YEAR         PIC 9(04).
               10  SALES-MONTH        PIC 9(02).
               10  SALES-DAY          PIC 9(02).
           05  SALES-INVOICE-NUMBER   PIC X(10).
           05  SALES-CUSTOMER-ID      PIC X(06).
           05  SALES-REGION           PIC X(03).
           05  SALES-PRODUCT-ID       PIC X(08).
           05  SALES-PRODUCT-CATEGORY PIC X(05).
           05  SALES-QUANTITY         PIC S9(05) COMP-3.
           05  SALES-UNIT-PRICE       PIC S9(05)V99 COMP-3.
           05  SALES-DISCOUNT-PCT     PIC S9(03)V99 COMP-3.
           05  SALES-NET-AMOUNT       PIC S9(07)V99 COMP-3.
           05  SALES-TAX-AMOUNT       PIC S9(05)V99 COMP-3.
           05  SALES-TOTAL-AMOUNT     PIC S9(07)V99 COMP-3.
           05  SALES-SALESPERSON-ID   PIC X(05).
           05  SALES-PAYMENT-TYPE     PIC X(02).
           05  FILLER                 PIC X(25).
       
       FD  REGION-REPORT
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 132 CHARACTERS.
       01  REGION-REPORT-RECORD       PIC X(132).
       
       FD  PRODUCT-REPORT
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 132 CHARACTERS.
       01  PRODUCT-REPORT-RECORD      PIC X(132).
       
       FD  SALESPERSON-REPORT
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 132 CHARACTERS.
       01  SALESPERSON-REPORT-RECORD  PIC X(132).
       
       WORKING-STORAGE SECTION.
       01  WS-SALES-STATUS            PIC X(02) VALUE SPACES.
           88  SALES-SUCCESS          VALUE '00'.
           88  SALES-EOF              VALUE '10'.
       
       01  WS-REGION-STATUS           PIC X(02) VALUE SPACES.
           88  REGION-SUCCESS         VALUE '00'.
       
       01  WS-PRODUCT-STATUS          PIC X(02) VALUE SPACES.
           88  PRODUCT-SUCCESS        VALUE '00'.
       
       01  WS-SALESPERSON-STATUS      PIC X(02) VALUE SPACES.
           88  SALESPERSON-SUCCESS    VALUE '00'.
       
       01  WS-SWITCHES.
           05  WS-END-OF-FILE-SW      PIC X(01) VALUE 'N'.
               88  END-OF-FILE        VALUE 'Y'.
       
       01  WS-CURRENT-DATE.
           05  WS-CURRENT-YEAR        PIC 9(04).
           05  WS-CURRENT-MONTH       PIC 9(02).
           05  WS-CURRENT-DAY         PIC 9(02).
       
       01  WS-REPORT-MONTH.
           05  WS-REPORT-MONTH-NUM    PIC 9(02).
           05  WS-REPORT-MONTH-NAME   PIC X(10).
       
       01  WS-REGION-TOTALS.
           05  WS-REGION-TABLE OCCURS 50 TIMES
                                INDEXED BY WS-REGION-IDX.
               10  WS-REGION-CODE     PIC X(03).
               10  WS-REGION-SALES    PIC S9(09)V99 COMP-3.
               10  WS-REGION-COUNT    PIC 9(05) COMP-3.
       
       01  WS-PRODUCT-TOTALS.
           05  WS-PRODUCT-TABLE OCCURS 100 TIMES
                                 INDEXED BY WS-PRODUCT-IDX.
               10  WS-PRODUCT-CATEGORY PIC X(05).
               10  WS-PRODUCT-SALES   PIC S9(09)V99 COMP-3.
               10  WS-PRODUCT-COUNT   PIC 9(05) COMP-3.
       
       01  WS-SALESPERSON-TOTALS.
           05  WS-SALESPERSON-TABLE OCCURS 100 TIMES
                                    INDEXED BY WS-SALESPERSON-IDX.
               10  WS-SALESPERSON-ID  PIC X(05).
               10  WS-SALESPERSON-SALES PIC S9(09)V99 COMP-3.
               10  WS-SALESPERSON-COUNT PIC 9(05) COMP-3.
       
       01  WS-REGION-COUNTERS.
           05  WS-REGION-COUNT        PIC 9(03) VALUE ZEROS.
           05  WS-REGION-FOUND-SW     PIC X(01).
               88  REGION-FOUND       VALUE 'Y'.
       
       01  WS-PRODUCT-COUNTERS.
           05  WS-PRODUCT-COUNT       PIC 9(03) VALUE ZEROS.
           05  WS-PRODUCT-FOUND-SW    PIC X(01).
               88  PRODUCT-FOUND      VALUE 'Y'.
       
       01  WS-SALESPERSON-COUNTERS.
           05  WS-SALESPERSON-COUNT   PIC 9(03) VALUE ZEROS.
           05  WS-SALESPERSON-FOUND-SW PIC X(01).
               88  SALESPERSON-FOUND  VALUE 'Y'.
       
       01  WS-TOTAL-SALES             PIC S9(09)V99 COMP-3 VALUE ZEROS.
       01  WS-TOTAL-TRANSACTIONS      PIC 9(07) COMP-3 VALUE ZEROS.
       
       01  WS-REGION-HEADER.
           05  FILLER                 PIC X(20) VALUE 'MONTHLY SALES REPORT '.
           05  FILLER                 PIC X(09) VALUE 'BY REGION'.
           05  FILLER                 PIC X(20) VALUE SPACES.
           05  FILLER                 PIC X(06) VALUE 'MONTH:'.
           05  FILLER                 PIC X(01) VALUE SPACES.
           05  WS-RH-MONTH-NAME       PIC X(10).
           05  FILLER                 PIC X(01) VALUE SPACES.
           05  WS-RH-YEAR             PIC 9(04).
           05  FILLER                 PIC X(61) VALUE SPACES.
       
       01  WS-PRODUCT-HEADER.
           05  FILLER                 PIC X(20) VALUE 'MONTHLY SALES REPORT '.
           05  FILLER                 PIC X(12) VALUE 'BY CATEGORY'.
           05  FILLER                 PIC X(17) VALUE SPACES.
           05  FILLER                 PIC X(06) VALUE 'MONTH:'.
           05  FILLER                 PIC X(01) VALUE SPACES.
           05  WS-PH-MONTH-NAME       PIC X(10).
           05  FILLER                 PIC X(01) VALUE SPACES.
           05  WS-PH-YEAR             PIC 9(04).
           05  FILLER                 PIC X(61) VALUE SPACES.
       
       01  WS-SALESPERSON-HEADER.
           05  FILLER                 PIC X(20) VALUE 'MONTHLY SALES REPORT '.
           05  FILLER                 PIC X(14) VALUE 'BY SALESPERSON'.
           05  FILLER                 PIC X(15) VALUE SPACES.
           05  FILLER                 PIC X(06) VALUE 'MONTH:'.
           05  FILLER                 PIC X(01) VALUE SPACES.
           05  WS-SH-MONTH-NAME       PIC X(10).
           05  FILLER                 PIC X(01) VALUE SPACES.
           05  WS-SH-YEAR             PIC 9(04).
           05  FILLER                 PIC X(61) VALUE SPACES.
       
       01  WS-REGION-COLUMN-HEADER.
           05  FILLER                 PIC X(10) VALUE 'REGION    '.
           05  FILLER                 PIC X(20) VALUE 'SALES AMOUNT        '.
           05  FILLER                 PIC X(20) VALUE 'TRANSACTION COUNT   '.
           05  FILLER                 PIC X(20) VALUE 'PERCENT OF TOTAL    '.
           05  FILLER                 PIC X(62) VALUE SPACES.
       
       01  WS-PRODUCT-COLUMN-HEADER.
           05  FILLER                 PIC X(10) VALUE 'CATEGORY  '.
           05  FILLER                 PIC X(20) VALUE 'SALES AMOUNT        '.
           05  FILLER                 PIC X(20) VALUE 'TRANSACTION COUNT   '.
           05  FILLER                 PIC X(20) VALUE 'PERCENT OF TOTAL    '.
           05  FILLER                 PIC X(62) VALUE SPACES.
       
       01  WS-SALESPERSON-COLUMN-HEADER.
           05  FILLER                 PIC X(10) VALUE 'SALES ID  '.
           05  FILLER                 PIC X(20) VALUE 'SALES AMOUNT        '.
           05  FILLER                 PIC X(20) VALUE 'TRANSACTION COUNT   '.
           05  FILLER                 PIC X(20) VALUE 'PERCENT OF TOTAL    '.
           05  FILLER                 PIC X(62) VALUE SPACES.
       
       01  WS-DETAIL-LINE.
           05  WS-DL-ID               PIC X(10).
           05  WS-DL-SALES-AMOUNT     PIC $$$,$$$,$$9.99.
           05  FILLER                 PIC X(03) VALUE SPACES.
           05  WS-DL-TRANS-COUNT      PIC ZZ,ZZ9.
           05  FILLER                 PIC X(09) VALUE SPACES.
           05  WS-DL-PERCENT          PIC Z9.99.
           05  FILLER                 PIC X(01) VALUE '%'.
           05  FILLER                 PIC X(75) VALUE SPACES.
       
       01  WS-TOTAL-LINE.
           05  FILLER                 PIC X(10) VALUE 'TOTAL     '.
           05  WS-TL-SALES-AMOUNT     PIC $$$,$$$,$$9.99.
           05  FILLER                 PIC X(03) VALUE SPACES.
           05  WS-TL-TRANS-COUNT      PIC ZZ,ZZ9.
           05  FILLER                 PIC X(09) VALUE SPACES.
           05  WS-TL-PERCENT          PIC Z9.99.
           05  FILLER                 PIC X(01) VALUE '%'.
           05  FILLER                 PIC X(75) VALUE SPACES.
       
       01  WS-MONTH-NAMES.
           05  FILLER                 PIC X(10) VALUE 'JANUARY   '.
           05  FILLER                 PIC X(10) VALUE 'FEBRUARY  '.
           05  FILLER                 PIC X(10) VALUE 'MARCH     '.
           05  FILLER                 PIC X(10) VALUE 'APRIL     '.
           05  FILLER                 PIC X(10) VALUE 'MAY       '.
           05  FILLER                 PIC X(10) VALUE 'JUNE      '.
           05  FILLER                 PIC X(10) VALUE 'JULY      '.
           05  FILLER                 PIC X(10) VALUE 'AUGUST    '.
           05  FILLER                 PIC X(10) VALUE 'SEPTEMBER '.
           05  FILLER                 PIC X(10) VALUE 'OCTOBER   '.
           05  FILLER                 PIC X(10) VALUE 'NOVEMBER  '.
           05  FILLER                 PIC X(10) VALUE 'DECEMBER  '.
       01  WS-MONTH-TABLE REDEFINES WS-MONTH-NAMES.
           05  WS-MONTH-NAME          PIC X(10) OCCURS 12 TIMES.
       
       PROCEDURE DIVISION.
       0000-MAIN-PROCESS.
           PERFORM 1000-INITIALIZATION
           PERFORM 2000-PROCESS-SALES-DATA
               UNTIL END-OF-FILE
           PERFORM 3000-GENERATE-REPORTS
           PERFORM 4000-TERMINATION
           GOBACK
           .
       
       1000-INITIALIZATION.
           OPEN INPUT SALES-FILE
                OUTPUT REGION-REPORT
                OUTPUT PRODUCT-REPORT
                OUTPUT SALESPERSON-REPORT
                
           IF NOT SALES-SUCCESS
               DISPLAY 'ERROR OPENING SALES FILE: ' WS-SALES-STATUS
               MOVE 'Y' TO WS-END-OF-FILE-SW
           END-IF
           
           IF NOT REGION-SUCCESS
               DISPLAY 'ERROR OPENING REGION REPORT: ' WS-REGION-STATUS
               MOVE 'Y' TO WS-END-OF-FILE-SW
           END-IF
           
           IF NOT PRODUCT-SUCCESS
               DISPLAY 'ERROR OPENING PRODUCT REPORT: ' WS-PRODUCT-STATUS
               MOVE 'Y' TO WS-END-OF-FILE-SW
           END-IF
           
           IF NOT SALESPERSON-SUCCESS
               DISPLAY 'ERROR OPENING SALESPERSON REPORT: ' 
                       WS-SALESPERSON-STATUS
               MOVE 'Y' TO WS-END-OF-FILE-SW
           END-IF
           
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE
           
           READ SALES-FILE
               AT END MOVE 'Y' TO WS-END-OF-FILE-SW
               NOT AT END
                   MOVE SALES-MONTH TO WS-REPORT-MONTH-NUM
                   IF WS-REPORT-MONTH-NUM > 0 AND WS-REPORT-MONTH-NUM < 13
                       MOVE WS-MONTH-NAME(WS-REPORT-MONTH-NUM) 
                         TO WS-REPORT-MONTH-NAME
                   ELSE
                       MOVE 'UNKNOWN' TO WS-REPORT-MONTH-NAME
                   END-IF
           END-READ
           .
           
       2000-PROCESS-SALES-DATA.
           ADD SALES-TOTAL-AMOUNT TO WS-TOTAL-SALES
           ADD 1 TO WS-TOTAL-TRANSACTIONS
           
           PERFORM 2100-PROCESS-REGION-DATA
           PERFORM 2200-PROCESS-PRODUCT-DATA
           PERFORM 2300-PROCESS-SALESPERSON-DATA
           
           READ SALES-FILE
               AT END MOVE 'Y' TO WS-END-OF-FILE-SW
           END-READ
           .
           
       2100-PROCESS-REGION-DATA.
           MOVE 'N' TO WS-REGION-FOUND-SW
           
           PERFORM VARYING WS-REGION-IDX FROM 1 BY 1
                   UNTIL WS-REGION-IDX > WS-REGION-COUNT OR
                         REGION-FOUND
               IF WS-REGION-CODE(WS-REGION-IDX) = SALES-REGION
                   MOVE 'Y' TO WS-REGION-FOUND-SW
                   ADD SALES-TOTAL-AMOUNT 
                     TO WS-REGION-SALES(WS-REGION-IDX)
                   ADD 1 TO WS-REGION-COUNT(WS-REGION-IDX)
               END-IF
           END-PERFORM
           
           IF NOT REGION-FOUND
               ADD 1 TO WS-REGION-COUNT
               MOVE SALES-REGION TO WS-REGION-CODE(WS-REGION-COUNT)
               MOVE SALES-TOTAL-AMOUNT TO WS-REGION-SALES(WS-REGION-COUNT)
               MOVE 1 TO WS-REGION-COUNT(WS-REGION-COUNT)
           END-IF
           .
           
       2200-PROCESS-PRODUCT-DATA.
           MOVE 'N' TO WS-PRODUCT-FOUND-SW
           
           PERFORM VARYING WS-PRODUCT-IDX FROM 1 BY 1
                   UNTIL WS-PRODUCT-IDX > WS-PRODUCT-COUNT OR
                         PRODUCT-FOUND
               IF WS-PRODUCT-CATEGORY(WS-PRODUCT-IDX) = 
                  SALES-PRODUCT-CATEGORY
                   MOVE 'Y' TO WS-PRODUCT-FOUND-SW
                   ADD SALES-TOTAL-AMOUNT 
                     TO WS-PRODUCT-SALES(WS-PRODUCT-IDX)
                   ADD 1 TO WS-PRODUCT-COUNT(WS-PRODUCT-IDX)
               END-IF
           END-PERFORM
           
           IF NOT PRODUCT-FOUND
               ADD 1 TO WS-PRODUCT-COUNT
               MOVE SALES-PRODUCT-CATEGORY 
                 TO WS-PRODUCT-CATEGORY(WS-PRODUCT-COUNT)
               MOVE SALES-TOTAL-AMOUNT 
                 TO WS-PRODUCT-SALES(WS-PRODUCT-COUNT)
               MOVE 1 TO WS-PRODUCT-COUNT(WS-PRODUCT-COUNT)
           END-IF
           .
           
       2300-PROCESS-SALESPERSON-DATA.
           MOVE 'N' TO WS-SALESPERSON-FOUND-SW
           
           PERFORM VARYING WS-SALESPERSON-IDX FROM 1 BY 1
                   UNTIL WS-SALESPERSON-IDX > WS-SALESPERSON-COUNT OR
                         SALESPERSON-FOUND
               IF WS-SALESPERSON-ID(WS-SALESPERSON-IDX) = 
                  SALES-SALESPERSON-ID
                   MOVE 'Y' TO WS-SALESPERSON-FOUND-SW
                   ADD SALES-TOTAL-AMOUNT 
                     TO WS-SALESPERSON-SALES(WS-SALESPERSON-IDX)
                   ADD 1 TO WS-SALESPERSON-COUNT(WS-SALESPERSON-IDX)
               END-IF
           END-PERFORM
           
           IF NOT SALESPERSON-FOUND
               ADD 1 TO WS-SALESPERSON-COUNT
               MOVE SALES-SALESPERSON-ID 
                 TO WS-SALESPERSON-ID(WS-SALESPERSON-COUNT)
               MOVE SALES-TOTAL-AMOUNT 
                 TO WS-SALESPERSON-SALES(WS-SALESPERSON-COUNT)
               MOVE 1 TO WS-SALESPERSON-COUNT(WS-SALESPERSON-COUNT)
           END-IF
           .
           
       3000-GENERATE-REPORTS.
           PERFORM 3100-GENERATE-REGION-REPORT
           PERFORM 3200-GENERATE-PRODUCT-REPORT
           PERFORM 3300-GENERATE-SALESPERSON-REPORT
           .
           
       3100-GENERATE-REGION-REPORT.
           MOVE WS-REPORT-MONTH-NAME TO WS-RH-MONTH-NAME
           MOVE SALES-YEAR TO WS-RH-YEAR
           
           WRITE REGION-REPORT-RECORD FROM WS-REGION-HEADER
           WRITE REGION-REPORT-RECORD FROM SPACES
           WRITE REGION-REPORT-RECORD FROM WS-REGION-COLUMN-HEADER
           WRITE REGION-REPORT-RECORD FROM SPACES
           
           PERFORM VARYING WS-REGION-IDX FROM 1 BY 1
                   UNTIL WS-REGION-IDX > WS-REGION-COUNT
               MOVE WS-REGION-CODE(WS-REGION-IDX) TO WS-DL-ID
               MOVE WS-REGION-SALES(WS-REGION-IDX) TO WS-DL-SALES-AMOUNT
               MOVE WS-REGION-COUNT(WS-REGION-IDX) TO WS-DL-TRANS-COUNT
               
               COMPUTE WS-DL-PERCENT = 
                   (WS-REGION-SALES(WS-REGION-IDX) / WS-TOTAL-SALES) * 100
               
               WRITE REGION-REPORT-RECORD FROM WS-DETAIL-LINE
           END-PERFORM
           
           WRITE REGION-REPORT-RECORD FROM SPACES
           
           MOVE WS-TOTAL-SALES TO WS-TL-SALES-AMOUNT
           MOVE WS-TOTAL-TRANSACTIONS TO WS-TL-TRANS-COUNT
           MOVE 100.00 TO WS-TL-PERCENT
           
           WRITE REGION-REPORT-RECORD FROM WS-TOTAL-LINE
           .
           
       3200-GENERATE-PRODUCT-REPORT.
           MOVE WS-REPORT-MONTH-NAME TO WS-PH-MONTH-NAME
           MOVE SALES-YEAR TO WS-PH-YEAR
           
           WRITE PRODUCT-REPORT-RECORD FROM WS-PRODUCT-HEADER
           WRITE PRODUCT-REPORT-RECORD FROM SPACES
           WRITE PRODUCT-REPORT-RECORD FROM WS-PRODUCT-COLUMN-HEADER
           WRITE PRODUCT-REPORT-RECORD FROM SPACES
           
           PERFORM VARYING WS-PRODUCT-IDX FROM 1 BY 1
                   UNTIL WS-PRODUCT-IDX > WS-PRODUCT-COUNT
               MOVE WS-PRODUCT-CATEGORY(WS-PRODUCT-IDX) TO WS-DL-ID
               MOVE WS-PRODUCT-SALES(WS-PRODUCT-IDX) TO WS-DL-SALES-AMOUNT
               MOVE WS-PRODUCT-COUNT(WS-PRODUCT-IDX) TO WS-DL-TRANS-COUNT
               
               COMPUTE WS-DL-PERCENT = 
                   (WS-PRODUCT-SALES(WS-PRODUCT-IDX) / WS-TOTAL-SALES) * 100
               
               WRITE PRODUCT-REPORT-RECORD FROM WS-DETAIL-LINE
           END-PERFORM
           
           WRITE PRODUCT-REPORT-RECORD FROM SPACES
           
           MOVE WS-TOTAL-SALES TO WS-TL-SALES-AMOUNT
           MOVE WS-TOTAL-TRANSACTIONS TO WS-TL-TRANS-COUNT
           MOVE 100.00 TO WS-TL-PERCENT
           
           WRITE PRODUCT-REPORT-RECORD FROM WS-TOTAL-LINE
           .
           
       3300-GENERATE-SALESPERSON-REPORT.
           MOVE WS-REPORT-MONTH-NAME TO WS-SH-MONTH-NAME
           MOVE SALES-YEAR TO WS-SH-YEAR
           
           WRITE SALESPERSON-REPORT-RECORD FROM WS-SALESPERSON-HEADER
           WRITE SALESPERSON-REPORT-RECORD FROM SPACES
           WRITE SALESPERSON-REPORT-RECORD FROM WS-SALESPERSON-COLUMN-HEADER
           WRITE SALESPERSON-REPORT-RECORD FROM SPACES
           
           PERFORM VARYING WS-SALESPERSON-IDX FROM 1 BY 1
                   UNTIL WS-SALESPERSON-IDX > WS-SALESPERSON-COUNT
               MOVE WS-SALESPERSON-ID(WS-SALESPERSON-IDX) TO WS-DL-ID
               MOVE WS-SALESPERSON-SALES(WS-SALESPERSON-IDX) 
                 TO WS-DL-SALES-AMOUNT
               MOVE WS-SALESPERSON-COUNT(WS-SALESPERSON-IDX) 
                 TO WS-DL-TRANS-COUNT
               
               COMPUTE WS-DL-PERCENT = 
                   (WS-SALESPERSON-SALES(WS-SALESPERSON-IDX) / 
                    WS-TOTAL-SALES) * 100
               
               WRITE SALESPERSON-REPORT-RECORD FROM WS-DETAIL-LINE
           END-PERFORM
           
           WRITE SALESPERSON-REPORT-RECORD FROM SPACES
           
           MOVE WS-TOTAL-SALES TO WS-TL-SALES-AMOUNT
           MOVE WS-TOTAL-TRANSACTIONS TO WS-TL-TRANS-COUNT
           MOVE 100.00 TO WS-TL-PERCENT
           
           WRITE SALESPERSON-REPORT-RECORD FROM WS-TOTAL-LINE
           .
           
       4000-TERMINATION.
           CLOSE SALES-FILE
                 REGION-REPORT
                 PRODUCT-REPORT
                 SALESPERSON-REPORT
           .
