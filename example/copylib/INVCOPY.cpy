      *----------------------------------------------------------------*
      * INVCOPY - INVENTORY RECORD LAYOUT                             *
      *----------------------------------------------------------------*
       01  INVENTORY-DETAILS.
           05  INV-ITEM-CODE          PIC X(10).
           05  INV-DESCRIPTION        PIC X(30).
           05  INV-CATEGORY           PIC X(10).
           05  INV-SUBCATEGORY        PIC X(10).
           05  INV-SUPPLIER-ID        PIC X(06).
           05  INV-ON-HAND            PIC S9(05) COMP-3.
           05  INV-ALLOCATED          PIC S9(05) COMP-3.
           05  INV-AVAILABLE          PIC S9(05) COMP-3.
           05  INV-ON-ORDER           PIC S9(05) COMP-3.
           05  INV-UNIT-COST          PIC S9(05)V99 COMP-3.
           05  INV-UNIT-PRICE         PIC S9(05)V99 COMP-3.
           05  INV-REORDER-POINT      PIC S9(05) COMP-3.
           05  INV-REORDER-QUANTITY   PIC S9(05) COMP-3.
           05  INV-LAST-RECEIPT-DATE.
               10  INV-LR-YEAR        PIC 9(04).
               10  INV-LR-MONTH       PIC 9(02).
               10  INV-LR-DAY         PIC 9(02).
           05  INV-LAST-ISSUE-DATE.
               10  INV-LI-YEAR        PIC 9(04).
               10  INV-LI-MONTH       PIC 9(02).
               10  INV-LI-DAY         PIC 9(02).
           05  INV-LOCATION           PIC X(05).
           05  INV-BIN-NUMBER         PIC X(05).
           05  INV-STATUS             PIC X(01).
               88  INV-ACTIVE         VALUE 'A'.
               88  INV-DISCONTINUED   VALUE 'D'.
               88  INV-PENDING        VALUE 'P'.
           05  INV-LAST-COUNT-DATE.
               10  INV-LC-YEAR        PIC 9(04).
               10  INV-LC-MONTH       PIC 9(02).
               10  INV-LC-DAY         PIC 9(02).
           05  FILLER                 PIC X(20).
