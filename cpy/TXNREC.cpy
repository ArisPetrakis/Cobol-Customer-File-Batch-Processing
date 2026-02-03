           10  TXN-CUST-ID         PIC 9(9).
           10  TXN-DATE            PIC 9(8).      *> YYYYMMDD
           10  TXN-TYPE            PIC X(2).      *> CH=Charge, CR=Credit, FE=Fee
           10  TXN-AMOUNT          PIC 9(7)V99.   *> implied decimal
           10  TXN-DESC            PIC X(20).
           10  FILLER              PIC X(74).
