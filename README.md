0000-MAIN
 ├─ 1000-INITIALIZE
 ├─ 1010-INIT-RATES
 ├─ 2000-OPEN-FILES-PASSA
 ├─ 2100-LOAD-CUSTOMERS
 │   └─ 2110-ADD-CUSTOMER-TO-TABLE
 │       └─ 6000-WRITE-REJECT         (μόνο αν overflow)
 ├─ 3000-PASSA-VALIDATE-AND-WRITE-OK
 │   ├─ 3200-VALIDATE-TXN
 │   │   ├─ 4100-LOOKUP-CUSTOMER
 │   │   ├─ 6100-REJECT-NO-CUSTOMER   -> 6000-WRITE-REJECT   (αν δεν βρεθεί)
 │   │   ├─ 6110-REJECT-INACTIVE      -> 6000-WRITE-REJECT   (αν status != 'A')
 │   │   ├─ 6120-REJECT-BAD-TYPE      -> 6000-WRITE-REJECT   (αν type όχι CH/CR/FE)
 │   │   └─ 4200-VALIDATE-DATE
 │   │       └─ 6130-REJECT-BAD-DATE  -> 6000-WRITE-REJECT   (αν invalid date)
 │   └─ 3300-WRITE-TXN-OK             (μόνο αν WS-FOUND='Y')
 ├─ 9000-CLOSE-FILES-PASSA
 ├─ 3500-SORT-OK-TO-SORTED            (SORT verb - no PERFORM inside)
 ├─ 4000-OPEN-FILES-PASSB
 ├─ 4100-PASSB-GENERATE-STATEMENTS
 │   └─ 4200-PROCESS-SORTED-TXN
 │       ├─ 4300-START-NEW-STATEMENT          (1η φορά ή σε αλλαγή customer)
 │       │   ├─ 4100-LOOKUP-CUSTOMER
 │       │   ├─ 4310-WRITE-PAGE-HEADING
 │       │   │   └─ 4800-WRITE-STMT-LINE
 │       │   ├─ 4320-WRITE-STMT-HEADER
 │       │   │   ├─ 4905-ENSURE-SPACE
 │       │   │   │   ├─ 4310-WRITE-PAGE-HEADING
 │       │   │   │   │   └─ 4800-WRITE-STMT-LINE
 │       │   │   │   └─ 4330-WRITE-STMT-COLHEAD
 │       │   │   │       └─ 4800-WRITE-STMT-LINE
 │       │   │   └─ 4800-WRITE-STMT-LINE
 │       │   ├─ 4330-WRITE-STMT-COLHEAD
 │       │   │   └─ 4800-WRITE-STMT-LINE
 │       │   └─ 4340-ADD-MONTHLY-FEE
 │       │       ├─ 5000-GET-MONTHLY-FEE
 │       │       └─ 4800-WRITE-STMT-LINE       (μόνο αν fee > 0)
 │       ├─ 4600-WRITE-STMT-TOTALS             (μόνο σε αλλαγή customer/EOF flush)
 │       │   ├─ 4905-ENSURE-SPACE
 │       │   │   ├─ 4310-WRITE-PAGE-HEADING
 │       │   │   │   └─ 4800-WRITE-STMT-LINE
 │       │   │   └─ 4330-WRITE-STMT-COLHEAD
 │       │   │       └─ 4800-WRITE-STMT-LINE
 │       │   └─ 4800-WRITE-STMT-LINE           (γράφει 3 lines: charges/credits/net)
 │       ├─ 4700-WRITE-STMT-BLANK
 │       │   └─ 4800-WRITE-STMT-LINE
 │       ├─ 4400-ACCUMULATE
 │       └─ 4500-WRITE-DETAIL-LINE
 │           ├─ 4905-ENSURE-SPACE
 │           │   ├─ 4310-WRITE-PAGE-HEADING
 │           │   │   └─ 4800-WRITE-STMT-LINE
 │           │   └─ 4330-WRITE-STMT-COLHEAD
 │           │       └─ 4800-WRITE-STMT-LINE
 │           └─ 4800-WRITE-STMT-LINE
 ├─ 7000-WRITE-CONTROL
 └─ 4900-CLOSE-FILES-PASSB
