       IDENTIFICATION DIVISION.
       PROGRAM-ID. BILSTMT.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTOMER-FILE ASSIGN TO "data/CUSTOMER.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT TXN-FILE ASSIGN TO "data/TXN.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT TXN-OK-FILE ASSIGN TO "out/TXN_OK.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT TXN-SORTED-FILE ASSIGN TO "out/TXN_SORTED.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT REJECT-FILE ASSIGN TO "out/REJECTS.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT CONTROL-FILE ASSIGN TO "out/CONTROL.RPT"
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT STMT-FILE ASSIGN TO "out/STATEMENTS.RPT"
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT SORT-WORK ASSIGN TO "out/SORTWORK.tmp".

       DATA DIVISION.
       FILE SECTION.

       FD  CUSTOMER-FILE.
       01  CUSTOMER-LINE           PIC X(120).

       FD  TXN-FILE.
       01  TXN-LINE                PIC X(120).

       FD  TXN-OK-FILE.
       01  TXN-OK-LINE             PIC X(120).

       FD  TXN-SORTED-FILE.
       01  TXN-SORTED-LINE         PIC X(120).

       FD  REJECT-FILE.
       01  REJECT-LINE             PIC X(120).

       FD  CONTROL-FILE.
       01  CONTROL-LINE            PIC X(120).

       FD  STMT-FILE.
       01  STMT-LINE               PIC X(120).

       SD  SORT-WORK.
       01  SORT-REC.
           05  SRT-FIELDS.
               10  SRT-CUST-ID         PIC 9(9).
               10  SRT-DATE            PIC 9(8).
               10  SRT-TYPE            PIC X(2).
               10  SRT-AMOUNT          PIC 9(7)V99.
               10  SRT-DESC            PIC X(20).
               10  FILLER              PIC X(81).

       WORKING-STORAGE SECTION.

       77  WS-CUST-EOF             PIC X VALUE "N".
       77  WS-TXN-EOF              PIC X VALUE "N".
       77  WS-SRT-EOF              PIC X VALUE "N".

       77  WS-READ-CUST            PIC 9(9) VALUE 0.
       77  WS-READ-TXN             PIC 9(9) VALUE 0.
       77  WS-ACCEPT-TXN           PIC 9(9) VALUE 0.
       77  WS-REJECT-TXN           PIC 9(9) VALUE 0.
       77  WS-WRITE-STMT-LINES     PIC 9(9) VALUE 0.

      *> Pagination
       77  WS-PAGE-NO              PIC 9(4) VALUE 0.
       77  WS-LINE-ON-PAGE         PIC 9(3) VALUE 0.
       77  WS-NEEDED-LINES         PIC 9(3) VALUE 0.
       77  WS-LINES-PER-PAGE       PIC 9(3) VALUE 55.

       77  WS-FOUND                PIC X VALUE "N".
       77  WS-LOOKUP-ID            PIC 9(9) VALUE 0.
       77  WS-LOOKUP-IDX           PIC 9(4) VALUE 0.
       77  WS-LOOKUP-STATUS        PIC X VALUE SPACE.
       77  WS-LOOKUP-NAME          PIC X(20) VALUE SPACES.
       77  WS-LOOKUP-PLAN          PIC X(3) VALUE SPACES.

      *> Amount formatting
       77  WS-AMT-DISP             PIC -ZZ,ZZZ,ZZ9.99.
       77  WS-AMT-POS              PIC ZZZ,ZZZ,ZZ9.99.
       77  WS-AMT-NEG              PIC -ZZZ,ZZZ,ZZ9.99.
       77  WS-AMT-SIGNED           PIC S9(9)V99 VALUE 0.

      *>--- Customer in-memory table
       77  WS-CUST-COUNT           PIC 9(4) VALUE 0.
       01  WS-CUST-TABLE.
           05  WS-CUST-ENTRY OCCURS 1 TO 1000 TIMES
               DEPENDING ON WS-CUST-COUNT.
               10  T-CUST-ID        PIC 9(9).
               10  T-CUST-STATUS    PIC X.
               10  T-CUST-PLAN      PIC X(3).
               10  T-CUST-NAME      PIC X(20).

      *>--- Copybook layouts for parsing raw files
       01  WS-CUST-PARSED.
           05  CUST-REC.
               10  CUST-ID             PIC 9(9).
               10  CUST-NAME           PIC X(20).
               10  CUST-STATUS         PIC X.
               10  CUST-PLAN           PIC X(3).
               10  CUST-CYCLE          PIC 9(2).
               10  FILLER              PIC X(85).

       01  WS-TXN-PARSED.
           05  TXN-REC.
               10  TXN-CUST-ID         PIC 9(9).
               10  TXN-DATE            PIC 9(8).
               10  TXN-TYPE            PIC X(2).
               10  TXN-AMOUNT          PIC 9(7)V99.
               10  TXN-DESC            PIC X(20).
               10  FILLER              PIC X(74).

       01  WS-REJ-PARSED.
           05  REJ-REC.
               10  REJ-REASON          PIC X(4).
               10  REJ-SOURCE          PIC X(4).
               10  REJ-CUST-ID         PIC 9(9).
               10  REJ-DETAIL          PIC X(80).
               10  FILLER              PIC X(23).

       01  WS-SRT-OUT.
           05  SRT-REC.
               10  SRT-CUST-ID         PIC 9(9).
               10  SRT-DATE            PIC 9(8).
               10  SRT-TYPE            PIC X(2).
               10  SRT-AMOUNT          PIC 9(7)V99.
               10  SRT-DESC            PIC X(20).
               10  FILLER              PIC X(81).

      *>--- Date parts
       01  WS-DATE-PARTS.
           05  WS-YYYY             PIC 9(4).
           05  WS-MM               PIC 9(2).
           05  WS-DD               PIC 9(2).

      *>--- Statement control-break
       77  WS-HAVE-CUST            PIC X VALUE "N".
       77  WS-CURR-CUST-ID         PIC 9(9) VALUE 0.

       77  WS-TOT-CHARGES          PIC 9(9)V99 VALUE 0.
       77  WS-TOT-CREDITS          PIC 9(9)V99 VALUE 0.
       77  WS-TOT-NET              PIC S9(9)V99 VALUE 0.

       77  WS-LINE                 PIC X(120).

      *>--- Rate table (hardcoded for now)
       01  WS-RATES.
           05  RATE-TABLE.
               10  RATE-ENTRY OCCURS 5 TIMES.
                   15  RATE-PLAN      PIC X(3).
                   15  RATE-MONTHLY   PIC 9(5)V99.

       77  WS-MONTHLY-FEE          PIC 9(5)V99 VALUE 0.

       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-INITIALIZE
           PERFORM 1010-INIT-RATES
           PERFORM 2000-OPEN-FILES-PASSA
           PERFORM 2100-LOAD-CUSTOMERS
           PERFORM 3000-PASSA-VALIDATE-AND-WRITE-OK
           PERFORM 9000-CLOSE-FILES-PASSA

           PERFORM 3500-SORT-OK-TO-SORTED

           PERFORM 4000-OPEN-FILES-PASSB
           PERFORM 4100-PASSB-GENERATE-STATEMENTS
           PERFORM 7000-WRITE-CONTROL
           PERFORM 4900-CLOSE-FILES-PASSB
           GOBACK.

       1000-INITIALIZE.
           MOVE 0 TO WS-READ-CUST WS-READ-TXN WS-ACCEPT-TXN 
           MOVE 0 TO WS-REJECT-TXN
           MOVE 0 TO WS-WRITE-STMT-LINES
           MOVE 0 TO WS-CUST-COUNT
           MOVE "N" TO WS-CUST-EOF WS-TXN-EOF WS-SRT-EOF
           MOVE "N" TO WS-HAVE-CUST
           MOVE 0 TO WS-CURR-CUST-ID
           MOVE 0 TO WS-TOT-CHARGES WS-TOT-CREDITS WS-TOT-NET
           MOVE 0 TO WS-PAGE-NO WS-LINE-ON-PAGE.

       1010-INIT-RATES.
           MOVE "PLN" TO RATE-PLAN (1)
           MOVE 000999 TO RATE-MONTHLY (1)  *> 9.99

           MOVE "PLN" TO RATE-PLAN (2)
           MOVE 001499 TO RATE-MONTHLY (2)  *> 14.99

           MOVE "BAS" TO RATE-PLAN (3)
           MOVE 000599 TO RATE-MONTHLY (3)  *> 5.99

           MOVE "PRM" TO RATE-PLAN (4)
           MOVE 001999 TO RATE-MONTHLY (4)  *> 19.99

           MOVE "ENT" TO RATE-PLAN (5)
           MOVE 004999 TO RATE-MONTHLY (5). *> 49.99

       2000-OPEN-FILES-PASSA.
           OPEN INPUT  CUSTOMER-FILE
                INPUT  TXN-FILE
                OUTPUT TXN-OK-FILE
                OUTPUT REJECT-FILE.

       9000-CLOSE-FILES-PASSA.
           CLOSE CUSTOMER-FILE TXN-FILE TXN-OK-FILE REJECT-FILE.

       2100-LOAD-CUSTOMERS.
           PERFORM UNTIL WS-CUST-EOF = "Y"
               READ CUSTOMER-FILE
                   AT END
                       MOVE "Y" TO WS-CUST-EOF
                   NOT AT END
                       ADD 1 TO WS-READ-CUST
                       MOVE CUSTOMER-LINE TO CUST-REC OF WS-CUST-PARSED
                       PERFORM 2110-ADD-CUSTOMER-TO-TABLE
               END-READ
           END-PERFORM.

       2110-ADD-CUSTOMER-TO-TABLE.
           IF WS-CUST-COUNT >= 1000
               MOVE "R99" TO REJ-REASON OF WS-REJ-PARSED
               MOVE "CUST" TO REJ-SOURCE OF WS-REJ-PARSED
               MOVE CUST-ID OF WS-CUST-PARSED TO REJ-CUST-ID OF 
                    WS-REJ-PARSED
               MOVE "Customer table overflow (increase OCCURS limit)" 
                    TO REJ-DETAIL OF WS-REJ-PARSED
               PERFORM 6000-WRITE-REJECT
           ELSE
               ADD 1 TO WS-CUST-COUNT
               MOVE CUST-ID OF WS-CUST-PARSED 
                    TO T-CUST-ID (WS-CUST-COUNT)
               MOVE CUST-STATUS OF WS-CUST-PARSED 
                    TO T-CUST-STATUS (WS-CUST-COUNT)
               MOVE CUST-PLAN OF WS-CUST-PARSED 
                    TO T-CUST-PLAN (WS-CUST-COUNT)
               MOVE CUST-NAME OF WS-CUST-PARSED 
                    TO T-CUST-NAME (WS-CUST-COUNT)
           END-IF.

       3000-PASSA-VALIDATE-AND-WRITE-OK.
           PERFORM UNTIL WS-TXN-EOF = "Y"
               READ TXN-FILE
                   AT END
                       MOVE "Y" TO WS-TXN-EOF
                   NOT AT END
                       ADD 1 TO WS-READ-TXN
                       MOVE TXN-LINE TO TXN-REC OF WS-TXN-PARSED
                       PERFORM 3200-VALIDATE-TXN
                       IF WS-FOUND = "Y"
                           PERFORM 3300-WRITE-TXN-OK
                           ADD 1 TO WS-ACCEPT-TXN
                       END-IF
               END-READ
           END-PERFORM.

       3200-VALIDATE-TXN.
           MOVE "N" TO WS-FOUND
           MOVE SPACE TO WS-LOOKUP-STATUS

           MOVE TXN-CUST-ID OF WS-TXN-PARSED TO WS-LOOKUP-ID
           PERFORM 4100-LOOKUP-CUSTOMER

           IF WS-FOUND NOT = "Y"
               PERFORM 6100-REJECT-NO-CUSTOMER
               EXIT PARAGRAPH
           END-IF

           IF WS-LOOKUP-STATUS NOT = "A"
               PERFORM 6110-REJECT-INACTIVE
               EXIT PARAGRAPH
           END-IF

           IF NOT (TXN-TYPE OF WS-TXN-PARSED = "CH"
                OR TXN-TYPE OF WS-TXN-PARSED = "CR"
                OR TXN-TYPE OF WS-TXN-PARSED = "FE")
               PERFORM 6120-REJECT-BAD-TYPE
               EXIT PARAGRAPH
           END-IF

           PERFORM 4200-VALIDATE-DATE
           IF WS-FOUND NOT = "Y"
               PERFORM 6130-REJECT-BAD-DATE
               EXIT PARAGRAPH
           END-IF

           MOVE "Y" TO WS-FOUND.

       3300-WRITE-TXN-OK.
           MOVE TXN-CUST-ID OF WS-TXN-PARSED TO SRT-CUST-ID 
                OF WS-SRT-OUT
           MOVE TXN-DATE    OF WS-TXN-PARSED TO SRT-DATE    
                OF WS-SRT-OUT
           MOVE TXN-TYPE    OF WS-TXN-PARSED TO SRT-TYPE    
                OF WS-SRT-OUT
           MOVE TXN-AMOUNT  OF WS-TXN-PARSED TO SRT-AMOUNT  
                OF WS-SRT-OUT
           MOVE TXN-DESC    OF WS-TXN-PARSED TO SRT-DESC    
                OF WS-SRT-OUT
           MOVE SRT-REC OF WS-SRT-OUT TO TXN-OK-LINE
           WRITE TXN-OK-LINE.

       3500-SORT-OK-TO-SORTED.
           SORT SORT-WORK
               ON ASCENDING KEY SRT-CUST-ID OF SRT-FIELDS OF SORT-REC
                                SRT-DATE OF SRT-FIELDS OF SORT-REC
               USING  TXN-OK-FILE
               GIVING TXN-SORTED-FILE.

       4100-LOOKUP-CUSTOMER.
           MOVE "N" TO WS-FOUND
           MOVE SPACES TO WS-LOOKUP-NAME WS-LOOKUP-PLAN
           PERFORM VARYING WS-LOOKUP-IDX FROM 1 BY 1 UNTIL 
                           WS-LOOKUP-IDX > WS-CUST-COUNT
               IF T-CUST-ID (WS-LOOKUP-IDX) = WS-LOOKUP-ID
                   MOVE "Y" TO WS-FOUND
                   MOVE T-CUST-STATUS (WS-LOOKUP-IDX) 
                        TO WS-LOOKUP-STATUS
                   MOVE T-CUST-NAME   (WS-LOOKUP-IDX) TO WS-LOOKUP-NAME
                   MOVE T-CUST-PLAN   (WS-LOOKUP-IDX) TO WS-LOOKUP-PLAN
                   EXIT PERFORM
               END-IF
           END-PERFORM.

       4200-VALIDATE-DATE.
           MOVE "N" TO WS-FOUND
           MOVE TXN-DATE OF WS-TXN-PARSED TO WS-DATE-PARTS
           IF WS-MM >= 01 AND WS-MM <= 12
              AND WS-DD >= 01 AND WS-DD <= 31
              AND WS-YYYY >= 1900 AND WS-YYYY <= 2099
               MOVE "Y" TO WS-FOUND
           END-IF.

       6000-WRITE-REJECT.
           MOVE REJ-REC OF WS-REJ-PARSED TO REJECT-LINE
           WRITE REJECT-LINE
           ADD 1 TO WS-REJECT-TXN.

       6100-REJECT-NO-CUSTOMER.
           MOVE "R01" TO REJ-REASON OF WS-REJ-PARSED
           MOVE "TXN" TO REJ-SOURCE OF WS-REJ-PARSED
           MOVE TXN-CUST-ID OF WS-TXN-PARSED TO REJ-CUST-ID 
                OF WS-REJ-PARSED
           MOVE "Customer not found for transaction" TO REJ-DETAIL 
                OF WS-REJ-PARSED
           PERFORM 6000-WRITE-REJECT.

       6110-REJECT-INACTIVE.
           MOVE "R02" TO REJ-REASON OF WS-REJ-PARSED
           MOVE "TXN" TO REJ-SOURCE OF WS-REJ-PARSED
           MOVE TXN-CUST-ID OF WS-TXN-PARSED TO REJ-CUST-ID 
                OF WS-REJ-PARSED
           MOVE "Customer status not ACTIVE" TO REJ-DETAIL 
                OF WS-REJ-PARSED
           PERFORM 6000-WRITE-REJECT.

       6120-REJECT-BAD-TYPE.
           MOVE "R03" TO REJ-REASON OF WS-REJ-PARSED
           MOVE "TXN" TO REJ-SOURCE OF WS-REJ-PARSED
           MOVE TXN-CUST-ID OF WS-TXN-PARSED TO REJ-CUST-ID 
                OF WS-REJ-PARSED
           MOVE "Invalid transaction type (expected CH/CR/FE)" 
                TO REJ-DETAIL OF WS-REJ-PARSED
           PERFORM 6000-WRITE-REJECT.

       6130-REJECT-BAD-DATE.
           MOVE "R04" TO REJ-REASON OF WS-REJ-PARSED
           MOVE "TXN" TO REJ-SOURCE OF WS-REJ-PARSED
           MOVE TXN-CUST-ID OF WS-TXN-PARSED TO REJ-CUST-ID 
                OF WS-REJ-PARSED
           MOVE "Invalid date (YYYYMMDD basic check failed)" 
                TO REJ-DETAIL OF WS-REJ-PARSED
           PERFORM 6000-WRITE-REJECT.

       4000-OPEN-FILES-PASSB.
           OPEN INPUT  TXN-SORTED-FILE
                OUTPUT STMT-FILE
                OUTPUT CONTROL-FILE.

       4900-CLOSE-FILES-PASSB.
           CLOSE TXN-SORTED-FILE STMT-FILE CONTROL-FILE.

       4100-PASSB-GENERATE-STATEMENTS.
           MOVE "N" TO WS-SRT-EOF
           PERFORM UNTIL WS-SRT-EOF = "Y"
               READ TXN-SORTED-FILE
                   AT END
                       MOVE "Y" TO WS-SRT-EOF
                   NOT AT END
                       MOVE TXN-SORTED-LINE TO SRT-REC OF WS-SRT-OUT
                       PERFORM 4200-PROCESS-SORTED-TXN
               END-READ
           END-PERFORM

           IF WS-HAVE-CUST = "Y"
               PERFORM 4600-WRITE-STMT-TOTALS
               PERFORM 4700-WRITE-STMT-BLANK
           END-IF.

       4200-PROCESS-SORTED-TXN.
           IF WS-HAVE-CUST = "N"
               MOVE SRT-CUST-ID OF WS-SRT-OUT TO WS-CURR-CUST-ID
               MOVE "Y" TO WS-HAVE-CUST
               PERFORM 4300-START-NEW-STATEMENT
           ELSE
               IF SRT-CUST-ID OF WS-SRT-OUT NOT = WS-CURR-CUST-ID
                   PERFORM 4600-WRITE-STMT-TOTALS
                   PERFORM 4700-WRITE-STMT-BLANK
                   MOVE SRT-CUST-ID OF WS-SRT-OUT TO WS-CURR-CUST-ID
                   PERFORM 4300-START-NEW-STATEMENT
               END-IF
           END-IF

           PERFORM 4400-ACCUMULATE
           PERFORM 4500-WRITE-DETAIL-LINE.

       4300-START-NEW-STATEMENT.
           MOVE 0 TO WS-TOT-CHARGES WS-TOT-CREDITS WS-TOT-NET

           MOVE WS-CURR-CUST-ID TO WS-LOOKUP-ID
           PERFORM 4100-LOOKUP-CUSTOMER

      *>   Reset pagination for each customer (enterprise option)
           MOVE 0 TO WS-PAGE-NO WS-LINE-ON-PAGE

           PERFORM 4310-WRITE-PAGE-HEADING
           PERFORM 4320-WRITE-STMT-HEADER
           PERFORM 4330-WRITE-STMT-COLHEAD
           PERFORM 4340-ADD-MONTHLY-FEE.

       4310-WRITE-PAGE-HEADING.
           ADD 1 TO WS-PAGE-NO
           MOVE 0 TO WS-LINE-ON-PAGE

           MOVE ALL SPACES TO WS-LINE
           STRING "BILLING STATEMENT RUN   PAGE " DELIMITED BY SIZE
                  WS-PAGE-NO                  DELIMITED BY SIZE
             INTO WS-LINE
           END-STRING
           PERFORM 4800-WRITE-STMT-LINE.

       4320-WRITE-STMT-HEADER.
           MOVE 5 TO WS-NEEDED-LINES
           PERFORM 4905-ENSURE-SPACE
           MOVE ALL SPACES TO WS-LINE
           STRING "CUSTOMER: "        DELIMITED BY SIZE
                  WS-CURR-CUST-ID     DELIMITED BY SIZE
                  "  NAME: "          DELIMITED BY SIZE
                  WS-LOOKUP-NAME      DELIMITED BY SIZE
                  "  PLAN: "          DELIMITED BY SIZE
                  WS-LOOKUP-PLAN      DELIMITED BY SIZE
             INTO WS-LINE
           END-STRING
           PERFORM 4800-WRITE-STMT-LINE.

       4330-WRITE-STMT-COLHEAD.
           MOVE 3 TO WS-NEEDED-LINES
           PERFORM 4905-ENSURE-SPACE
           MOVE ALL SPACES TO WS-LINE
           STRING "DATE       TYPE   AMOUNT           DESCRIPTION" 
                  DELIMITED BY SIZE
             INTO WS-LINE
           END-STRING
           PERFORM 4800-WRITE-STMT-LINE

           MOVE ALL SPACES TO WS-LINE
           STRING 
           "----------  ----  ---------------  --------------------" 
           DELIMITED BY SIZE
             INTO WS-LINE
           END-STRING
           PERFORM 4800-WRITE-STMT-LINE.

       4340-ADD-MONTHLY-FEE.
           PERFORM 5000-GET-MONTHLY-FEE
           IF WS-MONTHLY-FEE > 0
      *>       treat fee as CHARGE
               ADD WS-MONTHLY-FEE TO WS-TOT-CHARGES
               COMPUTE WS-TOT-NET = WS-TOT-CHARGES - WS-TOT-CREDITS

               MOVE ALL SPACES TO WS-LINE
               MOVE WS-MONTHLY-FEE TO WS-AMT-POS
               STRING "00000000" DELIMITED BY SIZE
                      "  FE    " DELIMITED BY SIZE
                      WS-AMT-POS DELIMITED BY SIZE
                      "  MONTHLY FEE" DELIMITED BY SIZE
                 INTO WS-LINE
               END-STRING
               PERFORM 4800-WRITE-STMT-LINE
           END-IF.

       4400-ACCUMULATE.
           IF SRT-TYPE OF WS-SRT-OUT = "CH"
               ADD SRT-AMOUNT OF WS-SRT-OUT TO WS-TOT-CHARGES
           ELSE
               IF SRT-TYPE OF WS-SRT-OUT = "FE"
                   ADD SRT-AMOUNT OF WS-SRT-OUT TO WS-TOT-CHARGES
               ELSE
                   ADD SRT-AMOUNT OF WS-SRT-OUT TO WS-TOT-CREDITS
               END-IF
           END-IF
           COMPUTE WS-TOT-NET = WS-TOT-CHARGES - WS-TOT-CREDITS.

       4500-WRITE-DETAIL-LINE.
           MOVE 2 TO WS-NEEDED-LINES
           PERFORM 4905-ENSURE-SPACE

           MOVE ALL SPACES TO WS-LINE

           IF SRT-TYPE OF WS-SRT-OUT = "CR"
               COMPUTE WS-AMT-SIGNED = 0 - SRT-AMOUNT OF WS-SRT-OUT
               MOVE WS-AMT-SIGNED TO WS-AMT-DISP
               STRING SRT-DATE  OF WS-SRT-OUT DELIMITED BY SIZE
                      "  "                    DELIMITED BY SIZE
                      SRT-TYPE  OF WS-SRT-OUT DELIMITED BY SIZE
                      "   "                   DELIMITED BY SIZE
                      WS-AMT-DISP             DELIMITED BY SIZE
                      "  "                    DELIMITED BY SIZE
                      SRT-DESC  OF WS-SRT-OUT DELIMITED BY SIZE
                 INTO WS-LINE
               END-STRING
           ELSE
               MOVE SRT-AMOUNT OF WS-SRT-OUT TO WS-AMT-POS
               STRING SRT-DATE  OF WS-SRT-OUT DELIMITED BY SIZE
                      "  "                    DELIMITED BY SIZE
                      SRT-TYPE  OF WS-SRT-OUT DELIMITED BY SIZE
                      "   "                   DELIMITED BY SIZE
                      WS-AMT-POS              DELIMITED BY SIZE
                      "  "                    DELIMITED BY SIZE
                      SRT-DESC  OF WS-SRT-OUT DELIMITED BY SIZE
                 INTO WS-LINE
               END-STRING
           END-IF

           PERFORM 4800-WRITE-STMT-LINE.

       4600-WRITE-STMT-TOTALS.
           MOVE 6 TO WS-NEEDED-LINES
           PERFORM 4905-ENSURE-SPACE

           MOVE ALL SPACES TO WS-LINE
           MOVE WS-TOT-CHARGES TO WS-AMT-POS
           STRING "TOTAL CHARGES: " DELIMITED BY SIZE
                  WS-AMT-POS        DELIMITED BY SIZE
             INTO WS-LINE
           END-STRING
           PERFORM 4800-WRITE-STMT-LINE

           MOVE ALL SPACES TO WS-LINE
           MOVE WS-TOT-CREDITS TO WS-AMT-POS
           STRING "TOTAL CREDITS: " DELIMITED BY SIZE
                  WS-AMT-POS        DELIMITED BY SIZE
             INTO WS-LINE
           END-STRING
           PERFORM 4800-WRITE-STMT-LINE

           MOVE ALL SPACES TO WS-LINE
           MOVE WS-TOT-NET TO WS-AMT-DISP
           STRING "NET DUE      : " DELIMITED BY SIZE
                  WS-AMT-DISP       DELIMITED BY SIZE
             INTO WS-LINE
           END-STRING
           PERFORM 4800-WRITE-STMT-LINE.

       4700-WRITE-STMT-BLANK.
           MOVE ALL SPACES TO WS-LINE
           PERFORM 4800-WRITE-STMT-LINE.

       4800-WRITE-STMT-LINE.
           MOVE WS-LINE TO STMT-LINE
           WRITE STMT-LINE
           ADD 1 TO WS-WRITE-STMT-LINES
           ADD 1 TO WS-LINE-ON-PAGE.

       4905-ENSURE-SPACE.
           IF (WS-LINE-ON-PAGE + WS-NEEDED-LINES) > WS-LINES-PER-PAGE
               PERFORM 4310-WRITE-PAGE-HEADING
               PERFORM 4330-WRITE-STMT-COLHEAD
           END-IF.

       5000-GET-MONTHLY-FEE.
           MOVE 0 TO WS-MONTHLY-FEE
           IF WS-LOOKUP-PLAN = "PLN"
               MOVE RATE-MONTHLY (1) TO WS-MONTHLY-FEE
           ELSE
               IF WS-LOOKUP-PLAN = "BAS"
                   MOVE RATE-MONTHLY (3) TO WS-MONTHLY-FEE
               ELSE
                   IF WS-LOOKUP-PLAN = "PRM"
                       MOVE RATE-MONTHLY (4) TO WS-MONTHLY-FEE
                   ELSE
                       IF WS-LOOKUP-PLAN = "ENT"
                           MOVE RATE-MONTHLY (5) TO WS-MONTHLY-FEE
                       END-IF
                   END-IF
               END-IF
           END-IF.

       7000-WRITE-CONTROL.
           MOVE ALL SPACES TO CONTROL-LINE
           STRING "BILSTMT CONTROL TOTALS" DELIMITED BY SIZE
             INTO CONTROL-LINE
           END-STRING
           WRITE CONTROL-LINE

           MOVE ALL SPACES TO CONTROL-LINE
           STRING "CUSTOMERS READ: " DELIMITED BY SIZE
                  WS-READ-CUST     DELIMITED BY SIZE
             INTO CONTROL-LINE
           END-STRING
           WRITE CONTROL-LINE

           MOVE ALL SPACES TO CONTROL-LINE
           STRING "TXNS READ     : " DELIMITED BY SIZE
                  WS-READ-TXN      DELIMITED BY SIZE
             INTO CONTROL-LINE
           END-STRING
           WRITE CONTROL-LINE

           MOVE ALL SPACES TO CONTROL-LINE
           STRING "TXNS ACCEPTED : " DELIMITED BY SIZE
                  WS-ACCEPT-TXN    DELIMITED BY SIZE
             INTO CONTROL-LINE
           END-STRING
           WRITE CONTROL-LINE

           MOVE ALL SPACES TO CONTROL-LINE
           STRING "TXNS REJECTED : " DELIMITED BY SIZE
                  WS-REJECT-TXN    DELIMITED BY SIZE
             INTO CONTROL-LINE
           END-STRING
           WRITE CONTROL-LINE

           MOVE ALL SPACES TO CONTROL-LINE
           STRING "STMT LINES    : " DELIMITED BY SIZE
                  WS-WRITE-STMT-LINES DELIMITED BY SIZE
             INTO CONTROL-LINE
           END-STRING
           WRITE CONTROL-LINE.
