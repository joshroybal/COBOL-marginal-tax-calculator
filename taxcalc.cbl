      ******************************************************************
      * MARGINAL.CBL
      * program to calculate marginal taxe rates
      * updated to reflect 2019 tax schedules
      * last change
      * Copyright 2015-2018 Josh Roybal
      * developer@joshroybal.com
      * https://joshroybal.com
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MTRC-004.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STDIN 
              ASSIGN TO KEYBOARD.
           SELECT SCHEDULES-FILE
              ASSIGN TO WS-SCHEDULES-FILENAME
              ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD STDIN.
       01 POST-DATA-BUFFER PIC X(256).

       FD SCHEDULES-FILE.
       01 SCHEDULES-FILE-RECORD     PIC X(80).
       01 SCHEDULES-FILE-EOF        PIC X(1).
           
       WORKING-STORAGE SECTION.
       01 WS-RATES.
           05 WS-RATE               PIC V999 OCCURS 7 TIMES.
       01 WS-FLOORS.
           05 WS-FLOOR              PIC 9(12)V99 OCCURS 7 TIMES.
       01 WS-BRACKETS.
           05 WS-BRACKET            PIC 9(13)V99 OCCURS 7 TIMES.    
       01 WS-TAXES.
           05 WS-MARGINAL-TAX       PIC 9(13)V99 OCCURS 7 TIMES.
       77 WS-SCHEDULES-FILENAME     PIC X(50) VALUE "schedules/2018".
       77 WS-TMP-BUFFER             PIC X(80) VALUE SPACES.
       77 WS-BLANK-LINE             PIC X(80) VALUE SPACES.
       77 WS-STATUS                 PIC X(24) VALUE "single".
       77 WS-TAX                    PIC 9(13)V99.
       77 WS-NET-INCOME             PIC 9(13)V99.
       77 WS-COUNTER                PIC 99.
       77 WS-STR-LEN                PIC 99.
       77 WS-AMOUNT                 PIC 9(13)V99 VALUE 0.
       77 WS-AMT-STR                PIC Z(12)9.99.
      * 77 WS-TAX-YEAR               PIC X(25) VALUE "schedules/2018".
       77 WS-AGI                    PIC S9(13)V99 VALUE 0.
       77 WS-TMP                    PIC S9(13)V999.
       77 WS-STD-DEDUCT             PIC 99999.
       77 WS-CURRENCY               PIC $$$$,$$$,$$9.99.
       77 WS-RANGE-VAL              PIC $,$$$,$$$,$$9.99.
       77 WS-PERCENT                PIC Z9.     
       77 WS-EFFECTIVE-RATE         PIC Z9.9.
      ******************************************************************
      * MAIN PROGRAM
      ******************************************************************
       PROCEDURE DIVISION.
           DISPLAY "Content-Type: text/html"
           DISPLAY X"0D"           
      ******************************************************************
      * AS USUAL MY DATA ACQUISITION IS THE OPPOSITE OF ROBUSTNESS
      ******************************************************************
           SET WS-COUNTER TO 0
           OPEN INPUT STDIN           
           PERFORM FOREVER
              READ STDIN
              AT END
                 EXIT PERFORM
              END-READ
              MOVE SPACES TO WS-TMP-BUFFER
              MOVE POST-DATA-BUFFER TO WS-TMP-BUFFER
              IF (WS-TMP-BUFFER = SPACES)
                 SET WS-COUNTER UP BY 1
                 READ STDIN
                    AT END
                       EXIT PERFORM
                 END-READ
                 MOVE SPACES TO WS-TMP-BUFFER
                 MOVE POST-DATA-BUFFER TO WS-TMP-BUFFER
                    
                 IF WS-COUNTER = 1
                    MOVE WS-TMP-BUFFER TO WS-AMOUNT
                 END-IF
                 IF WS-COUNTER = 2
                    MOVE WS-TMP-BUFFER TO WS-STATUS
                 END-IF
                 IF WS-COUNTER = 3
                    MOVE WS-TMP-BUFFER TO WS-SCHEDULES-FILENAME
                 END-IF
              END-IF
           END-PERFORM           
           CLOSE STDIN
      ******************************************************************
      * assign schedules file as per user choice
      ******************************************************************
      *     MOVE WS-TAX-YEAR TO WS-SCHEDULES-FILENAME
           MOVE 'N' TO SCHEDULES-FILE-EOF
           OPEN INPUT SCHEDULES-FILE
           PERFORM UNTIL SCHEDULES-FILE-EOF EQUAL TO 'Y'
              READ SCHEDULES-FILE INTO SCHEDULES-FILE-RECORD 
                 AT END MOVE 'Y' TO SCHEDULES-FILE-EOF
                 NOT AT END
      *
      * if WS-TMP-BUFFER = WS-STATUS read next 7 records
      *
                    IF SCHEDULES-FILE-RECORD EQUAL TO WS-STATUS
                       SET WS-COUNTER TO 1
                       PERFORM UNTIL WS-COUNTER > 7
                          READ SCHEDULES-FILE INTO SCHEDULES-FILE-RECORD
                          MOVE SPACES TO WS-TMP-BUFFER
                          MOVE SCHEDULES-FILE-RECORD TO WS-TMP-BUFFER
                          IF WS-COUNTER EQUAL TO 1
                             MOVE SCHEDULES-FILE-RECORD TO WS-TMP-BUFFER
                             MOVE WS-TMP-BUFFER TO WS-STD-DEDUCT
                             MOVE 0 TO WS-FLOOR(WS-COUNTER)
                          ELSE
                             MOVE SCHEDULES-FILE-RECORD TO WS-TMP-BUFFER
                             MOVE WS-TMP-BUFFER TO WS-FLOOR(WS-COUNTER)
                          END-IF
                          SET WS-COUNTER UP BY 1
                       END-PERFORM
                       EXIT PERFORM
                    END-IF
              END-READ
           END-PERFORM
           CLOSE SCHEDULES-FILE
      ******************************************************************
      * PERFORM PROCESSING SECTIONS
      ******************************************************************
           IF WS-AMOUNT NOT EQUAL TO ZERO
            PERFORM LOAD-MARGINAL-RATES
            PERFORM PROCESS-MARGINAL-RATES
           END-IF
      ******************************************************************
      * PERFORM OUTPUT SECTIONS
      ******************************************************************
           DISPLAY "<!DOCTYPE html>"
           DISPLAY "<html>"
           DISPLAY "<head>"
           DISPLAY "<link rel='stylesheet' media='all' 
           href='/includes/gradienttable.css'>"
           DISPLAY "<title>COBOL MARGINAL TAX CALCULATOR</title>"
           DISPLAY "</head>"
           DISPLAY "<body>"
           DISPLAY "<header><p>" NO ADVANCING
           DISPLAY "COBOL MARGINAL TAX CALCULATOR" NO ADVANCING 
           DISPLAY "</p></header>"
           DISPLAY "<div><a href='/index.html'>Home</a> | <a href='taxca
      -"lc.cgi'>Back</a></div>"           
           DISPLAY "<h1>COBOL MARGINAL TAX CALCULATOR</h1>"
           PERFORM PRINT-HTML-FORM
           IF WS-AMOUNT NOT EQUAL TO ZERO
            PERFORM OUTPUT-MARGINAL-TAXES-HTML
           END-IF
           DISPLAY "<div><a href='/index.html'>Home</a> | <a href='taxca
      -"lc.cgi'>Back</a></div>"           
           DISPLAY "<footer><p>" NO ADVANCING
           DISPLAY "Copyright &copy; 2019 Josh Roybal" NO ADVANCING
           DISPLAY "</p></footer>"
           DISPLAY "</body>"
           DISPLAY "</html>"
           STOP RUN.
      ******************************************************************
      * this section shall emit the primary html form of the application
      ******************************************************************
       PRINT-HTML-FORM SECTION.
      * strip leading spaces from WS-AMT-STR
           MOVE WS-AMOUNT TO WS-AMT-STR
           MOVE WS-AMOUNT TO WS-AMT-STR
           SET WS-COUNTER TO ZERO
           INSPECT WS-AMT-STR TALLYING WS-COUNTER FOR LEADING SPACE
           SET WS-COUNTER UP BY 1
           SUBTRACT WS-COUNTER FROM LENGTH WS-AMT-STR GIVING WS-STR-LEN
           SET WS-STR-LEN UP BY 1

           DISPLAY "<form action = 'taxcalc.cgi' method = 'POST'   
           accept-charset='US-ASCII' enctype='multipart/form-data'>"

           DISPLAY "<div>ANNUAL INCOME : $<input type = 'text' 
           name = 'amount' size='25' value='", 
           WS-AMT-STR(WS-COUNTER : WS-STR-LEN), "'></div>"
           
           IF WS-STATUS IS EQUAL TO "single"
              DISPLAY "<div><label><input type='radio' name='status' 
              value='single' checked>single</label></div>"
           ELSE
              DISPLAY "<div><label><input type='radio' name='status' 
              value='single'>single</label></div>"
           END-IF
           
           IF WS-STATUS IS EQUAL TO "joint"
              DISPLAY "<div><label><input type='radio' name='status' 
              value='joint' checked>married joint</label></div>"
           ELSE
              DISPLAY "<div><label><input type='radio' name='status' 
              value='joint'>married joint</label></div>"
           END-IF

           IF WS-STATUS IS EQUAL TO "separate"
              DISPLAY "<div><label><input type='radio' name='status' 
              value='separate' checked>married separate
              </label></div>"
           ELSE
              DISPLAY "<div><label><input type='radio' name='status' 
              value='separate'>married separate
              </label></div>"
           END-IF

           IF WS-STATUS IS EQUAL TO "head"
              DISPLAY "<div><label><input type='radio' name='status' 
              value='head' checked>head of household</label>
              </div>"
           ELSE
              DISPLAY "<div><label><input type='radio' name='status' 
              value='head'>head of household</label>
              </div>"
           END-IF

           DISPLAY "<br>"
           DISPLAY "<select name='year'>"
           
           IF WS-SCHEDULES-FILENAME(11:4) IS EQUAL "2019"
              DISPLAY "<option value='schedules/2019' selected>2019</opt
      -"ion>"
              DISPLAY "<option value='schedules/2018'>2018</option>"
           ELSE
              DISPLAY "<option value='schedules/2019'>2019</option>"
              DISPLAY "<option value='schedules/2018' selected>2018</opt
      -"ion>"
           END-IF
           DISPLAY "</select>"
           DISPLAY "<br>"
           DISPLAY "<br>"
           DISPLAY "<div><input type = 'submit' value = 'Submit'></div>"
           DISPLAY "</form>"
           DISPLAY "<br>".
      ******************************************************************
      * this section will load the appropriate marginal tax tables
      * based on the selection of the user
      ******************************************************************
       LOAD-MARGINAL-RATES SECTION.
      * THE RATES ARE INVARIANT 
           MOVE 0.1     TO WS-RATE(1)
           MOVE 0.12    TO WS-RATE(2)
           MOVE 0.22    TO WS-RATE(3)
           MOVE 0.24    TO WS-RATE(4)
           MOVE 0.32    TO WS-RATE(5)
           MOVE 0.35    TO WS-RATE(6)
           MOVE 0.37    TO WS-RATE(7)
           IF WS-STATUS EQUAL TO "single"
               SUBTRACT WS-STD-DEDUCT FROM WS-AMOUNT GIVING WS-AGI
           END-IF
      * married filing jointly    
           IF WS-STATUS EQUAL TO "joint"
               SUBTRACT WS-STD-DEDUCT FROM WS-AMOUNT GIVING WS-AGI
           END-IF
      * married filing separately
           IF WS-STATUS EQUAL TO "separate"
               SUBTRACT WS-STD-DEDUCT FROM WS-AMOUNT GIVING WS-AGI
           END-IF
      * head of household     
           IF WS-STATUS EQUAL TO "head"
               SUBTRACT WS-STD-DEDUCT FROM WS-AMOUNT GIVING WS-AGI
           END-IF
           
           IF WS-AGI < 0 MOVE 0 TO WS-AGI.
      ******************************************************************
      * process the data for marginal rates table construction
      ******************************************************************
       PROCESS-MARGINAL-RATES SECTION.
      * first fill the brackets table
           MOVE ZEROS TO WS-BRACKETS
           SET WS-COUNTER TO 1
           PERFORM UNTIL WS-COUNTER > 6 
               OR WS-AGI < WS-FLOOR(WS-COUNTER)
               IF WS-AGI < WS-FLOOR(WS-COUNTER + 1)
                   COMPUTE WS-BRACKET(WS-COUNTER)
                   = WS-AGI - WS-FLOOR(WS-COUNTER)
               ELSE
                    COMPUTE WS-BRACKET(WS-COUNTER) 
                    = WS-FLOOR(WS-COUNTER + 1) - WS-FLOOR(WS-COUNTER)
               END-IF
               SET WS-COUNTER UP BY 1
           END-PERFORM
           IF WS-AGI > WS-FLOOR(7)
               COMPUTE WS-BRACKET(7) = WS-AGI - WS-FLOOR(7)
           END-IF
      * then compute the marginal taxes
           SET WS-TAX TO 0
           SET WS-COUNTER TO 1
           PERFORM UNTIL WS-COUNTER > 7
               COMPUTE WS-MARGINAL-TAX(WS-COUNTER) 
               = WS-RATE(WS-COUNTER) * WS-BRACKET(WS-COUNTER)
               SET WS-TAX UP BY WS-MARGINAL-TAX(WS-COUNTER)
               SET WS-COUNTER UP BY 1   
           END-PERFORM
           SUBTRACT WS-TAX FROM WS-AMOUNT GIVING WS-NET-INCOME
           DIVIDE WS-TAX BY WS-AMOUNT GIVING WS-TMP.
           MULTIPLY WS-TMP BY 100.00 GIVING WS-TMP
           MOVE WS-TMP TO WS-EFFECTIVE-RATE.
      ******************************************************************
      * output the marginal rates &c table - html output
      * default css style overridden in first three lines of procedure
      ******************************************************************
       OUTPUT-MARGINAL-TAXES-HTML SECTION.
      *     DISPLAY "<p>", WS-SCHEDULES-FILENAME(11:4), 
      *        " income tax schedules</p>"
           MOVE WS-AGI TO WS-CURRENCY
           DISPLAY WS-BLANK-LINE
           MOVE WS-AMOUNT TO WS-CURRENCY
           DISPLAY "<table class='gradienttable'>"
           DISPLAY "<tr>" WITH NO ADVANCING
           DISPLAY "<th style='text-align:left;'>" NO ADVANCING
           DISPLAY "gross income </th>" WITH NO ADVANCING
           DISPLAY "<td style='text-align:right;'>", WS-CURRENCY, 
           "</td>" WITH NO ADVANCING
           DISPLAY "</tr>"
           MOVE WS-STD-DEDUCT TO WS-CURRENCY
           DISPLAY "<tr>" WITH NO ADVANCING
           DISPLAY "<th style='text-align:left;'>" NO ADVANCING
           DISPLAY "standard deduction</th>" WITH NO ADVANCING
           DISPLAY "<td style='text-align:right;'>", WS-CURRENCY, 
           "</td>" WITH NO ADVANCING
           DISPLAY "</tr>" 
           MOVE WS-AGI TO WS-CURRENCY
           DISPLAY "<tr>" WITH NO ADVANCING
           DISPLAY "<th style='text-align:left;'>" NO ADVANCING
           DISPLAY "adjusted gross income</th>" WITH NO ADVANCING
           DISPLAY "<td style='text-align:right;'>", WS-CURRENCY, 
           "</td>" WITH NO ADVANCING
           DISPLAY "</tr>"
           SET WS-COUNTER TO 1
           PERFORM UNTIL WS-COUNTER > 6 
           OR WS-AGI < WS-FLOOR(WS-COUNTER + 1)
               MOVE WS-MARGINAL-TAX(WS-COUNTER) TO WS-RANGE-VAL
               COMPUTE WS-PERCENT = 100 * WS-RATE(WS-COUNTER)
               MOVE WS-FLOOR(WS-COUNTER) TO WS-RANGE-VAL
               DISPLAY "<tr>" WITH NO ADVANCING
               DISPLAY "<th style='text-align:right;'>" NO ADVANCING
               DISPLAY WS-RANGE-VAL, " - " NO ADVANCING
               MOVE WS-FLOOR(WS-COUNTER + 1) TO WS-RANGE-VAL
               DISPLAY WS-RANGE-VAL, " @ " NO ADVANCING
               MOVE WS-MARGINAL-TAX(WS-COUNTER) TO WS-CURRENCY
               DISPLAY WS-PERCENT, "% </th>" NO ADVANCING
               DISPLAY "<td style='text-align:right;'>", WS-CURRENCY, 
               "</td>" NO ADVANCING
               DISPLAY "</tr>"
               SET WS-COUNTER UP BY 1
           END-PERFORM
           MOVE WS-MARGINAL-TAX(WS-COUNTER) TO WS-RANGE-VAL
           COMPUTE WS-PERCENT = 100 * WS-RATE(WS-COUNTER)
           MOVE WS-FLOOR(WS-COUNTER) TO WS-RANGE-VAL
           DISPLAY "<tr>" NO ADVANCING
           DISPLAY "<th style='text-align:right;'>" NO ADVANCING
           DISPLAY WS-RANGE-VAL, " - " NO ADVANCING
           MOVE WS-AGI TO WS-RANGE-VAL
           DISPLAY WS-RANGE-VAL, " @ ", WS-PERCENT, "%</th>"
           NO ADVANCING
           MOVE WS-MARGINAL-TAX(WS-COUNTER) TO WS-CURRENCY
           DISPLAY "<td style='text-align:right;'>", WS-CURRENCY, 
           "</td>" NO ADVANCING
           DISPLAY "</tr>"
           MOVE WS-TAX TO WS-CURRENCY
           DISPLAY "<tr>" NO ADVANCING
           DISPLAY "<th style='text-align:left;'>" NO ADVANCING
           DISPLAY "total tax</th>" NO ADVANCING
           DISPLAY "<td style='text-align:right;'>", WS-CURRENCY, 
           "</td>" NO ADVANCING
           DISPLAY "</tr>"
           MOVE WS-NET-INCOME TO WS-CURRENCY
           DISPLAY "<tr>" NO ADVANCING
           DISPLAY "<th style='text-align:left;'>" NO ADVANCING
           DISPLAY "net income after taxes</th>" NO ADVANCING
           DISPLAY "<td style='text-align:right;'>", WS-CURRENCY, 
           "</td>" NO ADVANCING
           DISPLAY "</tr>"
           
           DISPLAY "<tr><th style='text-align:left;'>" NO ADVANCING
           DISPLAY "effective tax rate</th>" NO ADVANCING
           DISPLAY "<td style='text-align:right;'>" NO ADVANCING
           DISPLAY WS-EFFECTIVE-RATE, "%</td></tr>"
           DISPLAY "</table>"
           DISPLAY "<br>".
