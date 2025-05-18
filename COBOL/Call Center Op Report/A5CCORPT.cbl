       identification division.
       program-id. A5CCORPT.
       author. Bilgan Kiris.
       date-written. March 7, 2025.
      *Program Description:
      *
       environment division.
       input-output section.
       file-control.
      *
           select emp-file
               assign to INFILE
               organization is sequential.
      *
           select report-file
               assign to RPTFILE
               organization is sequential.
      *
       data division.
       file section.
      *
       fd emp-file
           recording mode is F
           data record is emp-rec
           record contains 51 characters.
      *
       01 emp-rec.
           05 emp-rec-num              pic x(3).
           05 emp-rec-name             pic x(12).
           05 emp-rec-calls
                       occurs 12 times pic 999.
      *


      *
       fd report-file
           recording mode is F
           data record is report-line
           record contains 132 characters.
      *
       01 report-line                  pic x(132).
      *
       working-storage section.
      *
      *create the necessary working storage variables
      *
      *-----------------------------------------------
      *  CONSTANTS
      *-----------------------------------------------
       01 ws-constants.
           05 ws-number-of-months      pic 99   value 12.
      *
       01 ws-calculated-fields.
           05 ws-non-zero-month-count  pic 9(2) value 0.
      *
       01 ws-eof-flag                  pic x    value 'n'.
           88 ws-end-of-file                    value "y".
      *
      *-----------------------------------------------
      *  VARIABLES
      *-----------------------------------------------
       01 ws-vars.
           05 ws-month
                value "JULAUGSEPOCTNOVDECJANFEBMARAPRMAYJUN".
                10 ws-month-name       pic x(3) occurs 12 times.

      *
       01 ws-index                     pic 9(2) value 1.

       01 ws-totals.
           05 ws-grand-total           pic 9(5) value 0.
           05 ws-emp-total             pic 9(5) value 0.
           05 ws-total-no-calls        pic 9(5) value 0.
           05 ws-months-no-call        pic 9(5) value 0.
      *
       01 ws-emp-avg.
           05 ws-emp-avg-numeric       pic 9(3).


       01 ws-emp-avg-r redefines ws-emp-avg.
           05 ws-emp-avg-text          pic x(4).
      *
       01 ws-emp-rem                   pic 999.
       01 ws-max-emp-avg               pic 9(3).
       01 ws-opr-rem-total             pic 9(4).
       01 ws-detail-line-rem           pic 999.
       01 ws-avg-totally               pic 9(5) value 0.
       01 ws-opr-with-cal.
           05 ws-opr-with-calls occurs 12 times
                                       pic 9999.
       01 ws-opr-tt-avg.
           05 ws-opr-total-avg occurs 12 times
                                       pic 999.
       01 ws-opr-total-called.
           05 ws-opr-total-calls occurs 12 times
                                       pic 9999.
      *
       01 ws-monthly-opr-count         pic 999.
       01 ws-monthly-total-calls       pic 9.
       01 ws-emp-working-months        pic 99.
      *
       01 ws-highest-avg               pic 9(5) value 0.
       01 ws-lowest-avg                pic 9(5) value 999.
       01 ws-highest-operator          pic x(3).
       01 ws-lowest-operator           pic x(3).
       01 ws-highest-month-val         pic 9(4) value 0.
       01 ws-blank-line                pic x(132)  value spaces.
      *
      *-----------------------------------------------
      *  REPORT HEADER & TITLE
      *-----------------------------------------------
       01 ws-name-line.
           05 filler                   pic x(83)
               value spaces.
           05 filler                   pic x(14)
               value 'Bilgan Kiris, '.
      *               ----+----1----+----2----+
           05 filler                   pic x(12)
               value 'Assingment 5'.
      *               ----+----1----+----2----+----
           05 filler                   pic x(5)
               value spaces.
           05 ws-name-line-date        pic 9(6).
           05 filler                   pic x(4)
               value spaces.
           05 ws-name-line-time        pic 9(8).
      *
       01 ws-report-heading.
           05 filler                   pic x(40)
               value spaces.
           05 filler                   pic x(40)
               value 'call centre volumes for july - june     '.
      *               ----+----1----+----2----+----3----+----4
           05 filler                   pic x(40)
               value spaces.
           05 filler                   pic x(12)
               value spaces.
      *
      *-----------------------------------------------
      *  COLUMN HEADERS
      *-----------------------------------------------
       01 ws-heading-line1.
           05 filler                   pic x(2) value spaces.
           05 filler                   pic x(8) value 'operator'.
           05 filler                   pic x(2) value spaces.
           05 filler                   pic x(8) value 'operator'.
           05 filler                   pic x(6) value spaces.
           05 filler                   pic x(3) value 'jul'.
           05 filler                   pic x(4) value spaces.
           05 filler                   pic x(3) value 'aug'.
           05 filler                   pic x(4) value spaces.
           05 filler                   pic x(3) value 'sep'.
           05 filler                   pic x(4) value spaces.
           05 filler                   pic x(3) value 'oct'.
           05 filler                   pic x(4) value spaces.
           05 filler                   pic x(3) value 'nov'.
           05 filler                   pic x(4) value spaces.
           05 filler                   pic x(3) value 'dec'.
           05 filler                   pic x(4) value spaces.
           05 filler                   pic x(3) value 'jan'.
           05 filler                   pic x(4) value spaces.
           05 filler                   pic x(3) value 'feb'.
           05 filler                   pic x(4) value spaces.
           05 filler                   pic x(3) value 'mar'.
           05 filler                   pic x(4) value spaces.
           05 filler                   pic x(3) value 'apr'.
           05 filler                   pic x(4) value spaces.
           05 filler                   pic x(3) value 'may'.
           05 filler                   pic x(4) value spaces.
           05 filler                   pic x(3) value 'jun'.
           05 filler                   pic x(4) value spaces.
           05 filler                   pic x(5) value 'total'.
           05 filler                   pic x(4) value spaces.
           05 filler                   pic x(3) value 'avg'.
           05 filler                   pic x(4) value spaces.
           05 filler                   pic x(3) value 'rem'.
           05 filler                   pic x(3) value spaces.
      *
       01 ws-heading-line2.
           05 filler                   pic x(5) value spaces.
           05 filler                   pic x(1) value '#'.
           05 filler                   pic x(8) value spaces.
           05 filler                   pic x(4) value 'name'.
      *
      *-----------------------------------------------
      *  DETAIL LINE
      *-----------------------------------------------
       01 ws-detail-line.
           05 filler                   pic x(4)
               value spaces.
           05 ws-detail-line-num       pic x(3).
           05 filler                   pic x(6)
               value spaces.
           05 ws-detail-line-name      pic x(11).
           05 filler                   pic x(1)
               value spaces.
           05 ws-detail-line-months occurs 12 times.
                10 ws-num-calls        pic zzz9.
                10 filler              pic x(3)
                    value spaces.
           05 ws-detail-line-total     pic zzzz9.
           05 filler                   pic x(3)
               value spaces.
           05 ws-detail-line-avg-group.
                10 ws-detail-line-avg-num
                                       pic zzzz9.
                10 ws-detail-line-avg-text
                    redefines ws-detail-line-avg-num
                                       pic x(5).
           05 filler                   pic x(4)
               value spaces.
           05 dl-detail-line-rem       pic zzz9.
      *
      *-----------------------------------------------
      *  DETAIL LINE SUMMARY
      *-----------------------------------------------
       01 ws-operator-calls.
           05 filler                   pic x(4)
                value spaces.
           05 filler                   pic x(22)
                value "Operators with calls  ".
           05 ws-opr-calls occurs 12 times.
                10 dl-opr-with-calls   pic zz9.
                10 filler              pic x(4)
                    value spaces.

       01 ws-operator-totals.
           05 filler                   pic x(4)
                value spaces.
           05 filler                   pic x(21)
                value "Totals             ".
           05 ws-opr-totals occurs 12 times.
                10 dl-opr-total-calls  pic zzz9.
                10 filler              pic x(3)
                    value spaces.
           05 filler                   pic x(2)
                value spaces.
           05 dl-opr-totally           pic zzzz9.
           05 filler                   pic x
                value spaces.
           05 dl-avg-total             pic zzzzz9.
           05 filler                   pic x(2)
                value spaces.
           05 dl-opr-rem-total         pic zzz9.

       01 ws-operator-avg.
           05 filler                   pic x(4)
                value spaces.
           05 filler                   pic x(21)
                value "Averages             ".
           05 ws-opr-avg occurs 12 times.
                10 dl-opr-total-avg    pic zz9.
                10 filler              pic x(4)
                    value spaces.
      *-----------------------------------------------
      *  SUMMARY LINES
      *-----------------------------------------------
       01 ws-total-line1.
           05 filler                   pic x(4)
               value spaces.
           05 filler                   pic x(35)
               value "number of operators with no calls: ".
      *               ----+----1----+----2----+----3----+
           05 ws-total-line-no-calls   pic zzzz9.
      *
       01 ws-total-line2.
           05 filler                   pic x(4)
               value spaces.
           05 filler                   pic x(36)
               value "number of months with no calls:   ".
      *               ----+----1----+----2----+----3----+
           05 ws-total-line-zero-mths  pic zzzz9.
      *
       01 ws-total-line3.
           05 filler                   pic x(4)
               value spaces.
           05 filler                   pic x(35)
               value "overall total calls:               ".
      *               ----+----1----+----2----+----3----+
           05 ws-total-line-calls      pic zzzz9.
      *
       01 ws-highest-average.
           05 filler                   pic x(4)
                   value spaces.
           05 filler                   pic x(46)
               value "operator with the highest monthly average: ".
      *               ----+----1----+----2----+----3----+
           05 ws-opr-highest-no        pic x(3).
           05 filler                   pic x(4)
                value spaces.
           05 ws-opr-highest-avg       pic z(3).
      *
       01 ws-lowest-average.
           05 filler                   pic x(4)
                   value spaces.
           05 filler                   pic x(46)
               value "operator with the lowest monthly average: ".
      *               ----+----1----+----2----+----3----+
           05 ws-opr-lowest-no         pic x(3).
           05 filler                   pic x(4)
                value spaces.
           05 ws-opr-lowest-avg        pic z(3).
      *
       01 ws-total-line4.
           05 filler                   pic x(4)
               value spaces.
           05 filler                   pic x(39)
               value "month with highest monthly average:    ".
      *               ----+----1----+----2----+----3----+
           05 ws-highest-month-avg     pic z9.
           05 filler                   pic x(2)
                value spaces.
           05 ws-highest-month         pic x(3).

       procedure division.
      *
       000-main.
      *
      *open files
           open input  emp-file,
                output report-file.
      *
      *get the current date & time
           accept ws-name-line-date from date.
           accept ws-name-line-time from time.
      *
      *output first headings
           perform 100-print-headings.
      *
      *process input file & output results
           perform 200-read-input-file.
      *
           perform 300-process-records
               until ws-end-of-file.
      *
      *output total lines
           perform 400-print-totals.
      *
      *close files
           close emp-file
                 report-file.
      *
           stop run.
      *
       100-print-headings.
      *
           write report-line from ws-name-line
               after advancing 1 line.
           write report-line            from ws-blank-line.
      *
           write report-line from ws-report-heading
               after advancing 1 line.
      *
           write report-line from ws-heading-line1
               after advancing 2 lines.
      *
           write report-line from ws-heading-line2
               after advancing 1 line.
      *
       200-read-input-file.
      *reads a line from input file & stores it in emp-rec
      * - unless eof is encountered in which case it sets ws-eof-flag to y
           read emp-file
                 at end move 'y'         to ws-eof-flag.

       300-process-records.
      * TODO: Use Perform Varying to loop through monthly calls
      *       in each record to calculate the required values
      *       for each record and accumulate the required data
      *       for total lines
      *
      *-----------------------------------------------
      *  DETAIL LINE CALCULATIONS
      *-----------------------------------------------
           move 0 to ws-emp-total.
           move 0 to ws-emp-working-months.
           move 0 to ws-opr-with-calls(ws-index).
           move 0 to ws-opr-total-calls(ws-index).
           perform varying ws-index from 1 by 1
                until ws-index > 12
                move emp-rec-calls(ws-index)
                    to ws-num-calls(ws-index)
                add emp-rec-calls(ws-index) to ws-emp-total


      *         totals -- detail line summary
                add emp-rec-calls(ws-index)
                    to ws-opr-total-calls(ws-index)
                move ws-opr-total-calls(ws-index)
                to dl-opr-total-calls(ws-index)

      *         counter for the months worked
      *         operator with calls calculations

                if emp-rec-calls(ws-index) > 0
                    add 1 to ws-emp-working-months
                    add 1 to ws-opr-with-calls(ws-index)
                    move ws-opr-with-calls(ws-index)
                        to dl-opr-with-calls(ws-index)
                end-if

      *         MONTHS WITH NO CALLS CALCULATION -- SUMMARY LINE
                if emp-rec-calls(ws-index) = 0
                    add 1 to ws-months-no-call
                end-if


           end-perform.

           add ws-emp-total            to ws-grand-total
           move ws-grand-total         to dl-opr-totally.




      * TODO: Implement average calculation logic
      *       as outlined in the requirments



      * TODO: Move required data to detail line for output
      *
           move emp-rec-num            to ws-detail-line-num.
           move emp-rec-name           to ws-detail-line-name.
           move ws-emp-total           to ws-detail-line-total.


      *

      *-----------------------------------------------
      *  REMAINDER & AVERAGE CALCULATION
      *-----------------------------------------------
           if ws-emp-working-months > 0 then
                divide ws-emp-total by ws-emp-working-months
                    giving ws-emp-avg-numeric
                    remainder ws-emp-rem
           end-if.

           if ws-emp-avg-numeric = 0 then
                move " ZERO" to ws-detail-line-avg-text
           else
                move ws-emp-avg-numeric to ws-detail-line-avg-num
           end-if.


           add ws-emp-avg-numeric      to ws-avg-totally.
           move ws-avg-totally         to dl-avg-total.

           move ws-emp-rem             to ws-detail-line-rem.
           move ws-detail-line-rem     to dl-detail-line-rem.
           add ws-emp-rem              to ws-opr-rem-total.
           move ws-opr-rem-total       to dl-opr-rem-total.



      *
      * print detail line
           write report-line           from ws-detail-line
                after advancing 1 line.
      *

      *----------------------------------------------------------------
      *  OPERATOR WITH HIGHEST & LOWEST MONTHLY AVERAGE LINE CALCULATION
      *----------------------------------------------------------------
           if ws-emp-avg-numeric > ws-highest-avg then
                move ws-emp-avg-numeric to ws-highest-avg
                move emp-rec-num to ws-highest-operator
           end-if.

           if ws-emp-avg-numeric > 0
                and ws-emp-avg-numeric < ws-lowest-avg then

                move ws-emp-avg-numeric to ws-lowest-avg
                move emp-rec-num to ws-lowest-operator
           end-if.



      * TODO: reset fields for next record
           move 0                      to ws-emp-total.
           move 0                      to ws-emp-avg-numeric.
           move 0                      to ws-detail-line-rem.
           move 0                      to ws-opr-total-avg(ws-index).
           move 0                      to ws-emp-rem.
      *-----------------------------------------------
      *  OPERATORS WITH 0 CALLS CALCULATION
      *-----------------------------------------------
           if ws-detail-line-avg-text = " ZERO" then
                add 1 to ws-non-zero-month-count
                move ws-non-zero-month-count to ws-total-line-no-calls
           end-if.

      *
      * read next record (if any)
           perform 200-read-input-file.
      *
       400-print-totals.
      *
      *-----------------------------------------------
      *  MONTH WITH THE HIGHEST AVERAGE
      *-----------------------------------------------
           perform varying ws-index from 1 by 1 until ws-index > 12
               if ws-opr-with-calls(ws-index) > 0
                   divide ws-opr-total-calls(ws-index)
                        by ws-opr-with-calls(ws-index)
                        giving ws-opr-total-avg(ws-index)
                   move ws-opr-total-avg(ws-index)
                   to dl-opr-total-avg(ws-index)
               end-if

               if ws-opr-total-avg(ws-index) >
                    ws-highest-month-val then
                    move ws-index to ws-highest-month-avg
                    move ws-month-name(ws-index) to ws-highest-month
               end-if

           end-perform.

      * TODO: Move required data to total lines for output
      *
           write report-line           from ws-blank-line.
           write report-line           from ws-operator-calls.
           write report-line           from ws-operator-totals.
           write report-line           from ws-operator-avg.
           write report-line           from ws-blank-line.
           move ws-grand-total         to ws-total-line-calls.
           move ws-months-no-call      to ws-total-line-zero-mths.
           move ws-highest-operator    to ws-opr-highest-no.
           move ws-highest-avg         to ws-opr-highest-avg.
           move ws-lowest-operator     to ws-opr-lowest-no.
           move ws-lowest-avg          to ws-opr-lowest-avg.



      *
           write report-line from ws-total-line1.

           write report-line from ws-total-line2.

           write report-line from ws-highest-average.

           write report-line from ws-lowest-average.

           write report-line from ws-total-line4.

           write report-line from ws-total-line3.
      *
       end program A5CCORPT.