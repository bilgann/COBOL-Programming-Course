       identification division.
       program-id. A3SCOMM.
       date-written. February 4th 2025.
       author. Bilgan Kiris.
      *Description:
      *
       environment division.
       configuration section.
      *
       input-output section.
      *
       file-control.
      * input-file declaration
           select input-file
               assign to INFILE
               organization is sequential.
      *
      * output-file declaration
           select output-file
               assign to OUTFILE
               organization is sequential.
      *
       data division.
       file section.
      *
       fd input-file
           recording mode is F
           data record is input-line
           record contains 32 characters.
      *
       01 input-line.
           05 il-no                         pic x(3).
           05 il-name                       pic x(8).
           05 il-sales                      pic 9(6).
           05 il-min-comm                   pic 9(6).
           05 il-max-comm                   pic 9(6).
           05 il-perc-comm                  pic 99v9.
      *
       fd output-file
           recording mode is F
           data record is output-line
           record contains 95 characters.
      *
       01 output-line                       pic x(95).
      *
       working-storage section.
      *
       01 ws-report-header.
           05 filler                        pic x(59)
                value spaces.
           05 ws-report-title               pic x(36)
                value "Bilgan Kiris, A3 - February 4th 2025".


       01 ws-page-header.
           05 filler                        pic x(37)
                value spaces.
           05 ws-page-title                 pic x(23)
                value "SALES COMMISSION REPORT".
           05 filler                        pic x(37)
                value spaces.

      *-----------------------------------------------
      *  COLUMN HEADER FORMATTING
      *-----------------------------------------------

       01 ws-column-header1.
           05 ws-no                         pic x(3)
                value "NO.".
           05 filler                        pic x(5)
                value spaces.
           05 ws-name                       pic x(4)
                value "NAME".
           05 filler                        pic x(6)
                value spaces.
           05 ws-sales                      pic x(5)
                value "SALES".
           05 filler                        pic x(5)
                value spaces.
           05 ws-min                        pic x(3)
                value "MIN".
           05 filler                        pic x(6)
                value spaces.
           05 ws-max                        pic x(3)
                value "MAX".
           05 filler                        pic x(4)
                value spaces.
           05 ws-rate                       pic x(4)
                value "RATE".
           05 filler                        pic x(5)
                value spaces.
           05 ws-earned                     pic x(6)
                value "EARNED".
           05 filler                        pic x(6)
                value spaces.
           05 ws-paid                       pic x(4)
                value "PAID".
           05 filler                        pic x(5)
                value spaces.
           05 ws-bonus                      pic x(15)
                value "BONUS/ NO BONUS".
           05 filler                        pic x(2)
                value spaces.

       01 ws-column-header2.
           05 ws-dash-1                     pic x(3)
                value "---".
           05 filler                        pic x(3)
                value spaces.
           05 ws-dash-2                     pic x(8)
                value "--------".
           05 filler                        pic x(3)
                value spaces.
           05 ws-dash-3                     pic x(7)
                value "-------".
           05 filler                        pic x(2)
                value spaces.
           05 ws-dash-4                     pic x(7)
                value "-------".
           05 filler                        pic x(2)
                value spaces.
           05 ws-dash-5                     pic x(7)
                value "-------".
           05 filler                        pic x(2)
                value spaces.
           05 ws-dash-6                     pic x(5)
                value "-----".
           05 filler                        pic x(1)
                value spaces.
           05 ws-dash-7                     pic x(10)
                value "----------".
           05 filler                        pic x(2)
                value spaces.
           05 ws-dash-8                     pic x(10)
                value "----------".
           05 filler                        pic x(2)
                value spaces.
           05 ws-dash-9                     pic x(16)
                value "----------------".

      *-----------------------------------------------
      *  DETAIL LINE FORMATTING (For Report Output)
      *-----------------------------------------------
       01 ws-detail-line.
           05 dl-no                         pic x(3).
           05 filler                        pic x(3)
                value spaces.
           05 dl-name                       pic x(8).
           05 filler                        pic x(3)
                value spaces.
           05 dl-sales                      pic z(3),zzz.
           05 filler                        pic x(2)
                value spaces.
           05 dl-min                        pic z(3),zzz.
           05 filler                        pic x(2)
                value spaces.
           05 dl-max                        pic z(3),zzz.
           05 filler                        pic x(2)
                value spaces.
           05 dl-rate                       pic z9.9.
           05 filler                        pic x
                value "%".
           05 filler                        pic x(2)
                value spaces.
           05 dl-earned                     pic $zzz,zz9.
           05 filler                        pic x(2)
                value spaces.
           05 dl-paid                       pic $**,***,**9.
           05 filler                        pic x(5)
                value spaces.
           05 dl-bonus-flag                 pic x(16).



      *--------------------------------------------------------
      *  CALCULATION VARIABLES
      *--------------------------------------------------------
       01 ws-detail-calc.
           05 ws-earned-comm                pic 9(7)v99.
           05 ws-paid-comm                  pic 9(7)v99.
           05 ws-sales-amount               pic 9(7)v99.
           05 ws-comm-rate                  pic 999.
           05 ws-min-comm                   pic 9(6).
           05 ws-max-comm                   pic 9(6).
           05 ws-bonus-flag                 pic x(15).

      *--------------------------------------------------------
      *  TOTAL EARNED AND PAID COMMISSION VARIABLES
      *--------------------------------------------------------
       01 dl-total-amounts.
           05 filler                        pic x(41)
                value spaces.
           05 ws-total-title                pic x(6)
                value "Totals".
           05 filler                        pic x(4)
                value spaces.
           05 dl-total-earned               pic $z,zzz,zzz.
           05 filler                        pic x(1)
                value spaces.
           05 dl-total-paid                 pic $z,zzz,zzz.

       01 ws-total.
           05 ws-total-earned               pic 9(6)v99.
           05 ws-total-paid                 pic 9(6)v99.

      *--------------------------------------------------------
      *  NUMBER WITH BONUS MORE/LESS THAN MIN/MAX
      *--------------------------------------------------------
       01 number-max.
           05 ws-number-max-title           pic x(31)
                value "NUMBER WITH BONUS MORE THAN MAX".
           05 filler                        pic x(6)
                value spaces.
           05 dl-bonus-more-max             pic z(3).
       01 number-min.
           05 ws-number-min-title           pic x(31)
                value "NUMBER WITH BONUS LESS THAN MIN".
           05 filler                        pic x(6)
                value spaces.
           05 dl-bonus-less-min             pic z(3).

       01 ws-bonus-number.
           05 ws-bonus-more-max             pic 9(3).
           05 ws-bonus-less-min             pic 9(3).

      *--------------------------------------------------------
      *  NUMBER OF SALESPEOPLE WITH/WITHOUT BONUS
      *--------------------------------------------------------
       01 ws-people-bonus.
           05 ws-people-bonus-title         pic x(32)
                value "NUMBER OF SALESPEOPLE WITH BONUS".
           05 filler                        pic x(5)
                value spaces.
           05 dl-with-bonus                 pic z(3).
       01 ws-people-no-bonus.
           05 ws-people-no-bon-title        pic x(35)
                value "NUMBER OF SALESPEOPLE WITHOUT BONUS".
           05 filler                        pic x(2)
                value spaces.
           05 dl-without-bonus              pic z(3).
       01 ws-no-person.
           05 ws-no-person-title            pic x(21)
                value "NUMBER OF SALESPEOPLE".
           05 filler                        pic x(16)
                value spaces.
           05 dl-no-salesperson             pic z(3).

       01 ws-people-bonus2.
           05 ws-with-bonus                 pic 9(3).
           05 ws-without-bonus              pic 9(3).
           05 ws-no-salesperson             pic 9(3).

      *--------------------------------------------------------
      *  NUMBER OF SALESPEOPLE WITH/WITHOUT BONUS
      *--------------------------------------------------------
       01 ws-no-paid.
           05 ws-no-paid-title              pic x(29)
                value "NUMBER WITH PAID EQUAL EARNED".
           05 filler                        pic x(8)
                value spaces.
           05 dl-no-paid-earned             pic z(3).

       01 ws-percent-paid.
           05 ws-perc-paid-title            pic x(30)
                value "PERCENT WITH PAID EQUAL EARNED".
           05 filler                        pic x(7)
                value spaces.
           05 dl-perc-paid-earned           pic z(3).99.
           05 filler                        pic x
                value "%".

       01 ws-no-perc-paid.
           05 ws-no-paid-earned             pic 9(3).
           05 ws-perc-paid-earned           pic 9(3).99.


      *--------------------------------------------------------
      *  PERCENT WITH/WITHOUT BONUS
      *--------------------------------------------------------
       01 ws-perc-bonus.
           05 ws-perc-with-bon-title        pic x(28)
                value "PERCENT WITH BONUS   >300,00".
           05 filler                        pic x(9)
                value spaces.
           05 dl-perc-with-bonus            pic z(3).99.
           05 filler                        pic x
                value "%".

       01 ws-perc-no-bonus.
           05 ws-perc-no-bo-title           pic x(31)
                value "PERCENT WITHOUT BONUS <=300,000".
           05 filler                        pic x(6)
                value spaces.
           05 dl-perc-without-bonus         pic z(3).99.
           05 filler                        pic x
                value "%".

       01 ws-perc-bonus-perc.
           05 ws-perc-with-bonus            pic 9(3).99.
           05 ws-perc-without-bonus         pic 9(3).99.


      *-----------------------------------------
      *  CONSTANTS (77 LEVEL - NAMED CONSTANTS)
      *-----------------------------------------
       77 sales-comm                        pic 9(6)
           value 300000.
       77 bonus-rate                        pic 9(2)V9(2)
           value 15.25.
       77 hundred                           pic 999
           value 100.
       77 bonus-earned                      pic x(12)
           value "BONUS EARNED".
       77 bonus-not-earned                  pic x(16)
           value "BONUS NOT EARNED".

       01 ws-page-summary                   pic x(107)  value "Page".

       01 ws-report-summary                 pic x(107).

       01 ws-blank-line                     pic x(107)  value spaces.
      *

       01 ws-eof-flag                       pic x.
       77 ws-eof-y                          pic x       value "Y".
       77 ws-eof-n                          pic x       value "N".
      *
       01 ws-page.
           05 ws-lines-per-page             pic 99      value 5.
           05 ws-line-ctr                   pic 99      value 0.
           05 ws-page-num                   pic 99      value 0.
      *
       procedure division.
       000-main.
      *
           perform 100-open-files.
           write output-line                from ws-report-header.
           perform 200-read-file.
           perform 400-process-recs
                until ws-eof-flag is equal to ws-eof-y.
           write output-line                from ws-report-summary.



           write output-line                from ws-blank-line.

           move ws-bonus-more-max           to dl-bonus-more-max.
           write output-line                from number-max.


           move ws-bonus-less-min           to dl-bonus-less-min.
           write output-line                from number-min.

           write output-line                from ws-blank-line
           move ws-with-bonus               to dl-with-bonus.
           write output-line                from ws-people-bonus.

           move ws-without-bonus            to dl-without-bonus.
           write output-line                from ws-people-no-bonus.

           move ws-no-salesperson           to dl-no-salesperson.
           write output-line                from ws-no-person.

           write output-line                from ws-blank-line
           move ws-no-paid-earned           to dl-no-paid-earned.
           write output-line                from ws-no-paid.

           move ws-perc-paid-earned         to dl-perc-paid-earned.
           write output-line                from ws-percent-paid.

           write output-line                from ws-blank-line.
           move ws-perc-with-bonus          to dl-perc-with-bonus.
           write output-line                from ws-perc-bonus.

           move ws-perc-without-bonus       to dl-perc-without-bonus.
           write output-line                from ws-perc-no-bonus.

           perform 900-close-files.
           goback.
      *
       100-open-files.
           open input input-file.
           open output output-file.
           move ws-eof-n                    to    ws-eof-flag.
      *
       200-read-file.
           read input-file
               at end move ws-eof-y         to    ws-eof-flag.
      *
       400-process-recs.
           add 1 to ws-page-num.
           if ws-page-num is equal to 1 then
                write output-line           from ws-blank-line
                write output-line           from ws-blank-line
                write output-line           from ws-page-header
           else
                write output-line           from ws-blank-line
                    after advancing page
                write output-line           from ws-blank-line
                write output-line           from ws-report-header
                write output-line           from ws-blank-line
                write output-line           from ws-page-header
           end-if.
           write output-line                from ws-blank-line
           write output-line                from ws-blank-line
           write output-line                from ws-column-header1.
           write output-line                from ws-column-header2.
           perform 500-process-detail
                varying ws-line-ctr from 1 by 1
                    until ws-line-ctr > ws-lines-per-page or
                        ws-eof-flag is equal to ws-eof-y.

           write output-line                from ws-blank-line.

           write output-line                from dl-total-amounts.

           write output-line                from ws-page-summary.
      *
       500-process-detail.
      *    do all the calculations and manipulations and
      *    detail line preparations.

      *------------------------------------------------------
      *  Move Input Values into Working Storage for Calculations
      *------------------------------------------------------
           move il-sales                    to ws-sales-amount.
           move il-perc-comm                to ws-comm-rate.
           move il-min-comm                 to ws-min-comm.
           move il-max-comm                 to ws-max-comm.


      *------------------------------------------------------
      *  BONUS/ NO BONUS Determination
      *------------------------------------------------------
           if ws-sales-amount > sales-comm then
                move bonus-earned to ws-bonus-flag
           else
                move bonus-not-earned to ws-bonus-flag
           end-if.
      *------------------------------------------------------
      *  EARNED COMMISSION CALCULATION
      *------------------------------------------------------
           if ws-sales-amount <= sales-comm then
               compute ws-earned-comm = ws-sales-amount
                                        * (ws-comm-rate / hundred)
           else
               compute ws-earned-comm = (ws-sales-amount
                                        * (ws-comm-rate / hundred))
                                        + ((ws-sales-amount -
                                            sales-comm) * (15.25
                                            / hundred)).

      *------------------------------------------------------
      *  PAID COMMISSION CALCULATION
      *------------------------------------------------------
           if ws-sales-amount > sales-comm then
                if ws-earned-comm > ws-max-comm then
                    move ws-max-comm to ws-paid-comm
                else
                    move ws-earned-comm to ws-paid-comm
                end-if
           else
                if ws-earned-comm < ws-min-comm then
                    move ws-min-comm to ws-paid-comm
                else
                    move ws-earned-comm to ws-paid-comm
                end-if.

      *------------------------------------------------------
      *  TOTALS CALCULATIONS
      *------------------------------------------------------
           add ws-earned-comm               to ws-total-earned.
           add ws-paid-comm                 to ws-total-paid.


      *------------------------------------------------------
      *  NUMBER WITH BONUS MORE THAN MAX
      *  NUMBER WITH NO BONUS LESS THAN MIN
      *------------------------------------------------------
           if ws-sales-amount > ws-max-comm then
                add 1 to ws-bonus-more-max
           end-if.

           if ws-sales-amount < ws-min-comm then
                add 1 to ws-bonus-less-min
           end-if.

      *------------------------------------------------------
      *   NUMBER OF SALESPEOPLE WITH BONUS
      *   NUMBER OF SALESPEOPLE WITHOUT BONUS
      *   NUMBER OF SALESPEOPLE
      *------------------------------------------------------
           if ws-sales-amount > sales-comm then
                add 1 to ws-with-bonus
           else
                add 1 to ws-without-bonus
           end-if.

           compute ws-no-salesperson = ws-with-bonus + ws-without-bonus.

      *------------------------------------------------------
      *    NUMBER WITH PAID EQUAL EARNED
      *    PERCENT WITH PAID EQUAL EARNED
      *------------------------------------------------------
           if ws-earned-comm = ws-paid-comm then
                add 1 to ws-no-paid-earned
           end-if.

           if ws-no-salesperson > 0 then
                compute ws-perc-paid-earned =
                    (ws-no-paid-earned * 100) / ws-no-salesperson
           end-if.

      *------------------------------------------------------
      *   PERCENT WITH BONUS   >300,00
      *   PERCENT WITHOUT BONUS <=300,000
      *------------------------------------------------------
           compute ws-perc-with-bonus rounded =
                (ws-with-bonus * 100) / ws-no-salesperson.

           compute ws-perc-without-bonus rounded =
                (ws-without-bonus * 100) / ws-no-salesperson.


      * moving input file values into working storage for calculations
           move il-no                       to dl-no.
           move il-name                     to dl-name.
           move il-sales                    to dl-sales.
           move il-min-comm                 to dl-min.
           move il-max-comm                 to dl-max.
           move il-perc-comm                to dl-rate.
           move ws-earned-comm              to dl-earned.
           move ws-paid-comm                to dl-paid.
           move ws-bonus-flag               to dl-bonus-flag.
           move ws-total-earned             to dl-total-earned.
           move ws-total-paid               to dl-total-paid.


           write output-line                from ws-detail-line.





      *    do all the summary (accumulator) actions.
           perform 200-read-file.
      *
       900-close-files.
           close input-file.
           close output-file.
      *
       end program A3SCOMM.