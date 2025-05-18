       identification division.
       program-id. A4SALRPT.
       date-written. February 12th, 2025.
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
           record contains 28 characters.
      *
       01 input-line.
           05 il-no                         pic x(3).
           05 il-name                       pic x(15).
           05 il-code                       pic x.
                88 il-grad          value "G".
                88 il-nongrad       value "N".
           05 il-years                      pic 99.
                88 il-g-analyst     value 16 thru 99.
                88 il-g-senior      value 7 thru 15.
                88 il-g-prog        value 3 thru 6.
                88 il-g-unclass     value 0 thru 2.
                88 il-n-prog        value 11 thru 99.
                88 il-n-jrprog      value 5 thru 10.
                88 il-n-unclas      value 0 thru 4.
           05 il-salary                     pic 9(5)v99.
      *
       fd output-file
           recording mode is F
           data record is output-line
           record contains 82 characters.
      *
       01 output-line                       pic x(82).
      *
       working-storage section.
      *
      *-----------------------------------------------
      *  REPORT HEADER
      *-----------------------------------------------
       01 ws-report-header.
           05 filler                        pic x(21)
                value spaces.
           05 ws-report-title               pic x(27)
                value "Bilgan Kiris - Assignment 4".
           05 filler                        pic x(4)
                value spaces.
           05 ws-report-date                pic 9(8).
           05 filler                        pic x(21)
                value spaces.
      *-----------------------------------------------
      *  PAGE HEADER
      *-----------------------------------------------
       01 ws-page-header.
           05 filler                        pic x(30)
                value spaces.
           05 ws-salary-report-title        pic x(22)
                value "EMPLOYEE SALARY REPORT".
           05 filler                        pic x(30)
                value spaces.
      *-----------------------------------------------
      *  COLUMN HEADER
      *-----------------------------------------------
       01 ws-column-header.
           05 emp-no-1                      pic x(3)
                value "EMP".
           05 filler                        pic x(1)
                value spaces.
           05 emp-name-1                    pic x(3)
                value "EMP".
           05 filler                        pic x(28)
                value spaces.
           05 present-1                     pic x(7)
                value "PRESENT".
           05 filler                        pic x(2)
                value spaces.
           05 increase-1                    pic x(8)
                value "INCREASE".
           05 filler                        pic x(5)
                value spaces.
           05 pay-1                         pic x(3)
                value "PAY".
           05 filler                        pic x(11)
                value spaces.
           05 new-1                         pic x(3)
                value "NEW".
           05 filler                        pic x(8)
                value spaces.

       01 ws-column-header2.
           05 emp-no-2                      pic x(3)
                value "NUM".
           05 filler                        pic x
                value spaces.
           05 emp-name-2                    pic x(4)
                value "NAME".
           05 filler                        pic x(11)
                value spaces.
           05 years-1                       pic x(5)
                value "YEARS".
           05 filler                        pic x(1)
                value spaces.
           05 position-1                    pic x(8)
                value "POSITION".
           05 filler                        pic x(3)
                value spaces.
           05 present-2                     pic x(6)
                value "SALARY".
           05 filler                        pic x(5)
                value spaces.
           05 perc-sign                     pic x
                value "%".
           05 filler                        pic x(7)
                value spaces.
           05 pay-2                         pic x(8)
                value "INCREASE".
           05 filler                        pic x(7)
                value spaces.
           05 new-2                         pic x(6)
                value "SALARY".
      *-----------------------------------------------
      *  DETAIL LINE - VARIABLES & FORMATTING
      *-----------------------------------------------
       01 ws-detail-line.
           05 dl-emp-no                     pic x(3).
           05 filler                        pic x
                value spaces.
           05 dl-emp-name                   pic x(15).
           05 filler                        pic x(2)
                value spaces.
           05 dl-years                      pic x(2).
           05 filler                        pic x(2)
                value spaces.
           05 dl-position                   pic x(8).
           05 filler                        pic x(2)
                value spaces.
           05 dl-present-salary             pic zz,zz9.99.
           05 filler                        pic x(2)
                value spaces.
           05 dl-percent-inc                pic x(5).
           05 filler                        pic x(2)
                value spaces.
           05 dl-pay-increase               pic $,$$$,$$9.99+.
           05 filler                        pic x
                value spaces.
           05 dl-new-salary                 pic $z,zzz,zzz.99.
      *--------------------------------------------------------
      *  CALCULATION VARIABLES
      *--------------------------------------------------------
       01 ws-detail-calc.
           05 ws-pay-increase               pic 9(5)v99.
           05 ws-new-salary                 pic 9(7)v99.
           05 ws-total-analyst              pic 9 value 0.
           05 ws-total-sen-prog             pic 9 value 0.
           05 ws-total-prog                 pic 9 value 0.
           05 ws-total-jr-prog              pic 9 value 0.
           05 ws-total-unclas               pic 9 value 0.
      *-----------------------------------------------
      *  EMPLOYEE CLASS #
      *-----------------------------------------------
       01 ws-employee-class-1.
           05 class-title-1                 pic x(15)
                value "EMPLOYEE CLASS:".
           05 filler                        pic x(10)
                value spaces.
           05 analyst-title                 pic x(7)
                value "Analyst".
           05 filler                        pic x(4)
                value spaces.
           05 sen-prog-title                pic x(8)
                value "Sen Prog".
           05 filler                        pic x(4)
                value spaces.
           05 prog-title                    pic x(4)
                value "Prog".
           05 filler                        pic x(4)
                value spaces.
           05 jr-prog-title                 pic x(7)
                value "Jr Prog".
           05 filler                        pic x(4)
                value spaces.
           05 unclas-title                  pic x(12)
                value "Unclassified".
       01 ws-employee-class-2.
           05 class-title-2                 pic x(15)
                value "# ON THIS PAGE:".
           05 filler                        pic x(14)
                value spaces.
           05 dl-total-analyst              pic z.
           05 filler                        pic x(11)
                value spaces.
           05 dl-total-sen-prog             pic z.
           05 filler                        pic x(7)
                value spaces.
           05 dl-total-prog                 pic z.
           05 filler                        pic x(10)
                value spaces.
           05 dl-total-jr-prog              pic z.
           05 filler                        pic x(15)
                value spaces.
           05 dl-total-unclas               pic z.

      *-----------------------------------------------
      *  REPORT SUMMARY
      *-----------------------------------------------
       01 ws-report-summary-one.
           05 ws-avg-inc                    pic x(18)
                value "AVERAGE INCREASES:".
           05 filler                        pic x(3)
                value spaces.
           05 ws-ananlyst-sum               pic x(8)
                value "ANALYST=".
           05 filler                        pic x(5)
                value spaces.
           05 dl-analyst-avg                pic zz,zzz.99.
           05 filler                        pic x(5)
                value spaces.
           05 ws-sen-prog-sum               pic x(9)
                value "SEN PROG=".
           05 filler                        pic x(5)
                value spaces.
           05 dl-sen-prog-avg               pic zz,zzz.99.
       01 ws-report-summary-two.
           05 filler                        pic x(21)
                value spaces.
           05 ws-prog-sum                   pic x(5)
                value "PROG=".
           05 filler                        pic x(8)
                value spaces.
           05 dl-prog-avg                   pic zz,zzz.99.
           05 filler                        pic x(5)
                value spaces.
           05 ws-jr-prog-sum                pic x(8)
                value "JR PROG=".
           05 filler                        pic x(6)
                value spaces.
           05 dl-jr-prog-avg                pic zz,zzz.99.

       01 avg-calc.
           05 ws-analyst-avg                pic 9(5)v99.
           05 ws-sen-prog-avg               pic 9(5)v99.
           05 ws-prog-avg                   pic 9(5)v99.
           05 ws-jr-prog-avg                pic 9(5)v99.

           05 ws-analyst-avg-pay            pic 9(5)v99.
           05 ws-sen-prog-avg-pay           pic 9(5)v99.
           05 ws-prog-avg-pay               pic 9(5)v99.
           05 ws-jr-prog-avg-pay            pic 9(5)v99.

           05 ws-analyst-count              pic 99.
           05 ws-sen-prog-count             pic 99.
           05 ws-prog-count                 pic 99.
           05 ws-jr-prog-count              pic 99.

       01 ws-blank-line                     pic x(82)  value spaces.
      *
       01 ws-page-summary.
           05 ws-page-title                 pic x(5)
                value "Page".
           05 dl-page                       pic z.
      *
       01 ws-eof-flag                       pic x.
       77 ws-eof-y                          pic x       value "Y".
       77 ws-eof-n                          pic x       value "N".
      *
       01 ws-page.
           05 ws-lines-per-page             pic 99      value 10.
           05 ws-line-ctr                   pic 99      value 0.
           05 ws-page-num                   pic 99      value 0.

      *-----------------------------------------------
      *  CURRENT DATETIME CODE FROM IBM DOCUMENTATION
      *-----------------------------------------------
       01 ws-current-date.
           05 ws-date.
                10 ws-year                  pic 9(4).
                10 ws-month                 pic 9(2).
                10 ws-day                   pic 9(2).
           05 ws-time.
                10 ws-hour                  pic 9(2).
                10 ws-minute                pic 9(2).
                10 ws-second                pic 9(2).
                10 ws-millisec              pic 9(2).
           05 ws-GMT-offset                 pic s9(4).

      *-----------------------------------------------
      *  CONSTANTS
      *-----------------------------------------------
       77 ws-analyst                        pic x(7)
           value "ANALYST".
       77 ws-senior                         pic x(8)
           value "SEN PROG".
       77 ws-programmer                     pic x(4)
           value "PROG".
       77 ws-junior                         pic x(7)
           value "JR PROG".
       77 ws-analyst-increase               pic 9(1)v999
           value 0.138.
       77 ws-senior-increase                pic 9(1)v999
           value 0.103.
       77 ws-programmer-increase            pic 9(1)v999
           value 0.077.
       77 ws-junior-increase                pic 9(1)v999
           value 0.042.
       77 ws-unclass-zero                   pic 9(1)v999
           value 0.000.
       77 dl-analyst-inc                    pic x(5)
           value "13.8%".
       77 dl-senior-inc                     pic x(5)
           value "10.3%".
       77 dl-programmer-inc                 pic x(4)
           value "7.7%".
       77 dl-junior-inc                     pic x(4)
           value "4.2%".

       procedure division.
       000-main.
      *
           perform 100-open-files.
           perform 150-report-header.
           perform 200-read-file.
           perform 400-process-recs
                until ws-eof-flag is equal to ws-eof-y.
           perform 800-report-summary.
           perform 900-close-files.
           goback.
      *
       100-open-files.
           open input input-file.
           open output output-file.
           move ws-eof-n                    to    ws-eof-flag.
      *
       150-report-header.
           move function current-date       to ws-current-date.
           move ws-date                     to ws-report-date.
           write output-line                from ws-report-header.

       200-read-file.
           read input-file
               at end move ws-eof-y         to    ws-eof-flag.
      *
       400-process-recs.
           perform 450-page-header.
           perform 460-column-header.
           perform 500-process-detail
                varying ws-line-ctr from 1 by 1
                    until ws-line-ctr > ws-lines-per-page or
                        ws-eof-flag is equal to ws-eof-y.
           perform 600-page-summary.
      *
       450-page-header.
           add 1                            to ws-page-num.
           move ws-page-num                 to dl-page.
           move 0                           to ws-total-analyst.
           move 0                           to ws-total-sen-prog.
           move 0                           to ws-total-prog.
           move 0                           to ws-total-jr-prog.
           move 0                           to ws-total-unclas.


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
           write output-line                from ws-blank-line.
           write output-line                from ws-blank-line.

      *
       460-column-header.
           write output-line                from ws-column-header.
           write output-line                from ws-column-header2.
           write output-line                from ws-blank-line.

       500-process-detail.
      *-----------------------------------------------
      * CALCULATIONS AND MANIPULATIONS AND DETAIL LINE PREP
      *-----------------------------------------------
           write output-line                from ws-detail-line.
      *-----------------------------------------------
      * EMPLOYEE POSITIONS
      *-----------------------------------------------
           if il-grad and il-g-analyst
                move ws-analyst             to dl-position
                add 1 to ws-total-analyst

           else if il-grad and il-g-senior
                move ws-senior              to dl-position
                add 1 to ws-total-sen-prog

           else if il-grad and il-g-prog
                move ws-programmer          to dl-position
                add 1 to ws-total-prog

           else if il-grad and il-g-unclass
                move spaces                 to dl-position
                add 1 to ws-total-unclas

           else if il-nongrad and il-n-prog
                move ws-programmer          to dl-position
                add 1 to ws-total-prog

           else if il-n-jrprog
                move ws-junior              to dl-position
                add 1 to ws-total-jr-prog

           else if il-nongrad and il-n-unclas
                move spaces                 to dl-position
                add 1 to ws-total-unclas
           end-if.
      *-----------------------------------------------
      * PERCENT INCREASE PER POSITION
      *-----------------------------------------------
           if il-g-analyst
                move dl-analyst-inc         to dl-percent-inc
           else if il-g-senior
                move dl-senior-inc          to dl-percent-inc
           else if il-g-prog
                move dl-programmer-inc      to dl-percent-inc
           else if il-g-unclass
                move spaces                 to dl-percent-inc
           else if il-n-prog
                move dl-programmer-inc      to dl-percent-inc
           else if il-n-jrprog
                move dl-junior-inc          to dl-percent-inc
           else if il-n-unclas
                move spaces                 to dl-percent-inc
           end-if.
      *-----------------------------------------------
      * SALARY INCREASE AMOUNT CALCULATION
      *-----------------------------------------------
           if il-g-analyst
                compute ws-pay-increase =
                                    (il-salary *
                                    (ws-analyst-increase * 10))
                add ws-pay-increase         to ws-analyst-avg

           else if il-g-senior
                compute ws-pay-increase =
                                    (il-salary *
                                    (ws-senior-increase * 10))
                add ws-pay-increase         to ws-sen-prog-avg

           else if il-g-prog
                compute ws-pay-increase =
                                    (il-salary *
                                    ( ws-programmer-increase * 10))
                add ws-pay-increase         to ws-prog-avg

           else if il-g-unclass
                move 0 to ws-pay-increase
                compute ws-pay-increase =
                                    il-salary * 0

           else if il-n-prog
                compute ws-pay-increase =
                                    (il-salary *
                                    (ws-programmer-increase * 10))
                add ws-pay-increase         to ws-prog-avg

           else if il-n-jrprog
                compute ws-pay-increase =
                                    (il-salary *
                                    (ws-junior-increase * 10))
                add ws-pay-increase         to ws-jr-prog-avg

           else if il-n-unclas
                move 0 to ws-pay-increase
                compute ws-pay-increase =
                                    il-salary * 0
           end-if.
      *-----------------------------------------------
      * NEW SALARY CALCULATION
      *-----------------------------------------------
           compute ws-new-salary = il-salary + ws-pay-increase.



      *-----------------------------------------------
      *  MOVING INPUT FILE VALUES INTO WORKING STORAGE FOR CALCULATIONS
      *-----------------------------------------------
           move il-no                       to dl-emp-no.
           move il-name                     to dl-emp-name.
           move il-years                    to dl-years.
           move il-salary                   to dl-present-salary.

           move ws-pay-increase             to dl-pay-increase.
           move ws-new-salary               to dl-new-salary.

           add ws-total-analyst             to ws-analyst-count.
           add ws-total-sen-prog            to ws-sen-prog-count.
           add ws-total-prog                to ws-prog-count.
           add ws-total-jr-prog             to ws-jr-prog-count.




           move ws-total-analyst            to dl-total-analyst.
           move ws-total-sen-prog           to dl-total-sen-prog.
           move ws-total-prog               to dl-total-prog.
           move ws-total-jr-prog            to dl-total-jr-prog.
           move ws-total-unclas             to dl-total-unclas.



      *-----------------------------------------------
      * SUMMARY ACTIONS
      *-----------------------------------------------
           perform 200-read-file.
      *
       600-page-summary.
           write output-line                from ws-blank-line
           write output-line                from ws-employee-class-1.
           write output-line                from ws-employee-class-2.
           write output-line                from ws-blank-line.
           write output-line                from ws-page-summary.


      *
       800-report-summary.
      *-----------------------------------------------
      * CALCULATING AVERAGE PAY INCREASES
      *-----------------------------------------------
           if ws-analyst-count > 0
                compute ws-analyst-avg-pay = ws-analyst-avg
                                    / ws-analyst-count.

           if ws-sen-prog-count > 0
                compute ws-sen-prog-avg-pay = ws-sen-prog-avg
                                    / ws-sen-prog-count.
           if ws-prog-count > 0
                compute ws-prog-avg-pay = ws-prog-avg
                                    / ws-prog-count.
           if ws-jr-prog-count > 0
                compute ws-jr-prog-avg-pay = ws-jr-prog-avg
                                    / ws-jr-prog-count
           end-if.

           move ws-analyst-avg-pay          to dl-analyst-avg.
           move ws-sen-prog-avg-pay         to dl-sen-prog-avg.
           move ws-prog-avg-pay             to dl-prog-avg.
           move ws-jr-prog-avg-pay          to dl-jr-prog-avg.

           write output-line                from ws-blank-line.
           write output-line                from ws-report-summary-one.
           write output-line                from ws-report-summary-two.
           write output-line                from ws-blank-line.
      *
       900-close-files.
           close input-file.
           close output-file.
      *

       end program A4SALRPT.