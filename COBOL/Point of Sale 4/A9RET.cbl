       identification division.
       program-id. A9RET.
       date-written. April 3rd 2024.
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
           select report-file
               assign to RPTFILE
               organization is sequential.
      *
       data division.
       file section.
      *
       fd input-file
           recording mode is F
           data record is input-line
           record contains 36 characters.
      *
       01 input-line.
           05 il-tran-code                  pic x.
                88 il-code-S value "S".
                88 il-code-L value "L".
                88 il-code-R value "R".
           05 il-tran-amt                   pic 9(5)v99.
           05 il-pay-type                   pic xx.
                88 il-pay-CA value "CA".
                88 il-pay-CR value "CR".
                88 il-pay-DB value "DB".
           05 il-store-num                  pic 99.
                88 il-store-01 value 1.
                88 il-store-02 value 2.
                88 il-store-03 value 3.
                88 il-store-04 value 4.
                88 il-store-05 value 5.
                88 il-store-12 value 12.
           05 il-inv-num                    pic x(9).
           05 il-inv-num-redef redefines il-inv-num.
                10 il-inv-f-char            pic x.
                10 il-inv-s-char            pic x.
                10 il-inv-dash              pic x.
                10 il-inv-number            pic 9(6).
           05 il-sku-code                   pic x(15).
      *
       fd report-file
           recording mode is F
           data record is report-line
           record contains 107 characters.
      *
       01 report-line                       pic x(107).
      *
       working-storage section.
      *
      *-----------------------------------------------
      *  REPORT HEADER
      *-----------------------------------------------
       01 ws-report-title1.
           05 filler                        pic x(34)
                value spaces.
           05 ws-report-title               pic x(32)
                value "REPORT PROCESSING REPORT".
           05 filler                        pic x(4)
                value spaces.
           05 ws-report-date                pic 9(8).
           05 filler                        pic x(34)
                value spaces.

       01 ws-report-title2.
           05 filler                        pic x(45)
                value spaces.
           05 ws-report-title-2             pic x(27)
                value "BILGAN KIRIS".

       01 ws-report-title3.
           05 filler                        pic x(27)
                value spaces.
           05 ws-rep-title-line.
                10 ws-line occurs 27 times  pic x(27)
                    value "-".
           05 filler                        pic x(26)
                value spaces.
      *-----------------------------------------------
      *  COLUMN HEADERS
      *-----------------------------------------------
       01 ws-column-header1.
           05 filler                        pic x(2)
                value spaces.
           05 filler                        pic x(5)
                value "TRANS".
           05 filler                        pic x(5)
                value spaces.
           05 filler                        pic x(5)
                value "TRANS".
           05 filler                        pic x(8)
                value spaces.
           05 filler                        pic x(3)
                value "PAY".
           05 filler                        pic x(8)
                value spaces.
           05 filler                        pic x(5)
                value "STORE".
           05 filler                        pic x(9)
                value spaces.
           05 filler                        pic x(7)
                value "INVOICE".
           05 filler                        pic x(10)
                value spaces.
           05 filler                        pic x(3)
                value "SKU".
           05 filler                        pic x(21)
                value spaces.
           05 filler                        pic x(17)
                value "TAX AMOUNT".

       01 ws-column-header2.
           05 filler                        pic x(2)
                value spaces.
           05 filler                        pic x(4)
                value "CODE".
           05 filler                        pic x(6)
                value spaces.
           05 filler                        pic x(6)
                value "AMOUNT".
           05 filler                        pic x(7)
                value spaces.
           05 filler                        pic x(4)
                value "TYPE".
           05 filler                        pic x(7)
                value spaces.
           05 filler                        pic x(3)
                value "NUM".
           05 filler                        pic x(11)
                value spaces.
           05 filler                        pic x(3)
                value "NUM".
           05 filler                        pic x(14)
                value spaces.
           05 filler                        pic x(4)
                value "CODE".

       01 ws-column-header3.
           05 filler                        pic x(47)
                value spaces.
           05 filler                        pic x(4)
                value "ITEM".
           05 filler                        pic x(16)
                value spaces.
           05 filler                        pic x(4)
                value "TRAN".
           05 filler                        pic x(24)
                value spaces.
           05 filler                        pic x(3)
                value "TAX".

       01 ws-column-header4.
           05 filler                        pic x(2)
                value spaces.
           05 filler                        pic x(11)
                value "DESCRIPTION".
           05 filler                        pic x(17)
                value spaces.
           05 filler                        pic x(9)
                value "STORE NUM".
           05 filler                        pic x(8)
                value spaces.
           05 filler                        pic x(5)
                value "COUNT".
           05 filler                        pic x(15)
                value spaces.
           05 filler                        pic x(6)
                value "AMOUNT".
           05 filler                        pic x(20)
                value spaces.
           05 filler                        pic x(5)
                value "OWING".
      *-----------------------------------------------
      *  DETAIL LINE
      *-----------------------------------------------
      *-----------------  RECORDS  --------------------
       01 ws-detail-line.
           05 filler                        pic x(2)
                value spaces.
           05 dl-trans-code                 pic x.
           05 filler                        pic x(7)
                value spaces.
           05 dl-trans-amount               pic z(5).99.
           05 filler                        pic x(8)
                value spaces.
           05 dl-pay-type                   pic xx.
           05 filler                        pic x(9)
                value spaces.
           05 dl-store-num                  pic xx.
           05 filler                        pic x(11)
                value spaces.
           05 dl-inv-num                    pic x(9).
           05 filler                        pic x(8)
                value spaces.
           05 dl-sku-code                   pic x(15).
           05 filler                        pic x(6)
                value spaces.
           05 dl-tax-owing                  pic $$$,$$9.99.

      *------- TOTAL RECORDS OF RETURN AND TOTAL AMOUNTS ------------
       01 dl-r-records.
           05 filler                        pic x(2)
                value spaces.
           05 filler                        pic x(15)
                value "Return Records:".
           05 filler                        pic x(31)
                value spaces.
           05 dl-ret-records                pic 99.
           05 filler                        pic x(15)
                value spaces.
           05 dl-ret-amounts                pic $$$,$$9.99.
           05 filler                        pic x(12)
                value spaces.
           05 dl-r-tax                      pic $$$$,$$9.99.

      *-------- RET TOTAL RECORD COUNT & AMOUNT FOR EACH STORE -------
       01 dls-r-store-01.
           05 filler                        pic x(2)
                value spaces.
           05 filler                        pic x(21)
                value "Returns for Store:".
           05 filler                        pic x(9)
                value spaces.
           05 dl-r-store-num-01             pic 99.
           05 filler                        pic x(14)
                value spaces.
           05 dl-r-01-cnt                   pic 99.
           05 filler                        pic x(15)
                value spaces.
           05 dl-r-01-amt                   pic $$$,$$9.99.
           05 filler                        pic x(12)
                value spaces.
           05 dl-r-01-tax                   pic $$$$,$$9.99.

       01 dls-r-store-02.
           05 filler                        pic x(2)
                value spaces.
           05 filler                        pic x(21)
                value "Returns for Store:".
           05 filler                        pic x(9)
                value spaces.
           05 dl-r-store-num-02             pic 99.
           05 filler                        pic x(14)
                value spaces.
           05 dl-r-02-cnt                   pic 99.
           05 filler                        pic x(15)
                value spaces.
           05 dl-r-02-amt                   pic $$$,$$9.99.
           05 filler                        pic x(12)
                value spaces.
           05 dl-r-02-tax                   pic $$$$,$$9.99.

       01 dls-r-store-03.
           05 filler                        pic x(2)
                value spaces.
           05 filler                        pic x(21)
                value "Returns for Store:".
           05 filler                        pic x(9)
                value spaces.
           05 dl-r-store-num-03             pic 99.
           05 filler                        pic x(14)
                value spaces.
           05 dl-r-03-cnt                   pic 99.
           05 filler                        pic x(15)
                value spaces.
           05 dl-r-03-amt                   pic $$$,$$9.99.
           05 filler                        pic x(12)
                value spaces.
           05 dl-r-03-tax                   pic $$$$,$$9.99.

       01 dls-r-store-04.
           05 filler                        pic x(2)
                value spaces.
           05 filler                        pic x(21)
                value "Returns for Store:".
           05 filler                        pic x(9)
                value spaces.
           05 dl-r-store-num-04             pic 99.
           05 filler                        pic x(14)
                value spaces.
           05 dl-r-04-cnt                   pic 99.
           05 filler                        pic x(15)
                value spaces.
           05 dl-r-04-amt                   pic $$$,$$9.99.
           05 filler                        pic x(12)
                value spaces.
           05 dl-r-04-tax                   pic $$$$,$$9.99.

       01 dls-r-store-05.
           05 filler                        pic x(2)
                value spaces.
           05 filler                        pic x(21)
                value "Returns for Store:".
           05 filler                        pic x(9)
                value spaces.
           05 dl-r-store-num-05             pic 99.
           05 filler                        pic x(14)
                value spaces.
           05 dl-r-05-cnt                   pic 99.
           05 filler                        pic x(15)
                value spaces.
           05 dl-r-05-amt                   pic $$$,$$9.99.
           05 filler                        pic x(12)
                value spaces.
           05 dl-r-05-tax                   pic $$$$,$$9.99.

       01 dls-r-store-12.
           05 filler                        pic x(2)
                value spaces.
           05 filler                        pic x(21)
                value "Returns for Store:".
           05 filler                        pic x(9)
                value spaces.
           05 dl-r-store-num-12             pic 99.
           05 filler                        pic x(14)
                value spaces.
           05 dl-r-12-cnt                   pic 99.
           05 filler                        pic x(15)
                value spaces.
           05 dl-r-12-amt                   pic $$$,$$9.99.
           05 filler                        pic x(12)
                value spaces.
           05 dl-r-12-tax                   pic $$$$,$$9.99.

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
      *
       01 ws-blank-line                     pic x(107)  value spaces.
      *
      *-----------------------------------------------
      *  COUNTERS & CONSTANTS
      *-----------------------------------------------
       01 ws-variables.
           05 ws-tax-amount                 pic 9v99
                value 0.13.
           05 ws-tax-owing                  pic 999v99.
           05 ws-index                      pic 99 value 1.
                88 ws-index-valid value 1, 2, 3, 4, 5, 12.
           05 ws-line-limit                 pic 99 value 20.
           05 ws-r-counter                  pic 999 value 0.
           05 ws-r-total                    pic 999.
           05 ws-r-total-amt                pic 9(5)v99.
           05 ws-r-tax                      pic 9(4)v99.
           05 ws-r-store-01-cnt             pic 999.
           05 ws-r-store-01-amt             pic 9(5)v99.
           05 ws-r-store-02-cnt             pic 999.
           05 ws-r-store-02-amt             pic 9(5)v99.
           05 ws-r-store-03-cnt             pic 999.
           05 ws-r-store-03-amt             pic 9(5)v99.
           05 ws-r-store-04-cnt             pic 999.
           05 ws-r-store-04-amt             pic 9(5)v99.
           05 ws-r-store-05-cnt             pic 999.
           05 ws-r-store-05-amt             pic 9(5)v99.
           05 ws-r-store-12-cnt             pic 999.
           05 ws-r-store-12-amt             pic 9(5)v99.
           05 ws-r-01-tax                   pic 9(4)v99.
           05 ws-r-02-tax                   pic 9(4)v99.
           05 ws-r-03-tax                   pic 9(4)v99.
           05 ws-r-04-tax                   pic 9(4)v99.
           05 ws-r-05-tax                   pic 9(4)v99.
           05 ws-r-12-tax                   pic 9(4)v99.
      *
       01 ws-constants.
           05 ws-store-01                   pic 99 value 01.
           05 ws-store-02                   pic 99 value 02.
           05 ws-store-03                   pic 99 value 03.
           05 ws-store-04                   pic 99 value 04.
           05 ws-store-05                   pic 99 value 05.
           05 ws-store-12                   pic 99 value 12.
      *
       01 ws-page.
           05 ws-lines-per-page             pic 99 value 19.
           05 ws-line-ctr                   pic 99 value 0.
           05 ws-page-num                   pic 99 value 1.
      *
       01 ws-page-summary.
           05 ws-page-title                 pic x(5)
                value "Page".
           05 dl-page                       pic z.
      *
       01 ws-file.
           05 ws-eof-flag                   pic x.
           05 ws-eof-y                      pic x value "Y".
           05 ws-eof-n                      pic x value "N".
      *
       procedure division.
       000-main.
      *
           perform 100-open-files.
           perform 150-report-header.
           perform 200-read-file.
           perform 400-process-recs
                until ws-eof-flag is equal to ws-eof-y.
           perform 800-summary-files.
           perform 900-close-files.
           goback.
      *
       100-open-files.
           open input input-file.
           open output report-file.
           move ws-eof-n                    to ws-eof-flag.
      *
       150-report-header.
            move function current-date      to ws-current-date.
            move ws-date                    to ws-report-date.

            write report-line               from ws-report-title1
            write report-line               from ws-report-title2
            write report-line               from ws-report-title3


           write report-line                from ws-blank-line.
           write report-line                from ws-column-header1.
           write report-line                from ws-column-header2.
      *
       200-read-file.
           read input-file
               at end move ws-eof-y         to ws-eof-flag.
      *
       400-process-recs.
           add 1                            to ws-line-ctr.
           move ws-page-num                 to dl-page.
           perform 405-tax-owing.
           perform 406-detail-line.
           if ws-line-ctr > ws-lines-per-page then
                add 1                       to ws-page-num
                perform 407-page-num
                move 0                      to ws-line-ctr
           end-if.

           perform 410-r-store-01.
           perform 415-r-store-02.
           perform 420-r-store-03.
           perform 425-r-store-04.
           perform 430-r-store-05.
           perform 435-r-store-12.
           perform 700-return-process.
      *
       405-tax-owing.
           compute ws-tax-owing = il-tran-amt * ws-tax-amount.

           move il-tran-code                to dl-trans-code.
           move il-tran-amt                 to dl-trans-amount.
           move il-pay-type                 to dl-pay-type.
           move il-store-num                to dl-store-num.
           move il-inv-num                  to dl-inv-num.
           move il-sku-code                 to dl-sku-code.
           move ws-tax-owing                to dl-tax-owing.
      *
       406-detail-line.
           write report-line                from ws-detail-line.
      *
       407-page-num.
           write report-line                from ws-blank-line.
           write report-line                from ws-page-summary.
      *
       410-r-store-01.
           if il-store-01 then
                move ws-store-01            to dl-r-store-num-01
                add 1                       to ws-r-store-01-cnt
                add il-tran-amt             to ws-r-store-01-amt

                compute ws-r-01-tax =
                    ws-r-store-01-amt * ws-tax-amount
           end-if.
      *
       415-r-store-02.
           if il-store-02 then
                move ws-store-02            to dl-r-store-num-02
                add 1                       to ws-r-store-02-cnt
                add il-tran-amt             to ws-r-store-02-amt

                compute ws-r-02-tax =
                    ws-r-store-02-amt * ws-tax-amount

           end-if.
      *
       420-r-store-03.
           move ws-store-03                 to dl-r-store-num-03
           if il-store-03 then

                add 1                       to ws-r-store-03-cnt
                add il-tran-amt             to ws-r-store-03-amt

                compute ws-r-03-tax =
                    ws-r-store-03-amt * ws-tax-amount
           end-if.
      *
       425-r-store-04.
           if il-store-04 then
                move ws-store-04            to dl-r-store-num-04
                add 1                       to ws-r-store-04-cnt
                add il-tran-amt             to ws-r-store-04-amt

                compute ws-r-04-tax =
                    ws-r-store-04-amt * ws-tax-amount
           end-if.
      *
       430-r-store-05.
           if il-store-05 then
                move ws-store-05            to dl-r-store-num-05
                add 1                       to ws-r-store-05-cnt
                add il-tran-amt             to ws-r-store-05-amt

                compute ws-r-05-tax =
                    ws-r-store-05-amt * ws-tax-amount
           end-if.
      *
       435-r-store-12.
           if il-store-12 then
                move ws-store-12            to dl-r-store-num-12
                add 1                       to ws-r-store-12-cnt
                add il-tran-amt             to ws-r-store-12-amt

                compute ws-r-12-tax =
                    ws-r-store-12-amt * ws-tax-amount
           end-if.
      *

      *
       700-return-process.
           if il-tran-code = "R" then
                add 1                       to ws-r-counter
                move ws-r-counter           to ws-r-total
                add il-tran-amt             to ws-r-total-amt

                compute ws-r-tax = ws-r-total-amt * ws-tax-amount
           end-if.

           perform 200-read-file.
      *
       800-summary-files.
           perform varying ws-index from 1 by 1 until ws-index > 12
                if ws-index-valid then

                move ws-r-total             to dl-ret-records
                move ws-r-total-amt         to dl-ret-amounts

                move ws-r-tax               to dl-r-tax

                move ws-r-store-01-cnt      to dl-r-01-cnt
                move ws-r-store-01-amt      to dl-r-01-amt
                move ws-r-01-tax            to dl-r-01-tax

                move ws-r-store-02-cnt      to dl-r-02-cnt
                move ws-r-store-02-amt      to dl-r-02-amt
                move ws-r-02-tax            to dl-r-02-tax

                move ws-r-store-03-cnt      to dl-r-03-cnt
                move ws-r-store-03-amt      to dl-r-03-amt
                move ws-r-03-tax            to dl-r-03-tax

                move ws-r-store-04-cnt      to dl-r-04-cnt
                move ws-r-store-04-amt      to dl-r-04-amt
                move ws-r-04-tax            to dl-r-04-tax

                move ws-r-store-05-cnt      to dl-r-05-cnt
                move ws-r-store-05-amt      to dl-r-05-amt
                move ws-r-05-tax            to dl-r-05-tax

                move ws-r-store-12-cnt      to dl-r-12-cnt
                move ws-r-store-12-amt      to dl-r-12-amt
                move ws-r-12-tax            to dl-r-12-tax

                end-if
           end-perform.

           write report-line                from ws-blank-line.
           write report-line                from ws-column-header3.
           write report-line                from ws-column-header4.
           write report-line                from ws-blank-line.
           write report-line                from dls-r-store-01.
           write report-line                from dls-r-store-02.
           write report-line                from dls-r-store-03.
           write report-line                from dls-r-store-04.
           write report-line                from dls-r-store-05.
           write report-line                from dls-r-store-12.
           write report-line                from ws-blank-line.
           write report-line                from dl-r-records.


      *
       900-close-files.
           close input-file.
           close report-file.
       end program A9RET.