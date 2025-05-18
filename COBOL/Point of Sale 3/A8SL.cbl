       identification division.
       program-id. A8SL.
       date-written. April 1st 2024.
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
                value "SALES & LAYWAY PROCESSING REPORT".
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
           05 filler                        pic x(8)
                value spaces.
           05 filler                        pic x(7)
                value "INVOICE".
           05 filler                        pic x(11)
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
           05 filler                        pic x(10)
                value spaces.
           05 filler                        pic x(3)
                value "NUM".
           05 filler                        pic x(15)
                value spaces.
           05 filler                        pic x(4)
                value "CODE".

       01 ws-column-header3.
           05 filler                        pic x(47)
                value spaces.
           05 filler                        pic x(4)
                value "ITEM".
           05 filler                        pic x(9)
                value spaces.
           05 filler                        pic x(4)
                value "TRAN".
           05 filler                        pic x(14)
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
           05 filler                        pic x(8)
                value spaces.
           05 filler                        pic x(6)
                value "AMOUNT".
           05 filler                        pic x(11)
                value spaces.
           05 filler                        pic x(5)
                value "OWING".
           05 filler                        pic x(7)
                value spaces.
           05 filer                         pic x(7)
                value "PERCENT".
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

      *------- TOTAL RECORDS OF S&L AND TOTAL AMOUNTS --------------
       01 dl-sl-records.
           05 filler                        pic x(2)
                value spaces.
           05 filler                        pic x(13)
                value "S&L Records:".
           05 filler                        pic x(33)
                value spaces.
           05 dl-sl-rec                     pic 99.
           05 filler                        pic x(8)
                value spaces.
           05 dl-sl-amounts                 pic $$$,$$9.99.
           05 filler                        pic x(5)
                value spaces.
           05 dl-sl-tax                     pic $$$$,$$9.99.

      *------- TOTAL RECORDS OF SALE AND TOTAL AMOUNTS --------------
       01 dl-s-records.
           05 filler                        pic x(2)
                value spaces.
           05 filler                        pic x(13)
                value "Sale Records:".
           05 filler                        pic x(33)
                value spaces.
           05 dl-sale-records               pic 99.
           05 filler                        pic x(8)
                value spaces.
           05 dl-sale-amounts               pic $$$,$$9.99.
           05 filler                        pic x(5)
                value spaces.
           05 dl-s-tax                      pic $$$$,$$9.99.

      *------- TOTAL RECORDS OF LAYWAY AND TOTAL AMOUNTS ------------
       01 dl-l-records.
           05 filler                        pic x(2)
                value spaces.
           05 filler                        pic x(15)
                value "Layway Records:".
           05 filler                        pic x(31)
                value spaces.
           05 dl-lay-records                pic 99.
           05 filler                        pic x(7)
                value spaces.
           05 dl-lay-amounts                pic $$$,$$9.99.
           05 filler                        pic x(4)
                value spaces.
           05 dl-lay-tax                    pic $$$$,$$9.99.

      *-------- S&L TOTAL RECORD COUNT & AMOUNT FOR EACH STORE -------
       01 dls-sl-store-01.
           05 filler                        pic x(2)
                value spaces.
           05 filler                        pic x(14)
                value "S&L for Store:".
           05 filler                        pic x(16)
                value spaces.
           05 dl-sl-store-num-01            pic 99.
           05 filler                        pic x(14)
                value spaces.
           05 dl-sl-01-cnt                  pic 99.
           05 filler                        pic x(8)
                value spaces.
           05 dl-sl-01-amt                  pic $$$,$$9.99.
           05 filler                        pic x(5)
                value spaces.
           05 dl-sl-01-tax                  pic $$$$,$$9.99.

       01 dls-sl-store-02.
           05 filler                        pic x(2)
                value spaces.
           05 filler                        pic x(14)
                value "S&L for Store:".
           05 filler                        pic x(16)
                value spaces.
           05 dl-sl-store-num-02            pic 99.
           05 filler                        pic x(14)
                value spaces.
           05 dl-sl-02-cnt                  pic 99.
           05 filler                        pic x(8)
                value spaces.
           05 dl-sl-02-amt                  pic $$$,$$9.99.
           05 filler                        pic x(5)
                value spaces.
           05 dl-sl-02-tax                  pic $$$$,$$9.99.

       01 dls-sl-store-03.
           05 filler                        pic x(2)
                value spaces.
           05 filler                        pic x(14)
                value "S&L for Store:".
           05 filler                        pic x(16)
                value spaces.
           05 dl-sl-store-num-03            pic 99.
           05 filler                        pic x(14)
                value spaces.
           05 dl-sl-03-cnt                  pic 99.
           05 filler                        pic x(8)
                value spaces.
           05 dl-sl-03-amt                  pic $$$,$$9.99.
           05 filler                        pic x(5)
                value spaces.
           05 dl-sl-03-tax                  pic $$$$,$$9.99.

       01 dls-sl-store-04.
           05 filler                        pic x(2)
                value spaces.
           05 filler                        pic x(14)
                value "S&L for Store:".
           05 filler                        pic x(16)
                value spaces.
           05 dl-sl-store-num-04            pic 99.
           05 filler                        pic x(14)
                value spaces.
           05 dl-sl-04-cnt                  pic 99.
           05 filler                        pic x(8)
                value spaces.
           05 dl-sl-04-amt                  pic $$$,$$9.99.
           05 filler                        pic x(5)
                value spaces.
           05 dl-sl-04-tax                  pic $$$$,$$9.99.

       01 dls-sl-store-05.
           05 filler                        pic x(2)
                value spaces.
           05 filler                        pic x(14)
                value "S&L for Store:".
           05 filler                        pic x(16)
                value spaces.
           05 dl-sl-store-num-05            pic 99.
           05 filler                        pic x(14)
                value spaces.
           05 dl-sl-05-cnt                  pic 99.
           05 filler                        pic x(8)
                value spaces.
           05 dl-sl-05-amt                  pic $$$,$$9.99.
           05 filler                        pic x(5)
                value spaces.
           05 dl-sl-05-tax                  pic $$$$,$$9.99.

       01 dls-sl-store-12.
           05 filler                        pic x(2)
                value spaces.
           05 filler                        pic x(14)
                value "S&L for Store:".
           05 filler                        pic x(16)
                value spaces.
           05 dl-sl-store-num-12            pic 99.
           05 filler                        pic x(14)
                value spaces.
           05 dl-sl-12-cnt                  pic 99.
           05 filler                        pic x(8)
                value spaces.
           05 dl-sl-12-amt                  pic $$$,$$9.99.
           05 filler                        pic x(5)
                value spaces.
           05 dl-sl-12-tax                  pic $$$$,$$9.99.

      *-------- SL % AND TRANSACTIONS FOR EACH PAYMENT TYPE -------
       01 dl-CA.
           05 filler                        pic x(2)
                value spaces.
           05 filler                        pic x(13)
                value "Payment Type:".
           05 filler                        pic x(17)
                value spaces.
           05 dl-CA-type                    pic xx.
           05 filler                        pic x(14)
                value spaces.
           05 dl-CA-cnt                     pic 99.
           05 filler                        pic x(7)
                value spaces.
           05 dl-CA-amt                     pic $$$$,$$9.99.
           05 filler                        pic x(5)
                value spaces.
           05 dl-CA-tax                     pic $$$$,$$9.99.
           05 filler                        pic x(5)
                value spaces.
           05 dl-CA-per                     pic z9.99.
           05 filler                        pic x
                value "%".

       01 dl-CR.
           05 filler                        pic x(2)
                value spaces.
           05 filler                        pic x(13)
                value "Payment Type:".
           05 filler                        pic x(17)
                value spaces.
           05 dl-CR-type                    pic xx.
           05 filler                        pic x(14)
                value spaces.
           05 dl-CR-cnt                     pic 99.
           05 filler                        pic x(7)
                value spaces.
           05 dl-CR-amt                     pic $$$$,$$9.99.
           05 filler                        pic x(5)
                value spaces.
           05 dl-CR-tax                     pic $$$$,$$9.99.
           05 filler                        pic x(5)
                value spaces.
           05 dl-CR-per                     pic z9.99.
           05 filler                        pic x
                value "%".

       01 dl-DB.
           05 filler                        pic x(2)
                value spaces.
           05 filler                        pic x(13)
                value "Payment Type:".
           05 filler                        pic x(17)
                value spaces.
           05 dl-DB-type                    pic xx.
           05 filler                        pic x(14)
                value spaces.
           05 dl-DB-cnt                     pic 99.
           05 filler                        pic x(7)
                value spaces.
           05 dl-DB-amt                     pic $$$$,$$9.99.
           05 filler                        pic x(5)
                value spaces.
           05 dl-DB-tax                     pic $$$$,$$9.99.
           05 filler                        pic x(5)
                value spaces.
           05 dl-DB-per                     pic z9.99.
           05 filler                        pic x
                value "%".

      *-------- STORE WITH LARGEST & SMALLEST SL TOTAL -------
       01 dl-largest.
           05 filler                        pic x(2)
                value spaces.
           05 filler                        pic x(28)
                value "Store with largest SL total:".
           05 filler                        pic x(3)
                value spaces.
           05 dl-largest-store              pic zz.

       01 dl-smallest.
           05 filler                        pic x(2)
                value spaces.
           05 filler                        pic x(29)
                value "Store with smallest SL total:".
           05 filler                        pic x(2)
                value spaces.
           05 dl-smallest-store             pic zz.

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
           05 ws-sl-tax                     pic 9999v99.
           05 ws-s-tax                      pic 9999v99.
           05 ws-lay-tax                    pic 9999v99.
           05 ws-s-01-tax                   pic 9999v99.
           05 ws-s-02-tax                   pic 9999v99.
           05 ws-s-03-tax                   pic 9999v99.
           05 ws-s-04-tax                   pic 9999v99.
           05 ws-s-05-tax                   pic 9999v99.
           05 ws-s-12-tax                   pic 9999v99.
           05 ws-CA-tax                     pic 9999v99.
           05 ws-CR-tax                     pic 9999v99.
           05 ws-DB-tax                     pic 9999v99.
           05 ws-index                      pic 99 value 1.
                88 ws-index-valid value 1, 2, 3, 4, 5, 12.
           05 ws-line-limit                 pic 99 value 20.
           05 ws-SL-total                   pic 999.
           05 ws-s-counter                  pic 999 value 0.
           05 ws-lay-counter                pic 999 value 0.
           05 ws-s-total                    pic 999.
           05 ws-s-total-amt                pic 9(5)v99.
           05 ws-l-total                    pic 999.
           05 ws-lay-total-amt              pic 9(5)v99.
           05 ws-SL-total-amt               pic 9(5)v99.
           05 ws-sl-store-01-cnt            pic 999.
           05 ws-sl-store-01-amt            pic 9(8)v99.
           05 ws-sl-store-02-cnt            pic 999.
           05 ws-sl-store-02-amt            pic 9(8)v99.
           05 ws-sl-store-03-cnt            pic 999.
           05 ws-sl-store-03-amt            pic 9(8)v99.
           05 ws-sl-store-04-cnt            pic 999.
           05 ws-sl-store-04-amt            pic 9(8)v99.
           05 ws-sl-store-05-cnt            pic 999.
           05 ws-sl-store-05-amt            pic 9(8)v99.
           05 ws-sl-store-12-cnt            pic 999.
           05 ws-sl-store-12-amt            pic 9(8)v99.
           05 ws-CA-count                   pic 999 value 0.
           05 ws-CR-count                   pic 999 value 0.
           05 ws-DB-count                   pic 999 value 0.
           05 ws-CA-perc                    pic 99v99.
           05 ws-CR-perc                    pic 99v99.
           05 ws-DB-perc                    pic 99v99.
           05 ws-CA-amount                  pic 9(5)v99.
           05 ws-CR-amount                  pic 9(5)v99.
           05 ws-DB-amount                  pic 9(5)v99.
           05 ws-highest-amount             pic 9(8)v99 value 0.
           05 ws-highest-store              pic 99.
           05 ws-smallest-amount            pic 9(8)v99
                value 99999999.99.
           05 ws-smallest-store             pic 99.
      *
       01 ws-constants.
           05 ws-store-01                   pic 99 value 01.
           05 ws-store-02                   pic 99 value 02.
           05 ws-store-03                   pic 99 value 03.
           05 ws-store-04                   pic 99 value 04.
           05 ws-store-05                   pic 99 value 05.
           05 ws-store-12                   pic 99 value 12.
           05 ws-CA-type                    pic xx value "CA".
           05 ws-CR-type                    pic xx value "CR".
           05 ws-DB-type                    pic xx value "DB".
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

           perform 410-split-sl.
           perform 415-sl-store-01.
           perform 420-sl-store-02.
           perform 425-sl-store-03.
           perform 430-sl-store-04.
           perform 435-sl-store-05.
           perform 440-sl-store-12.
           perform 700-split-record.
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
       410-split-sl.
           if il-tran-code = "S" or il-tran-code = "L" then
                add il-tran-amt             to ws-SL-total-amt

                compute ws-sl-tax = ws-SL-total-amt * ws-tax-amount
           end-if.
      *
       415-sl-store-01.
           if (il-tran-code = "S" or il-tran-code = "L")
                and il-store-01 then
                move ws-store-01            to dl-sl-store-num-01
                add 1                       to ws-sl-store-01-cnt
                add il-tran-amt             to ws-sl-store-01-amt

                compute ws-s-01-tax =
                    ws-sl-store-01-amt * ws-tax-amount

           end-if.

           if (ws-sl-store-01-amt > ws-highest-amount) then
                    move ws-store-01        to ws-highest-store
                    move ws-sl-store-01-amt to ws-highest-amount
                else if
                    (ws-sl-store-01-amt < ws-smallest-amount) then
                     move ws-store-01       to ws-smallest-store
                     move ws-sl-store-01-amt to ws-smallest-amount
           end-if.
      *
       420-sl-store-02.
           if (il-tran-code = "S" or il-tran-code = "L")
                and il-store-02 then
                move ws-store-02            to dl-sl-store-num-02
                add 1                       to ws-sl-store-02-cnt
                add il-tran-amt             to ws-sl-store-02-amt

                compute ws-s-02-tax =
                    ws-sl-store-02-amt * ws-tax-amount

           end-if.

                if (ws-sl-store-02-amt > ws-highest-amount) then
                    move ws-store-02        to ws-highest-store
                    move ws-sl-store-02-amt to ws-highest-amount
                else if
                    (ws-sl-store-02-amt < ws-smallest-amount) then
                     move ws-store-02       to ws-smallest-store
                     move ws-sl-store-02-amt to ws-smallest-amount
                end-if.
      *
       425-sl-store-03.
           if (il-tran-code = "S" or il-tran-code = "L")
                and il-store-03 then
                move ws-store-03            to dl-sl-store-num-03
                add 1                       to ws-sl-store-03-cnt
                add il-tran-amt             to ws-sl-store-03-amt

                compute ws-s-03-tax =
                    ws-sl-store-03-amt * ws-tax-amount

           end-if.

           if (ws-sl-store-03-amt > ws-highest-amount) then
                    move ws-store-03        to ws-highest-store
                    move ws-sl-store-03-amt to ws-highest-amount
                else if
                    (ws-sl-store-03-amt < ws-smallest-amount) then
                     move ws-store-03       to ws-smallest-store
                     move ws-sl-store-03-amt to ws-smallest-amount
                end-if.
      *
       430-sl-store-04.
           if (il-tran-code = "S" or il-tran-code = "L")
                and il-store-04 then
                move ws-store-04            to dl-sl-store-num-04
                add 1                       to ws-sl-store-04-cnt
                add il-tran-amt             to ws-sl-store-04-amt

                compute ws-s-04-tax =
                    ws-sl-store-04-amt * ws-tax-amount

           end-if.

                if (ws-sl-store-04-amt > ws-highest-amount) then
                    move ws-store-04        to ws-highest-store
                    move ws-sl-store-04-amt to ws-highest-amount
                else if
                    (ws-sl-store-04-amt < ws-smallest-amount) then
                     move ws-store-04       to ws-smallest-store
                     move ws-sl-store-04-amt to ws-smallest-amount
                end-if.
      *
       435-sl-store-05.
           if (il-tran-code = "S" or il-tran-code = "L")
                and il-store-05 then
                move ws-store-05            to dl-sl-store-num-05
                add 1                       to ws-sl-store-05-cnt
                add il-tran-amt             to ws-sl-store-05-amt

                compute ws-s-05-tax =
                    ws-sl-store-05-amt * ws-tax-amount

           end-if.

           if (ws-sl-store-05-amt > ws-highest-amount) then
                    move ws-store-05        to ws-highest-store
                    move ws-sl-store-05-amt to ws-highest-amount
                else if
                    (ws-sl-store-05-amt < ws-smallest-amount) then
                     move ws-store-05       to ws-smallest-store
                     move ws-sl-store-05-amt to ws-smallest-amount
                end-if.
      *
       440-sl-store-12.
           if (il-tran-code = "S" or il-tran-code = "L")
                and il-store-12 then
                move ws-store-12            to dl-sl-store-num-12
                add 1                       to ws-sl-store-12-cnt
                add il-tran-amt             to ws-sl-store-12-amt

                compute ws-s-12-tax =
                    ws-sl-store-12-amt * ws-tax-amount

           end-if.

           if (ws-sl-store-12-amt > ws-highest-amount) then
                    move ws-store-12        to ws-highest-store
                    move ws-sl-store-12-amt to ws-highest-amount
                else if
                    (ws-sl-store-12-amt < ws-smallest-amount) then
                     move ws-store-12       to ws-smallest-store
                     move ws-sl-store-12-amt to ws-smallest-amount
                end-if.

      *
       700-split-record.
           if il-code-S then
                add 1                       to ws-s-counter
                move ws-s-counter           to ws-s-total
                add il-tran-amt             to ws-s-total-amt

                compute ws-s-tax = ws-s-total-amt * ws-tax-amount

           else if il-code-L then
                add 1                       to ws-lay-counter
                move ws-lay-counter         to ws-l-total
                add il-tran-amt             to ws-lay-total-amt

                compute ws-lay-tax = ws-lay-total-amt * ws-tax-amount

           end-if.

           compute ws-SL-total = ws-s-total + ws-l-total

           if il-pay-CA then
                move ws-CA-type             to dl-CA-type
                add 1                       to ws-CA-count
                add il-tran-amt             to ws-CA-amount

                compute ws-CA-tax = ws-CA-amount * ws-tax-amount

           else if il-pay-CR then
                move ws-CR-type             to dl-CR-type
                add 1                       to ws-CR-count
                add il-tran-amt             to ws-CR-amount

                compute ws-CR-tax = ws-CR-amount * ws-tax-amount

           else if il-pay-DB then
                move ws-DB-type             to dl-DB-type
                add 1                       to ws-DB-count
                add il-tran-amt             to ws-DB-amount

                compute ws-DB-tax = ws-DB-amount * ws-tax-amount
           end-if.

           compute ws-CA-perc = (ws-CA-count * 100) /  ws-SL-total.
           compute ws-CR-perc = (ws-CR-count * 100) /  ws-SL-total.
           compute ws-DB-perc = (ws-DB-count * 100) /  ws-SL-total.

           perform 200-read-file.
      *
       800-summary-files.
           perform varying ws-index from 1 by 1 until ws-index > 12
                if ws-index-valid then

                    move ws-SL-total        to dl-sl-rec
                    move ws-SL-total-amt    to dl-sl-amounts
                    move ws-sl-tax          to dl-sl-tax

                    move ws-s-total         to dl-sale-records
                    move ws-s-total-amt     to dl-sale-amounts
                    move ws-s-tax           to dl-s-tax

                    move ws-l-total         to dl-lay-records
                    move ws-lay-total-amt   to dl-lay-amounts
                    move ws-lay-tax         to dl-lay-tax

                    move ws-sl-store-01-cnt to dl-sl-01-cnt
                    move ws-sl-store-01-amt to dl-sl-01-amt
                    move ws-s-01-tax        to dl-sl-01-tax

                    move ws-sl-store-02-cnt to dl-sl-02-cnt
                    move ws-sl-store-02-amt to dl-sl-02-amt
                    move ws-s-02-tax        to dl-sl-02-tax

                    move ws-sl-store-03-cnt to dl-sl-03-cnt
                    move ws-sl-store-03-amt to dl-sl-03-amt
                    move ws-s-03-tax        to dl-sl-03-tax

                    move ws-sl-store-04-cnt to dl-sl-04-cnt
                    move ws-sl-store-04-amt to dl-sl-04-amt
                    move ws-s-04-tax        to dl-sl-04-tax

                    move ws-sl-store-05-cnt to dl-sl-05-cnt
                    move ws-sl-store-05-amt to dl-sl-05-amt
                    move ws-s-05-tax        to dl-sl-05-tax

                    move ws-sl-store-12-cnt to dl-sl-12-cnt
                    move ws-sl-store-12-amt to dl-sl-12-amt
                    move ws-s-12-tax        to dl-sl-12-tax

                    move ws-CA-count        to dl-CA-cnt
                    move ws-CA-perc         to dl-CA-per
                    move ws-CA-tax          to dl-CA-tax
                    move ws-CA-amount       to dl-CA-amt

                    move ws-CR-count        to dl-CR-cnt
                    move ws-CR-perc         to dl-CR-per
                    move ws-CR-tax          to dl-CR-tax
                    move ws-CR-amount       to dl-CR-amt

                    move ws-DB-count        to dl-DB-cnt
                    move ws-DB-perc         to dl-DB-per
                    move ws-DB-tax          to dl-DB-tax
                    move ws-DB-amount       to dl-DB-amt

                    if ws-sl-store-01-amt < ws-smallest-amount then
                        move ws-sl-store-01-amt to ws-smallest-amount
                        move ws-store-01        to ws-smallest-store
                    end-if

                    if ws-smallest-amount < ws-sl-store-02-amt then
                        move ws-sl-store-02-amt to ws-smallest-amount
                        move ws-store-02        to ws-smallest-store
                    end-if

                    if ws-smallest-amount < ws-sl-store-03-amt then
                        move ws-sl-store-03-amt to ws-smallest-amount
                        move ws-store-03        to ws-smallest-store
                    end-if

                    if ws-smallest-amount < ws-sl-store-04-amt then
                        move ws-sl-store-03-amt to ws-smallest-amount
                        move ws-store-03       to ws-smallest-store
                    end-if

                    if ws-smallest-amount < ws-sl-store-05-amt then
                        move ws-sl-store-04-amt to ws-smallest-amount
                        move ws-store-04        to ws-smallest-store
                    end-if

                    if ws-smallest-amount < ws-sl-store-12-amt then
                        move ws-sl-store-05-amt to ws-smallest-amount
                        move ws-store-05        to ws-smallest-store
                    end-if




                    move ws-smallest-store  to dl-smallest-store

                    move ws-highest-store   to dl-largest-store


                end-if
           end-perform.

           write report-line                from ws-blank-line.
           write report-line                from ws-column-header3.
           write report-line                from ws-column-header4.
           write report-line                from ws-blank-line.
           write report-line                from dl-s-records.
           write report-line                from dl-l-records.
           write report-line                from dl-sl-records.
           write report-line                from ws-blank-line.
           write report-line                from dls-sl-store-01.
           write report-line                from dls-sl-store-02.
           write report-line                from dls-sl-store-03.
           write report-line                from dls-sl-store-04.
           write report-line                from dls-sl-store-05.
           write report-line                from dls-sl-store-12.
           write report-line                from ws-blank-line.
           write report-line                from dl-CA.
           write report-line                from dl-CR.
           write report-line                from dl-DB.
           write report-line                from ws-blank-line.
           write report-line                from dl-largest.
           write report-line                from dl-smallest.

      *
       900-close-files.
           close input-file.
           close report-file.
       end program A8SL.