       identification division.
       program-id. A7SPLIT.
       date-written. March 26th 2025.
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
      * output-file declaration
           select report-file
               assign to RPTFILE
               organization is sequential.
      * invalid file declaration
           select output-sl
               assign to SLFILE
               organization is sequential.
      * valid file declaration
           select output-r
               assign to RFILE
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
       fd output-sl
           recording mode is F
           data record is output-sl-line
           record contains 36 characters.
      *
       01 output-sl-line                    pic x(36).
      *
       fd output-r
           recording mode is F
           data record is output-r-line
           record contains 36 characters.
      *
       01 output-r-line                     pic x(36).

      *
      *
       working-storage section.
      *
       01 ws-table.
           05 ws-tbl occurs 12 times.
                10 ws-SL-count              pic 999.
                10 ws-SL-amount             pic 9(8)v99.
                10 ws-R-count               pic 999.
                10 ws-R-amount              pic 9(8)v99.
      *
      *-----------------------------------------------
      *  REPORT HEADER
      *-----------------------------------------------
       01 ws-report-title1.
           05 filler                        pic x(34)
                value spaces.
           05 ws-report-title               pic x(27)
                value "DATA SPLIT AND COUNT REPORT".
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
      *
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
      *-----------------------------------------------
      *  COLUMN HEADERS
      *-----------------------------------------------
       01 ws-column-header1.
           05 filler                        pic x(60)
                value spaces.
           05 filler                        pic x(4)
                value "ITEM".
           05 filler                        pic x(16)
                value spaces.
           05 filler                        pic x(4)
                value "ITEM".

       01 ws-column-header2.
           05 filler                        pic x(2)
                value spaces.
           05 filler                        pic x(11)
                value "DESCRIPTION".
           05 filler                        pic x(30)
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
      *
      *-----------------------------------------------
      *  DETAIL LINE
      *-----------------------------------------------
      *------- TOTAL RECORDS OF S&L AND TOTAL AMOUNTS --------------
       01 dl-sl-records.
           05 filler                        pic x(2)
                value spaces.
           05 filler                        pic x(13)
                value "S&L Records:".
           05 filler                        pic x(47)
                value spaces.
           05 dl-sl-rec                     pic 99.
           05 filler                        pic x(15)
                value spaces.
           05 dl-sl-amounts                 pic $$$,$$9.99.

      *------- TOTAL RECORDS OF SALE AND TOTAL AMOUNTS --------------
       01 dl-s-records.
           05 filler                        pic x(2)
                value spaces.
           05 filler                        pic x(13)
                value "Sale Records:".
           05 filler                        pic x(47)
                value spaces.
           05 dl-sale-records               pic 99.
           05 filler                        pic x(15)
                value spaces.
           05 dl-sale-amounts               pic $$$,$$9.99.

      *------- TOTAL RECORDS OF LAYWAY AND TOTAL AMOUNTS ------------
       01 dl-l-records.
           05 filler                        pic x(2)
                value spaces.
           05 filler                        pic x(15)
                value "Layway Records:".
           05 filler                        pic x(45)
                value spaces.
           05 dl-lay-records                pic 99.
           05 filler                        pic x(15)
                value spaces.
           05 dl-lay-amounts                pic $$$,$$9.99.

      *------- TOTAL RECORDS OF RETURN AND TOTAL AMOUNTS ------------
       01 dl-r-records.
           05 filler                        pic x(2)
                value spaces.
           05 filler                        pic x(15)
                value "Return Records:".
           05 filler                        pic x(45)
                value spaces.
           05 dl-ret-records                pic 99.
           05 filler                        pic x(15)
                value spaces.
           05 dl-ret-amounts                pic $$$,$$9.99.

      *-------------------- GRAND TOTAL ---------------------------
       01 dl-grand-total.
           05 filler                        pic x(2)
                value spaces.
           05 filler                        pic x(12)
                value "Grand Total:".
           05 filler                        pic x(65)
                value spaces.
           05 dl-slr-total                  pic $$$,$$9.99.

      *-------- S&L TOTAL RECORD COUNT & AMOUNT FOR EACH STORE -------
       01 dls-sl-store-01.
           05 filler                        pic x(2)
                value spaces.
           05 filler                        pic x(14)
                value "S&L for Store:".
           05 filler                        pic x(30)
                value spaces.
           05 dl-sl-store-num-01            pic 99.
           05 filler                        pic x(14)
                value spaces.
           05 dl-sl-01-cnt                  pic 99.
           05 filler                        pic x(15)
                value spaces.
           05 dl-sl-01-amt                  pic $$$,$$9.99.

       01 dls-sl-store-02.
           05 filler                        pic x(2)
                value spaces.
           05 filler                        pic x(14)
                value "S&L for Store:".
           05 filler                        pic x(30)
                value spaces.
           05 dl-sl-store-num-02            pic 99.
           05 filler                        pic x(14)
                value spaces.
           05 dl-sl-02-cnt                  pic 99.
           05 filler                        pic x(15)
                value spaces.
           05 dl-sl-02-amt                  pic $$$,$$9.99.

       01 dls-sl-store-03.
           05 filler                        pic x(2)
                value spaces.
           05 filler                        pic x(14)
                value "S&L for Store:".
           05 filler                        pic x(30)
                value spaces.
           05 dl-sl-store-num-03            pic 99.
           05 filler                        pic x(14)
                value spaces.
           05 dl-sl-03-cnt                  pic 99.
           05 filler                        pic x(15)
                value spaces.
           05 dl-sl-03-amt                  pic $$$,$$9.99.

       01 dls-sl-store-04.
           05 filler                        pic x(2)
                value spaces.
           05 filler                        pic x(14)
                value "S&L for Store:".
           05 filler                        pic x(30)
                value spaces.
           05 dl-sl-store-num-04            pic 99.
           05 filler                        pic x(14)
                value spaces.
           05 dl-sl-04-cnt                  pic 99.
           05 filler                        pic x(15)
                value spaces.
           05 dl-sl-04-amt                  pic $$$,$$9.99.

       01 dls-sl-store-05.
           05 filler                        pic x(2)
                value spaces.
           05 filler                        pic x(14)
                value "S&L for Store:".
           05 filler                        pic x(30)
                value spaces.
           05 dl-sl-store-num-05            pic 99.
           05 filler                        pic x(14)
                value spaces.
           05 dl-sl-05-cnt                  pic 99.
           05 filler                        pic x(15)
                value spaces.
           05 dl-sl-05-amt                  pic $$$,$$9.99.

       01 dls-sl-store-12.
           05 filler                        pic x(2)
                value spaces.
           05 filler                        pic x(14)
                value "S&L for Store:".
           05 filler                        pic x(30)
                value spaces.
           05 dl-sl-store-num-12            pic 99.
           05 filler                        pic x(14)
                value spaces.
           05 dl-sl-12-cnt                  pic 99.
           05 filler                        pic x(15)
                value spaces.
           05 dl-sl-12-amt                  pic $$$,$$9.99.

      *-------- SL % AND TRANSACTIONS FOR EACH PAYMENT TYPE -------
       01 dl-CA.
           05 filler                        pic x(2)
                value spaces.
           05 filler                        pic x(13)
                value "Payment Type:".
           05 filler                        pic x(31)
                value spaces.
           05 dl-CA-type                    pic xx.
           05 filler                        pic x(14)
                value spaces.
           05 dl-CA-cnt                     pic 99.
           05 filler                        pic x(19)
                value spaces.
           05 dl-CA-per                     pic z9.99.
           05 filler                        pic x
                value "%".

       01 dl-CR.
           05 filler                        pic x(2)
                value spaces.
           05 filler                        pic x(13)
                value "Payment Type:".
           05 filler                        pic x(31)
                value spaces.
           05 dl-CR-type                    pic xx.
           05 filler                        pic x(14)
                value spaces.
           05 dl-CR-cnt                     pic 99.
           05 filler                        pic x(19)
                value spaces.
           05 dl-CR-per                     pic z9.99.
           05 filler                        pic x
                value "%".

       01 dl-DB.
           05 filler                        pic x(2)
                value spaces.
           05 filler                        pic x(13)
                value "Payment Type:".
           05 filler                        pic x(31)
                value spaces.
           05 dl-DB-type                    pic xx.
           05 filler                        pic x(14)
                value spaces.
           05 dl-DB-cnt                     pic 99.
           05 filler                        pic x(19)
                value spaces.
           05 dl-DB-per                     pic z9.99.
           05 filler                        pic x
                value "%".

      *-------- RET TOTAL RECORD COUNT & AMOUNT FOR EACH STORE -------
       01 dls-r-store-01.
           05 filler                        pic x(2)
                value spaces.
           05 filler                        pic x(21)
                value "Returns for Store:".
           05 filler                        pic x(23)
                value spaces.
           05 dl-r-store-num-01             pic 99.
           05 filler                        pic x(14)
                value spaces.
           05 dl-r-01-cnt                   pic 99.
           05 filler                        pic x(15)
                value spaces.
           05 dl-r-01-amt                   pic $$$,$$9.99.

       01 dls-r-store-02.
           05 filler                        pic x(2)
                value spaces.
           05 filler                        pic x(21)
                value "Returns for Store:".
           05 filler                        pic x(23)
                value spaces.
           05 dl-r-store-num-02             pic 99.
           05 filler                        pic x(14)
                value spaces.
           05 dl-r-02-cnt                   pic 99.
           05 filler                        pic x(15)
                value spaces.
           05 dl-r-02-amt                   pic $$$,$$9.99.

       01 dls-r-store-03.
           05 filler                        pic x(2)
                value spaces.
           05 filler                        pic x(21)
                value "Returns for Store:".
           05 filler                        pic x(23)
                value spaces.
           05 dl-r-store-num-03             pic 99.
           05 filler                        pic x(14)
                value spaces.
           05 dl-r-03-cnt                   pic 99.
           05 filler                        pic x(15)
                value spaces.
           05 dl-r-03-amt                   pic $$$,$$9.99.

       01 dls-r-store-04.
           05 filler                        pic x(2)
                value spaces.
           05 filler                        pic x(21)
                value "Returns for Store:".
           05 filler                        pic x(23)
                value spaces.
           05 dl-r-store-num-04             pic 99.
           05 filler                        pic x(14)
                value spaces.
           05 dl-r-04-cnt                   pic 99.
           05 filler                        pic x(15)
                value spaces.
           05 dl-r-04-amt                   pic $$$,$$9.99.

       01 dls-r-store-05.
           05 filler                        pic x(2)
                value spaces.
           05 filler                        pic x(21)
                value "Returns for Store:".
           05 filler                        pic x(23)
                value spaces.
           05 dl-r-store-num-05             pic 99.
           05 filler                        pic x(14)
                value spaces.
           05 dl-r-05-cnt                   pic 99.
           05 filler                        pic x(15)
                value spaces.
           05 dl-r-05-amt                   pic $$$,$$9.99.

       01 dls-r-store-12.
           05 filler                        pic x(2)
                value spaces.
           05 filler                        pic x(21)
                value "Returns for Store:".
           05 filler                        pic x(23)
                value spaces.
           05 dl-r-store-num-12             pic 99.
           05 filler                        pic x(14)
                value spaces.
           05 dl-r-12-cnt                   pic 99.
           05 filler                        pic x(15)
                value spaces.
           05 dl-r-12-amt                   pic $$$,$$9.99.
      *
       01 ws-blank-line                     pic x(107)  value spaces.
      *
      *-----------------------------------------------
      *  COUNTERS & CONSTANTS
      *-----------------------------------------------
       01 ws-counters.
           05 ws-CA-count                   pic 999 value 0.
           05 ws-CR-count                   pic 999 value 0.
           05 ws-DB-count                   pic 999 value 0.
           05 ws-SL-total                   pic 999.
           05 ws-s-counter                  pic 999 value 0.
           05 ws-lay-counter                pic 999 value 0.
           05 ws-r-counter                  pic 999 value 0.
           05 ws-SL-total-amt               pic 9(5)v99.
           05 ws-s-total                    pic 999.
           05 ws-s-total-amt                pic 9(5)v99.
           05 ws-l-total                    pic 999.
           05 ws-lay-total-amt              pic 9(5)v99.
           05 ws-r-total                    pic 999.
           05 ws-r-total-amt                pic 9(5)v99.
           05 ws-grand-total                pic 9(5)v99.
           05 ws-sl-store-01-cnt            pic 999.
           05 ws-sl-store-01-amt            pic 9(5)v99.
           05 ws-sl-store-02-cnt            pic 999.
           05 ws-sl-store-02-amt            pic 9(5)v99.
           05 ws-sl-store-03-cnt            pic 999.
           05 ws-sl-store-03-amt            pic 9(5)v99.
           05 ws-sl-store-04-cnt            pic 999.
           05 ws-sl-store-04-amt            pic 9(5)v99.
           05 ws-sl-store-05-cnt            pic 999.
           05 ws-sl-store-05-amt            pic 9(5)v99.
           05 ws-sl-store-12-cnt            pic 999.
           05 ws-sl-store-12-amt            pic 9(5)v99.
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
           05 ws-CA-perc                    pic 99v99.
           05 ws-CR-perc                    pic 99v99.
           05 ws-DB-perc                    pic 99v99.
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
           05 ws-lines-per-page             pic 99 value 5.
           05 ws-line-ctr                   pic 99 value 0.
           05 ws-page-num                   pic 99 value 0.
      *
       01 ws-in-line-num                    pic 999 value 0.
       01 ws-index                          pic 99 value 1.
           88 ws-index-valid value 1, 2, 3, 4, 5, 12.
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
           open output report-file, output-sl, output-r.
           move ws-eof-n                    to    ws-eof-flag.
      *
       150-report-header.
           move function current-date       to ws-current-date.
           move ws-date                     to ws-report-date.
           write report-line                from ws-report-title1.
           write report-line                from ws-report-title2.
           write report-line                from ws-report-title3.
           write report-line                from ws-column-header1.
           write report-line                from ws-column-header2.

      *
       200-read-file.
           read input-file
               at end move ws-eof-y         to ws-eof-flag.

           if il-code-S then
                add 1                       to ws-s-counter
                move ws-s-counter           to ws-s-total
                add il-tran-amt             to ws-s-total-amt
           else if il-code-L then
                add 1                       to ws-lay-counter
                move ws-lay-counter         to ws-l-total
                add il-tran-amt             to ws-lay-total-amt
           end-if.

           compute ws-SL-total = ws-s-total + ws-l-total

           if il-tran-code = "R" then
                add 1                       to ws-r-counter
                move ws-r-counter           to ws-r-total
                add il-tran-amt             to ws-r-total-amt
           end-if.

           compute ws-grand-total = ws-SL-total-amt - ws-r-total-amt.

           if il-pay-CA then
                move ws-CA-type             to dl-CA-type
                add 1                       to ws-CA-count

           else if il-pay-CR then
                move ws-CR-type             to dl-CR-type
                add 1                       to ws-CR-count

           else if il-pay-DB then
                move ws-DB-type             to dl-DB-type
                add 1                       to ws-DB-count
           end-if.

           compute ws-CA-perc = (ws-CA-count * 100) /  ws-SL-total.
           compute ws-CR-perc = (ws-CR-count * 100) /  ws-SL-total.
           compute ws-DB-perc = (ws-DB-count * 100) /  ws-SL-total.

      *
       400-process-recs.
           perform 510-split-sl.
           perform 515-split-r.
           perform 520-sl-store-01.
           perform 525-sl-store-02.
           perform 530-sl-store-03.
           perform 535-sl-store-04.
           perform 540-sl-store-05.
           perform 545-sl-store-12.
           perform 550-r-store-01.
           perform 555-r-store-02.
           perform 560-r-store-03.
           perform 565-r-store-04.
           perform 570-r-store-05.
           perform 575-r-store-12.
           perform 700-split-record.

           perform 500-process-detail
                varying ws-line-ctr from 1 by 1
                    until ws-line-ctr > ws-lines-per-page or
                        ws-eof-flag is equal to ws-eof-y.
      *
       500-process-detail.
      *
       510-split-sl.
           if il-tran-code = "S" or il-tran-code = "L" then
                write output-sl-line        from input-line
                add il-tran-amt             to ws-SL-total-amt
           end-if.
      *
       515-split-r.
           if il-tran-code = "R" then
                write output-r-line         from input-line
           end-if.
      *
       520-sl-store-01.
           if (il-tran-code = "S" or il-tran-code = "L")
                and il-store-01 then
                move ws-store-01            to dl-sl-store-num-01
                add 1                       to ws-sl-store-01-cnt
                add il-tran-amt             to ws-sl-store-01-amt
           end-if.
      *
       525-sl-store-02.
           if (il-tran-code = "S" or il-tran-code = "L")
                and il-store-02 then
                move ws-store-02            to dl-sl-store-num-02
                add 1                       to ws-sl-store-02-cnt
                add il-tran-amt             to ws-sl-store-02-amt
           end-if.
      *
       530-sl-store-03.
           if (il-tran-code = "S" or il-tran-code = "L")
                and il-store-03 then
                move ws-store-03            to dl-sl-store-num-03
                add 1                       to ws-sl-store-03-cnt
                add il-tran-amt             to ws-sl-store-03-amt
           end-if.
      *
       535-sl-store-04.
           if (il-tran-code = "S" or il-tran-code = "L")
                and il-store-04 then
                move ws-store-04            to dl-sl-store-num-04
                add 1                       to ws-sl-store-04-cnt
                add il-tran-amt             to ws-sl-store-04-amt
           end-if.
      *
       540-sl-store-05.
           if (il-tran-code = "S" or il-tran-code = "L")
                and il-store-05 then
                move ws-store-05            to dl-sl-store-num-05
                add 1                       to ws-sl-store-05-cnt
                add il-tran-amt             to ws-sl-store-05-amt
           end-if.
      *
       545-sl-store-12.
           if (il-tran-code = "S" or il-tran-code = "L")
                and il-store-12 then
                move ws-store-12            to dl-sl-store-num-12
                add 1                       to ws-sl-store-12-cnt
                add il-tran-amt             to ws-sl-store-12-amt
           end-if.
      *
       550-r-store-01.
           if il-tran-code = "R" and il-store-01 then
                move ws-store-01            to dl-r-store-num-01
                add 1                       to ws-r-store-01-cnt
                add il-tran-amt             to ws-r-store-01-amt
           end-if.
      *
       555-r-store-02.
           if il-tran-code = "R" and il-store-02 then
                move ws-store-02            to dl-r-store-num-02
                add 1                       to ws-r-store-02-cnt
                add il-tran-amt             to ws-r-store-02-amt
           end-if.
      *
       560-r-store-03.
           move ws-store-03            to dl-r-store-num-03
           if il-tran-code = "R" and il-store-03 then

                add 1                       to ws-r-store-03-cnt
                add il-tran-amt             to ws-r-store-03-amt
           end-if.
      *
       565-r-store-04.
           if il-tran-code = "R" and il-store-04 then
                move ws-store-04            to dl-r-store-num-04
                add 1                       to ws-r-store-04-cnt
                add il-tran-amt             to ws-r-store-04-amt
           end-if.
      *
       570-r-store-05.
           if il-tran-code = "R" and il-store-05 then
                move ws-store-05            to dl-r-store-num-05
                add 1                       to ws-r-store-05-cnt
                add il-tran-amt             to ws-r-store-05-amt
           end-if.
      *
       575-r-store-12.
           if il-tran-code = "R" and il-store-12 then
                move ws-store-12            to dl-r-store-num-12
                add 1                       to ws-r-store-12-cnt
                add il-tran-amt             to ws-r-store-12-amt
           end-if.
      *


      *
       700-split-record.

           perform 200-read-file.
      *
       800-summary-files.
           perform varying ws-index from 1 by 1 until ws-index > 12
                if ws-index-valid then
                    move ws-SL-total        to dl-sl-rec
                    move ws-SL-total-amt    to dl-sl-amounts

                    move ws-s-total         to dl-sale-records
                    move ws-s-total-amt     to dl-sale-amounts

                    move ws-l-total         to dl-lay-records
                    move ws-lay-total-amt   to dl-lay-amounts

                    move ws-r-total         to dl-ret-records
                    move ws-r-total-amt     to dl-ret-amounts

                    move ws-grand-total     to dl-slr-total

                    move ws-sl-store-01-cnt to dl-sl-01-cnt
                    move ws-sl-store-01-amt to dl-sl-01-amt

                    move ws-sl-store-02-cnt to dl-sl-02-cnt
                    move ws-sl-store-02-amt to dl-sl-02-amt

                    move ws-sl-store-03-cnt to dl-sl-03-cnt
                    move ws-sl-store-03-amt to dl-sl-03-amt

                    move ws-sl-store-04-cnt to dl-sl-04-cnt
                    move ws-sl-store-04-amt to dl-sl-04-amt

                    move ws-sl-store-05-cnt to dl-sl-05-cnt
                    move ws-sl-store-05-amt to dl-sl-05-amt

                    move ws-sl-store-12-cnt to dl-sl-12-cnt
                    move ws-sl-store-12-amt to dl-sl-12-amt

                    move ws-r-store-01-cnt  to dl-r-01-cnt
                    move ws-r-store-01-amt  to dl-r-01-amt

                    move ws-r-store-02-cnt  to dl-r-02-cnt
                    move ws-r-store-02-amt  to dl-r-02-amt

                    move ws-r-store-03-cnt  to dl-r-03-cnt
                    move ws-r-store-03-amt  to dl-r-03-amt

                    move ws-r-store-04-cnt  to dl-r-04-cnt
                    move ws-r-store-04-amt  to dl-r-04-amt

                    move ws-r-store-05-cnt  to dl-r-05-cnt
                    move ws-r-store-05-amt  to dl-r-05-amt

                    move ws-r-store-12-cnt  to dl-r-12-cnt
                    move ws-r-store-12-amt  to dl-r-12-amt

                    move ws-CA-count        to dl-CA-cnt
                    move ws-CA-perc         to dl-CA-per

                    move ws-CR-count        to dl-CR-cnt
                    move ws-CR-perc         to dl-CR-per

                    move ws-DB-count        to dl-DB-cnt
                    move ws-DB-perc         to dl-DB-per

                end-if
           end-perform.

           write report-line                from ws-blank-line.
           write report-line                from dls-sl-store-01.
           write report-line                from dls-sl-store-02.
           write report-line                from dls-sl-store-03.
           write report-line                from dls-sl-store-04.
           write report-line                from dls-sl-store-05.
           write report-line                from dls-sl-store-12.
           write report-line                from ws-blank-line.
           write report-line                from ws-blank-line.
           write report-line                from dl-s-records.
           write report-line                from dl-l-records.
           write report-line                from dl-sl-records.
           write report-line                from ws-blank-line.
           write report-line                from ws-blank-line.
           write report-line                from dl-CA.
           write report-line                from dl-CR.
           write report-line                from dl-DB.
           write report-line                from ws-blank-line.
           write report-line                from ws-blank-line.
           write report-line                from dls-r-store-01.
           write report-line                from dls-r-store-02.
           write report-line                from dls-r-store-03.
           write report-line                from dls-r-store-04.
           write report-line                from dls-r-store-05.
           write report-line                from dls-r-store-12.
           write report-line                from ws-blank-line.
           write report-line                from dl-r-records.
           write report-line                from ws-blank-line.
           write report-line                from dl-grand-total.

      *
       900-close-files.
           close input-file.
           close report-file.
           close output-sl.
           close output-r.
      *
       end program A7SPLIT.