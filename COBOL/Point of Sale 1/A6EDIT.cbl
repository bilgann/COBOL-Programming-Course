       identification division.
       program-id. A6EDIT.
       date-written. March 18th 2025.
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
      * invalid file declaration
           select invalid-file
               assign to INVFILE
               organization is sequential.
      * valid file declaration
           select valid-file
               assign to VALFILE
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
           05 il-tran-amt                   pic 9(5)v99.
           05 il-pay-type                   pic xx.
           05 il-store-num                  pic 99.
           05 il-inv-num                    pic x(9).
           05 il-inv-num-redef redefines il-inv-num.
                10 il-inv-f-char            pic x.
                10 il-inv-s-char            pic x.
                10 il-inv-dash              pic x.
                10 il-inv-number            pic 9(6).
           05 il-sku-code                   pic x(15).
      *
       fd output-file
           recording mode is F
           data record is output-line
           record contains 107 characters.
      *
       01 output-line                       pic x(107).
      *
       fd valid-file
           recording mode is F
           data record is valid-line
           record contains 36 characters.
      *
       01 valid-line                        pic x(36).
      *
       fd invalid-file
           recording mode is F
           data record is invalid-line
           record contains 36 characters.
      *
       01 invalid-line                      pic x(36).

      *
      *
       working-storage section.
      *
      *-----------------------------------------------
      *  REPORT HEADER
      *-----------------------------------------------
       01 ws-report-header.
           05 filler                        pic x(34)
                value spaces.
           05 ws-report-title               pic x(27)
                value "Bilgan Kiris - Assignment 6".
           05 filler                        pic x(4)
                value spaces.
           05 ws-report-date                pic 9(8).
           05 filler                        pic x(34)
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
           05 filler                        pic x(2)
                value "no".
           05 filler                        pic x(3)
                value spaces.
           05 filler                        pic x(5)
                value "trans".
           05 filler                        pic x
                value spaces.
           05 filler                        pic x(5)
                value "trans".
           05 filler                        pic x(3)
                value spaces.
           05 filler                        pic x(3)
                value "pay".
           05 filler                        pic x(3)
                value spaces.
           05 filler                        pic x(5)
                value "store".
           05 filler                        pic x(3)
                value spaces.
           05 filler                        pic x(7)
                value "invoice".
           05 filler                        pic x(4)
                value spaces.
           05 filler                        pic x(3)
                value "sku".
           05 filler                        pic x(15)
                value spaces.
           05 filler                        pic x(17)
                value "error description".

       01 ws-column-header2.
           05 filler                        pic x(5)
                value spaces.
           05 filler                        pic x(4)
                value "code".
           05 filler                        pic x(2)
                value spaces.
           05 filler                        pic x(6)
                value "amount".
           05 filler                        pic x(2)
                value spaces.
           05 filler                        pic x(4)
                value "type".
           05 filler                        pic x(2)
                value spaces.
           05 filler                        pic x(3)
                value "num".
           05 filler                        pic x(5)
                value spaces.
           05 filler                        pic x(3)
                value "num".
           05 filler                        pic x(8)
                value spaces.
           05 filler                        pic x(4)
                value "code".
      *
      *-----------------------------------------------
      *  DETAIL LINE
      *-----------------------------------------------
       01 ws-detail-line.
           05 dl-recno                      pic 999.
           05 filler                        pic x(2)
                value spaces.
           05 dl-trans-code                 pic x.
           05 filler                        pic x(2)
                value spaces.
           05 dl-trans-amount               pic z(5).99.
           05 filler                        pic x(5)
                value spaces.
           05 dl-pay-type                   pic xx.
           05 filler                        pic x(4)
                value spaces.
           05 dl-store-num                  pic xx.
           05 filler                        pic x(4)
                value spaces.
           05 dl-inv-num                    pic x(9).
           05 filler                        pic x(2)
                value spaces.
           05 dl-sku-code                   pic x(15).
           05 filler                        pic x(3)
                value spaces.
           05 dl-error-desc                 pic x(46) value spaces.

       01 ws-extra-line.
           05 filler                        pic x(39)
                value spaces.
           05 dl-extra-line                 pic x(50) value spaces.
      *-----------------------------------------------
      *  DETAIL LINE SUMMARY
      *-----------------------------------------------
       01 ws-summary-header.
           05 dl-summary-title              pic x(25)
                value "----- REPORT TOTALS -----".
       01 ws-total-records.
           05 dl-tt-rec                     pic x(15)
                value "TOTAL RECORDS:".
           05 dl-total-records              pic 9(3) value 0.
       01 ws-valid-records.
           05 dl-vl-rec                     pic x(15)
                value "VALID RECORDS:".
           05 dl-valid-records              pic 9(3) value 0.
       01 ws-invalid-records.
           05 dl-inv-rec                    pic x(17)
                value "INVALID RECORDS:".
           05 dl-invalid-records            pic 9(3) value 0.
      *-----------------------------------------------
      *  ERROR TYPES
      *-----------------------------------------------
       77 e1-desc                           pic x(35)
           value "Transaction Code must be S, R, or L".
       77 e2-desc                           pic x(34)
           value "Transaction amount must be numeric".
       77 e3-desc                           pic x(34)
           value "Payment type must be CA, CR, or DB".
       77 e4-desc                           pic x(46)
           value "Store num must be 01, 02, 03, 04, 05, or 12".
       77 e5-desc                           pic x(46)
           value "The Invoice Num can only be A, B, C, D or E".
       77 e6-desc                           pic x(48)
           value "Invoice Num cannot have two letters the same".
       77 e7-desc                           pic x(46)
           value "Invoice Num cannot be > 900000 or < 100000".
       77 e8-desc                           pic x(32)
           value "SKU Code can not be empty spaces".
      *
       01 ws-blank-line                     pic x(107)  value spaces.
      *
      *-----------------------------------------------
      *  COUNTERS
      *-----------------------------------------------
       01 ws-invalid-count                  pic 999 value 0.
       01 ws-valid-count                    pic 999 value 0.
       01 ws-total-count                    pic 999 value 0.
      *
       01 ws-page.
           05 ws-lines-per-page             pic 99 value 5.
           05 ws-line-ctr                   pic 99 value 0.
           05 ws-page-num                   pic 99 value 0.
      *
       01 ws-in-line-num                    pic 999 value 0.
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
           perform 800-report-summary.
           perform 900-close-files.
           goback.
      *
       100-open-files.
           open input input-file.
           open output output-file, invalid-file, valid-file.
           move ws-eof-n                    to    ws-eof-flag.
      *
       150-report-header.
           move function current-date       to ws-current-date.
           move ws-date                     to ws-report-date.
           write output-line                from ws-report-header.
           write output-line                from ws-blank-line.
           write output-line                from ws-column-header1.
           write output-line                from ws-column-header2.
      *
       200-read-file.
           read input-file
               at end move ws-eof-y         to ws-eof-flag.

           add 1                            to ws-total-count.
           move ws-total-count              to dl-total-records.
           add 1                            to ws-in-line-num.
           move ws-in-line-num              to dl-recno.
      *
       400-process-recs.
           move spaces                      to dl-error-desc.

           perform 510-check-trans-code.
           perform 520-check-numeric.
           perform 530-check-pay-type.
           perform 540-check-store-num.
           perform 550-check-in-num.
           perform 560-check-sku.
           perform 570-count-records.



           perform 500-process-detail
                varying ws-line-ctr from 1 by 1
                    until ws-line-ctr > ws-lines-per-page or
                        ws-eof-flag is equal to ws-eof-y.

      *
       500-process-detail.
      * check if the records is valid and prepare error report
       510-check-trans-code.
      * check if the transaction code is either S, R, or L
           if (il-tran-code = "S"
                or il-tran-code = "R"
                or il-tran-code = "L") then
                write valid-line            from input-line
           else
                move e1-desc                to dl-error-desc
                move il-tran-code           to dl-trans-code
                move il-tran-amt            to dl-trans-amount
                move il-pay-type            to dl-pay-type
                move il-store-num           to dl-store-num
                move il-inv-num             to dl-inv-num
                move il-sku-code            to dl-sku-code
                write invalid-line          from input-line
                write output-line           from ws-detail-line
                write output-line           from ws-blank-line
           end-if.
      *
       520-check-numeric.
      * check if the transaction amount is numeric.
           if il-tran-amt is numeric then
                write valid-line            from input-line
           else
                move e2-desc                to dl-error-desc
                move il-tran-code           to dl-trans-code
                move il-tran-amt            to dl-trans-amount
                move il-pay-type            to dl-pay-type
                move il-store-num           to dl-store-num
                move il-inv-num             to dl-inv-num
                move il-sku-code            to dl-sku-code
                write invalid-line          from input-line
                write output-line           from ws-detail-line
                write output-line           from ws-blank-line
           end-if.
      *
       530-check-pay-type.
      * check if the payment type is CA, CR or DB
           if (il-pay-type = "CA" or
                il-pay-type = "CR" or
                il-pay-type = "DB") then
                write valid-line            from input-line
           else
                move e3-desc                to dl-error-desc
                move il-tran-code           to dl-trans-code
                move il-tran-amt            to dl-trans-amount
                move il-pay-type            to dl-pay-type
                move il-store-num           to dl-store-num
                move il-inv-num             to dl-inv-num
                move il-sku-code            to dl-sku-code
                write invalid-line          from input-line
                write output-line           from ws-detail-line
                write output-line           from ws-blank-line
           end-if.
      *
       540-check-store-num.
      * check if the store number is of 01, 02, 03, 04, 05, or 12.
           if (il-store-num = "01" or
               il-store-num = "02" or
               il-store-num = "03" or
               il-store-num = "04" or
               il-store-num = "05" or
               il-store-num = "12")
               then
               write valid-line             from input-line
           else
                move e4-desc                to dl-error-desc
                move il-tran-code           to dl-trans-code
                move il-tran-amt            to dl-trans-amount
                move il-pay-type            to dl-pay-type
                move il-store-num           to dl-store-num
                move il-inv-num             to dl-inv-num
                move il-sku-code            to dl-sku-code
                write invalid-line          from input-line
                write output-line           from ws-detail-line
                write output-line           from ws-blank-line
           end-if.
      *
       550-check-in-num.
      * check if the invoice number is in the format XX-000000
           if il-inv-f-char = il-inv-s-char then
                move e6-desc                to dl-error-desc
                move il-tran-code           to dl-trans-code
                move il-tran-amt            to dl-trans-amount
                move il-pay-type            to dl-pay-type
                move il-store-num           to dl-store-num
                move il-inv-num             to dl-inv-num
                move il-sku-code            to dl-sku-code
                write invalid-line          from input-line
                write output-line           from ws-detail-line
                write output-line           from ws-blank-line
           else
               write valid-line             from input-line
           end-if.

           if (il-inv-f-char = "A" or il-inv-f-char = "B"
                or il-inv-f-char = "C" or il-inv-f-char = "D"
                or il-inv-f-char = "E")
                then
                write valid-line            from input-line
           else
                move e5-desc                to dl-error-desc
                move il-tran-code           to dl-trans-code
                move il-tran-amt            to dl-trans-amount
                move il-pay-type            to dl-pay-type
                move il-store-num           to dl-store-num
                move il-inv-num             to dl-inv-num
                move il-sku-code            to dl-sku-code
                write invalid-line          from input-line
                write output-line           from ws-detail-line
                write output-line           from ws-blank-line
           end-if.

           if (il-inv-s-char = "A" or il-inv-s-char = "B"
                or il-inv-s-char = "C" or il-inv-s-char = "D"
                or il-inv-s-char = "E")
                then
                write valid-line            from input-line
           else
                move e5-desc                to dl-error-desc
                move il-tran-code           to dl-trans-code
                move il-tran-amt            to dl-trans-amount
                move il-pay-type            to dl-pay-type
                move il-store-num           to dl-store-num
                move il-inv-num             to dl-inv-num
                move il-sku-code            to dl-sku-code
                write invalid-line          from input-line
                write output-line           from ws-detail-line
                write output-line           from ws-blank-line
           end-if.

           if (il-inv-number is numeric
              and il-inv-number > 100000
              and il-inv-number < 900000) then
                write valid-line            from input-line
           else
                move e7-desc                to dl-error-desc
                move il-tran-code           to dl-trans-code
                move il-tran-amt            to dl-trans-amount
                move il-pay-type            to dl-pay-type
                move il-store-num           to dl-store-num
                move il-inv-num             to dl-inv-num
                move il-sku-code            to dl-sku-code
                write invalid-line          from input-line
                write output-line           from ws-detail-line
                write output-line           from ws-blank-line
           end-if.

      *
       560-check-sku.
      * check if SKU code is empty
           if not il-sku-code = spaces then
                write valid-line             from input-line
           else
                move e8-desc                to dl-error-desc
                move il-tran-code           to dl-trans-code
                move il-tran-amt            to dl-trans-amount
                move il-pay-type            to dl-pay-type
                move il-store-num           to dl-store-num
                move il-inv-num             to dl-inv-num
                move il-sku-code            to dl-sku-code
                write invalid-line          from input-line
                write output-line           from ws-detail-line
                write output-line           from ws-blank-line
           end-if.
      *
       570-count-records.
           move 0                           to ws-invalid-count.
           move 0                           to ws-valid-count.

           if dl-error-desc = spaces then
                add 1                       to ws-valid-count
                move ws-valid-count         to dl-valid-records
           else
                add 1                       to ws-invalid-count
                move ws-invalid-count       to dl-invalid-records
           end-if.

      *

           perform 200-read-file.

      *
       800-report-summary.

           write output-line                from ws-blank-line.
           write output-line                from ws-summary-header.
           write output-line                from ws-total-records.
           write output-line                from ws-valid-records.
           write output-line                from ws-invalid-records.
      *
       900-close-files.
           close input-file.
           close output-file.
           close invalid-file.
           close valid-file.
      *
       end program A6EDIT.