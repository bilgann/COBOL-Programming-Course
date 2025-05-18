       identification division.
       program-id. A2ILIST.
       date-written. January 26th, 2025.
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
           record contains 27 characters.
      *
       01 input-line.
           05 il-itemNo                     pic x(4).
           05 il-class                      pic x.
           05 il-desc                       pic x(13).
           05 il-qty                        pic 9(3).
           05 il-price                      pic 9(4)v99.

      *
       fd output-file
           recording mode is F
           data record is output-line
           record contains 108 characters.
      *
       01 output-line                       pic x(107).

       working-storage section.
      *
       01 ws-page-header.
           05 filler                        pic x(91)
                value spaces.
           05 ws-title-name                 pic x(16)
                value "Bilgan Kiris, A2".

       01 ws-report-header                  pic x(107)
           value "Page".

       01 ws-column-header1.
           05 ws-item-cl                    pic x(4)
                value "ITEM".
           05 filler                        pic x(5)
                value spaces.
           05 ws-item-cl2                   pic x(4)
                value "ITEM".
           05 filler                        pic x(7)
                value spaces.
           05 ws-qty-cl                     pic x(3)
                value "QTY".
           05 filler                        pic x(5)
                value spaces.
           05 ws-unit-cl                    pic x(4)
                value "UNIT".
           05 filler                        pic x(7)
                value spaces.
           05 ws-extp-cl                    pic x(8)
                value "EXTENDED".
           05 filler                        pic x(6)
                value spaces.
           05 ws-disc-cl                    pic x(8)
                value "DISCOUNT".
           05 filler                        pic x(6)
                value spaces.
           05 ws-netp-cl                    pic x(9)
                value "NET PRICE".
           05 filler                        pic x(2)
                value spaces.
           05 ws-class-cl                   pic x(5)
                value "CLASS".
           05 filler                        pic x(2)
                value spaces.
           05 ws-trans-cl                   pic x(5)
                value "TRANS".
           05 filler                        pic x(3)
                value spaces.
           05 ws-transpor-cl                pic x(14)
                value "TRANSPORTATION".


       01 ws-column-header2.
           05 filler                        pic x(2)
                value spaces.
           05 ws-hash-cl                    pic x
                value "#".
           05 filler                        pic x(3)
                value spaces.
           05 ws-desc-cl                    pic x(11)
                value "DESCRIPTION".
           05 filler                        pic x(10)
                value spaces.
           05 ws-unitp-cl                   pic x(5)
                value "PRICE".
           05 filler                        pic x(7)
                value spaces.
           05 ws-extp-cl2                   pic x(5)
                value "PRICE".
           05 filler                        pic x(10)
                value spaces.
           05 ws-disc-cl2                   pic x(6)
                value "AMOUNT".
           05 filler                        pic x(26)
                value spaces.
           05 ws-percent-cl                 pic x
                value "%".
           05 filler                        pic x(11)
                value spaces.
           05 ws-transpor-cl2               pic x(6)
                value "CHARGE".
           05 filler                        pic x(4)
                value spaces.

       01 ws-page-summary                   pic x(107)
           value "PageS".

       01 ws-report-summary                 pic x(107)
           value "ReportS".

       01 ws-blank-line                     pic x(107)
           value spaces.


       01 ws-detail-line.
           05 item-no                       pic x(6).
           05 description                   pic x(14).
           05 ws-qty                        pic z(3).
           05 ws-unit-price                 pic zz,zz9.99.
           05 filler                        pic x(4)
                value spaces.
           05 ws-extended-price             pic $$$$,$$$9.99.
           05 filler                        pic x(4)
                value spaces.
           05 ws-discount                   pic zz,zz9.99.
           05 filler                        pic x(3)
                value spaces.
           05 ws-net-price                  pic $$,$$9.99.
           05 filler                        pic x(5)
                value spaces.
           05 product-class                 pic x(1).
           05 filler                        pic x(1)
                value spaces.
           05 ws-trans-percentage           pic zz,z9.99.
           05 filler                        pic x(7)
                value spaces.
           05 ws-trans-charge               pic $$,$$9.99.

       01 ws-detail-calc.
           05 qty                           pic 9(4).
           05 unit-price                    pic 9(10)v99.
           05 extended-price                pic 9(15)v99.
           05 net-price                     pic 9(15)v99.
           05 discount                      pic 9(15)v99.
           05 trans-percentage              pic 9(3)v99.
           05 trans-charge                  pic 9(9)v99.


       01 ws-file.
           05 ws-eof-flag                   pic x.
           05 ws-eof-y                      pic x
                value "Y".
           05 ws-eof-n                      pic x
                value "N".

       01 ws-page.
           05 ws-lines-per-page             pic 99
                value 5.
           05 ws-line-ctr                   pic 99
                value 0.
           05 ws-page-num                   pic 99
                value 0.

       01 ws-total-lines.
           05 total-extended-price          pic 9(10)v99.
           05 total-net-price               pic 9(15)v99.
           05 total-trans-charge            pic 9(7)v99.
           05 total-no-discount             pic 9(5)v99.
           05 items-no-discount             pic 9(5).
           05 total-items                   pic 9(5).


       01 ws-item-no-disc.
           05 filler                        pic x(23)
                value "ITEMS WITHOUT DISCOUNT:".
           05 perc-no-discount              pic 9(2)v99.
           05 filler                        pic x(1).
           05 filler                        pic x
                value "%".

       01 ws-item-no-disc-perc              pic zzz,zz9.99.


       procedure division.
       000-main.
      *
           perform 100-open-files.
           write output-line                from ws-report-header.
           perform 200-read-file.
           perform 400-process-recs
                until ws-eof-flag is equal to ws-eof-y.
           write output-line                from ws-report-summary.
           perform 900-close-files.
           goback.

       100-open-files.
           open input input-file.
           open output output-file.
           move ws-eof-n                    to ws-eof-flag.

       200-read-file.
           read input-file
                at end move ws-eof-y        to ws-eof-flag.

       400-process-recs.
           add 1 to ws-page-num.
           if ws-page-num is equal to 1 then
                write output-line           from ws-page-header
                write output-line           from ws-blank-line
                write output-line           from ws-page-header
                write output-line           from ws-blank-line
           else
                write output-line           from ws-blank-line
                    after advancing page
                write output-line           from ws-blank-line
                write output-line           from ws-page-header
                write output-line           from ws-blank-line
           end-if.


           write output-line                from ws-column-header1.
           write output-line                from ws-column-header2.
           write output-line                from ws-blank-line.


           perform 500-process-detail
                varying ws-line-ctr from 1 by 1
                    until ws-line-ctr > ws-lines-per-page or
                        ws-eof-flag is equal to ws-eof-y.

      *    printing the "items without discount" line
           write output-line                from ws-item-no-disc.

           write output-line                from ws-page-summary.

       500-process-detail.
      *    do all the calculations and manipulations and
      *    detail line preparations.

      *    calculate extended price
           multiply il-qty by il-price giving extended-price.


      * Reset discount before calculating
      *      move 0 to discount.


      *    apply discount based on class
           if il-class = "A" and extended-price > 200
                multiply extended-price by 7.5 giving discount
                divide discount by 100 giving discount
           else if il-class = "F" and extended-price > 1000
                multiply extended-price by 7.5 giving discount
                divide discount by 100 giving discount
           else if il-class = "B" and il-qty > 100
               multiply extended-price by 7.5 giving discount
               divide discount by 100 giving discount
           else
               move 0.00 to discount


      *    transportation charge calculation
           if il-class = "A"
                multiply extended-price by 15.5 giving trans-charge
                divide trans-charge by 100 giving trans-charge
                move 15.5 to trans-percentage
           else if il-class = "D"
                multiply extended-price by 11.5 giving trans-charge
                divide trans-charge by 100 giving trans-charge
                move 11.5 to trans-percentage
           else if il-class = "F"
                multiply extended-price by 6.5 giving trans-charge
                divide trans-charge by 100 giving trans-charge
                move 6.5 to trans-percentage
           else if il-qty <= 200
                multiply extended-price by 4.5 giving trans-charge
                divide trans-charge by 100 giving trans-charge
                move 4.5 to trans-percentage
           else
                move 0.00 to trans-percentage
                move 75.00 to trans-charge
           end-if.

      *    items that have no discount
      *    counting total items
           add 1 to total-items.

      *    check if the item has no discount
           if total-items > 0
                divide items-no-discount by total-items
                giving perc-no-discount
                multiply perc-no-discount by 100
                giving perc-no-discount
           else
                move 0 to perc-no-discount
           end-if.

           if discount = 0
                add 1 to items-no-discount
           end-if.

      * calculate net price
           subtract discount from extended-price giving net-price




      *    store calculated data
           move il-itemNo                   to item-no.
           move il-desc                     to description.
           move il-qty                      to ws-qty.
           move il-price                    to ws-unit-price.
           move extended-price              to ws-extended-price.
           move discount                    to ws-discount.
           move net-price                   to ws-net-price.
           move il-class                    to product-class.
           move trans-percentage            to ws-trans-percentage.
           move trans-charge                to ws-trans-charge.
           move perc-no-discount            to ws-item-no-disc-perc.


           write output-line                from ws-detail-line.
           write output-line                from ws-blank-line
           write output-line                from ws-blank-line


           perform 200-read-file.



       900-close-files.
           close input-file.
           close output-file.

       end program A2ILIST.