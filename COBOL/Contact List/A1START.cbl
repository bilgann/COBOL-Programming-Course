       identification division.
       program-id. A1START.
       date-written. January 21, 2025.
       author. Bilgan Kiris.
      *Description:
      *
       environment division.
       configuration section.
      *
       input-output section.
       file-control.
      *
           select output-file
                assign to OUTFILE
                organization is sequential.
      *
       data division.
       file section.
       fd output-file
           data record is output-line
           record contains 80 characters.
      *
       01 output-line                   pic x(80).
      *
       working-storage section.
      *
       01 ws-product.
           05 ws-product-name           pic x(15)
                value "Product Name".
           05 ws-product-description    pic x(25)
                value "Description".
           05 ws-product-quantity       pic x(8)
                value "Quantity".
      *               ----+----0--
       01 ws-title.
           05 ws-title-space            pic x(15).
           05 ws-title-name             pic x(25)
                value "Mainframe I Product List".
      *
       procedure division.
       000-main.
      *
           open output output-file.
      *
           move spaces                  to output-line.
           write output-line.
      *
           write output-line from ws-title.
      *
           move spaces                  to output-line.
           write output-line.
      *
           write output-line from ws-product.
      *
           move spaces                  to output-line.
           write output-line.
      *
           move "3000-001"              to ws-product-name.
           move "Ethernet Card"         to ws-product-description.
           move "000100"                to ws-product-quantity.
           write output-line from ws-product.
      *
           move "H324-1-COAX"           to ws-product-name.
           move "Cable Hub/Router"      to ws-product-description.
           move "000018"                to ws-product-quantity.
           write output-line from ws-product.
      *
           move "LP17-I9-32GB"          to ws-product-name.
           move "Laptop I9 CPU"         to ws-product-description.
           move "000033"                to ws-product-quantity.
           write output-line from ws-product.
      *
           move "CAT5-50"               to ws-product-name.
           move "CAT5 Ethernet cable"   to ws-product-description.
           move "000700"                to ws-product-quantity.
           write output-line from ws-product.
      *
           move "MS-USB"                to ws-product-name.
           move "Mouse USB"             to ws-product-description.
           move "000068"                to ws-product-quantity.
           write output-line from ws-product.
      *
           close output-file.
      *
           goback.
      *
       end program A1START.