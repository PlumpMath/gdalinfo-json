#lang racket

(provide string->gdalinfo
         gdalinfo->jsexpr
         gdalinfo)

(require test-engine/racket-tests)

(struct gdalinfo
  (driver
   files
   width
   height
   geogcs-name
   datum-name
   spheroid-name
   spheroid-semi-major-axis
   spheroid-authority-name
   spheroid-authority-id
   primem-name
   primem-value
   unit-name
   unit-value
   authority-name
   authority-value
   origin-x
   origin-y
   pixel-size-width
   pixel-size-height
   metadata
   img-structure-metadata
   upper-left-x
   upper-left-y
   lower-left-x
   lower-left-y
   upper-right-x
   upper-right-y
   lower-right-x
   lower-right-y
   center-x
   center-y
   band-1-block
   color-type
   color-interp)
  #:transparent)

(define (string->gdalinfo str)
  (let ([driver (second (regexp-match #px"Driver: (\\S+)" str))]
        [files  (second (regexp-match #px"Files: (\\S+)" str))]
        [width  (second (regexp-match #px"Size is (\\d+), \\d+\n" str))]
        [height (second (regexp-match #px"Size is \\d+, (\\d+)\n" str))]
        [geogcs-name (second (regexp-match #px"GEOGCS\\[\"(.*?)\"," str))]
        [datum-name  (second (regexp-match #px"DATUM\\[\"(.*?)\"," str))]
        [spheroid-name (second (regexp-match #px"SPHEROID\\[\"(.*?)\"," str))]
        [spheroid-semi-major-axis (second (regexp-match #px"SPHEROID\\[\".*?\",(\\d+\\.*\\d*)" str))]
        [spheroid-inverse-fytening (second (regexp-match #px"SPHEROID\\[\".*?\",\\d+\\.*\\d*,(\\d+\\.*\\d*)," str))]
        [spheroid-authority-name
          (second (regexp-match #px"SPHEROID\\[\".*?\",\\d+\\.*\\d*,\\d+\\.*\\d*,\\s*AUTHORITY\\[\"(\\S+)\","
                                str))]
        [spheroid-authority-id
          (second (regexp-match #px"SPHEROID\\[\".*?\",\\d+\\.*\\d*,\\d+\\.*\\d*,\\s*AUTHORITY\\[\"\\S+\",\"(\\d+)\""
                                str))]
        [primem-name (second (regexp-match #px"PRIMEM\\[\"(\\S+)\"" str))]
        [primem-value (second (regexp-match #px"PRIMEM\\[\"\\S+\",(\\d+)" str))]
        [unit-name (second (regexp-match #px"UNIT\\[\"(\\S+)\"" str))]
        [unit-value (second (regexp-match #px"UNIT\\[\"\\S+\",(\\d+\\.\\d+)" str))]
        [authority-name
          (second (regexp-match #px"UNIT\\[\"\\S+\",\\d+\\.\\d+\\],\\s*AUTHORITY\\[\"(\\S+)\"," str))]
        [authority-value
          (second (regexp-match #px"UNIT\\[\"\\S+\",\\d+\\.\\d+\\],\\s*AUTHORITY\\[\"\\S+\",\"(\\S+)\"" str))]
        [origin-x (second (regexp-match #px"Origin = \\((-{0,1}\\d+\\.*\\d*)," str))]
        [origin-y (second (regexp-match #px"Origin = \\(-{0,1}\\d+\\.*\\d*,(-{0,1}\\d+\\.*\\d*)" str))]
        [pixel-size-width (second (regexp-match #px"Pixel Size = \\((-{0,1}\\d+\\.*\\d*)," str))]
        [pixel-size-height (second (regexp-match #px"Pixel Size = \\(-{0,1}\\d+\\.*\\d*,(-{0,1}\\d+\\.*\\d*)" str))]
        [metadata (second (regexp-match #px"Metadata:\\s*(\\S+)\\s*Image Structure Metadata:" str))]
        [img-structure-metadata
          (second (regexp-match #px"Metadata:\\s*\\S+\\s*Image Structure Metadata:\\s*(\\S+)\\s*Corner" str))]
        [upper-left-x
          (second (regexp-match #px"Upper Left\\s+\\(\\s*(-{0,1}\\d+\\.*\\d*)," str))]
        [upper-left-y
          (second (regexp-match #px"Upper Left\\s+\\(\\s*-{0,1}\\d+\\.*\\d*,\\s*(-{0,1}\\d+\\.*\\d*)" str))]
        [lower-left-x
          (second (regexp-match #px"Lower Left\\s+\\(\\s*(-{0,1}\\d+\\.*\\d*)," str))]
        [lower-left-y
          (second (regexp-match #px"Lower Left\\s+\\(\\s*-{0,1}\\d+\\.*\\d*,\\s*(-{0,1}\\d+\\.*\\d*)" str))]
        [upper-right-x
          (second (regexp-match #px"Upper Right\\s+\\(\\s*(-{0,1}\\d+\\.*\\d*)," str))]
        [upper-right-y
          (second (regexp-match #px"Upper Right\\s+\\(\\s*-{0,1}\\d+\\.*\\d*,\\s*(-{0,1}\\d+\\.*\\d*)" str))]
        [lower-right-x
          (second (regexp-match #px"Lower Right\\s+\\(\\s*(-{0,1}\\d+\\.*\\d*)," str))]
        [lower-right-y
          (second (regexp-match #px"Lower Right\\s+\\(\\s*-{0,1}\\d+\\.*\\d*,\\s*(-{0,1}\\d+\\.*\\d*)" str))]
        [center-x
          (second (regexp-match #px"Center\\s+\\(\\s*(-{0,1}\\d+\\.*\\d*)," str))]
        [center-y
          (second (regexp-match #px"Center\\s+\\(\\s*-{0,1}\\d+\\.*\\d*,\\s*(-{0,1}\\d+\\.*\\d*)" str))]
        [band-1-block
          (second (regexp-match #px"Band 1 Block=(\\S+)\\s*" str))]
        [color-type
          (second (regexp-match #px"Type=(\\S+)," str))]
        [color-interp
          (second (regexp-match #px"ColorInterp=(\\S+)" str))])
    (gdalinfo
      driver
      files
      width
      height
      geogcs-name
      datum-name
      spheroid-name
      spheroid-semi-major-axis
      spheroid-authority-name
      spheroid-authority-id
      primem-name
      primem-value
      unit-name
      unit-value
      authority-name
      authority-value
      origin-x
      origin-y
      pixel-size-width
      pixel-size-height
      metadata
      img-structure-metadata
      upper-left-x
      upper-left-y
      lower-left-x
      lower-left-y
      upper-right-x
      upper-right-y
      lower-right-x
      lower-right-y
      center-x
      center-y
      band-1-block
      color-type
      color-interp)))

(define (gdalinfo->jsexpr info)
  (hasheq 'driver (gdalinfo-driver info)
          'files  (gdalinfo-files info)
          'width  (gdalinfo-width info)
          'height (gdalinfo-height info)
          'geogcs-name (gdalinfo-geogcs-name info)
          'datum-name (gdalinfo-datum-name info)
          'spheroid-name (gdalinfo-spheroid-name info)
          'spheroid-semi-major-axis (gdalinfo-spheroid-semi-major-axis info)
          'spheroid-authority-name (gdalinfo-spheroid-authority-name info)
          'spheroid-authority-id (gdalinfo-spheroid-authority-id info)
          'primem-name (gdalinfo-primem-name info)
          'premem-value (gdalinfo-primem-value info)
          'unit-name (gdalinfo-unit-name info)
          'unit-value (gdalinfo-unit-value info)
          'authority-name (gdalinfo-authority-name info)
          'authority-value (gdalinfo-authority-value info)
          'origin-x (gdalinfo-origin-x info)
          'origin-y (gdalinfo-origin-y info)
          'pixel-size-width (gdalinfo-pixel-size-width info)
          'pixel-size-height (gdalinfo-pixel-size-height info)
          'metadata (gdalinfo-metadata info)
          'img-structure-metadata (gdalinfo-metadata info)
          'upper-left-x (gdalinfo-upper-left-x info)
          'upper-left-y (gdalinfo-upper-left-y info)
          'lower-left-x (gdalinfo-lower-left-x info)
          'lower-left-y (gdalinfo-lower-left-y info)
          'upper-right-x (gdalinfo-upper-right-x info)
          'upper-right-y (gdalinfo-upper-right-y info)
          'lower-right-x (gdalinfo-lower-right-x info)
          'lower-right-y (gdalinfo-lower-right-y info)
          'center-x (gdalinfo-center-x info)
          'center-y (gdalinfo-center-y info)
          'band-1-block (gdalinfo-band-1-block info)
          'color-type (gdalinfo-color-type info)
          'color-interp (gdalinfo-color-interp info)))

(check-expect (string->gdalinfo (file->string "test/gdalinfo.txt"))
              (gdalinfo
                "GTiff/GeoTIFF"
                "/tmp/data/srtm_37_02_cropped.tif"
                "9792"
                "39132"
                "WGS 84"
                "WGS_1984"
                "WGS 84"
                "6378137"
                "EPSG"
                "7030"
                "Greenwich"
                "0"
                "degree"
                "0.0174532925199433"
                "EPSG"
                "4326"
                "2.683330000000000"
                "50.378999999999998"
                "0.000009259259259"
                "-0.000009259259259"
                "AREA_OR_POINT=Area"
                "INTERLEAVE=BAND"
                "2.6833300"
                "50.3790000"
                "2.6833300"
                "50.0166667"
                "2.7739967"
                "50.3790000"
                "2.7739967"
                "50.0166667"
                "2.7286633"
                "50.1978333"
                "9792x1"
                "Int16"
                "Gray"))
