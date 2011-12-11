find -iname '*.hs' \
     -exec grep -q ^main {} \; \
     -exec runghc {} \;
