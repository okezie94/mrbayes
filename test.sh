# for j in 1 2 3 4 5 6 8 10 15 20 24; do
#   ccache -C >/dev/null
#   echo "=== -j$j ==="
#   SECONDS=0
#   MAKEFLAGS=-j$j R CMD INSTALL --preclean . >/dev/null 2>&1
#   echo "elapsed: ${SECONDS}s"
# done

# ccache -C        # full clean
# ccache -z        # zero stats
# SECONDS=0
# MAKEFLAGS=-j6 R CMD INSTALL --preclean .
# echo "clang cold: ${SECONDS}s"
# ccache -s

# ccache -z        # zero stats but keep cache
# SECONDS=0
# MAKEFLAGS=-j6 R CMD INSTALL --preclean .
# echo "clang warm (no edits): ${SECONDS}s"
# ccache -s

# Edit one .stan file
# ccache -z
# SECONDS=0
# MAKEFLAGS=-j6 R CMD INSTALL --preclean .
# echo "clang warm (1 edit): ${SECONDS}s"
# ccache -s

# for model in inst/stan/*.stan; do
#   cp "$model" "$model.bak"
#   echo "// benchmark $(date +%s%N)" >> "$model"
#   ccache -z
#   SECONDS=0
#   R CMD INSTALL --preclean . >/dev/null 2>&1
#   echo "$(basename $model): ${SECONDS}s"
#   mv "$model.bak" "$model"
# done

for model in inst/stan/*.stan; do
  cp "$model" "$model.bak"
  # Insert at the top of the file
  sed -i "1i // benchmark $(date +%s%N)" "$model"
  ccache -z
  SECONDS=0
  R CMD INSTALL --preclean . >/dev/null 2>&1
  echo "$(basename $model): ${SECONDS}s"
  ccache -s | grep -E "Hits|Misses" | head -2
  mv "$model.bak" "$model"
done
