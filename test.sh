for j in 1 2 3 4 6; do
  ccache -C >/dev/null
  echo "=== -j$j ==="
  SECONDS=0
  MAKEFLAGS=-j$j R CMD INSTALL --preclean . >/dev/null 2>&1
  echo "elapsed: ${SECONDS}s"
done
