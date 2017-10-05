PKGDB=$(echo "--package-db=$GHC_PACKAGE_PATH" | sed -e "s/\:/ --package-db=/g")
GHC_PKG=$GHC_PACKAGE_PATH 
unset GHC_PACKAGE_PATH
cabal $PKGDB $@
export GHC_PACKAGE_PATH=${GHC_PKG}
