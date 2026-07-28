
template() {
	cpp -P -traditional-cpp "${@:1:$#-1}" | sed '/./,$!d' > "${!#}"
}

mkdir -p solutions templates

template -DTEMPLATE Polynomial.hs templates/Polynomial.hs
template -UTEMPLATE Polynomial.hs solutions/Polynomial.hs

# template -DTEMPLATE Polynomial.hs templates/Polynomial.hs
# template -UTEMPLATE Polynomial.hs solutions/Polynomial.hs
