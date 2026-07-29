
template() {
	cpp -P -traditional-cpp "${@:1:$#-1}" | sed '/./,$!d' > "${!#}"
}

mkdir -p solutions templates

template -DTEMPLATE src/Polynomial.hs templates/Polynomial.hs
template -UTEMPLATE src/Polynomial.hs solutions/Polynomial.hs
template -DTEMPLATE src/Rational.hs templates/Rational.hs
template -UTEMPLATE src/Rational.hs solutions/Rational.hs

