export PATH="$coreutils/bin"
mkdir -p $out/bin
cp $webcatguipath $out/bin/webcatgui
mkdir -p $out/static/js
mkdir $out/static/css
cp -r $staticpath/js/*js $out/static/js/
cp -r $staticpath/css/*css $out/static/css/
