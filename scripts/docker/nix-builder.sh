export PATH="$coreutils/bin"
mkdir -p $out/bin
cp $syscatguipath $out/bin/syscatgui
mkdir -p $out/static/js
mkdir $out/static/css
cp -r $staticpath/js/*js $out/static/js/
cp -r $staticpath/css/*css $out/static/css/
