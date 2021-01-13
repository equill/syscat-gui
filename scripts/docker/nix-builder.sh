export PATH="$coreutils/bin"
mkdir -p $out/bin
cp $clwebcatpath $out/bin/clwebcat
mkdir -p $out/templates
cp $templatepath/* $out/templates/
mkdir -p $out/static/js
mkdir $out/static/css
cp -r $staticpath/js/*js $out/static/js/
cp -r $staticpath/css/*css $out/static/css/
