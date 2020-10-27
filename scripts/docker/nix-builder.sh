export PATH="$coreutils/bin"
mkdir -p $out/bin
cp $clwebcatpath $out/bin/clwebcat
mkdir -p $out/templates
cp $templatepath/*.tmpl $out/templates/
