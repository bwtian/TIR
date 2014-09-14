git checkout -b travis && 
wget https://github.com/craigcitro/r-travis/raw/master/sample.travis.yml -O .travis.yml &&
sed -i -e '$a\' .Rbuildignore &&
echo '^\.travis\.yml$' >> .Rbuildignore &&
git add .travis.yml .Rbuildignore &&
git commit -m "enable continuous integration via craigcitro/r-travis" &&
git push origin travis
