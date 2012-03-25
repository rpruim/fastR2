# provide package.tar.gz as argument

echo "ftp  -u ftp://cran.r-project.org/incoming/$1 $1"
ftp  -u ftp://cran.r-project.org/incoming/$1 $1

