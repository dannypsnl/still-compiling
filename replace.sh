new_proj_name=$1

# repo name
sed -i "" "s/racket-project/$new_proj_name/g" ./info.rkt
sed -i "" "s/racket-project/$new_proj_name/g" ./main.rkt
sed -i "" "s/racket-project/$new_proj_name/g" ./Makefile
sed -i "" "s/racket-project/$new_proj_name/g" ./scribblings/racket-project.scrbl
echo "# $new_proj_name" > README.md

# author name
if [ -z $2 ]; then echo "keep origin github id"; else sed -i "" "s/dannypsnl/$2/g" ./info.rkt; fi
if [ -z $3 ]; then echo "keep origin author name"; else sed -i "" "s/Lîm Tsú-thuàn/$3/g" ./scribblings/racket-project.scrbl; fi

mv ./scribblings/racket-project.scrbl ./scribblings/$new_proj_name.scrbl
