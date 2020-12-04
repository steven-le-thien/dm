
if [[ $3 -eq 1 ]]
then
    cabal v2-build; cabal v2-install --overwrite-policy=always;
fi

dm < $1 > mytest;
python ~/External_Software/INC/tools/compare_trees.py mytest $2;
printf "\n";
