#!/bin/bash

set -e

## run on wenjie's droplets
pkg=$(grep "Package" DESCRIPTION | awk '{print $NF}')
build_dir=$(pwd)
docs_repo=$HOME/wenjie/wwenjie.org
target_dir=$docs_repo/static/$pkg
tmp_log=.git_status.log

## update docs by pkgdown
make pkgdown

# go to the repository for wwenjie.org
cd $docs_repo
git checkout -f
git checkout main
git pull origin main
mkdir -p $target_dir
cp -r $build_dir/docs/* $target_dir
git status > $tmp_log
cat $tmp_log
if grep -q "static/$pkg/" $tmp_log
then
    git add static/$pkg/
    git commit -m "deploy $pkg $CI_COMMIT_SHORT_SHA by gitlab-runner"
    git push origin main
else
    printf "The docs was not updated.\n"
fi
rm $tmp_log
