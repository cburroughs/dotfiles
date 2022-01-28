#!/bin/bash -x

# A coarse grained way of taking the current contents of a directory and
# shunting it into a git repo as a commit. (Like jenkins xml crontab of yore)

source=${1}
target=${2}

# eg 2022-W04 ; again this is intended to be coarse grained and line up with
# weekly planning rituals
date_stamp=$(date +"%G-W%V")


(cd ${target} && git pull --rebase)
rsync -a --delete --exclude=.git --exclude='.git*' ${source} ${target}
(cd ${target} && git add -A && git commit -m "${date_stamp}" && git push origin master)
