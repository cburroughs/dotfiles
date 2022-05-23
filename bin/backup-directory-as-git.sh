#!/bin/bash -x

# A coarse grained way of taking the current contents of a directory and
# shunting it into a git repo as a commit. (Like jenkins xml crontab of yore)

pull_cmd="git pull --rebase"
push_cmd="git push origin master"

source=${1}
target=${2}
local=${3}

if [ ${local} == 'local' ]; then
    pull_cmd='true'
    push_cmd='true'
fi

# eg 2022-W04 ; again this is intended to be coarse grained and line up with
# weekly planning rituals
date_stamp=$(date +"%G-W%V")

(cd ${target} && ${pull_cmd})
rsync -a --delete --exclude=.git --exclude='.git*' ${source} ${target}
(cd ${target} && git add -A && git commit -m "${date_stamp}" && ${push_cmd})
