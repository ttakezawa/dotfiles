#!/usr/bin/env bash

dir=$HOME/.emacs.d
lockfile=$dir/filelist.lock

home_filelist=$dir/home.filelist
system_filelist=$dir/system.filelist

filelock ()   { exec 9>>$1; flock -w 0 -x 9; }
fileunlock () { exec 9>&-; }

if ! filelock $lockfile; then
    echo "$(date "+%Y/%m/%d %H:%M:%S%t")Failed to get lock"
    exit 1
fi

ignore='(\.lo|\.o|\.elc|\.jpg|\.png|\.gif|\.swf|~|#)$'
ignore_dir='/([^/]+\.old|cache)/'

rm -f $home_filelist.new
locate --regex "^$HOME" | grep -Pv $ignore_dir | grep -Pv $ignore | sed -r "s|^$HOME|~|" >> $home_filelist.new
if [[ -s $home_filelist.new ]]; then
    mv $home_filelist.new $home_filelist
    echo "$(date "+%Y/%m/%d %H:%M:%S%t")updated $home_filelist"
fi

rm -f $system_filelist.new
locate --regex '^/(bin|boot|etc|lib|opt|sbin|usr|var)' | grep -Pv $ignore_dir | grep -Pv $ignore | sed -r "s|^$HOME|~|" >> $system_filelist.new
if [[ -s $system_filelist.new ]]; then
    mv $system_filelist.new $system_filelist
    echo "$(date "+%Y/%m/%d %H:%M:%S%t")updated $system_filelist"
fi

fileunlock
