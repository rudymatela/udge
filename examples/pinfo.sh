#!/bin/bash
# Copyright 2020 Rudy Matela

env
echo
echo 'stdout is working'
echo 'stdout is working' >&1
echo 'stderr is working' >&2

echo "$@"
pwd
whoami
echo $BASHPID
