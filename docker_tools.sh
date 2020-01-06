#!/bin/bash

export git_root=`git rev-parse --show-toplevel`

if [[ -n "$1" ]]; then
    if [[ "$1" = "--build" ]]; then
        docker build -f $git_root/Dockerfile -t aml .
        docker build -f $git_root/Dockerfile_tests -t aml_tests .
        exit 0
    fi
    if [[ "$1" = "--repl" ]]; then
        docker run --mount src=$git_root,target=/ArtisanalMachineLearning,type=bind -it aml /bin/bash
        exit 0
    fi
    if [[ "$1" = "--test" ]]; then
        docker build -f $git_root/Dockerfile_tests -t aml_tests .
        docker run --mount src=$git_root,target=/ArtisanalMachineLearning,type=bind -it aml_tests
        exit 0
    fi
else
    echo "Pass a flag such as --build or --repl or --test"
fi
