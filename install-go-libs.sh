#!/bin/bash

go get -u -v github.com/stapelberg/expanderr
go get -u -v github.com/nsf/gocode
go get -u -v github.com/rogpeppe/godef
go get -u -v golang.org/x/tools/cmd/guru
go get -u -v golang.org/x/tools/cmd/gorename
go get -u -v golang.org/x/tools/cmd/goimports

go get -u -v github.com/alecthomas/gometalinter
gometalinter --install --update

go get -u -v github.com/godoctor/godoctor
go install github.com/godoctor/godoctor

go get -u -v github.com/fatih/gomodifytags


