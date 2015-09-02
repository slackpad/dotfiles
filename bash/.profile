export PROJECT_DIR=/Users/james/projects
export GOPATH=$PROJECT_DIR/_go
export GOMAXPROCS=$(sysctl -n hw.ncpu)
export ALTERNATE_EDITOR=""
export PATH=$HOME/bin:$GOPATH/bin:$PATH

alias bt="cd $PROJECT_DIR"
alias ll="ls -la"
alias ptags="find . -type f -name '*.py' | xargs etags"
alias gt="cd \$GOPATH/src/github.com"

function gm() { sh -c "clear && time $*" ; fswatch `find . -name \*.go` | xargs -n1 -I{} sh -c "clear && time $*" ;}

if [ -f `brew --prefix`/etc/bash_completion ]; then
    . `brew --prefix`/etc/bash_completion
fi

cd $PROJECT_DIR
