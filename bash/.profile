export PROJECT_DIR=/Users/james/projects
export GOPATH=$PROJECT_DIR/_go
export PYTHONPATH=/Users/james/Library/Python/2.7/lib/python/site-packages
export ALTERNATE_EDITOR=""
export PATH=$HOME/bin:$GOPATH/bin:$PATH:/Users/james/Library/Python/2.7/bin

alias bt="cd $PROJECT_DIR"
alias ll="ls -la"
alias ptags="find . -type f -name '*.py' | xargs etags"
alias gt="cd \$GOPATH/src/github.com"
alias docker_clean_images='docker rmi $(docker images -a --filter=dangling=true -q)'
alias docker_clean_ps='docker rm $(docker ps --filter=status=exited --filter=status=created -q)'

function gm() { gmw $* ; fswatch $(find . -name \*.go) | xargs -n1 -I{} gmw $* ;}
function gmm() { make $* ; fswatch $(find . -name \*.go) | xargs -n1 -I{} make $* ;}
function gmc() { $* ; fswatch $(find . -name \*.go) | xargs -n1 -I{} $* ;}

if [ -f "$HOME/.aliases" ]; then
    . "$HOME/.aliases"
fi

if [ -f `brew --prefix`/etc/bash_completion ]; then
    . `brew --prefix`/etc/bash_completion
fi

function color() {
  echo "\[$(tput setaf $1)\]"
}

function resetcolor() {
  echo "\[$(tput sgr0)\]"
}

function bashprompt() {
    local last_status=$?
    local failure="✘"
    local success="✔"
    if [[ "$last_status" != "0" ]]; then
        last_status="$(color 1)$failure$(resetcolor)"
    else
        last_status="$(color 2)$success$(resetcolor)"
    fi

    local git_status="$(git status 2> /dev/null)"
    local on_branch="On branch ([^${IFS}]*)"
    local on_commit="HEAD detached at ([^${IFS}]*)"

    local git_prompt=""
    if [[ $git_status =~ $on_branch ]]; then
        local branch=${BASH_REMATCH[1]}
        git_prompt=" ($branch)"
        if [[ ! $git_status =~ "working tree clean" ]]; then
            git_prompt="$(color 3)$git_prompt$(resetcolor)"
        fi
    elif [[ $git_status =~ $on_commit ]]; then
        local commit=${BASH_REMATCH[1]}
        git_prompt=" ($commit)"
        if [[ ! $git_status =~ "working tree clean" ]]; then
            git_prompt="$(color 3)$git_prompt$(resetcolor)"
        fi
    fi

    echo "$last_status [\W]$git_prompt \$ "
}

PROMPT_COMMAND='PS1=$(bashprompt)'

cd $PROJECT_DIR
