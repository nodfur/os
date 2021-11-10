{ pkgs, config, ... }:
{
  programs.bash.promptInit = ''
    if [ "$TERM" != "dumb" -o -n "$INSIDE_EMACS" ]; then
      PS1=$'\[\e[1m\]\h\[\e[0m\]:\w\[\e[1m\]`eval "$PS1GIT"`\[\e[0m\]\$ '
      PS1GIT='[[ `git status --short 2>/dev/null` ]] && echo \*'
      [[ $TERM = xterm* ]] && PS1='\[\033]2;\h:\w\007\]'"$PS1"
    fi
  '';
}
