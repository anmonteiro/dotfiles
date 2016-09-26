# Setting PATH for Python 3.4
# The orginal version is saved in .bash_profile.pysave
PATH="/Library/Frameworks/Python.framework/Versions/3.4/bin:${PATH}"
export PATH

# OPAM configuration
. /Users/anmonteiro/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true
eval $(opam config env)
