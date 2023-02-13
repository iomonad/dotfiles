# 05-devkits.zsh
# (c) 2015 iomonad <iomonad@riseup.net>

#
# GNUPG
#

AGENT_SOCK=$(gpgconf --list-dirs | grep agent-socket | cut -d : -f 2)
if [[ ! -S $AGENT_SOCK ]]; then
  gpg-agent --daemon --use-standard-socket &>/dev/null
fi

export GPG_TTY=$(tty)
export GPG_TTY=$TTY

# Set SSH to use gpg-agent if it's enabled
GNUPGCONFIG="${GNUPGHOME:-"$HOME/.gnupg"}/gpg-agent.conf"

if [[ -r $GNUPGCONFIG ]] && command grep -q enable-ssh-support "$GNUPGCONFIG"; then
  export SSH_AUTH_SOCK="$AGENT_SOCK.ssh"
  unset SSH_AGENT_PID
fi

#
# SDKMAN
#

export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"

#
# Cargo
#

export PATH="$PATH:$HOME/.cargo/bin"

# NVM

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

#
# Google SDK
#

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/opt/google-cloud-sdk/path.zsh.inc' ]; then . '/opt/google-cloud-sdk/path.zsh.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/opt/google-cloud-sdk/completion.zsh.inc' ]; then . '/opt/google-cloud-sdk/completion.zsh.inc'; fi

#
# Docker
#

# Alias docker top podman if exists
if command -v podman &> /dev/null
then
    if [[ $DEBUG > 0 ]]; then
	echo "ZSH: using podman as docker client"
    fi
    export DOCKER_HOST="unix://$XDG_RUNTIME_DIR/podman/podman.sock"
    alias docker=podman
fi

alias d=docker
alias p=podman
alias pc=podman-compose

#
# Kubernetes
#
export USE_GKE_GCLOUD_AUTH_PLUGIN=True
export PATH="${KREW_ROOT:-$HOME/.krew}/bin:$PATH"

if command -v kubectl &> /dev/null
then
    if [[ $DEBUG > 0 ]]; then
	echo "ZSH: Initialzed kubernetes module"
    fi

    source <(kubectl completion zsh)

    # env
    export KUBECTL_EXTERNAL_DIFF=colordiff
    export PATH="${KREW_ROOT:-$HOME/.krew}/bin:$PATH"
    export USE_GKE_GCLOUD_AUTH_PLUGIN=True

    # aliases
    alias k=kubectl

    # Execute a kubectl command against all namespaces
    alias kca='_kca(){ kubectl "$@" --all-namespaces;  unset -f _kca; }; _kca'

    # Apply a YML file
    alias kaf='kubectl apply -f'

    # Drop into an interactive terminal on a container
    alias keti='kubectl exec -ti'

    # Manage configuration quickly to switch contexts between local, dev ad staging.
    alias kcuc='kubectl config use-context'
    alias kcsc='kubectl config set-context'
    alias kcdc='kubectl config delete-context'
    alias kccc='kubectl config current-context'

    # List all contexts
    alias kcgc='kubectl config get-contexts'

    # General aliases
    alias kdel='kubectl delete'
    alias kdelf='kubectl delete -f'

    alias kgj='kubectl get jobs'

    # Pod management.
    alias kgp='kubectl get pods'
    alias kgpa='kubectl get pods --all-namespaces'
    alias kgpw='kgp --watch'
    alias kgpwide='kgp -o wide'
    alias kep='kubectl edit pods'
    alias kdp='kubectl describe pods'
    alias kdelp='kubectl delete pods'
    alias kgpall='kubectl get pods --all-namespaces -o wide'

    # get pod by label: kgpl "app=myapp" -n myns
    alias kgpl='kgp -l'

    # get pod by namespace: kgpn kube-system"
    alias kgpn='kgp -n'

    # Service management.
    alias kgs='kubectl get svc'
    alias kgsa='kubectl get svc --all-namespaces'
    alias kgsw='kgs --watch'
    alias kgswide='kgs -o wide'
    alias kes='kubectl edit svc'
    alias kds='kubectl describe svc'
    alias kdels='kubectl delete svc'

    # Ingress management
    alias kgi='kubectl get ingress'
    alias kgia='kubectl get ingress --all-namespaces'
    alias kei='kubectl edit ingress'
    alias kdi='kubectl describe ingress'
    alias kdeli='kubectl delete ingress'

    # Namespace management
    alias kgns='kubectl get namespaces'
    alias kens='kubectl edit namespace'
    alias kdns='kubectl describe namespace'
    alias kdelns='kubectl delete namespace'
    alias kcn='kubectl config set-context --current --namespace'

    # ConfigMap management
    alias kgcm='kubectl get configmaps'
    alias kgcma='kubectl get configmaps --all-namespaces'
    alias kecm='kubectl edit configmap'
    alias kdcm='kubectl describe configmap'
    alias kdelcm='kubectl delete configmap'

    # Secret management
    alias kgsec='kubectl get secret'
    alias kgseca='kubectl get secret --all-namespaces'
    alias kdsec='kubectl describe secret'
    alias kdelsec='kubectl delete secret'

    # Deployment management.
    alias kgd='kubectl get deployment'
    alias kgda='kubectl get deployment --all-namespaces'
    alias kgdw='kgd --watch'
    alias kgdwide='kgd -o wide'
    alias ked='kubectl edit deployment'
    alias kdd='kubectl describe deployment'
    alias kdeld='kubectl delete deployment'
    alias ksd='kubectl scale deployment'
    alias krsd='kubectl rollout status deployment'

    # Rollout management.
    alias kgrs='kubectl get rs'
    alias krh='kubectl rollout history'
    alias kru='kubectl rollout undo'

    # Statefulset management.
    alias kgss='kubectl get statefulset'
    alias kgssa='kubectl get statefulset --all-namespaces'
    alias kgssw='kgss --watch'
    alias kgsswide='kgss -o wide'
    alias kess='kubectl edit statefulset'
    alias kdss='kubectl describe statefulset'
    alias kdelss='kubectl delete statefulset'
    alias ksss='kubectl scale statefulset'
    alias krsss='kubectl rollout status statefulset'

    # Port forwarding
    alias kpf="kubectl port-forward"

    # Tools for accessing all information
    alias kga='kubectl get all'
    alias kgaa='kubectl get all --all-namespaces'

    # Logs
    alias kl='kubectl logs'
    alias kl1h='kubectl logs --since 1h'
    alias kl1m='kubectl logs --since 1m'
    alias kl1s='kubectl logs --since 1s'
    alias klf='kubectl logs -f'
    alias klf1h='kubectl logs --since 1h -f'
    alias klf1m='kubectl logs --since 1m -f'
    alias klf1s='kubectl logs --since 1s -f'

    # File copy
    alias kcp='kubectl cp'

    # Node Management
    alias kgno='kubectl get nodes'
    alias keno='kubectl edit node'
    alias kdno='kubectl describe node'
    alias kdelno='kubectl delete node'

    # PVC management.
    alias kgpvc='kubectl get pvc'
    alias kgpvca='kubectl get pvc --all-namespaces'
    alias kgpvcw='kgpvc --watch'
    alias kepvc='kubectl edit pvc'
    alias kdpvc='kubectl describe pvc'
    alias kdelpvc='kubectl delete pvc'

    # Service account management.
    alias kdsa="kubectl describe sa"
    alias kdelsa="kubectl delete sa"

    # DaemonSet management.
    alias kgds='kubectl get daemonset'
    alias kgdsw='kgds --watch'
    alias keds='kubectl edit daemonset'
    alias kdds='kubectl describe daemonset'
    alias kdelds='kubectl delete daemonset'

    # CronJob management.
    alias kgcj='kubectl get cronjob'
    alias kecj='kubectl edit cronjob'
    alias kdcj='kubectl describe cronjob'
    alias kdelcj='kubectl delete cronjob'

    # packages
    alias kctx=kubectx
    alias kns=kubens

    # functions

    function kres(){
	kubectl set env $@ REFRESHED_AT=$(date +%Y%m%d%H%M%S)
    }

fi

# NixPKG

if [ -e $HOME/.nix-profile/etc/profile.d/nix.sh ]; then
    if [[ $DEBUG > 0 ]]; then
	echo "ZSH: Initialzed NixPKGs"
    fi
    . $HOME/.nix-profile/etc/profile.d/nix.sh;
fi

# Perl

export PATH="$PATH:~/.perl/bin"
export PERL5LIB="~/.perl/lib/perl5"
export PERL_MB_OPT="--install_base '$HOME/.perl'"
export PERL_MM_OPT="INSTALL_BASE=$HOME/.perl"
