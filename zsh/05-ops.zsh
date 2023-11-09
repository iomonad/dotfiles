# 05-ops.zsh
# (c) 2023 iomonad <iomonad@riseup.net>

##################################################
# Elastic Search
##################################################

##################################################
# Kubernetes
##################################################

function kube-force-delete-ns () {
    NAMESPACE=$1
    OUT="$(mktemp)"
    kubectl proxy &
    kubectl get namespace $NAMESPACE -o json |jq '.spec = {"finalizers":[]}' > ${OUT}
    curl -k -H "Content-Type: application/json" -X PUT --data-binary @${OUT} 127.0.0.1:8001/api/v1/namespaces/$NAMESPACE/finalize

    rm ${OUT}
    echo "namespace $1 deleted"
}

function kube-toggle-cronjob () {
    NAMESPACE=$1

    if [ $# -eq 0 ]
    then
	echo "usage: $0 <namespace>"
	exit 1
    fi

    vared -p 'Suspends all Cronjobs in $NAMESPACE? (true/false): ' -c RESULT

    CRONJOBS=($(kubectl -n $NAMESPACE get cronjobs.batch -o json | jq -r '.items[].metadata.name'))

    for j in "${CRONJOBS[@]}"; do
	echo kubectl patch -n $NAMESPACE cronjobs $j -p "{\"spec\" : {\"suspend\" : ${RESULT} }}"
	kubectl patch -n $NAMESPACE cronjobs $j -p "{\"spec\" : {\"suspend\" : ${RESULT} }}"
    done
}
