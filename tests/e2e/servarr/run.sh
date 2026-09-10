#!/usr/bin/env bash
set -euo pipefail
ROOT=$(cd "$(dirname "$0")/../../.." && pwd)
cd "$ROOT"
# shellcheck source=images.env
source tests/e2e/servarr/images.env
CLUSTER="dpn-e2e-${GITHUB_RUN_ID:-$$}-${GITHUB_RUN_ATTEMPT:-1}"
ARTIFACTS=${E2E_ARTIFACTS:-$PWD/artifacts/servarr-e2e}
mkdir -p "$ARTIFACTS"
ARTIFACTS=$(cd "$ARTIFACTS" && pwd)
# Never switch or use the operator's Kubernetes context.
KUBE_DIR=$(mktemp -d)
chmod 700 "$KUBE_DIR"
export KUBECONFIG="$KUBE_DIR/config"
created=false
collect_and_cleanup() {
    rc=$?
    trap - EXIT
    if $created; then
        kubectl get pods -n dpn-e2e -o wide > "$ARTIFACTS/pods.txt" 2>&1 || true
        kubectl get events -n dpn-e2e --sort-by=.lastTimestamp > "$ARTIFACTS/events.txt" 2>&1 || true
        for app in sonarr radarr; do
            kubectl logs -n dpn-e2e "$app" > "$ARTIFACTS/$app-container.log" 2>&1 || true
            # Only synthetic media and test config exist in this cluster. Do not
            # collect /config/config.xml (API key) or the private kubeconfig.
            kubectl exec -n dpn-e2e "$app" -- sh -c \
                'tar -czf - /data/hook*.log /data/hook-events.jsonl /data/e2e-results.json /data/api-transcript.jsonl /data/seed.json /config/logs /opt/dpn/Cargo.lock 2>/dev/null' \
                > "$ARTIFACTS/$app-debug.tar.gz" 2>/dev/null || true
        done
        kind delete cluster --name "$CLUSTER" || true
    fi
    rm -f "$KUBECONFIG"
    rmdir "$KUBE_DIR"
    exit "$rc"
}
trap collect_and_cleanup EXIT

for app in sonarr radarr; do
    variable="${app^^}_IMAGE"
    docker build -f tests/e2e/servarr/Dockerfile \
        --build-arg "APP_IMAGE=${!variable}" -t "dpn-e2e-$app:local" .
done
created=true
kind create cluster --name "$CLUSTER" --image "$KIND_IMAGE" --wait 120s
# Optional Docker-outside-Docker access (for containerized development shells).
# Keep TLS verification against the kind certificate's localhost SAN.
if [[ -n ${E2E_DOCKER_HOST_GATEWAY:-} ]]; then
    port=$(docker inspect "${CLUSTER}-control-plane" --format '{{(index (index .NetworkSettings.Ports "6443/tcp") 0).HostPort}}')
    kubectl config set-cluster "kind-$CLUSTER" \
        --server="https://${E2E_DOCKER_HOST_GATEWAY}:$port" --tls-server-name=localhost
fi
kind load docker-image --name "$CLUSTER" dpn-e2e-sonarr:local dpn-e2e-radarr:local
kubectl create namespace dpn-e2e
for app in sonarr radarr; do
    if [[ $app == sonarr ]]; then port=8989; else port=7878; fi
    kubectl apply -f - <<YAML
apiVersion: v1
kind: Pod
metadata:
  name: $app
  namespace: dpn-e2e
spec:
  automountServiceAccountToken: false
  terminationGracePeriodSeconds: 10
  containers:
    - name: arr
      image: dpn-e2e-$app:local
      imagePullPolicy: Never
      env:
        - {name: PUID, value: "1000"}
        - {name: PGID, value: "1000"}
        - {name: TZ, value: "Etc/UTC"}
        - {name: DPN_E2E_EPHEMERAL, value: "1"}
      resources:
        requests: {cpu: "250m", memory: "256Mi"}
        limits: {cpu: "2", memory: "2Gi"}
      readinessProbe:
        tcpSocket: {port: $port}
        periodSeconds: 2
        failureThreshold: 60
      volumeMounts:
        - {name: config, mountPath: /config}
        - {name: data, mountPath: /data}
  volumes:
    - name: config
      emptyDir: {sizeLimit: "1Gi"}
    - name: data
      emptyDir: {sizeLimit: "1Gi"}
YAML
done
kubectl wait -n dpn-e2e --for=condition=Ready pod/sonarr pod/radarr --timeout=180s
failed=0
for app in sonarr radarr; do
    kubectl exec -n dpn-e2e "$app" -- sh -c 'chown 1000:1000 /data'
    # Arr's add endpoints require public metadata services. Seed only synthetic
    # metadata in the fresh migrated DB while the application is stopped; all
    # imports, notifications, upgrades and scans still use the real HTTP API.
    kubectl exec -n dpn-e2e "$app" -- python3 -c '
import sys, time, urllib.request, xml.etree.ElementTree as ET
port = 8989 if sys.argv[1] == "sonarr" else 7878
deadline = time.monotonic() + 120
while True:
    try:
        key = ET.parse("/config/config.xml").findtext("ApiKey")
        request = urllib.request.Request(f"http://127.0.0.1:{port}/api/v3/qualityprofile", headers={"X-Api-Key": key})
        with urllib.request.urlopen(request, timeout=5) as response:
            import json
            if json.load(response):
                break
    except (OSError, ET.ParseError):
        pass
    if time.monotonic() >= deadline:
        raise TimeoutError("Arr migrations and quality profiles did not become ready")
    time.sleep(0.5)
' "$app"
    kubectl exec -n dpn-e2e "$app" -- s6-svc -d "/run/service/service-$app"
    kubectl exec -n dpn-e2e "$app" -- s6-svwait -d -t 30000 "/run/service/service-$app"
    kubectl exec -n dpn-e2e "$app" -- s6-setuidgid hotio \
        python3 /opt/e2e/harness.py --app "$app" --seed-offline
    kubectl exec -n dpn-e2e "$app" -- s6-svc -u "/run/service/service-$app"
    # Run the driver as the same non-root user as the real Arr custom script.
    kubectl exec -n dpn-e2e "$app" -- s6-setuidgid hotio \
        python3 /opt/e2e/harness.py --app "$app" \
        > "$ARTIFACTS/$app-test.log" 2>&1 || failed=1
    cat "$ARTIFACTS/$app-test.log"
done
exit "$failed"
