export SERVER_NAME=$1
export FULLCHAIN_PATH=$2
export PRIVKEY_PATH=$3
envsubst "\${SERVER_NAME}" < nginx-production.conf.template > nginx-production.conf
envsubst "\${FULLCHAIN_PATH} \${PRIVKEY_PATH}" < docker-compose-production.yml.template > docker-compose-production.yml
docker compose up -f docker-compose.yml -d