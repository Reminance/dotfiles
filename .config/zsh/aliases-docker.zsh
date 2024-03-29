# ------------------------------------
# Docker alias and function
# ------------------------------------

# Get latest container
alias dpl="docker ps -l"

# Get latest container ID
alias dplq="docker ps -l -q"

# Get container process
alias dps="docker ps"

# Get process included stop container
alias dpa="docker ps -a"

# Search 
alias ds="docker search"

# log 
alias dl="docker logs"

# Get images
alias di="docker images"

# Get container
alias dc="docker container"

# Docker compose
alias dcp="docker-compose"

# Get container IP
alias dip="docker inspect --format '{{ .NetworkSettings.IPAddress }}'"

# Run deamonized container, e.g., $dkd base /bin/echo hello
alias dkd="docker run -d -P"

# Run interactive container, e.g., $dki base /bin/bash
alias dki="docker run -i -t -P"

# Execute interactive container, e.g., $dex base /bin/bash
alias dex="docker exec -i -t"

# docker stop $1
alias dstop="docker stop"

# docker start
alias dstart="docker start"

# Stop all containers
dstopall() { docker stop $(docker ps -a -q); }

# Remove all containers
# drm() { docker rm $(docker ps -a -q); }
drm() { docker rm $(docker container ls -a --format '{{.Names}}' | grep $1); }

# Stop and Remove all containers
alias drmf='docker stop $(docker ps -a -q) && docker rm $(docker ps -a -q)'

# Remove all images
dri() { docker rmi $(docker images -q); }

# Dockerfile build, e.g., $dbu tcnksm/test 
dbu() { docker build -t=$1 .; }

# Show all alias related docker
dalias() { alias | grep 'docker' | sed "s/^\([^=]*\)=\(.*\)/\1 => \2/"| sed "s/['|\']//g" | sort; dfunc; }

# show all docker function
dfunc() { cat ~/dotfiles/.config/zsh/aliases-docker.zsh | grep '.*() {.*}' }

# Bash into running container
dbash() { docker exec -it $(docker ps -aqf "name=$1") bash; }
